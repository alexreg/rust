// Copyright 2018 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Not in interpret to make sure we do not use private implementation details

use std::fmt;
use std::error::Error;

use rustc::hir::{self, def_id::DefId};
use rustc::hir::def::Def;
use rustc::mir::interpret::{ConstEvalErr, ErrorHandled};
use rustc::mir;
use rustc::ty::{self, TyCtxt, Instance, query::TyCtxtAt};
use rustc::ty::layout::{LayoutOf, TyLayout};
use rustc::ty::subst::Subst;
use rustc::util::nodemap::FxHashSet;
use rustc_data_structures::indexed_vec::{IndexVec, Idx};
use rustc::util::common::ErrorReported;

use syntax::ast::Mutability;
use syntax::source_map::Span;

use rustc::mir::interpret::{
    EvalResult, EvalError, EvalErrorKind, GlobalId,
    Scalar, Allocation, ConstValue,
};
use interpret::{self,
    Place, PlaceTy, MemPlace, OpTy, Operand, Value,
    EvalContext, StackPopCleanup, MemoryKind,
};

pub fn mk_borrowck_eval_cx<'a, 'mir, 'tcx>(
    tcx: TyCtxt<'a, 'tcx, 'tcx>,
    instance: Instance<'tcx>,
    mir: &'mir mir::Mir<'tcx>,
    span: Span,
) -> EvalResult<'tcx, EvalContext<'a, 'mir, 'tcx, CompileTimeEvaluator>> {
    debug!("mk_borrowck_eval_cx: {:?}", instance);
    let param_env = tcx.param_env(instance.def_id());
    let mut ecx = EvalContext::new(tcx.at(span), param_env, CompileTimeEvaluator, ());
    // insert a stack frame so any queries have the correct substs
    ecx.stack.push(interpret::Frame {
        block: mir::START_BLOCK,
        locals: IndexVec::new(),
        instance,
        span,
        mir,
        return_place: Place::null(tcx),
        return_to_block: StackPopCleanup::Goto(None), // never pop
        stmt: 0,
    });
    Ok(ecx)
}

pub fn mk_eval_cx<'a, 'tcx>(
    tcx: TyCtxt<'a, 'tcx, 'tcx>,
    instance: Instance<'tcx>,
    param_env: ty::ParamEnv<'tcx>,
) -> EvalResult<'tcx, EvalContext<'a, 'tcx, 'tcx, CompileTimeEvaluator>> {
    debug!("mk_eval_cx: {:?}, {:?}", instance, param_env);
    let span = tcx.def_span(instance.def_id());
    let mut ecx = EvalContext::new(tcx.at(span), param_env, CompileTimeEvaluator, ());
    let mir = ecx.load_mir(instance.def)?;
    // insert a stack frame so any queries have the correct substs
    ecx.push_stack_frame(
        instance,
        mir.span,
        mir,
        Place::null(tcx),
        StackPopCleanup::Goto(None), // never pop
    )?;
    Ok(ecx)
}

pub fn eval_promoted<'a, 'mir, 'tcx>(
    ecx: &mut EvalContext<'a, 'mir, 'tcx, CompileTimeEvaluator>,
    cid: GlobalId<'tcx>,
    mir: &'mir mir::Mir<'tcx>,
    param_env: ty::ParamEnv<'tcx>,
) -> EvalResult<'tcx, OpTy<'tcx>> {
    ecx.with_fresh_body(|ecx| {
        eval_body_using_ecx(ecx, cid, Some(mir), param_env)
    })
}

pub fn op_to_const<'tcx>(
    ecx: &EvalContext<'_, '_, 'tcx, CompileTimeEvaluator>,
    op: OpTy<'tcx>,
    normalize: bool,
) -> EvalResult<'tcx, &'tcx ty::Const<'tcx>> {
    let normalized_op = if normalize {
        ecx.try_read_value(op)?
    } else {
        match op.op {
            Operand::Indirect(mplace) => Err(mplace),
            Operand::Immediate(val) => Ok(val)
        }
    };
    let val = match normalized_op {
        Err(MemPlace { ptr, align, extra }) => {
            // extract alloc-offset pair
            assert!(extra.is_none());
            let ptr = ptr.to_ptr()?;
            let alloc = ecx.memory.get(ptr.alloc_id)?;
            assert!(alloc.align.abi() >= align.abi());
            assert!(alloc.bytes.len() as u64 - ptr.offset.bytes() >= op.layout.size.bytes());
            let mut alloc = alloc.clone();
            alloc.align = align;
            // FIXME shouldnt it be the case that `mark_static_initialized` has already
            // interned this?  I thought that is the entire point of that `FinishStatic` stuff?
            let alloc = ecx.tcx.intern_const_alloc(alloc);
            ConstValue::ByRef(ptr.alloc_id, alloc, ptr.offset)
        },
        Ok(Value::Scalar(x)) =>
            ConstValue::Scalar(x.not_undef()?),
        Ok(Value::ScalarPair(a, b)) =>
            ConstValue::ScalarPair(a.not_undef()?, b),
    };
    Ok(ty::Const::from_const_value(ecx.tcx.tcx, val, op.layout.ty))
}

fn eval_body_and_ecx<'a, 'mir, 'tcx>(
    tcx: TyCtxt<'a, 'tcx, 'tcx>,
    cid: GlobalId<'tcx>,
    mir: Option<&'mir mir::Mir<'tcx>>,
    param_env: ty::ParamEnv<'tcx>,
) -> (EvalResult<'tcx, OpTy<'tcx>>, EvalContext<'a, 'mir, 'tcx, CompileTimeEvaluator>) {
    // we start out with the best span we have
    // and try improving it down the road when more information is available
    let span = tcx.def_span(cid.instance.def_id());
    let span = mir.map(|mir| mir.span).unwrap_or(span);
    let mut ecx = EvalContext::new(tcx.at(span), param_env, CompileTimeEvaluator, ());
    let r = eval_body_using_ecx(&mut ecx, cid, mir, param_env);
    (r, ecx)
}

// Returns a pointer to where the result lives
fn eval_body_using_ecx<'a, 'mir, 'tcx>(
    ecx: &mut EvalContext<'a, 'mir, 'tcx, CompileTimeEvaluator>,
    cid: GlobalId<'tcx>,
    mir: Option<&'mir mir::Mir<'tcx>>,
    param_env: ty::ParamEnv<'tcx>,
) -> EvalResult<'tcx, OpTy<'tcx>> {
    debug!("eval_body_using_ecx: {:?}, {:?}", cid, param_env);
    let tcx = ecx.tcx.tcx;
    let mut mir = match mir {
        Some(mir) => mir,
        None => ecx.load_mir(cid.instance.def)?,
    };
    if let Some(index) = cid.promoted {
        mir = &mir.promoted[index];
    }
    let layout = ecx.layout_of(mir.return_ty().subst(tcx, cid.instance.substs))?;
    assert!(!layout.is_unsized());
    let ret = ecx.allocate(layout, MemoryKind::Stack)?;

    let name = ty::tls::with(|tcx| tcx.item_path_str(cid.instance.def_id()));
    let prom = cid.promoted.map_or(String::new(), |p| format!("::promoted[{:?}]", p));
    trace!("eval_body_using_ecx: pushing stack frame for global: {}{}", name, prom);
    assert!(mir.arg_count == 0);
    ecx.push_stack_frame(
        cid.instance,
        mir.span,
        mir,
        Place::Ptr(*ret),
        StackPopCleanup::None { cleanup: false },
    )?;

    // The main interpreter loop.
    ecx.run()?;

    // Intern the result
    let internally_mutable = !layout.ty.is_freeze(tcx, param_env, mir.span);
    let is_static = tcx.is_static(cid.instance.def_id());
    let mutability = if is_static == Some(hir::Mutability::MutMutable) || internally_mutable {
        Mutability::Mutable
    } else {
        Mutability::Immutable
    };
    ecx.memory.intern_static(ret.ptr.to_ptr()?.alloc_id, mutability)?;

    debug!("eval_body_using_ecx done: {:?}", *ret);
    Ok(ret.into())
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct CompileTimeEvaluator;

impl<'tcx> Into<EvalError<'tcx>> for ConstEvalError {
    fn into(self) -> EvalError<'tcx> {
        EvalErrorKind::MachineError(self.to_string()).into()
    }
}

impl_stable_hash_for!(struct CompileTimeEvaluator {});

#[derive(Clone, Debug)]
enum ConstEvalError {
    NeedsRfc(String),
    NotConst(String),
}

impl fmt::Display for ConstEvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ConstEvalError::*;
        match *self {
            NeedsRfc(ref msg) => {
                write!(
                    f,
                    "\"{}\" needs an rfc before being allowed inside constants",
                    msg
                )
            }
            NotConst(ref msg) => write!(f, "{}", msg),
        }
    }
}

impl Error for ConstEvalError {
    fn description(&self) -> &str {
        use self::ConstEvalError::*;
        match *self {
            NeedsRfc(_) => "this feature needs an rfc before being allowed inside constants",
            NotConst(_) => "this feature is not compatible with constant evaluation",
        }
    }

    fn cause(&self) -> Option<&dyn Error> {
        None
    }
}

impl<'mir, 'tcx> interpret::Machine<'mir, 'tcx> for CompileTimeEvaluator {
    type MemoryData = ();
    type MemoryKinds = !;

    const MUT_STATIC_KIND: Option<!> = None; // no mutating of statics allowed

    fn find_fn<'a>(
        ecx: &mut EvalContext<'a, 'mir, 'tcx, Self>,
        instance: ty::Instance<'tcx>,
        args: &[OpTy<'tcx>],
        dest: Option<PlaceTy<'tcx>>,
        ret: Option<mir::BasicBlock>,
    ) -> EvalResult<'tcx, Option<&'mir mir::Mir<'tcx>>> {
        debug!("eval_fn_call: {:?}", instance);
        if !ecx.tcx.is_const_fn(instance.def_id()) {
            // Some functions we support even if they are non-const -- but avoid testing
            // that for const fn!
            if ecx.hook_fn(instance, args, dest)? {
                ecx.goto_block(ret)?; // fully evaluated and done
                return Ok(None);
            }
            return Err(
                ConstEvalError::NotConst(format!("calling non-const fn `{}`", instance)).into(),
            );
        }
        // This is a const fn. Call it.
        Ok(Some(match ecx.load_mir(instance.def) {
            Ok(mir) => mir,
            Err(err) => {
                if let EvalErrorKind::NoMirFor(ref path) = err.kind {
                    return Err(
                        ConstEvalError::NeedsRfc(format!("calling extern function `{}`", path))
                            .into(),
                    );
                }
                return Err(err);
            }
        }))
    }

    fn call_intrinsic<'a>(
        ecx: &mut EvalContext<'a, 'mir, 'tcx, Self>,
        instance: ty::Instance<'tcx>,
        args: &[OpTy<'tcx>],
        dest: PlaceTy<'tcx>,
    ) -> EvalResult<'tcx> {
        if ecx.emulate_intrinsic(instance, args, dest)? {
            return Ok(());
        }
        // An intrinsic that we do not support
        let intrinsic_name = &ecx.tcx.item_name(instance.def_id()).as_str()[..];
        Err(
            ConstEvalError::NeedsRfc(format!("calling intrinsic `{}`", intrinsic_name)).into()
        )
    }

    fn ptr_op<'a>(
        _ecx: &EvalContext<'a, 'mir, 'tcx, Self>,
        _bin_op: mir::BinOp,
        _left: Scalar,
        _left_layout: TyLayout<'tcx>,
        _right: Scalar,
        _right_layout: TyLayout<'tcx>,
    ) -> EvalResult<'tcx, (Scalar, bool)> {
        Err(
            ConstEvalError::NeedsRfc("pointer arithmetic or comparison".to_string()).into(),
        )
    }

    fn find_foreign_static<'a>(
        _tcx: TyCtxtAt<'a, 'tcx, 'tcx>,
        _def_id: DefId,
    ) -> EvalResult<'tcx, &'tcx Allocation> {
        err!(ReadForeignStatic)
    }

    fn box_alloc<'a>(
        _ecx: &mut EvalContext<'a, 'mir, 'tcx, Self>,
        _dest: PlaceTy<'tcx>,
    ) -> EvalResult<'tcx> {
        Err(
            ConstEvalError::NeedsRfc("heap allocations via `box` keyword".to_string()).into(),
        )
    }
}

/// Project to a field of a (variant of a) const
pub fn const_field<'a, 'tcx>(
    tcx: TyCtxt<'a, 'tcx, 'tcx>,
    param_env: ty::ParamEnv<'tcx>,
    instance: ty::Instance<'tcx>,
    variant: Option<usize>,
    field: mir::Field,
    value: &'tcx ty::Const<'tcx>,
) -> ::rustc::mir::interpret::ConstEvalResult<'tcx> {
    trace!("const_field: {:?}, {:?}, {:?}", instance, field, value);
    let ecx = mk_eval_cx(tcx, instance, param_env).unwrap();
    let result = (|| {
        // get the operand again
        let op = ecx.const_to_op(value)?;
        // downcast
        let down = match variant {
            None => op,
            Some(variant) => ecx.operand_downcast(op, variant)?
        };
        // then project
        let field = ecx.operand_field(down, field.index() as u64)?;
        // and finally move back to the const world, always normalizing because
        // this is not called for statics.
        op_to_const(&ecx, field, true)
    })();
    result.map_err(|error| {
        let stacktrace = ecx.generate_stacktrace(None);
        let err = ::rustc::mir::interpret::ConstEvalErr { error, stacktrace, span: ecx.tcx.span };
        err.report_as_error(ecx.tcx, "could not access field of constant");
        ErrorHandled::Reported
    })
}

pub fn const_variant_index<'a, 'tcx>(
    tcx: TyCtxt<'a, 'tcx, 'tcx>,
    param_env: ty::ParamEnv<'tcx>,
    instance: ty::Instance<'tcx>,
    val: &'tcx ty::Const<'tcx>,
) -> EvalResult<'tcx, usize> {
    trace!("const_variant_index: {:?}, {:?}", instance, val);
    let ecx = mk_eval_cx(tcx, instance, param_env).unwrap();
    let op = ecx.const_to_op(val)?;
    Ok(ecx.read_discriminant(op)?.1)
}

fn validate_const<'a, 'tcx>(
    tcx: ty::TyCtxt<'a, 'tcx, 'tcx>,
    constant: &'tcx ty::Const<'tcx>,
    key: ty::ParamEnvAnd<'tcx, GlobalId<'tcx>>,
) -> ::rustc::mir::interpret::ConstEvalResult<'tcx> {
    let cid = key.value;
    let ecx = mk_eval_cx(tcx, cid.instance, key.param_env).unwrap();
    let val = (|| {
        let op = ecx.const_to_op(constant)?;
        let mut todo = vec![(op, Vec::new())];
        let mut seen = FxHashSet();
        seen.insert(op);
        while let Some((op, mut path)) = todo.pop() {
            ecx.validate_operand(
                op,
                &mut path,
                &mut seen,
                &mut todo,
            )?;
        }
        Ok(constant)
    })();

    val.map_err(|error| {
        let stacktrace = ecx.generate_stacktrace(None);
        let err = ::rustc::mir::interpret::ConstEvalErr { error, stacktrace, span: ecx.tcx.span };
        match err.struct_error(ecx.tcx, "it is undefined behavior to use this value") {
            Ok(mut diag) => {
                diag.note("The rules on what exactly is undefined behavior aren't clear, \
                    so this check might be overzealous. Please open an issue on the rust compiler \
                    repository if you believe it should not be considered undefined behavior",
                );
                diag.emit();
                ErrorHandled::Reported
            }
            Err(err) => err,
        }
    })
}

pub fn const_eval_provider<'a, 'tcx>(
    tcx: TyCtxt<'a, 'tcx, 'tcx>,
    key: ty::ParamEnvAnd<'tcx, GlobalId<'tcx>>,
) -> ::rustc::mir::interpret::ConstEvalResult<'tcx> {
    tcx.const_eval_raw(key).and_then(|val| {
        validate_const(tcx, val, key)
    })
}

pub fn const_eval_raw_provider<'a, 'tcx>(
    tcx: TyCtxt<'a, 'tcx, 'tcx>,
    key: ty::ParamEnvAnd<'tcx, GlobalId<'tcx>>,
) -> ::rustc::mir::interpret::ConstEvalResult<'tcx> {
    trace!("const eval: {:?}", key);
    let cid = key.value;
    let def_id = cid.instance.def.def_id();

    if let Some(id) = tcx.hir.as_local_node_id(def_id) {
        let tables = tcx.typeck_tables_of(def_id);

        // Do match-check before building MIR
        if let Err(ErrorReported) = tcx.check_match(def_id) {
            return Err(ErrorHandled::Reported)
        }

        if let hir::BodyOwnerKind::Const = tcx.hir.body_owner_kind(id) {
            tcx.mir_const_qualif(def_id);
        }

        // Do not continue into miri if typeck errors occurred; it will fail horribly
        if tables.tainted_by_errors {
            return Err(ErrorHandled::Reported)
        }
    };

    let (res, ecx) = eval_body_and_ecx(tcx, cid, None, key.param_env);
    res.and_then(|op| {
        let normalize = tcx.is_static(def_id).is_none() && cid.promoted.is_none();
        if !normalize {
            // Sanity check: These must always be a MemPlace
            match op.op {
                Operand::Indirect(_) => { /* all is good */ },
                Operand::Immediate(_) => bug!("const eval gave us an Immediate"),
            }
        }
        op_to_const(&ecx, op, normalize)
    }).map_err(|error| {
        let stacktrace = ecx.generate_stacktrace(None);
        let err = ConstEvalErr { error, stacktrace, span: ecx.tcx.span };
        if tcx.is_static(def_id).is_some() {
            let err = err.report_as_error(ecx.tcx, "could not evaluate static initializer");
            if tcx.sess.err_count() == 0 {
                span_bug!(ecx.tcx.span, "static eval failure didn't emit an error: {:#?}", err);
            }
            err
        } else if def_id.is_local() {
            // constant defined in this crate, we can figure out a lint level!
            match tcx.describe_def(def_id) {
                Some(Def::Const(_)) | Some(Def::AssociatedConst(_)) => {
                    let node_id = tcx.hir.as_local_node_id(def_id).unwrap();
                    err.report_as_lint(
                        tcx.at(tcx.def_span(def_id)),
                        "any use of this value will cause an error",
                        node_id,
                    )
                },
                _ => if let Some(p) = cid.promoted {
                    let span = tcx.optimized_mir(def_id).promoted[p].span;
                    if let EvalErrorKind::ReferencedConstant = err.error.kind {
                        err.report_as_error(
                            tcx.at(span),
                            "evaluation of constant expression failed",
                        )
                    } else {
                        err.report_as_lint(
                            tcx.at(span),
                            "reaching this expression at runtime will panic or abort",
                            tcx.hir.as_local_node_id(def_id).unwrap(),
                        )
                    }
                } else {
                    err.report_as_error(
                        ecx.tcx,
                        "evaluation of constant value failed",
                    )
                },
            }
        } else {
            // use of constant from other crate
            err.report_as_error(ecx.tcx, "could not evaluate constant")
        }
    })
}
