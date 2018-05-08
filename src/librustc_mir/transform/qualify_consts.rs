// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! A pass that qualifies constness of temporaries in constants,
//! static initializers and functions and also drives promotion.
//!
//! The Qualif flags below can be used to also provide better
//! diagnostics as to why a constant rvalue wasn't promoted.

use rustc_data_structures::bitvec::BitVector;
use rustc_data_structures::indexed_set::IdxSetBuf;
use rustc_data_structures::indexed_vec::{IndexVec, Idx};
use rustc_data_structures::fx::FxHashSet;
use rustc::hir;
use rustc::hir::def_id::DefId;
use rustc::middle::const_val::ConstVal;
use rustc::traits::{self, TraitEngine};
use rustc::ty::{self, TyCtxt, Ty, TypeFoldable};
use rustc::ty::cast::CastTy;
use rustc::ty::maps::Providers;
use rustc::mir::*;
use rustc::mir::traversal::ReversePostorder;
use rustc::mir::visit::{PlaceContext, Visitor};
use rustc::middle::lang_items;
use rustc_target::spec::abi::Abi;
use syntax::attr;
use syntax::ast::LitKind;
use syntax::feature_gate::UnstableFeatures;
use syntax_pos::{Span, DUMMY_SP};

use std::fmt;
use rustc_data_structures::sync::Lrc;

use transform::{MirPass, MirSource};
use super::promote_consts::{self, Candidate, TempState};

pub const MUT_INTERIOR_BITS: u8 = 1 << 0;
pub const NEEDS_DROP_BITS: u8 = 1 << 1;
pub const NOT_CONST_BITS: u8 = 1 << 2;

fn qualif_bits((mut_interior, needs_drop, not_const): (bool, bool, bool)) -> u8 {
    let mut bits = 0;
    if mut_interior {
        bits |= MUT_INTERIOR_BITS;
    }
    if needs_drop {
        bits |= NEEDS_DROP_BITS;
    }
    if not_const {
        bits |= NOT_CONST_BITS;
    }
    bits
}

/// What kind of item we are in.
#[derive(Copy, Clone, PartialEq, Eq)]
enum Mode {
    Const,
    Static,
    StaticMut,
    Fn
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Mode::Const => write!(f, "constant"),
            Mode::Static | Mode::StaticMut => write!(f, "static"),
            Mode::Fn => write!(f, "function")
        }
    }
}

struct MayHaveMutInterior<'a, 'gcx: 'a+'tcx, 'tcx: 'a> {
    span: Span,
    mir: &'a Mir<'tcx>,
    tcx: TyCtxt<'a, 'gcx, 'tcx>,
    param_env: ty::ParamEnv<'tcx>,
    local_mut_interior: IdxSetBuf<Local>,
    mut_interior: bool
}

struct MayNeedDrop<'a, 'gcx: 'a+'tcx, 'tcx: 'a> {
    span: Span,
    mir: &'a Mir<'tcx>,
    tcx: TyCtxt<'a, 'gcx, 'tcx>,
    param_env: ty::ParamEnv<'tcx>,
    local_needs_drop: IdxSetBuf<Local>,
    needs_drop: bool
}

struct ConstChecker<'a, 'gcx: 'a+'tcx, 'tcx: 'a> {
    mode: Mode,
    span: Span,
    def_id: DefId,
    mir: &'a Mir<'tcx>,
    rpo: ReversePostorder<'a, 'tcx>,
    tcx: TyCtxt<'a, 'gcx, 'tcx>,
    param_env: ty::ParamEnv<'tcx>,
    local_not_const: IdxSetBuf<Local>,
    not_const: bool,
    may_have_mut_interior: MayHaveMutInterior<'a, 'gcx, 'tcx>,
    may_need_drop: MayNeedDrop<'a, 'gcx, 'tcx>,
    temp_promotion_state: IndexVec<Local, TempState>,
    promotion_candidates: Vec<Candidate>
}

impl<'a, 'tcx> MayHaveMutInterior<'a, 'tcx, 'tcx> {
    fn new(tcx: TyCtxt<'a, 'tcx, 'tcx>,
           def_id: DefId,
           mir: &'a Mir<'tcx>)
           -> MayHaveMutInterior<'a, 'tcx, 'tcx> {
        let param_env = tcx.param_env(def_id);

        MayHaveMutInterior {
            span: mir.span,
            mir,
            tcx,
            param_env,
            local_mut_interior: IdxSetBuf::new_empty(mir.local_decls.len()),
            mut_interior: false
        }
    }

    /// Add the given type's qualification to `self.mut_interior`.
    fn add_type(&mut self, ty: Ty<'tcx>) {
        self.mut_interior = !ty.is_freeze(self.tcx, self.param_env, DUMMY_SP);
    }

    /// Within the provided closure, `self.mut_interior` will start
    /// out false, and its value after the closure returns will
    /// be combined with the value before the call to nest.
    fn nest<F: FnOnce(&mut Self)>(&mut self, f: F) {
        let original = self.mut_interior;
        self.mut_interior = false;
        f(self);
        self.mut_interior |= original;
    }

    /// Assign the current qualification to the given destination.
    fn assign(&mut self, dest: &Place<'tcx>, _: Location) {
        if let Place::Local(index) = *dest {
            debug!("store to {:?} {:?}", self.mir.local_kind(index), index);
            self.local_mut_interior.set_member(&index, self.mut_interior);
        }
    }
}

impl<'a, 'tcx> MayNeedDrop<'a, 'tcx, 'tcx> {
    fn new(tcx: TyCtxt<'a, 'tcx, 'tcx>,
           def_id: DefId,
           mir: &'a Mir<'tcx>)
           -> MayNeedDrop<'a, 'tcx, 'tcx> {
        let param_env = tcx.param_env(def_id);

        let mut local_needs_drop = IdxSetBuf::new_empty(mir.local_decls.len());
        for arg in mir.args_iter() {
            if mir.local_decls[arg].ty.needs_drop(tcx, param_env) {
                local_needs_drop.add(&arg);
            }
        }

        MayNeedDrop {
            span: mir.span,
            mir,
            tcx,
            param_env,
            local_needs_drop,
            needs_drop: false
        }
    }

    /// Add the given type's qualification to `self.needs_drop`.
    fn add_type(&mut self, ty: Ty<'tcx>) {
        self.needs_drop = ty.needs_drop(self.tcx, self.param_env);
    }

    /// Within the provided closure, `self.needs_drop` will start
    /// out false, and its value after the closure returns will
    /// be combined with the value before the call to nest.
    fn nest<F: FnOnce(&mut Self)>(&mut self, f: F) {
        let original = self.needs_drop;
        self.needs_drop = false;
        f(self);
        self.needs_drop |= original;
    }

    /// Assign the current qualification to the given destination.
    fn assign(&mut self, dest: &Place<'tcx>, _: Location) {
        if let Place::Local(index) = *dest {
            debug!("store to {:?} {:?}", self.mir.local_kind(index), index);
            self.local_needs_drop.set_member(&index, self.needs_drop);
        }
    }
}

impl<'a, 'tcx> ConstChecker<'a, 'tcx, 'tcx> {
    fn new(tcx: TyCtxt<'a, 'tcx, 'tcx>,
           def_id: DefId,
           mir: &'a Mir<'tcx>,
           mode: Mode)
           -> ConstChecker<'a, 'tcx, 'tcx> {
        let mut rpo = traversal::reverse_postorder(mir);
        let temps = promote_consts::collect_temps(mir, &mut rpo);
        rpo.reset();

        let param_env = tcx.param_env(def_id);

        let mut local_not_const = IdxSetBuf::new_filled(mir.local_decls.len());
        for arg in mir.args_iter() {
            local_not_const.remove(&arg);
        }

        let may_have_mut_interior = MayHaveMutInterior::new(
            tcx,
            def_id,
            mir);
        let may_need_drop = MayNeedDrop::new(
            tcx,
            def_id,
            mir);

        ConstChecker {
            mode,
            span: mir.span,
            def_id,
            mir,
            rpo,
            tcx,
            param_env,
            local_not_const,
            not_const: false,
            may_have_mut_interior,
            may_need_drop,
            temp_promotion_state: temps,
            promotion_candidates: Vec::new()
        }
    }

    /// Returns whether all qualification flags are unset.
    fn is_qualif_empty(&self) -> bool {
        !(self.not_const ||
          self.may_have_mut_interior.mut_interior ||
          self.may_need_drop.needs_drop)
    }

    // FIXME(eddyb) we could split the errors into meaningful
    // categories, but enabling full miri would make that
    // slightly pointless (even with feature-gating).
    fn not_const(&mut self) {
        self.not_const = true;
        if self.mode != Mode::Fn {
            let mut err = struct_span_err!(
                self.tcx.sess,
                self.span,
                E0019,
                "{} contains unimplemented expression type",
                self.mode
            );
            if self.tcx.sess.teach(&err.get_code().unwrap()) {
                err.note("A function call isn't allowed in the const's initialization expression \
                          because the expression's value must be known at compile-time.");
                err.note("Remember: you can't use a function call inside a const's initialization \
                          expression! However, you can use it anywhere else.");
            }
            err.emit();
        }
    }

    /// Error about extra statements in a constant.
    fn statement_like(&mut self) {
        self.not_const = true;
        if self.mode != Mode::Fn {
            let mut err = struct_span_err!(
                self.tcx.sess,
                self.span,
                E0016,
                "blocks in {}s are limited to items and tail expressions",
                self.mode
            );
            if self.tcx.sess.teach(&err.get_code().unwrap()) {
                err.note("Blocks in constants may only contain items (such as constant, function \
                          definition, etc...) and a tail expression.");
                err.help("To avoid it, you have to replace the non-item object.");
            }
            err.emit();
        }
    }

    /// Within the provided closure, `self.not_const` will start
    /// out false, and its value after the closure returns will
    /// be combined with the value before the call to `nest`.
    fn nest<F: FnOnce(&mut Self)>(&mut self, f: F) {
        let original = self.not_const;
        self.not_const = false;
        f(self);
        self.not_const |= original;
    }

    /// Assign the current qualification to the given destination.
    fn assign(&mut self, dest: &Place<'tcx>, location: Location) {
        // Only handle promotable temps in non-const functions.
        if self.mode == Mode::Fn {
            if let Place::Local(index) = *dest {
                if self.mir.local_kind(index) == LocalKind::Temp
                && self.temp_promotion_state[index].is_promotable() {
                    debug!("store to promotable temp {:?}", index);
                    self.local_not_const.set_member(&index, self.not_const);
                }
            }
            return;
        }

        match *dest {
            Place::Local(index) if self.mir.local_kind(index) == LocalKind::Temp => {
                debug!("store to temp {:?}", index);
                self.local_not_const.set_member(&index, self.not_const);
            }
            Place::Local(index) if self.mir.local_kind(index) == LocalKind::ReturnPointer => {
                debug!("store to return place {:?}", index);
                self.local_not_const.set_member(&index, self.not_const);
            }

            Place::Projection(box Projection {
                base: Place::Local(index),
                elem: ProjectionElem::Deref
            }) if self.mir.local_kind(index) == LocalKind::Temp
               && self.mir.local_decls[index].ty.is_box()
               && self.local_not_const.contains(&index) => {
                // Part of `box expr`, we should've errored
                // already for the Box allocation Rvalue.
            }

            // This must be an explicit assignment.
            _ => {
                // Catch more errors in the destination.
                self.visit_place(dest, PlaceContext::Store, location);
                self.statement_like();
            }
        }
    }

    /// Qualify a whole const, static initializer or const fn.
    fn qualify_const(&mut self) -> ((bool, bool, bool), Lrc<IdxSetBuf<Local>>) {
        debug!("qualifying {} {:?}", self.mode, self.def_id);

        let mir = self.mir;

        let mut seen_blocks = BitVector::new(mir.basic_blocks().len());
        let mut bb = START_BLOCK;
        loop {
            seen_blocks.insert(bb.index());

            self.visit_basic_block_data(bb, &mir[bb]);

            let target = match mir[bb].terminator().kind {
                TerminatorKind::Goto { target } |
                TerminatorKind::Drop { target, .. } |
                TerminatorKind::Assert { target, .. } |
                TerminatorKind::Call { destination: Some((_, target)), .. } => {
                    Some(target)
                }

                // Non-terminating calls cannot produce any value.
                TerminatorKind::Call { destination: None, .. } => {
                    break;
                }

                TerminatorKind::SwitchInt {..} |
                TerminatorKind::DropAndReplace { .. } |
                TerminatorKind::Resume |
                TerminatorKind::Abort |
                TerminatorKind::GeneratorDrop |
                TerminatorKind::Yield { .. } |
                TerminatorKind::Unreachable |
                TerminatorKind::FalseEdges { .. } |
                TerminatorKind::FalseUnwind { .. } => None,

                TerminatorKind::Return => {
                    break;
                }
            };

            match target {
                Some(target) => {
                    bb = target;
                }
                _ => {
                    self.not_const();
                    break;
                }
            }
        }

        self.may_have_mut_interior.mut_interior =
            self.may_have_mut_interior.local_mut_interior.contains(&RETURN_PLACE);
        self.may_need_drop.needs_drop =
            self.may_need_drop.local_needs_drop.contains(&RETURN_PLACE);
        self.not_const =
            self.local_not_const.contains(&RETURN_PLACE);

        // Collect all the temps we need to promote.
        let mut promoted_temps = IdxSetBuf::new_empty(self.temp_promotion_state.len());

        for candidate in &self.promotion_candidates {
            match *candidate {
                Candidate::Ref(Location { block: bb, statement_index: stmt_idx }) => {
                    match self.mir[bb].statements[stmt_idx].kind {
                        StatementKind::Assign(_, Rvalue::Ref(_, _, Place::Local(index))) => {
                            promoted_temps.add(&index);
                        }
                        _ => {}
                    }
                }
                Candidate::Argument { .. } => {}
            }
        }

        let qualif = (self.may_have_mut_interior.mut_interior,
                      self.may_need_drop.needs_drop,
                      self.not_const);
        (qualif, Lrc::new(promoted_temps))
    }
}

/// Accumulates an Rvalue or a Call's effects into `self.mut_interior`.
impl<'a, 'tcx> Visitor<'tcx> for MayHaveMutInterior<'a, 'tcx, 'tcx> {
    fn visit_local(&mut self,
                   &local: &Local,
                   _: PlaceContext<'tcx>,
                   _: Location) {
        let kind = self.mir.local_kind(local);
        match kind {
            LocalKind::ReturnPointer |
            LocalKind::Var => {}

            LocalKind::Arg |
            LocalKind::Temp => {
                self.mut_interior |= self.local_mut_interior.contains(&local);
            }
        }
    }

    fn visit_place(&mut self,
                   place: &Place<'tcx>,
                   context: PlaceContext<'tcx>,
                   location: Location) {
        match *place {
            Place::Local(ref local) => self.visit_local(local, context, location),
            Place::Static(..) => {},
            Place::Projection(ref proj) => {
                self.nest(|this| {
                    this.super_place(place, context, location);
                    match proj.elem {
                        ProjectionElem::Deref => {}

                        ProjectionElem::Field(..) |
                        ProjectionElem::Index(_) => {
                            let ty = place.ty(this.mir, this.tcx).to_ty(this.tcx);
                            this.add_type(ty);
                        }

                        ProjectionElem::ConstantIndex {..} |
                        ProjectionElem::Subslice {..} |
                        ProjectionElem::Downcast(..) => {}
                    }
                });
            }
        }
    }

    fn visit_operand(&mut self, operand: &Operand<'tcx>, location: Location) {
        match *operand {
            Operand::Copy(_) |
            Operand::Move(_) => {
                self.nest(|this| {
                    this.super_operand(operand, location);
                });
            }

            Operand::Constant(ref constant) => {
                if let Literal::Value {
                    value: &ty::Const { val: ConstVal::Unevaluated(def_id, _), ty }
                } = constant.literal {
                    // Don't peek inside trait-associated constants.
                    if self.tcx.trait_of_item(def_id).is_some() {
                        self.add_type(ty);
                    } else {
                        let (bits, _) = self.tcx.at(constant.span).mir_const_qualif(def_id);
                        self.mut_interior |= bits & MUT_INTERIOR_BITS != 0;

                        // Just in case the type is more specific than
                        // the definition, e.g. impl associated const
                        // with type parameters, take it into account.
                        self.add_type(ty);
                    }
                }
            }
        }
    }

    fn visit_rvalue(&mut self, rvalue: &Rvalue<'tcx>, location: Location) {
        // Recurse through operands and places.
        if let Rvalue::Ref(region, kind, ref place) = *rvalue {
            let mut is_reborrow = false;
            if let Place::Projection(ref proj) = *place {
                if let ProjectionElem::Deref = proj.elem {
                    let base_ty = proj.base.ty(self.mir, self.tcx).to_ty(self.tcx);
                    if let ty::TyRef(..) = base_ty.sty {
                        is_reborrow = true;
                    }
                }
            }

            if is_reborrow {
                self.nest(|this| {
                    this.super_place(place, PlaceContext::Borrow {
                        region,
                        kind
                    }, location);
                });
            } else {
                self.super_rvalue(rvalue, location);
            }
        } else {
            self.super_rvalue(rvalue, location);
        }
    }

    fn visit_terminator_kind(&mut self,
                             bb: BasicBlock,
                             kind: &TerminatorKind<'tcx>,
                             location: Location) {
        if let TerminatorKind::Call { ref func, ref destination, .. } = *kind {
            self.visit_operand(func, location);

            if let Some((ref dest, _)) = *destination {
                // Be conservative about the returned value of a const fn.
                let ty = dest.ty(self.mir, self.tcx).to_ty(self.tcx);
                self.add_type(ty);
                self.assign(dest, location);
            }
        } else if let TerminatorKind::Drop { .. } = *kind {
            self.super_terminator_kind(bb, kind, location);
        } else {
            // Qualify any operands inside other terminators.
            self.super_terminator_kind(bb, kind, location);
        }
    }

    fn visit_assign(&mut self,
                    _: BasicBlock,
                    dest: &Place<'tcx>,
                    rvalue: &Rvalue<'tcx>,
                    location: Location) {
        self.visit_rvalue(rvalue, location);
        self.assign(dest, location);
    }

    fn visit_source_info(&mut self, source_info: &SourceInfo) {
        self.span = source_info.span;
    }

    fn visit_statement(&mut self, bb: BasicBlock, statement: &Statement<'tcx>, location: Location) {
        self.nest(|this| {
            this.visit_source_info(&statement.source_info);
            match statement.kind {
                StatementKind::Assign(ref place, ref rvalue) => {
                    this.visit_assign(bb, place, rvalue, location);
                }
                StatementKind::SetDiscriminant { .. } |
                StatementKind::StorageLive(_) |
                StatementKind::StorageDead(_) |
                StatementKind::InlineAsm {..} |
                StatementKind::EndRegion(_) |
                StatementKind::Validate(..) |
                StatementKind::UserAssertTy(..) |
                StatementKind::Nop => {}
            }
        });
    }

    fn visit_terminator(&mut self,
                        bb: BasicBlock,
                        terminator: &Terminator<'tcx>,
                        location: Location) {
        self.nest(|this| this.super_terminator(bb, terminator, location));
    }
}

/// Accumulates an Rvalue or a Call's effects into `self.needs_drop`.
impl<'a, 'tcx> Visitor<'tcx> for MayNeedDrop<'a, 'tcx, 'tcx> {
    fn visit_local(&mut self,
                   &local: &Local,
                   _: PlaceContext<'tcx>,
                   _: Location) {
        let kind = self.mir.local_kind(local);
        match kind {
            LocalKind::ReturnPointer => {}
            LocalKind::Var => {}

            LocalKind::Arg |
            LocalKind::Temp => {
                self.needs_drop |= self.local_needs_drop.contains(&local);
            }
        }
    }

    fn visit_place(&mut self,
                   place: &Place<'tcx>,
                   context: PlaceContext<'tcx>,
                   location: Location) {
        match *place {
            Place::Local(ref local) => self.visit_local(local, context, location),
            Place::Static(..) => {}

            Place::Projection(ref proj) => {
                self.nest(|this| {
                    this.super_place(place, context, location);
                    match proj.elem {
                        ProjectionElem::Deref => {}

                        ProjectionElem::Field(..) |
                        ProjectionElem::Index(_) => {
                            let ty = place.ty(this.mir, this.tcx).to_ty(this.tcx);
                            this.add_type(ty);
                        }

                        ProjectionElem::ConstantIndex {..} |
                        ProjectionElem::Subslice {..} |
                        ProjectionElem::Downcast(..) => {}
                    }
                });
            }
        }
    }

    fn visit_operand(&mut self, operand: &Operand<'tcx>, location: Location) {
        match *operand {
            Operand::Copy(_) |
            Operand::Move(_) => {
                self.nest(|this| {
                    this.super_operand(operand, location);
                });

                // Mark the consumed locals to indicate later drops are noops.
                if let Operand::Move(Place::Local(local)) = *operand {
                    self.local_needs_drop.remove(&local);
                }
            }

            Operand::Constant(ref constant) => {
                if let Literal::Value {
                    value: &ty::Const { val: ConstVal::Unevaluated(def_id, _), ty }
                } = constant.literal {
                    // Don't peek inside trait-associated constants.
                    if self.tcx.trait_of_item(def_id).is_some() {
                        self.add_type(ty);
                    } else {
                        let (bits, _) = self.tcx.at(constant.span).mir_const_qualif(def_id);
                        self.needs_drop |= bits & NEEDS_DROP_BITS != 0;

                        // Just in case the type is more specific than
                        // the definition, e.g. impl associated const
                        // with type parameters, take it into account.
                        self.add_type(ty);
                    }
                }
            }
        }
    }

    fn visit_rvalue(&mut self, rvalue: &Rvalue<'tcx>, location: Location) {
        // Recurse through operands and places.
        if let Rvalue::Ref(region, kind, ref place) = *rvalue {
            let mut is_reborrow = false;
            if let Place::Projection(ref proj) = *place {
                if let ProjectionElem::Deref = proj.elem {
                    let base_ty = proj.base.ty(self.mir, self.tcx).to_ty(self.tcx);
                    if let ty::TyRef(..) = base_ty.sty {
                        is_reborrow = true;
                    }
                }
            }

            if is_reborrow {
                self.nest(|this| {
                    this.super_place(place, PlaceContext::Borrow {
                        region,
                        kind
                    }, location);
                });
            } else {
                self.super_rvalue(rvalue, location);
            }
        } else {
            self.super_rvalue(rvalue, location);
        }

        match *rvalue {
            Rvalue::Use(_) |
            Rvalue::Repeat(..) |
            Rvalue::UnaryOp(UnOp::Neg, _) |
            Rvalue::UnaryOp(UnOp::Not, _) |
            Rvalue::NullaryOp(NullOp::SizeOf, _) |
            Rvalue::CheckedBinaryOp(..) |
            Rvalue::Cast(CastKind::ReifyFnPointer, ..) |
            Rvalue::Cast(CastKind::UnsafeFnPointer, ..) |
            Rvalue::Cast(CastKind::ClosureFnPointer, ..) |
            Rvalue::Cast(CastKind::Unsize, ..) |
            Rvalue::Discriminant(..) |
            Rvalue::Len(_) |
            Rvalue::Ref(..) |
            Rvalue::Cast(CastKind::Misc, ..) |
            Rvalue::BinaryOp(..) |
            Rvalue::NullaryOp(NullOp::Box, _) => {}

            Rvalue::Aggregate(ref kind, _) => {
                if let AggregateKind::Adt(def, ..) = **kind {
                    if def.has_dtor(self.tcx) {
                        self.needs_drop = true;
                    }

                    if Some(def.did) == self.tcx.lang_items().unsafe_cell_type() {
                        let ty = rvalue.ty(self.mir, self.tcx);
                        self.add_type(ty);
                    }
                }
            }
        }
    }

    fn visit_terminator_kind(&mut self,
                             bb: BasicBlock,
                             kind: &TerminatorKind<'tcx>,
                             location: Location) {
        if let TerminatorKind::Call { ref func, ref destination, .. } = *kind {
            self.visit_operand(func, location);

            if let Some((ref dest, _)) = *destination {
                // Be conservative about the returned value of a const fn.
                let ty = dest.ty(self.mir, self.tcx).to_ty(self.tcx);
                self.add_type(ty);
                self.assign(dest, location);
            }
        } else if let TerminatorKind::Drop { .. } = *kind {
            self.super_terminator_kind(bb, kind, location);
        } else {
            // Qualify any operands inside other terminators.
            self.super_terminator_kind(bb, kind, location);
        }
    }

    fn visit_assign(&mut self,
                    _: BasicBlock,
                    dest: &Place<'tcx>,
                    rvalue: &Rvalue<'tcx>,
                    location: Location) {
        self.visit_rvalue(rvalue, location);
        self.assign(dest, location);
    }

    fn visit_source_info(&mut self, source_info: &SourceInfo) {
        self.span = source_info.span;
    }

    fn visit_statement(&mut self, bb: BasicBlock, statement: &Statement<'tcx>, location: Location) {
        self.nest(|this| {
            this.visit_source_info(&statement.source_info);
            match statement.kind {
                StatementKind::Assign(ref place, ref rvalue) => {
                    this.visit_assign(bb, place, rvalue, location);
                }
                StatementKind::SetDiscriminant { .. } |
                StatementKind::StorageLive(_) |
                StatementKind::StorageDead(_) |
                StatementKind::InlineAsm {..} |
                StatementKind::EndRegion(_) |
                StatementKind::Validate(..) |
                StatementKind::UserAssertTy(..) |
                StatementKind::Nop => {}
            }
        });
    }

    fn visit_terminator(&mut self,
                        bb: BasicBlock,
                        terminator: &Terminator<'tcx>,
                        location: Location) {
        self.nest(|this| this.super_terminator(bb, terminator, location));
    }
}

/// Accumulates an Rvalue or Call's effects in `self.not_const`.
/// For functions (constant or not), it also records
/// candidates for promotion in promotion_candidates.
impl<'a, 'tcx> Visitor<'tcx> for ConstChecker<'a, 'tcx, 'tcx> {
    fn visit_local(&mut self,
                   &local: &Local,
                   context: PlaceContext<'tcx>,
                   location: Location) {
        self.may_have_mut_interior.visit_local(&local, context, location);
        self.may_need_drop.visit_local(&local, context, location);

        let kind = self.mir.local_kind(local);
        match kind {
            LocalKind::ReturnPointer => {
                self.not_const();
            }
            LocalKind::Var => {
                self.not_const = true;
            }
            LocalKind::Arg |
            LocalKind::Temp => {
                if !self.temp_promotion_state[local].is_promotable() {
                    self.not_const = true;
                }

                self.not_const |= self.local_not_const.contains(&local);
            }
        }
    }

    fn visit_place(&mut self,
                   place: &Place<'tcx>,
                   context: PlaceContext<'tcx>,
                   location: Location) {
        self.may_have_mut_interior.visit_place(place, context, location);
        self.may_need_drop.visit_place(place, context, location);

        match *place {
            Place::Local(ref local) => self.visit_local(local, context, location),
            Place::Static(ref global) => {
                if self.mode == Mode::Fn {
                    self.not_const = true;
                }

                if self.mode != Mode::Fn {
                    for attr in &self.tcx.get_attrs(global.def_id)[..] {
                        if attr.check_name("thread_local") {
                            span_err!(self.tcx.sess, self.span, E0625,
                                      "thread-local statics cannot be \
                                       accessed at compile-time");
                            self.not_const = true;
                            return;
                        }
                    }
                }
            }
            Place::Projection(ref proj) => {
                self.nest(|this| {
                    this.super_place(place, context, location);
                    match proj.elem {
                        ProjectionElem::Deref => {
                            this.not_const = true;

                            let base_ty = proj.base.ty(this.mir, this.tcx).to_ty(this.tcx);
                            if let ty::TyRawPtr(_) = base_ty.sty {
                                if this.mode != Mode::Fn {
                                    let mut err = struct_span_err!(
                                        this.tcx.sess,
                                        this.span,
                                        E0396,
                                        "raw pointers cannot be dereferenced in {}s",
                                        this.mode
                                    );
                                    err.span_label(this.span,
                                                   "dereference of raw pointer in constant");
                                    if this.tcx.sess.teach(&err.get_code().unwrap()) {
                                        err.note(
                                            "The value behind a raw pointer can't be determined \
                                             at compile-time (or even link-time), which means it \
                                             can't be used in a constant expression."
                                        );
                                        err.help("A possible fix is to dereference your pointer \
                                                  at some point in run-time.");
                                    }
                                    err.emit();
                                }
                            }
                        }

                        ProjectionElem::Field(..) |
                        ProjectionElem::Index(_) => {}

                        ProjectionElem::ConstantIndex {..} |
                        ProjectionElem::Subslice {..} |
                        ProjectionElem::Downcast(..) => {
                            this.not_const()
                        }
                    }
                });
            }
        }
    }

    fn visit_operand(&mut self, operand: &Operand<'tcx>, location: Location) {
        self.may_have_mut_interior.visit_operand(operand, location);
        self.may_need_drop.visit_operand(operand, location);

        match *operand {
            Operand::Copy(_) |
            Operand::Move(_) => {
                self.nest(|this| {
                    this.super_operand(operand, location);
                });
            }
            Operand::Constant(ref constant) => {
                if let Literal::Value {
                    value: &ty::Const { val: ConstVal::Unevaluated(def_id, _), .. }
                } = constant.literal {
                    // Don't peek inside trait-associated constants.
                    if self.tcx.trait_of_item(def_id).is_none() {
                        let (bits, _) = self.tcx.at(constant.span).mir_const_qualif(def_id);
                        self.not_const |= bits & NOT_CONST_BITS != 0;
                    }
                }
            }
        }
    }

    fn visit_rvalue(&mut self, rvalue: &Rvalue<'tcx>, location: Location) {
        self.may_have_mut_interior.visit_rvalue(rvalue, location);
        self.may_need_drop.visit_rvalue(rvalue, location);

        // Recurse through operands and places.
        if let Rvalue::Ref(region, kind, ref place) = *rvalue {
            let mut is_reborrow = false;
            if let Place::Projection(ref proj) = *place {
                if let ProjectionElem::Deref = proj.elem {
                    let base_ty = proj.base.ty(self.mir, self.tcx).to_ty(self.tcx);
                    if let ty::TyRef(..) = base_ty.sty {
                        is_reborrow = true;
                    }
                }
            }

            if is_reborrow {
                self.nest(|this| {
                    this.super_place(place, PlaceContext::Borrow {
                        region,
                        kind
                    }, location);
                });
            } else {
                self.super_rvalue(rvalue, location);
            }
        } else {
            self.super_rvalue(rvalue, location);
        }

        match *rvalue {
            Rvalue::Use(_) |
            Rvalue::Repeat(..) |
            Rvalue::UnaryOp(UnOp::Neg, _) |
            Rvalue::UnaryOp(UnOp::Not, _) |
            Rvalue::NullaryOp(NullOp::SizeOf, _) |
            Rvalue::CheckedBinaryOp(..) |
            Rvalue::Cast(CastKind::ReifyFnPointer, ..) |
            Rvalue::Cast(CastKind::UnsafeFnPointer, ..) |
            Rvalue::Cast(CastKind::ClosureFnPointer, ..) |
            Rvalue::Cast(CastKind::Unsize, ..) |
            Rvalue::Discriminant(..) => {}

            Rvalue::Len(_) => {}

            Rvalue::Ref(_, kind, ref place) => {
                let ty = place.ty(self.mir, self.tcx).to_ty(self.tcx);
                if let BorrowKind::Mut { .. } = kind {
                    // In theory, any zero-sized value could be borrowed
                    // mutably without consequences. However, only &mut []
                    // is allowed right now, and only in functions.
                    let allow = if self.mode == Mode::StaticMut {
                        // Inside a `static mut`, &mut [...] is also allowed.
                        match ty.sty {
                            ty::TyArray(..) | ty::TySlice(_) => true,
                            _ => false
                        }
                    } else if let ty::TyArray(_, len) = ty.sty {
                        len.val.unwrap_u64() == 0 && self.mode == Mode::Fn
                    } else {
                        false
                    };

                    if !allow {
                        self.not_const = true;
                        if self.mode != Mode::Fn {
                            let mut err = struct_span_err!(self.tcx.sess,  self.span, E0017,
                                                           "references in {}s may only refer \
                                                            to immutable values", self.mode);
                            err.span_label(self.span, format!("{}s require immutable values",
                                                                self.mode));
                            if self.tcx.sess.teach(&err.get_code().unwrap()) {
                                err.note("References in statics and constants may only refer to \
                                          immutable values.\n\n\
                                          Statics are shared everywhere, and if they refer to \
                                          mutable data one might violate memory safety since \
                                          holding multiple mutable references to shared data is \
                                          not allowed.\n\n\
                                          If you really want global mutable state, try using \
                                          static mut or a global UnsafeCell.");
                            }
                            err.emit();
                        }
                    }
                } else {
                    // Constants cannot be borrowed if they contain interior mutability as
                    // it means that our "silent insertion of statics" could change
                    // initializer values (very bad).
                    if self.may_have_mut_interior.mut_interior {
                        // Replace MUTABLE_INTERIOR with NOT_CONST to avoid
                        // duplicate errors (from reborrowing, for example).
                        self.may_have_mut_interior.mut_interior = false;
                        self.not_const = true;
                        if self.mode != Mode::Fn {
                            span_err!(self.tcx.sess, self.span, E0492,
                                      "cannot borrow a constant which may contain \
                                       interior mutability, create a static instead");
                        }
                    }
                }

                // We might have a candidate for promotion.
                let candidate = Candidate::Ref(location);
                if self.is_qualif_empty() {
                    // We can only promote direct borrows of temps.
                    if let Place::Local(local) = *place {
                        if self.mir.local_kind(local) == LocalKind::Temp {
                            self.promotion_candidates.push(candidate);
                        }
                    }
                }
            }

            Rvalue::Cast(CastKind::Misc, ref operand, cast_ty) => {
                let operand_ty = operand.ty(self.mir, self.tcx);
                let cast_in = CastTy::from_ty(operand_ty).expect("bad input type for cast");
                let cast_out = CastTy::from_ty(cast_ty).expect("bad output type for cast");
                match (cast_in, cast_out) {
                    (CastTy::Ptr(_), CastTy::Int(_)) |
                    (CastTy::FnPtr, CastTy::Int(_)) => {
                        self.not_const = true;
                        if self.mode != Mode::Fn {
                            let mut err = struct_span_err!(
                                self.tcx.sess,
                                self.span,
                                E0018,
                                "raw pointers cannot be cast to integers in {}s",
                                self.mode
                            );
                            if self.tcx.sess.teach(&err.get_code().unwrap()) {
                                err.note("\
The value of static and constant integers must be known at compile time. You can't cast a pointer \
to an integer because the address of a pointer can vary.

For example, if you write:

```
static MY_STATIC: u32 = 42;
static MY_STATIC_ADDR: usize = &MY_STATIC as *const _ as usize;
static WHAT: usize = (MY_STATIC_ADDR^17) + MY_STATIC_ADDR;
```

Then `MY_STATIC_ADDR` would contain the address of `MY_STATIC`. However, the address can change \
when the program is linked, as well as change between different executions due to ASLR, and many \
linkers would not be able to calculate the value of `WHAT`.

On the other hand, static and constant pointers can point either to a known numeric address or to \
the address of a symbol.

```
static MY_STATIC: u32 = 42;
static MY_STATIC_ADDR: &'static u32 = &MY_STATIC;
const CONST_ADDR: *const u8 = 0x5f3759df as *const u8;
```

This does not pose a problem by itself because they can't be accessed directly.");
                            }
                            err.emit();
                        }
                    }
                    _ => {}
                }
            }

            Rvalue::BinaryOp(op, ref lhs, _) => {
                if let ty::TyRawPtr(_) = lhs.ty(self.mir, self.tcx).sty {
                    assert!(op == BinOp::Eq || op == BinOp::Ne ||
                            op == BinOp::Le || op == BinOp::Lt ||
                            op == BinOp::Ge || op == BinOp::Gt ||
                            op == BinOp::Offset);

                    self.not_const = true;
                    if self.mode != Mode::Fn {
                        struct_span_err!(
                            self.tcx.sess, self.span, E0395,
                            "raw pointers cannot be compared in {}s",
                            self.mode)
                        .span_label(
                            self.span,
                            "comparing raw pointers in static")
                        .emit();
                    }
                }
            }

            Rvalue::NullaryOp(NullOp::Box, _) => {
                self.not_const = true;
                if self.mode != Mode::Fn {
                    let mut err = struct_span_err!(self.tcx.sess, self.span, E0010,
                                                   "allocations are not allowed in {}s", self.mode);
                    err.span_label(self.span, format!("allocation not allowed in {}s", self.mode));
                    if self.tcx.sess.teach(&err.get_code().unwrap()) {
                        err.note(
                            "The value of statics and constants must be known at compile time, \
                             and they live for the entire lifetime of a program. Creating a boxed \
                             value allocates memory on the heap at runtime, and therefore cannot \
                             be done at compile time."
                        );
                    }
                    err.emit();
                }
            }

            Rvalue::Aggregate(..) => {}
        }
    }

    fn visit_terminator_kind(&mut self,
                             bb: BasicBlock,
                             kind: &TerminatorKind<'tcx>,
                             location: Location) {
        self.may_have_mut_interior.visit_terminator_kind(bb, kind, location);
        self.may_need_drop.visit_terminator_kind(bb, kind, location);

        if let TerminatorKind::Call { ref func, ref destination, ref args, .. } = *kind {
            self.visit_operand(func, location);

            let fn_ty = func.ty(self.mir, self.tcx);
            let mut callee_def_id = None;
            let (mut is_shuffle, mut is_const_fn) = (false, None);
            if let ty::TyFnDef(def_id, _) = fn_ty.sty {
                callee_def_id = Some(def_id);
                match self.tcx.fn_sig(def_id).abi() {
                    Abi::RustIntrinsic |
                    Abi::PlatformIntrinsic => {
                        assert!(!self.tcx.is_const_fn(def_id));
                        match &self.tcx.item_name(def_id).as_str()[..] {
                            "size_of" | "min_align_of" | "type_id" => is_const_fn = Some(def_id),

                            name if name.starts_with("simd_shuffle") => {
                                is_shuffle = true;
                            }

                            _ => {}
                        }
                    }
                    _ => {
                        if self.tcx.is_const_fn(def_id) {
                            is_const_fn = Some(def_id);
                        }
                    }
                }
            }

            let constant_arguments = callee_def_id.and_then(|id| {
                args_required_const(self.tcx, id)
            });
            for (i, arg) in args.iter().enumerate() {
                self.nest(|this| {
                    this.visit_operand(arg, location);
                    if this.mode != Mode::Fn {
                        return
                    }
                    let candidate = Candidate::Argument { bb, index: i };
                    if is_shuffle && i == 2 {
                        if this.is_qualif_empty() {
                            this.promotion_candidates.push(candidate);
                        } else {
                            span_err!(this.tcx.sess, this.span, E0526,
                                      "shuffle indices are not constant");
                        }
                        return
                    }

                    let constant_arguments = match constant_arguments.as_ref() {
                        Some(s) => s,
                        None => return,
                    };
                    if !constant_arguments.contains(&i) {
                        return
                    }
                    if this.is_qualif_empty() {
                        this.promotion_candidates.push(candidate);
                    } else {
                        this.tcx.sess.span_err(this.span,
                            &format!("argument {} is required to be a constant",
                                     i + 1));
                    }
                });
            }

            // Const fn calls.
            if let Some(def_id) = is_const_fn {
                // find corresponding rustc_const_unstable feature
                if let Some(&attr::Stability {
                    rustc_const_unstable: Some(attr::RustcConstUnstable {
                        feature: ref feature_name
                    }),
                .. }) = self.tcx.lookup_stability(def_id) {

                    // We are in a const or static initializer,
                    if self.mode != Mode::Fn &&

                        // feature-gate is not enabled,
                        !self.tcx.features()
                            .declared_lib_features
                            .iter()
                            .any(|&(ref sym, _)| sym == feature_name) &&

                        // this doesn't come from a crate with the feature-gate enabled,
                        self.def_id.is_local() &&

                        // this doesn't come from a macro that has #[allow_internal_unstable]
                        !self.span.allows_unstable()
                    {
                        let mut err = self.tcx.sess.struct_span_err(self.span,
                            &format!("`{}` is not yet stable as a const fn",
                                     self.tcx.item_path_str(def_id)));
                        help!(&mut err,
                              "in Nightly builds, add `#![feature({})]` \
                               to the crate attributes to enable",
                              feature_name);
                        err.emit();
                    }
                }
            } else {
                self.may_have_mut_interior.mut_interior = false;
                self.may_need_drop.needs_drop = false;
                self.not_const = true;
                if self.mode != Mode::Fn && self.mode != Mode::Const {
                    // FIXME(#24111) Remove this check when const fn stabilizes
                    let (msg, note) = if let UnstableFeatures::Disallow =
                            self.tcx.sess.opts.unstable_features {
                        (format!("calls in {}s are limited to \
                                  tuple structs and tuple variants",
                                 self.mode),
                         Some("a limited form of compile-time function \
                               evaluation is available on a nightly \
                               compiler via `const fn`"))
                    } else {
                        (format!("calls in {}s are limited \
                                  to constant functions, \
                                  tuple structs and tuple variants",
                                 self.mode),
                         None)
                    };
                    let mut err = struct_span_err!(self.tcx.sess, self.span, E0015, "{}", msg);
                    if let Some(note) = note {
                        err.span_note(self.span, note);
                    }
                    err.emit();
                }
            }

            if let Some((ref dest, _)) = *destination {
                self.assign(dest, location);
            }
        } else if let TerminatorKind::Drop { location: ref place, .. } = *kind {
            self.super_terminator_kind(bb, kind, location);

            // Deny *any* live drops anywhere other than functions.
            if self.mode != Mode::Fn {
                // HACK(eddyb) Emulate a bit of dataflow analysis,
                // conservatively, that drop elaboration will do.
                let needs_drop = if let Place::Local(local) = *place {
                    if self.may_need_drop.local_needs_drop.contains(&local) {
                        Some(self.mir.local_decls[local].source_info.span)
                    } else {
                        None
                    }
                } else {
                    Some(self.span)
                };

                if let Some(span) = needs_drop {
                    // Double-check the type being dropped, to minimize false positives.
                    let ty = place.ty(self.mir, self.tcx).to_ty(self.tcx);
                    if ty.needs_drop(self.tcx, self.param_env) {
                        struct_span_err!(self.tcx.sess, span, E0493,
                                         "destructors cannot be evaluated at compile-time")
                            .span_label(span, format!("{}s cannot evaluate destructors",
                                                      self.mode))
                            .emit();
                    }
                }
            }
        } else {
            // Qualify any operands inside other terminators.
            self.super_terminator_kind(bb, kind, location);
        }
    }

    fn visit_assign(&mut self,
                    bb: BasicBlock,
                    dest: &Place<'tcx>,
                    rvalue: &Rvalue<'tcx>,
                    location: Location) {
        self.may_have_mut_interior.visit_assign(bb, dest, rvalue, location);
        self.may_need_drop.visit_assign(bb, dest, rvalue, location);

        self.visit_rvalue(rvalue, location);
        self.assign(dest, location);
    }

    fn visit_source_info(&mut self, source_info: &SourceInfo) {
        self.may_have_mut_interior.visit_source_info(source_info);
        self.may_need_drop.visit_source_info(source_info);

        self.span = source_info.span;
    }

    fn visit_statement(&mut self, bb: BasicBlock, statement: &Statement<'tcx>, location: Location) {
        self.may_have_mut_interior.visit_statement(bb, statement, location);
        self.may_need_drop.visit_statement(bb, statement, location);

        self.nest(|this| {
            this.visit_source_info(&statement.source_info);
            match statement.kind {
                StatementKind::Assign(ref place, ref rvalue) => {
                    this.visit_assign(bb, place, rvalue, location);
                }
                StatementKind::SetDiscriminant { .. } |
                StatementKind::StorageLive(_) |
                StatementKind::StorageDead(_) |
                StatementKind::InlineAsm {..} |
                StatementKind::EndRegion(_) |
                StatementKind::Validate(..) |
                StatementKind::UserAssertTy(..) |
                StatementKind::Nop => {}
            }
        });
    }

    fn visit_terminator(&mut self,
                        bb: BasicBlock,
                        terminator: &Terminator<'tcx>,
                        location: Location) {
        self.may_have_mut_interior.visit_terminator(bb, terminator, location);
        self.may_need_drop.visit_terminator(bb, terminator, location);

        self.nest(|this| this.super_terminator(bb, terminator, location));
    }
}

pub fn provide(providers: &mut Providers) {
    *providers = Providers {
        mir_const_qualif,
        ..*providers
    };
}

fn mir_const_qualif<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>,
                              def_id: DefId)
                              -> (u8, Lrc<IdxSetBuf<Local>>) {
    // NB: This `borrow()` is guaranteed to be valid (i.e., the value
    // cannot yet be stolen), because `mir_validated()`, which steals
    // from `mir_const(), forces this query to execute before
    // performing the steal.
    let mir = &tcx.mir_const(def_id).borrow();

    if mir.return_ty().references_error() {
        tcx.sess.delay_span_bug(mir.span, "mir_const_qualif: Mir had errors");
        return (qualif_bits((false, false, true)), Lrc::new(IdxSetBuf::new_empty(0)));
    }

    let mut checker = ConstChecker::new(tcx, def_id, mir, Mode::Const);
    let (qualif, promoted_temps) = checker.qualify_const();
    (qualif_bits(qualif), promoted_temps)
}

pub struct QualifyAndPromoteConstants;

impl MirPass for QualifyAndPromoteConstants {
    fn run_pass<'a, 'tcx>(&self,
                          tcx: TyCtxt<'a, 'tcx, 'tcx>,
                          src: MirSource,
                          mir: &mut Mir<'tcx>) {
        // There's not really any point in promoting errorful MIR.
        if mir.return_ty().references_error() {
            tcx.sess.delay_span_bug(mir.span, "QualifyAndPromoteConstants: Mir had errors");
            return;
        }

        if src.promoted.is_some() {
            return;
        }

        let def_id = src.def_id;
        let id = tcx.hir.as_local_node_id(def_id).unwrap();
        let mut const_promoted_temps = None;
        let mode = match tcx.hir.body_owner_kind(id) {
            hir::BodyOwnerKind::Fn => {
                Mode::Fn
            }
            hir::BodyOwnerKind::Const => {
                const_promoted_temps = Some(tcx.mir_const_qualif(def_id).1);
                Mode::Const
            }
            hir::BodyOwnerKind::Static(hir::MutImmutable) => Mode::Static,
            hir::BodyOwnerKind::Static(hir::MutMutable) => Mode::StaticMut,
        };

        if mode == Mode::Fn {
            // This is ugly because `ConstChecker` holds onto mir,
            // which can't be mutated until its scope ends.
            let (temps, candidates) = {
                let mut checker = ConstChecker::new(tcx, def_id, mir, mode);
                while let Some((bb, data)) = checker.rpo.next() {
                    checker.visit_basic_block_data(bb, data);
                }

                (checker.temp_promotion_state, checker.promotion_candidates)
            };

            // Do the actual promotion, now that we know what's viable.
            promote_consts::promote_candidates(mir, tcx, temps, candidates);
        } else {
            let promoted_temps = if mode == Mode::Const {
                // Already computed by `mir_const_qualif`.
                const_promoted_temps.unwrap()
            } else {
                ConstChecker::new(tcx, def_id, mir, mode).qualify_const().1
            };

            // In `const` and `static` everything without `StorageDead`
            // is `'static`, we don't have to create promoted MIR fragments,
            // just remove `Drop` and `StorageDead` on "promoted" locals.
            for block in mir.basic_blocks_mut() {
                block.statements.retain(|statement| {
                    match statement.kind {
                        StatementKind::StorageDead(index) => {
                            !promoted_temps.contains(&index)
                        }
                        _ => true
                    }
                });
                let terminator = block.terminator_mut();
                match terminator.kind {
                    TerminatorKind::Drop { location: Place::Local(index), target, .. } => {
                        if promoted_temps.contains(&index) {
                            terminator.kind = TerminatorKind::Goto {
                                target,
                            };
                        }
                    }
                    _ => {}
                }
            }
        }

        // Statics must be Sync.
        if mode == Mode::Static {
            // `#[thread_local]` statics don't have to be `Sync`.
            for attr in &tcx.get_attrs(def_id)[..] {
                if attr.check_name("thread_local") {
                    return;
                }
            }
            let ty = mir.return_ty();
            tcx.infer_ctxt().enter(|infcx| {
                let param_env = ty::ParamEnv::empty();
                let cause = traits::ObligationCause::new(mir.span, id, traits::SharedStatic);
                let mut fulfillment_cx = traits::FulfillmentContext::new();
                fulfillment_cx.register_bound(&infcx,
                                              param_env,
                                              ty,
                                              tcx.require_lang_item(lang_items::SyncTraitLangItem),
                                              cause);
                if let Err(err) = fulfillment_cx.select_all_or_error(&infcx) {
                    infcx.report_fulfillment_errors(&err, None, false);
                }
            });
        }
    }
}

fn args_required_const(tcx: TyCtxt, def_id: DefId) -> Option<FxHashSet<usize>> {
    let attrs = tcx.get_attrs(def_id);
    let attr = attrs.iter().find(|a| a.check_name("rustc_args_required_const"))?;
    let mut ret = FxHashSet();
    for meta in attr.meta_item_list()? {
        match meta.literal()?.node {
            LitKind::Int(a, _) => { ret.insert(a as usize); }
            _ => return None,
        }
    }
    Some(ret)
}
