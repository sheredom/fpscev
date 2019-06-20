define i1 @fcmp_oeq_false(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp oeq float %f3, %f
  ret i1 %b
}

define i1 @fcmp_oeq_nan(i4 %i, float %f) {
  %f2 = uitofp i4 %i to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp oeq float %f, %f3
  ret i1 %b
}

define i1 @fcmp_ogt_true(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp ogt float %f3, %f
  ret i1 %b
}

define i1 @fcmp_ogt_false(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp ogt float %f, %f3
  ret i1 %b
}

define i1 @fcmp_ogt_nan(i4 %i, float %f) {
  %f2 = uitofp i4 %i to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp ogt float %f, %f3
  ret i1 %b
}

define i1 @fcmp_oge_true(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 15.0
  %b = fcmp oge float %f3, %f
  ret i1 %b
}

define i1 @fcmp_oge_false(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp oge float %f, %f3
  ret i1 %b
}

define i1 @fcmp_oge_nan(i4 %i, float %f) {
  %f2 = uitofp i4 %i to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp oge float %f, %f3
  ret i1 %b
}

define i1 @fcmp_olt_true(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp olt float %f, %f3
  ret i1 %b
}

define i1 @fcmp_olt_false(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp olt float %f3, %f
  ret i1 %b
}

define i1 @fcmp_olt_nan(i4 %i, float %f) {
  %f2 = uitofp i4 %i to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp olt float %f, %f3
  ret i1 %b
}

define i1 @fcmp_ole_true(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 15.0
  %b = fcmp ole float %f, %f3
  ret i1 %b
}

define i1 @fcmp_ole_false(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp ole float %f3, %f
  ret i1 %b
}

define i1 @fcmp_ole_nan(i4 %i, float %f) {
  %f2 = uitofp i4 %i to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp ole float %f, %f3
  ret i1 %b
}

define i1 @fcmp_one_true(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp one float %f3, %f
  ret i1 %b
}

define i1 @fcmp_one_nan(i4 %i, float %f) {
  %f2 = uitofp i4 %i to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp one float %f, %f3
  ret i1 %b
}

define i1 @fcmp_ord_true(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp ord float %f3, %f
  ret i1 %b
}

define i1 @fcmp_ord_nan(i4 %i, float %f) {
  %f2 = uitofp i4 %i to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp ord float %f, %f3
  ret i1 %b
}

define i1 @fcmp_uno_false(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp uno float %f3, %f
  ret i1 %b
}

define i1 @fcmp_uno_nan(i4 %i, float %f) {
  %f2 = uitofp i4 %i to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp uno float %f, %f3
  ret i1 %b
}

define i1 @fcmp_ueq_false(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp ueq float %f3, %f
  ret i1 %b
}

define i1 @fcmp_ugt_true(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp ugt float %f3, %f
  ret i1 %b
}

define i1 @fcmp_ugt_false(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp ugt float %f, %f3
  ret i1 %b
}

define i1 @fcmp_uge_true(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 15.0
  %b = fcmp uge float %f3, %f
  ret i1 %b
}

define i1 @fcmp_uge_false(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp uge float %f, %f3
  ret i1 %b
}

define i1 @fcmp_ult_true(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp ult float %f, %f3
  ret i1 %b
}

define i1 @fcmp_ult_false(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp ult float %f3, %f
  ret i1 %b
}

define i1 @fcmp_ule_true(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 15.0
  %b = fcmp ule float %f, %f3
  ret i1 %b
}

define i1 @fcmp_ule_false(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp ule float %f3, %f
  ret i1 %b
}

define i1 @fcmp_une_true(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %b = fcmp une float %f3, %f
  ret i1 %b
}

define float @sqrt_all_negative(float %f) {
  %f2 = call float @llvm.copysign.f32(float %f, float -1.0)
  %f3 = call float @llvm.sqrt.f32(float %f2)
  ret float %f3
}

define float @sqrt_all_negative_nnan(i4 %i) {
  %f = uitofp i4 %i to float
  %f2 = fneg float %f
  %f3 = call nnan float @llvm.sqrt.f32(float %f2)
  ret float %f3
}

define float @pow(float %f, i25 %i2) {
  %f2 = sitofp i25 %i2 to float
  %f3 = call float @llvm.pow.f32(float %f, float %f2)
  ret float %f3
}

define float @pow_too_large(float %f, i25 %i2) {
  %f2 = uitofp i25 %i2 to float
  %f3 = call float @llvm.pow.f32(float %f, float %f2)
  ret float %f3
}

define float @pow_fmf(float %f, i25 %i2) {
  %f2 = sitofp i25 %i2 to float
  %f3 = call nsz float @llvm.pow.f32(float %f, float %f2)
  ret float %f3
}

define float @fabs_already_positive(float %f) {
  %f2 = call float @llvm.copysign.f32(float %f, float 1.0)
  %f3 = call float @llvm.fabs.f32(float %f2)
  ret float %f3
}

define float @minnum_x(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %f4 = call float @llvm.minnum.f32(float %f, float %f3)
  ret float %f4
}

define float @minnum_y(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %f4 = call float @llvm.minnum.f32(float %f3, float %f)
  ret float %f4
}

define float @maxnum_x(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %f4 = call float @llvm.maxnum.f32(float %f3, float %f)
  ret float %f4
}

define float @maxnum_y(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %f4 = call float @llvm.maxnum.f32(float %f, float %f3)
  ret float %f4
}

define float @minimum_x(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %f4 = call float @llvm.minimum.f32(float %f, float %f3)
  ret float %f4
}

define float @minimum_y(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %f4 = call float @llvm.minimum.f32(float %f3, float %f)
  ret float %f4
}

define float @maximum_x(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %f4 = call float @llvm.maximum.f32(float %f3, float %f)
  ret float %f4
}

define float @maximum_y(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f2, 16.0
  %f4 = call float @llvm.maximum.f32(float %f, float %f3)
  ret float %f4
}

define float @copysign_both_positive(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = call float @llvm.copysign.f32(float %f, float %f2)
  ret float %f3
}

define float @copysign_both_negative(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fneg float %f
  %f4 = fneg float %f2
  %f5 = call float @llvm.copysign.f32(float %f3, float %f4)
  ret float %f5
}

define float @copysign_both_opposite_0(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fneg float %f
  %f4 = call float @llvm.copysign.f32(float %f2, float %f3)
  ret float %f4
}

define float @copysign_both_opposite_1(i4 %i, i4 %i2) {
  %f = uitofp i4 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fneg float %f
  %f4 = call float @llvm.copysign.f32(float %f3, float %f2)
  ret float %f4
}

define float @copysign_y_positive(float %f, i4 %i) {
  %f2 = uitofp i4 %i to float
  %f3 = call float @llvm.copysign.f32(float %f, float %f2)
  ret float %f3
}

define float @floor_already_integer(i4 %i) {
  %f = uitofp i4 %i to float
  %f2 = call float @llvm.floor.f32(float %f)
  ret float %f2
}

define float @ceil_already_integer(i4 %i) {
  %f = uitofp i4 %i to float
  %f2 = call float @llvm.ceil.f32(float %f)
  ret float %f2
}

define float @trunc_already_integer(i4 %i) {
  %f = uitofp i4 %i to float
  %f2 = call float @llvm.trunc.f32(float %f)
  ret float %f2
}

define float @rint_already_integer(i4 %i) {
  %f = uitofp i4 %i to float
  %f2 = call float @llvm.rint.f32(float %f)
  ret float %f2
}

define float @nearbyint_already_integer(i4 %i) {
  %f = uitofp i4 %i to float
  %f2 = call float @llvm.nearbyint.f32(float %f)
  ret float %f2
}

define float @round_already_integer(i4 %i) {
  %f = uitofp i4 %i to float
  %f2 = call float @llvm.round.f32(float %f)
  ret float %f2
}

declare float @some_called_func()
declare float @llvm.sqrt.f32(float %f)
declare float @llvm.pow.f32(float %f, float %f2)
declare float @llvm.fabs.f32(float %f)
declare float @llvm.minnum.f32(float %f, float %f2)
declare float @llvm.maxnum.f32(float %f, float %f2)
declare float @llvm.minimum.f32(float %f, float %f2)
declare float @llvm.maximum.f32(float %f, float %f2)
declare float @llvm.copysign.f32(float %f, float %f2)
declare float @llvm.floor.f32(float %f)
declare float @llvm.ceil.f32(float %f)
declare float @llvm.trunc.f32(float %f)
declare float @llvm.rint.f32(float %f)
declare float @llvm.nearbyint.f32(float %f)
declare float @llvm.round.f32(float %f)
