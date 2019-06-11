define i1 @fcmp(i9 %i, i7 %i2) {
  %f = uitofp i9 %i to float
  %f2 = uitofp i7 %i2 to float
  %b = fcmp olt float %f, %f2
  ret i1 %b
}

define float @fneg(i8 %i) {
  %f = uitofp i8 %i to float
  %f2 = fneg float %f
  ret float %f2
}

define float @fadd(i9 %i, i7 %i2) {
  %f = uitofp i9 %i to float
  %f2 = uitofp i7 %i2 to float
  %f3 = fadd float %f, %f2
  ret float %f3
}

define float @fsub(i9 %i, i7 %i2) {
  %f = uitofp i9 %i to float
  %f2 = uitofp i7 %i2 to float
  %f3 = fsub float %f, %f2
  ret float %f3
}

define float @fmul(i9 %i, i7 %i2) {
  %f = uitofp i9 %i to float
  %f2 = uitofp i7 %i2 to float
  %f3 = fmul float %f, %f2
  ret float %f3
}

define float @fdiv(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = fdiv float %f, 400000000.0
  ret float %f2
}

define float @sqrt(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = call float @llvm.sqrt.f32(float %f)
  ret float %f2
}

define float @sqrt_input_0_to_1(i8 %i) {
  %f = uitofp i8 %i to float
  %f2 = fdiv float %f, 255.0
  %f3 = call float @llvm.sqrt.f32(float %f2)
  ret float %f3
}

define float @powi(i16 %i, i8 %i2) {
  %f = uitofp i16 %i to float
  %i3 = zext i8 %i2 to i32
  %f2 = call float @llvm.powi.f32(float %f, i32 %i3)
  ret float %f2
}

define float @cos(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = call float @llvm.cos.f32(float %f)
  ret float %f2
}

define float @sin(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = call float @llvm.sin.f32(float %f)
  ret float %f2
}

define float @exp(float %f) {
  %f2 = call float @llvm.exp.f32(float %f)
  ret float %f2
}

define float @exp2(float %f) {
  %f2 = call float @llvm.exp.f32(float %f)
  ret float %f2
}

define float @log(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = call float @llvm.log.f32(float %f)
  ret float %f2
}

define float @log_at_least_1(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = fadd float %f, 1.0
  %f3 = call float @llvm.log.f32(float %f2)
  ret float %f3
}

define float @log10(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = call float @llvm.log10.f32(float %f)
  ret float %f2
}

define float @log10_at_least_1(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = fadd float %f, 1.0
  %f3 = call float @llvm.log10.f32(float %f2)
  ret float %f3
}

define float @log2(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = call float @llvm.log2.f32(float %f)
  ret float %f2
}

define float @log2_at_least_1(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = fadd float %f, 1.0
  %f3 = call float @llvm.log2.f32(float %f2)
  ret float %f3
}

define float @fma(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = fneg float %f
  %f3 = call float @llvm.fma.f32(float %f, float %f2, float 4.0)
  ret float %f3
}

define float @fabs(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = fneg float %f
  %f3 = call float @llvm.fabs.f32(float %f2)
  ret float %f3
}

define float @fabs_small_range(i1 %b) {
  %f = select i1 %b, float -4.0, float 1.5
  %f2 = call float @llvm.fabs.f32(float %f)
  ret float %f2
}

define float @minnum(i9 %i, float %f) {
  %f2 = uitofp i9 %i to float
  %f3 = call float @llvm.minnum.f32(float %f, float %f2)
  ret float %f3
}

define float @minnum_small_range(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = sitofp i9 %i to float
  %f3 = call float @llvm.minnum.f32(float %f, float %f2)
  ret float %f3
}

define float @maxnum(i9 %i, float %f) {
  %f2 = uitofp i9 %i to float
  %f3 = call float @llvm.maxnum.f32(float %f, float %f2)
  ret float %f3
}

define float @maxnum_small_range(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = sitofp i9 %i to float
  %f3 = call float @llvm.maxnum.f32(float %f, float %f2)
  ret float %f3
}

define float @minimum(i9 %i, float %f) {
  %f2 = uitofp i9 %i to float
  %f3 = call float @llvm.minimum.f32(float %f, float %f2)
  ret float %f3
}

define float @minimum_small_range(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = sitofp i9 %i to float
  %f3 = call float @llvm.minimum.f32(float %f, float %f2)
  ret float %f3
}

define float @maximum(i9 %i, float %f) {
  %f2 = uitofp i9 %i to float
  %f3 = call float @llvm.maximum.f32(float %f, float %f2)
  ret float %f3
}

define float @maximum_small_range(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = sitofp i9 %i to float
  %f3 = call float @llvm.maximum.f32(float %f, float %f2)
  ret float %f3
}

define float @copysign(i9 %i, i9 %i2) {
  %f = sitofp i9 %i to float
  %f2 = sitofp i9 %i2 to float
  %f3 = call float @llvm.copysign.f32(float %f, float %f2)
  ret float %f3
}

define float @copysign_x_all_positive(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = sitofp i9 %i to float
  %f3 = call float @llvm.copysign.f32(float %f, float %f2)
  ret float %f3
}

define float @copysign_positive(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = sitofp i9 %i to float
  %f3 = call float @llvm.copysign.f32(float %f2, float %f)
  ret float %f3
}

define float @copysign_negative(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = fneg float %f
  %f3 = sitofp i9 %i to float
  %f4 = call float @llvm.copysign.f32(float %f3, float %f2)
  ret float %f4
}

define float @floor(i32 %i) {
  %f = sitofp i32 %i to float
  %f2 = fdiv float %f, 65535.0
  %f3 = call float @llvm.floor.f32(float %f2)
  ret float %f3
}

define float @ceil(i32 %i) {
  %f = sitofp i32 %i to float
  %f2 = fdiv float %f, 65535.0
  %f3 = call float @llvm.ceil.f32(float %f2)
  ret float %f3
}

define float @trunc(i32 %i) {
  %f = sitofp i32 %i to float
  %f2 = fdiv float %f, 65535.0
  %f3 = call float @llvm.trunc.f32(float %f2)
  ret float %f3
}

define float @rint(i32 %i) {
  %f = sitofp i32 %i to float
  %f2 = fdiv float %f, 65535.0
  %f3 = call float @llvm.rint.f32(float %f2)
  ret float %f3
}

define float @nearbyint(i32 %i) {
  %f = sitofp i32 %i to float
  %f2 = fdiv float %f, 65535.0
  %f3 = call float @llvm.nearbyint.f32(float %f2)
  ret float %f3
}

define float @round(i32 %i) {
  %f = sitofp i32 %i to float
  %f2 = fdiv float %f, 65535.0
  %f3 = call float @llvm.round.f32(float %f2)
  ret float %f3
}

declare float @some_called_func()
declare float @llvm.sqrt.f32(float %f)
declare float @llvm.powi.f32(float %f, i32 %i)
declare float @llvm.cos.f32(float %f)
declare float @llvm.sin.f32(float %f)
declare float @llvm.exp.f32(float %f)
declare float @llvm.exp2.f32(float %f)
declare float @llvm.log.f32(float %f)
declare float @llvm.log10.f32(float %f)
declare float @llvm.log2.f32(float %f)
declare float @llvm.fma.f32(float %f, float %f2, float %f3)
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
