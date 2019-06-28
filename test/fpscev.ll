define float @i8_sitofp(i8 %i) {
  %f = sitofp i8 %i to float
  ret float %f
}

define float @i8_uitofp(i8 %i) {
  %f = uitofp i8 %i to float
  ret float %f
}

define float @i16_sitofp(i16 %i) {
  %f = sitofp i16 %i to float
  ret float %f
}

define float @i16_uitofp(i16 %i) {
  %f = uitofp i16 %i to float
  ret float %f
}

define float @i32_sitofp(i32 %i) {
  %f = sitofp i32 %i to float
  ret float %f
}

define float @i32_uitofp(i32 %i) {
  %f = uitofp i32 %i to float
  ret float %f
}

define float @i64_sitofp(i64 %i) {
  %f = sitofp i64 %i to float
  ret float %f
}

define float @i64_uitofp(i64 %i) {
  %f = uitofp i64 %i to float
  ret float %f
}

define float @float_fptrunc(i8 %i) {
  %d = sitofp i8 %i to double
  %f = fptrunc double %d to float
  ret float %f
}

define half @half_fptrunc(i8 %i) {
  %d = sitofp i8 %i to double
  %h = fptrunc double %d to half
  ret half %h
}

define float @float_fpext(i8 %i) {
  %h = sitofp i8 %i to half
  %f = fpext half %h to float
  ret float %f
}

define double @double_fpext(i8 %i) {
  %h = sitofp i8 %i to half
  %d = fpext half %h to double
  ret double %d
}

define double @select(i1 %b, double %in) {
  %d = select i1 %b, double 0.0, double %in
  ret double %d
}

; Only works on LLVM 9+
; define double @select_nnan(i1 %b, double %in) {
;   %d = select nnan i1 %b, double 0.0, double %in
;   ret double %d
; }
; 
; define double @select_nnan_ninf(i1 %b, double %in) {
;   %d = select nnan ninf i1 %b, double 0.0, double %in
;   ret double %d
; }
; 
; define double @select_nsz(i1 %b) {
;   %d = select nsz i1 %b, double -0.0, double 1.5
;   ret double %d
; }

define float @branch_phi(i1 %b) {
  br i1 %b, label %true, label %false
true:
  br label %merge
false:
  br label %merge
merge:
  %f = phi float [1.0, %true], [42.0, %false]
  ret float %f
}

define float @loop_phi(i1 %b) {
entry:
  br label %loop
loop:
  %f = phi float [0.0, %entry], [%f2, %loop]
  %f2 = fadd float %f, 1.0
  %c = fcmp oge float %f2, 42.0
  br i1 %c, label %loop, label %merge
merge:
  ret float %f
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

define float @fdiv_maybe_nan(float %f) {
  %f2 = fdiv float %f, 400000000.0
  ret float %f2
}

define float @fdiv_denominator_less_than(i9 %i, i7 %i2) {
  %f = uitofp i9 %i to float
  %f2 = uitofp i7 %i2 to float
  %f3 = fdiv float %f2, 127.0
  %f4 = fdiv float %f, %f3
  ret float %f4
}

define float @fdiv_denominator_one_or_greater(i9 %i, i7 %i2) {
  %f = uitofp i9 %i to float
  %f2 = uitofp i7 %i2 to float
  %f3 = fadd float %f2, 1.0
  %f4 = fdiv float %f, %f3
  ret float %f4
}

define float @fdiv_arcp(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = fdiv arcp float %f, 400000000.0
  ret float %f2
}

define float @frem_nnan(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = frem nnan float %f, 400000000.0
  ret float %f2
}

define float @frem_nnan_ninf(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = frem nnan ninf float %f, 400000000.0
  ret float %f2
}

define float @call_nnan() {
  %f = call nnan float @some_called_func()
  ret float %f
}

define float @call_nnan_ninf() {
  %f = call nnan ninf float @some_called_func()
  ret float %f
}

define float @sqrt(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = call float @llvm.sqrt.f32(float %f)
  ret float %f2
}

define float @sqrt_input_negative(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = fneg float %f
  %f3 = call float @llvm.sqrt.f32(float %f2)
  ret float %f3
}

define float @sqrt_input_0_to_1(i8 %i) {
  %f = uitofp i8 %i to float
  %f2 = fdiv float %f, 255.0
  %f3 = call float @llvm.sqrt.f32(float %f2)
  ret float %f3
}

define float @pow(i3 %i, i4 %i2) {
  %f = uitofp i3 %i to float
  %f2 = uitofp i4 %i2 to float
  %f3 = fadd float %f, 1.0
  %f4 = call float @llvm.pow.f32(float %f3, float %f2)
  ret float %f4
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

define float @cos_nnan(float %f) {
  %f2 = call nnan float @llvm.cos.f32(float %f)
  ret float %f2
}

define float @cos_nnan_ninf(float %f) {
  %f2 = call nnan ninf float @llvm.cos.f32(float %f)
  ret float %f2
}

define float @sin(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = call float @llvm.sin.f32(float %f)
  ret float %f2
}

define float @sin_nnan(float %f) {
  %f2 = call nnan float @llvm.sin.f32(float %f)
  ret float %f2
}

define float @sin_nnan_ninf(float %f) {
  %f2 = call nnan ninf float @llvm.sin.f32(float %f)
  ret float %f2
}

define float @exp(i5 %i) {
  %f = sitofp i5 %i to float
  %f2 = call float @llvm.exp.f32(float %f)
  ret float %f2
}

define float @exp2(i5 %i) {
  %f = sitofp i5 %i to float
  %f2 = call float @llvm.exp2.f32(float %f)
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

define float @log_all_negative(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = fneg float %f
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

define float @log10_all_negative(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = fneg float %f
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

define float @log2_all_negative(i9 %i) {
  %f = uitofp i9 %i to float
  %f2 = fneg float %f
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
declare float @llvm.pow.f32(float %f, float %f2)
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
