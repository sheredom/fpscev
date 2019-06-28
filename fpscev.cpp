// This is free and unencumbered software released into the public domain.
//
// Anyone is free to copy, modify, publish, use, compile, sell, or
// distribute this software, either in source code form or as a compiled
// binary, for any purpose, commercial or non-commercial, and by any
// means.
//
// In jurisdictions that recognize copyright laws, the author or authors
// of this software dedicate any and all copyright interest in the
// software to the public domain. We make this dedication for the benefit
// of the public at large and to the detriment of our heirs and
// successors. We intend this dedication to be an overt act of
// relinquishment in perpetuity of all present and future rights to this
// software under copyright law.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
// OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
// ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.
//
// For more information, please refer to <http://unlicense.org/>

#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/ValueMap.h"

using namespace llvm;

static cl::opt<bool> fpscevAnalysis("fpscev-analysis", cl::init(false),
                                    cl::desc("Output analysis from fpscev"));

#define ANALYSIS(x)                                                            \
  {                                                                            \
    if (fpscevAnalysis) {                                                      \
      x;                                                                       \
    }                                                                          \
  }

namespace {
const fltSemantics &getSemanticsForType(Type *const type) {
  if (type->isHalfTy()) {
    return APFloat::IEEEhalf();
  } else if (type->isFloatTy()) {
    return APFloat::IEEEsingle();
  } else if (type->isDoubleTy()) {
    return APFloat::IEEEdouble();
  } else {
    llvm_unreachable("Unknown floating point type");
  }
}

APFloat getMinimum(ArrayRef<APFloat> apFloats) {
  APFloat result = apFloats[0];

  bool allNaN = true;

  for (APFloat apFloat : apFloats.slice(1)) {
    allNaN = allNaN && apFloat.isNaN();

    switch (result.compare(apFloat)) {
    case APFloat::cmpLessThan:
      break;
    case APFloat::cmpEqual:
      break;
    case APFloat::cmpGreaterThan:
      result = apFloat;
      break;
    case APFloat::cmpUnordered:
      // Unordered means that a NaN was involved.
      if (apFloat.isNaN()) {
        result = apFloat;
      }
      break;
    }
  }

  // If the result is a positive NaN, but all inputs were not NaN, it we need to
  // flip the sign to encapsulate the range properly.
  if (result.isNaN() && !result.isNegative() && !allNaN) {
    result.changeSign();
  }

  return result;
}

APFloat getMaximum(ArrayRef<APFloat> apFloats) {
  APFloat result = apFloats[0];

  bool allNaN = true;

  for (APFloat apFloat : apFloats.slice(1)) {
    allNaN = allNaN && apFloat.isNaN();

    switch (result.compare(apFloat)) {
    case APFloat::cmpLessThan:
      result = apFloat;
      break;
    case APFloat::cmpEqual:
      break;
    case APFloat::cmpGreaterThan:
      break;
    case APFloat::cmpUnordered:
      // Unordered means that a NaN was involved.
      if (apFloat.isNaN()) {
        result = apFloat;
      }
      break;
    }
  }

  // If the result is a negative NaN, but all inputs were not NaN, it we need to
  // flip the sign to encapsulate the range properly.
  if (result.isNaN() && result.isNegative() && !allNaN) {
    result.changeSign();
  }

  return result;
}

APFloat applyFastMathFlags(APFloat apFloat, const FastMathFlags flags) {
  // If our float is a NaN, but we can't have NaNs, change the float to an
  // infinity with the correct sign.
  if (apFloat.isNaN() && flags.noNaNs()) {
    apFloat = APFloat::getInf(apFloat.getSemantics(), apFloat.isNegative());
  }

  // If our float is an infinity, but we can't have infinities, change the float
  // to the largest floating point number with the correct sign.
  if (apFloat.isInfinity() && flags.noInfs()) {
    apFloat = APFloat::getLargest(apFloat.getSemantics(), apFloat.isNegative());
  }

  // If our float is -0, but we don't care about the sign of zero, flip the
  // sign.
  if (apFloat.isNegZero() && flags.noSignedZeros()) {
    apFloat.changeSign();
  }

  return apFloat;
}

APFloat getFromInt(const fltSemantics &semantics, const APInt &apint,
                   bool isSigned) {
  APFloat apfloat = APFloat::getNaN(semantics);
  apfloat.convertFromAPInt(apint, isSigned,
                           isSigned && apint.isNegative()
                               ? APFloat::rmTowardNegative
                               : APFloat::rmTowardPositive);
  return apfloat;
}

APFloat getOne(const fltSemantics &semantics) {
  const APInt oneInt(32, 1);
  return getFromInt(semantics, oneInt, false);
}

bool isZero(const APFloat &min, const APFloat &max) {
  return (min.isNegative() && !max.isNegative()) || min.isZero() ||
         max.isZero();
}

bool isInRange(const APFloat &x, const APFloat &min, const APFloat &max) {
  switch (x.compare(min)) {
  case APFloat::cmpLessThan:
    return false;
  case APFloat::cmpEqual:
    break;
  case APFloat::cmpGreaterThan:
    break;
  case APFloat::cmpUnordered:
    return false;
  }

  switch (x.compare(max)) {
  case APFloat::cmpLessThan:
    break;
  case APFloat::cmpEqual:
    break;
  case APFloat::cmpGreaterThan:
    return false;
  case APFloat::cmpUnordered:
    return false;
  }

  return true;
}

bool isInRangeExclusive(const APFloat &x, const APFloat &min,
                        const APFloat &max) {
  switch (x.compare(min)) {
  case APFloat::cmpLessThan:
    return false;
  case APFloat::cmpEqual:
    return false;
  case APFloat::cmpGreaterThan:
    break;
  case APFloat::cmpUnordered:
    return false;
  }

  switch (x.compare(max)) {
  case APFloat::cmpLessThan:
    break;
  case APFloat::cmpEqual:
    return false;
  case APFloat::cmpGreaterThan:
    return false;
  case APFloat::cmpUnordered:
    return false;
  }

  return true;
}

bool isAllNegative(const ConstantRange &range) {
#if LLVM_VERSION_MAJOR > 8
  return range.isAllNegative();
#else
  if (range.isEmptySet()) {
    return true;
  }

  if (range.isFullSet()) {
    return false;
  }

  return !range.getLower().sgt(range.getUpper()) &&
         !range.getUpper().isStrictlyPositive();
#endif
}

bool isAllNonNegative(const ConstantRange &range) {
#if LLVM_VERSION_MAJOR > 8
  return range.isAllNonNegative();
#else
  return !range.isSignWrappedSet() && range.getLower().isNonNegative();
#endif
}

struct FPSCEV final {
  APFloat min;
  APFloat max;
  bool isInteger;

  explicit FPSCEV()
      : min(APFloat::getLargest(APFloat::Bogus(), true)),
        max(APFloat::getLargest(APFloat::Bogus(), false)), isInteger(false) {}

  explicit FPSCEV(Type *const type)
      : min(APFloat::getNaN(getSemanticsForType(type), true)),
        max(APFloat::getNaN(getSemanticsForType(type), false)),
        isInteger(false) {}

  explicit FPSCEV(const APFloat &min, const APFloat &max, bool isInteger)
      : min(min), max(max), isInteger(isInteger) {}

  void dump() const {
    errs() << "FPSCEV: " << this << ":\n";
    errs() << "min: ";
    if (min.isNaN() && min.isNegative()) {
      errs() << "-";
    }
    min.print(errs());
    errs() << "max: ";
    if (max.isNaN() && max.isNegative()) {
      errs() << "-";
    }
    max.print(errs());
    errs() << "isInteger: ";
    errs() << isInteger << "\n";
  }

  bool isNaN() const { return min.isNaN() || max.isNaN(); }

  bool isFinite() const { return min.isFinite() && max.isFinite(); }

  bool isAllNegative() const { return max.isNegative(); }

  bool isAllNonNegative() const { return !min.isNegative(); }

  bool hasOverlappingRange(const FPSCEV &fpscev) const {
    return isInRange(fpscev.min, min, max) || isInRange(fpscev.max, min, max) ||
           isInRange(min, fpscev.min, fpscev.max) ||
           isInRange(max, fpscev.min, fpscev.max);
  }

  bool isLessThan(const APFloat &apfloat) const {
    switch (max.compare(apfloat)) {
    case APFloat::cmpLessThan:
      return true;
    case APFloat::cmpEqual:
    case APFloat::cmpGreaterThan:
    case APFloat::cmpUnordered:
      return false;
    }
  }

  bool isLessThan(const FPSCEV &fpscev) const { return isLessThan(fpscev.min); }

  bool isLessThanEqual(const APFloat &apfloat) const {
    switch (max.compare(apfloat)) {
    case APFloat::cmpLessThan:
    case APFloat::cmpEqual:
      return true;
    case APFloat::cmpGreaterThan:
    case APFloat::cmpUnordered:
      return false;
    }
  }

  bool isLessThanEqual(const FPSCEV &fpscev) const {
    return isLessThanEqual(fpscev.min);
  }

  bool isGreaterThan(const APFloat &apfloat) const {
    switch (min.compare(apfloat)) {
    case APFloat::cmpGreaterThan:
      return true;
    case APFloat::cmpLessThan:
    case APFloat::cmpEqual:
    case APFloat::cmpUnordered:
      return false;
    }
  }

  bool isGreaterThan(const FPSCEV &fpscev) const {
    return isGreaterThan(fpscev.max);
  }

  bool isGreaterThanEqual(const APFloat &apfloat) const {
    switch (min.compare(apfloat)) {
    case APFloat::cmpGreaterThan:
    case APFloat::cmpEqual:
      return true;
    case APFloat::cmpLessThan:
    case APFloat::cmpUnordered:
      return false;
    }
  }

  bool isGreaterThanEqual(const FPSCEV &fpscev) const {
    return isGreaterThanEqual(fpscev.max);
  }

  bool isSingleElement() const { return min.bitwiseIsEqual(max); }

  FPSCEV cloneWithFastMathFlags(FastMathFlags fmf) const {
    FPSCEV fpscev(*this);
    fpscev.min = applyFastMathFlags(fpscev.min, fmf);
    fpscev.max = applyFastMathFlags(fpscev.max, fmf);
    return fpscev;
  }

  const fltSemantics &getSemantics() const { return min.getSemantics(); }
};

struct FPScalarEvolution final {
  ValueMap<const Value *, FPSCEV> map;

  FPSCEV *getFPSCEV(const Value *const value) {
    if (map.count(value) == 0) {
      if (const ConstantFP *const constant = dyn_cast<ConstantFP>(value)) {
        const APFloat &apfloat = constant->getValueAPF();
        map[value] = FPSCEV(apfloat, apfloat, apfloat.isInteger());
      } else if (value->getType()->isFloatingPointTy()) {
        map[value] = FPSCEV(value->getType());
      } else {
        return nullptr;
      }
    }

    return &map[value];
  }

  const FPSCEV *getFPSCEV(const Value *const value) const {
    auto iterator = map.find(value);

    if (iterator == map.end()) {
      return nullptr;
    }

    return &iterator->second;
  }

  void copyFPSCEVFromValue(const Value *const dst, const Value *const src) {
    auto iterator = map.find(src);

    if (iterator != map.end()) {
      map[dst] = iterator->second;
    }
  }

  void forgetValue(const Value *const value) { map.erase(value); }
};

// Define our LLVM pass as inheriting from a FunctionPass.
struct FPScalarEvolutionPass final : FunctionPass,
                                     InstVisitor<FPScalarEvolutionPass> {
  // We always have to include a default constructor - this is called during
  // pass registry to tell LLVM about our pass. This constructor must pass in
  // the static char ID of this pass to the parent constructor. LLVM uses the
  // address of the ID to uniquely identify each type of pass.
  FPScalarEvolutionPass() : FunctionPass(ID) {}

  StringRef getPassName() const override {
    return "Floating Point Scalar Evolution";
  }

  // Since we are implementing a FunctionPass, we need to provide the
  // runOnFunction override.
  bool runOnFunction(Function &f) override {
    // Clear the map first.
    fpse.map.clear();

    // Get the scalar evolution analysis we are going to use.
    scalarEvolution = &getAnalysis<ScalarEvolutionWrapperPass>().getSE();

    ReversePostOrderTraversal<Function *> rpot(&f);

    for (BasicBlock *const bb : rpot) {
      visit(bb);
    }

    for (auto fpscev : fpse.map) {
      const Value &inst = *fpscev.first;
      ANALYSIS(inst.print(errs()));
      ANALYSIS(errs() << "\n");
      ANALYSIS(if (auto fpscev = fpse.getFPSCEV(&inst)) fpscev->dump());
    }

    // We have to return false because as we are an analysis we cannot modify
    // the function.
    return false;
  }

  void visitSIToFPInst(SIToFPInst &inst) {
    ConstantRange range = scalarEvolution->getSignedRange(
        scalarEvolution->getSCEV(inst.getOperand(0)));

    const fltSemantics &semantics = getSemanticsForType(inst.getType());

    APFloat min = getFromInt(semantics, range.getSignedMin(), true);
    APFloat max = getFromInt(semantics, range.getSignedMax(), true);
    bool isInteger = min.isInteger() && max.isInteger();

    fpse.map[&inst] = FPSCEV(min, max, isInteger);
  }

  void visitUIToFPInst(UIToFPInst &inst) {
    ConstantRange range = scalarEvolution->getUnsignedRange(
        scalarEvolution->getSCEV(inst.getOperand(0)));

    const fltSemantics &semantics = getSemanticsForType(inst.getType());

    APFloat min = getFromInt(semantics, range.getUnsignedMin(), false);
    APFloat max = getFromInt(semantics, range.getUnsignedMax(), false);
    bool isInteger = min.isInteger() && max.isInteger();

    fpse.map[&inst] = FPSCEV(min, max, isInteger);
  }

  void visitFPTruncInst(FPTruncInst &inst) {
    const FPSCEV *fpscev = fpse.getFPSCEV(inst.getOperand(0));

    const fltSemantics &semantics = getSemanticsForType(inst.getType());

    APFloat min = fpscev->min;
    APFloat max = fpscev->max;

    bool losesInfo = false;
    min.convert(semantics, APFloat::rmTowardNegative, &losesInfo);
    max.convert(semantics, APFloat::rmTowardPositive, &losesInfo);

    bool isInteger = fpscev->isInteger && min.isInteger() && max.isInteger();

    fpse.map[&inst] = FPSCEV(min, max, isInteger);
  }

  void visitFPExtInst(FPExtInst &inst) {
    const FPSCEV *fpscev = fpse.getFPSCEV(inst.getOperand(0));

    const fltSemantics &semantics = getSemanticsForType(inst.getType());

    APFloat min = fpscev->min;
    APFloat max = fpscev->max;

    bool losesInfo = false;
    min.convert(semantics, APFloat::rmTowardNegative, &losesInfo);
    assert(!losesInfo);
    max.convert(semantics, APFloat::rmTowardPositive, &losesInfo);
    assert(!losesInfo);

    bool isInteger = fpscev->isInteger;

    fpse.map[&inst] = FPSCEV(min, max, isInteger);
  }

  void visitSelectInst(SelectInst &inst) {
    const FastMathFlags flags = inst.getFastMathFlags();

    // Skip any select that isn't between two selects.
    if (!inst.getType()->isFloatingPointTy()) {
      return;
    }

    const FPSCEV *const trueFPSCEV = fpse.getFPSCEV(inst.getTrueValue());
    APFloat trueMin = applyFastMathFlags(trueFPSCEV->min, flags);
    APFloat trueMax = applyFastMathFlags(trueFPSCEV->max, flags);

    const FPSCEV *const falseFPSCEV = fpse.getFPSCEV(inst.getFalseValue());
    APFloat falseMin = applyFastMathFlags(falseFPSCEV->min, flags);
    APFloat falseMax = applyFastMathFlags(falseFPSCEV->max, flags);

    APFloat min = getMinimum({trueMin, falseMin});
    APFloat max = getMaximum({trueMax, falseMax});
    bool isInteger = trueFPSCEV->isInteger && falseFPSCEV->isInteger;

    fpse.map[&inst] = FPSCEV(min, max, isInteger);
  }

  void visitPHINode(PHINode &inst) {
    FPSCEV *const firstFPSCEV = fpse.getFPSCEV(inst.getIncomingValue(0));
    APFloat min = firstFPSCEV->min;
    APFloat max = firstFPSCEV->max;
    bool isInteger = firstFPSCEV->isInteger;

    for (unsigned i = 1, e = inst.getNumIncomingValues(); i < e; i++) {
      FPSCEV *const iFPSCEV = fpse.getFPSCEV(inst.getIncomingValue(i));
      min = getMinimum({min, iFPSCEV->min});
      max = getMaximum({max, iFPSCEV->max});
      isInteger = isInteger && iFPSCEV->isInteger;
    }

    fpse.map[&inst] = FPSCEV(min, max, isInteger);
  }

  void visitUnaryOperator(UnaryOperator &inst) {
    switch (inst.getOpcode()) {
    default:
      return;
    case Instruction::FNeg:
      break;
    }

    const FastMathFlags flags = inst.getFastMathFlags();

    const FPSCEV *const instFPSCEV = fpse.getFPSCEV(inst.getOperand(0));

    APFloat min = applyFastMathFlags(instFPSCEV->min, flags);
    APFloat max = applyFastMathFlags(instFPSCEV->max, flags);

    // Flip the signs of min and max because we are negating them.
    min.changeSign();
    max.changeSign();

    // If we have min = -1 & max = 1, when we flip the sign min = 1 & max = -1,
    // which is clearly incorrect. Need to recheck which is smaller/larger.
    APFloat realMin = getMinimum({min, max});
    APFloat realMax = getMaximum({min, max});

    realMin = applyFastMathFlags(realMin, flags);
    realMax = applyFastMathFlags(realMax, flags);

    fpse.map[&inst] = FPSCEV(realMin, realMax, instFPSCEV->isInteger);
  }

  void visitFAdd(BinaryOperator &inst) {
    const FastMathFlags flags = inst.getFastMathFlags();

    const FPSCEV *const xFPSCEV = fpse.getFPSCEV(inst.getOperand(0));
    const APFloat xMin = applyFastMathFlags(xFPSCEV->min, flags);
    const APFloat xMax = applyFastMathFlags(xFPSCEV->max, flags);

    const FPSCEV *const yFPSCEV = fpse.getFPSCEV(inst.getOperand(1));
    const APFloat yMin = applyFastMathFlags(yFPSCEV->min, flags);
    const APFloat yMax = applyFastMathFlags(yFPSCEV->max, flags);

    APFloat mins[4] = {xMin, xMin, xMax, xMax};
    mins[0].add(yMin, APFloat::rmTowardNegative);
    mins[1].add(yMax, APFloat::rmTowardNegative);
    mins[2].add(yMin, APFloat::rmTowardNegative);
    mins[3].add(yMax, APFloat::rmTowardNegative);
    APFloat min = getMinimum(mins);

    APFloat maxs[4] = {xMin, xMin, xMax, xMax};
    maxs[0].add(yMin, APFloat::rmTowardPositive);
    maxs[1].add(yMax, APFloat::rmTowardPositive);
    maxs[2].add(yMin, APFloat::rmTowardPositive);
    maxs[3].add(yMax, APFloat::rmTowardPositive);
    APFloat max = getMaximum(maxs);

    min = applyFastMathFlags(min, flags);
    max = applyFastMathFlags(max, flags);

    fpse.map[&inst] =
        FPSCEV(min, max, xFPSCEV->isInteger && yFPSCEV->isInteger);
  }

  void visitFSub(BinaryOperator &inst) {
    const FastMathFlags flags = inst.getFastMathFlags();

    const FPSCEV *const xFPSCEV = fpse.getFPSCEV(inst.getOperand(0));
    const APFloat xMin = applyFastMathFlags(xFPSCEV->min, flags);
    const APFloat xMax = applyFastMathFlags(xFPSCEV->max, flags);

    const FPSCEV *const yFPSCEV = fpse.getFPSCEV(inst.getOperand(1));
    const APFloat yMin = applyFastMathFlags(yFPSCEV->min, flags);
    const APFloat yMax = applyFastMathFlags(yFPSCEV->max, flags);

    APFloat mins[4] = {xMin, xMin, xMax, xMax};
    mins[0].subtract(yMin, APFloat::rmTowardNegative);
    mins[1].subtract(yMax, APFloat::rmTowardNegative);
    mins[2].subtract(yMin, APFloat::rmTowardNegative);
    mins[3].subtract(yMax, APFloat::rmTowardNegative);
    APFloat min = getMinimum(mins);

    APFloat maxs[4] = {xMin, xMin, xMax, xMax};
    maxs[0].subtract(yMin, APFloat::rmTowardPositive);
    maxs[1].subtract(yMax, APFloat::rmTowardPositive);
    maxs[2].subtract(yMin, APFloat::rmTowardPositive);
    maxs[3].subtract(yMax, APFloat::rmTowardPositive);
    APFloat max = getMaximum(maxs);

    min = applyFastMathFlags(min, flags);
    max = applyFastMathFlags(max, flags);

    fpse.map[&inst] =
        FPSCEV(min, max, xFPSCEV->isInteger && yFPSCEV->isInteger);
  }

  void visitFMul(BinaryOperator &inst) {
    const FastMathFlags flags = inst.getFastMathFlags();

    const FPSCEV *const xFPSCEV = fpse.getFPSCEV(inst.getOperand(0));
    const APFloat xMin = applyFastMathFlags(xFPSCEV->min, flags);
    const APFloat xMax = applyFastMathFlags(xFPSCEV->max, flags);

    const FPSCEV *const yFPSCEV = fpse.getFPSCEV(inst.getOperand(1));
    const APFloat yMin = applyFastMathFlags(yFPSCEV->min, flags);
    const APFloat yMax = applyFastMathFlags(yFPSCEV->max, flags);

    APFloat mins[4] = {xMin, xMin, xMax, xMax};
    mins[0].multiply(yMin, APFloat::rmTowardNegative);
    mins[1].multiply(yMax, APFloat::rmTowardNegative);
    mins[2].multiply(yMin, APFloat::rmTowardNegative);
    mins[3].multiply(yMax, APFloat::rmTowardNegative);
    APFloat min = getMinimum(mins);

    APFloat maxs[4] = {xMin, xMin, xMax, xMax};
    maxs[0].multiply(yMin, APFloat::rmTowardPositive);
    maxs[1].multiply(yMax, APFloat::rmTowardPositive);
    maxs[2].multiply(yMin, APFloat::rmTowardPositive);
    maxs[3].multiply(yMax, APFloat::rmTowardPositive);
    APFloat max = getMaximum(maxs);

    min = applyFastMathFlags(min, flags);
    max = applyFastMathFlags(max, flags);

    fpse.map[&inst] =
        FPSCEV(min, max, xFPSCEV->isInteger && yFPSCEV->isInteger);
  }

  void visitFDiv(BinaryOperator &inst) {
    const FastMathFlags flags = inst.getFastMathFlags();

    const FPSCEV *const xFPSCEV = fpse.getFPSCEV(inst.getOperand(0));
    const APFloat xMin = applyFastMathFlags(xFPSCEV->min, flags);
    const APFloat xMax = applyFastMathFlags(xFPSCEV->max, flags);

    const FPSCEV *const yFPSCEV = fpse.getFPSCEV(inst.getOperand(1));
    const APFloat yMin = applyFastMathFlags(yFPSCEV->min, flags);
    const APFloat yMax = applyFastMathFlags(yFPSCEV->max, flags);

    const fltSemantics &semantics = xMin.getSemantics();

    const APFloat zero = APFloat::getZero(semantics);
    const APFloat one = getOne(semantics);
    APFloat minusOne = one;
    minusOne.changeSign();

    APFloat min = APFloat::getNaN(semantics, true);
    APFloat max = APFloat::getNaN(semantics, false);

    if (xMin.isFinite() && xMax.isFinite() && yMin.isFinite() &&
        yMax.isFinite()) {

      // If y does not pass through zero, cannot produce a NaN.
      if (!isInRange(zero, yMin, yMax)) {
        min = APFloat::getInf(semantics, true);
        max = APFloat::getInf(semantics, false);
      }

      // If any value of y is not between (-1..1) it means the result is
      // strictly less than the bounds.
      if (!isInRangeExclusive(yMin, minusOne, one) &&
          !isInRangeExclusive(yMax, minusOne, one)) {
        APFloat mins[4] = {xMin, xMin, xMax, xMax};
        mins[0].divide(yMin, APFloat::rmTowardNegative);
        mins[1].divide(yMax, APFloat::rmTowardNegative);
        mins[2].divide(yMin, APFloat::rmTowardNegative);
        mins[3].divide(yMax, APFloat::rmTowardNegative);
        min = getMinimum(mins);

        APFloat maxs[4] = {xMin, xMin, xMax, xMax};
        maxs[0].divide(yMin, APFloat::rmTowardPositive);
        maxs[1].divide(yMax, APFloat::rmTowardPositive);
        maxs[2].divide(yMin, APFloat::rmTowardPositive);
        maxs[3].divide(yMax, APFloat::rmTowardPositive);
        max = getMaximum(maxs);

        // If reciprocals are allowed, we need to add a bunch more checks to our
        // divide.
        if (flags.allowReciprocal()) {
          APFloat reciprocals[4] = {one, one, one, one};
          reciprocals[0].divide(yMin, APFloat::rmTowardNegative);
          reciprocals[1].divide(yMin, APFloat::rmTowardPositive);
          reciprocals[2].divide(yMax, APFloat::rmTowardNegative);
          reciprocals[3].divide(yMax, APFloat::rmTowardPositive);

          APFloat mins[9] = {xMin, xMin, xMin, xMin, xMax,
                             xMax, xMax, xMax, min};

          mins[0].multiply(reciprocals[0], APFloat::rmTowardNegative);
          mins[1].multiply(reciprocals[1], APFloat::rmTowardNegative);
          mins[2].multiply(reciprocals[2], APFloat::rmTowardNegative);
          mins[3].multiply(reciprocals[3], APFloat::rmTowardNegative);
          mins[4].multiply(reciprocals[0], APFloat::rmTowardNegative);
          mins[5].multiply(reciprocals[1], APFloat::rmTowardNegative);
          mins[6].multiply(reciprocals[2], APFloat::rmTowardNegative);
          mins[7].multiply(reciprocals[3], APFloat::rmTowardNegative);

          min = getMinimum(mins);

          APFloat maxs[9] = {xMin, xMin, xMin, xMin, xMax,
                             xMax, xMax, xMax, max};

          maxs[0].multiply(reciprocals[0], APFloat::rmTowardPositive);
          maxs[1].multiply(reciprocals[1], APFloat::rmTowardPositive);
          maxs[2].multiply(reciprocals[2], APFloat::rmTowardPositive);
          maxs[3].multiply(reciprocals[3], APFloat::rmTowardPositive);
          maxs[4].multiply(reciprocals[0], APFloat::rmTowardPositive);
          maxs[5].multiply(reciprocals[1], APFloat::rmTowardPositive);
          maxs[6].multiply(reciprocals[2], APFloat::rmTowardPositive);
          maxs[7].multiply(reciprocals[3], APFloat::rmTowardPositive);

          max = getMaximum(maxs);
        }
      }
    }

    min = applyFastMathFlags(min, flags);
    max = applyFastMathFlags(max, flags);

    fpse.map[&inst] = FPSCEV(min, max, false);
  }

  void visitFRem(BinaryOperator &inst) {
    // There is no remainder that takes a rounding mode, so we can't do anything
    // other than apply fast math flags.
    FPSCEV fpscev(inst.getType());

    // Record the arguments to the frem even if we don't actually need them yet.
    fpse.getFPSCEV(inst.getOperand(0));
    fpse.getFPSCEV(inst.getOperand(1));

    const FastMathFlags flags = inst.getFastMathFlags();
    fpscev.min = applyFastMathFlags(fpscev.min, flags);
    fpscev.max = applyFastMathFlags(fpscev.max, flags);

    fpse.map[&inst] = fpscev;
  }

  void visitBinaryOperator(BinaryOperator &inst) {
    switch (inst.getOpcode()) {
    default:
      return;
    case Instruction::FAdd:
      visitFAdd(inst);
      return;
    case Instruction::FSub:
      visitFSub(inst);
      return;
    case Instruction::FMul:
      visitFMul(inst);
      return;
    case Instruction::FDiv:
      visitFDiv(inst);
      return;
    case Instruction::FRem:
      visitFRem(inst);
      return;
    }
  }

  void visitCallInst(CallInst &inst) {
    // Skip any calls that doesn't result in a floating point.
    if (!inst.getType()->isFloatingPointTy()) {
      return;
    }

    FPSCEV fpscev(inst.getType());

    const FastMathFlags flags = inst.getFastMathFlags();
    fpscev.min = applyFastMathFlags(fpscev.min, flags);
    fpscev.max = applyFastMathFlags(fpscev.max, flags);

    fpse.map[&inst] = fpscev;
  }

  template <Intrinsic::ID> void visitIntrinsic(IntrinsicInst &inst);

  void visitIntrinsicInst(IntrinsicInst &inst);

  // Get analysis usage allows us to tell LLVM which passes we require.
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    // We are going to use the existing scalar evolution to let us work if we
    // have an int -> float conversion, what can we expect the value to be.
    AU.addRequired<ScalarEvolutionWrapperPass>();

    // Since this is just an analysis, tell LLVM that we preserve all passes
    // (this means we cannot modify anything though!).
    AU.setPreservesAll();
  }

  // The ID of this pass - the address of which is used by LLVM to uniquely
  // identify the pass.
  static char ID;

  FPScalarEvolution &getFPSCEV() { return fpse; }
  const FPScalarEvolution &getFPSCEV() const { return fpse; }

private:
  FPScalarEvolution fpse;
  ScalarEvolution *scalarEvolution;
};

FPSCEV sqrtBounds(FPSCEV fpscev) {
  const fltSemantics &semantics = fpscev.getSemantics();

  // If the full range is negative, always returns a NaN.
  if (fpscev.isAllNegative()) {
    APFloat nan = APFloat::getNaN(semantics, false);
    return FPSCEV(nan, nan, false);
  }

  bool losesInfo;

  if (fpscev.isAllNonNegative()) {
    fpscev.min.convert(APFloat::IEEEdouble(), APFloat::rmTowardNegative,
                       &losesInfo);

    // Get the next number less than the current (unless we are zero).
    if (!fpscev.min.isZero()) {
      fpscev.min.next(true);
    }
    fpscev.min = APFloat(std::sqrt(fpscev.min.convertToDouble()));
    fpscev.min.convert(semantics, APFloat::rmTowardNegative, &losesInfo);
  }

  // If the whole fpscev is not negative, it means max must be positive.
  if (!fpscev.isAllNegative()) {
    fpscev.max.convert(APFloat::IEEEdouble(), APFloat::rmTowardPositive,
                       &losesInfo);
    fpscev.max.next(false);
    fpscev.max = APFloat(std::sqrt(fpscev.max.convertToDouble()));
    fpscev.max.convert(semantics, APFloat::rmTowardPositive, &losesInfo);
  }

  fpscev.isInteger = false;

  return fpscev;
}

// For exp2, we convert the fpscev that we are using as a power to integer, and
// then use scalbn to get a lower/upper bound for it (rounding the min value
// down, and the max value up).
FPSCEV exp2Bounds(FPSCEV fpscev) {
  const fltSemantics &semantics = fpscev.getSemantics();
  const APFloat one = getOne(semantics);

  bool losesInfo;

  if (fpscev.min.isFinite()) {
    fpscev.min.roundToIntegral(APFloat::rmTowardNegative);
    fpscev.min.convert(APFloat::IEEEdouble(), APFloat::rmTowardNegative,
                       &losesInfo);

    const int power = static_cast<int>(fpscev.min.convertToDouble());

    fpscev.min = scalbn(one, power, APFloat::rmTowardNegative);
  } else {
    fpscev.min = APFloat::getInf(semantics, true);
  }

  if (fpscev.max.isFinite()) {
    fpscev.max.roundToIntegral(APFloat::rmTowardPositive);
    fpscev.max.convert(APFloat::IEEEdouble(), APFloat::rmTowardPositive,
                       &losesInfo);

    const int power = static_cast<int>(fpscev.max.convertToDouble());

    fpscev.max = scalbn(one, power, APFloat::rmTowardPositive);
  } else {
    fpscev.max = APFloat::getInf(semantics, false);
  }

  return fpscev;
}

FPSCEV log2Bounds(FPSCEV fpscev) {
  const fltSemantics &semantics = fpscev.getSemantics();

  // If the full range is negative, always returns a NaN.
  if (fpscev.isAllNegative()) {
    APFloat nan = APFloat::getNaN(semantics, false);
    return FPSCEV(nan, nan, false);
  }

  const APInt ilogbMin(32, ilogb(fpscev.min));
  // We do +1 here so we round the range up.
  const APInt ilogbMax(32, ilogb(fpscev.max) + 1);

  APFloat values[2] = {getFromInt(semantics, ilogbMin, true),
                       getFromInt(semantics, ilogbMax, true)};

  // If the input is entirely greater than zero, we can use ilogb to get a much
  // firmer estimate on the log result.
  if (fpscev.isGreaterThan(APFloat::getZero(semantics))) {
    fpscev.min = getMinimum(values);
    fpscev.max = getMaximum(values);
  } else {
    fpscev.min = APFloat::getInf(semantics, true);
    fpscev.max = values[1];
  }

  // Can't be sure about this because we're getting very vague bounds, so wipe
  // it.
  fpscev.isInteger = false;

  return fpscev;
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::sqrt>(
    IntrinsicInst &inst) {
  const FastMathFlags fmf = inst.getFastMathFlags();

  FPSCEV fpscev =
      fpse.getFPSCEV(inst.getOperand(0))->cloneWithFastMathFlags(fmf);

  fpscev = sqrtBounds(fpscev);

  fpse.map[&inst] = fpscev.cloneWithFastMathFlags(fmf);
}

// For pow we use exp2(y * log2(x)) to get the best bounds we can hope for.
template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::pow>(
    IntrinsicInst &inst) {
  const FastMathFlags fmf = inst.getFastMathFlags();

  const FPSCEV xFpscev =
      fpse.getFPSCEV(inst.getOperand(0))->cloneWithFastMathFlags(fmf);
  const FPSCEV yFpscev =
      fpse.getFPSCEV(inst.getOperand(1))->cloneWithFastMathFlags(fmf);

  FPSCEV log2Fpscev = log2Bounds(xFpscev);
  FPSCEV fpscevs[4] = {log2Fpscev, log2Fpscev, log2Fpscev, log2Fpscev};

  fpscevs[0].min.multiply(yFpscev.min, APFloat::rmTowardNegative);
  fpscevs[0].max.multiply(yFpscev.min, APFloat::rmTowardNegative);

  fpscevs[1].min.multiply(yFpscev.max, APFloat::rmTowardNegative);
  fpscevs[1].max.multiply(yFpscev.max, APFloat::rmTowardNegative);

  fpscevs[2].min.multiply(yFpscev.min, APFloat::rmTowardPositive);
  fpscevs[2].max.multiply(yFpscev.min, APFloat::rmTowardPositive);

  fpscevs[3].min.multiply(yFpscev.max, APFloat::rmTowardPositive);
  fpscevs[3].max.multiply(yFpscev.max, APFloat::rmTowardPositive);

  fpscevs[0] = exp2Bounds(fpscevs[0]);
  fpscevs[1] = exp2Bounds(fpscevs[1]);
  fpscevs[2] = exp2Bounds(fpscevs[2]);
  fpscevs[3] = exp2Bounds(fpscevs[3]);

  FPSCEV result;

  result.min = getMinimum({fpscevs[0].min, fpscevs[0].max, fpscevs[1].min,
                           fpscevs[1].max, fpscevs[2].min, fpscevs[2].max,
                           fpscevs[3].min, fpscevs[3].max});
  result.max = getMaximum({fpscevs[0].min, fpscevs[0].max, fpscevs[1].min,
                           fpscevs[1].max, fpscevs[2].min, fpscevs[2].max,
                           fpscevs[3].min, fpscevs[3].max});
  result.isInteger = false;

  fpse.map[&inst] = result.cloneWithFastMathFlags(fmf);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::powi>(
    IntrinsicInst &inst) {
  const FastMathFlags flags = inst.getFastMathFlags();

  const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));
  ConstantRange range = scalarEvolution->getSignedRange(
      scalarEvolution->getSCEV(inst.getOperand(1)));

  APFloat min = applyFastMathFlags(fpscev->min, flags);
  APFloat max = applyFastMathFlags(fpscev->max, flags);

  const fltSemantics &semantics = min.getSemantics();

  const APFloat zero = APFloat::getZero(semantics, false);
  const APFloat one = getOne(semantics);

  // If the value to raise to a power is positive.
  if (!min.isNegative()) {
    const bool isOneOrLess =
        isInRange(min, zero, one) && isInRange(max, zero, one);

    // If the integer power is a negative and the float is greater than one,
    // the result will always be at least as small as the inputs.
    if (isAllNegative(range) && !isOneOrLess) {
      fpse.map[&inst] = FPSCEV(zero, max, false);
      return;
    }

    // If the integer power is definitely not a negative, the result will be
    // in the range [min(1, min)..infinity] if x is greater than 1. Otherwise
    // the output will be between 0 and 1.
    if (isAllNonNegative(range)) {
      // Because x^0 == 1, the output could always be 1 or lower.
      min = isOneOrLess ? zero : getMinimum({min, one});

      // If the input x is one or greater then the output could be as
      max = isOneOrLess ? one : APFloat::getInf(semantics, false);

      max = applyFastMathFlags(max, flags);

      fpse.map[&inst] = FPSCEV(min, max, false);
      return;
    }
  }

  FPSCEV newFpscev(inst.getType());

  newFpscev.min = applyFastMathFlags(newFpscev.min, flags);
  newFpscev.max = applyFastMathFlags(newFpscev.max, flags);

  fpse.map[&inst] = newFpscev;
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::cos>(
    IntrinsicInst &inst) {
  const FastMathFlags flags = inst.getFastMathFlags();

  const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));
  APFloat min = applyFastMathFlags(fpscev->min, flags);
  APFloat max = applyFastMathFlags(fpscev->max, flags);

  // If the range is finite (not inf or nan) the output is always [-1..1].
  if (min.isFinite() && max.isFinite()) {
    const APFloat one = getOne(min.getSemantics());
    APFloat minusOne = one;
    minusOne.changeSign();

    fpse.map[&inst] = FPSCEV(minusOne, one, false);
    return;
  }

  FPSCEV newFpscev(inst.getType());

  newFpscev.min = applyFastMathFlags(newFpscev.min, flags);
  newFpscev.max = applyFastMathFlags(newFpscev.max, flags);

  fpse.map[&inst] = newFpscev;
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::sin>(
    IntrinsicInst &inst) {
  const FastMathFlags flags = inst.getFastMathFlags();

  const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));
  APFloat min = applyFastMathFlags(fpscev->min, flags);
  APFloat max = applyFastMathFlags(fpscev->max, flags);

  // If the range is finite (not inf or nan) the output is always [-1..1].
  if (min.isFinite() && max.isFinite()) {
    const APFloat one = getOne(min.getSemantics());
    APFloat minusOne = one;
    minusOne.changeSign();

    fpse.map[&inst] = FPSCEV(minusOne, one, false);
    return;
  }

  FPSCEV newFpscev(inst.getType());

  newFpscev.min = applyFastMathFlags(newFpscev.min, flags);
  newFpscev.max = applyFastMathFlags(newFpscev.max, flags);

  fpse.map[&inst] = newFpscev;
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::exp>(
    IntrinsicInst &inst) {
  FPSCEV fpscev(inst.getType());

  // Exp always has a positive result.
  const APFloat zero = APFloat::getZero(fpscev.min.getSemantics(), false);
  fpscev.min = zero;

  const FastMathFlags flags = inst.getFastMathFlags();
  fpscev.max = applyFastMathFlags(fpscev.max, flags);

  fpse.map[&inst] = fpscev;
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::exp2>(
    IntrinsicInst &inst) {
  const FastMathFlags fmf = inst.getFastMathFlags();

  FPSCEV fpscev =
      fpse.getFPSCEV(inst.getOperand(0))->cloneWithFastMathFlags(fmf);

  fpscev = exp2Bounds(fpscev);

  fpse.map[&inst] = fpscev.cloneWithFastMathFlags(fmf);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::log>(
    IntrinsicInst &inst) {
  const FastMathFlags fmf = inst.getFastMathFlags();

  FPSCEV fpscev =
      fpse.getFPSCEV(inst.getOperand(0))->cloneWithFastMathFlags(fmf);

  fpscev = log2Bounds(fpscev);

  // We are calculating log(x), but we only have ilogb (which gives us an
  // effective lower bound of log2(x)). To convert this bound into log(x) by
  // multiplying it by log(2).
  APFloat converter(
      0.693147180559945309417232121458176568075500134360255254120);
  bool losesInfo;
  converter.convert(fpscev.getSemantics(), APFloat::rmNearestTiesToEven,
                    &losesInfo);

  fpscev.min.multiply(converter, APFloat::rmTowardNegative);
  fpscev.max.multiply(converter, APFloat::rmTowardPositive);

  fpse.map[&inst] = fpscev.cloneWithFastMathFlags(fmf);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::log10>(
    IntrinsicInst &inst) {
  const FastMathFlags fmf = inst.getFastMathFlags();

  FPSCEV fpscev =
      fpse.getFPSCEV(inst.getOperand(0))->cloneWithFastMathFlags(fmf);

  fpscev = log2Bounds(fpscev);

  // We are calculating log10(x), but we only have ilogb (which gives us an
  // effective lower bound of log2(x)). To convert this bound into log10(x) by
  // multiplying it by 1/log2(10).
  APFloat converter(
      0.301029995663981195213738894724493026768189881462108541310);
  bool losesInfo;
  converter.convert(fpscev.getSemantics(), APFloat::rmNearestTiesToEven,
                    &losesInfo);

  fpscev.min.multiply(converter, APFloat::rmTowardNegative);
  fpscev.max.multiply(converter, APFloat::rmTowardPositive);

  fpse.map[&inst] = fpscev.cloneWithFastMathFlags(fmf);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::log2>(
    IntrinsicInst &inst) {
  const FastMathFlags fmf = inst.getFastMathFlags();

  const FPSCEV fpscev =
      fpse.getFPSCEV(inst.getOperand(0))->cloneWithFastMathFlags(fmf);

  fpse.map[&inst] = log2Bounds(fpscev).cloneWithFastMathFlags(fmf);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::fma>(
    IntrinsicInst &inst) {
  const FastMathFlags flags = inst.getFastMathFlags();

  const FPSCEV *const xFPSCEV = fpse.getFPSCEV(inst.getOperand(0));
  const APFloat xMin = applyFastMathFlags(xFPSCEV->min, flags);
  const APFloat xMax = applyFastMathFlags(xFPSCEV->max, flags);

  const FPSCEV *const yFPSCEV = fpse.getFPSCEV(inst.getOperand(1));
  const APFloat yMin = applyFastMathFlags(yFPSCEV->min, flags);
  const APFloat yMax = applyFastMathFlags(yFPSCEV->max, flags);

  const FPSCEV *const zFPSCEV = fpse.getFPSCEV(inst.getOperand(2));
  const APFloat zMin = applyFastMathFlags(zFPSCEV->min, flags);
  const APFloat zMax = applyFastMathFlags(zFPSCEV->max, flags);

  APFloat mins[8] = {xMin, xMin, xMin, xMin, xMax, xMax, xMax, xMax};
  mins[0].fusedMultiplyAdd(yMin, zMin, APFloat::rmTowardNegative);
  mins[1].fusedMultiplyAdd(yMin, zMax, APFloat::rmTowardNegative);
  mins[2].fusedMultiplyAdd(yMax, zMin, APFloat::rmTowardNegative);
  mins[3].fusedMultiplyAdd(yMax, zMax, APFloat::rmTowardNegative);
  mins[4].fusedMultiplyAdd(yMin, zMin, APFloat::rmTowardNegative);
  mins[5].fusedMultiplyAdd(yMin, zMax, APFloat::rmTowardNegative);
  mins[6].fusedMultiplyAdd(yMax, zMin, APFloat::rmTowardNegative);
  mins[7].fusedMultiplyAdd(yMax, zMax, APFloat::rmTowardNegative);
  APFloat min = getMinimum(mins);

  APFloat maxs[8] = {xMin, xMin, xMin, xMin, xMax, xMax, xMax, xMax};
  maxs[0].fusedMultiplyAdd(yMin, zMin, APFloat::rmTowardPositive);
  maxs[1].fusedMultiplyAdd(yMin, zMax, APFloat::rmTowardPositive);
  maxs[2].fusedMultiplyAdd(yMax, zMin, APFloat::rmTowardPositive);
  maxs[3].fusedMultiplyAdd(yMax, zMax, APFloat::rmTowardPositive);
  maxs[4].fusedMultiplyAdd(yMin, zMin, APFloat::rmTowardPositive);
  maxs[5].fusedMultiplyAdd(yMin, zMax, APFloat::rmTowardPositive);
  maxs[6].fusedMultiplyAdd(yMax, zMin, APFloat::rmTowardPositive);
  maxs[7].fusedMultiplyAdd(yMax, zMax, APFloat::rmTowardPositive);
  APFloat max = getMaximum(maxs);

  min = applyFastMathFlags(min, flags);
  max = applyFastMathFlags(max, flags);

  fpse.map[&inst] = FPSCEV(min, max, false);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::fabs>(
    IntrinsicInst &inst) {
  const FastMathFlags flags = inst.getFastMathFlags();

  const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));

  APFloat min = applyFastMathFlags(fpscev->min, flags);
  APFloat max = applyFastMathFlags(fpscev->max, flags);

  // if the range contains positive and negative numbers.
  const bool positiveAndNegative = min.isNegative() && !max.isNegative();

  // Flip the signs of min and max because we are negating them.
  min.clearSign();
  max.clearSign();

  // If we have min = -4 & max = 1, when we clear the sign min = 4 & max = 1,
  // which is clearly incorrect. Need to recheck which is smaller/larger.
  APFloat realMin = getMinimum({min, max});
  APFloat realMax = getMaximum({min, max});

  if (positiveAndNegative) {
    realMin = APFloat::getZero(realMin.getSemantics());
  }

  realMin = applyFastMathFlags(realMin, flags);
  realMax = applyFastMathFlags(realMax, flags);

  fpse.map[&inst] = FPSCEV(realMin, realMax, fpscev->isInteger);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::minnum>(
    IntrinsicInst &inst) {
  const FastMathFlags flags = inst.getFastMathFlags();

  const FPSCEV *const xFpscev = fpse.getFPSCEV(inst.getOperand(0));
  const FPSCEV *const yFpscev = fpse.getFPSCEV(inst.getOperand(1));

  APFloat xMin = applyFastMathFlags(xFpscev->min, flags);
  APFloat xMax = applyFastMathFlags(xFpscev->max, flags);
  APFloat yMin = applyFastMathFlags(yFpscev->min, flags);
  APFloat yMax = applyFastMathFlags(yFpscev->max, flags);

  const fltSemantics &semantics = xMin.getSemantics();

  APFloat min = APFloat::getNaN(semantics, true);

  if (xMin.isFinite() && yMin.isFinite()) {
    // If both our mins are finite, choose the smallest.
    min = getMinimum({xMin, yMin});
  } else if (xMin.isFinite() ^ yMin.isFinite()) {
    // If one of our mins is finite, we definitely do not produce a NaN.
    min =
        APFloat::getLargest(semantics, xMin.isNegative() || yMin.isNegative());
  }

  APFloat max = APFloat::getNaN(semantics, false);

  if (xMax.isFinite() && yMax.isFinite()) {
    // If both our maxs are finite, choose the smallest.
    max = getMinimum({xMax, yMax});
  } else if (xMax.isFinite() ^ yMax.isFinite()) {
    // If one of our maxs is finite, we definitely do not produce a NaN.
    max =
        APFloat::getLargest(semantics, xMax.isNegative() || yMax.isNegative());
  }

  min = applyFastMathFlags(min, flags);
  max = applyFastMathFlags(max, flags);

  fpse.map[&inst] = FPSCEV(min, max, xFpscev->isInteger && yFpscev->isInteger);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::maxnum>(
    IntrinsicInst &inst) {
  const FastMathFlags flags = inst.getFastMathFlags();

  const FPSCEV *const xFpscev = fpse.getFPSCEV(inst.getOperand(0));
  const FPSCEV *const yFpscev = fpse.getFPSCEV(inst.getOperand(1));

  APFloat xMin = applyFastMathFlags(xFpscev->min, flags);
  APFloat xMax = applyFastMathFlags(xFpscev->max, flags);
  APFloat yMin = applyFastMathFlags(yFpscev->min, flags);
  APFloat yMax = applyFastMathFlags(yFpscev->max, flags);

  const fltSemantics &semantics = xMin.getSemantics();

  APFloat min = APFloat::getNaN(semantics, true);

  if (xMin.isFinite() && yMin.isFinite()) {
    // If both our mins are finite, choose the largest.
    min = getMaximum({xMin, yMin});
  } else if (xMin.isFinite() ^ yMin.isFinite()) {
    // If one of our mins is finite, we definitely do not produce a NaN.
    min =
        APFloat::getLargest(semantics, xMin.isNegative() || yMin.isNegative());
  }

  APFloat max = APFloat::getNaN(semantics, false);

  if (xMax.isFinite() && yMax.isFinite()) {
    // If both our maxs are finite, choose the largest.
    max = getMaximum({xMax, yMax});
  } else if (xMax.isFinite() ^ yMax.isFinite()) {
    // If one of our maxs is finite, we definitely do not produce a NaN.
    max =
        APFloat::getLargest(semantics, xMax.isNegative() || yMax.isNegative());
  }

  min = applyFastMathFlags(min, flags);
  max = applyFastMathFlags(max, flags);

  fpse.map[&inst] = FPSCEV(min, max, xFpscev->isInteger && yFpscev->isInteger);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::minimum>(
    IntrinsicInst &inst) {
  const FastMathFlags flags = inst.getFastMathFlags();

  const FPSCEV *const xFpscev = fpse.getFPSCEV(inst.getOperand(0));
  const FPSCEV *const yFpscev = fpse.getFPSCEV(inst.getOperand(1));

  APFloat xMin = applyFastMathFlags(xFpscev->min, flags);
  APFloat xMax = applyFastMathFlags(xFpscev->max, flags);
  APFloat yMin = applyFastMathFlags(yFpscev->min, flags);
  APFloat yMax = applyFastMathFlags(yFpscev->max, flags);

  const fltSemantics &semantics = xMin.getSemantics();

  APFloat min = APFloat::getNaN(semantics, true);

  if (xMin.isFinite() && yMin.isFinite()) {
    // If both our mins are finite, choose the smallest.
    min = getMinimum({xMin, yMin});
  }

  APFloat max = APFloat::getNaN(semantics, false);

  if (xMax.isFinite() && yMax.isFinite()) {
    // If both our maxs are finite, choose the smallest.
    max = getMinimum({xMax, yMax});
  }

  min = applyFastMathFlags(min, flags);
  max = applyFastMathFlags(max, flags);

  fpse.map[&inst] = FPSCEV(min, max, xFpscev->isInteger && yFpscev->isInteger);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::maximum>(
    IntrinsicInst &inst) {
  const FastMathFlags flags = inst.getFastMathFlags();

  const FPSCEV *const xFpscev = fpse.getFPSCEV(inst.getOperand(0));
  const FPSCEV *const yFpscev = fpse.getFPSCEV(inst.getOperand(1));

  APFloat xMin = applyFastMathFlags(xFpscev->min, flags);
  APFloat xMax = applyFastMathFlags(xFpscev->max, flags);
  APFloat yMin = applyFastMathFlags(yFpscev->min, flags);
  APFloat yMax = applyFastMathFlags(yFpscev->max, flags);

  const fltSemantics &semantics = xMin.getSemantics();

  APFloat min = APFloat::getNaN(semantics, true);

  if (xMin.isFinite() && yMin.isFinite()) {
    // If both our mins are finite, choose the largest.
    min = getMaximum({xMin, yMin});
  }

  APFloat max = APFloat::getNaN(semantics, false);

  if (xMax.isFinite() && yMax.isFinite()) {
    // If both our maxs are finite, choose the largest.
    max = getMaximum({xMax, yMax});
  }

  min = applyFastMathFlags(min, flags);
  max = applyFastMathFlags(max, flags);

  fpse.map[&inst] = FPSCEV(min, max, xFpscev->isInteger && yFpscev->isInteger);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::copysign>(
    IntrinsicInst &inst) {
  const FastMathFlags flags = inst.getFastMathFlags();

  const FPSCEV *const xFpscev = fpse.getFPSCEV(inst.getOperand(0));
  const FPSCEV *const yFpscev = fpse.getFPSCEV(inst.getOperand(1));

  APFloat xMin = applyFastMathFlags(xFpscev->min, flags);
  APFloat xMax = applyFastMathFlags(xFpscev->max, flags);
  APFloat yMin = applyFastMathFlags(yFpscev->min, flags);
  APFloat yMax = applyFastMathFlags(yFpscev->max, flags);

  const fltSemantics &semantics = xMin.getSemantics();

  APFloat min = xMin;
  APFloat max = xMax;

  if (yMin.isNegative() && !yMax.isNegative()) {
    // If the range of y includes negative and positive numbers.
    APFloat maxs[2] = {min, max};
    maxs[0].clearSign();
    maxs[1].clearSign();
    max = getMaximum(maxs);

    min = max;
    min.changeSign();
  } else {
    // If the range of x contains positive and negative numbers.
    const bool positiveAndNegative = min.isNegative() && !max.isNegative();

    if (positiveAndNegative) {
      APFloat mins[2] = {min, max};
      mins[1].changeSign();
      min = getMinimum(mins);

      APFloat maxs[2] = {min, max};
      maxs[0].changeSign();
      max = getMaximum(maxs);
    }

    APFloat range[2] = {min, max};

    // Wipe the sign from our range.
    range[0].clearSign();
    range[1].clearSign();

    if (yMax.isNegative()) {
      // If y is all negative, flip the sign of the ranges to negative.
      range[0].changeSign();
      range[1].changeSign();
    }

    min = getMinimum(range);
    max = getMaximum(range);

    if (positiveAndNegative) {
      if (yMax.isNegative()) {
        max = APFloat::getZero(semantics, true);
      } else {
        min = APFloat::getZero(semantics, false);
      }
    }
  }

  min = applyFastMathFlags(min, flags);
  max = applyFastMathFlags(max, flags);

  fpse.map[&inst] = FPSCEV(min, max, xFpscev->isInteger);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::floor>(
    IntrinsicInst &inst) {
  const FastMathFlags flags = inst.getFastMathFlags();

  const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));

  APFloat min = applyFastMathFlags(fpscev->min, flags);
  APFloat max = applyFastMathFlags(fpscev->max, flags);

  min.roundToIntegral(APFloat::rmTowardNegative);
  max.roundToIntegral(APFloat::rmTowardNegative);

  fpse.map[&inst] = FPSCEV(min, max, true);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::ceil>(
    IntrinsicInst &inst) {
  const FastMathFlags flags = inst.getFastMathFlags();

  const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));

  APFloat min = applyFastMathFlags(fpscev->min, flags);
  APFloat max = applyFastMathFlags(fpscev->max, flags);

  min.roundToIntegral(APFloat::rmTowardPositive);
  max.roundToIntegral(APFloat::rmTowardPositive);

  fpse.map[&inst] = FPSCEV(min, max, true);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::trunc>(
    IntrinsicInst &inst) {
  const FastMathFlags flags = inst.getFastMathFlags();

  const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));

  APFloat min = applyFastMathFlags(fpscev->min, flags);
  APFloat max = applyFastMathFlags(fpscev->max, flags);

  min.roundToIntegral(APFloat::rmTowardZero);
  max.roundToIntegral(APFloat::rmTowardZero);

  fpse.map[&inst] = FPSCEV(min, max, true);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::rint>(
    IntrinsicInst &inst) {
  const FastMathFlags flags = inst.getFastMathFlags();

  const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));

  APFloat min = applyFastMathFlags(fpscev->min, flags);
  APFloat max = applyFastMathFlags(fpscev->max, flags);

  min.roundToIntegral(APFloat::rmNearestTiesToEven);
  max.roundToIntegral(APFloat::rmNearestTiesToEven);

  fpse.map[&inst] = FPSCEV(min, max, true);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::nearbyint>(
    IntrinsicInst &inst) {
  const FastMathFlags flags = inst.getFastMathFlags();

  const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));

  APFloat min = applyFastMathFlags(fpscev->min, flags);
  APFloat max = applyFastMathFlags(fpscev->max, flags);

  min.roundToIntegral(APFloat::rmTowardNegative);
  max.roundToIntegral(APFloat::rmTowardPositive);

  fpse.map[&inst] = FPSCEV(min, max, true);
}

template <>
void FPScalarEvolutionPass::visitIntrinsic<Intrinsic::round>(
    IntrinsicInst &inst) {
  const FastMathFlags flags = inst.getFastMathFlags();

  const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));

  APFloat min = applyFastMathFlags(fpscev->min, flags);
  APFloat max = applyFastMathFlags(fpscev->max, flags);

  min.roundToIntegral(APFloat::rmNearestTiesToAway);
  max.roundToIntegral(APFloat::rmNearestTiesToAway);

  fpse.map[&inst] = FPSCEV(min, max, true);
}

void FPScalarEvolutionPass::visitIntrinsicInst(IntrinsicInst &inst) {
  // Skip any intrinsic that doesn't result in a floating point.
  if (!inst.getType()->isFloatingPointTy()) {
    return;
  }

  switch (inst.getIntrinsicID()) {
  default:
    break;
#define INTRINSIC_VISIT(x)                                                     \
  case x:                                                                      \
    return visitIntrinsic<x>(inst)
    INTRINSIC_VISIT(Intrinsic::sqrt);
    INTRINSIC_VISIT(Intrinsic::pow);
    INTRINSIC_VISIT(Intrinsic::powi);
    INTRINSIC_VISIT(Intrinsic::sin);
    INTRINSIC_VISIT(Intrinsic::cos);
    INTRINSIC_VISIT(Intrinsic::exp);
    INTRINSIC_VISIT(Intrinsic::exp2);
    INTRINSIC_VISIT(Intrinsic::log);
    INTRINSIC_VISIT(Intrinsic::log10);
    INTRINSIC_VISIT(Intrinsic::log2);
    INTRINSIC_VISIT(Intrinsic::fma);
    INTRINSIC_VISIT(Intrinsic::fabs);
    INTRINSIC_VISIT(Intrinsic::minnum);
    INTRINSIC_VISIT(Intrinsic::maxnum);
    INTRINSIC_VISIT(Intrinsic::minimum);
    INTRINSIC_VISIT(Intrinsic::maximum);
    INTRINSIC_VISIT(Intrinsic::copysign);
    INTRINSIC_VISIT(Intrinsic::floor);
    INTRINSIC_VISIT(Intrinsic::ceil);
    INTRINSIC_VISIT(Intrinsic::trunc);
    INTRINSIC_VISIT(Intrinsic::rint);
    INTRINSIC_VISIT(Intrinsic::nearbyint);
    INTRINSIC_VISIT(Intrinsic::round);
#undef INTRINSIC_VISIT
  }

  FPSCEV fpscev(inst.getType());

  const FastMathFlags flags = inst.getFastMathFlags();
  fpscev.min = applyFastMathFlags(fpscev.min, flags);
  fpscev.max = applyFastMathFlags(fpscev.max, flags);

  fpse.map[&inst] = fpscev;
}

} // namespace

char FPScalarEvolutionPass::ID;

namespace {
struct FastMathPropagationPass final : public FunctionPass,
                                       InstVisitor<FastMathPropagationPass> {
  FastMathPropagationPass() : FunctionPass(ID) {}

  bool runOnFunction(Function &function) override {
    fpse = &getAnalysis<FPScalarEvolutionPass>().getFPSCEV();
    modified = false;
    visit(function);
    return modified;
  }

  void visitFCmpInst(FCmpInst &inst) {
    const FPSCEV *const xFpscev = fpse->getFPSCEV(inst.getOperand(0));
    const FPSCEV *const yFpscev = fpse->getFPSCEV(inst.getOperand(1));

    if (!xFpscev || !yFpscev) {
      return;
    }

    if (xFpscev->isFinite() && yFpscev->isFinite()) {
      inst.setHasNoInfs(true);
      modified = true;
    }

    if (!xFpscev->isNaN() && !yFpscev->isNaN()) {
      inst.setHasNoNaNs(true);
      modified = true;
    }
  }

  void visitUnaryOperator(UnaryOperator &inst) {
    switch (inst.getOpcode()) {
    default:
      return;
    case Instruction::FNeg:
      break;
    }

    const FPSCEV *const fpscev = fpse->getFPSCEV(inst.getOperand(0));

    if (fpscev->isFinite()) {
      inst.setHasNoInfs(true);
      modified = true;
    }

    if (!fpscev->isNaN()) {
      inst.setHasNoNaNs(true);
      modified = true;
    }
  }

  void visitBinaryOperator(BinaryOperator &inst) {
    switch (inst.getOpcode()) {
    default:
      return;
    case Instruction::FAdd:
    case Instruction::FSub:
    case Instruction::FMul:
    case Instruction::FDiv:
    case Instruction::FRem:
      break;
    }

    const FPSCEV *const xFpscev = fpse->getFPSCEV(inst.getOperand(0));
    const FPSCEV *const yFpscev = fpse->getFPSCEV(inst.getOperand(1));
    const FPSCEV *const fpscev = fpse->getFPSCEV(&inst);

    if (fpscev->isFinite() && xFpscev->isFinite() && yFpscev->isFinite()) {
      inst.setHasNoInfs(true);
      modified = true;
    }

    if (!fpscev->isNaN() && !xFpscev->isNaN() && !yFpscev->isNaN()) {
      inst.setHasNoNaNs(true);
      modified = true;
    }
  }

  void visitIntrinsicInst(IntrinsicInst &inst) {
    bool atLeastOneFP = false;
    bool allFinite = true;
    bool allNotNaN = true;

    for (Value *const arg : inst.args()) {
      if (arg->getType()->isFloatingPointTy()) {
        const FPSCEV *const fpscev = fpse->getFPSCEV(arg);

        atLeastOneFP = true;
        allFinite = allFinite && fpscev->isFinite();
        allNotNaN = allNotNaN && !fpscev->isNaN();
      }
    }

    if (inst.getType()->isFloatingPointTy()) {
      const FPSCEV *const fpscev = fpse->getFPSCEV(&inst);

      atLeastOneFP = true;
      allFinite = allFinite && fpscev->isFinite();
      allNotNaN = allNotNaN && !fpscev->isNaN();
    }

    if (atLeastOneFP) {
      if (allFinite) {
        inst.setHasNoInfs(true);
        modified = true;
      }

      if (allNotNaN) {
        inst.setHasNoNaNs(true);
        modified = true;
      }
    }
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<FPScalarEvolutionPass>();
    AU.addPreserved<FPScalarEvolutionPass>();
  }

  // The ID of this pass - the address of which is used by LLVM to uniquely
  // identify the pass.
  static char ID;

private:
  FPScalarEvolution *fpse;
  bool modified;
};
} // namespace

char FastMathPropagationPass::ID;

namespace {
struct FPInstSimplifyPass final : public FunctionPass,
                                  InstVisitor<FPInstSimplifyPass> {
  FPInstSimplifyPass() : FunctionPass(ID) {}

  bool runOnFunction(Function &function) override {
    fpse = &getAnalysis<FPScalarEvolutionPass>().getFPSCEV();
    modified = false;
    visit(function);

    for (Instruction *const inst : toRemoves) {
      fpse->forgetValue(inst);
      inst->eraseFromParent();
    }

    modified |= !toRemoves.empty();

    toRemoves.clear();
    return modified;
  }

  void visitFCmpInst(FCmpInst &inst) {
    FastMathFlags fmf = inst.getFastMathFlags();
    const FPSCEV xFpscev =
        fpse->getFPSCEV(inst.getOperand(0))->cloneWithFastMathFlags(fmf);
    const FPSCEV yFpscev =
        fpse->getFPSCEV(inst.getOperand(1))->cloneWithFastMathFlags(fmf);

    CmpInst::Predicate predicate = CmpInst::BAD_FCMP_PREDICATE;

    switch (inst.getPredicate()) {
    default:
      return;
    case CmpInst::FCMP_OEQ:
      if (!xFpscev.isNaN() && !yFpscev.isNaN()) {
        if (!xFpscev.hasOverlappingRange(yFpscev)) {
          predicate = CmpInst::FCMP_FALSE;
        }
      }
      break;
    case CmpInst::FCMP_OGT:
      if (!xFpscev.isNaN() && !yFpscev.isNaN()) {
        if (xFpscev.isGreaterThan(yFpscev)) {
          predicate = CmpInst::FCMP_TRUE;
        } else if (xFpscev.isLessThanEqual(yFpscev)) {
          predicate = CmpInst::FCMP_FALSE;
        }
      }
      break;
    case CmpInst::FCMP_OGE:
      if (!xFpscev.isNaN() && !yFpscev.isNaN()) {
        if (xFpscev.isGreaterThanEqual(yFpscev)) {
          predicate = CmpInst::FCMP_TRUE;
        } else if (xFpscev.isLessThan(yFpscev)) {
          predicate = CmpInst::FCMP_FALSE;
        }
      }
      break;
    case CmpInst::FCMP_OLT:
      if (!xFpscev.isNaN() && !yFpscev.isNaN()) {
        if (xFpscev.isLessThan(yFpscev)) {
          predicate = CmpInst::FCMP_TRUE;
        } else if (xFpscev.isGreaterThanEqual(yFpscev)) {
          predicate = CmpInst::FCMP_FALSE;
        }
      }
      break;
    case CmpInst::FCMP_OLE:
      if (!xFpscev.isNaN() && !yFpscev.isNaN()) {
        if (xFpscev.isLessThanEqual(yFpscev)) {
          predicate = CmpInst::FCMP_TRUE;
        } else if (xFpscev.isGreaterThan(yFpscev)) {
          predicate = CmpInst::FCMP_FALSE;
        }
      }
      break;
    case CmpInst::FCMP_ONE:
      if (!xFpscev.isNaN() && !yFpscev.isNaN()) {
        if (!xFpscev.hasOverlappingRange(yFpscev)) {
          predicate = CmpInst::FCMP_TRUE;
        }
      }
      break;
    case CmpInst::FCMP_ORD:
      if (!xFpscev.isNaN() && !yFpscev.isNaN()) {
        predicate = CmpInst::FCMP_TRUE;
      }
      break;
    case CmpInst::FCMP_UNO:
      if (!xFpscev.isNaN() && !yFpscev.isNaN()) {
        predicate = CmpInst::FCMP_FALSE;
      }
      break;
    case CmpInst::FCMP_UEQ:
      if (!xFpscev.hasOverlappingRange(yFpscev)) {
        predicate = CmpInst::FCMP_FALSE;
      }
      break;
    case CmpInst::FCMP_UGT:
      if (xFpscev.isGreaterThan(yFpscev)) {
        predicate = CmpInst::FCMP_TRUE;
      } else if (xFpscev.isLessThanEqual(yFpscev)) {
        predicate = CmpInst::FCMP_FALSE;
      }
      break;
    case CmpInst::FCMP_UGE:
      if (xFpscev.isGreaterThanEqual(yFpscev)) {
        predicate = CmpInst::FCMP_TRUE;
      } else if (xFpscev.isLessThan(yFpscev)) {
        predicate = CmpInst::FCMP_FALSE;
      }
      break;
    case CmpInst::FCMP_ULT:
      if (xFpscev.isLessThan(yFpscev)) {
        predicate = CmpInst::FCMP_TRUE;
      } else if (xFpscev.isGreaterThanEqual(yFpscev)) {
        predicate = CmpInst::FCMP_FALSE;
      }
      break;
    case CmpInst::FCMP_ULE:
      if (xFpscev.isLessThanEqual(yFpscev)) {
        predicate = CmpInst::FCMP_TRUE;
      } else if (xFpscev.isGreaterThan(yFpscev)) {
        predicate = CmpInst::FCMP_FALSE;
      }
      break;
    case CmpInst::FCMP_UNE:
      if (!xFpscev.hasOverlappingRange(yFpscev)) {
        predicate = CmpInst::FCMP_TRUE;
      }
      break;
    }

    if (predicate != CmpInst::BAD_FCMP_PREDICATE) {
      inst.setPredicate(predicate);
      modified = true;
    }
  }

  template <Intrinsic::ID> void visitIntrinsic(IntrinsicInst &inst);

  void visitIntrinsicInst(IntrinsicInst &inst);

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<FPScalarEvolutionPass>();
    AU.addPreserved<FPScalarEvolutionPass>();
  }

  // The ID of this pass - the address of which is used by LLVM to uniquely
  // identify the pass.
  static char ID;

private:
  FPScalarEvolution *fpse;
  bool modified;
  SmallVector<Instruction *, 8> toRemoves;
};

template <>
void FPInstSimplifyPass::visitIntrinsic<Intrinsic::sqrt>(IntrinsicInst &inst) {
  FastMathFlags fmf = inst.getFastMathFlags();
  Value *const x = inst.getOperand(0);
  const FPSCEV xFpscev = fpse->getFPSCEV(x)->cloneWithFastMathFlags(fmf);

  // If the input is all negative, the result is NaN.
  if (xFpscev.isAllNegative()) {
    // If the instruction cannot return NaNs, replace it with undef.
    if (inst.hasNoNaNs()) {
      inst.replaceAllUsesWith(UndefValue::get(inst.getType()));
    } else {
      inst.replaceAllUsesWith(ConstantFP::getNaN(inst.getType()));
    }

    toRemoves.push_back(&inst);
  }
}

template <>
void FPInstSimplifyPass::visitIntrinsic<Intrinsic::pow>(IntrinsicInst &inst) {
  FastMathFlags fmf = inst.getFastMathFlags();
  Value *const x = inst.getOperand(0);
  Value *const y = inst.getOperand(1);
  const FPSCEV xFpscev = fpse->getFPSCEV(x)->cloneWithFastMathFlags(fmf);
  const FPSCEV yFpscev = fpse->getFPSCEV(y)->cloneWithFastMathFlags(fmf);

  // If y is an integer
  if (yFpscev.isInteger) {
    const fltSemantics &semantics = xFpscev.min.getSemantics();

    // 2^24 is the last integer number that we can fully represent in both
    // floating-point and i32.
    const APInt minInt(32, -(1 << 24), true);
    const APInt maxInt(32, 1 << 24, true);

    // If y is between min/max int, we can use powi instead of pow!
    if (yFpscev.isGreaterThanEqual(getFromInt(semantics, minInt, true)) &&
        yFpscev.isLessThanEqual(getFromInt(semantics, maxInt, true))) {
      IRBuilder<> irb(&inst);
      StringRef yName(y->getName());
      Twine name(yName, ".i32cast");
      Value *const yCast = irb.CreateFPToSI(y, irb.getInt32Ty(), name);

      Instruction *const powi = irb.CreateIntrinsic(
          Intrinsic::powi, inst.getType(), {x, yCast}, &inst);
      powi->takeName(&inst);

      // Copy the fpscev information from the original instruction onto the new.
      fpse->copyFPSCEVFromValue(powi, &inst);

      inst.replaceAllUsesWith(powi);

      toRemoves.push_back(&inst);
    }
  }
}

template <>
void FPInstSimplifyPass::visitIntrinsic<Intrinsic::fabs>(IntrinsicInst &inst) {
  FastMathFlags fmf = inst.getFastMathFlags();
  Value *const x = inst.getOperand(0);
  const FPSCEV xFpscev = fpse->getFPSCEV(x)->cloneWithFastMathFlags(fmf);

  // If the input is all not negative, we can just use the input.
  if (xFpscev.isAllNonNegative()) {
    inst.replaceAllUsesWith(x);
    toRemoves.push_back(&inst);
  }
}

template <>
void FPInstSimplifyPass::visitIntrinsic<Intrinsic::minnum>(
    IntrinsicInst &inst) {
  FastMathFlags fmf = inst.getFastMathFlags();
  Value *const x = inst.getOperand(0);
  Value *const y = inst.getOperand(1);
  const FPSCEV xFpscev = fpse->getFPSCEV(x)->cloneWithFastMathFlags(fmf);
  const FPSCEV yFpscev = fpse->getFPSCEV(y)->cloneWithFastMathFlags(fmf);

  // If one of the inputs is always less than the other, we fold away the
  // intrinsic.
  if (xFpscev.isLessThanEqual(yFpscev)) {
    inst.replaceAllUsesWith(x);
    toRemoves.push_back(&inst);
  } else if (yFpscev.isLessThanEqual(xFpscev)) {
    inst.replaceAllUsesWith(y);
    toRemoves.push_back(&inst);
  }
}

template <>
void FPInstSimplifyPass::visitIntrinsic<Intrinsic::maxnum>(
    IntrinsicInst &inst) {
  FastMathFlags fmf = inst.getFastMathFlags();
  Value *const x = inst.getOperand(0);
  Value *const y = inst.getOperand(1);
  const FPSCEV xFpscev = fpse->getFPSCEV(x)->cloneWithFastMathFlags(fmf);
  const FPSCEV yFpscev = fpse->getFPSCEV(y)->cloneWithFastMathFlags(fmf);

  // If one of the inputs is always less than the other, we fold away the
  // intrinsic.
  if (xFpscev.isGreaterThanEqual(yFpscev)) {
    inst.replaceAllUsesWith(x);
    toRemoves.push_back(&inst);
  } else if (yFpscev.isGreaterThanEqual(xFpscev)) {
    inst.replaceAllUsesWith(y);
    toRemoves.push_back(&inst);
  }
}

template <>
void FPInstSimplifyPass::visitIntrinsic<Intrinsic::minimum>(
    IntrinsicInst &inst) {
  FastMathFlags fmf = inst.getFastMathFlags();
  Value *const x = inst.getOperand(0);
  Value *const y = inst.getOperand(1);
  const FPSCEV xFpscev = fpse->getFPSCEV(x)->cloneWithFastMathFlags(fmf);
  const FPSCEV yFpscev = fpse->getFPSCEV(y)->cloneWithFastMathFlags(fmf);

  // If one of the inputs is always less than the other, we fold away the
  // intrinsic.
  if (xFpscev.isLessThanEqual(yFpscev)) {
    inst.replaceAllUsesWith(x);
    toRemoves.push_back(&inst);
  } else if (yFpscev.isLessThanEqual(xFpscev)) {
    inst.replaceAllUsesWith(y);
    toRemoves.push_back(&inst);
  }
}

template <>
void FPInstSimplifyPass::visitIntrinsic<Intrinsic::maximum>(
    IntrinsicInst &inst) {
  FastMathFlags fmf = inst.getFastMathFlags();
  Value *const x = inst.getOperand(0);
  Value *const y = inst.getOperand(1);
  const FPSCEV xFpscev = fpse->getFPSCEV(x)->cloneWithFastMathFlags(fmf);
  const FPSCEV yFpscev = fpse->getFPSCEV(y)->cloneWithFastMathFlags(fmf);

  // If one of the inputs is always less than the other, we fold away the
  // intrinsic.
  if (xFpscev.isGreaterThanEqual(yFpscev)) {
    inst.replaceAllUsesWith(x);
    toRemoves.push_back(&inst);
  } else if (yFpscev.isGreaterThanEqual(xFpscev)) {
    inst.replaceAllUsesWith(y);
    toRemoves.push_back(&inst);
  }
}

template <>
void FPInstSimplifyPass::visitIntrinsic<Intrinsic::copysign>(
    IntrinsicInst &inst) {
  FastMathFlags fmf = inst.getFastMathFlags();
  Value *const x = inst.getOperand(0);
  Value *const y = inst.getOperand(1);
  const FPSCEV xFpscev = fpse->getFPSCEV(x)->cloneWithFastMathFlags(fmf);
  const FPSCEV yFpscev = fpse->getFPSCEV(y)->cloneWithFastMathFlags(fmf);

  if ((xFpscev.isAllNonNegative() && yFpscev.isAllNonNegative()) ||
      (xFpscev.isAllNegative() && yFpscev.isAllNegative())) {
    inst.replaceAllUsesWith(x);
    toRemoves.push_back(&inst);
  } else if ((xFpscev.isAllNonNegative() && yFpscev.isAllNegative()) ||
             (xFpscev.isAllNegative() && yFpscev.isAllNonNegative())) {
    IRBuilder<> irb(&inst);
    Value *const value = irb.CreateFNeg(x);
    value->takeName(&inst);

    // Copy the fpscev information from the original instruction onto the new.
    fpse->copyFPSCEVFromValue(value, &inst);

    if (Instruction *const otherInst = dyn_cast<Instruction>(value)) {
      otherInst->setFastMathFlags(inst.getFastMathFlags());
    }

    inst.replaceAllUsesWith(value);
    toRemoves.push_back(&inst);
  } else if (yFpscev.isAllNonNegative()) {
    IRBuilder<> irb(&inst);
    Instruction *const fabs =
        irb.CreateUnaryIntrinsic(Intrinsic::fabs, x, &inst);
    fabs->takeName(&inst);
    // Copy the fpscev information from the original instruction onto the new.
    fpse->copyFPSCEVFromValue(fabs, &inst);
    inst.replaceAllUsesWith(fabs);
    toRemoves.push_back(&inst);
  }
}

template <>
void FPInstSimplifyPass::visitIntrinsic<Intrinsic::floor>(IntrinsicInst &inst) {
  FastMathFlags fmf = inst.getFastMathFlags();
  Value *const x = inst.getOperand(0);
  const FPSCEV xFpscev = fpse->getFPSCEV(x)->cloneWithFastMathFlags(fmf);

  // If the input is already an integer, floor is a no-op.
  if (xFpscev.isInteger) {
    inst.replaceAllUsesWith(x);
    toRemoves.push_back(&inst);
  }
}

template <>
void FPInstSimplifyPass::visitIntrinsic<Intrinsic::ceil>(IntrinsicInst &inst) {
  FastMathFlags fmf = inst.getFastMathFlags();
  Value *const x = inst.getOperand(0);
  const FPSCEV xFpscev = fpse->getFPSCEV(x)->cloneWithFastMathFlags(fmf);

  // If the input is already an integer, floor is a no-op.
  if (xFpscev.isInteger) {
    inst.replaceAllUsesWith(x);
    toRemoves.push_back(&inst);
  }
}

template <>
void FPInstSimplifyPass::visitIntrinsic<Intrinsic::trunc>(IntrinsicInst &inst) {
  FastMathFlags fmf = inst.getFastMathFlags();
  Value *const x = inst.getOperand(0);
  const FPSCEV xFpscev = fpse->getFPSCEV(x)->cloneWithFastMathFlags(fmf);

  // If the input is already an integer, floor is a no-op.
  if (xFpscev.isInteger) {
    inst.replaceAllUsesWith(x);
    toRemoves.push_back(&inst);
  }
}

template <>
void FPInstSimplifyPass::visitIntrinsic<Intrinsic::rint>(IntrinsicInst &inst) {
  FastMathFlags fmf = inst.getFastMathFlags();
  Value *const x = inst.getOperand(0);
  const FPSCEV xFpscev = fpse->getFPSCEV(x)->cloneWithFastMathFlags(fmf);

  // If the input is already an integer, floor is a no-op.
  if (xFpscev.isInteger) {
    inst.replaceAllUsesWith(x);
    toRemoves.push_back(&inst);
  }
}

template <>
void FPInstSimplifyPass::visitIntrinsic<Intrinsic::nearbyint>(
    IntrinsicInst &inst) {
  FastMathFlags fmf = inst.getFastMathFlags();
  Value *const x = inst.getOperand(0);
  const FPSCEV xFpscev = fpse->getFPSCEV(x)->cloneWithFastMathFlags(fmf);

  // If the input is already an integer, floor is a no-op.
  if (xFpscev.isInteger) {
    inst.replaceAllUsesWith(x);
    toRemoves.push_back(&inst);
  }
}

template <>
void FPInstSimplifyPass::visitIntrinsic<Intrinsic::round>(IntrinsicInst &inst) {
  FastMathFlags fmf = inst.getFastMathFlags();
  Value *const x = inst.getOperand(0);
  const FPSCEV xFpscev = fpse->getFPSCEV(x)->cloneWithFastMathFlags(fmf);

  // If the input is already an integer, floor is a no-op.
  if (xFpscev.isInteger) {
    inst.replaceAllUsesWith(x);
    toRemoves.push_back(&inst);
  }
}

void FPInstSimplifyPass::visitIntrinsicInst(IntrinsicInst &inst) {
  switch (inst.getIntrinsicID()) {
  default:
    return;
#define INTRINSIC_VISIT(x)                                                     \
  case x:                                                                      \
    return visitIntrinsic<x>(inst)
    INTRINSIC_VISIT(Intrinsic::sqrt);
    INTRINSIC_VISIT(Intrinsic::pow);
    INTRINSIC_VISIT(Intrinsic::fabs);
    INTRINSIC_VISIT(Intrinsic::minnum);
    INTRINSIC_VISIT(Intrinsic::maxnum);
    INTRINSIC_VISIT(Intrinsic::minimum);
    INTRINSIC_VISIT(Intrinsic::maximum);
    INTRINSIC_VISIT(Intrinsic::copysign);
    INTRINSIC_VISIT(Intrinsic::floor);
    INTRINSIC_VISIT(Intrinsic::ceil);
    INTRINSIC_VISIT(Intrinsic::trunc);
    INTRINSIC_VISIT(Intrinsic::rint);
    INTRINSIC_VISIT(Intrinsic::nearbyint);
    INTRINSIC_VISIT(Intrinsic::round);
#undef INTRINSIC_VISIT
  }
}
} // namespace

char FPInstSimplifyPass::ID;

namespace llvm {
void initializeFPScalarEvolutionPassPass(PassRegistry &);
void initializeFastMathPropagationPassPass(PassRegistry &);
void initializeFPInstSimplifyPassPass(PassRegistry &);

Pass *createFastMathPropagationPass() { return new FastMathPropagationPass(); }
Pass *createFPInstSimplifyPass() { return new FPInstSimplifyPass(); }
} // namespace llvm

INITIALIZE_PASS_BEGIN(FPScalarEvolutionPass, "fp-scalar-evolution",
                      "Floating Point Scalar Evolution Analysis", false, true);
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass);
INITIALIZE_PASS_END(FPScalarEvolutionPass, "fp-scalar-evolution",
                    "Floating Point Scalar Evolution Analysis", false, true);

INITIALIZE_PASS_BEGIN(FastMathPropagationPass, "fast-math-propagation",
                      "Fast Math Propagation", false, false);
INITIALIZE_PASS_DEPENDENCY(FPScalarEvolutionPass);
INITIALIZE_PASS_END(FastMathPropagationPass, "fast-math-propagation",
                    "Fast Math Propagation", false, false);

INITIALIZE_PASS_BEGIN(FPInstSimplifyPass, "fp-inst-simplify",
                      "Floating Point Instruction Simplification", false,
                      false);
INITIALIZE_PASS_DEPENDENCY(FPScalarEvolutionPass);
INITIALIZE_PASS_END(FPInstSimplifyPass, "fp-inst-simplify",
                    "Floating Point Instruction Simplification", false, false);
