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

#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/Analysis/ScalarEvolution.h"
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
};

struct FPScalarEvolution final {
  ValueMap<Value *, FPSCEV> map;

  FPSCEV *getFPSCEV(Value *const value) {
    if (map.count(value) == 0) {
      if (ConstantFP *const constant = dyn_cast<ConstantFP>(value)) {
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

  const FPSCEV *getFPSCEV(Value *const value) const {
    auto iterator = map.find(value);

    if (iterator == map.end()) {
      return nullptr;
    }

    return &iterator->second;
  }
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
      Value &inst = *fpscev.first;
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

  void visitSqrt(IntrinsicInst &inst) {
    const FastMathFlags flags = inst.getFastMathFlags();

    const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));

    APFloat min = applyFastMathFlags(fpscev->min, flags);
    APFloat max = applyFastMathFlags(fpscev->max, flags);

    const fltSemantics &semantics = min.getSemantics();

    const APFloat nan = APFloat::getNaN(semantics, false);
    const APFloat zero = APFloat::getZero(semantics, false);

    // If the upper bound of the range is negative, all inputs are negative and
    // we always produce a NaN.
    if (max.isNegative()) {
      min = nan;
    }

    // If min is negative or max is potentially a NaN, the output max is at most
    // a NaN.
    if (min.isNegative() || max.isNaN()) {
      max = nan;
    }

    min = zero;
    max = applyFastMathFlags(fpscev->max, flags);

    // The sqrt of something is at most (input is NaN or infinity) as big as the
    // maximum input value, and as small as zero.
    fpse.map[&inst] = FPSCEV(zero, max, false);
  }

  void visitPowi(IntrinsicInst &inst) {
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

  void visitTrig(IntrinsicInst &inst) {
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

  void visitExp(IntrinsicInst &inst) {
    FPSCEV fpscev(inst.getType());

    // Exp always has a positive result.
    const APFloat zero = APFloat::getZero(fpscev.min.getSemantics(), false);
    fpscev.min = zero;

    const FastMathFlags flags = inst.getFastMathFlags();
    fpscev.max = applyFastMathFlags(fpscev.max, flags);

    fpse.map[&inst] = fpscev;
  }

  void visitLog(IntrinsicInst &inst) {
    const FastMathFlags flags = inst.getFastMathFlags();

    const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));
    APFloat min = applyFastMathFlags(fpscev->min, flags);
    APFloat max = applyFastMathFlags(fpscev->max, flags);

    const fltSemantics &semantics = min.getSemantics();

    // If the full range is negative, always returns a NaN.
    if (max.isNegative()) {
      APFloat nan = APFloat::getNaN(semantics, false);
      fpse.map[&inst] = FPSCEV(nan, nan, false);
      return;
    }

    FPSCEV newFpscev(inst.getType());

    newFpscev.min = applyFastMathFlags(newFpscev.min, flags);

    // If min is greater than 1, we definitely return a positive result.
    if (isInRange(min, getOne(semantics),
                  APFloat::getLargest(semantics, false))) {
      newFpscev.min = APFloat::getZero(semantics, false);
    } else if (!min.isNegative()) {
      // If the range is entirely positive, then min is not infinity.
      if (min.isNonZero()) {
        // If min is greater than zero, the minimum is at least not infinity.
        newFpscev.min = APFloat::getLargest(semantics, true);
      } else {
        newFpscev.min = APFloat::getInf(semantics, true);
      }
    }

    // We could do a lot better than this if we had a way to calculate the log
    // of an APFloat. Our best assumption that we can make at present is that
    // the maximum result is no larger than the original maximum.
    newFpscev.max = max;

    fpse.map[&inst] = newFpscev;
  }

  void visitFma(IntrinsicInst &inst) {
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

  void visitFabs(IntrinsicInst &inst) {
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

  void visitMinnum(IntrinsicInst &inst) {
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
      min = APFloat::getLargest(semantics,
                                xMin.isNegative() || yMin.isNegative());
    }

    APFloat max = APFloat::getNaN(semantics, false);

    if (xMax.isFinite() && yMax.isFinite()) {
      // If both our maxs are finite, choose the smallest.
      max = getMinimum({xMax, yMax});
    } else if (xMax.isFinite() ^ yMax.isFinite()) {
      // If one of our maxs is finite, we definitely do not produce a NaN.
      max = APFloat::getLargest(semantics,
                                xMax.isNegative() || yMax.isNegative());
    }

    min = applyFastMathFlags(min, flags);
    max = applyFastMathFlags(max, flags);

    fpse.map[&inst] =
        FPSCEV(min, max, xFpscev->isInteger && yFpscev->isInteger);
  }

  void visitMaxnum(IntrinsicInst &inst) {
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
      min = APFloat::getLargest(semantics,
                                xMin.isNegative() || yMin.isNegative());
    }

    APFloat max = APFloat::getNaN(semantics, false);

    if (xMax.isFinite() && yMax.isFinite()) {
      // If both our maxs are finite, choose the largest.
      max = getMaximum({xMax, yMax});
    } else if (xMax.isFinite() ^ yMax.isFinite()) {
      // If one of our maxs is finite, we definitely do not produce a NaN.
      max = APFloat::getLargest(semantics,
                                xMax.isNegative() || yMax.isNegative());
    }

    min = applyFastMathFlags(min, flags);
    max = applyFastMathFlags(max, flags);

    fpse.map[&inst] =
        FPSCEV(min, max, xFpscev->isInteger && yFpscev->isInteger);
  }

  void visitMinimum(IntrinsicInst &inst) {
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

    fpse.map[&inst] =
        FPSCEV(min, max, xFpscev->isInteger && yFpscev->isInteger);
  }

  void visitMaximum(IntrinsicInst &inst) {
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

    fpse.map[&inst] =
        FPSCEV(min, max, xFpscev->isInteger && yFpscev->isInteger);
  }

  void visitCopysign(IntrinsicInst &inst) {
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

  void visitFloor(IntrinsicInst &inst) {
    const FastMathFlags flags = inst.getFastMathFlags();

    const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));

    APFloat min = applyFastMathFlags(fpscev->min, flags);
    APFloat max = applyFastMathFlags(fpscev->max, flags);

    min.roundToIntegral(APFloat::rmTowardNegative);
    max.roundToIntegral(APFloat::rmTowardNegative);

    fpse.map[&inst] = FPSCEV(min, max, true);
  }

  void visitCeil(IntrinsicInst &inst) {
    const FastMathFlags flags = inst.getFastMathFlags();

    const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));

    APFloat min = applyFastMathFlags(fpscev->min, flags);
    APFloat max = applyFastMathFlags(fpscev->max, flags);

    min.roundToIntegral(APFloat::rmTowardPositive);
    max.roundToIntegral(APFloat::rmTowardPositive);

    fpse.map[&inst] = FPSCEV(min, max, true);
  }

  void visitTruncIntr(IntrinsicInst &inst) {
    const FastMathFlags flags = inst.getFastMathFlags();

    const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));

    APFloat min = applyFastMathFlags(fpscev->min, flags);
    APFloat max = applyFastMathFlags(fpscev->max, flags);

    min.roundToIntegral(APFloat::rmTowardZero);
    max.roundToIntegral(APFloat::rmTowardZero);

    fpse.map[&inst] = FPSCEV(min, max, true);
  }

  void visitRint(IntrinsicInst &inst) {
    const FastMathFlags flags = inst.getFastMathFlags();

    const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));

    APFloat min = applyFastMathFlags(fpscev->min, flags);
    APFloat max = applyFastMathFlags(fpscev->max, flags);

    min.roundToIntegral(APFloat::rmNearestTiesToEven);
    max.roundToIntegral(APFloat::rmNearestTiesToEven);

    fpse.map[&inst] = FPSCEV(min, max, true);
  }

  void visitNearbyInt(IntrinsicInst &inst) {
    const FastMathFlags flags = inst.getFastMathFlags();

    const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));

    APFloat min = applyFastMathFlags(fpscev->min, flags);
    APFloat max = applyFastMathFlags(fpscev->max, flags);

    min.roundToIntegral(APFloat::rmTowardNegative);
    max.roundToIntegral(APFloat::rmTowardPositive);

    fpse.map[&inst] = FPSCEV(min, max, true);
  }

  void visitRound(IntrinsicInst &inst) {
    const FastMathFlags flags = inst.getFastMathFlags();

    const FPSCEV *const fpscev = fpse.getFPSCEV(inst.getOperand(0));

    APFloat min = applyFastMathFlags(fpscev->min, flags);
    APFloat max = applyFastMathFlags(fpscev->max, flags);

    min.roundToIntegral(APFloat::rmNearestTiesToAway);
    max.roundToIntegral(APFloat::rmNearestTiesToAway);

    fpse.map[&inst] = FPSCEV(min, max, true);
  }

  void visitIntrinsicInst(IntrinsicInst &inst) {
    // Skip any intrinsic that doesn't result in a floating point.
    if (!inst.getType()->isFloatingPointTy()) {
      return;
    }

    switch (inst.getIntrinsicID()) {
    default:
      break;
    case Intrinsic::sqrt:
      visitSqrt(inst);
      return;
    case Intrinsic::powi:
      visitPowi(inst);
      return;
    case Intrinsic::sin:
      visitTrig(inst);
      return;
    case Intrinsic::cos:
      visitTrig(inst);
      return;
    case Intrinsic::exp:
      visitExp(inst);
      return;
    case Intrinsic::exp2:
      visitExp(inst);
      return;
    case Intrinsic::log:
      visitLog(inst);
      return;
    case Intrinsic::log10:
      visitLog(inst);
      return;
    case Intrinsic::log2:
      visitLog(inst);
      return;
    case Intrinsic::fma:
      visitFma(inst);
      return;
    case Intrinsic::fabs:
      visitFabs(inst);
      return;
    case Intrinsic::minnum:
      visitMinnum(inst);
      return;
    case Intrinsic::maxnum:
      visitMaxnum(inst);
      return;
    case Intrinsic::minimum:
      visitMinimum(inst);
      return;
    case Intrinsic::maximum:
      visitMaximum(inst);
      return;
    case Intrinsic::copysign:
      visitCopysign(inst);
      return;
    case Intrinsic::floor:
      visitFloor(inst);
      return;
    case Intrinsic::ceil:
      visitCeil(inst);
      return;
    case Intrinsic::trunc:
      visitTruncIntr(inst);
      return;
    case Intrinsic::rint:
      visitRint(inst);
      return;
    case Intrinsic::nearbyint:
      visitNearbyInt(inst);
      return;
    case Intrinsic::round:
      visitRound(inst);
      return;
    }

    FPSCEV fpscev(inst.getType());

    const FastMathFlags flags = inst.getFastMathFlags();
    fpscev.min = applyFastMathFlags(fpscev.min, flags);
    fpscev.max = applyFastMathFlags(fpscev.max, flags);

    fpse.map[&inst] = fpscev;
  }

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

  const FPScalarEvolution &getFPSCEV() const { return fpse; }

private:
  FPScalarEvolution fpse;
  ScalarEvolution *scalarEvolution;
};
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
      const FPSCEV *const fpscev = fpse->getFPSCEV(arg);

      if (fpscev) {
        atLeastOneFP = true;
        allFinite = allFinite && fpscev->isFinite();
        allNotNaN = allNotNaN && !fpscev->isNaN();
      }
    }

    const FPSCEV *const fpscev = fpse->getFPSCEV(&inst);

    if (fpscev) {
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
  }

  // The ID of this pass - the address of which is used by LLVM to uniquely
  // identify the pass.
  static char ID;

private:
  const FPScalarEvolution *fpse;
  bool modified;
};
} // namespace

char FastMathPropagationPass::ID;

namespace llvm {
void initializeFPScalarEvolutionPassPass(PassRegistry &);
void initializeFastMathPropagationPassPass(PassRegistry &);

Pass *createFPScalarEvolutionPass() { return new FPScalarEvolutionPass(); }
Pass *createFastMathPropagationPass() { return new FastMathPropagationPass(); }
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
