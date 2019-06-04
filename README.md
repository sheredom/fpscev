# Floating-Point Scalar Evolution

[![Build Status](https://travis-ci.org/sheredom/fpscev.svg)](https://travis-ci.org/sheredom/fpscev)

This repo provides an investigation into doing a floating-point scalar
evolution in LLVM.

## What is Scalar Evolution?

Scalar evolution lets a compiler understand the scope of a value throughout a
program. The classic place that scalar evolution is used in compiler
optimizations is with respect to loop index variables to let a compiler infer
the number of loop iterations that a loop will perform.

Scalar evolution can also be used to fold branches away. Imagine you have code
like:

```
if (i < 4) {
  // Do a million lines of awful code that will bloat you executable!
}
```

And lets say that via scalar evolution we know that `i` could _never_ be greater
than 3. That allows the compiler to remove the entire if branch as it will never
be hit!

## Why Floating-Point Scalar Evolution?

Many floating-point algorithms can be vastly improved if we know the scope of
the inputs to math functions are. Lots of high performance code that game
developers throw through compiler stacks will use many operations that could
benefit from knowing a little more about the value of the float.

A great use of this would be something like the following:

```
float f = ...; // definitely not NaN or Infinity
f = sin(f); // because f wasn't NaN or Infinity f is now in the range [-1..1]

if (isfinite(f)) {
  // Do a million lines of awful code that will bloat you executable!
}

```

In the example above that if check should always return false - but the compiler
doesn't have enough information to know that.

If we look at the latest (as of the time of writing) version of LLVM from trunk
SVN (9.0.0svn) and how it handles the above:

```
define float @func(float %0) {
1:
  %2 = tail call nnan ninf float @llvm.sin.f32(float %0)
  %3 = tail call float @llvm.fabs.f32(float %2)
  %4 = fcmp ueq float %3, 0x7FF0000000000000
  br i1 %4, label %5, label %7

5:
  %6 = ... ; do something complicated
  br label %7

7:
  %8 = phi float [ %2, %1 ], [ %6, %5 ]
  ret float %8
}

```

Even with the additional _fast-math_ flags 'nnan' (no NaNs) and 'ninf' (no
Infinity) on the call stored into `%2` - the compiler cannot deduce that the
branch in `%4` is redundant.

This is all because LLVM's scalar evolution only cares about _integers_. This
generally makes sense - scalar evolution came about because of loops, and loops
have integer indices (for the most part...). What if we extended some similar
techniques as found in scalar evolution to floating-point - what optimization
opportunites we could open up!

## The Approach

The overall approach that this repository will attempt is to classify all
operations that produce a floating-point result as having a result within a
given range. We will keep track of the minimum (down to -NaN), the maximum (up
to +NaN) and whether the value is an integer (whole number hiding in a
floating-point) or not.

The analysis runs as a function pass that iterates on the basic blocks of
that function in a reverse post-order traversal - so ensure that for graphs of
basic blocks that are _not_ loops, we will always have classified an input float
before it is used.

To keep things simple - no attempt has been made to classify floating-point
values that are persistant around a loop - any phi node can attempt to classify
a float which has not been identified yet, and thus will have to make a default
worst-case assumption on the float.

## Compiling the Code

Easiest is to check what the `.travis.yml` does. Essentially the CMake will look
for an LLVM install on the `PATH` variable, or in the CMake variable `LLVM_DIR`,
and use that LLVM install to build with.


