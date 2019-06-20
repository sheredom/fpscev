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

#include "llvm/AsmParser/Parser.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"

using namespace llvm;

namespace llvm {
extern void initializeFPScalarEvolutionPassPass(PassRegistry &);
extern void initializeFastMathPropagationPassPass(PassRegistry &);
extern void initializeFPInstSimplifyPassPass(PassRegistry &);
extern Pass *createFastMathPropagationPass();
extern Pass *createFPInstSimplifyPass();
} // namespace llvm

static cl::opt<std::string> inputFilename(cl::Positional,
                                          cl::desc("<input bitcode file>"),
                                          cl::init("-"),
                                          cl::value_desc("filename"));

static cl::opt<std::string> outputFilename("o",
                                           cl::desc("Override output filename"),
                                           cl::init("-"),
                                           cl::value_desc("filename"));

static cl::opt<bool> outputAssembly("S", cl::init(false),
                                    cl::desc("Write output as LLVM assembly"));

int main(const int argc, const char *const argv[]) {
  cl::ParseCommandLineOptions(argc, argv);

  LLVMContext context;

  auto errorOrMemoryBuffer = MemoryBuffer::getFileOrSTDIN(inputFilename);

  if (!errorOrMemoryBuffer) {
    errs() << "Could not open file: '" << inputFilename << "'\n";
    return 1;
  }

  std::unique_ptr<MemoryBuffer> memoryBuffer(
      std::move(errorOrMemoryBuffer.get()));

  std::unique_ptr<Module> module;

  if (isBitcode(reinterpret_cast<const unsigned char *>(
                    memoryBuffer->getBufferStart()),
                reinterpret_cast<const unsigned char *>(
                    memoryBuffer->getBufferEnd()))) {
    /// Read the specified bitcode file, returning the module.
    auto errorOrModule = parseBitcodeFile(*memoryBuffer, context);

    if (!errorOrModule) {
      errs() << "Could not parse bitcode file: '" << inputFilename << "'\n";
      return 1;
    }

    module = std::move(errorOrModule.get());
  } else {
    SMDiagnostic error;
    module = std::move(parseAssemblyFile(inputFilename, error, context));

    if (!module) {
      error.print(argv[0], errs());
      return 1;
    }
  }

  PassRegistry *const passRegistry = PassRegistry::getPassRegistry();

  initializeFPScalarEvolutionPassPass(*passRegistry);
  initializeFastMathPropagationPassPass(*passRegistry);
  initializeFPInstSimplifyPassPass(*passRegistry);

  legacy::PassManager passManager;
  passManager.add(createFastMathPropagationPass());
  passManager.add(createFPInstSimplifyPass());
  passManager.run(*module);

  std::error_code error;
  raw_fd_ostream ostream(outputFilename, error);

  if (error) {
    errs() << error.message();
    return error.value();
  }

  if (outputAssembly) {
    module->print(ostream, nullptr);
  } else {
    WriteBitcodeToFile(*module, ostream);
  }
}
