# This is free and unencumbered software released into the public domain.
#
# Anyone is free to copy, modify, publish, use, compile, sell, or
# distribute this software, either in source code form or as a compiled
# binary, for any purpose, commercial or non-commercial, and by any
# means.
#
# In jurisdictions that recognize copyright laws, the author or authors
# of this software dedicate any and all copyright interest in the
# software to the public domain. We make this dedication for the benefit
# of the public at large and to the detriment of our heirs and
# successors. We intend this dedication to be an overt act of
# relinquishment in perpetuity of all present and future rights to this
# software under copyright law.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
# OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.
#
# For more information, please refer to <http://unlicense.org/>

# Create an executable to actually run our fpscev pass. LLVM's opt has a
# builtin mechanism to load optimizations from a shared library (opt -load),
# but I've found it to be horrendously broken to the point it isn't actually
# usable on any default LLVM install.
add_executable(fpscev_opt fpscev_opt.cpp)

# Update the compiler flags to match what LLVM was built with (disabling RTTI).
llvm_update_compile_flags(fpscev_opt)

# Get the LLVM targets for the libraries we nee.
llvm_map_components_to_libnames(llvm_libs AsmParser BitReader BitWriter)

# Link everything in, including our fpscev optimization.
target_link_libraries(fpscev_opt PRIVATE ${llvm_libs} fpscev)
