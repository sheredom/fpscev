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

cmake_minimum_required(VERSION 3.4)
project(fpscev)

# LLVM requires C++11.
set(CMAKE_CXX_STANDARD 11)

# Find LLVM (this looks for LLVMConfig.cmake somewhere in the PATH or LLVM_DIR).
find_package(LLVM REQUIRED CONFIG)

# Enable us to use the LLVM CMake modules.
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${LLVM_CMAKE_DIR}")

# Enable us to use add_llvm_library.
include(AddLLVM)

# Bring in the LLVM include directories.
include_directories(${LLVM_INCLUDE_DIRS})

# Bring in any definitions LLVM requires.
add_definitions(${LLVM_DEFINITIONS})

# Add a static library for our fpscev.
add_library(fpscev STATIC fpscev.cpp)

# Update the compiler flags to match what LLVM was built with (disabling RTTI).
llvm_update_compile_flags(fpscev)

# Get the LLVM targets for the libraries we nee.
llvm_map_components_to_libnames(llvm_libs Analysis Core Support)

# Link everything in.
target_link_libraries(fpscev PUBLIC ${llvm_libs})

add_subdirectory(driver)
