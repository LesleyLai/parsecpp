cmake_minimum_required(VERSION 3.14)

project(parsecpp VERSION 0.0.0 LANGUAGES CXX)

include("cmake/StandardProjectSettings.cmake")
include("CMakeDependentOption")

include("cmake/compiler.cmake")
include("cmake/clangformat.cmake")
include("cmake/StaticAnalyzers.cmake")

include("cmake/conan.cmake")
run_conan()

add_library(parsecpp INTERFACE)
target_sources(parsecpp INTERFACE
        "include/parsec.hpp"
        "include/tuple.hpp")
target_include_directories(parsecpp INTERFACE "${CMAKE_SOURCE_DIR}/include")

if (CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
    include(CTest)
endif ()

if (CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME AND BUILD_TESTING)
    add_subdirectory(test)
endif ()

add_subdirectory(examples)
