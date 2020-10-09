set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

add_executable(parsecpp_point3d_example point3d.cpp)

target_link_libraries(parsecpp_point3d_example PRIVATE parsecpp compiler_options)