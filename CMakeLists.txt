cmake_minimum_required(VERSION 3.0)

project(FortranBenchmark LANGUAGES Fortran)

set(version 0.0.1)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "-ffree-line-length-none")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
endif ()
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR})

add_library(fortran_benchmark SHARED src/benchmark.f90)
set_property(TARGET fortran_benchmark PROPERTY VERSION ${version})
set_property(TARGET fortran_benchmark PROPERTY SOVERSION 1)
set_property(TARGET fortran_benchmark PROPERTY Fortran_MODULE_DIRECTORY
    "${CMAKE_CURRENT_BINARY_DIR}/modules")
target_include_directories(fortran_benchmark
    PUBLIC
       $<INSTALL_INTERFACE:include>
       $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules>)

add_executable(benchmark src/main.f90)
target_link_libraries(benchmark fortran_benchmark)

install(TARGETS fortran_benchmark EXPORT FortranBenchmarkTargets
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
)

install(DIRECTORY "${CMAKE_BINARY_DIR}/fortran_benchmark" DESTINATION include)

include(CMakePackageConfigHelpers)
write_basic_package_version_file (
  "${CMAKE_CURRENT_BINARY_DIR}/FortranBenchmark/FortranBenchmarkConfigVersion.cmake"
  VERSION ${version}
  COMPATIBILITY AnyNewerVersion
)

export(EXPORT FortranBenchmarkTargets
  FILE "${CMAKE_CURRENT_BINARY_DIR}/FortranBenchmark/FortranBenchmarkTargets.cmake"
)
configure_file(cmake/FortranBenchmarkConfig.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/FortranBenchmark/FortranBenchmarkConfig.cmake"
)

set(ConfigPackageLocation lib/cmake/FortranBenchmark)
install(EXPORT FortranBenchmarkTargets
  FILE FortranBenchmarkTargets.cmake
  DESTINATION ${ConfigPackageLocation}
)
install(
  FILES
    "${CMAKE_CURRENT_BINARY_DIR}/FortranBenchmark/FortranBenchmarkConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/FortranBenchmark/FortranBenchmarkConfigVersion.cmake"
  DESTINATION
    ${ConfigPackageLocation}
)
