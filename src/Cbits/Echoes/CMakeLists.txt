cmake_minimum_required(VERSION 3.10)
project(EchoesDriver VERSION 1.0 LANGUAGES C)

set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED True)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if (APPLE)
	set(CMAKE_OSX_ARCHITECTURES "arm64")
endif()


include_directories(${CMAKE_SOURCE_DIR}/include)

add_subdirectory(${CMAKE_SOURCE_DIR}/src)
