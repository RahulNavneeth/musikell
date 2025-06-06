file(GLOB SOURCES
	${CMAKE_CURRENT_SOURCE_DIR}/main.c
	${CMAKE_CURRENT_SOURCE_DIR}/driver-mac.c
	${CMAKE_CURRENT_SOURCE_DIR}/driver-linux.c
	${CMAKE_CURRENT_SOURCE_DIR}/driver-windows.c
	${CMAKE_CURRENT_SOURCE_DIR}/IO.c
)

add_executable(main ${SOURCES})
set_target_properties(main PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

if (APPLE)
	target_link_libraries(main "-framework CoreAudio" "-framework AudioToolbox" "-framework AudioUnit" "-framework CoreFoundation")
	target_compile_options(main PRIVATE -Wall)
endif()
