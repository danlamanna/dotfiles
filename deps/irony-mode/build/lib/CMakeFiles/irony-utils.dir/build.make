# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 2.8

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list

# Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/dan/src/irony-mode

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/dan/src/irony-mode/build

# Include any dependencies generated for this target.
include lib/CMakeFiles/irony-utils.dir/depend.make

# Include the progress variables for this target.
include lib/CMakeFiles/irony-utils.dir/progress.make

# Include the compile flags for this target's objects.
include lib/CMakeFiles/irony-utils.dir/flags.make

lib/CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.o: lib/CMakeFiles/irony-utils.dir/flags.make
lib/CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.o: ../lib/util/JSONObjectWrapper.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/dan/src/irony-mode/build/CMakeFiles $(CMAKE_PROGRESS_1)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object lib/CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.o"
	cd /home/dan/src/irony-mode/build/lib && /usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.o -c /home/dan/src/irony-mode/lib/util/JSONObjectWrapper.cpp

lib/CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.i"
	cd /home/dan/src/irony-mode/build/lib && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/dan/src/irony-mode/lib/util/JSONObjectWrapper.cpp > CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.i

lib/CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.s"
	cd /home/dan/src/irony-mode/build/lib && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/dan/src/irony-mode/lib/util/JSONObjectWrapper.cpp -o CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.s

lib/CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.o.requires:
.PHONY : lib/CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.o.requires

lib/CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.o.provides: lib/CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.o.requires
	$(MAKE) -f lib/CMakeFiles/irony-utils.dir/build.make lib/CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.o.provides.build
.PHONY : lib/CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.o.provides

lib/CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.o.provides.build: lib/CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.o

lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.o: lib/CMakeFiles/irony-utils.dir/flags.make
lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.o: ../lib/SimpleJSON/src/JSON.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/dan/src/irony-mode/build/CMakeFiles $(CMAKE_PROGRESS_2)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.o"
	cd /home/dan/src/irony-mode/build/lib && /usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.o -c /home/dan/src/irony-mode/lib/SimpleJSON/src/JSON.cpp

lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.i"
	cd /home/dan/src/irony-mode/build/lib && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/dan/src/irony-mode/lib/SimpleJSON/src/JSON.cpp > CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.i

lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.s"
	cd /home/dan/src/irony-mode/build/lib && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/dan/src/irony-mode/lib/SimpleJSON/src/JSON.cpp -o CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.s

lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.o.requires:
.PHONY : lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.o.requires

lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.o.provides: lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.o.requires
	$(MAKE) -f lib/CMakeFiles/irony-utils.dir/build.make lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.o.provides.build
.PHONY : lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.o.provides

lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.o.provides.build: lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.o

lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.o: lib/CMakeFiles/irony-utils.dir/flags.make
lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.o: ../lib/SimpleJSON/src/JSONValue.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/dan/src/irony-mode/build/CMakeFiles $(CMAKE_PROGRESS_3)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.o"
	cd /home/dan/src/irony-mode/build/lib && /usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.o -c /home/dan/src/irony-mode/lib/SimpleJSON/src/JSONValue.cpp

lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.i"
	cd /home/dan/src/irony-mode/build/lib && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/dan/src/irony-mode/lib/SimpleJSON/src/JSONValue.cpp > CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.i

lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.s"
	cd /home/dan/src/irony-mode/build/lib && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/dan/src/irony-mode/lib/SimpleJSON/src/JSONValue.cpp -o CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.s

lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.o.requires:
.PHONY : lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.o.requires

lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.o.provides: lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.o.requires
	$(MAKE) -f lib/CMakeFiles/irony-utils.dir/build.make lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.o.provides.build
.PHONY : lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.o.provides

lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.o.provides.build: lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.o

lib/CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.o: lib/CMakeFiles/irony-utils.dir/flags.make
lib/CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.o: ../lib/str/wstring_to_string.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/dan/src/irony-mode/build/CMakeFiles $(CMAKE_PROGRESS_4)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object lib/CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.o"
	cd /home/dan/src/irony-mode/build/lib && /usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.o -c /home/dan/src/irony-mode/lib/str/wstring_to_string.cpp

lib/CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.i"
	cd /home/dan/src/irony-mode/build/lib && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/dan/src/irony-mode/lib/str/wstring_to_string.cpp > CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.i

lib/CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.s"
	cd /home/dan/src/irony-mode/build/lib && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/dan/src/irony-mode/lib/str/wstring_to_string.cpp -o CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.s

lib/CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.o.requires:
.PHONY : lib/CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.o.requires

lib/CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.o.provides: lib/CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.o.requires
	$(MAKE) -f lib/CMakeFiles/irony-utils.dir/build.make lib/CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.o.provides.build
.PHONY : lib/CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.o.provides

lib/CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.o.provides.build: lib/CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.o

# Object files for target irony-utils
irony__utils_OBJECTS = \
"CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.o" \
"CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.o" \
"CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.o" \
"CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.o"

# External object files for target irony-utils
irony__utils_EXTERNAL_OBJECTS =

lib/libirony-utils.a: lib/CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.o
lib/libirony-utils.a: lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.o
lib/libirony-utils.a: lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.o
lib/libirony-utils.a: lib/CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.o
lib/libirony-utils.a: lib/CMakeFiles/irony-utils.dir/build.make
lib/libirony-utils.a: lib/CMakeFiles/irony-utils.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --red --bold "Linking CXX static library libirony-utils.a"
	cd /home/dan/src/irony-mode/build/lib && $(CMAKE_COMMAND) -P CMakeFiles/irony-utils.dir/cmake_clean_target.cmake
	cd /home/dan/src/irony-mode/build/lib && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/irony-utils.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
lib/CMakeFiles/irony-utils.dir/build: lib/libirony-utils.a
.PHONY : lib/CMakeFiles/irony-utils.dir/build

lib/CMakeFiles/irony-utils.dir/requires: lib/CMakeFiles/irony-utils.dir/util/JSONObjectWrapper.cpp.o.requires
lib/CMakeFiles/irony-utils.dir/requires: lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSON.cpp.o.requires
lib/CMakeFiles/irony-utils.dir/requires: lib/CMakeFiles/irony-utils.dir/SimpleJSON/src/JSONValue.cpp.o.requires
lib/CMakeFiles/irony-utils.dir/requires: lib/CMakeFiles/irony-utils.dir/str/wstring_to_string.cpp.o.requires
.PHONY : lib/CMakeFiles/irony-utils.dir/requires

lib/CMakeFiles/irony-utils.dir/clean:
	cd /home/dan/src/irony-mode/build/lib && $(CMAKE_COMMAND) -P CMakeFiles/irony-utils.dir/cmake_clean.cmake
.PHONY : lib/CMakeFiles/irony-utils.dir/clean

lib/CMakeFiles/irony-utils.dir/depend:
	cd /home/dan/src/irony-mode/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/dan/src/irony-mode /home/dan/src/irony-mode/lib /home/dan/src/irony-mode/build /home/dan/src/irony-mode/build/lib /home/dan/src/irony-mode/build/lib/CMakeFiles/irony-utils.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : lib/CMakeFiles/irony-utils.dir/depend

