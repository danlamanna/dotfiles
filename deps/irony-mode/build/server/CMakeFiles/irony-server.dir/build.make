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
include server/CMakeFiles/irony-server.dir/depend.make

# Include the progress variables for this target.
include server/CMakeFiles/irony-server.dir/progress.make

# Include the compile flags for this target's objects.
include server/CMakeFiles/irony-server.dir/flags.make

server/CMakeFiles/irony-server.dir/IPlugin.cpp.o: server/CMakeFiles/irony-server.dir/flags.make
server/CMakeFiles/irony-server.dir/IPlugin.cpp.o: ../server/IPlugin.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/dan/src/irony-mode/build/CMakeFiles $(CMAKE_PROGRESS_1)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object server/CMakeFiles/irony-server.dir/IPlugin.cpp.o"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/irony-server.dir/IPlugin.cpp.o -c /home/dan/src/irony-mode/server/IPlugin.cpp

server/CMakeFiles/irony-server.dir/IPlugin.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/irony-server.dir/IPlugin.cpp.i"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/dan/src/irony-mode/server/IPlugin.cpp > CMakeFiles/irony-server.dir/IPlugin.cpp.i

server/CMakeFiles/irony-server.dir/IPlugin.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/irony-server.dir/IPlugin.cpp.s"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/dan/src/irony-mode/server/IPlugin.cpp -o CMakeFiles/irony-server.dir/IPlugin.cpp.s

server/CMakeFiles/irony-server.dir/IPlugin.cpp.o.requires:
.PHONY : server/CMakeFiles/irony-server.dir/IPlugin.cpp.o.requires

server/CMakeFiles/irony-server.dir/IPlugin.cpp.o.provides: server/CMakeFiles/irony-server.dir/IPlugin.cpp.o.requires
	$(MAKE) -f server/CMakeFiles/irony-server.dir/build.make server/CMakeFiles/irony-server.dir/IPlugin.cpp.o.provides.build
.PHONY : server/CMakeFiles/irony-server.dir/IPlugin.cpp.o.provides

server/CMakeFiles/irony-server.dir/IPlugin.cpp.o.provides.build: server/CMakeFiles/irony-server.dir/IPlugin.cpp.o

server/CMakeFiles/irony-server.dir/ClangString.cpp.o: server/CMakeFiles/irony-server.dir/flags.make
server/CMakeFiles/irony-server.dir/ClangString.cpp.o: ../server/ClangString.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/dan/src/irony-mode/build/CMakeFiles $(CMAKE_PROGRESS_2)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object server/CMakeFiles/irony-server.dir/ClangString.cpp.o"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/irony-server.dir/ClangString.cpp.o -c /home/dan/src/irony-mode/server/ClangString.cpp

server/CMakeFiles/irony-server.dir/ClangString.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/irony-server.dir/ClangString.cpp.i"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/dan/src/irony-mode/server/ClangString.cpp > CMakeFiles/irony-server.dir/ClangString.cpp.i

server/CMakeFiles/irony-server.dir/ClangString.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/irony-server.dir/ClangString.cpp.s"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/dan/src/irony-mode/server/ClangString.cpp -o CMakeFiles/irony-server.dir/ClangString.cpp.s

server/CMakeFiles/irony-server.dir/ClangString.cpp.o.requires:
.PHONY : server/CMakeFiles/irony-server.dir/ClangString.cpp.o.requires

server/CMakeFiles/irony-server.dir/ClangString.cpp.o.provides: server/CMakeFiles/irony-server.dir/ClangString.cpp.o.requires
	$(MAKE) -f server/CMakeFiles/irony-server.dir/build.make server/CMakeFiles/irony-server.dir/ClangString.cpp.o.provides.build
.PHONY : server/CMakeFiles/irony-server.dir/ClangString.cpp.o.provides

server/CMakeFiles/irony-server.dir/ClangString.cpp.o.provides.build: server/CMakeFiles/irony-server.dir/ClangString.cpp.o

server/CMakeFiles/irony-server.dir/Server.cpp.o: server/CMakeFiles/irony-server.dir/flags.make
server/CMakeFiles/irony-server.dir/Server.cpp.o: ../server/Server.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/dan/src/irony-mode/build/CMakeFiles $(CMAKE_PROGRESS_3)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object server/CMakeFiles/irony-server.dir/Server.cpp.o"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/irony-server.dir/Server.cpp.o -c /home/dan/src/irony-mode/server/Server.cpp

server/CMakeFiles/irony-server.dir/Server.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/irony-server.dir/Server.cpp.i"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/dan/src/irony-mode/server/Server.cpp > CMakeFiles/irony-server.dir/Server.cpp.i

server/CMakeFiles/irony-server.dir/Server.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/irony-server.dir/Server.cpp.s"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/dan/src/irony-mode/server/Server.cpp -o CMakeFiles/irony-server.dir/Server.cpp.s

server/CMakeFiles/irony-server.dir/Server.cpp.o.requires:
.PHONY : server/CMakeFiles/irony-server.dir/Server.cpp.o.requires

server/CMakeFiles/irony-server.dir/Server.cpp.o.provides: server/CMakeFiles/irony-server.dir/Server.cpp.o.requires
	$(MAKE) -f server/CMakeFiles/irony-server.dir/build.make server/CMakeFiles/irony-server.dir/Server.cpp.o.provides.build
.PHONY : server/CMakeFiles/irony-server.dir/Server.cpp.o.provides

server/CMakeFiles/irony-server.dir/Server.cpp.o.provides.build: server/CMakeFiles/irony-server.dir/Server.cpp.o

server/CMakeFiles/irony-server.dir/TUManager.cpp.o: server/CMakeFiles/irony-server.dir/flags.make
server/CMakeFiles/irony-server.dir/TUManager.cpp.o: ../server/TUManager.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/dan/src/irony-mode/build/CMakeFiles $(CMAKE_PROGRESS_4)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object server/CMakeFiles/irony-server.dir/TUManager.cpp.o"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/irony-server.dir/TUManager.cpp.o -c /home/dan/src/irony-mode/server/TUManager.cpp

server/CMakeFiles/irony-server.dir/TUManager.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/irony-server.dir/TUManager.cpp.i"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/dan/src/irony-mode/server/TUManager.cpp > CMakeFiles/irony-server.dir/TUManager.cpp.i

server/CMakeFiles/irony-server.dir/TUManager.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/irony-server.dir/TUManager.cpp.s"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/dan/src/irony-mode/server/TUManager.cpp -o CMakeFiles/irony-server.dir/TUManager.cpp.s

server/CMakeFiles/irony-server.dir/TUManager.cpp.o.requires:
.PHONY : server/CMakeFiles/irony-server.dir/TUManager.cpp.o.requires

server/CMakeFiles/irony-server.dir/TUManager.cpp.o.provides: server/CMakeFiles/irony-server.dir/TUManager.cpp.o.requires
	$(MAKE) -f server/CMakeFiles/irony-server.dir/build.make server/CMakeFiles/irony-server.dir/TUManager.cpp.o.provides.build
.PHONY : server/CMakeFiles/irony-server.dir/TUManager.cpp.o.provides

server/CMakeFiles/irony-server.dir/TUManager.cpp.o.provides.build: server/CMakeFiles/irony-server.dir/TUManager.cpp.o

server/CMakeFiles/irony-server.dir/main.cpp.o: server/CMakeFiles/irony-server.dir/flags.make
server/CMakeFiles/irony-server.dir/main.cpp.o: ../server/main.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/dan/src/irony-mode/build/CMakeFiles $(CMAKE_PROGRESS_5)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object server/CMakeFiles/irony-server.dir/main.cpp.o"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/irony-server.dir/main.cpp.o -c /home/dan/src/irony-mode/server/main.cpp

server/CMakeFiles/irony-server.dir/main.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/irony-server.dir/main.cpp.i"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/dan/src/irony-mode/server/main.cpp > CMakeFiles/irony-server.dir/main.cpp.i

server/CMakeFiles/irony-server.dir/main.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/irony-server.dir/main.cpp.s"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/dan/src/irony-mode/server/main.cpp -o CMakeFiles/irony-server.dir/main.cpp.s

server/CMakeFiles/irony-server.dir/main.cpp.o.requires:
.PHONY : server/CMakeFiles/irony-server.dir/main.cpp.o.requires

server/CMakeFiles/irony-server.dir/main.cpp.o.provides: server/CMakeFiles/irony-server.dir/main.cpp.o.requires
	$(MAKE) -f server/CMakeFiles/irony-server.dir/build.make server/CMakeFiles/irony-server.dir/main.cpp.o.provides.build
.PHONY : server/CMakeFiles/irony-server.dir/main.cpp.o.provides

server/CMakeFiles/irony-server.dir/main.cpp.o.provides.build: server/CMakeFiles/irony-server.dir/main.cpp.o

server/CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.o: server/CMakeFiles/irony-server.dir/flags.make
server/CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.o: ../server/plugins/CodeCompletion.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/dan/src/irony-mode/build/CMakeFiles $(CMAKE_PROGRESS_6)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object server/CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.o"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.o -c /home/dan/src/irony-mode/server/plugins/CodeCompletion.cpp

server/CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.i"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/dan/src/irony-mode/server/plugins/CodeCompletion.cpp > CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.i

server/CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.s"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/dan/src/irony-mode/server/plugins/CodeCompletion.cpp -o CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.s

server/CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.o.requires:
.PHONY : server/CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.o.requires

server/CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.o.provides: server/CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.o.requires
	$(MAKE) -f server/CMakeFiles/irony-server.dir/build.make server/CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.o.provides.build
.PHONY : server/CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.o.provides

server/CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.o.provides.build: server/CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.o

server/CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.o: server/CMakeFiles/irony-server.dir/flags.make
server/CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.o: ../server/plugins/SyntaxChecker.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/dan/src/irony-mode/build/CMakeFiles $(CMAKE_PROGRESS_7)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object server/CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.o"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.o -c /home/dan/src/irony-mode/server/plugins/SyntaxChecker.cpp

server/CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.i"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/dan/src/irony-mode/server/plugins/SyntaxChecker.cpp > CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.i

server/CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.s"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/dan/src/irony-mode/server/plugins/SyntaxChecker.cpp -o CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.s

server/CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.o.requires:
.PHONY : server/CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.o.requires

server/CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.o.provides: server/CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.o.requires
	$(MAKE) -f server/CMakeFiles/irony-server.dir/build.make server/CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.o.provides.build
.PHONY : server/CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.o.provides

server/CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.o.provides.build: server/CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.o

server/CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.o: server/CMakeFiles/irony-server.dir/flags.make
server/CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.o: ../server/plugins/CacheInvalidation.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/dan/src/irony-mode/build/CMakeFiles $(CMAKE_PROGRESS_8)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object server/CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.o"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.o -c /home/dan/src/irony-mode/server/plugins/CacheInvalidation.cpp

server/CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.i"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/dan/src/irony-mode/server/plugins/CacheInvalidation.cpp > CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.i

server/CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.s"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/dan/src/irony-mode/server/plugins/CacheInvalidation.cpp -o CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.s

server/CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.o.requires:
.PHONY : server/CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.o.requires

server/CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.o.provides: server/CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.o.requires
	$(MAKE) -f server/CMakeFiles/irony-server.dir/build.make server/CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.o.provides.build
.PHONY : server/CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.o.provides

server/CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.o.provides.build: server/CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.o

server/CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.o: server/CMakeFiles/irony-server.dir/flags.make
server/CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.o: ../server/plugins/CompileChecker.cpp
	$(CMAKE_COMMAND) -E cmake_progress_report /home/dan/src/irony-mode/build/CMakeFiles $(CMAKE_PROGRESS_9)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object server/CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.o"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.o -c /home/dan/src/irony-mode/server/plugins/CompileChecker.cpp

server/CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.i"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/dan/src/irony-mode/server/plugins/CompileChecker.cpp > CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.i

server/CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.s"
	cd /home/dan/src/irony-mode/build/server && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/dan/src/irony-mode/server/plugins/CompileChecker.cpp -o CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.s

server/CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.o.requires:
.PHONY : server/CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.o.requires

server/CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.o.provides: server/CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.o.requires
	$(MAKE) -f server/CMakeFiles/irony-server.dir/build.make server/CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.o.provides.build
.PHONY : server/CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.o.provides

server/CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.o.provides.build: server/CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.o

# Object files for target irony-server
irony__server_OBJECTS = \
"CMakeFiles/irony-server.dir/IPlugin.cpp.o" \
"CMakeFiles/irony-server.dir/ClangString.cpp.o" \
"CMakeFiles/irony-server.dir/Server.cpp.o" \
"CMakeFiles/irony-server.dir/TUManager.cpp.o" \
"CMakeFiles/irony-server.dir/main.cpp.o" \
"CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.o" \
"CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.o" \
"CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.o" \
"CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.o"

# External object files for target irony-server
irony__server_EXTERNAL_OBJECTS =

server/irony-server: server/CMakeFiles/irony-server.dir/IPlugin.cpp.o
server/irony-server: server/CMakeFiles/irony-server.dir/ClangString.cpp.o
server/irony-server: server/CMakeFiles/irony-server.dir/Server.cpp.o
server/irony-server: server/CMakeFiles/irony-server.dir/TUManager.cpp.o
server/irony-server: server/CMakeFiles/irony-server.dir/main.cpp.o
server/irony-server: server/CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.o
server/irony-server: server/CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.o
server/irony-server: server/CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.o
server/irony-server: server/CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.o
server/irony-server: server/CMakeFiles/irony-server.dir/build.make
server/irony-server: lib/libirony-utils.a
server/irony-server: /usr/lib/libclang.so
server/irony-server: server/CMakeFiles/irony-server.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --red --bold "Linking CXX executable irony-server"
	cd /home/dan/src/irony-mode/build/server && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/irony-server.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
server/CMakeFiles/irony-server.dir/build: server/irony-server
.PHONY : server/CMakeFiles/irony-server.dir/build

server/CMakeFiles/irony-server.dir/requires: server/CMakeFiles/irony-server.dir/IPlugin.cpp.o.requires
server/CMakeFiles/irony-server.dir/requires: server/CMakeFiles/irony-server.dir/ClangString.cpp.o.requires
server/CMakeFiles/irony-server.dir/requires: server/CMakeFiles/irony-server.dir/Server.cpp.o.requires
server/CMakeFiles/irony-server.dir/requires: server/CMakeFiles/irony-server.dir/TUManager.cpp.o.requires
server/CMakeFiles/irony-server.dir/requires: server/CMakeFiles/irony-server.dir/main.cpp.o.requires
server/CMakeFiles/irony-server.dir/requires: server/CMakeFiles/irony-server.dir/plugins/CodeCompletion.cpp.o.requires
server/CMakeFiles/irony-server.dir/requires: server/CMakeFiles/irony-server.dir/plugins/SyntaxChecker.cpp.o.requires
server/CMakeFiles/irony-server.dir/requires: server/CMakeFiles/irony-server.dir/plugins/CacheInvalidation.cpp.o.requires
server/CMakeFiles/irony-server.dir/requires: server/CMakeFiles/irony-server.dir/plugins/CompileChecker.cpp.o.requires
.PHONY : server/CMakeFiles/irony-server.dir/requires

server/CMakeFiles/irony-server.dir/clean:
	cd /home/dan/src/irony-mode/build/server && $(CMAKE_COMMAND) -P CMakeFiles/irony-server.dir/cmake_clean.cmake
.PHONY : server/CMakeFiles/irony-server.dir/clean

server/CMakeFiles/irony-server.dir/depend:
	cd /home/dan/src/irony-mode/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/dan/src/irony-mode /home/dan/src/irony-mode/server /home/dan/src/irony-mode/build /home/dan/src/irony-mode/build/server /home/dan/src/irony-mode/build/server/CMakeFiles/irony-server.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : server/CMakeFiles/irony-server.dir/depend
