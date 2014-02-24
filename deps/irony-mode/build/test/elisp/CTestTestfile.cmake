# CMake generated Testfile for 
# Source directory: /home/dan/src/irony-mode/test/elisp
# Build directory: /home/dan/src/irony-mode/build/test/elisp
# 
# This file includes the relevent testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
ADD_TEST(irony-el "/usr/local/bin/emacs" "-batch" "-l" "/home/dan/src/irony-mode/test/elisp/test-irony.el" "-f" "ert-run-tests-batch-and-exit")
ADD_TEST(irony-cdb-el "/usr/local/bin/emacs" "-batch" "-l" "/home/dan/src/irony-mode/test/elisp/test-irony-cdb.el" "-f" "ert-run-tests-batch-and-exit")
