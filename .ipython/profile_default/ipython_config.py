# lines of code to run at IPython startup.
c.InteractiveShellApp.exec_lines = [
    'import csv',
    'import hashlib',
    'import json',
    'import os',
    'import re',
    'import requests',
    'import shutil',
    'import sys',
    'from collections import Counter, defaultdict'
]

# Whether to display a banner upon starting IPython.
c.TerminalIPythonApp.display_banner = False

# Make IPython automatically call any callable object even if you didn't type
# explicit parentheses. For example, 'str 43' becomes 'str(43)' automatically.
# The value can be '0' to disable the feature, '1' for 'smart' autocall, where
# it is not applied if there are no more arguments on the line, and '2' for
# 'full' autocall, where all callable objects are automatically called (even if
# no arguments are present).
c.InteractiveShell.autocall = 1

# Automatically call the pdb debugger after every exception.
c.InteractiveShell.pdb = True
