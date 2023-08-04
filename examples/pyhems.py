'''
Simple example showing how to call HEMS functions from Python program.
'''

import cl4py

# get a handle to the lisp subprocess with quicklisp loaded.
lisp = cl4py.Lisp(quicklisp=True, backtrace=True)

# get a handle to quicklisp
ql = lisp.find_package('QL')

# find and load the hems package.
ql.quickload('HEMS')

#load hems and retain reference.
hems = lisp.find_package("HEMS")

bn = hems.compile_program_from_file("prog1.hems")
print(bn)
