'''
Simple example showing how to call HEMS functions from Python program.
'''

import cl4py
from cl4py import Symbol
from cl4py import List as lst

# get a handle to the lisp subprocess with quicklisp loaded.
lisp = cl4py.Lisp(quicklisp=True, backtrace=True)

# Start quicklisp and import HEMS package
lisp.find_package('QL').quickload('HEMS')

#load hems and retain reference.
hems = lisp.find_package("HEMS")

bn1 = hems.compile_program_from_file("prog1.hems")
bn2 = hems.compile_program_from_file("prog2.hems")
bn3 = hems.compile_program_from_file("prog3.hems")
bn4 = hems.compile_program_from_file("prog4.hems")
bn5 = hems.compile_program_from_file("prog5.hems")

cue = hems.compile_program_from_file("cue.hems")

for bn in [bn1, bn2, bn3, bn4, bn5]:
    hems.push_to_ep_buffer(state=bn, insertp=True)

(recollection, eme) = hems.remember(hems.get_eltm(), lst(cue), Symbol('+', 'HEMS'), 1, True)
print()
print(recollection)
