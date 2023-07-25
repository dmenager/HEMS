import cl4py

# get a handle to the lisp subprocess with quicklisp loaded.
lisp = cl4py.Lisp(quicklisp=True, backtrace=True)

# find_package() finds common-lisp user package
cl = lisp.find_package('CL')
print(cl)

# find_package() does not find custom hems package
hems = lisp.find_package('hems')
print(hems)

# get a handle to quicklisp
ql = lisp.find_package('QL')

# find and load the hems package.
ql.quickload('hems')
print(ql)

# no handle to loaded hems library and can't call exported test_fun().
print(ql.test_fun())

