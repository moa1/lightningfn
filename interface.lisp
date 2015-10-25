;;(load "/home/toni/quicklisp/setup.lisp")
;;(ql:quickload :cl-autowrap)
;;(ql:quickload :lightningfn)

(in-package :lightningfn)

(defmacro with-init ((&optional (progname (cffi:null-pointer))) &body body)
  `(progn
     (init-fn-jit ,progname)
     (unwind-protect
	  ,@body
       (finish-fn-jit))))

(defun new-state ()
  "Create a new JIT state that is automatically garbage-collected when going out of scope.
Note that you may NOT call #'FN-JIT-CLEAR-STATE on the JIT-object/PTR??? TODO"
  (let* ((jit (fn-jit-new-state))
	 (ptr (autowrap:ptr jit)))
    (tg:finalize jit (lambda () (fn-jit-clear-state ptr) (fn-jit-destroy-state ptr)))
    jit))

#|
(defun test ()
  "The same as test2, but with automatic garbage collection."
  (lightningfn:with-init ()
    (let* ((jit (lightningfn:new-state)))
      (lightningfn-ffi.functions:fn-jit-prolog jit)
      (let ((in (lightningfn-ffi.functions:fn-jit-arg jit))
	    (r0 (lightningfn-ffi.functions:fn-jit-r 0)))
	(lightningfn-ffi.functions:fn-jit-getarg jit r0 in)
	(lightningfn-ffi.functions:fn-jit-addi jit r0 r0 1)
	(lightningfn-ffi.functions:fn-jit-retr jit r0)
	(let ((emit (lightningfn-ffi::jit-pointer-t-ptr (lightningfn-ffi.functions:fn-jit-emit jit))))
	  (cffi:foreign-funcall-pointer emit (:convention :stdcall) :int (1- (ash 1 31)) :int))))))

(defun test2 ()
  (lightningfn:with-init ()
    (let* ((jit (lightningfn-ffi.functions:fn-jit-new-state))
	   (result))
      (lightningfn-ffi.functions:fn-jit-prolog jit)
      (let ((in (lightningfn-ffi.functions:fn-jit-arg jit))
	    (r0 (lightningfn-ffi.functions:fn-jit-r 0)))
	(lightningfn-ffi.functions:fn-jit-getarg jit r0 in)
	(lightningfn-ffi.functions:fn-jit-addi jit r0 r0 1)
	(lightningfn-ffi.functions:fn-jit-retr jit r0)
	(let ((emit (lightningfn-ffi::jit-pointer-t-ptr (lightningfn-ffi.functions:fn-jit-emit jit))))
	  (lightningfn-ffi.functions:fn-jit-clear-state jit)
	  (setf result (cffi:foreign-funcall-pointer emit (:convention :stdcall) :int (1- (ash 1 31)) :int))))
      (lightningfn-ffi.functions:fn-jit-destroy-state jit)
      result)))
|#
