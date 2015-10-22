;;(load "/home/toni/quicklisp/setup.lisp")
;;(ql:quickload :cl-autowrap)

(defpackage #:lightningfn
  (:use #:cl #:autowrap.minimal #:plus-c)
  (:import-from :cffi
		#:mem-ref #:with-foreign-objects #:with-foreign-object
		#:foreign-alloc #:foreign-free #:null-pointer-p)
  (:export
   #:with-init
   #:quit))

(in-package :lightningfn)

(cffi:define-foreign-library liblightningfn
  (:unix (:or "lightningfn.so" "/home/toni/soft/lightningfn/lightningfn.so"))
  (t (:default "lightningfn")))

(cffi:use-foreign-library liblightningfn)

(setf autowrap:*c2ffi-program* "/home/toni/soft/c2ffi-master/src/c2ffi")

(autowrap:c-include
 '(lightningfn autowrap-spec "lightningfn.h")
 :sysincludes '("/usr/include/" "/usr/include/i386-linux-gnu/" "/usr/include/linux/")
 :exclude-sources ("/usr/include/*")
 :spec-path '(lightningfn autowrap-spec)
 )


(defmacro with-init ((&optional (progname (cffi:null-pointer))) &body body)
  `(progn
     (init-fn-jit ,progname)
     (unwind-protect
	  ,@body
       (finish-fn-jit))))

#|
(defmacro with-jit ((jit-symbol) &body body)
  `(let ((,jit-symbol (fn-jit-new-state)))
     (unwind-protect
	  (,@body)
       (
|#

#|
(lightningfn:with-init ()
  (let* ((jit (lightningfn:fn-jit-new-state)))
    (lightningfn:fn-jit-prolog jit)
    (let ((in (lightningfn:fn-jit-arg jit))
	  (r0 (lightningfn:fn-jit-r 0)))
      (lightningfn:fn-jit-getarg jit r0 in)
      (lightningfn:fn-jit-addi jit r0 r0 1)
      (lightningfn:fn-jit-retr jit r0)
      (let ((emit (lightningfn::jit-pointer-t-ptr (lightningfn:fn-jit-emit jit))))
	(lightningfn:fn-jit-clear-state jit)
	(print (cffi:foreign-funcall-pointer emit (:convention :stdcall) :int (1- (ash 1 31)) :int))))
    (lightningfn:fn-jit-destroy-state jit)))
|#
