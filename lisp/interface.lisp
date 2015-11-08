;;(load "/home/toni/quicklisp/setup.lisp")
;;(ql:quickload :cl-autowrap)
;;(ql:quickload :lightningfn)

(in-package :lightningfn)

(defmacro with-init ((&optional (progname nil)) &body body)
  (let ((progname-sym (gensym "progname")))
    `(let ((,progname-sym ,progname))
       (lightningfn-ffi::init-fn-jit (if (null ,progname-sym) (cffi:null-pointer) ,progname-sym))
       (unwind-protect
	    ,@body
	 (lightningfn-ffi::finish-fn-jit)))))

(defun test-automatic-garbage-collection (&key (mb 10) (rep 10) (explicit-gc nil))
  "Allocate a number of LIGHTNINGFN-FFI::JIT-STATE so that MB Megabytes are allocated in total. Repeat this REP times."
  (with-init ()
    (let* ((bytes 10000) ;a jit-state occupies about 10000 bytes.
	   (reptotal (* mb 1000000))
	   (count (floor (/ reptotal bytes))))
      (format t "(test ~A :mb ~A :rep ~A)~%" bytes mb rep)
      (format t "If automatic garbage collection is not triggerd, we will use ~A MB in total.~%" (* mb rep))
      (finish-output)
      (tg:gc :full t)
      ;; In the loop, I'm not calling #'TG:GC on purpose to test automatic garbage collection.
      (loop for r below rep do
	   (when explicit-gc
	     (tg:gc))
	   (format t "~A/~A (allocating ~A times ~A bytes)~%" r rep count bytes)
	   (finish-output)
	   (loop for i below count collect (make-instance 'lightningfn-ffi::jit-state))
	   (finish-output)))))

#|
;; Example:
(lightningfn:with-init ()
  (let ((jit (make-instance 'lightningfn-ffi::jit-state)))
    (lightningfn-ffi::fn-jit-prolog jit)
    (let* ((in (lightningfn-ffi::fn-jit-arg jit))
	   (r (lightningfn-ffi::fn-jit-r 0)))
      (lightningfn-ffi::fn-jit-getarg-i jit r in)
      (lightningfn-ffi::fn-jit-addi jit r r 1)
      (lightningfn-ffi::fn-jit-retr jit r)
      (let ((incr (lightningfn-ffi::fn-jit-emit jit)))
	(cffi:foreign-funcall-pointer incr (:convention :stdcall) :int 5 :int)))))
|#
