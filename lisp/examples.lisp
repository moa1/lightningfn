(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :lightningfn)
(ql:quickload :utils)

(defparameter *clear-state-manually* t)

(defstruct jitfun
  jit
  fun)

(defun make-incr ()
  (flet ((make-incr ()
	   (lightningfn:with-new-state (:clear-state-manually *clear-state-manually*)
	     (let (in r)
	       (lightningfn:prolog)
	       (setf in (lightningfn:arg))
	       (setf r (lightningfn:reg-r 0))
	       (lightningfn:getarg-i r in) ;;use GETARG-L here for 64-bit inputs
	       (lightningfn:addi r r 1)
	       (lightningfn:retr r)
	       (lightningfn:epilog)
	       (make-jitfun :jit lightningfn:*jit*
			    :fun (lightningfn:emit))))))
    (let ((incr (make-incr)))
      (lambda (x)
	(cffi:foreign-funcall-pointer (jitfun-fun incr) (:convention :cdecl) :int x :int) ;use :LONG here for 64-bit inputs
	))))

(defun incr (x)
  (funcall (make-incr) x))

;; The following gave an unhandled memory fault on pc1400 and SBCL, which was due to the lightningfn:*jit* variable being garbage-collected eventually, which also garbage-collected the generated function pointer. Now, we don't return the generated function pointer directly, but wrap it in a JITFUN-instance.
;; (let ((incr (make-incr)))
;;   (utils:timesec (lambda () (funcall incr 5))))
;; But this didn't:
;; (let ((incr (make-incr)))
;;   (loop do (funcall incr 5)))

(defun incr-test ()
  (let ((iterations 1000)
	(start (get-internal-real-time))
	(onesecond internal-time-units-per-second))
    (do ((i 0 (+ i iterations))
	 (now (get-internal-real-time) (get-internal-real-time)))
	((>= (- now start) onesecond)
	 (float (/ i (/ (- now start) onesecond))))
      (utils:timeit (iterations)
	(incr 5)))))

(defun test ()
  (lightningfn:with-init ()
    (incr-test)))

;; On pc1400 and SBCL, the built-in function 1+ is 5 times faster than #'INCR:
;; CL-USER> (let ((incr (lambda (x) (1+ x)))
;; 	       (incr-fn-jit (make-incr)))
;; 	   (utils:timediff (funcall incr 5) (funcall incr-fn-jit 5) :showtimes t :maxtime 2))
;; Body1 is 0.00014877318 ms faster (5.3 speedup) per call than Body2.

;; On pc1400 and CLISP, the built-in function 1+ is 1.4 times faster than #'INCR:
;; CL-USER> (let ((incr (lambda (x) (1+ x)))
;; 	       (incr-fn-jit (make-incr)))
;; 	   (utils:timediff (funcall incr 5) (funcall incr-fn-jit 5) :showtimes t :maxtime 2))
;; Body1 is 0.001377976 ms faster (1.4 speedup) per call than Body2.

;; defining and calling a callback
(cffi:defcallback callback1 :int ((a :int) (b :float) (c :float))
  (format t "a:~A b:~A c:~A~%" a b c)
  (let ((res (round (+ a (* b c)))))
    (format t "res:~A~%" res)
    res))

(defun test-callback ()
  (lightningfn:with-init ()
    (lightningfn:with-new-state ()
      (let (in r)
	(lightningfn:prolog)
	(setf in (lightningfn:arg))
	(setf r (lightningfn:reg-r 0))
	(lightningfn:getarg-i r in)
	(lightningfn:addi r r 1)
	(lightningfn:prepare)
	(lightningfn:pushargr r)
	(lightningfn:pushargi-f 3.50)
	(lightningfn:pushargi-f 10.2)
	(lightningfn:finishi (cffi:callback callback1))
	(lightningfn:retval r)
	(lightningfn:retr r)
	(lightningfn:epilog)
	(let ((incr (lightningfn:emit)))
	  (cffi:foreign-funcall-pointer incr (:convention :cdecl) :int 5 :int))))))
