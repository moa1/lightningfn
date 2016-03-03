(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :lightningfn)
(ql:quickload :utils)

(defun incr (x)
  (lightningfn:with-new-state
    (let (in r incr)
      (lightningfn:prolog)
      (setf in (lightningfn:arg))
      (setf r (lightningfn:reg-r 0))
      (lightningfn:getarg-i r in) ;;use GETARG-L here for 64-bit inputs
      (lightningfn:addi r r 1)
      (lightningfn:retr r)
      (lightningfn:epilog)
      (setf incr (lightningfn:emit))
      ;;(lightningfn:clear-state-
      (cffi:foreign-funcall-pointer incr (:convention :cdecl) :int x :int)))) ;use :LONG here for 64-bit inputs

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
