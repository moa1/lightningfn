;;(load "/home/toni/quicklisp/setup.lisp")
;;(ql:quickload :lightningfn)

(in-package :lightningfn)

(defmacro with-init ((&key (progname nil) alloc-function realloc-function free-function) &body body)
  "Call #'LIGHTNINGFN-FFI::INIT-FN-JIT before executing BODY and #'LIGHTNINGFN-FFI::FINISH-FN-JIT after BODY.
If any of ALLOC-FUNCTION, REALLOC-FUNCTION, or FREE-FUNCTION are non-NIL, they will be passed to #'LIGHTNINGFN-FFI::FN-JIT-SET-MEMORY-FUNCTIONS before anything is executed."
  (let ((progname-sym (gensym "PROGNAME"))
	(alloc-function-sym (gensym "ALLOC-FUNCTION"))
	(realloc-function-sym (gensym "REALLOC-FUNCTION"))
	(free-function-sym (gensym "FREE-FUNCTION"))
	(old-alloc-function-sym (gensym "OLD-ALLOC-FUNCTION"))
	(old-realloc-function-sym (gensym "OLD-REALLOC-FUNCTION"))
	(old-free-function-sym (gensym "OLD-FREE-FUNCTION")))
    `(let ((,progname-sym ,progname)
	   (,alloc-function-sym ,alloc-function)
	   (,realloc-function-sym ,realloc-function)
	   (,free-function-sym ,free-function))
       (when (not (and (null ,alloc-function-sym) (null ,realloc-function-sym) (null ,free-function-sym))) ;TODO: test this
	 (when (or (null ,alloc-function-sym) (null ,realloc-function-sym) (null ,free-function-sym))
	   (cffi:with-foreign-objects ((,old-alloc-function-sym :pointer) (,old-realloc-function-sym :pointer) (,old-free-function-sym :pointer))
	     (lightningfn-ffi::fn-jit-get-memory-functions (cffi:mem-aptr ,old-alloc-function-sym :pointer) (cffi:mem-aptr ,old-realloc-function-sym :pointer) (cffi:mem-aptr ,old-free-function-sym :pointer))
	     (when (null ,alloc-function-sym)
	       (setf ,alloc-function-sym ,old-alloc-function-sym))
	     (when (null ,realloc-function-sym)
	       (setf ,realloc-function-sym ,old-realloc-function-sym))
	     (when (null ,free-function-sym)
	       (setf ,free-function-sym ,old-free-function-sym))))
	 (lightningfn-ffi::fn-jit-set-memory-functions ,alloc-function-sym ,realloc-function-sym ,free-function-sym))
       (lightningfn-ffi::init-fn-jit (if (null ,progname-sym) (cffi:null-pointer) ,progname-sym))
       (unwind-protect
	    ,@body
	 (lightningfn-ffi::finish-fn-jit)))))

(defun test-automatic-garbage-collection (&key (mb 10) (rep 10) (explicit-gc nil) (alternative-memory-functions nil))
  "Allocate a number of LIGHTNINGFN-FFI::JIT-STATE so that MB Megabytes are allocated in total. Repeat this REP times."
  (let ((alloc-function (if alternative-memory-functions (cffi:foreign-symbol-pointer "malloc") nil))
	(realloc-function (if alternative-memory-functions (cffi:foreign-symbol-pointer "realloc") nil))
	(free-function (if alternative-memory-functions (cffi:foreign-symbol-pointer "free") nil)))
    (with-init (:alloc-function alloc-function :realloc-function realloc-function :free-function free-function)
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
	     (finish-output))))))

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

(defun new-state ()
  "Make a new JIT-STATE instance."
  (make-instance 'lightningfn-ffi::jit-state))

(defun reg-r (reg-index)
  "Return the caller-save register with index REG-INDEX."
  (assert (< reg-index (lightningfn-ffi::fn-jit-r-num)))
  (lightningfn-ffi::fn-jit-r reg-index))

(defun reg-v (reg-index)
  "Return the callee-save register with index REG-INDEX."
  (assert (< reg-index (lightningfn-ffi::fn-jit-v-num)))
  (lightningfn-ffi::fn-jit-v reg-index))

(defun reg-f (reg-index)
  "Return the floating-point register with index REG-INDEX."
  (assert (< reg-index (lightningfn-ffi::fn-jit-f-num)))
  (lightningfn-ffi::fn-jit-f reg-index))

(defvar *jit* nil "The global JIT-STATE object.")

(defmacro with-new-state (&body body)
  "Binds *JIT* to a new JIT-STATE and executes BODY."
  `(let ((*jit* (new-state)))
     ,@body))

(defun clear-state ()
  "This call cleanups any data not required for jit execution.
Note that it must not be called before any call to #'PRINT or #'ADDRESS, as this call destroy the GNU lightning intermediate representation.
Note also that it is not required to invoke this call, as it will be done automatically by garbage collection."
  (lightningfn-ffi::fn-jit-clear-state-raw (lightningfn-ffi::jit-state-ptr *jit*)))

;;; Wrapper generator
;; generate wrapper functions for the functions LIGHTNINGFN-FFI::FN-JIT-* that use the global LIGHTNINGFN-FFI::JIT-STATE *JIT*.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun maptree (atom-function tree)
    "Traverse TREE and non-destructively copy its structure but replace its atoms with the value returned by calling #'ATOM-FUNCTION with the atom."
    (labels ((rec (tree)
	       (maplist (lambda (x) (let ((head (car x)))
				      (if (atom head) (funcall atom-function head) (rec head))))
			tree)))
      (rec tree)))

  (defun convert-package (symbol &optional (package *package*))
    "Return a symbol with the same symbol-name as SYMBOL, but symbol-package changed to PACKAGE."
    (let ((name (symbol-name symbol)))
      (intern name package)))
  
  (defvar *to-be-exported-functions* nil "A list of FN-JIT-* function names (without the FN-JIT-prefix) from package LIGHTNINGFN-FFI to be exported.")
  
  (defmacro generate-fn-jit-functions ()
    "Emit forms that will generate wrapper functions of the functions listed in LIGHTNINGFN-FFI::*FN-JIT-INVENTORY*."
    (let ((forms nil))
      (loop for entry in lightningfn-ffi::*fn-jit-inventory* do
	   (labels ((strip-function-name (symbol)
		      (let* ((prefix "FN-JIT-")
			     (prefix-len (length prefix))
			     (str (string symbol)))
			(assert (equal prefix (subseq (string symbol) 0 prefix-len)))
			(intern (subseq str prefix-len))))
		    (make-defun-form (symbol args function-symbol)
		      (assert (equal '(jit jit-state) (car args)))
		      (let ((new-function-name (strip-function-name symbol))
			    (new-args (mapcar #'first (cdr args))))
			(push new-function-name *to-be-exported-functions*)
			`(defun ,new-function-name ,new-args
			   (,function-symbol *jit* ,@new-args)))
		      ))
	     (let* ((function-symbol (lightningfn-ffi::fn-jit-function-symbol entry))
		    (symbol (convert-package function-symbol))
		    (args (maptree #'convert-package (lightningfn-ffi::fn-jit-function-args entry)))
		    (defun-form (make-defun-form symbol args function-symbol)))
	       ;;(format t "(defcfun* ~A ~A)~%=>  ~A~%" symbol args defun-form)
	       (push defun-form forms)
	       )))
      (append '(progn) forms)
      ))

  (generate-fn-jit-functions))

#|
Example:
(lightningfn:with-init ()
  (lightningfn:with-new-state
    (lightningfn:prolog)
    (let* ((in (lightningfn:arg))
	   (r (lightningfn:reg-r 0)))
      (lightningfn:getarg-i r in)
      (lightningfn:addi r r 1)
      (lightningfn:retr r)
      (let ((incr (lightningfn:emit)))
	;;(lightningfn:clear-state) ;#'CLEAR-STATE may not be called twice for an object! (it will be called in the finalizer)
	(cffi:foreign-funcall-pointer incr (:convention :stdcall) :int 5 :int)))))
|#

;;;; Convenience macros

(defun unique (list)
  "Return a newly consed list with duplicate elements in LIST removed, and in the same order if first occurrence as in LIST."
  (let ((ht (make-hash-table :test #'eq)))
    (loop for i in list do
	 (when (not (gethash i ht))
	   (setf (gethash i ht) (hash-table-count ht))))
    (let ((u (loop for key being the hash-key in ht collect (cons key (gethash key ht)))))
      (setf u (sort u #'< :key #'cdr))
      (mapcar #'car u))))

(defun imperative-bind-walker (forms)
  (let ((vars nil))
    (labels ((set!-detector (form)
	       ;;(princ *package*)
	       ;;(format t "form:~A        (" form)
	       ;;(prog1
		   (and
		    ;;  (or (format t "~A " (listp form)) t)
		    (listp form)
		    ;;  (or (format t "~A " (= (length form) 3)) t)
		    (= (length form) 3)
		    ;;  (or (format t "~A " (intern (string (car form)) *package*)) t)
		    (eq (intern (string (car form))) (intern "SET!"))
		    ;;  (or (format t "~A SUCCESS" (symbolp (cadr form))) t)
		    (symbolp (cadr form)))
		 ;;(format t ")~%")
		 ;;)
	       )
	     (rec (tree)
	       (cond
		 ((atom tree) tree)
		 (t
		  (maplist (lambda (x)
			     ;;(format t "x:~A~%" x)
			     (let ((head (car x)))
			       (cond
				 ((set!-detector head)
				  (let ((var (cadr head))
					(value (caddr head)))
				    (push var vars)
				    (list 'setf var (car (rec (list value))))))
				 ((atom head) head)
				 (t (rec head)))))
			   tree)))))
      (let ((body (rec forms)))
	`(let (,@(unique (nreverse vars))) ,@body)))))

(assert (equal (imperative-bind-walker '((a) (set! x b) (c (set! y (set! d (e))))))
	       '(let (x y d) (a) (setf x b) (c (setf y (setf d (e)))))))

(defmacro imperative-bind (&body body)
  "Create new variable bindings of certain variables in BODY.
Find forms of the form (SETF! variable value) in BODY, and replaces them with normal (SETF variable value).
The variables are all bound to NIL, and then BODY is executed."
  (imperative-bind-walker body))

(defmacro with-jit ((&key (progname nil) alloc-function realloc-function free-function) &body body)
  "Convenience macro calling (WITH-INIT (...) (WITH-NEW-STATE (IMPERATIVE-BIND ,@BODY)))."
  (let ((progname-sym (gensym "PROGNAME"))
	(alloc-function-sym (gensym "ALLOC-FUNCTION"))
	(realloc-function-sym (gensym "REALLOC-FUNCTION"))
	(free-function-sym (gensym "FREE-FUNCTION")))
    `(let ((,progname-sym ,progname)
	   (,alloc-function-sym ,alloc-function)
	   (,realloc-function-sym ,realloc-function)
	   (,free-function-sym ,free-function))
       (with-init (:progname ,progname-sym :alloc-function ,alloc-function-sym :realloc-function ,realloc-function-sym :free-function ,free-function-sym)
	 (with-new-state
	   (imperative-bind
	     ,@body))))))

#|
Example:
(lightningfn:with-jit ()
  (lightningfn:prolog)
  (set! in (lightningfn:arg))
  (set! r (lightningfn:reg-r 0))
  (lightningfn:getarg-i r in)
  (lightningfn:addi r r 1)
  (lightningfn:retr r)
  (set! incr (lightningfn:emit))
  ;;(lightningfn:clear-state) ;#'CLEAR-STATE may not be called twice for an object! (it will be called in the finalizer)
  (cffi:foreign-funcall-pointer incr (:convention :stdcall) :int 5 :int))

or when (use-package :lightning)
(with-jit ()
  (prolog)
  (set! in (arg))
  (set! r (reg-r 0))
  (getarg-i r in)
  (addi r r 1)
  (retr r)
  (set! incr (emit))
  ;;(clear-state) ;#'CLEAR-STATE may not be called twice for an object! (it will be called in the finalizer)
  (cffi:foreign-funcall-pointer incr (:convention :stdcall) :int 5 :int))
|#

#|
Symbol name conflicts
---------------------

On SBCL:

Select a symbol to be made accessible in package COMMON-LISP-USER:
  1. LIGHTNINGFN:ARG
  2. SB-DEBUG:ARG
  
Enter an integer (between 1 and 2): 1

Select a symbol to be made accessible in package COMMON-LISP-USER:
  1. LIGHTNINGFN:ADDR
  2. SB-ALIEN:ADDR
  
Enter an integer (between 1 and 2): 1


On CLISP:

(USE-PACKAGE (#<PACKAGE LIGHTNINGFN>) #1=#<PACKAGE
     COMMON-LISP-USER>): 1 name conflicts remain
Which symbol with name "ADDRESS" should be accessible in #1#?
   [Condition of type SYSTEM::SIMPLE-PACKAGE-ERROR]

(USE-PACKAGE (#<PACKAGE LIGHTNINGFN>) #1=#<PACKAGE
     COMMON-LISP-USER>): 1 name conflicts remain
Which symbol with name "SET!" should be accessible in #1#?
   [Condition of type SYSTEM::SIMPLE-PACKAGE-ERROR]
|#

#|
(load "/home/toni/quicklisp/setup.lisp")
(ql:quickload :lightningfn)
(ql:quickload :utils)
|#

#|
;; defining and calling a callback
(cffi:defcallback callback1 :int ((a :int) (b :float) (c :float))
  (format t "a:~A b:~A c:~A~%" a b c)
  (let ((res (round (+ a (* b c)))))
    (format t "res:~A~%" res)
    res))
(lightningfn:with-jit ()
  (lightningfn:prolog)
  (set! in (lightningfn:arg))
  (set! r (lightningfn:reg-r 0))
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
  (set! incr (lightningfn:emit))
  (cffi:foreign-funcall-pointer incr (:convention :stdcall) :int 5 :int))
|#

;; TODO: It seems that calling #'LIGHTNINGFN-FFI::FN-JIT-CLEAR-STATE-RAW after #'LIGHTNINGFN-FFI::FN-JIT-EMIT has been called (and not calling FN-JIT-CLEAR-STATE-RAW in the finalizer, because otherwise there are memory faults) speeds up the "incr" example above quite a bit, from 5.5 needed time to 3.5 needed time. I should change the interface by writing a macro that automates that, and then exporting that macro and remove the other exported macros.
