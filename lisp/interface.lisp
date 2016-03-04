;;(load "/home/toni/quicklisp/setup.lisp")
;;(ql:quickload :lightningfn)

(in-package :lightningfn)

;; initialize liblightning on load. Use WITH-INIT if you want to pass a program name to #'INIT-FN-JIT.
(eval-when (:load-toplevel)
  (lightningfn-ffi::init-fn-jit (cffi:null-pointer)))

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

(defparameter *new-state-calls* 0 "The number of calls to #'NEW-STATE after it triggered a normal garbage collection.")
(defparameter *new-state-max-calls-before-gc* 1000 "The number of calls to #'NEW-STATE before it triggers a normal garbage collection. NIL means normal garbage collection is never initiated.")
(defparameter *new-state-calls-full* 0 "The number of calls to #'NEW-STATE after it triggered a full garbage collection.")
(defparameter *new-state-max-calls-before-full-gc* 25000 "The number of calls to #'NEW-STATE before it triggers a full garbage collection. NIL means full garbage collection is never initiated.")
(flet ((new-state-maybe-gc ()
	 (incf *new-state-calls*)
	 (incf *new-state-calls-full*)
	 (when (and *new-state-max-calls-before-gc* (> *new-state-calls* *new-state-max-calls-before-gc*))
	   (setf *new-state-calls* 0)
	   (tg:gc))
	 (when (and *new-state-max-calls-before-full-gc* (> *new-state-calls-full* *new-state-max-calls-before-full-gc*))
	   (setf *new-state-calls-full* 0)
	   (tg:gc :full t))))
  ;; Rationale for the CLEAR-STATE-MANUALLY parameter: It seems that calling #'LIGHTNINGFN-FFI::FN-JIT-CLEAR-STATE-UNWRAPPED after #'LIGHTNINGFN-FFI::FN-JIT-EMIT has been called (and not calling FN-JIT-CLEAR-STATE-UNWRAPPED in the finalizer, because otherwise there are memory faults) speeds up the "incr" example above quite a bit, from 5.5 needed time to 3.5 needed time (or more than doubles speed on pc1400).
  (defun new-state (&key (clear-state-manually nil))
    "Make a new JIT-STATE instance.
If CLEAR-STATE-MANUALLY is non-NIL, #'CLEAR-STATE must be called manually.
Automatically triggers a normal garbage collection every *NEW-STATE-MAX-CALLS-BEFORE-GC* calls to #'NEW-STATE, and a full garbage collection every *NEW-STATE-MAX-CALLS-BEFORE-FULL-GC* calls. This is needed for some LISPs."
    (new-state-maybe-gc)
    (make-instance 'lightningfn-ffi::jit-state :clear-state-manually clear-state-manually)))

(defun clear-state (jit)
  "This call cleanups any data not required for jit execution.
Note that it must not be called before any call to #'PRINT-JIT or #'ADDRESS, as this call destroy the GNU lightning intermediate representation.
This function must not be called twice for a given JIT. If #'NEW-STATE is called with CLEAR-STATE-MANUALLY==NIL, then an equivalent to this function will be called in the garbage collector."
  (lightningfn-ffi::fn-jit-clear-state-unwrapped (lightningfn-ffi::jit-state-ptr jit)))

(defun reg-r (reg-index)
  "Return the caller-save register with index REG-INDEX."
  (declare (type (and unsigned-byte fixnum) reg-index))
  (assert (< reg-index (lightningfn-ffi::fn-jit-r-num)))
  (lightningfn-ffi::fn-jit-r reg-index))

(defun reg-v (reg-index)
  "Return the callee-save register with index REG-INDEX."
  (declare (type (and unsigned-byte fixnum) reg-index))
  (assert (< reg-index (lightningfn-ffi::fn-jit-v-num)))
  (lightningfn-ffi::fn-jit-v reg-index))

(defun reg-f (reg-index)
  "Return the floating-point register with index REG-INDEX."
  (declare (type (and unsigned-byte fixnum) reg-index))
  (assert (< reg-index (lightningfn-ffi::fn-jit-f-num)))
  (lightningfn-ffi::fn-jit-f reg-index))

(defvar *jit* nil "The global JIT-STATE object.")

(defmacro with-new-state (&environment env (&key (clear-state-manually t)) &body body)
  "Binds *JIT* to a new JIT-STATE and executes BODY.
CLEAR-STATE-MANUALLY is passed to #'NEW-STATE.
If CLEAR-STATE-MANUALLY is non-NIL, the BODY is wrapped by an UNWIND-PROTECT form which calls #'CLEAR-STATE on exit. (This means that #'CLEAR-STATE must NOT be called in BODY!)"
  (let ((clear-state-manually-sym (gensym "CLEAR-STATE-MANUALLY"))
	(form-prot `(unwind-protect ;prevent memory leak
			 (progn ,@body)
		      (clear-state *jit*)))
	(form-unpr `(progn ,@body)))
    `(let* ((,clear-state-manually-sym ,clear-state-manually)
	    (*jit* (new-state :clear-state-manually ,clear-state-manually-sym)))
       ,(cond
	 ((eq clear-state-manually nil)
	  form-unpr)
	 ((constantp clear-state-manually env)
	  form-prot)
	 (t
	  `(if ,clear-state-manually-sym
	       ,form-prot
	       ,form-unpr))))))

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

(defun unique (list)
  "Return a newly consed list with duplicate elements in LIST removed, and in the same order if first occurrence as in LIST."
  (let ((ht (make-hash-table :test #'eq)))
    (loop for i in list do
	 (when (not (gethash i ht))
	   (setf (gethash i ht) (hash-table-count ht))))
    (let ((u (loop for key being the hash-key in ht collect (cons key (gethash key ht)))))
      (setf u (sort u #'< :key #'cdr))
      (mapcar #'car u))))

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

Which symbol with name "ADDRESS" should be accessible in #1#?
   [Condition of type SYSTEM::SIMPLE-PACKAGE-ERROR]

Restarts:
 0: [LIGHTNINGFN] #<PACKAGE LIGHTNINGFN>
 1: [COMMON-LISP-USER] #<PACKAGE COMMON-LISP-USER>
|#
