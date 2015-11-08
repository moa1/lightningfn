(defpackage #:lightningfn-ffi
  (:use #:cl #:cffi))

(defpackage #:lightningfn
  (:use #:cl #:trivial-garbage #:lightningfn-ffi #:cffi)
  ;; (:import-from :cffi
  ;; 		#:mem-ref #:with-foreign-objects #:with-foreign-object
  ;; 		#:foreign-alloc #:foreign-free #:null-pointer-p)
  (:export
   #:with-init
   #:new-state))

