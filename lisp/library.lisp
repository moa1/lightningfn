(in-package :lightningfn)

(cffi:define-foreign-library liblightningfn
  (:unix (:or "lightningfn.so" "/home/toni/soft/lightningfn/lightningfn.so"))
  (t (:default "lightningfn")))

(cffi:use-foreign-library liblightningfn)
