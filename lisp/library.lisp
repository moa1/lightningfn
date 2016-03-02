(in-package :lightningfn)

(cffi:define-foreign-library liblightningfn
  (:unix (:or "liblightningfn.so" "/home/toni/soft/lightningfn/liblightningfn.so"))
  (t (:default "liblightningfn")))

(cffi:use-foreign-library liblightningfn)
