(in-package :matfyz)

(deftype option (inner-type)
  `(or null ,inner-type))

(deftype list-of (element-type)
  `(serapeum:soft-list-of ,element-type))
