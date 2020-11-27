;;; macro_try.el --- kyt/setq-one

;;; Commentary:
;;

;;; Code:

(defmacro ++ (x)
  "Increase X by one."
  (print x)
  `(setq ,x (1+ ,x)))


(defmacro kyt/setq-one (x y)
  "My setq for configuration, one variable only.
X: the var to set, should not be quoted.  Y: the value."
  (let ((set-fun (if (local-variable-if-set-p x)
                     'setq-default
                   'setq)))
    `(,set-fun ,x ,y)))

(defmacro kyt/setq (&rest args)
  "My setq for configuration, ARGS are VAR VAL VAR VAL ..."
  (let ((arg-pairs (-partition-all 2 args)))
    (when (not (= (length (-last-item arg-pairs)) 2))
      (error "Odd number arguments, can't pair arguments as VAR-VAL pairs"))
    (cons 'progn
          (--map (cons 'kyt/setq-one it) arg-pairs)))
  )

(defvar a-non-local-var 100 "Testing var for `kyt/setq-one'.")
(defvar-local a-local-var 100 "Testing var for `kyt/setq-one'.")

(kyt/setq-one a-local-var (list 1 2 3))

(kyt/setq-many a 2 b 4 c 6)



(provide 'macro_try)

;;; macro_try.el ends here
