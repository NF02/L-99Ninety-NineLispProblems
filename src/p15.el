(defun repli (lst n)
  "Replicate each element of LST N times."
  (apply #'append
         (mapcar (lambda (elem)
                   (make-list n elem))
                 lst)))

(repli '(a b c) 3) ;; => (a a a b b b c c c)
