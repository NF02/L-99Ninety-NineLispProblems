;; L-99: Ninety-Nine Lisp Problems
;; https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
;; P05 (*) Reverse a list.

(defun my-reverse (l)
  (if (eq l '())
      '()
    (append
     (my-reverse (cdr l))
     (list (car l)))))
(my-reverse '(a b c)) ; (c b a) 
