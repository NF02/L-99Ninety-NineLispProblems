;; L-99: Ninety-Nine Lisp Problems
;; https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
;; P04 (*) Find the number of elements of a list.

(defun my-length (l)
  (if (eq l '()) ;; if the list is empty, it returns 0
      0
    (+ 1 (my-length (cdr l)))))

(my-length '(a b c)) ;; return 3 
