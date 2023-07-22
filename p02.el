;; L-99: Ninety-Nine Lisp Problems
;; https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
;; P02 (*) Find the last but one box of a list.
;;    Example:
;;    * (my-but-last '(a b c d))
;;    (C D)

(defun my-but-last (l)
  (cond
   ;; base cases
   ((eq l '()) '()) ;; empty list
   ((eq (cdr l) '()) '()) ;; check the bottom of the list
   ((eq (cddr l) '())  l) ;; double check the bottom of the list (cdr (cdr l))
   ;; recursive step
   ('t (my-but-last (cdr l))))

(my-but-last '(a b c d e)) ;; (d e) 
