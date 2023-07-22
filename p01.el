;; https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
;; L-99: Ninety-Nine Lisp Problems
;; P01 (*) Find the last box of a list.
;;    Example:
;;    * (my-last '(a b c d))
;;    (D)

(defun my-last (l)
  (cond
   ;; base cases
   ((eq l '()) ())
   ((eq (cdr l) '()) l)
   ;; recursive
   ('t (my-last (cdr l)))))

(my-last '(a b c d e)) ;; (e) the last item in the list
