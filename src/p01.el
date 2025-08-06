;; https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
;; L-99: Ninety-Nine Lisp Problems
;; P01 (*) Find the last box of a list.
;;    Example:
;;    * (my-last '(a b c d))
;;    (D)

(defun my-last (l)
  (cond
   ;; base cases
   ((eq l '()) '()) ;; empty list 
   ((eq (cdr l) '()) l) ;; list consisting of a single element 
   ;; else and recursive step
   ('t (my-last (cdr l))))) ;; (my-last '(a b c d e)) --> (my-last '(b c d e)) --> (my-last '(c d e)) --> (my-last '(d e)) --> (my-last '(e))

(my-last '(a b c d e)) ;; (e) the last item in the list
