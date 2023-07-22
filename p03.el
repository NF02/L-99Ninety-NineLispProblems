;; L-99: Ninety-Nine Lisp Problems
;; https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
;; P03 (*) Find the K'th element of a list.
;;    The first element in the list is number 1.
;;    Example:
;;    * (element-at '(a b c d e) 3)
;;    C

(defun my-element (l k)
  (cond
   ;; bases cases
   ((eq l '()) '()) ;; protection condition, because if the selected value is in overflow or underflow it avoids the program crash
   ((eq k 1) (car l)) ;; checks if k = 1 and returns the car of the list
   ;; recursive step
   ('t (my-element (cdr l) (- k 1))))) ;; 

(my-element '(a b c d e) 2) ;; b my-element ((a b c d e) k) k = index + 1 "(0 1 2 3 4) -> (1 2 3 4 5)" 
