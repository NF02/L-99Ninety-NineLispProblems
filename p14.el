;; https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
;; Duplicate the elements of a list.
;;    Example:
;;    * (dupli '(a b c c d))
;;    (A A B B C C C C D D)

(defun duplication (l)
  "A simple fuction for duplicate list item"
  ;; if list is nil return nil list
  (if (eq l '())
      '()
    ;; if the list has an element inside it, it renders the car element twice
    ;; and then makes a recursive call with the cdr element of the list.
    (append (list (car l) (car l)) (duplication (cdr l)))))


;; int cases
(duplication '(1 2 3)) ;; -> (1 1 2 2 3 3)

;; char cases
(duplication '(A B C C D)) ;; -> (A A B B C C C C D D)

;; empty cases
(duplication '()) ;; -> return nil list
