(defun combination (k lst)
  "Generate the combinations of K distinct objects chosen from the N elements of a list"
  (cond 
   ;; Base case 1: When we need to select 0 elements, return list with empty combination
   ((= k 0) '(()))
   
   ;; Base case 2: When list is empty but we still need to select elements, return empty list
   ((null lst) '())
   
   ;; Recursive case:
   (t 
    ;; Combine two possibilities:
    ;; 1. Combinations that include the first element
    ;; 2. Combinations that exclude the first element
    (append 
     ;; 1. Take all combinations of size (k-1) from the remaining elements (cdr lst)
     ;;    and prepend the current first element (car lst) to each of them
     (mapcar (lambda (x) (cons (car lst) x))
             (combination (- k 1) (cdr lst)))
      
      ;; 2. Take all combinations of size k from the remaining elements (cdr lst)
      (combination k (cdr lst))))))



(combination 3 '(a b c d e f)) ;; => ((a b c) (a b d) (a b e) (a b f) (a c d) (a c e) (a c f) (a d e) (a d f) (a e f) (b c d) (b c e) ...)
