;; Helper function to remove element at position k from list
;; Note: Positions are 1-based (first element is position 1)
;; Parameters:
;;   lst - the input list
;;   k - position to remove (1-based index)
;; Returns:
;;   List with element at position k removed
(defun remove-at (lst k)
  ;; Check if k is out of bounds (<=0 or > length)
  (if (or (<= k 0) (> k (length lst)))
      lst  ; Return original list if k is invalid
    (remove-at-helper lst 1 k)))  ; Otherwise call helper function

;; Recursive helper for remove-at
;; Parameters:
;;   lst - current list being processed
;;   current - current position (1-based)
;;   k - position to remove
;; Returns:
;;   List with element at position k removed
(defun remove-at-helper (lst current k)
  (cond ((null lst) nil)  ; Base case: empty list
        ;; If current position matches k, skip this element
        ((= current k) (remove-at-helper (cdr lst) (+ current 1) k))
        ;; Otherwise include current element and continue
        (t (cons (car lst) (remove-at-helper (cdr lst) (+ current 1) k)))))

;; Main function to randomly select n elements from a list
;; Parameters:
;;   lst - the input list
;;   n - number of elements to select
;; Returns:
;;   A list of n randomly selected elements
(defun rnd-select (lst n)
  ;; Base case: if n is 0 or negative, or list is empty, return nil
  (if (or (<= n 0) (null lst))
      nil
    ;; Select a random element and recurse
    (let* ((len (length lst))  ; Get current length of list
           (pos (random len))  ; Generate random position (0 to len-1)
           (selected (nth pos lst))  ; Get element at random position
           ;; Remove the selected element from list using helper function
           (remaining (remove-at lst (+ pos 1))))
      ;; Cons the selected element with result of recursive call
      (cons selected (rnd-select remaining (- n 1))))))


(rnd-select '(a b c d e f g h) 3) ; => (c g e)

