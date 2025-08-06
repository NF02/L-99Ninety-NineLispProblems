(defun encode-direct (lst)
  "Perform run-length encoding directly on LST, simplifying single elements."
  (let ((result '())        ; Initialize empty result list
        (current (car lst)) ; Track current element being counted
        (count 1))          ; Initialize count for current element
    
    ;; Iterate through the list starting from second element
    (dolist (elem (cdr lst) 
      ;; After processing all elements, handle the last element and return result
      (reverse (cons (if (> count 1) 
                    (list count current) 
                  current)
               result))
      
      ;; Check if current element matches the one we're counting
      (if (equal elem current)
          ;; If match, increment count
          (setq count (1+ count))
        
        ;; When element changes
        (progn
          ;; Add encoded element to result:
          ;; - As (count element) if repeated
          ;; - As just element if single
          (push (if (> count 1) 
                  (list count current) 
                current)
                result)
          
          ;; Reset tracking for new element
          (setq current elem)
          (setq count 1)))))
    
(encode-direct '(a a a a b c c a a d e e e e)) ;; ((4 A) B (2 C) (2 A) D (4 E))"
