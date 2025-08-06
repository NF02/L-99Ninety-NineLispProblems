(defun drop (elements n)
  "Drop every N'th element from ELEMENTS."
  ;; Initialize counter to track position in list (1-based index)
  (let ((counter 1)
        ;; Initialize empty result list we'll build incrementally
        (result '()))
    
    ;; Iterate through each element in the input list
    (dolist (elem elements 
		  ;; After processing all elements, reverse and return the result
		  ;; (We built it in reverse order for efficiency)
		  (nreverse result))
      
      ;; Check if current position is a multiple of N
      (if (zerop (mod counter n))
          ;; If yes, skip this element (just increment counter)
          (setq counter (1+ counter))
        
        ;; Otherwise, keep this element
        (progn
          ;; Add element to our result (using push for O(1) prepend)
          (push elem result)
          ;; Increment position counter
          (setq counter (1+ counter)))))))

(drop '(a b c d e f g h i k) 3)
