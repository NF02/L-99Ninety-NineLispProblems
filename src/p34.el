(defun totient-phi (m &optional r count)
  "Recursively calculate Euler's totient function φ(m). Counts numbers from 1 to m-1 that are coprime with m."
  (let ((r (or r 1))       ; Start from 1 if not provided
        (count (or count 0))) ; Initialize count to 0
    (cond
     ((= m 1) 1)           ; Special case φ(1) = 1
     ((>= r m) count)      ; Base case: finished checking all numbers
     ((coprime r m)        ; If current r is coprime with m
      (totient-phi m (1+ r) (1+ count))) ; Recurse with incremented count
     (t                   ; If not coprime
      (totient-phi m (1+ r) count))))) ; Recurse with same count

;; Example usage
;; m = 10: r = 1,3,7,9;
(totient-phi 10) ;; => thus phi(m) = 4
