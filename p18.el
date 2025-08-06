(defun slice (lst i k)
  "Extract a slice from LST between positions I and K (inclusive).
   Indices are 1-based. Both I and K must be valid positions in the list."
  (let ((result '())
        (index 1))
    (dolist (elem lst (reverse result))
      (when (and (>= index i) (<= index k))
        (push elem result))
      (setq index (1+ index)))))

(slice '(a b c d e f g h i k) 3 7)  ; => (C D E F G)
(slice '(1 2 3 4 5) 1 5)            ; => (1 2 3 4 5)
