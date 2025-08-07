(require 'cl-lib)

(defun combinations (lst k)
  "Generate all combinations of K elements from LST."
  (cond ((= k 0) '(()))
        ((null lst) '())
        (t (append (mapcar (lambda (x) (cons (car lst) x))
                           (combinations (cdr lst) (- k 1)))
                   (combinations (cdr lst) k)))))

(defun seq-difference (lst1 lst2)
  "Return elements in LST1 that are not in LST2."
  (cl-remove-if (lambda (x) (member x lst2)) lst1))

(defun group3 (lst)
  "Generate all possible groups of 2, 3, and 4 from a list of 9 elements."
  (if (not (= (length lst) 9))
      (error "List must have exactly 9 elements")
    (let ((result '()))
      (dolist (g2 (combinations lst 2))
        (let* ((remaining-after-g2 (seq-difference lst g2))
               (g3-combinations (combinations remaining-after-g2 3)))
          (dolist (g3 g3-combinations)
            (let* ((remaining-after-g3 (seq-difference remaining-after-g2 g3))
                   (g4 remaining-after-g3))
              (push (list g2 g3 g4) result)))))
      result)))

(defun group (lst sizes)
  "Generate all possible groups of the given SIZES from LST."
  (if (not (= (apply '+ sizes) (length lst)))
      (error "Sum of sizes must equal the length of the list")
    (let ((result '()))
      (cl-labels ((recurse (lst sizes acc)
                   (if (null sizes)
                       (push (reverse acc) result)
                     (let ((k (car sizes)))
                       (dolist (c (combinations lst k))
                         (recurse (seq-difference lst c)
                                  (cdr sizes)
                                  (cons c acc)))))))
        (recurse lst sizes '())
        result))))

;; P27-1
(group3 '(aldo beat carla david evi flip gary hugo ida)) ;; => (((hugo ida) (evi flip gary) (aldo beat carla david)) ((hugo ida) (david flip gary) (aldo beat carla evi)) ((hugo ida) (david evi gary) (aldo beat carla flip)) ((hugo ida) (david evi flip) (aldo beat carla gary)) ((hugo ida) (carla flip gary) (aldo beat david evi)) ...

;; P27-2
(group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5)) ;; => (((hugo ida) (flip gary) (aldo beat carla david evi)) ((hugo ida) (evi gary) (aldo beat carla david flip)) ...

