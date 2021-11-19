;;; levenshtein --- Summary
;;; Commentary:
;;; Based on the video [[https://www.youtube.com/watch?v=Cu7Tl7FGigQ][How do Spell Checkers work?]]
;;; Code:
(require 'cl-libs)

(defun lev (a b)
  "Computes the Levenshtein Edit Distance between strings A and B using the original recursive algorithm."
  (let ((ǀaǀ (length a))
        (ǀbǀ (length b)))
    (cond ((= 0 ǀaǀ) ǀbǀ)
          ((= 0 ǀbǀ) ǀaǀ)
          (t
           (let ((head-a (substring a 0 1))
                 (head-b (substring b 0 1))
                 (tail-a (substring a 1))
                 (tail-b (substring b 1)))
             (if (string-equal head-a head-b) (lev tail-a tail-b)
               (+ 1 (min (lev tail-a b)
                         (lev a tail-b)
                         (lev tail-a tail-b)))))))))

(defun 2d-index (cols i j)
  "Calculates index of a one-dimensional vector as if it were two-dimensional.
It returns the value that would be at the equivalent of the col I and row J.

COLS is the number of columns in the two-dimensional vector.

I and J are zero-indexed."
  (if (>= i cols) (error "Argument I must be less than number of COLS")
    (+ (* j cols) i)))

(defun pretty-print (v cols)
  "Pretty print vector V as if it were a matrix with COLS columns."
  (string-join
   (mapcar (lambda (row)
             (format "[%s]"
                     (string-join
                      (mapcar (lambda (col) (format "%d" (aref v (2d-index cols col row))))
                              (number-sequence 0 (- cols 1)))
                      ", ")))
           (number-sequence 0 (- (/ (length v) cols) 1)))
   "\n"))

;; TODO: implement functionally rather than imperatively.
(defun lev-matrix (a b)
  "Computes the Levenshtein Edit Distance between strings A and B.
Uses a matrix to store substring distances and avoid recomputation.

The matrix is initialized with the distances for the input strings to
the empty strings.  To calculate any subsequent distance: compare the
characters, if they are the same, the value is the same as in the cell
one above and one to the left; otherwise it is one plus the minimum of
the cells to the left, one above and one to the left, or one above.

For example, the distances between 'bat' and 'catch' is displayed below:
  _ c a t c h
_ 0 1 2 3 4 5
b 1 1 2 3 4 5
a 2 2 1 2 3 4
t 3 3 2 1 2 3

Underscores represent the empty string."
  (let* ((cols (+ 1 (length b)))
         (rows (+ 1 (length a)))
         (l (* cols rows)))
    (setq v (make-vector l nil))
    (dotimes (i cols)
      (setf (aref v i) i))
    (dotimes (j rows)
      (setf (aref v (2d-index cols 0 j)) j))
    (dotimes (j rows)
      (dotimes (i cols)
        ;; Skip the empty string iteration, it was already populated.
        (if (and (> i 0) (> j 0))
            (let ((ɑ (aref a (- j 1)))
                  (β (aref b (- i 1))))
              (message "setting %d" (2d-index cols i j))
              (setf (aref v (2d-index cols i j))
                    (if (eq ɑ β) (aref v (2d-index cols (- i 1) (- j 1)))
                      (+ 1 (min (aref v (2d-index cols (- i 1) j))
                                (aref v (2d-index cols i (- j 1)))
                                (aref v (2d-index cols (- i 1) (- j 1)))))))))))
    (aref v (- (* cols rows) 1))))

(provide 'levenshtein)
;;; levenshtein.el ends here
