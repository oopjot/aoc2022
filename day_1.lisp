(ql:quickload "str")

(defparameter *input* (uiop:read-file-string (merge-pathnames #p"input_1.txt"
                                        *default-pathname-defaults*)))

(defun sum-up (backpack)
  (let* ((strings (remove-if #'(lambda (x) (string= x "")) (str:split #\NewLine backpack)))
         (integers (mapcar #'(lambda (x) (parse-integer x)) strings)))
    (reduce #'+ integers)
  )
)

;; Part ONE
(first (sort (mapcar #'sum-up (str:split 
                            (str:concat (list #\NewLine #\Newline)) 
                            *input*))
      #'>))


;; Part TWO
(reduce #'+ (subseq (sort (mapcar #'sum-up (str:split 
                            (str:concat (list #\NewLine #\Newline)) 
                            *input*))
      #'>) 0 3))

