(require 'org-element)


(defun is-no-tangle-start (string)
  (s-ends-with? "==== No Tangle Start ====" string))


(defun is-no-tangle-end (string)
  (s-ends-with? "==== No Tangle End ====" string))


(defun omit-no-tangle-from-strings (list)
  (reverse
   (car
    (reduce (lambda (acc string)
              (let ((big-acc (car acc))
                    (smol-acc (cadr acc)))
                (cond ((is-no-tangle-start string)
                       `(,big-acc ,(cons string smol-acc)))
                      ((is-no-tangle-end string)
                       `(,big-acc nil))
                      (t
                       (if (null smol-acc)
                           `(,(cons string big-acc) ,smol-acc)
                           `(,big-acc ,(cons string smol-acc)))))))
            list
            :initial-value '(nil nil)))))


(defun omit-no-tangle ()
  (let* ((raw-file-content (buffer-substring-no-properties
                            (point-min)
                            (point-max)))
         (file-lines (split-string raw-file-content "\n"))
         (file-lines-without-no-tangle
          (omit-no-tangle-from-strings file-lines)))
    (progn
      (erase-buffer)
      (dolist (line file-lines-without-no-tangle)
        (insert (concat line "\n"))))))


(add-hook 'org-babel-tangle-body-hook #'omit-no-tangle)


(provide 'org-babel-no-tangle)
