(require 'org-element)


(defun import-stmt-p (string)
  (s-starts-with? "import" (s-trim string)))


(defun package-decl-p (string)
  (s-starts-with? "package" (s-trim string)))


(defun send-imports-to-top-and-dedup (string-list)
  (let* ((import-stmts (cl-remove-if-not
                        #'import-stmt-p
                        string-list))
         (rest (cl-remove-if
                (lambda (str)
                  (or (import-stmt-p str)
                      (package-decl-p str)))
                string-list)))
    (append (cl-delete-duplicates import-stmts) rest)))


(defun handle-imports ()
  (let* ((raw-file-content (buffer-substring-no-properties
                            (point-min)
                            (point-max)))
         (file-lines (split-string raw-file-content "\n"))
         (package-stmt (cl-find-if #'package-decl-p file-lines)))
    (progn
      (erase-buffer)
      (when package-stmt
        (insert package-stmt "\n"))
      (dolist (line (send-imports-to-top-and-dedup file-lines))
        (insert (concat line "\n")))
      (spacemacs/indent-region-or-buffer)
      (save-buffer))))


(add-hook 'org-babel-post-tangle-hook #'handle-imports)

org-babel-post-tangle-hook

(provide 'org-babel-handle-imports)
