;; TODO under construction

(defconst *directories* '("/Users/jslee/Lab_Notes"
                          "/Users/jslee/InferenceEngine"))

(defun check-for-staged (directory)
  (let ((git-result (magit-staged-files)))
    (when (not (null git-result))
      (print (concat directory " has staged files")))
    (null magit-staged-files)))

(defun check-for-unstaged (directory)
  (let ((git-result (magit-unstaged-files)))
    (when (not (null git-result))
      (print (concat directory " has unstaged and unpushed files")))
    (null magit-unstaged-files)))

(defun check-for-unpushed (directory)
  (let ((git-result (magit-unpushed-files)))
    (when (not (null git-result))
      (print (concat directory " has unpushed files")))
    (null magit-unpushed-files)))

(defun can-i-go-home? ()
  (interactive)
  (cl-loop for directory in *directories*
           do (progn
                (find-file directory)
                (magit-))))
