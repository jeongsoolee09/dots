(require 'cl)
(require 's)
(require 'dash)

(defvar *last-sent-lines* nil)

(defun is-comment-line (line)
  (or (s-starts-with? "//" (s-trim-left line))
      (s-starts-with? "/*" (s-trim-left line))))


(defun is-blank-line (line)
  (s-blank-str? (s-trim-left line)))


(defun is-ignored-by-repl (line)
  ;; comments and empty lines are ignored
  (or (is-comment-line line)
      (is-blank-line line)))


(defun filter-lines (lines)
  (remove-if #'is-comment-line lines))


(defun concat-string-list-with-newline (string-list)
  (concat (reduce (lambda (acc elem)
                    (concat acc elem "\n")) (butlast string-list) :initial-value "")
          (car (last string-list))))


;; Credit: http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun read-lines (file-path)
  "Return a list of lines of a file at file-path."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))


(defun find-import-statements (string-list)
  (cl-loop for string in string-list
           if (s-contains? "import " string) collect string))


(defun send-import-statement-scala (string)
  (let ((sbt-buffer-name (car (find-sbt-buffer))))
    (comint-send-string sbt-buffer-name string)))


(defun find-sbt-buffer ()
  (cl-loop for buffer in (mapcar #'buffer-name (buffer-list))
           if (s-contains? "*sbt*" buffer) collect buffer))


(defun sbt-startup-console ()
  (interactive)
  (sbt-command "console"))


(defun sbt-console-send-import ()
  (interactive)
  (let ((import-stats (->> (buffer-file-name)
                           (read-lines)
                           (find-import-statements)))
        (sbt-buffer-name (car (find-sbt-buffer))))
    (dolist (string import-stats)
      (send-import-statement-scala (concat string "\n")))
    (comint-send-string sbt-buffer-name "\n")))


(defun sbt-console-send-defun ()
  "Send the definition to the sbt console buffer."
  (interactive)
  (save-mark-and-excursion
    (let (start end)
      (scala-syntax:beginning-of-definition)
      (setq start (point))
      (scala-syntax:end-of-definition)
      (setq end (point))
      (let ((code (buffer-substring-no-properties start end))
            (sbt-buffer-name (car (find-sbt-buffer))))
        (comint-send-string sbt-buffer-name code)
        (comint-send-string sbt-buffer-name "\n")))))


(defun sbt-console-send-region (start end)
  "Send the region to the sbt console buffer.
Argument START the start region.
Argument END the end region."
  (interactive "r")
  (let ((code (buffer-substring-no-properties start end))
        (sbt-buffer-name (car (find-sbt-buffer))))
    (setq *last-sent-lines* (s-split "\n" code))
    (comint-send-string sbt-buffer-name code)
    (comint-send-string sbt-buffer-name "\n")))


(defun sbt-console-send-buffer ()
  "Send the buffer to the sbt console buffer."
  (interactive)
  (save-mark-and-excursion
    (goto-char (point-min))
    (re-search-forward "^package .+\n+" nil t)
    (sbt-console-send-region (point) (point-max))))


(defun sbt-console--send-string (string)
  "Send the code to the sbt console.
   Argument STRING the code to send."
  (let ((sbt-buffer-name (car (find-sbt-buffer))))
    (comint-send-string sbt-buffer-name string)
    (comint-send-string sbt-buffer-name "\n")))


(defun remove-all-whitespace (raw-code-string)
  (cl-reduce (lambda (acc char)
               (let ((string (char-to-string char)))
                 (if (equal " " (char-to-string char))
                     acc
                   (concat acc (char-to-string char))))) raw-code-string :initial-value ""))


(defun insert-space-at (index string)
  "insert space at the given index of a given string."
  (let ((prefix (substring string 0 index))
        (suffix (substring string index)))
    (concat prefix " " suffix)))


(defun find-object-or-class (string-list)
  "class Hey[Jude](sad: Int, song: Double) { ... }
   object Yayay { ... }
   |-> (\"Hey[Jude]\" \"Yayay\")"
  (let ((classes-or-objects (cl-loop for string in string-list
                                     if (let ((normalized (remove-all-whitespace string)))
                                          (and (or (s-starts-with? "object" normalized)
                                                   (s-starts-with? "class" normalized))
                                               (s-ends-with? "{" string)
                                               (not (s-contains? "case" string))))
                                     collect (let* ((normalized (remove-all-whitespace string))
                                                    (splitted-on-paren (s-split "(" normalized))
                                                    (classname-with-class (car splitted-on-paren))
                                                    (without-extends (s-split "extends" classname-with-class)))
                                               (->> (car without-extends)
                                                    (s-chop-suffix "{"))))))
    (mapcar (lambda (str) (cond ((s-contains? "object" str) (insert-space-at 6 str))
                                ((s-contains? "class" str) (insert-space-at 5 str))
                                (:else (error (concat "unexpected value: " str)))))
            classes-or-objects)))


(defun return-lines-btw-braces (search-phrase)
  "If given class Hey[Jude](sad: Int, song: Double) { ... },
   throw the statements inside the braces into the REPL."
  (when (evil-normal-state-p)
    (evil-insert 0))
  (save-excursion
    (goto-char (point-min))
    (let ((success? (ignore-errors (search-forward search-phrase))))
      (when success?
        (let ((success2? (ignore-errors (search-forward "{"))))
          (when success2?
            (let* ((current-point (point))
                   (other-point (save-excursion
                                  (backward-char)
                                  (forward-sexp)
                                  (backward-char)
                                  (point)))
                   (region-string (buffer-substring-no-properties current-point other-point))
                   (code-list (s-split "\n" region-string)))
              (evil-normal-state)
              code-list)))))))


(defun alist-get-key-with-max-val (alist)
  (let* ((keys (mapcar #'car alist))
         (vals (mapcar #'cdr alist))
         (rev-alist (-zip vals keys))
         (max-val (apply #'max vals)))
    (alist-get max-val rev-alist)))


(defun closest-backward-matching (current-point string-list)
  (let* ((points (save-excursion
                   (mapcar #'(lambda (string)
                               (goto-char (point-max))
                               (search-backward string))
                           string-list)))
         (string-and-points (-zip string-list points))
         (behind-only (seq-filter (lambda (cell) (< (cdr cell) current-point))
                                  string-and-points)))
    (print behind-only)
    (alist-get-key-with-max-val behind-only)))


(defun sbt-console-send-current-class ()
  (interactive)
  (save-excursion
    (let* ((object-or-class-strings (->> buffer-file-name
                                         (read-lines)
                                         (find-object-or-class)))
           (closest-match (closest-backward-matching (point) object-or-class-strings))
           (code-lines (return-lines-btw-braces closest-match)))
      (setq *last-sent-lines* code-lines)
      (message "Sending current class...")
      (sbt-console--send-string
       (concat-string-list-with-newline code-lines)))))


(defun sbt-console-send-all-classes ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((object-or-class-strings (->> buffer-file-name
                                        (read-lines)
                                        (find-object-or-class)))
          (acc))
      (dolist (object-or-class-string object-or-class-strings)
        (push (return-lines-btw-braces object-or-class-string) acc))
      (let ((code-lines (flatten-list acc)))
        (setq *last-sent-lines* code-lines)
        (message "Sending all classes in this file...")
        (sbt-console--send-string
         (concat-string-list-with-newline code-lines))))))


(defun sbt-console-convert-console-line ()
  (interactive)
  (let* ((console-linum (string-to-number
                         (read-from-minibuffer "Console line: ")))
         (found-line (nth console-linum (filter-lines *last-sent-lines*))))
    (message "Jumped to indicated line in the source code.")
    ;; TEMP for debugging
    ;; (print (length *last-sent-lines*))
    ;; (print "found-line:")
    ;; (print found-line)
    (goto-char (point-min))
    (search-forward found-line)))


(provide 'sbt-console)
