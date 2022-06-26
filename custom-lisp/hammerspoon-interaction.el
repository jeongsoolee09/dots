(require 's)
(require 'cl-lib)

(defvar hs-buffer-name "*hs*")


(defun hammerspoon-fennel-repl-run ()
  (interactive)
  (comint-run "hs" '())
  (comint-send-string hs-buffer-name "fennel = require(\"fennel\")\n"))


(defun hammerspoon-fennel-repl-wrap-sexp (code)
  (cl-flet ((handle-function-declaration (code)
                                         (cl-assert (fennel-declaration-p code))
                                         (let ((identifier
                                                (cond ((fennel-fn-decl-p code) (fennel-extract-fn-symbol code))
                                                      ((fennel-lambda-decl-p code) (fennel-extract-lambda-symbol code))
                                                      (:else (error "wtf")))))
                                           (format "%s = fennel.eval(\"%s\")" identifier code)))
            (handle-variable-declaration (code)
                                         (cl-assert (fennel-declaration-p code))
                                         (let ((identifier
                                                (cond ((fennel-local-decl-p code) (fennel-extract-local-symbol code))
                                                      ((fennel-var-decl-p code) (fennel-extract-var-symbol code))
                                                      (:else (error "wtf")))))
                                           (format "%s = fennel.eval(\"%s\")" identifier code))))
    (let ((string-escaped (->> code
                               (prin1-to-string)
                               (s-trim)
                               (s-replace "\n" "\\n"))))
      (if (fennel-declaration-p code)
          (handle-declaration code)
          (concat "fennel.eval(" string-escaped ")\n")))))


(hammerspoon-fennel-repl-wrap-sexp "(+ 1 2 3)")
(hammerspoon-fennel-repl-wrap-sexp "(fn hihi [] (print \"hihi!!!\"))")
(hammerspoon-fennel-repl-wrap-sexp "(lambda hihi [] (print \"hihi!!!\"))")
(hammerspoon-fennel-repl-wrap-sexp "(var hihi \"hihi!!!\")")
(hammerspoon-fennel-repl-wrap-sexp "(local hihi \"hihi!!!\")")


(defun hammerspoon-fennel-repl-send-string (string wrap?)
  (if wrap?
      (->> string
           (hammerspoon-fennel-repl-wrap-sexp)
           (comint-send-string hs-buffer-name))
      (comint-send-string hs-buffer-name string)))


(defun hammerspoon-fennel-repl-eval-region (start end)
  (interactive "r")
  (let ((content (buffer-substring-no-properties start end)))
    (hammerspoon-fennel-repl-send-string content t)
    (hammerspoon-fennel-repl-send-string "\n" nil)))


(defun hammerspoon-fennel-repl-eval-last-sexp ()
  (interactive)
  (hammerspoon-fennel-repl-eval-region
   (save-excursion (backward-sexp) (point))
   (point)))


(defun hammerspoon-fennel-repl-eval-form-and-next ()
  (interactive "")
  (while (not (zerop (car (syntax-ppss))))
    (up-list))
  (hammerspoon-fennel-repl-eval-last-sexp)
  (forward-sexp))


(defun hammerspoon-fennel-repl-eval-buffer ()
  (interactive)
  (let ((buffer-content (buffer-substring-no-properties (point-min) (point-max))))
    (hammerspoon-fennel-repl-send-string buffer-content t)))


(provide 'hammerspoon-interaction)


;; if it's a declaration: then do some extra steps.

;; - variable
;; - "(var"
;; - "(local"

;; - function
;; - "(fn"
;; - "(lambda"

;; 아 이거 생각 외로 꽤 대공사가 될 거 같은데


(defvar code "(fn hihi [] (print \"hihi!!!\"))")


(defvar fn-pattern "( *fn +\\([a-zA-Z0-9?!_-]+\\).*")

(defvar lambda-pattern "( *lambda +\\([a-zA-Z0-9?!_-]+\\).*")

(defvar local-pattern "( *local +\\([a-zA-Z0-9?!_-]+\\).*")

(defvar var-pattern "( *var +\\([a-zA-Z0-9?!_-]+\\).*")


(defun pattern-exact-match (pattern target)
  (string= (car (s-match pattern target))
           target))


;; some very-not-enough unit tests
(pattern-exact-match "[a-z]+" code)
(pattern-exact-match lambda-pattern code)
(pattern-exact-match fn-pattern code)


(defun fennel-fn-decl-p (code)
  (pattern-exact-match fn-pattern code))


(defun fennel-extract-fn-symbol (code)
  (cl-assert (pattern-exact-match fn-pattern code))
  (car (last (s-match fn-pattern code))))


(defun fennel-lambda-decl-p (code)
  (pattern-exact-match lambda-pattern code))


(defun fennel-extract-lambda-symbol (code)
  (cl-assert (pattern-exact-match lambda-pattern code))
  (car (last (s-match lambda-pattern code))))


(defun fennel-local-decl-p (code)
  (pattern-exact-match local-pattern code))


(defun fennel-extract-local-symbol (code)
  (cl-assert (pattern-exact-match local-pattern code))
  (car (last (s-match local-pattern code))))


(defun fennel-var-decl-p (code)
  (pattern-exact-match var-pattern code))


(defun fennel-extract-var-symbol (code)
  (cl-assert (pattern-exact-match var-pattern code))
  (car (last (s-match var-pattern code))))


(defun fennel-declaration-p (code)
  (or (fennel-fn-decl-p code)
      (fennel-lambda-decl-p code)
      (fennel-local-decl-p code)
      (fennel-var-decl-p code)))


(defun handle-declaration (code)
  (cl-assert (fennel-declaration-p code))
  (let ((identifier
         (cond ((fennel-fn-decl-p code) (fennel-extract-fn-symbol code))
               ((fennel-lambda-decl-p code) (fennel-extract-lambda-symbol code))
               ((fennel-local-decl-p code) (fennel-extract-local-symbol code))
               ((fennel-var-decl-p code) (fennel-extract-var-symbol code))
               (:else (error "wtf")))))
    (format "%s = fennel.eval(%s)" identifier code)))
