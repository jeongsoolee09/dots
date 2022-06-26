(require 'mu4e)
(require 'smtpmail)
(require 'org-mu4e)
(require 'mu4e-alert)

(defun mu4e-unjam ()
  (interactive)
  (progn
    (shell-command "pkill -2 -u $UID mu")
    (shell-command "sleep 1")
    (shell-command "mu index")))

(mu4e t)  ;; mu4e at startup
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 465 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
      smtpmail-debug-info t
      mu4e-update-interval 60
      mu4e-headers-auto-update t)
;; Show mail notification in mode-line
(setq mu4e-alert-interesting-mail-query
      (concat
       "flag:unread"
       " AND NOT flag:trashed"
       " AND maildir:/INBOX"))
(setq mu4e-enable-mode-line t)
(setq mu4e-enable-notifications t)
(with-eval-after-load 'mu4e-alert
  (mu4e-alert-set-default-style 'notifier))
(setq mu4e-hide-index-messages t)

;; Mu4e general settings
(setq mail-user-agent 'mu4e-user-agent ;; Use mu4e as default Emacs mail agent
      mu4e-maildir "~/Mail"

      ;; Use mbsync for mail sync
      mu4e-get-mail-command "mbsync -a"

      ;; Don't save message to Sent Messages, Gmail/IMAP takes care of this
      ;; Override in context switching for other type of mailboxes
      mu4e-sent-messages-behavior 'delete
      message-kill-buffer-on-exit t

      ;; This fixes the error 'mbsync error: UID is x beyond highest assigned UID x'
      mu4e-change-filenames-when-moving t

      ;; Eye candies & attachment handling
      mu4e-view-show-images t
      mu4e-use-fancy-chars t
      mu4e-attachment-dir "~/Downloads"

      ;; Store link to message if in header view, not to header query
      org-mu4e-link-query-in-headers-mode nil

      ;; This helps when using a dark theme (shr)
      shr-color-visible-luminance-min 80

      ;; Citation format
      message-citation-line-format "On %a, %b %d %Y, %N wrote:"
      message-citation-line-function 'message-insert-formatted-citation-line

      ;; Always use 587 for sending emails
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-smtp-service 587

      ;; Use 'helm' to for mailbox selection completion
      mu4e-completing-read-function 'completing-read

      ;; Context switch policy
      mu4e-context-policy 'ask
      mu4e-compose-context-policy nil)

;; Add option to view html message in a browser
;; `aV` in view to activate
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "private"
          :enter-func (lambda () (mu4e-message "Switched to the Private context"))
          ;; leave-func not defined
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg
                                                              :to "volcano0909@gmail.com")))
          :vars '(  ( user-mail-address . "volcano0909@gmail.com"  )
                    ( user-full-name . "이정수" )
                    ( mu4e-compose-signature . "이정수 드림")))

        ,(make-mu4e-context
          :name "work"
          :enter-func (lambda () (mu4e-message "Switched to the Work context"))
          ;; leave-fun not defined
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg
                                                              :to "jeongsoolee@korea.ac.kr")))
          :vars '(  ( user-mail-address . "jeongsoolee@korea.ac.kr" )
                    ( user-full-name . "이정수" )
                    ( mu4e-compose-signature . "이정수 드림")))))

;; Use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; Emulate shr key bindings
(add-hook 'mu4e-view-mode-hook
          (lambda ()
            ;; try to emulate some of the eww key-bindings
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))

(provide 'mu4e-my-config)
