(require 's)

(setq poll-interval 2)
(setq old-chat-list nil)
(setq twitch-chat-timer nil)

(defun show-twitch-chat ()
  (interactive)
  (let ((twitch-id (read-from-minibuffer "Twitch ID: ")))
    (comint-run "tc" `("connect" ,twitch-id))))


(defun get-fresh-chats (buffer)
  (with-current-buffer buffer
    (seq-filter (lambda (str) (not (s-blank? str)))
                (s-split "\n"
                         (buffer-substring-no-properties (point-min) (point-max))))))


(defun diff-list (new old)
  (-take-while (lambda (new-chat)
                 (not (-contains? old new-chat)))
               new))

(diff-list '(1 2 3 4) '(3 4))

(defun diff-chat ()
  (let* ((new-chat-list (nreverse (get-fresh-chats "*tc*")))
         (new-chats (diff-list new-chat-list old-chat-list)))
    (progn
      (setq old-chat-list new-chat-list)
      (car new-chats))))


(defun display-chat (chat)
  (message "%s" chat))


(defun main ()
  (if (buffer-live-p (get-buffer "*tc*"))
      (let ((new-chat (diff-chat)))
        (when new-chat
          (display-chat new-chat)))
      (cancel-timer twitch-chat-timer)))


(defun show-twitch-chat-in-minibuffer ()
  (interactive)
  (setq twitch-chat-timer (run-with-timer 0 poll-interval #'main)))
