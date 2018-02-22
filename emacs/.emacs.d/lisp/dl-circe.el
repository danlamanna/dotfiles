(use-package circe
  :ensure t
  :config (progn
            (require 'dl-credentials)
            (setq circe-reduce-lurker-spam t)
            (enable-circe-color-nicks)))

(use-package circe-notifications
  :ensure t
  :commands enable-circe-notifications
  :after circe
  :init (add-hook 'circe-server-connected-hook #'enable-circe-notifications)
  :config (progn
            ;; needed for x-urgency-hint
            (require 'dl-utils)

            ;; condition which notifications should definitely happen
            ;; todo - this needs to be generalized to work based on server buffers
            ;; so nicks don't have to be hardcoded
            (defun dl-circe-notifications-should-notify (nick userhost channel body)
              (if (and (s-equals? channel "danlamanna")
                       (not (s-equals? nick "danlamanna")) ;; messages sent from me show up sometimes
                       (not (s-equals? channel "&bitlbee"))
                       (not circe-notifications-emacs-focused))
                  t))
            (advice-add 'circe-notifications-should-notify :before-until 'dl-circe-notifications-should-notify)

            ;; condition which notifications shouldn't happen
            (defun dl-circe-should-notify (nick userhost channel body)
              (not (s-equals? nick "gitter")))

            (advice-add 'circe-notifications-should-notify :before-while 'dl-circe-should-notify)

            (defun dl-circe-notify (nick body channel)
              "Handle circe notifications.
               If the system is lantea, or there are no visible frames to set an urgency hint, use
               libnotify, otherwise display an urgency hint on the relevant frame."
              (if (s-equals? (system-name) "lantea")
                  ;; setting it to nil lets us use the default notification
                  ;; without infinitely recursing
                  (let ((circe-notifications-notify-function nil))
                    (circe-notifications-notify nick body channel))
                (if (= 0 (length (frame-list)))
                    (let ((circe-notifications-notify-function nil)) ;; see above comment
                      (circe-notifications-notify nick "new message" channel))
                  (x-urgency-hint (or (dl-circe-get-chat-frame-from-buffer
                                       (dl-circe-get-relevant-chat-buffer (current-buffer) nick channel))
                                      (dl-circe-guess-chat-frame)) t))))


            (defun dl-circe-get-relevant-chat-buffer (server-buffer nick channel)
              "Return the chat buffer (if it exists) relevant to the notify parameters.
               If the circe-nick being used (in the SERVER-BUFFER) is the same as the channel,
               it's a one on one chat and the nick should be used to lookup the right buffer.
               If it's a group chat, the channel name can be used.
               This can return nil if such a buffer doesn't exist yet, which may be the case for the
               first message from a person."
              (with-current-buffer server-buffer
                (if (s-equals? circe-nick channel)
                    (gethash nick circe-server-chat-buffer-table)
                  (gethash channel circe-server-chat-buffer-table))))


            (defun dl-circe-get-chat-frame-from-buffer (buffer)
              "Given a circe BUFFER corresponding to a chat, return the associated frame."
              (if-let* ((window (get-buffer-window buffer 'visible)))
                  (window-frame window)))


            (defun dl-circe-is-circe-window (window)
              "Determine if a WINDOW is a circe window."
              (with-current-buffer (window-buffer window)
                (derived-mode-p 'circe-mode)))


            (defun dl-circe-guess-chat-frame ()
              "Guess which frame is most likely used for chat.
               Look through all visible frames and pick the one that is most likely the
               frame implicitly designated for chat.  This amounts to just finding the
               visible frame with the most circe-mode windows in it.  This can return
               'nil' if there are no visible frames."
              (-first-item (-sort (lambda (frame-a frame-b)
                                    (> (length (-filter 'dl-circe-is-circe-window (window-list frame-a)))
                                       (length (-filter 'dl-circe-is-circe-window (window-list frame-b)))))
                                  (frame-list))))

            (setq circe-notifications-notify-function 'dl-circe-notify)

            (setq circe-notifications-watch-strings '())))

(provide 'dl-circe)
