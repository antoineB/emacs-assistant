(require 'cl)

(defvar -ab-connection nil)

(defvar -ab-buffer nil)

(defvar -ab-server-port 1026)
(defvar -ab-server-ip "localhost")

(defun -ab-message-to-buffer (msg)
  (with-current-buffer -ab-buffer
    (goto-char (point-max))
    (insert (if (stringp msg) msg (prin1-to-string msg)))))

(defun -ab-filter-fn (proc str)
  ;; For debug purpose.
  (-ab-message-to-buffer str)
  (let* ((data (read str))
         (sym (intern(concat "ab-resp-" (symbol-name (car data)))))
         (data (cdr data)))
    ;;test (fboundp sym)
    (funcall sym data)))

(defun ab-start ()
  (interactive)
  (setq -ab-connection (open-network-stream "*AB server*" nil -ab-server-ip -ab-server-port))
  (set-process-coding-system -ab-connection 'utf-8-unix 'utf-8-unix)
  (setq -ab-buffer (generate-new-buffer "*AB server*"))
  (set-process-buffer -ab-connection (get-buffer "*AB server*"))
  (set-process-filter -ab-connection '-ab-filter-fn))

(defun ab-exit ()
  (process-send-string -ab-connection "(exit)")
  (kill-buffer "*AB server*")
  (kill-process -ab-connection))

(defun ab-bonjour-test ()
  (process-send-string -ab-connection "(bonjour)"))

(defun ab-resp-bonjour (args)
  (message "bonjour from ab"))

(defvar -ab-token (make-hash-table))

(defvar -ab-counter 0)

(defun next-counter ()
  (setq -ab-counter (+ -ab-counter 1))
  -ab-counter)

(defun ab-resp-token (args)
  (let ((nb (car args)))
    (when (gethash nb -ab-token nil)
      (puthash nb (cdr args) -ab-token))))

(defun ab-send-string-timeout (str timeout)
  (let ((counter (next-counter)))
    (when (with-timeout (timeout nil)
            (process-send-string -ab-connection (concat "(token " (prin1-to-string counter) " " str ")"))
            (puthash counter 'wait -ab-token)
            (while (equalp 'wait (gethash counter -ab-token))
              (sleep-for 0.01))
            'done)
      (let ((data (gethash counter -ab-token)))
        (when data
          (let ((sym (intern(concat "ab-resp-sync-" (symbol-name (car data)))))
                (data (cdr data)))
            (funcall sym data)))))))

(defun ab-resp-sync-bonjour (args)
  (message "coucou"))

(defun ab-bonjour-sync ()
  (ab-send-string-timeout "(bonjour)" 10.0))
