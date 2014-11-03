(defvar ab-sql-completion-delay 0.1)
(defvar ab-sql-terminate-delay 0.5)

(defun sql-ask-complete ()
  (save-excursion
    (let ((p (point)))
      (comint-bol nil)
      (list
       (buffer-substring-no-properties (point) (point-max))
       (+ (- p (point)) 1)))))

;; (defun ab-sql-candidates-test ()
;;   (process-send-string -ab-connection "(sql-completion \"SELECT f.n FROM fireman AS f\" 11)"))

;; (defun ab-resp-sql-completion (args)
;;   (message (prin1-to-string args)))

(defun ab-sql-completion-sync ()
  (ab-send-string-timeout (prin1-to-string (cons 'sql-completion (sql-ask-complete)))
                          ab-sql-completion-delay))

(defun ab-resp-sync-sql-completion (args)
  args)

(defun ac-ab-sql-candidates ()
  (let ((data (ab-sql-completion-sync)))
    (if data
        data
      '())))

(defvar ac-source-ab-sql
      '((candidates . ac-ab-sql-candidates)
;;	(prefix . "\\([a-zA-Z_0-9.-]+\\)")
        (symb1ol . "c")
;;	(requires . 2)
;;	(cache)
))


(defun ac-ab-sql-setup ()
  (interactive)
  (add-to-list 'ac-sources
               'ac-source-ab-sql))

;;SELECT fh.ad FROM fireman AS fi, firehouse AS fh


(defun ab-sql-terminate-sync ()
  (ab-send-string-timeout (prin1-to-string (list 'sql-terminate (car (sql-ask-complete))))
			  ab-sql-terminate-delay))

(defun ab-resp-sync-sql-terminate (args)
  (if (= 0 (length args))
      (minibuffer-message "no fix available")
    (if (= (length args) 1)
        (progn
          (comint-bol nil)
          (comint-kill-region (point) (point-max))
          (insert (car args)))
      (with-current-buffer "*ab-candidates*"
        (kill-region (point-min) (point-max))
        (dolist (line args)
          (insert line))))))
