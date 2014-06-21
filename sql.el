(defun sql-ask-complete ()
  (save-excursion
    (let ((p (point)))
      (comint-bol nil)
      (list
       (buffer-substring-no-properties (point) (point-max))
       (+ (- p (point)) 1)))))

(defun ab-sql-candidates-test ()
  (process-send-string -ab-connection "(sql-candidates \"SELECT f.n FROM fireman AS f\" 11)"))

(defun ab-resp-ac-complete (args)
  (message (prin1-to-string args)))

(defun ab-sync-complete ()
  (ab-send-string-timeout (prin1-to-string (cons 'sql-candidates (sql-ask-complete)))
			  0.5))

(defun ab-resp-sync-ac-complete (args)
  args)

(defun ac-ab-sql-candidates ()
  (let ((data (ab-sync-complete)))
    (if data
  	data
      '())))

(defvar ac-source-ab-sql 
      '((candidates . ac-ab-sql-candidates)
;;	(prefix . "\\([a-zA-Z_0-9.-]+\\)")
	(symbol . "c")
;;	(requires . 2)
;;	(cache)
))


(defun ac-ab-sql-setup ()
  (interactive)
  (add-to-list 'ac-sources
               'ac-source-ab-sql))

;;SELECT f FROM fireman AS fi, firehouse AS fh 
