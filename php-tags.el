(defun helm-etags-line (line filename)
  (when (string-match "^\\([^]+\\)\\(\\([^]+\\)\\)?\\([0-9]+\\)" line)
    (let ((number (match-string 4 line))
	  (text (concat filename ":" (match-string 3 line) "\n")))
      (if number
	    (propertize text 'position (string-to-int number))
	text))))
	
(defun insert-into-buffer (filename)
  (let ((lines
	 (with-current-buffer (find-file-noselect filename)
	   (prog1
	       (split-string (buffer-string) "\n" 'omit-nulls)
	     (kill-buffer)))))
    (cl-loop
     with fname
     for i in lines
     do (if (string-match "^\\([^,]+\\),[0-9]+$" i)
	    (setq fname (match-string 1 i))
	  (let ((line (helm-etags-line i fname)))
	    (when line
	      (insert line)))))))

(defvar php-tags-file "/home/antoine/emacs-assistant/TEST")

(defun helm-php-etags-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (insert-into-buffer php-tags-file)))

(defun php-etags-match-part (candidate)
  (string-match "[^:]+:\\(.+\\)" candidate)
  (match-string 1 candidate))


(defun php-etags-action (-candidate)
  (let ((filename (car (split-string -candidate ":")))
	(candidate (helm-get-selection nil 'withprop)))
    (find-file filename)
    (if (get-text-property 0 'position candidate)
	(goto-line (get-text-property 0 'position candidate))
      1)))

(defvar helm-source-php-etags
  '((name . "Search throught tags files")
    (init . helm-php-etags-init)
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (match-part . php-etags-match-part)
    (action . php-etags-action)))

(defun helm-php-tags ()
  (interactive)
  (helm :sources 'helm-source-php-etags :buffer "*helm php tags*"))
