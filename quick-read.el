;; quick search
;; Copyright (c) 2014, antoineB, All rights reserved.
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3.0 of the License, or (at your option) any later version.
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Lesser General Public License for more details.
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library.

;; Extract all symbols from a buffer/region and uniq them and display
;; them in order of apperance.

(defvar qr-symbol-regexp "[a-zA-Z_0-9#%*+$^/-]+"
  "The regular expression used to filter the symbol to match")

(defvar qr-predicate-symbol 'identity
  "A predicate function to filter out matched symbols")

(defun qr--default-predicate (str)
  (or (not (get-text-property 0 'fontified str))
      (not (memq (get-text-property 0 'face str)
		 '(font-lock-doc-face font-lock-string-face
		 font-lock-comment-face
		 font-lock-keyword-face)))))

(defun qr--search-region (min max)
  (save-excursion
    (goto-char min)
    (let ((syms (make-hash-table :test 'equal))
	  (lst '()))
      (while (re-search-forward qr-symbol-regexp max 'noerror)
	(let ((str (match-string-no-properties 0)))
	  (when (and (not (gethash str syms))
		     (if qr-predicate-symbol
			 (funcall qr-predicate-symbol (match-string 0))
		       't))
	    (puthash str 't syms)
	    (setq lst (cons str lst)))))
      (reverse lst))))

(defun qr-search ()
  (interactive)
  (let ((lst (if (region-active-p)
		 (qr--search-region (region-beginning) (region-end))
	       (qr--search-region (point-min) (point-max)))))
    (switch-to-buffer (get-buffer-create "*qr search*"))
    (dolist (sym lst)
      (insert sym "\n"))))
