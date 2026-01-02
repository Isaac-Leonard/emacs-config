;;; -*- lexical-binding: t; -*-
(setq visible-bell nil) ;; use sound instead of screen flash
(defun play-urgent-notification ()
  "Plays an urgent notification for news"
  (start-process "bell" nil "play" "/Users/isaac/emacspeak/sounds/chimes/alarm.ogg"))

(use-package elfeed-score
  :ensure t
  :config
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)))

(setq elfeed-tags (list "technology" "climate" "geopolitics" "economics" "science" "health" "civilrights" "sports" "entertainment" "crime" "other"))

(defun isaac/elfeed-remove-empty-tags ()
  "Remove tags formed from running intern on an empty string from all entries"
  (maphash (lambda (id entry) 
	     (when (member (intern "") (elfeed-entry-tags entry))
	       (setf (elfeed-entry-tags entry)
		     (remove (intern "") (elfeed-entry-tags entry)))))
	   elfeed-db-entries))

(defun isaac/elfeed-keep-only-matching-tags (text)
  "Return a string containing only TAGS found in TEXT, in order."
  (let* ((tag-regex (format "\\(%s\\)" (string-join elfeed-tags "\\|")))
         (matches '())
         (start 0))
    (while (string-match tag-regex text start)
      (push (match-string 0 text) matches)
      (setq start (match-end 0)))
    (cl-map 'list 'intern matches)))

(defun isaac/elfeed-classify-entry(entry)
  "Classify Elfeed ENTRY with gptel + llama.cpp and tag it."
  (let* ((title (elfeed-entry-title entry))
	 (text (elfeed-entry-content entry))
         (prompt (format "Classify the following news article into the following categories: %s. Return only the relevant categories separated by a comma. Then on a new line give a score between 1 and 10 for how urgent the article is to read with a ten being immediately and directly relevant and a 1 meaning I can ignore it.\n\n%s\n%s. Then on another line list the names of any people, companies or organisations the article mentions seperated by commas." (string-join elfeed-tags ", ") title text)))
    (if (not (seq-intersection elfeed-tags (elfeed-entry-tags entry)))
	(gptel-request
	    prompt
	  :callback
	  (lambda (response info)
	    (let* ((cleaned (string-trim (downcase response)))
		   (lines (string-split cleaned "\n"))
		   (categories-line (nth 0 lines))
		   (score (string-to-number (nth 1 lines)))
		   (categories (isaac/elfeed-keep-only-matching-tags categories-line)))
	      (setf (elfeed-entry-tags entry)
		    (cl-union (elfeed-entry-tags entry) categories :test #'equal))
	      (if (eq score 10) (prog (play-urgent-notification)
				      (dtk-speak (format "Breaking news %s" title))
				      (elfeed-score-scoring-set-score-on-entry entry score t)))
	      (elfeed-search-update-entry entry)))))))

;; (defun elfeed-search-selected (&optional ignore-region-p)
;;   "Return a list of the currently selected feeds.

;; If IGNORE-REGION-P is non-nil, only return the entry under point."
;;   (let ((use-region (and (not ignore-region-p) (use-region-p))))
;;     (let* ((start (if use-region (region-beginning) (point)))
;;            (end   (if use-region (region-end)       (point)))
;;            (start-line (line-number-at-pos start))
;;            (end-line (line-number-at-pos end))
;;            (range (if ignore-region-p
;;                       (list (line-number-at-pos (point)))
;;                     (number-sequence start-line end-line))))
;;       (let (selected)
;;         (dolist (line range)
;;           (let ((entry (cdr (assoc line entry-positions))))
;;             (when entry
;;               (push entry selected))))
;;         (if ignore-region-p
;;             (car selected)
;;           (nreverse selected))))))

;; (defun elfeed-search-update (&optional force)
;;   "Update the elfeed-search buffer listing to match the database.
;; When FORCE is non-nil, redraw even when the database hasn't changed."
;;   (interactive)
;;   (with-current-buffer (elfeed-search-buffer)
;;     (when (or force (and (not elfeed-search-filter-active)
;;                          (< elfeed-search-last-update (elfeed-db-last-update))))
;;       (elfeed-save-excursion
;;         (let ((inhibit-read-only t)
;;               (standard-output (current-buffer))
	      
;;               (grouped (make-hash-table :test #'equal)))
;;           (erase-buffer)
;;           (elfeed-search--update-list)
;; 	  ;;; Ensure all entries are classified

;; 	  ;;; Group by category
;; 	  (dolist (entry elfeed-search-entries)
;;             (isaac/elfeed-classify-entry entry tags)
;; 	    (let* ((tag (or (elfeed-tagged-p entry :category) "Uncategorized"))
;; 		   (existing (gethash tag grouped)))
;;               (puthash tag (cons entry existing) grouped)))

;; 	  (setq entry-positions (list))
;;           (maphash
;;            (lambda (tag entries)
;;              (insert (propertize (format "\n%s:\n" tag)
;; 				 'face '(:weight bold :height 1.2)))
;;              (dolist (entry (sort entries (lambda (a b)
;;                                             (> (elfeed-entry-date a)
;;                                                (elfeed-entry-date b)))))
;;                (funcall elfeed-search-print-entry-function entry)
;; 	       (let ((line (line-number-at-pos)))
;; 		 (push (cons line entry) entry-positions))
;; 	       (insert "\n")))
;;            grouped)
;;           (setf elfeed-search-last-update (float-time))))
;;       (when (zerop (buffer-size))
;;         ;;; If nothing changed, force a header line update
;;         (force-mode-line-update))
;;       (run-hooks 'elfeed-search-update-hook))))


(add-hook 'elfeed-new-entry-hook 'isaac/elfeed-classify-entry)
(setq-default elfeed-search-filter "@1-week-ago +unread -sports -entertainment ")
(run-at-time nil (* 15 60) #'elfeed-update)
