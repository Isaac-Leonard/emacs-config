;;; -*- lexical-binding: t; -*-
;; Makes some of the lambda code work later on, hopefully it doesn't break any old packages
;; Set up emacspeak before everything else so that it runs even if there's an error later on
(setq dtk-program "mac")
(load-file "~/emacspeak/lisp/emacspeak-setup.el")
(setq mac-default-voice-string "[{voice Karen}]")
;; Customising emacspeak
(global-set-key (kbd "M-c") 'emacspeak-speak-current-column)
(dtk-set-rate 720 t)
(setq emacspeak-auditory-icon-function 'emacspeak-queue-auditory-icon)
;; Set correct environment variables
;;(require 'exec-path-from-shell)

;; Make the startup cleaner
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t)

;;Update buffers when fixxing with linters and version control
(global-auto-revert-mode)

;; Don't notify about autosaving
(setq-default auto-save-no-message t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(emacspeak-play-program "afplay")
)

;; Sometimes emacs uses the wrong path on Mac
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;; Needed for mplayer
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Straight package setup, means I can use this config anywhere and packages will be installed automatically on start
;; Use "use-package" macro for setting up packages
(setq straight-use-package-by-default t)
;; Downloads and runs straight.el if its not installed
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Download and use "use-package"
(straight-use-package 'use-package)

;; Config and cache files get all tucked away
(use-package no-littering)
;; Hide those nasty backup files
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Set up language server protocol
(use-package lsp-mode
  :commands lsp
  :hook (python-mode . lsp)
  (java-mode . lsp)
  :custom
  (lsp-server-install-dir "~/language-servers")
  (lsp-enable-snippet nil)
  (lsp-keymap-prefix "s-l")
  )

;; LSP support for tex
;; Disabled cause its slow and causes crashes
;;(add-to-list  'load-path, "/usr/local/bin/texlab")
(use-package lsp-latex)
(setq-default TeX-master nil)
(setq TeX-parse-self t)
(setq TeX-auto-save t)
(setq Tex-electric-math (cons "$" "$"))
(use-package tex
  :straight (auctex :type git :host github :repo "emacs-straight/auctex" :files ("*" (:exclude ".git")))
  )
;; For cross references and interfile navigation for tex
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode

;; Does something to make java work for lsp, better then eclim
(use-package lsp-java)

;; Easy navigation and dumb inserting for general coding
(defun setup-smartparens ()
  "Adds my own key maps for smart parens"
  (smartparens-mode)
  (local-set-key (kbd "C-9") 'sp-beginning-of-sexp)
  (local-set-key (kbd "C-0") 'sp-end-of-sexp)
  )

(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook   (after-change-major-mode . setup-smartparens)
  )

;; Auto complete for almost every buffer
(use-package company
  ;; Start offering suggestions as soon as we start typing
  :custom (company-minimum-prefix-length 1)
  (global-company-mode t)
  ;; This is just annoying to have enabled
  ;; Disables function parameters being inserted automatically when selecting completion for a function when writing c and c++
  (company-clang-insert-arguments nil)
  (company-selection-wraparound t)
  )

;; Inline documentation where possible
(use-package eldoc
  :hook (lsp-mode . eldoc-mode)
  )

;; Error checking, never actually noticed what it does though
(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :config (advice-add 'flycheck-verify-setup :after (lambda ()(switch-to-buffer "*Flycheck checkers*"))))

;;This works somehow
;; Sets up emacs for typescript using tide-mode
(defun setup-tide-mode ()
  "Sets up tide for typescript"
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint))
  ;; Make navigating errors easier, wrapped in lambda so the error gets read out then read the line out afterwards for context
  ;; Cancel reading the current line if I perform an action
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ( tsserver-node-modules))

;; Function to use your node_modules's TSServer to avoid possible collisions with project's Typescript version and Global Typescript version
(defun tsserver-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (tsserver
          (and root
               (expand-file-name "node_modules/.bin/tsserver"
                                 root))))
    (when (and tsserver (file-executable-p tsserver))
      (setq-default tide-tsserver-executable tsserver))))

(use-package tide
  :after
  (typescript-mode company flycheck)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'tide-mode)
  :config (advice-add 'tide-references :after (lambda ()(switch-to-buffer "*tide-references*")))  )

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Format js and ts code
(use-package prettier-js
  :hook 
  (tide-mode . prettier-mode)
  (web-mode . prettier-mode)
  )

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
	 (lambda ()
	   (when (string-equal "tsx" (file-name-extension buffer-file-name))
	     (setup-tide-mode))))

;; Function to use your node_modules's TSServer to avoid possible collisions with project's Typescript version and Global Typescript version
(defun tsserver-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (tsserver
          (and root
               (expand-file-name "node_modules/.bin/tsserver"
                                 root))))
    (when (and tsserver (file-executable-p tsserver))
      (setq-default tide-tsserver-executable tsserver))))
(use-package elisp-slime-nav
  :hook (elisp-mode . elisp-slime-nav-mode)
  )

;; RSS feeds
(use-package elfeed
  :custom (elfeed-search-title-max-width 280)
  (elfeed-search-title-min-width 280)
  :config (global-set-key (kbd "C-x w") 'elfeed))
(use-package elfeed-org
  :config (elfeed-org)
  :custom (rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

(defun elfeed-search-print-entry--custom (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                    tags ","))
         (title-width (- (window-width) 10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width)
                        :left)))
    (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
    (when feed-title
      (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when tags
      (insert "(" tags-str ")"))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")))
(setq elfeed-search-print-entry-function 'elfeed-search-print-entry--custom)

;; Org- setup
(use-package org)
;; Include holidays and stuff in org-agender
(setq org-agenda-include-diary t)
;; Capture notes
(setq org-directory "~/org/")

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "" "Tasks")
         "* TODO %?\n  %i\n  %a")
	("n" "Note" entry (file+headline "" "Notes")
         "* %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/diary.org")
         "* %?\nEntered on %U\n  %i\n  %a" :kill-buffer)
	("P" "process-soon" entry (file+headline "~/org/notes.org" "Emails")
	 "* TODO %:fromname: %a \nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")))

;; ORG-EXPORT TIMESTAMPS
;; From https://www.reddit.com/r/emacs/comments/fgcw2b/org_change_date_format_only_on_export/
;; custom format to 'euro' timestamp
(setq org-time-stamp-custom-formats '("<%d.%m.%Y>" . "<%d.%m.%Y %a %H:%M>"))
;; function with hook on export
(defun my-org-export-ensure-custom-times (backend)
  (setq-local org-display-custom-times t))
(add-hook 'org-export-before-processing-hook 'my-org-export-ensure-custom-times)
;; remove brackets on export
(defun org-export-filter-timestamp-remove-brackets (timestamp backend info)
  "removes relevant brackets from a timestamp"
  (cond
   ((org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "[<>]\\|[][]" "" timestamp))
   ((org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "&[lg]t;\\|[][]" "" timestamp))))
(eval-after-load 'ox '(add-to-list
                       'org-export-filter-timestamp-functions
                       'org-export-filter-timestamp-remove-brackets))

;; Prevents most accidental deletions
(setq-default org-catch-invisible-edits 'smart)
;; Spellchecking
(use-package flyspell-correct
  :config
  (defun custom-flycheck-executable-find (executable)
  "Replaces flychecks original so it can find project specific versions of eslint.x
Resolve EXECUTABLE to a full path.

Like `executable-find', but supports relative paths.

Attempts invoking `executable-find' first; if that returns nil,
and EXECUTABLE contains a directory component, expands to a full
path and tries invoking `executable-find' again."
  ;; file-name-directory returns non-nil iff the given path has a
  ;; directory component.
  (message executable)
  (or
   (when (string-equal executable "eslint")
     (concat (projectile-project-root) "node_modules/.bin/eslint"))
   (or
    (executable-find executable)
    (when (file-name-directory executable)
      (executable-find (expand-file-name executable))))))
:custom (flycheck-executable-find 'custom-flycheck-executable-find)
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(add-hook 'text-mode-hook 'flyspell-mode)
;; Useful for correcting spelling in comments in code
(add-hook 'prog-mode-hook 'flyspell-mode)
;; Sorts suggestions in order of most used
(use-package company-prescient
  :hook
  (prog-mode . company-prescient-mode)
  (prog-mode . prescient-persist-mode)
  )

;; Helpful for general writing
(use-package company-wordfreq
  :straight (company-wordfreq :type git :host github :repo "johannes-mueller/company-wordfreq.el")
  :custom (ispell-local-dictionary "english")
  )

(add-hook 'text-mode-hook (lambda ()
                            (setq-local company-backends '(company-wordfreq))
                            (setq-local company-transformers nil)))
(use-package define-word)
(use-package synonyms)
(setq synonyms-file "~/mthesaur.txt")
(use-package org-ref)


(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "/path/to/journal/files/")
  (org-journal-date-format "%A, %d %B %Y"))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org-roam/"))

;; My own custom functions
(defun jump-to-end-of-buffer ()
  (goto-char (point-max))
  )

(defun get-readable-time ()
  (interactive)
  (substring (current-time-string) 11 16)
  )

(defun insert-current-time () "Inserts the current time in 24 hour format in the current buffer"
      (interactive)
      (insert (get-readable-time))
      )

;; Automating work
(defun clock-on () "Opens ~/work/hours.org and adds the time to the end of the file"
      (interactive)
      (find-file "~/work/hours.org")
      (goto-char (point-max))
      (insert "** ")
      (org-time-stamp t)
      (insert "\nStart ")
      (insert-current-time)
      (insert "\n")
      (save-buffer)
      )

(defun clock-off () "Jumps to the end of ~/work/hours.org and inserts the time"
      (interactive)
      (find-file "~/work/hours.org")
      (goto-char (point-max))
      (insert "\nEnd ")
      (insert-current-time)
      (insert "\n")
      (save-buffer)
      )

;; Music and audiobooks setup
(use-package emms
  :straight (emms :type git :host github :repo "emacsmirror/emms")
:config
(require 'emms-setup)
(require 'emms-player-mplayer)
(emms-all)
(setq emms-player-list '(
                         emms-player-mpg321
                         emms-player-ogg123
                         emms-player-mplayer))
(defun emms-player-mplayer-volume(amount)
  (process-send-string
   emms-player-simple-process-name
   (format "volume %d\n" amount)))
(setq emms-volume-change-function 'emms-player-mplayer-volume)
(setq emms-source-file-default-directory "~/audiobooks/")
(emms-add-directory-tree emms-source-file-default-directory)
)

;;(define-emms-simple-player mplayer '(file url)
;;     (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
;;		   ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
;;		   ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
;;     "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")

(setq emms-info-functions '(emms-info-tinytag))
(setq emms-source-file-default-directory "~/audiobooks/")
;; Toggles between playing and paused despite only being called pause
(global-set-key (kbd "C-x p") 'emms-pause)

;; Magit
(use-package magit
  :custom
  (global-magit-file-mode 1)
  (magit-define-global-key-bindings t)
  )
(global-set-key (kbd "C-c g") 'magit-file-dispatch)

;; Diary and calendar stuff
(setq diary-file "~/.emacs.d/diary")
(setq diary-location "~/.emacs.d/diary-files/")
(setq diary-display-function 'diary-fancy-display)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)

; calendars you want to download
; each item links to a remote iCal calendar
(setq calendars
      '(
	("timetable" . "https://mytimetable.newcastle.edu.au/odd/rest/calendar/ical/90dc6e24-5cef-4fd5-b0d2-be310d5d4fea")
        ))

(defun cpm--getcal (url file)
  "Download ics file and add it to file"
  (let ((tmpfile (url-file-local-copy url)))
    (icalendar-import-file tmpfile file)
    (kill-buffer)
    (kill-buffer)))

(defun update-calendars ()
  "Load a set of ics calendars into emacs diary files"
  (interactive)
  (mapcar #'(lambda (x)
              (let ((file (concat diary-location (car x)))
                    (url (cdr x)))
                (message (concat "Loading " url " into " file))
                (find-file file)
                ;; (flush-lines "^[& ]") ;; if you import ical as non marking
                (erase-buffer) ;; to avoid duplicating events
                (cpm--getcal url file)
                ))
          calendars))

(setq org-agenda-files "~/org/agenda-files.txt")
(setq org-agenda-span 7)
(setq org-agenda-custom-commands '(
				   ("m" "Overview" ((agenda "") (alltodo ""))
				    )))
;; (use-package md4rd
;;   :config
;;   (add-hook 'md4rd-mode-hook 'md4rd-indent-all-the-lines)
;;   (setq md4rd-subs-active '(emacs lisp+Common_Lisp prolog clojure))
;;   (setq md4rd--oauth-access-token
;;         "your-access-token-here")
;;   (setq md4rd--oauth-refresh-token
;;         "your-refresh-token-here")
;;   (run-with-timer 0 3540 'md4rd-refresh-login))

;; Python jupitor mode
(use-package ein)

;; In case it crashes
(defun restart-emacspeak ()
  (interactive)
  (setq dtk-program "mac")
  (load-file "~/emacspeak/lisp/emacspeak-setup.el")
  ;; Customising emacspeak
  (global-set-key (kbd "M-c") 'emacspeak-speak-current-column)
  (dtk-set-rate 720 t)
(setq mac-default-voice-string "[{voice Karen}]"))

;; Uml
(defun setup-plantuml-mode ()
  (local-set-key (kbd "M-.") 'plantuml-find-entity)
  (emacspeak-toggle-audio-indentation))
(use-package plantuml-mode
  :hook (plantuml-mode . setup-plantuml-mode))

;; Helper function
(defun do-lines (fun &optional start end)
  "Invoke function FUN on the text of each line from START to END."
  (interactive
   (let ((fn   (intern (completing-read "Function: " obarray 'functionp t))))
     (if (use-region-p)
         (list fn (region-beginning) (region-end))
       (list fn (point-min) (point-max)))))
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (funcall fun (buffer-substring (line-beginning-position) (line-end-position)))
      (forward-line 1))))
(setq TeX-electric-math (cons "$" "$"))

;; Arduinos
(use-package arduino-mode)
(use-package arduino-cli-mode)

;; Auto-formatting for scentences
(use-package twauctex
  :straight (twauctex :type git :host github :repo "jeeger/twauctex"))

;; navigation to functions from help buffers
(add-hook 'help-mode-hook 'elisp-slime-nav-mode)

;; convert file to use one scentence per line mode
(defun convert-to-one-sentence-per-line ()
  "Adds newline after full stops in a file at the end of sentences"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward-regexp "\\([a-z][a-z]\\|\\$\\)\\. " nil t)
      (open-line 1)
      (indent-relative-maybe)
      ))
  )

;; Clean up lectures and notes
;; Can be used for other lectures too
(defun org-clean-lecture ()
  "Removes unneeded stuff from lectures"
  (interactive)
  (save-excursion
    (convert-to-one-sentence-per-line)
  (beginning-of-buffer)
  (while (search-forward "\n\n" nil t)
    (replace-match "\n" nil t))
  (beginning-of-buffer)
  (while (search-forward "#+begin_center\n" nil t)
    (replace-match "" nil t))
  (beginning-of-buffer)
  (while (search-forward "#+end_center\n" nil t)
    (replace-match "" nil t))
  (beginning-of-buffer)
  (while (search-forward "#+begin_example\n#+end_example\n" nil t)
    (replace-match "" nil t))
  (beginning-of-buffer)
  (while (search-forward "\\(" nil t)
    (replace-match "$" nil t))
  (beginning-of-buffer)
  (while (search-forward "\\)" nil t)
    (replace-match "$" nil t))
  (beginning-of-buffer)
  (while (search-forward "$ $" nil t)
    (replace-match "$\n$" nil t))
    (beginning-of-buffer)
  (while (search-forward "),(" nil t)
    (replace-match "),\n(" nil t))
  ))

(setq org-startup-folded t)

;; Hide every properties drawer
(defun org-hide-all-properties ()
  "Hides all the property drawers in the current file"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward ":PROPERTIES:"  nil t) (org-hide-drawer-toggle))
    )
  )

;; Tables of contents in org files
(defun org-make-table-of-contents ()
  "Makes a table of contents at the start of the buffer"
  (interactive)
  (let (base-indent))
  (setq  base-indent (current-indentation))
  (save-excursion
    (if (search-forward ":contents:" nil t)
	(progn
	  (search-forward ":end:" nil t)
	  (end-of-line)
	  (setq end-contents-position (point))
	  (search-backward ":contents:" nil t)
	  (beginning-of-line)
	  (kill-forward-chars (- end-contents-position (point)))
	  )
      (beginning-of-buffer))
    (setq links (org-map-entries (lambda ()
		       (list (org-store-link nil) (nth 0 (org-heading-components)))
		       ) nil nil))
    (insert ":contents:")
    (insert "\n")
    (message "before loop")
    (cl-loop for link in links do
	     (progn
	       (dotimes (_ (+ base-indent (* 2 (nth 1 link)))) (insert " "))
	       (message "indent added")
	       (insert "- ")
	       (insert (nth 0 link))
	       (insert "\n")
	       )
	     )
    (insert ":end:\n")
    ))


(defun org-replace-link-by-link-description ()
    "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2) 
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))

(defun org-replace-all-links-by-their-description ()
  "Replaces every org link in the buffer by its description"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward "[[" nil t)
      (org-replace-link-by-link-description))))
;; Improve latex editing in org mode
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; Spotify setup
;; Settings
(setq auth-sources '(password-store))
(setq epg-pinentry-mode 'loopback)
(setq mu4e-get-mail-command (format "INSIDE_EMACS=%s mbsync -a" emacs-version))
(use-package pinentry
  :config (pinentry-start))

(load-file "~/.emacs.d/smudge-config.el.gpg")
(setq smudge-transport 'connect)
(use-package smudge
  :straight (smudge :type git :host github :repo "Isaac-Leonard/smudge"))
(define-key smudge-mode-map (kbd "C-c .") 'smudge-command-map)
(global-smudge-remote-mode)

;; Disable EWW line trunkation
(defadvice shr-fill-text (around shr-no-fill-text activate)
  "Do not fill text when `shr-no-fill-mode' is enabled."
  (if (bound-and-true-p shr-no-fill-mode)
      (ad-get-arg 0)
    ad-do-it))

(defadvice shr-fill-lines (around shr-no-fill-lines activate)
  "Do not fill text when `shr-no-fill-mode' is enabled."
  (unless (bound-and-true-p shr-no-fill-mode)
    ad-do-it))

(defadvice shr-fill-line (around shr-no-fill-line activate)
  "Do not fill text when `shr-no-fill-mode' is enabled."
  (unless (bound-and-true-p shr-no-fill-mode)
    ad-do-it))

(define-minor-mode shr-no-fill-mode
  "Global minor mode which prevents `shr' and `eww' from filling text output."
  ;; :lighter (:eval (if (derived-mode-p 'eww-mode) " ShrNoFill"))
  :global t)

(shr-no-fill-mode 1) ;; To enable by default.
;; M-x shr-no-fill-mode to toggle.

;; Quickly open this file
(defun open-init-file ()
  "Opens the ~/.emacs.d/init.el file"
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )

;; Rust setup
(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t))

(use-package cdlatex)

;; Matrix-client
;; Install `plz' HTTP library (not on MELPA yet).
(use-package plz
  :straight (plz :type git :host github :repo "alphapapa/plz.el"))

;; Install Ement.
(use-package ement
  :straight (ement :type git :host github :repo "alphapapa/ement.el"))

;; Makes magit work
(setq project-switch-commands t)

;; Change the default ms-sql program to a more modern one
(setq sql-ms-program "sqlcmd")
(setq sql-connection-alist
      '((connection-string-a (sql-product 'ms)
                             (sql-server "127.0.0.1:1443")
                             (sql-user "SA")
                             (sql-password "P1:sword'")
                             (sql-database "master"))))

;; Email
;;(use-package mu4e)
(add-to-list 'load-path (directory-file-name "/usr/local/share/emacs/site-lisp/mu/mu4e"))
(load-file "~/.config/mu4e/mu4e-config.el")
;; Fetch email every 5 minutes
(setq mu4e-update-interval (* 60 5))
(setq mu4e-compose-signature "Best regards Isaac")
(setq message-kill-buffer-on-exit t)
(defun org-store-link-to-mail-query ()
  "Stores a link to the current header view"
  (interactive)
  (setq mu4e-org-link-query-in-headers-mode t)
  (org-store-link)
  (setq mu4e-org-link-query-in-headers-mode nil))
(require 'mu4e-icalendar)
(mu4e-icalendar-setup)
(setq mu4e-icalendar-trash-after-reply t)
(setq mu4e-icalendar-diary-file (expand-file-name "~/.emacs.d/diary-files/mu4e"))
(setq dired-auto-revert-buffer t)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
       (define-key mu4e-headers-mode-map (kbd "C-c c") 'mu4e-org-store-and-capture)
       (define-key mu4e-view-mode-map    (kbd "C-c c") 'mu4e-org-store-and-capture)

(add-hook 'mu4e-index-updated-hook
	  (defun handle-new-emails ()
	    (play-sound-file "/System/Library/Sounds/Glass.aiff")
	    (make-process
	     :name "say-mail"
	     :command (list "espeak" "Mail")
	     :sentinel 'ignore)
	    (make-process
	     :name "kill-mu"
	     :command (list "killall" "mu")
	     :sentinel 'ignore)))

(setq mu4e-hide-index-messages t)
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-display-update-status-in-modeline t)
(setq mu4e-headers-precise-alignment t)
(advice-add 'mu4e~headers-truncate-field-precise :override (lambda (field val width) val))
;; Enable proper sound support on macos
(use-package play-sound
  :straight (play-sound :type git :host github :repo "leoliu/play-sound-osx"))

(defun my-org-capture-notes-file (buffer)
  "Returns the name of the file to write notes to when org-capture is invoked"
  (setq in-uni (string-match "/uni/" buffer))
  (if in-uni (expand-file-name "notes.org" (string-join (seq-subseq (split-string buffer "/") 0 6) "/"))
    (expand-file-name "~/org/notes.org")))
(defun my-org-capture-notes-file-from-buffer (&rest args)
  (setq org-default-notes-file (my-org-capture-notes-file buffer-file-name)))

(advice-add 'org-capture :before 'my-org-capture-notes-file-from-buffer)

(setq org-plantuml-executable-path (expand-file-name "/usr/local/bin/plantuml"))
(setq org-plantuml-exec-mode 'plantuml)
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(require 'ob-sql)
(org-babel-do-load-languages 'org-babel-load-languages '(
  (plantuml . t)
  (sql t)))
(setq org-odt-preferred-output-format "docx")

(use-package pdf-tools
  :config (pdf-tools-install))

(defun smudge-connect-set-volume (new-volume)
  "Set the volume on the actively playing device."
  (smudge-connect-when-device-active
   (smudge-api-get-player-status
    (lambda (status)
        (smudge-api-set-volume
         (smudge-connect-get-device-id status)
         new-volume
         (lambda (_)
           (message "Volume set to %d%%" new-volume)))))))

(defun smudge-controller-set-volume (volume)
  "Increase the volume for the active device."
  (interactive "nVolume to set to:")
  (smudge-controller-apply "set-volume" volume))
(define-key smudge-command-map (kbd "M-v") #'smudge-controller-set-volume)
(defun plantuml-find-entity ()
  "Try jump to the name of the refered entity for plantuml"
  (interactive)
  (let ((name (thing-at-point 'word t)))
    (progn (search-backward (concat "entity " name) nil t)
	   (beginning-of-line 1)
	   (forward-word)
	   (forward-char)
	   (emacspeak-speak-word)
	   )
    )
)

(setq company-idle-delay 1)

(defun safe-replace-string(char1 char2)
  "replace-string for non interactive use"
  (save-excursion   (beginning-of-buffer)
		    (while (search-forward char1 nil t)
		      (replace-match char2 nil t))))
(defun switch-letters (char1 char2)
  "Replaces all instances of char 1 with char1 and all instances of char2 with char1"
  (interactive "MCharacter 1\nMCharacter2")
  (save-excursion (beginning-of-buffer)
		  (safe-replace-string char1 "1")
		  (beginning-of-buffer)
		  (safe-replace-string char2 char1)
		  (beginning-of-buffer)
		  (safe-replace-string "1" char2)))

(ido-mode t)
(setq ido-use-filename-at-point 'guess)
(setq ido-use-url-at-point t)
(setq ido-show-dot-for-dired t)
(ido-everywhere t)
;;(setq ido-auto-merge-delay-time 0.9)
(setq emacspeak-ido-typing-delay 0.4)
(setq ido-enable-flex-matching t)
(use-package yasnippet
  :config (yas-global-mode t))
(use-package yasnippet-snippets)

(use-package god-mode)
(global-set-key (kbd "M-g m") (lambda ()
				(interactive)
				(god-local-mode 'toggle)
				(read-only-mode 'toggle)))

(use-package smex
  :bind (("M-x" . smex))
  :config (smex-initialize))

(use-package pass)

;; Shit goes slow with the defaults for some reason
;; The original code seems to scroll visually I think so not needed
(define-key org-agenda-mode-map (kbd "n") #'next-line)
(define-key org-agenda-mode-map (kbd "C-n") #'next-line)
(define-key org-agenda-mode-map (kbd "p") #'previous-line)
(define-key org-agenda-mode-map (kbd "C-p") #'previous-line)
(mu4e-update-mail-and-index t)
(add-hook 'after-change-major-mode-hook (lambda () (setq dtk-caps nil)))

(save-place-mode)

;; Starts the spotifyd demon and attempts to restart it on crashes
(defun run-spotifyd ()
  "Runs spotifyd and seemlessly restarts it on crashes"
  (interactive)
  (if (internet-up-p "play.spotify.com")
      (progn
	(make-process
	 :name "spotifyd"
	 :command (list "spotifyd" "--no-daemon")
	 :sentinel (lambda (a b)
		     (run-spotifyd)))
	(run-with-timer 1 nil  (lambda() (smudge-api-device-list
					  (lambda (json)
					    (when-let ((devices (gethash 'devices json)))
					      (while (let* ((device (car devices))
							    (is-active (smudge-device-get-device-is-active device))
							    (name (smudge-device-get-device-name device))
							    (is-daemon (= (string-match "Demon" name) 0))
							    (device-id (smudge-device-get-device-id device)))
						       (progn
							 (if (and device is-active)
							     (progn
							       (setq smudge-selected-device-id device-id)
							       (smudge-controller-player-status))
							   (if is-daemon
							       (smudge-api-transfer-player
								device-id
								(lambda (json)
								  (setq smudge-selected-device-id device-id)
								  (smudge-controller-set-volume 70)
								  (smudge-controller-toggle-play)
								  (message "Device '%s' selected" name)))))
							 (setq devices (cdr devices))
							 (and device (not (or is-daemon is-active))))))))))))
			(message "spotify appears to be down")))

;; Tries to start the music when we open emacs
(add-hook 'after-init-hook (lambda ()
			     (run-spotifyd)))

(defun internet-up-p (&optional host)
  "Checks to see if the internet is working"
  (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1" 
                     (if host host "www.google.com"))))

(use-package browse-kill-ring)

(defun completing-read-by (fn prompt list &rest args)
  "Apply FN to each element of LIST and prompt the user to select a resulting value.
The output of the function will be the corresponding element of LIST
ARGS will be passed to `completing-read' after the PROMPT and COLLECTION arguments.
Copied from:
https://emacs.stackexchange.com/a/26527
with modifications made for ido"
  (let ((hash (make-hash-table :test 'equal))
	(keys (list)))
    (mapc (lambda (elem)
	    (let ((key (funcall fn elem)))
	      (puthash key elem hash)
	      (push key keys))) list)
    (gethash (ido-completing-read prompt keys) hash)))

(defun smudge-playlist-tracks-view (playlist)
  "Displays the tracks that belongs to the playlist passed to it"
  (interactive)
  (let* ((selected-playlist (smudge-api-get-item-id playlist))
         (name (smudge-api-get-item-name playlist))
         (buffer (get-buffer-create (format "*Playlist Tracks: %s*" name))))
    (with-current-buffer buffer
      (smudge-track-search-mode)
      (setq-local smudge-selected-playlist playlist)
      (smudge-track-playlist-tracks-update 1))))

(defun smudge-playlists-view ()
  "Shows a list of user playlists for selection in an ido completion buffer"
  (interactive)
  (smudge-api-current-user
   (lambda (user)
    (smudge-api-user-playlists
       (smudge-api-get-item-id user)
     1
     (lambda (playlists)
       (if-let ((items (smudge-api-get-items playlists)))
	   (smudge-playlist-tracks-view (completing-read-by 'smudge-api-get-item-name "Playlist: " items))
         (message "No more playlists")))))))

(add-hook 'java-mode-hook (lambda()(setq tab-width 4)))
 
(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
        (message "No non-ascii characters."))))

(setq kill-ring-max 1000)

;;; runs eslint --fix on the current file after save
;;; alpha quality -- use at your own risk
;;; From https://gist.github.com/ustun/73321bfcb01a8657e5b8
(defun eslint-fix-file ()
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat "eslint --fix " (buffer-file-name))))

(defun eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

(add-hook 'js2-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))

(defun kill-type-definition-buffers ()
  "Kills any *.d.ts buffers"
  (interactive)
  (kill-matching-buffers "[A-Za-z\-]*.d.ts[A-Za-z\-<>]*" t t))
(setq help-window-select t)

(add-hook 'flycheck-mode-hook (lambda() (bind-key "C-c n" (lambda ()(interactive) (flycheck-next-error)(run-with-timer 3.4 nil 'cancel-timer (run-with-idle-timer 3 nil 'emacspeak-speak-line '(4)))))
				(bind-key "C-c p" (lambda ()(interactive) (flycheck-previous-error) (run-with-timer 3.4 nil 'cancel-timer (run-with-idle-timer 3 nil 'emacspeak-speak-line '(4)))))))

;; Kill zomby mu processes
(add-hook 'mu4e-update-pre-hook (lambda ()(run-with-timer 240 nil 'mu4e-kill-update-mail)))

;; Start up dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (add-to-list 'dashboard-items '(projects . 10))
  (add-to-list 'dashboard-items '(recents . 20))
  (add-to-list 'dashboard-items '(agenda) t)
  ;; advice-add doesn't work for line movement functions for some reason
  ;; Copied this from emacspeak/lsip/advice.el with modifications for dashboard
  (cl-loop
   for f in
   '(dashboard-next-line dashboard-previous-line)
   do
   (eval
    `(defadvice ,f (after emacspeak pre act comp)
       "Speak line. Speak  (visual) line if
`visual-line-mode' is  on, and 
indicate  point  by an aural highlight. Moving to 
beginning or end of a physical line produces an  auditory icon."
       (when (ems-interactive-p)
	 (cond
          ((or line-move-visual visual-line-mode) (emacspeak-speak-visual-line))
          (t (emacspeak-speak-line)))))))
  (advice-add 'dashboard-jump-to-projects :after (lambda()(emacspeak-speak-line)))
  (advice-add 'dashboard-jump-to-recent-files :after (lambda()(emacspeak-speak-line)))
  (advice-add 'dashboard-jump-to-bookmarks :after (lambda()(emacspeak-speak-line)))
  ;; Save recentf at regular intervals
  (run-with-timer 3 30 'recentf-save-list)

  ;; Exclude the recentf file itself
  (add-to-list 'recentf-exclude
               (expand-file-name "~/.emacs.d/var/recentf-save.el"))
  (add-to-list 'recentf-exclude
               (expand-file-name "~/org/agenda-files.txt"))
  ;; Exclude the org-agenda files
  ;; (they flood the recentf because dashboard always checks their content)
  (add-to-list 'recentf-exclude (org-agenda-files))

  :custom (dashboard-projects-backend 'projectile)
  (dashboard-filter-agenda-entry 'dashboard-no-filter-agenda))
(use-package projectile
  :init
  (projectile-mode +1))

(use-package python-black
  :hook ((python-mode . python-black-on-save-mode)))

(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :hook ((python-mode . dap-ui-mode)
	 (python-mode . dap-mode))
  :config (eval-when-compile (require 'cl))
  (require 'dap-python)
  (require 'dap-lldb)
  ;; Temporal fix
  (defun dap-python--pyenv-executable-find (command) (with-venv (executable-find "python"))) )
(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))
(use-package python-mode
  :config
  (defun dd/py-workon-project-venv ()
    "Call pyenv-workon with the current projectile project name.
This will return the full path of the associated virtual
environment found in $WORKON_HOME, or nil if the environment does
not exist."
    (let ((pname (projectile-project-name)))
      (pyvenv-workon pname)
      (if (file-directory-p pyvenv-virtual-env)
          pyvenv-virtual-env
	(pyvenv-deactivate))))
  
  (defun dd/py-auto-lsp ()
    "Turn on lsp mode in a Python project with some automated logic.
Try to automatically determine which pyenv virtual environment to
activate based on the project name, using
`dd/py-workon-project-venv'. If successful, call `lsp'. If we
cannot determine the virtualenv automatically, first call the
interactive `pyvenv-workon' function before `lsp'"
    (interactive)
    (let ((pvenv (dd/py-workon-project-venv)))
      (if pvenv
          (lsp)
	(progn
          (call-interactively #'pyvenv-workon)
          (lsp)))))

  (bind-key (kbd "C-c C-a") #'dd/py-auto-lsp python-mode-map))

(use-package lsp-pyright
  :hook (python-mode . dd/py-auto-lsp))
(use-package poetry)
(use-package py-isort
  :hook ((python-mode . (lambda () (add-hook 'before-save-hook  'py-isort-before-save)))))

