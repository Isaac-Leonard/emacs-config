;;; -*- lexical-binding: t; -*-

;; Set up emacspeak before everything else so that it runs even if there's an error later on
;; (toggle-debug-on-error)
(setq dtk-program "swiftmac")
(load-file "~/emacspeak/lisp/emacspeak-setup.el")
(setq mac-default-voice-string "[{voice Karen-premium}]")
;; Customising emacspeak
(global-set-key (kbd "M-c") 'emacspeak-speak-current-column)
(dtk-set-rate 720 t)
(setq emacspeak-auditory-icon-function 'emacspeak-serve-auditory-icon)
(setq emacspeak-pronounce-dictionaries-file (expand-file-name "~/.emacs.d/pronounciations.el"))
(setq emacspeak-play-program "afplay")

;; In case it crashes
(defun restart-emacspeak ()
  (interactive)
  (setq dtk-program "swiftmac")
  (load-file "~/emacspeak/lisp/emacspeak-setup.el")
  ;; Customising emacspeak
  (global-set-key (kbd "M-c") 'emacspeak-speak-current-column)
  (dtk-set-rate 720 t)
  (setq mac-default-voice-string "[{voice Karen-premium}]"))

;; Needed to fix bugs
;; TODO: Not sure if this is still needed
(add-to-list 'image-types 'svg)
;; overriding image.el function image-type-available-p
(defun image-type-available-p (type)
  "Return t if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
  (if (eq 'svg type)
      nil
    (and (fboundp 'init-image-library)
         (init-image-library type))))

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
 '(message-fill-column 0)
 '(safe-local-variable-values
   '((lsp-rust-features . ["heap"]) (lsp-rust-features . ["all"])
     (lsp-rust-features . listp))))

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

;; Use "use-package" and download if on older versions of emacs
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
  (lsp-keymap-prefix "s-l")
  (lsp-eldoc-render-all t)
  ;; Rust stuff
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-inlay-hint-enable t)
  (lsp-rust-analyzer-completion-add-call-argument-snippets nil)
  (lsp-rust-analyzer-completion-add-call-parenthesis nil)  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil))


;; LaTeX setup
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

;; Java
(use-package lsp-java
  :config (setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.37.0/jdt-language-server-1.37.0-202406271335.tar.gz")
  (setq lsp-java-import-gradle-enabled t)
    (setq lsp-java-configuration-runtimes '[(:name "JavaSE-21"
                                                 :path "/opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home"
                                                 :default t)])
    (setq lsp-java-import-gradle-wrapper-enabled t)
  )

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
  (company-selection-wrap-around t)
  )

;; Inline documentation where possible
(use-package eldoc
  :hook (lsp-mode . eldoc-mode)
  :custom (eldoc-echo-area-use-multiline-p t)
  :config (advice-add 'eldoc-doc-buffer :after (lambda (&rest _) (switch-to-buffer eldoc--doc-buffer)))
)

;; Error checking, never actually noticed what it does though
(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :config (advice-add 'flycheck-verify-setup :after (lambda ()(switch-to-buffer "*Flycheck checkers*")))
  (defun find-eslint-bin ()
    "Returns the eslint path if it exists"
    (concat (locate-dominating-file (buffer-file-name) "node_modules/.bin/eslint")  "node_modules/.bin/eslint"))
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
     (find-eslint-bin))
   (or
    (executable-find executable)
    (when (file-name-directory executable)
      (executable-find (expand-file-name executable))))))
  :custom (flycheck-executable-find 'custom-flycheck-executable-find))

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
  (typescript-ts-mode company flycheck)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'tide-mode)
  :config (advice-add 'tide-references :after (lambda ()(switch-to-buffer "*tide-references*")))  )

(add-hook 'typescript-ts-mode-hook #'setup-tide-mode)

;; Format js and ts code
(use-package prettier-js
  :hook 
  (tide-mode . prettier-js-mode)
  (web-mode . prettier-js-mode)
  )

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
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

;; Org- setup
(use-package org)
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

;; Variables
(setq org-agenda-files "~/org/agenda-files.txt")
(setq org-agenda-span 7)
(setq org-agenda-custom-commands '(
				   ("m" "Overview" ((agenda "") (alltodo ""))
				    )))
;; Include holidays and stuff in org-agender
(setq org-agenda-include-diary t)
;; Capture notes
(setq org-directory "~/org/")
(setq org-default-notes-file "~/org/notes.org")

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "" "Tasks")
         "* TODO %?\n  %i\n  %a")
	("n" "Note" entry (file+headline "" "Notes")
         "* %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/diary.org")
         "* %?\nEntered on %U\n  %i\n  %a" :kill-buffer)
	("P" "process-soon" entry (file+headline "~/org/notes.org" "Emails")
	 "** TODO %:fromname: %a \nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+5d\"))")))
(use-package org-roam
  :custom (org-roam-directory (file-truename "~/org-roam/"))
  (org-roam-v2-ack t)
  (org-roam-db-location "/Users/isaac/.emacs.d/org-roam.db")
  (org-roam-capture-templates '(("m" "main" plain "%?"
				 :target (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
				 :unnarrowed t)
				("r" "reference" plain "%?"
				 :target (file+head "reference/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
				 :unnarrowed t)))
  (org-roam-dailies-directory "dailies/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-node-display-template
   (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :config (org-roam-db-autosync-mode)
  (defun org-roam-make-note-of-interest ()
    "Prompts the user for information and stores it in a list of things to review later"
    (interactive)
    (write-region (format "* %s\n%s\n%s\n" (read-string "Title: ") (read-string "Description: ") (read-string "Source: ")) nil "~/org-roam/links.org" t))
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
	(file-name-nondirectory
	 (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  :ensure t
  :bind (("C-o" . nil) ;; Got to remove default open-line that I never use
	 ("C-o r f" . org-roam-node-find)
         ("C-o r r" . org-roam-node-random)		    
	 ("C-o r c" . org-roam-capture)
	 ("C-o r n" . org-roam-make-note-of-interest)
         (:map org-mode-map
               (("C-o r i" . org-roam-node-insert)
                ("C-o r o" . org-id-get-create)
                ("C-o r t" . org-roam-tag-add)
                ("C-o r a" . org-roam-alias-add)
                ("C-o r l" . org-roam-buffer-toggle)))))

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
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(add-hook 'text-mode-hook 'flyspell-mode)
;; Useful for correcting spelling in comments in code
(add-hook 'prog-mode-hook 'flyspell-mode)
(use-package org-ref)
(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/org/journal/")
  (org-journal-date-format "%A, %d %B %Y"))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))
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

(setq org-plantuml-executable-path (expand-file-name "/usr/local/bin/plantuml"))
(setq org-plantuml-exec-mode 'plantuml)
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(require 'ob-sql)
(org-babel-do-load-languages 'org-babel-load-languages '(
  (plantuml . t)
  (sql t)))
(setq org-odt-preferred-output-format "docx")

;; RSS feeds
(defun my/elfeed-jump-to-article-body (buf &optional NORECORD FORCE-SAME-WINDOW)
  "Switch to elfeed article buffer and move point to the beginning of the article text in `elfeed-show-mode`."
  (switch-to-buffer buf NORECORD FORCE-SAME-WINDOW)
  (when (eq major-mode 'elfeed-show-mode)
    (goto-char (point-min))
    ;; Skip past title, tags, and metadata
    (re-search-forward "\n\n" nil t)))

(use-package elfeed
  :custom (elfeed-search-title-max-width 280)
  (elfeed-search-title-min-width 280)
  :config (global-set-key (kbd "C-x w") 'elfeed)
  :custom (elfeed-show-entry-switch 'my/elfeed-jump-to-article-body))
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
  (defun emms-bookmark-on-pause ()
    (if (emms-playlist-current-selected-track)
     (emms-bookmarks-set-current "paused")
      (error "No current track to bookmark")))
  (defun emms-jump-to-bookmark-on-play ()
    "Goes to a paused bookmark if its set when a track starts playing"
    (let ((bookmark (emms-bookmarks-next)))
    (while (not (or (equal bookmark "No next bookmark") (equal bookmark "paused")))
      (setq bookmark (emms-bookmarks-next))))
    (emms-bookmarks-clear))
  :hook (emms-player-paused . emms-bookmark-on-pause)
  (emms-player-started . emms-jump-to-bookmark-on-play))


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
    (kill-buffer)))

(defun update-calendars ()
  "Load a set of ics calendars into emacs diary files"
  (interactive)
  (mapcar #'(lambda (x)
              (let ((file (concat diary-location (car x)))
                    (url (cdr x)))
                (message "%s" (format "Loading %s into %s" url file))
                (find-file file)
                ;; (flush-lines "^[& ]") ;; if you import ical as non marking
                (erase-buffer) ;; to avoid duplicating events
                (cpm--getcal url file)
                ))
          calendars))
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


;; Security settings
(setq auth-sources '(password-store "~/.emacs.d/auth-info.gpg"))
(setq epg-pinentry-mode 'loopback)

(use-package pinentry
  :config (pinentry-start))

(setq mu4e-get-mail-command (format "INSIDE_EMACS=%s mbsync -a" emacs-version))

(load-file "~/.emacs.d/smudge-config.el.gpg")
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
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq safe-local-variable-values '((lsp-rust-features . listp)))
  (advice-add 'rustic-cargo-run :after (lambda (&optional v w)(switch-to-buffer "*cargo-run*")))
  (advice-add 'rustic-cargo-check :after (lambda (&optional v w)(switch-to-buffer "*rustic-compilation*")))
  (advice-add 'rustic-cargo-clippy :after (lambda (&optional v w)(switch-to-buffer "*cargo-clippy*")))
  (add-hook 'rustic-mode-hook 'flycheck-mode)
  (add-hook 'rustic-mode-hook #'(lambda() (add-hook 'before-save-hook 'lsp-format-buffer nil t)))
  (add-hook 'rustic-mode-hook #'(lambda () (setq-local tab-width 4)))
  :custom (rustic-default-test-arguments "--all-features"))

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
(add-to-list 'load-path (directory-file-name "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"))
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

(setq mu4e-hide-index-messages t)
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-display-update-status-in-modeline t)
(setq mu4e-headers-precise-alignment t)
(advice-add 'mu4e~headers-truncate-field-precise :override (lambda (field val width) val))
;; Enable proper sound support on macos
(use-package play-sound
  :straight (play-sound :type git :host github :repo "leoliu/play-sound-osx"))

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
;;(add-hook 'after-init-hook (lambda ()
;;			     (run-spotifyd)))

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
  (projectile-mode +1)
  :config ;; (advice-add 'grep :after (open-buffer "*grep*"))
  (setq projectile-use-git-grep t))

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
(use-package python-mode
  :config
  (bind-key (kbd "C-c C-a") #'dd/py-auto-lsp python-mode-map))

(use-package lsp-pyright
  :hook (python-mode . lsp))
(use-package py-isort
  :hook ((python-mode . (lambda () (add-hook 'before-save-hook  'py-isort-before-save)))))


(setq lsp-sqls-connections '(((driver . "postgresql")
			      (dataSourceName b. "host=127.0.0.1 port=5432 user=docker password=password dbname=docker"))))


(defun my-sql-hook ()
  (add-to-list 'flycheck-disabled-checkers 'lsp)
  (flycheck-select-checker 'sql-sqlint)
  (lsp))
(add-hook 'sql-mode-hook #'my-sql-hook)

(setq org-duration-format (quote h:mm))

;; Automating work
(defun clock-on ()
  "Opens ~/work/hours.org and adds the time to the end of the file"
  (interactive)
  (find-file "~/work/hours.org")
  (goto-char (point-max))
  (insert "* ")
  (org-insert-time-stamp (current-time) nil t)
  (insert "\n")
  (org-clock-in)
  (save-buffer))

(defun clock-off ()
  "Jumps to the end of ~/work/hours.org and inserts the time"
  (interactive)
  (save-window-excursion (switch-to-buffer "hours.org")
		       (org-clock-out)
		       (save-buffer)))
(load-file (expand-file-name "~/.emacs.d/zoom.config.el.gpg"))
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
;; Emacspeaks' usage of the read function seems to be broken
(setq emacspeak-pronounce-pronunciation-keys '(("buffer" . "buffer")
 ("file" . "file")
 ("directory" . "directory")
 ("mode" . "mode")))
(defun emacspeak-pronounce-get-key ()
  "Collect key from user.
Returns a pair of the form (key-type . key)."
  (cl-declare (special emacspeak-pronounce-pronunciation-keys))
  (let ((key nil)
        (key-type
         (completing-read
          "Define pronunciation that is specific to: "
          emacspeak-pronounce-pronunciation-keys nil t)))
    (when (called-interactively-p 'interactive) ;cleanup minibuffer history
      (pop minibuffer-history))
    (cond
     ((string-equal key-type "buffer")
      (setq key (buffer-name))) ;handled differently
     ((string-equal key-type "file")
      (setq key (buffer-file-name))
      (or key
          (error "Current buffer is not associated with a file"))
      (setq key (intern key)))
     ((string-equal key-type "directory")
      (setq key
            (or
             (condition-case nil
                 (file-name-directory (buffer-file-name))
               (error nil))
             default-directory))
      (or key (error "No directory associated with current buffer"))
      (setq key (intern key)))
     ((string-equal key-type "mode")
      (setq key
            major-mode)
      (or key (error "No major mode found for current buffer")))
     (t (error "Cannot define pronunciations with key type %s" key-type)))
    (cons key-type key)))
(setq emacspeak-maths-inferior-program "/usr/local/opt/node@16/bin/node")

(defun replace-img-with-alt ()
  (interactive)
  (save-excursion (goto-char (point-min))
		  (while(search-forward "<img " nil t)
		    (backward-char 5)
		    (let ((start (point)))
		      (search-forward "alt=\"")
		      (kill-region start (point))
		      (search-forward "\"" nil t)
		      (setq start (- (point) 1))
		      (search-forward "/>")
		      (kill-region start (point))))))

(defun remove-tex-bf ()
  "Remove {\bf ...} from tex files"
  (interactive)
  (while (search-forward "{\\bf " nil t)
    (let ((pos (point)))
      (sp-end-of-sexp)
      (delete-char 1)
      (goto-char pos)
      (delete-char -5))))

(defun join-broken-sentences ()
  (interactive)
  (save-excursion 
    (while (re-search-forward "\\([a-z]\\)\n\\\s*\\([a-z]\\)" nil t)
      (replace-match "\\1 \\2"))))

(defun replace-string-in-buffer (str1 str2)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward str1 nil t)
      (replace-match str2))))

(defun clean-converted-org-file ()
  (interactive)
  (save-excursion (goto-char (point-min))
		  (join-broken-sentences)
		  (convert-to-one-sentence-per-line)
		  (replace-string-in-buffer "\\(" "$")
		  (replace-string-in-buffer "\\)" "$")
		  (replace-string-in-buffer "\\," " ")
		  (replace-string-in-buffer " \n" "\n")
		  (replace-string-in-buffer "\n\n" "\n")))

(use-package vlf
  :config (require 'vlf-setup))




(define-derived-mode bs-mode prog-mode "borrow script")
(require 'lsp-mode)
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection '("cargo" "run" "--bin" "lsp"))
  :server-id 'bscript
  :major-modes '(bs-mode)
  ))

(add-to-list 'lsp-language-id-configuration '(bs-mode . "bscript"))

(use-package ess)
(use-package polymode)
(use-package poly-R
  :config
  (add-to-list 'auto-mode-alist
               '("\\.[rR]md\\'" . poly-gfm+r-mode))
  :custom (markdown-code-block-braces t))


(advice-add 'ess-eval-region-or-function-or-paragraph-and-step :after (lambda (&optional v w)(switch-to-buffer "*R*")))

;; Stop numbers moving to the right in org tables
(setq org-table-number-fraction 1.1)


(use-package flashcards
  :straight (flashcards :type git :host github :repo "Isaac-Leonard/flashcards.el"))

;; Shuts up warnings when not connected to internet
(advice-add 'smudge-controller-player-status :around (lambda (fn) (if (internet-up-p) (funcall fn))) nil)

(use-package forge
  :after magit)

(use-package code-review
  :straight (code-review :type git :host github :repo "phelrine/code-review" :branch "fix/closql-update")
  :custom (code-review-auth-login-marker 'code-review))

(use-package crdt)

(defun open-buffer (buffer-name)
  "Returns a function that will open the specified buffer when called"
    (lambda (&rest _)(switch-to-buffer buffer-name)))

(use-package nodejs-repl
  :custom (nodejs-repl-command "ts-node"))

(defun say-current-line ()
  "Calls the say command with the text of the current line
Intended for debugging when emacspeak is not working correctly"
  (interactive)
  (start-process "say" "*Say*" "say" (thing-at-point 'line)))

(use-package vue-mode)

(defun speak-smudge-player-status ()
  "Update and speak the current spotify status"
  (interactive)
  (smudge-controller-player-status)
  (dtk-speak smudge-controller-player-status))
(keymap-global-set "C-c . c" 'speak-smudge-player-status)

(defun md-to-org-region (start end)
  "Convert region from markdown to org"
  (interactive "r")
  (shell-command-on-region start end "pandoc -f markdown -t org" t t))

(defun convert-5etools-md-to-org (start end)
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
	 (new-text (string-replace "|:---:|:---:|:---:|:---:|:---:|:---:|\n" ""
				   (string-replace "___\n" ""
						   (string-replace "#" "*"
								   (string-replace "##" "***"
										   (string-replace ">" ""
												   (string-replace "*" "" text))))))))
    (delete-region start end)
    (insert new-text)))

(defun del-binary_characters (beg end)
  "Delete binary characters in a region"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "[^[:ascii:]]" nil t)
        (replace-match "")))))

(setq eglot-server-programs (list))

(add-to-list 'eglot-server-programs
	     `(c++-mode . ("run-lsp.sh")))

(setq print-circle t)

(defun clean-math1210-lecture ()
  "Removes unneeded stuff from lectures"
  (interactive)
  (save-excursion
  (beginning-of-buffer)
  (while (search-forward "\\begin{frame}" nil t)
    (replace-match "\\section" nil t))
  (beginning-of-buffer)
  (while (search-forward "\\vctr" nil t)
    (replace-match "\\vec" nil t))
  (beginning-of-buffer)
  (while (search-forward "\\end{frame}" nil t)
    (replace-match "" nil t))
  (beginning-of-buffer)
  (while (search-forward-regexp "\\\\bigskip\\|\\\\smallskip\\|\\\\medskip\\|\\\\quad\\|\\\\qquad\\|\\\\center\\|\\\\mbox{}\\|\\\\box" nil t)
    (replace-match "\n" nil t))
  (beginning-of-buffer)
  (while (search-forward-regexp "\\\\visible<[0-9]+->" nil t)
    (replace-match "" nil t))
  (beginning-of-buffer)
  (while (search-forward "\\begin{block}" nil t)
    (replace-match "" nil t))
  (beginning-of-buffer)
  (while (search-forward "\\end{block}" nil t)
    (replace-match "" nil t))
  (beginning-of-buffer)
  (while (search-forward-regexp "\\\\parbox\\[[a-z]\\]{0.\\([0-9]*\\)\\\\textwidth}" nil t)
    (replace-match "" nil t))
  (beginning-of-buffer)
  (while (search-forward-regexp "{[cr|]+}" nil t)
    (replace-match "" nil t))
  (beginning-of-buffer)
  (while (search-forward-regexp "\\[-?\\([0-9]+\\)mm\\]" nil t)
    (replace-match "" nil t))
  (beginning-of-buffer)
  (while (search-forward "{array}" nil t)
    (replace-match "{matrix}" nil t))
  (beginning-of-buffer)
  (while (search-forward "%\n" nil t)
    (replace-match "\n" nil t))
  (beginning-of-buffer)
  (while (search-forward "\n}" nil t)
    (replace-match "\n" nil t))
  (beginning-of-buffer)
  (while (search-forward "\n{" nil t)
    (replace-match "\n" nil t))
  (beginning-of-buffer)
  (while (search-forward "{note}" nil t)
    (replace-match "\\note " nil t))
  (beginning-of-buffer)
  (while (search-forward "\n " nil t)
    (replace-match "\n" nil t))
  (beginning-of-buffer)
  (while (search-forward "\n\\\\" nil t)
    (replace-match "\n" nil t))
  (beginning-of-buffer)
  (while (search-forward "\n\n" nil t)
    (replace-match "\n" nil t))
  (beginning-of-buffer)
  ))

(defun replace-all-in-buffer-regexp (matcher replacement literal)
  "Replaces every instance of matcher with replacement in the buffer, if literal is non-nil then the replacement will be inserted directly, if it is nil then '\' will be treated as special"
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward-regexp matcher nil t)
      (replace-match replacement nil literal))))

(defun replace-all-in-buffer (matcher replacement)
  "Replaces every instance of matcher with replacement in the buffer"
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward matcher nil t)
      (replace-match replacement nil t))))

