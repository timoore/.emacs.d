;;; Tim Moore's .emacs file.
(defconst running-emacs19 (if (string-match "Emacs 19" (emacs-version))
			      t
			    nil))
;;; XEmacs
(defconst running-lucid (if (string-match "Lucid" (emacs-version)) t  nil))
(defconst running-emacs19-20 (or running-emacs19
				 (if (string-match "Emacs 2[01]"
						   (emacs-version))
				     t
				   nil)))

;;; Customize the load path for my own functions.
(setq load-path (append '( ;"/usr/share/maxima/5.27.0/emacs"
			  "~/gnu" ;"~/gnu/egg" ;"~/gnu/imaxima-imath-1.0"
			  "~/gnu/glsl-mode")
			load-path))

;;; general customizations
(put 'eval-expression 'disabled nil)
(setq multi-line-comment t)
(setq comment-multi-line t)
(setq-default indent-tabs-mode nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; I like this behavior much better
(setq dabbrev-case-replace nil)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(global-set-key (kbd "C-x C-b") (lambda () (interactive) (ibuffer t)))
(require 'compile)
;;; From the emacs wiki
(global-set-key [(control c) (m)] 'compile-again)
(setq compilation-last-buffer nil)
(defun compile-again (pfx)
  """Run the same compile as the last time.

If there was no last time, or there is a prefix argument, this acts like
M-x compile.
"""
  (interactive "p")
  (if (and (eq pfx 1)
	   compilation-last-buffer)
      (progn
	(set-buffer compilation-last-buffer)
	(revert-buffer t t))
    (call-interactively 'compile)))

;;; My preferred mode for Git

;(with-demoted-errors
;  ;(require 'egg)
;  (load-library "egg-grep")
;  (delete 'Git vc-handled-backends)
;  (remove-hook 'find-file-hooks 'vc-find-file-hook)
;  )

(with-demoted-errors
  (require 'gtags))

(with-demoted-errors
  (require 'cmake-mode)
  (setq auto-mode-alist
        (append
         '(("CMakeLists\\.txt\\'" . cmake-mode)
           ("\\.cmake\\'" . cmake-mode))
         auto-mode-alist)))
;;; C and C++

;;; .h files are likely to be c++
(setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))

;;; We hates it!
(setq parens-require-spaces nil)

;;; From http://emacswiki.org/emacs/EmacsTags

(require 'cl)

(defun find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the default-directory.
looking for a file with name file-to-find.  Returns the path to it
or nil if not found."
  (labels
      ((find-file-r (path)
                    (let* ((parent (file-name-directory path))
                           (possible-file (concat parent file-to-find)))
                      (cond
                       ((file-exists-p possible-file) possible-file) ; Found
                       ;; The parent of ~ is nil and the parent of / is itself.
                       ;; Thus the terminating condition for not finding the file
                       ;; accounts for both.
                       ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
                       (t (find-file-r (directory-file-name parent))))))) ; Continue
    (find-file-r default-directory)))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (local-set-key "\r" 'newline-and-indent)
	    (if (memq 'gtags features)
		(gtags-mode t))
            ;; Inventor stuff
            (let ((oiv-include (find-file-upwards "OIVHOME/include")))
              (when oiv-include
                (let ((new-ff-directories (cons oiv-include
                                                cc-search-directories))
                      (local-include (find-file-upwards "include")))
                  (when (and local-include
                             (not (string= local-include oiv-include)))
                    (push local-include new-ff-directories))
                  (setq ff-search-directories new-ff-directories))))))

(global-set-key [(control c) (control f)] 'ff-find-other-file)

;;; This makes much more sense and agrees better with tools like git diff.
(setq c-offsets-alist '((namespace-open . 0)
                        (namespace-close . 0)
                        (innamespace . 0)
                        (inextern-lang . 0)))

;;; Stroustrup, with the namespace changes above and different inline open

(c-add-style "PERSONAL-C++"
             '("stroustrup"
               (c-offsets-alist
                (inline-open . 0))))

(c-add-style "my-gnu"
             '("gnu"
               (c-offsets-alist
                (innamespace . 0))))

(c-add-style "my-linux"
             '("linux"
               (indent-tabs-mode t)))

(c-add-style "inventor"
             '("PERSONAL-C++"
               (c-basic-offset . 2)))

(setq my-c++-styles-alist
      '(("OIVHOME" . "inventor")
        (nil . "PERSONAL-C++")))

(add-hook 'c++-mode-hook
	  (lambda ()
            (let ((style
                   (assoc-default buffer-file-name my-c++-styles-alist
                                  (lambda (file path)
                                    (or (not file)
                                        (find-file-upwards file))))))
              (cond ((stringp style)
                     (c-set-style style 'dont-override))
                    (t nil)))))

(setq add-log-mailing-address "timoore33@gmail.com")

(add-hook 'change-log-mode-hook
	  '(lambda ()
	     (auto-fill-mode 1)))

(with-demoted-errors
  (require 'glsl-mode))

(defvar git-grep-history nil "History list for git-grep.")

(defun git-grep (args)
  "Run git grep."
  (interactive
   (progn
     (grep-compute-defaults)
     (let ((default (grep-default-command)))
       (list (read-shell-command "Run git grep (like this): "
                                 (if current-prefix-arg default
                                   "git --no-pager grep -n ")
                                 'git-grep-history
                                 (if current-prefix-arg nil default))))))
  (grep args))

(defun git-gra (args)
  "Run git gra."
  (interactive
   (progn
     (grep-compute-defaults)
     (let ((default (grep-default-command)))
       (list (read-shell-command "Run git gra (like this): "
                                 (if current-prefix-arg default
                                   "git --no-pager gra ")
                                 'git-grep-history
                                 (if current-prefix-arg nil default))))))
  (grep args))

;;; Common Lisp and Emacs Lisp

(defun my-blink ()
  "interactive version of blink-matching-open"
  (interactive)
  (save-excursion
    (goto-char (+ (point) 1))
    (blink-matching-open)))

(defun copy-sexp-as-kill (arg)
  "Save the sexp as if killed, but don't kill it"
  (interactive "p")
  (save-excursion
    (let ((opoint (point)))
      (forward-sexp arg)
      (copy-region-as-kill opoint (point)))))

;;; Need to set this before hyperspec.el is sucked in.
(setq common-lisp-hyperspec-root "file:///home/moore/lisp/HyperSpec/")

(setq common-lisp-hyperspec-symbol-table
      "/home/moore/lisp/HyperSpec/Data/Map_Sym.txt")

;(with-demoted-errors
;  (require 'slime))
;(slime-setup '(slime-repl))

(if (file-exists-p "~/quicklisp/slime-helper.el")
    (progn
      (load "~/quicklisp/slime-helper.el")))

(defun restore-slime-translations ()
  (setq slime-translate-from-lisp-filename-function
	'identity)
  (setq slime-translate-to-lisp-filename-function
	'identity))

(setq inferior-lisp-program
      "sbcl")

(defun sbcl ()
  (interactive)
  (restore-slime-translations)
  (setq slime-net-coding-system 'utf-8-unix)
  (slime))

(add-hook 'lisp-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (local-set-key "\r" 'newline-and-indent)
	    (local-set-key "\C-c\C-b" 'my-blink)
	    (setq lisp-indent-function 'common-lisp-indent-function)
	    (setq comment-column 40)
	    (set-fill-column 79)
	    (font-lock-mode)))

(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () inferior-slime-mode t))

(setq auto-mode-alist (cons '("\\.cl\\'" . lisp-mode) auto-mode-alist))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (local-set-key "\r" 'newline-and-indent)
	    (setq comment-column 40)
	    (set-fill-column 79)
	    (font-lock-mode)))

; Make -, ., * and _ letters.
(modify-syntax-entry ?- "w" lisp-mode-syntax-table)
(modify-syntax-entry ?. "w" lisp-mode-syntax-table)
(modify-syntax-entry ?* "w" lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" lisp-mode-syntax-table)

;;; Indent some things differently
(put 'collect
     'common-lisp-indent-function
     '((&whole 4 &rest (&whole 1 1 2)) &body))
(put 'once-only
     'common-lisp-indent-function
     '((&whole 4 &rest (&whole 1 1 2)) &body))
(put 'pseudo-atomic
     'common-lisp-indent-function
     0)
(put 'sc-case
     'common-lisp-indent-function
     '(4 &rest (&whole 2 &rest 1)))

(put 'defopen
     'common-lisp-indent-hook
     1)

(put 'defopenp
     'common-lisp-indent-hook
     3)

(put 'updating-output
     'common-lisp-indent-function
     '(&lambda &body))

(require 'tramp)

;;; Remote slime hackery
(defvar *my-box-tramp-path*
  "/ssh:moore@10.0.1.3:")
 
(defvar *current-tramp-path* nil)

(defun connect-to-host (path)
  (setq *current-tramp-path* path)
  (setq slime-translate-from-lisp-filename-function
    (lambda (f)
      (concat *current-tramp-path* f)))
  (setq slime-translate-to-lisp-filename-function
    (lambda (f)
      (substring f (length *current-tramp-path*))))
  (slime-connect "localhost" 4005))
 
(defun mac-slime ()
  (interactive)
  (connect-to-host *my-box-tramp-path*))
 
(defun mac-homedir ()
  (interactive)
  (find-file (concat *my-box-tramp-path* "/Users/moore/")))

(autoload 'maxima-mode "maxima" "Maxima editing mode" t)
(autoload 'maxima "maxima" "Running Maxima interactively" t)

(autoload 'imaxima "imaxima" "Maxima frontend" t)
(autoload 'imath "imath" "Interactive Math mode" t)


;;; AucTeX
;(with-demoted-errors
;  (require 'tex-site))

(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq-default TeX-master nil)

;;; Slide templates for Beamer

(require 'skeleton)

(define-skeleton my-slide-block
  "Prosper slide body"
  nil
  \n "\\begin{slide}"
  ?\{ (skeleton-read "{title}: ") & ?\} | -1
  > \n _ \n
  "\\end{slide}" > \n)

(define-skeleton my-frame-block
  "Beamer slide body"
  nil
  \n "\\begin{frame}"
  \n "\\frametitle" ?\{ (skeleton-read "{title}: ") & ?\} | -1
  > \n _ \n
  "\\end{frame}" > \n)

(setq tex-mode-hook
      '(lambda ()
	 (define-key latex-mode-map "\C-c\C-s" 'my-slide-block)
	 (auto-fill-mode 1)))
(setq latex-mode-hook tex-mode-hook)

(add-hook 'TeX-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)))

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)))

;;; mail-setup-hook from rlk's lecture, plus my own stuff
;;;
;;; I don't use RMAIL anymore, but this has been in my .emacs file since 1987!
;(defvar my-reply-to "moore@wolfenet.com")

(setq mail-setup-hook			;when sending mail
     '(lambda ()
	 (if to				;if reply, point is below seperator
	     (forward-line -1)
	   (forward-line 2))		;skip To: and Subject: 
	 (if (not cc) (insert "Cc: \n"))
	 (insert "Reply-to: " my-reply-to "\n")
	 (insert
	  "Full-Name: Timothy B. Moore\n")
	 ;; New rmail-reply doesn't insert "Re: ", but I like it.
	 (if (and in-reply-to subject (not (string-match "\\`Re: " subject)))
	     (progn
	       (goto-char (point-min))
	       (re-search-forward "^Subject: ")
	       (insert "Re: ")))
	 (if to			;if a reply move below headers
	     (goto-char (point-max))
	   (goto-char (point-min))	;else go to the To: line
	   (re-search-forward "^To: "))
	 (auto-fill-mode 1)
	 (local-set-key "\^c\^w" 'honig-signature)
	 (if running-lucid
	     (progn
	       (highlight-headers (point-min) (point-max) nil)))))
;;; bind "R" in rmail mode to reply to sender only.

(setq rmail-mode-hook
      '(lambda ()
	 (setq rmail-dont-reply-to-names "moore[%@]?\\|tim@morgan")
	 (setq rmail-ignored-headers
	       (concat "^[xX]-[^ ]*:\\|^precedence:\\|" rmail-ignored-headers))
	 (define-key rmail-mode-map "R" '(lambda ()
					   (interactive)
					   (rmail-reply t)))))

(setq rmail-enable-mime t)

(setq electric-command-history-hook
      '(lambda ()
	 (local-set-key "\C-s" 'isearch-forward)
	 (local-set-key "\C-r" 'isearch-backward)))

;;;
;;; some new key bindings
(define-key esc-map "G" 'goto-line)
(define-key esc-map "M" 'compile)
(define-key esc-map "s" 'spell-word)
(define-key esc-map "S" 'spell-buffer)
(define-key global-map "\^cw" 'copy-sexp-as-kill)

(global-set-key [(control c) (g) (s)] 'magit-status)
(global-set-key [(control c) (g) (b)] 'magit-blame-mode)
(global-set-key [(control c) (g) (a)] 'git-gra)
(global-set-key [(control c) (g) (g)] 'git-grep)


;;; map \^h to delete
(setq keyboard-translate-table "\0\1\2\3\4\5\6\7\177")
(define-key global-map "\M-?" 'help-command)
(define-key global-map "\M-?a" 'apropos)

;(setq grep-files-aliases (cons '("j" . "*.java") grep-files-aliases))

;;; The scheme program
(defvar scheme-program-name "guile")

;Sometimes you want to save the *Help* buffer for later examination,
;e.g., when you do an apropos.  save-help will rename the *Help* buffer
;*Help<1>*, *Help<2>*, etc., so the information won't get clobbered by
;further help requests.

;Dale

(defun save-help ()
  (interactive)
  (save-excursion
    (let ((i 1) 
	  (buffer (get-buffer "*Help*"))
	  name)
      (if (not buffer)
	  (ding)
	(while
	    (progn
	      (setq name (concat "*Help<" (int-to-string i) ">*"))
	      (get-buffer name))
	  (setq i (1+ i)))
	(set-buffer buffer)
	(rename-buffer name)
	(message (concat "Help buffer renamed " name))))))

(setq spell-filter
      '(lambda ()
	 (let ((end-of-orig (point-max)))
	   (call-process-region (point-min) end-of-orig "detex"
				t (current-buffer)))))

(global-set-key "\e\$" 'ispell-word)

(global-set-key "\C-x\e" 'electric-command-history)

;;; For broken terminal emulators...
(global-set-key "\^c " 'set-mark-command)

(defun my-new-screen (&optional screen-name)
  "Creates a new emacs screen with the same buffer as the current one."
  (interactive)
  (let ((buffer (current-buffer)))
    (select-screen (x-create-screen
		  (append (if screen-name
			      (list (cons 'name screen-name))
			    nil)
			  screen-default-alist)))
    (switch-to-buffer buffer)))

(global-set-key "\C-c5" 'my-new-screen)

(setq visible-bell t)

;;; org mode and remember
(defun template-valid-p (template)
  (let ((template-file (nth 3 template)))
    (if (or (null template-file)
	    (file-exists-p (nth 3 template)))
	template
      nil)))

(defvar my-remember-templates
  '(("Clipboard" ?c "* %T %^{Description}\n %x" nil "Interesting")
    ("Bookmark" ?b "* Bookmark %?\n %i\n %a" nil "Bookmark")
    ("Tasks" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" "~/organizer.org" "Tasks") ;; (2)
    ("Appointments" ?a "* Appointment: %?\n%^T\n%i\n  %a"
     "~/organizer.org")
    ("Book" ?b "** %^{Head Line} %^g\n%i%?"  "~/book/book.org" 'bottom)
    ("Ideas" ?i "* %?\n  %i" "~/solo/ideas.org")))

(require 'org)

(org-remember-insinuate)

(setq org-directory "~/orgfiles/")
(setq org-default-notes-file "~/orgfiles/notes.org")

(setq org-remember-templates
      (apply 'append 
	     (mapcar (lambda (x)
		       (if (template-valid-p x)
			   (list x)
			 nil))
		     my-remember-templates)))

(global-set-key (kbd "C-S-r") 'org-remember)
(global-set-key (kbd "C-c a") 'org-agenda)                       ;; (5)

(setq org-agenda-custom-commands
      '(("P" "Projects"   
         ((tags "PROJECT")))
        ("H" "Office and Home Lists"
         ((agenda)
          (tags-todo "AWAY")
          (tags-todo "HOME")
          (tags-todo "COMPUTER")
          (tags-todo "ERRAND")))
        ("D" "Daily Action List"
         ((agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0)))))))

;; from http://emacs-fu.blogspot.fr/2009/04/remember.html
;;
;; you might also want to set:
;;   (setq org-agenda-skip-unavailable-files t)
;; so these warnings won't annoy the little remember-frame
;; also: I have noted infrequent problems when using ElScreen
;;  (the wrong frame might be chosen for Remember in about 10% of the cases)

(defun make-remember-frame ()
  "turn the current frame into a small popup frame for remember mode;
this is meant to be called with 
     emacsclient -c -e '(djcb-remember-frame)'"
  (modify-frame-parameters nil
    '( (name . "*Remember*") ;; must be same as in mode-hook below  
       (width .  80)
       (height . 10)
       (vertical-scroll-bars . nil)
       (menu-bar-lines . nil)
       (tool-bar-lines . nil)))
  (org-remember)
  (when (fboundp 'x-focus-frame) (x-focus-frame nil)) ;; X only....

  (delete-other-windows)) 

;; when we're in such a remember-frame, close it when done.
(add-hook 'org-remember-mode-hook
  (lambda()
    (define-key org-remember-mode-map (kbd "C-c C-c")
      '(lambda()(interactive)
         (let ((remember-frame-p 
                 (string= (frame-parameter nil 'name) "*Remember*")))
           (when remember-frame-p (make-frame-invisible))  ;; hide quickly

           (org-remember-finalize)
           (when remember-frame-p (delete-frame)))))))

;; 'djcb-org-article' for export org documents to the LaTex 'article', using
;; XeTeX and some fancy fonts; requires XeTeX (see org-latex-to-pdf-process)
(require 'org-latex)
(add-to-list
 'org-export-latex-classes
 '("djcb-org-article" "\\documentclass[11pt,a4paper]{article}
\\usepackage{fontspec}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\defaultfontfeatures{Mapping=tex-text}
\\setromanfont[BoldFont={Gentium Basic Bold}]{Gentium}
\\setsansfont{Charis SIL}
\\setmonofont[Scale=0.8]{DejaVu Sans Mono}
\\usepackage{geometry}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\pagestyle{empty}
\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]" ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}") ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-latex-to-pdf-process 
  '("xelatex -interaction nonstopmode %f"
     "xelatex -interaction nonstopmode %f")) ;; for multiple passes
;;; fonts
;;(add-to-list 'default-frame-alist '(font "Inconsolata-12"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (whiteboard)))
 '(custom-safe-themes (quote ("60a65134827577812cab9974a7c368f8ad15746fb032ea4a39d0768eafb9e6e2" default)))
 '(ecb-options-version "2.40")
 '(org-agenda-files (quote ("~/solo/ideas.org" "~/solo/brico.org" "~/organizer.org")))
 '(org-agenda-ndays 7)
 '(org-agenda-repeating-timestamp-show-all nil)
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-sorting-strategy (quote ((agenda time-up priority-down tag-up) (todo tag-up))))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-todo-ignore-deadlines t)
 '(org-agenda-todo-ignore-scheduled t)
 '(org-agenda-todo-ignore-with-date t)
 '(org-agenda-window-setup (quote other-window))
 '(org-deadline-warning-days 7)
 '(org-fast-tag-selection-single-key nil)
 '(org-format-latex-header "\\documentclass{article}
\\usepackage{fullpage}         % do not remove
\\usepackage{amssymb}
\\usepackage[usenames]{color}
\\usepackage{amsmath}
\\usepackage{latexsym}
\\usepackage[mathscr]{eucal}
\\usepackage{lmodern}
\\pagestyle{empty}             % do not remove")
 '(org-log-done (quote (done)))
 '(org-refile-targets (quote (("organizer.org" :maxlevel . 1) ("someday.org" :level . 2))))
 '(org-reverse-note-order nil)
 '(org-tags-column -78)
 '(org-use-fast-todo-selection t)
 '(safe-local-variable-values (quote ((Base . 10) (Syntax . ANSI-Common-Lisp) (eval progn (c-set-offset (quote innamespace) (quote 0)) (c-set-offset (quote inline-open) (quote 0))) (package . user) (Package . JAPANESE-GRAPHICS-EDITOR) (TeX-master . "these") (Package . USER) (TeX-master . t) (Package . GOATEE) (Package . TOOL) (Package . ACL-CLIM) (Syntax . ANSI-Common-lisp) (Package . CLIM-DEFSYSTEM) (Package . CLIM-UTILS) (package . tk) \.\.\.)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 123 :width normal))))
 '(magit-item-highlight ((t nil))))
