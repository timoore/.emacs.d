;;; Tim Moore's .emacs file.

(when  (version< emacs-version "27")
  (load "~/.emacs.d/early-init.el")
  (package-initialize))

(require 'package)
(require 'cl-lib)

;;; Wide, tall frames
(setq default-frame-alist '((width . 110) (height . 50)))

;;; XEmacs
(defconst running-lucid (if (string-match "Lucid" (emacs-version)) t  nil))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)

(use-package helm
  :diminish helm-mode
  :init
  (progn
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
          helm-buffer-max-length 40)
    (helm-mode))
  :bind (("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c SPC" . helm-all-mark-rings)
         ("C-x C-f" . helm-find-files)))

;;; From https://tuhdo.github.io/helm-intro.html

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z


;;; general customizations

(if (file-readable-p
     "~/.emacs.d/emacs-color-theme-solarized/color-theme-solarized.el")
    (progn
      (add-to-list 'custom-theme-load-path
                   "~/.emacs.d/emacs-color-theme-solarized")
      (load-theme 'solarized t)))

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

;;; window and buffer navigation
(global-set-key (kbd "C-x C-b") (lambda () (interactive) (ibuffer t)))
(windmove-default-keybindings)

(require 'compile)

(setq real-compilation-last-buffer nil)
(setq compilation-last-buffer nil)

;;; From the emacs wiki
(global-set-key [(control c) (m)] 'compile-again)
(defun compile-again (pfx)
  "Run the same compile as the last time.

If there was no last time, or there is a prefix argument, this acts like
M-x compile."
  (interactive "p")
  (if (and (eq pfx 1)
           real-compilation-last-buffer)
      (progn
        (set-buffer real-compilation-last-buffer)
        (revert-buffer t t))
      (progn
        (call-interactively 'compile)
        (setq real-compilation-last-buffer compilation-last-buffer))))


;;; customization for magit

;;; Find git tools
(when nil
(progn
  (delete 'Git vc-handled-backends)
  (remove-hook 'find-file-hooks 'vc-find-file-hook)
  ;; TODO: handle cygwin
  (if (eq system-type 'windows-nt)
      (progn
        (setenv "PATH" (concat "C:\\Program Files (x86)\\Git\\bin;"
                               (getenv "PATH")))
        (push "c:/Program Files (x86)/Git/bin" exec-path))))
)
;;; Better way to do this; currently in .emacs
(when nil
  (setq shell-file-name "C:/Program Files/Git/bin/bash.exe")
  (defvar my-windows-path
    '("C:\\Program Files\\Git\\bin" "C:\\Program Files\\Git\\usr\\bin"))
  (setenv "PATH"
          (apply 'concat
                 `(,@(cl-mapcan (lambda (f) (list f ";")) my-windows-path) ,(getenv "PATH"))))
  (setq exec-path (append my-windows-path exec-path))
  )

(use-package magit)

(setq magit-define-global-key-bindings 'recommended)

(global-set-key [(control c) (g) (s)] 'magit-status)
(global-set-key [(control c) (g) (b)] 'magit-blame)
(global-set-key [(control c) (g) (a)] 'git-grep-toplevel)
(global-set-key [(control c) (g) (g)] 'vc-git-grep)

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

(use-package treesit-auto
  :init
  (setq treesit-auto-langs '(bash cmake yaml))
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; big old bug in treesit-auto
(let ((glsl-entry (rassoc 'glsl-ts-mode auto-mode-alist)))
  (if (and glsl-entry (null (car glsl-entry)))
      (rplaca glsl-entry "\\.glsl\\'")))


;;; From http://emacswiki.org/emacs/EmacsTags

(defun find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the default-directory.
looking for a file with name file-to-find.  Returns the path to it
or nil if not found."
  (cl-labels
      ((find-file-r (path)
                    (let* ((parent (file-name-directory path))
                           (possible-file (concat parent file-to-find))
                           (expansion (file-expand-wildcards possible-file)))
                      (cond
                       (expansion (car expansion)) ; Found
                       ;; The parent of ~ is nil and the parent of / is itself.
                       ;; Thus the terminating condition for not finding the file
                       ;; accounts for both.
                       ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
                       (t (find-file-r (directory-file-name parent))))))) ; Continue
    (find-file-r default-directory)))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (auto-fill-mode 1)
            (setq tab-width 4)
	    (local-set-key "\r" 'newline-and-indent)
	    (if (memq 'gtags features)
		(gtags-mode t))
            (if (fboundp 'eglot-insure)
                (eglot-ensure))
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
;;; Actually I don't like this anymore.
(setq my-c-offsets-alist '((namespace-open . 0)
                           (namespace-close . 0)
                           (innamespace . 0)
                           (inextern-lang . 0)))

;;; Stroustrup, with the namespace changes above and different inline open

(c-add-style "personal-c++"
             `("stroustrup"
               (c-offsets-alist
                (inline-open . 0)
                ,@my-c-offsets-alist)))

(c-add-style "my-gnu"
             `("gnu"
               (c-offsets-alist
                ,@my-c-offsets-alist)))

(c-add-style "my-linux"
             '("linux"
               (indent-tabs-mode t)))

;;; Open Inventor likes nested namespaces
(c-add-style "inventor"
             '("stroustrup"
               (c-basic-offset . 2)
               (c-offsets-alist
                (inline-open . 0))))

(c-add-style "unreal"
             '("personal-c++"
               (tab-width . 4)
               (indent-tabs-mode t)))

(c-add-style "oe"                       ;osgEarth
             '("stroustrup"
               (c-offsets-alist
                (inline-open . 0))))

(c-add-style "cesium-omniverse"
             '("oe"
               (c-basic-offset . 4)     ; Guessed value
               (c-offsets-alist
                (arglist-cont . 0)      ; Guessed value
                (arglist-intro . +)     ; Guessed value
                (block-close . 0)       ; Guessed value
                (defun-block-intro . +) ; Guessed value
                (defun-close . 0)       ; Guessed value
                (inline-close . 0)      ; Guessed value
                (innamespace . 0)       ; Guessed value
                (member-init-cont . -2)  
                (member-init-intro . +) ; Guessed value
                (namespace-close . 0)   ; Guessed value
                (statement . 0)         ; Guessed value
                (statement-block-intro . +) ; Guessed value
                (statement-cont . +)        ; Guessed value
                (topmost-intro . 0)         ; Guessed value
                (topmost-intro-cont . 0) ; Guessed value
                (access-label . -)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (arglist-close . c-lineup-close-paren)
                (arglist-cont-nonempty . c-lineup-arglist)
                (block-open . 0)
                (brace-entry-open . 0)
                (brace-list-close . 0)
                (brace-list-entry . 0)
                (brace-list-intro first c-lineup-2nd-brace-entry-in-arglist c-lineup-class-decl-init-+ +)
                (brace-list-open . 0)
                (c . c-lineup-C-comments)
                (case-label . 0)
                (catch-clause . 0)
                (class-close . 0)
                (class-open . 0)
                (comment-intro . c-lineup-comment)
                (composition-close . 0)
                (composition-open . 0)
                (cpp-define-intro c-lineup-cpp-define +)
                (cpp-macro . -1000)
                (cpp-macro-cont . +)
                (defun-open . 0)
                (do-while-closure . 0)
                (else-clause . 0)
                (extern-lang-close . 0)
                (extern-lang-open . 0)
                (friend . 0)
                (func-decl-cont . +)
                (inclass . +)
                (incomposition . +)
                (inexpr-class . +)
                (inexpr-statement . +)
                (inextern-lang . +)
                (inher-cont . c-lineup-multi-inher)
                (inher-intro . +)
                (inlambda . 0)
                (inline-open . 0)
                (inmodule . +)
                (knr-argdecl . 0)
                (knr-argdecl-intro . +)
                (label . 0)
                (lambda-intro-cont . +)
                (module-close . 0)
                (module-open . 0)
                (namespace-open . 0)
                (objc-method-args-cont . c-lineup-ObjC-method-args)
                (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
                (objc-method-intro .
                                   [0])
                (statement-case-intro . +)
                (statement-case-open . 0)
                (stream-op . c-lineup-streamop)
                (string . -1000)
                (substatement . +)
                (substatement-label . 0)
                (substatement-open . 0)
                (template-args-cont c-lineup-template-args +))))

(defun llvm-lineup-statement (langelem)
  (let ((in-assign (c-lineup-assignments langelem)))
    (if (not in-assign)
        '++
      (aset in-assign 0
            (+ (aref in-assign 0)
               (* 2 c-basic-offset)))
      in-assign)))

;; Add a cc-mode style for editing LLVM C and C++ code. Used by most Cesium code
(c-add-style "llvm.org"
             '("gnu"
	       (fill-column . 80)
	       (c++-indent-level . 2)
	       (c-basic-offset . 2)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((arglist-intro . ++)
				   (innamespace . 0)
				   (member-init-intro . ++)
				   (statement-cont . llvm-lineup-statement)))))
(setq my-c++-styles-alist
      '(("cesium-native" . "llvm.org")
        ("cesium-unreal" . "llvm.org")
        ("*.uplugin" . "unreal")
        ("UE4Games.uprojectdirs" . "unreal")
        ("OIVHOME" . "inventor")
        ("osgearth" . "oe")
        ("CesiumGS" . "cesium-omniverse")
        (nil . "oe")))

(setq c-noise-macro-names '("OSGEARTH_EXPORT" "VSG_DECLSPEC"))
(c-make-noise-macro-regexps)

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
;;; set JSON indentation to match everybody's...
(setq js-indent-level 2)

(defun git-grep-toplevel (regexp &optional files)
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep"
				   nil nil 'grep-history)))
      (t (let* ((regexp (grep-read-regexp))
		(files
                 (mapconcat #'shell-quote-argument
                            (split-string (grep-read-files regexp)) " ")))
	   (list regexp files))))))
  (vc-git-grep regexp files (magit-toplevel)))

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
      (load "~/quicklisp/slime-helper.el")
      (add-to-list 'slime-contribs 'slime-asdf)))

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
	    (set-fill-column 100)
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

;;; Alternate fonts for imaxima
(setq imaxima-latex-preamble "\\usepackage{libertine} \\usepackage[libertine]{newtxmath}")
(add-hook 'imaxima-startup-hook
          (lambda ()
            (let ((b (get-buffer "*imaxima*"))
                  (p (get-process "imaxima")))
              (if (and b p)
                  (apply comint-input-sender
                         (list (get-process "imaxima")
                               "load(\"mactex-utilities\");"))))))
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

;;; julia fun
(add-hook 'julia-mode-hook
          (lambda ()
            (set-fill-column 100)
            (local-set-key (kbd "TAB") 'julia-latexsub-or-indent)
            (auto-fill-mode 1)))

(unless (eq system-type 'windows-nt)
  (use-package vterm
    :ensure t)

  (use-package julia-snail
    :ensure t
    :requires vterm
    :hook (julia-mode . julia-snail-mode)))

(global-set-key "\C-c\C-z" 'julia-snail)

(use-package yaml-pro
  :ensure t)

;;; browse-apropos-url from emacswiki

(setq apropos-url-alist
      '(("^gw?:? +\\(.*\\)" . ;; Google Web
         "http://www.google.com/search?q=\\1")

        ("^gl:? +\\(.*\\)" .  ;; Google Linux
         "http://www.google.com/linux?q=\\1")

        ("^gi:? +\\(.*\\)" . ;; Google Images
         "http://images.google.com/images?sa=N&tab=wi&q=\\1")


        ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(\\w+://.*\\)" . ;; Google Translate URL
         "http://translate.google.com/translate?langpair=\\1|\\2&u=\\3")

        ("^gt:? +\\(\\w+\\)|? *\\(\\w+\\) +\\(.*\\)" . ;; Google Translate Text
         "http://translate.google.com/translate_t?langpair=\\1|\\2&text=\\3")

        ("^ewiki:? +\\(.*\\)" . ;; Emacs Wiki Search
         "http://www.emacswiki.org/cgi-bin/wiki?search=\\1")

        ("^ewiki$" . ;; Emacs Wiki
         "http://www.emacswiki.org")

        ("^arda$" . ;; The Encyclopedia of Arda
         "http://www.glyphweb.com/arda/")
        ))

;; Don't know if it's the best way , but it seemed to work. (Requires emacs >= 20)
(defun browse-apropos-url (text &optional new-window)
  (interactive (browse-url-interactive-arg "Location: "))
  (let ((text (replace-regexp-in-string
               "^ *\\| *$" ""
               (replace-regexp-in-string "[ \t\n]+" " " text))))
    (let ((url (assoc-default
                text apropos-url-alist
                '(lambda (a b) (let () (setq __braplast a) (string-match a b)))
                text)))
      (browse-url (replace-regexp-in-string __braplast url text) new-window))))

;;; using keyword-search to do the same thing
(autoload 'keyword-search-get-query "keyword-search")

(defun google-search (query &optional new-window)
  "keyword-search that defaults to google."
  (interactive
   (list (let ((thing (keyword-search-get-query)))
           (read-string
	    (if thing
		(format "google (%s): " thing)
	      "google: ")
	    nil nil thing))))
  (keyword-search 'google query))

(global-set-key (kbd "C-c k") 'google-search)
(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(julia-snail vterm magit helm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
