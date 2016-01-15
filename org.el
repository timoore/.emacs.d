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

(with-demoted-errors
    (org-remember-insinuate))

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
(with-demoted-errors
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
      [NO-PACKAGES]" ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}") ("\\subsubsection{%s}" . "\\subsubsection*{%s}") ("\\paragraph{%s}" . "\\paragraph*{%s}") ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-latex-to-pdf-process 
  '("xelatex -interaction nonstopmode %f"
     "xelatex -interaction nonstopmode %f")) ;; for multiple passes

(setq org-agenda-files (quote ("~/solo/ideas.org" "~/solo/brico.org" "~/organizer.org")))
(setq org-agenda-ndays 7)
(setq org-agenda-repeating-timestamp-show-all nil)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-show-all-dates t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-sorting-strategy (quote ((agenda time-up priority-down tag-up) (todo tag-up))))
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-todo-ignore-deadlines t)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-with-date t)
(setq org-agenda-window-setup (quote other-window))
(setq org-deadline-warning-days 7)
(setq org-fast-tag-selection-single-key nil)
(setq org-format-latex-header "\\documentclass{article}
\\usepackage{fullpage}         % do not remove
\\usepackage{amssymb}
\\usepackage[usenames]{color}
\\usepackage{amsmath}
\\usepackage{latexsym}
\\usepackage[mathscr]{eucal}
\\usepackage{lmodern}
\\pagestyle{empty}             % do not remove")
(setq org-log-done (quote (done)))
(setq org-refile-targets (quote (("organizer.org" :maxlevel . 1) ("someday.org" :level . 2))))
(setq org-reverse-note-order nil)
(setq org-tags-column -78)
(setq org-use-fast-todo-selection t)

(setq org-publish-project-alist
      '(("org-sicm"
        :base-directory "~/solo/blog/lets-read"
        :publishing-directory "~/solo/blog/lets-read/output"
        :publishing-function org-publish-org-to-html
        :headline-levels 6
        :html-extension "html"
        :body-only t
        :section-numbers nil
        :table-of-contents nil)))

(require 'org-crypt)
;;; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;;; GPG key to use for encryption
(setq org-crypt-key "E5A885EA")
(setq org-crypt-disable-auto-save 'encrypt)
