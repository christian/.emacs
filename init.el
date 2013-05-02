;;
;; PACKAGES
;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)


;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; yasnippets
(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/vendor/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/vendor/yasnippets-rails/rails-snippets")
(yas/load-directory "~/.emacs.d/vendor/yasnippets-rspec/rspec-snippets")

;; browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

;; copy paste should work with other apps
(setq x-select-enable-clipboard t)

;; encryption of org files
(require 'epa-file)
(epa-file-enable)

;; save recently opened files
(require 'recentf)
(recentf-mode 1)
(global-set-key "\C-x\ r" 'recentf-open-files)


;; terminal background color
(setq term-default-bg-color "#211E1E")

;; font
(set-default-font "Bitstream Vera Sans Mono-9")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 90 :family "Bitstream Vera Sans Mono" :embolden t))))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) (:background "gray20")))))

;; autocomplete from buffers
;(add-to-list 'load-path "~/.emacs.d/")
;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "/Users/cristi/.emacs.d/vendor/ac-dict")
;(ac-config-default)

;; Prevent Emacs from making backup files
(setq make-backup-files nil)

;; stop creating those #autosave# files
(setq auto-save-default nil)

;; delete should act like DEL not like BACKSPACE
(normal-erase-is-backspace-mode 1)

;; Before save hook: delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; bar cursor
(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'bar-cursor)
(bar-cursor-mode 1)

(setq isearch-allow-scroll t)

;; show matching parenthesis
(show-paren-mode 1)
(set-face-background 'show-paren-match-face (face-background 'default))
(set-face-foreground 'show-paren-match-face "#ff6600")

;; highlight current line
(global-hl-line-mode 1)

;; ruby electric play nicely with yasnippet
(defun yas/advise-indent-function (function-symbol)
  (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
           ,(format
             "Try to expand a snippet before point, then call `%s' as usual"
             function-symbol)
           (let ((yas/fallback-behavior nil))
             (unless (and (interactive-p)
                          (yas/expand))
               ad-do-it)))))

(yas/advise-indent-function 'ruby-indent-line)


;; restore emacs after shutdown
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(custom-enabled-themes (quote (dichromacy)))
 '(custom-safe-themes (quote ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(desktop-enable t nil (desktop))
 '(desktop-save-mode t nil (desktop))
 '(fci-rule-color "#efefef")
 '(js2-basic-offset 2)
 '(org-agenda-files (quote ("~/org/agenda.org")))
 '(save-place t nil (saveplace))
 '(speedbar-mode-specific-contents-flag t)
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images nil))



;; autoindent when paste
(dolist (command '(yank yank-pop))
       (eval `(defadvice ,command (after indent-region activate)
                (and (not current-prefix-arg)
                     (member major-mode '(emacs-lisp-mode lisp-mode
                                                          clojure-mode    scheme-mode
                                                          haskell-mode    ruby-mode
                                                          rspec-mode      python-mode
                                                          c-mode          c++-mode
                                                          objc-mode       latex-mode
                                                          plain-tex-mode  css-mode
                                                          scala-mode
                                                          js2-mode
                                                          scss-mode))
                     (let ((mark-even-if-inactive transient-mark-mode))
                       (indent-region (region-beginning) (region-end) nil))))))

;; when deleting at the end of the line; don't consider indentation on
;; next line
(defun kill-and-join-forward (&optional arg)
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1)
             (kill-line arg))
    (kill-line arg)))

;; auto revert mode for files
(global-auto-revert-mode)

;; Ruby Deep Indent Paren
(setq ruby-deep-indent-paren nil)

;; replace when pasting
(delete-selection-mode 1)

;; Rinari
(add-to-list 'load-path "~/.emacs.d/vendor/rinari")
(require 'rinari)

;; exuberant tags
(setq rinari-tags-file-name "TAGS")


;; C-a toggle bignning of line / beginning of code line
(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))


;; close tag nxhtml
(defun msh-close-tag ()
  "Close the previously defined XML tag"
  (interactive)
  (let ((tag nil)
        (quote nil))
    (save-excursion
      (do ((skip 1))
          ((= 0 skip))
        (re-search-backward "</?[a-zA-Z0-9_-]+")
        (cond ((looking-at "</")
               (setq skip (+ skip 1)))
              ((not (looking-at "<[a-zA-Z0-9_-]+[^>]*?/>"))
               (setq skip (- skip 1)))))
      (when (looking-at "<\\([a-zA-Z0-9_-]+\\)")
        (setq tag (match-string 1)))
      (if (eq (get-text-property (point) 'face)
              'font-lock-string-face)
          (setq quote t)))
    (when tag
      (setq quote (and quote
                       (not (eq (get-text-property (- (point) 1) 'face)
                                'font-lock-string-face))))
      (if quote
          (insert "\""))
      (insert "</" tag ">")
      (if quote
          (insert "\"")))))


;; make json beautiful
;(defun beautify-json ()
;  (interactive)
;  (let ((b (if mark-active (min (point) (mark)) (point-min)))
;        (e (if mark-active (max (point) (mark)) (point-max))))
;    (shell-command-on-region b e
;     "python -mjson.tool" (current-buffer) t)))
;(define-key json-mode-map (kbd "C-c C-f") 'beautify-json)



;; shift block to left and right by a certain number of positions
(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))
(defun shift-right (count)
  (interactive "p")
  (shift-text count))
(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))


;; cursor should not blink
(blink-cursor-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; no splash screen. disable splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)


;; CUA mode
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted

;; org mode faces
(setq org-todo-keyword-faces
      '(
        ("INPR" . (:foreground "yellow" :weight bold))
        ))
;; source code in .org files
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . nil)
;;    (java . t)))
(setq org-log-done 'time) ;; show time stamp of DONE notes
;; MobileOrg
;; Set to the location of your Org files on your local system
(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/todos.org")
;(setq org-mobile-inbox-for-pull "~/org/kooaba.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox-personal/Dropbox/MobileOrg")


;; disable scrolling
(scroll-bar-mode -1)

;; shut up!
(setq visible-bell t)

;; To cause TAB characters to not be used in the file for compression, and for only spaces to be used, do this:
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;; Now some people find the flashing annoying. To turn the alarm totally off, you can use this:
(setq ring-bell-function 'ignore)

;; julia mode
;; (require 'julia-mode "~/.emacs.d/vendor/julia-mode.el")

;; markdown mode
(add-to-list 'load-path "~/.emacs.d/vendor/markdown-mode")
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; dot mode
(load-file "~/.emacs.d/vendor/dot/graphviz-dot-mode.el")

;; d mode
;; (add-to-list 'load-path "~/.emacs.d/vendor")
;; (autoload 'd-mode "d-mode.el"
;;   "Major mode for editing Markdown files" t)
;; (setq auto-mode-alist
;;       (cons '("\\.d" . d-mode) auto-mode-alist))


(setq load-path (cons (expand-file-name "~/.emacs.d/vendor") load-path))
(require 'cmake-mode)
(setq auto-mode-alist
       (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                 ("\\.cmake\\'" . cmake-mode))
               auto-mode-alist))



;; scala mode
(add-to-list 'load-path "~/.emacs.d/vendor/scala-emacs")
;; load the ensime lisp code...
(add-to-list 'load-path "~/.emacs.d/vendor/ensime_2.9.2-RC1-0.9.3.RC4/elisp/")
(require 'ensime)
(require 'scala-mode-auto)
(add-hook 'scala-mode-hook
   '(lambda ()
       (scala-mode-feature-electric-mode)
       (yas/minor-mode-on)
       (ensime-scala-mode-hook)
))
(when (and (require 'scala-mode-auto nil 'noerror) (require 'ensime nil 'noerror))
  (add-to-list 'auto-mode-alist '("\\.scala.html$" . scala-mode))
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))


;; show colors in css files
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(defun css-custom ()
  "css-mode-hook"
  (rainbow-mode))
(add-hook 'css-mode-hook
	  '(lambda()
             (css-custom)
             ;(auto-complete-mode)
))


;; highlight FIXME, TODO, BUG
(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)
; (require 'myfixme)
; (myfixme-mode 1)

;; iedit
(require 'iedit)
(global-set-key (kbd "C-:") 'iedit-mode)


;; Start in server mode
(server-start)

;;
;; KEY BINDINGS
;;

;; Fs
(global-set-key [f1] 'cd)
(global-set-key [f2] 'goto-line)
(global-set-key [f4] 'ack)
(global-set-key [f5] 'query-replace)
(global-set-key [f6] 'occur)
(global-set-key [f11] 'toggle-fullscreen)
(global-set-key [f12] 'menu-bar-mode)

;; indent region
(global-set-key "\M-i" 'indent-region)

;; undo / redo
(define-key global-map (kbd "C-/") 'undo)
(define-key global-map (kbd "C-x C-/") 'redo)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-=") 'dabbrev-expand)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key "\C-xO" (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key "\C-x\C-o" (lambda () (interactive) (other-window 2))) ;; forward two

;; delete work backwards
(global-set-key "\C-\M-h" 'backward-kill-word)

;; shift block left / right
(global-set-key "\M-[" 'shift-left)
(global-set-key "\M-]" 'shift-right)

;; toggle beginning of line / beginning of code line
(global-set-key [C-a] 'beginning-of-line-or-indentation)

;; when hitting escape
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; enlarging and shrinking windows
(global-set-key [C-S-right] 'enlarge-window-horizontally)
(global-set-key [C-S-left] 'shrink-window-horizontally)
(global-set-key [C-S-down] 'enlarge-window)

;; comment or uncomment
(defun comment-or-uncomment-region-or-line ()
  "Like comment-or-uncomment-region, but if there's no mark \(that means no
region\) apply comment-or-uncomment to the current line"
  (interactive)
  (if (not mark-active)
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (if (< (point) (mark))
        (comment-or-uncomment-region (point) (mark))
      (comment-or-uncomment-region (mark) (point)))))
(global-set-key "\M-/" 'comment-or-uncomment-region-or-line)

;; delete blank lines
(global-set-key [C-S-d] 'delete-blank-lines)

;; no # -*- coding: utf-8 -*- on save
(setq ruby-insert-encoding-magic-comment nil)


;; move mouse to the newly created window after splitting
(defadvice split-window-horizontally (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'split-window-horizontally)
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

;; close xml tag
(global-set-key [C-c-/] 'msh-close-tag)

;; Close all buffers
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key "\C-cx" 'close-all-buffers)

;; create directories if they don't exist
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))


;; (require 'adwaita) ; do M-x load-theme



;;
;; MODES
;;

;; coffe script mode https://github.com/defunkt/coffee-mode
(add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook
	  '(lambda() (coffee-custom)))


;; org-mode
(setq load-path (cons "/usr/local/share/emacs/site-lisp" load-path))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; ruby electric
(add-hook 'ruby-mode-hook
    (lambda()
        (add-to-list 'load-path "~/.emacs.d/vendor/ruby-electric")
        (require 'ruby-electric)
        (define-key ruby-mode-map "\C-m" 'newline-and-indent)
        (ruby-electric-mode t) ))

;; rspec-mode
(add-to-list 'load-path "~/.emacs.d/vendor/rspec")
(require 'rspec-mode)

;; scss-mode
(setq css-indent-offset 2)
(add-to-list 'load-path "~/.emacs.d/vendor/scss")
(require 'scss-mode)

;; Erlang-mode
(setq load-path (cons "~/.emacs.d/vendor/erlware-mode" load-path))
(setq erlang-root-dir "/opt/local/lib/erlang")
(setq exec-path (cons "/opt/local/lib/erlang/bin" exec-path))
(require 'erlang-start)

;; Haskell-mode
(load "~/.emacs.d/vendor/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; MuMaMo-Mode for rhtml files
(load "~/.emacs.d/vendor/nxhtml/autostart")
(add-to-list 'load-path "~/.emacs.d/vendor/nxhtml/util/")
(require 'mumamo-fun)
(setq mumamo-chunk-coloring 1)
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . html-mumamo))
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . html-mumamo))
(add-to-list 'auto-mode-alist '("\\.jst\\.eco\\'" . html-mumamo))
(add-to-list 'auto-mode-alist '("\\.js\\.erb\\'" . javascript-mumamo))
(add-to-list 'auto-mode-alist '("\\.js\\.rjs\\'" . javascript-mumamo))
(add-to-list 'auto-mode-alist '("\\.scala\\.html\\'" . html-mumamo)) ; TODO must fix; it's scala not ruby
;; Mumamo is making emacs 23.3 freak out:
(when (and (equal emacs-major-version 23)
           (equal emacs-minor-version 3))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function))
  ;; tramp-compat.el clobbers this variable!
  (eval-after-load "tramp-compat"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-beginning-of-syntax-function)))
;; (add-to-list 'load-path "~/.emacs.d/vendor/")
;; (require 'nanoc-mumamo)

;; js2-mode for javascripts
(add-to-list 'load-path "~/.emacs.d/vendor/js2")
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-consistent-level-indent-inner-bracket-p t)
(setq js2-pretty-multiline-decl-indentation-p t)
(put 'narrow-to-region 'disabled nil)

;; yaml-mode
(add-to-list 'load-path "~/.emacs.d/vendor/yaml")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
  '(lambda ()
     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; show file name
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
(global-set-key [C-f1] 'show-file-name) ; Or any other key you want


;; scratch buffer for any mode
(require 'scratch)
(global-set-key (kbd "C-c t") 'scratch)


(setq load-path (append (list (expand-file-name "~/.emacs.d/vendor/lilypond")) load-path))

;; (add-to-list 'load-path "~/.emacs.d/vendor/lilypond")
;; (require 'lilypond-mode)
(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))



;; format json
(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)
    )
  )

;; format xml
(defun xml-format (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))


(setq
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)

;; SET debugging. Always at the end in order to override everything.
;; (setq debug-on-error t)
(setq debug-on-error nil)
