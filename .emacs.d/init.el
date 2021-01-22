(require 'package)
(require 'cl)
(add-to-list 'package-archives
       '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defun is-gui()
  (display-graphic-p))

(defmacro when-gui(&rest body)
  `(when (is-gui) ,@body))

(defun is-term()
  (not (display-graphic-p)))

(defmacro when-term(&rest body)
  `(when (is-term),@body))

;get rid of all distractions. No fear baby
(setq inhibit-startup-message t)
;emacs bugs up if you fullscreen immediately

(run-with-idle-timer 0.1 nil 'toggle-frame-maximized)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq default-directory "~/")

(add-to-list 'default-frame-alist
             '(font . "Fira Mono-11"))
;; consider font size 12 for ubuntu

;consolidate all auto-save files and backups
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(gdb-many-windows t)
 '(git-commit-style-convention-checks (quote (non-empty-second-line overlong-summary-line)))
 '(git-commit-summary-max-length 50)
 '(package-selected-packages
   (quote
    (ace-window magit helm minimap docker-compose-mode docker clojure-mode-extra-font-locking gruvbox-theme zenburn-theme blacken py-autopep8 flymake-python-pyflakes flycheck-pyflakes elpy rainbow-delimiters cider flycheck-clojure clojure-mode boogie-friends iy-go-to-char fill-column-indicator auto-complete web-mode jinja2-mode geiser racket-mode smart-tab paredit parinfer ace-jump-mode w3m slime flycheck ample-theme))))

;if necessary, create the directory for autosaving
(make-directory "~/.emacs.d/autosaves/" t)

(setq frame-title-format "emacs ~ %b")
(fset 'yes-or-no-p 'y-or-n-p)
(global-linum-mode 1)
;give spacing between line number and text in terminal
(when-term
 (setq linum-format "%d "))
(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t)
;(setq ido-everywhere t)
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq next-line-add-newlines t)
(setq-default indent-tabs-mode nil)

(dolist (command '(yank yank-pop))
   (eval `(defadvice ,command (after indent-region activate)
            (and (not current-prefix-arg)
                 (member major-mode '(emacs-lisp-mode lisp-mode
                                                      clojure-mode    scheme-mode
                                                      haskell-mode    ruby-mode
                                                      rspec-mode      python-mode
                                                      c-mode          c++-mode
                                                      objc-mode       latex-mode
                                                      plain-tex-mode))
                 (let ((mark-even-if-inactive transient-mark-mode))
                   (indent-region (region-beginning) (region-end) nil))))))

(when-gui
 (load-theme 'gruvbox-dark-soft t)
 (enable-theme 'gruvbox-dark-soft))
;(load-theme 'ample-flat t t)
;(load-theme 'ample-light t t)


;;;80 Column indicator;;;
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))

(add-hook 'prog-mode-hook #'whitespace-mode)

;;;LISP settings;;;
;;parinfer
(use-package parinfer
    :ensure t
    :bind
    (("C-," . parinfer-toggle-mode))
    :init
    (progn
      (setq parinfer-extensions
            '(defaults       ; should be included.
              pretty-parens  ; different paren styles for different modes.
              paredit        ; Introduce some paredit commands.
              smart-tab))      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;      (setq parinfer-auto-switch-indent-mode t) ;; automatically switch to indent-mode
;     (setq parinfer-auto-switch-indent-mode-when-closing t) ;; likewise as above
      (add-hook 'clojure-mode-hook #'parinfer-mode)
      (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
      (add-hook 'common-lisp-mode-hook #'parinfer-mode)
      (add-hook 'scheme-mode-hook #'parinfer-mode)
      (add-hook 'lisp-mode-hook #'parinfer-mode)))

(when-gui
 (load (expand-file-name  "~/quicklisp/slime-helper.el"))
 (setq inferior-lisp-program "/bin/sbcl")
 (setq slime-contribs '(slime-fancy))
 (setq slime-protocol-version 'ignore)
 (add-hook 'slime-repl-mode-hook #'delete-other-windows))
; (slime))

;;geiser for scheme
(setq geiser-active-implementations '(racket))
(setq geiser-repl-use-other-window nil)
(setq geiser-mode-start-repl-p t)

;;;Python settings;;;
(elpy-enable)
(pyenv-mode)

(pyvenv-activate "~/anaconda3/")
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; Enable Flycheck

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8

(require 'py-autopep8)

;disable autopep8
;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(add-hook 'python-mode-hook '(lambda ()
                              (setq python-indent 4)))

;; Configure to wait a bit longer after edits before starting
(setq-default flymake-no-changes-timeout '3)

;; Set as a minor mode for Python
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))

;; To avoid having to mouse hover for the error message, these functions make flymake error messages
;; appear in the minibuffer
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (require 'cl)
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
          (let ((err (car (second elem))))
            (message "%s" (flymake-ler-text err)))))))

(add-hook 'post-command-hook 'show-fly-err-at-point)

;; toggle python buffers
(defvar python-last-buffer nil
  "Name of the Python buffer that last invoked `toggle-between-python-buffers'")

(make-variable-buffer-local 'python-last-buffer)

(defun toggle-between-python-buffers ()
  "Toggles between a `python-mode' buffer and its inferior Python process

When invoked from a `python-mode' buffer it will switch the
active buffer to its associated Python process. If the command is
invoked from a Python process, it will switch back to the `python-mode' buffer."
  (interactive)
  ;; check if `major-mode' is `python-mode' and if it is, we check if
  ;; the process referenced in `python-buffer' is running
  (if (and (eq major-mode 'python-mode)
           (processp (get-buffer-process python-buffer)))
      (progn
        ;; store a reference to the current *other* buffer; relying
        ;; on `other-buffer' alone wouldn't be wise as it would never work
        ;; if a user were to switch away from the inferior Python
        ;; process to a buffer that isn't our current one. 
        (switch-to-buffer python-buffer)
        (setq python-last-buffer (other-buffer)))
    ;; switch back to the last `python-mode' buffer, but only if it
    ;; still exists.
    (when (eq major-mode 'inferior-python-mode)
      (if (buffer-live-p python-last-buffer)
          (switch-to-buffer python-last-buffer)
        ;; buffer's dead; clear the variable.
        (setq python-last-buffer nil)))))

(defun switch-to-python-shell ()
  (interactive)
  (setq python-last-buffer (current-buffer))
  (elpy-shell-switch-to-shell))

(defun switch-to-python-buffer ()
  (interactive)
  (switch-to-buffer python-last-buffer))

(define-key inferior-python-mode-map (kbd "<f9>") 'switch-to-python-buffer)
(define-key python-mode-map (kbd "<f9>") 'switch-to-python-shell)

(define-key inferior-python-mode-map (kbd "<f5>") 'elpy-shell-send-statement-and-step)
(define-key python-mode-map (kbd "<f5>") 'elpy-shell-send-statement-and-step)


;;;C settings;;;
;;set default indentation to 4 spaces
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))
(setq-default c-basic-offset 4)
;; Run C programs directly from within emacs

(setq async-shell-command-display-buffer nil)

(defun execute-c-program ()
  (interactive)
  (gcc-program :cpp nil))

(defun execute-cpp-program ()
  (interactive)
  (gcc-program :cpp t))

(defun compile-and-run-gcc (&key cpp)
  (save-buffer)
  (delete-other-windows)
  (split-window-right)
  (other-window 2)
  (let* ((sans-extension (file-name-sans-extension buffer-file-name))
         (compiler (if cpp "g++" "gcc -std=c11"))
         (foo (if (file-exists-p "Makefile")
                  (concat "make && " sans-extension)
                (concat compiler " -g -Wall -o "
                        sans-extension " " buffer-file-name
                        " && " sans-extension))))
    (async-shell-command foo)))

(defun compile-gcc (&key cpp)
  (save-buffer)
  (let* ((sans-extension (file-name-sans-extension buffer-file-name))
         (compiler (if cpp "g++" "gcc -std=c11"))
         (foo (if (file-exists-p "Makefile")
                  "make"
                (concat compiler " -g -Wall -o "
                        sans-extension " " buffer-file-name))))
    (async-shell-command foo)))

 
(defun gcc-program (&key cpp)
  "Dispatch compiler for 'C' or 'C++'."
  (if (one-window-p)
      (compile-and-run-gcc :cpp cpp)
    (compile-gcc :cpp cpp)))

(defun my-c-initialization-hook ()
  (local-set-key (kbd "C-c C-c") 'execute-c-program))
;;c-initialization-hook evaluated when c major mode first starts
(add-hook 'c-mode-hook 'my-c-initialization-hook)

(defun my-cpp-initialization-hook ()
  (local-set-key (kbd "C-c C-c") 'execute-cpp-program))
(add-hook 'c++-mode-hook 'my-cpp-initialization-hook)

;; function decides whether .h file is C or C++ header, sets C++ by
;; default because there's more chance of there being a .h without a
;; .cc than a .h without a .c (ie. for C++ template files)
(defun c-c++-header ()
  "Set either 'c-mode' or 'c++-mode', whichever is appropriate for header."
  (interactive)
  (let ((c-file (concat (substring (buffer-file-name) 0 -1) "c")))
    (if (file-exists-p c-file)
        (c-mode)
      (c++-mode))))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-c++-header))

; j2s-mode for javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;(add-hook 'web-mode-hook 'js2-minor-mode)

;;change default browser for 'browse-url'  to w3m
(setq browse-url-browser-function 'w3m-goto-url-new-session)

;;change w3m user-agent to android
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")

;;when I want to enter the web address all by hand
(defun w3m-open-site (site)
  "Opens site in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address(default: w3m-home):" nil nil w3m-home-page nil)))
  (w3m-goto-url-new-session
   (concat "http://" site)))

(setq w3m-default-display-inline-images t)

;;
;; tramp settings
;;
(require 'tramp)
(setq tramp-default-method "ssh")

;; Many thanks go to Andy Sloane at https://www.a1k0n.net for the following tramp settings (with my own edits, of course)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

(setq lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

(defvar *my-box-tramp-path*
  "/ssh:desmond@desmondcheong.com:")

(defvar *current-tramp-path* nil)
(defun connect-to-host (path)
  (setq *current-tramp-path* path)
  (setq slime-translate-from-lisp-filename-function
        (lambda (f)
          (concat *current-tramp-path* f)))
  (setq slime-translate-to-lisp-filename-function
        (lambda (f)
          (substring f (length *current-tramp-path*))))
  ;establish shh tunnel with server to connect slime
  (shell-command "ssh -f -N slime-tunnel")
  (slime-connect "localhost" 4005))

(defun my-slime ()
  (interactive)
  (connect-to-host *my-box-tramp-path*))

(defun my-box ()
  (interactive)
  (find-file *my-box-tramp-path*))

;;
;; ace jump mode major function
;; 
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;;only use lower case characters
(setq ace-jump-mode-move-keys
      (loop for i from ?a to ?z collect i))
;; you can select the key you prefer to
(define-key global-map (kbd "C-c C-f") 'ace-jump-mode)
(define-key global-map (kbd "C-c C-l") 'ace-jump-line-mode)


;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-c C-b") 'ace-jump-mode-pop-mark)

;;magit settings
(global-set-key (kbd "C-x g") 'magit-status)
;;(add-hook 'after-save-hook 'magit-after-save-refresh-status t)


;; matlab-mode
(add-to-list 'load-path "~/.emacs.d/matlab-emacs-src/")
(require 'matlab-load)

;; use matlab-mode when you load .m files
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

(defun run-matlab-code ()
  "Execute matlab file."
  (interactive)
  (if (get-buffer "*MATLAB*")
      (matlab-shell-run-cell)
      (progn
        (matlab-mode)
        (matlab-shell)
        (split-window-right)
        (switch-to-buffer nil))))

(add-hook 'matlab-mode-hook
          (lambda ()
            (local-set-key (kbd "C-h") 'delete-backward-char)
            (local-set-key (kbd "C-c C-c") 'run-matlab-code)))

;; use F10 to submit selected txt
;;(define-key matlab-mode-map (kbd "C-c C-c") `matlab-shell-run-cell)
;;(define-key matlab-mode-map (kbd "C-h") nil)

;;(defun my-matlab-initialization-hook ()
;;  (local-set-key (kbd "C-c C-c") 'execute-c-program))
;;c-initialization-hook evaluated when c major mode first starts
;;(add-hook 'matlab-mode-hook 'my-matlab-initialization-hook)


;;auto-complete
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;;web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hook for web-mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'my-web-mode-hook)

;; iy-go-to-char, immediately jump to next occurence of char
(require 'iy-go-to-char)
(global-set-key (kbd "M-m") 'iy-go-to-char)

;;shell shortcut
(global-set-key [f1] 'shell)

;; disable bell
(setq ring-bell-function 'ignore)

;;transpose shortcuts
(defun push-line-up ()
  "Move the current line up, reverse transpose."
  (interactive)
  (forward-line -1)
  (transpose-lines 1))
(forward-line -1)
(indent-according-to-mode)

(global-set-key (kbd "M-p") 'push-line-up)
(global-set-key (kbd "M-n") 'transpose-lines)

;;control h to backspace
(global-set-key (kbd "C-? k") 'describe-key)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
;;(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "C-.") 'ace-window)
;;(global-set-key (kbd "C-i") 'back-to-indentation)

;;start flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-dafny-executable "BASE-DIRECTORY/dafny/Binaries/dafny")

;; helm
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

;; org-mode
;; -*- mode: elisp -*-

;; Enable transient mark mode
(transient-mark-mode 1)

;;;;Org mode configuration
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
;; set up org-capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(global-set-key (kbd "C-c c") 'org-capture)

;; default org file
(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file
                                          (concat org-directory "/main.org"))))
(setq org-return-follows-link 1)
(setq org-startup-folded nil)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("i" "Idea" entry (file+headline "~/org/ideas.org" "Ideas")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry
         (file+headline "~/org/notes.org" "Notes")
         "* %?\n  %i\n %a")
        ("r" "Random thoughts" entry
         (file+headline "~/org/random.org" "Random Thoughts")
         "* TODO %?\n  %i\n")
        ("k" "Kill-ring long-term storage" plain (file+datetree "~/org/clipboard.org")
         "%c" :immediate-finish t :empty-lines 1)
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i")))



;;AucTex
(setq TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Ctags
(global-set-key (kbd "M-,") 'xref-pop-marker-stack)
(setq tags-revert-without-query 1)

                                        ;set the default dictionary
;;(setq ispell-dictionary "british")

;;always start a shell in emacs
;;but in the same window
(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
;;same goes for shell commands
(push (cons "\\*Async Shell Command\\*" display-buffer--same-window-action) display-buffer-alist)
(push (cons "\\*Shell Command\\*" display-buffer--same-window-action) display-buffer-alist)
;(when-gui
; (shell))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:background "gray" :foreground "#fb4933" :box (:line-width 2 :color "grey75" :style released-button) :height 1.0)))))
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
