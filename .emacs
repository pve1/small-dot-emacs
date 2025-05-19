;; -*- lexical-binding: t -*-

;;;; PvE's minimal Emacs configuration.

;;; To use it, run:
;;; $ emacs -q --load .emacs
;;;
;;; For "dark" mode, run:
;;; $ emacs -q -rv --load .emacs

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Libs
;;;

(require 'package)
(require 'cl-lib)
(require 'dired-x)
(require 'ibuffer)
(require 'shell)
(require 'windmove)

;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load path
;;;

(when load-file-name
  (let ((here (file-name-directory (file-truename load-file-name))))
    (add-to-list 'load-path here)))

(add-to-list 'load-path "~/emacs")
(add-to-list 'load-path "~/emacs/elisp")
(add-to-list 'load-path "~/Emacs")
(add-to-list 'load-path "~/Emacs/elisp")

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modes
;;;

(menu-bar-mode 1)
(tool-bar-mode 0)
(show-paren-mode 1)
(global-font-lock-mode 1)
(column-number-mode 1)
(windmove-default-keybindings)
(indent-tabs-mode -1)
(ido-mode 'buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables etc.
;;;

(setq inhibit-startup-screen t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      scroll-bar-mode nil
      dabbrev-case-fold-search t
      dired-dwim-target t
      dired-listing-switches "-alh"
      dired-recursive-copies 'always
      calendar-date-style 'iso
      calendar-week-start-day 1
      european-calendar-style t)

(fset 'yes-or-no-p #'y-or-n-p)

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (and (< 1 (length (window-list)))
                       (equal "*scratch*" (buffer-name (current-buffer))))
              (delete-window))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Keys
;;;

(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)
(global-set-key (kbd "§") 'dabbrev-expand)
(global-set-key (kbd "M-1") "(")
(global-set-key (kbd "M-2") ")")
(global-set-key (kbd "C-<delete>") 'fixup-whitespace)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-<") 'other-window)
(global-set-key (kbd "<insert>") 'other-window)
(global-set-key (kbd "<insertchar>") 'other-window)
(global-set-key (kbd "M-<up>") 'backward-sexp)
(global-set-key (kbd "M-<down>") 'forward-sexp)
(global-set-key (kbd "M-<end>") 'pve-buffer-cycle-go)

(define-key emacs-lisp-mode-map (kbd "M-r") 'raise-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dark window borders
;;;

(defun pve-set-frame-dark (frame)
  (let ((id (frame-parameter frame 'outer-window-id)))
    (call-process-shell-command
     (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"dark\" -id \""
             id
             "\""))))

(defun pve-set-selected-frame-dark ()
  (interactive)
  (pve-set-frame-dark (selected-frame)))

(defun pve-ensure-all-frames-dark (&optional frame)
  (dolist (frame (frame-list))
    (pve-set-frame-dark frame)))

(when (window-system)
  (pve-ensure-all-frames-dark))

;; (add-hook 'after-make-frame-functions 'pve-set-frame-dark)
;; (add-hook 'desktop-after-read-hook 'pve-ensure-all-frames-dark)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; pve-query-replace
;;;

(defun pve-region-as-string ()
  (buffer-substring-no-properties (mark) (point)))

(defun pve-query-replace (prefix)
  (interactive "P")
  (if (region-active-p)
      (let* ((from (pve-region-as-string))
             (to (read-from-minibuffer
                  (format "Query replace: %s -> " from)
                  from)))
        (if prefix
            (goto-char (min (mark) (point)))
          (goto-char 1))
        (deactivate-mark)
        (query-replace from to))
    (call-interactively 'query-replace)))

(global-set-key (kbd "C-M-5") 'pve-query-replace)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dired
;;;

(defun pve-dired-kill-filename-at-point ()
  (interactive)
  (kill-new (file-truename (dired-file-name-at-point))))

(define-key dired-mode-map (kbd "C-c C-f") 'pve-dired-kill-filename-at-point)

(defun pve-dired-open-terminal-here ()
  (interactive)
  (call-process "gnome-terminal"))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Emacs lisp
;;;

(defun pve-emacs-lisp-hook ()
  (eldoc-mode 1))

(add-hook 'emacs-lisp-mode-hook 'pve-emacs-lisp-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Simple buffer cycling
;;;

(defvar pve-buffer-cycle-buffers nil)
(defvar pve-buffer-cycle-seen-buffers nil)
(defvar pve-buffer-cycle-active nil)

(defun pve-buffer-cycle-make-stop-cmd (command)
  (lambda ()
    (interactive)
    (pve-buffer-cycle-stop)
    (call-interactively command)))

(define-minor-mode pve-buffer-cycle
  "Simple buffer cycling."
  :lighter #(" Cycle" 0 6 (face (:foreground "Red" :weight bold)))
  :group pve
  :keymap
  (list (cons (kbd "C-g") (pve-buffer-cycle-make-stop-cmd
                           'pve-buffer-cycle-restore-sanity))
        (cons (kbd "<up>") (pve-buffer-cycle-make-stop-cmd 'previous-line))
        (cons (kbd "<down>") (pve-buffer-cycle-make-stop-cmd 'next-line))
        (cons (kbd "<left>") (pve-buffer-cycle-make-stop-cmd 'left-char))
        (cons (kbd "<right>") (pve-buffer-cycle-make-stop-cmd 'right-char))))

(defun pve-buffer-cycle-get-next-buffer ()
  (let ((buf (pop pve-buffer-cycle-buffers)))
    (when buf
      (if (or (minibufferp buf)
              (string-prefix-p " *Echo Area " (buffer-name buf))
              (string-prefix-p "*Echo Area " (buffer-name buf)))
          (pve-buffer-cycle-get-next-buffer)
        buf))))

(defun pve-buffer-cycle-present-buffer (buffer)
  (switch-to-buffer buffer t t)
  (push buffer pve-buffer-cycle-seen-buffers)
  (pve-buffer-cycle))

(defun pve-buffer-cycle-go ()
  (interactive)
  (unless pve-buffer-cycle
    (setf pve-buffer-cycle-buffers (cdr (buffer-list))))
  (let ((buf (pve-buffer-cycle-get-next-buffer)))
    (when buf
      (pve-buffer-cycle-present-buffer buf))))

(defun pve-buffer-cycle-stop ()
  (interactive)
  (dolist (buf pve-buffer-cycle-seen-buffers)
    (with-current-buffer buf
      (pve-buffer-cycle -1)))
  (setf pve-buffer-cycle-seen-buffers nil)
  (setf pve-buffer-cycle-buffers nil)
  (switch-to-buffer (current-buffer)))

(defun pve-buffer-cycle-restore-sanity ()
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (pve-buffer-cycle -1)))
  (keyboard-quit))
