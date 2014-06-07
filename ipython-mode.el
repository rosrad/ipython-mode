;;; ipython-mode.el --- ESS-like behavior in Python-mode & Comint-mode

;; Copyright (c) 2014- Bernhard Pröll

;; Author: Bernhard Pröll 
;; Maintainer: Bernhard Pröll 
;; URL: https://github.com/mutbuerger/ipython-mode.el
;; Created: 26.02.2014
;; Version: 0.1
;; Keywords: ipython, customization

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


(require 'python-mode)
(require 'auto-complete)
(require 'jedi)
(require 'yasnippet)

;;; Rename Interpreter Buffer
(setq py-buffer-name "*Ipython*")

;;; Enable AC & Jedi - Python Autocompletion Library for Emacs
;; If you haven't done it yet, enable Auto-complete and Jedi for
;; Python-mode by uncommenting the following lines:
;; (add-hook 'python-mode-hook 'auto-complete-mode)
;; (add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot nil)
(setq jedi:tooltip-method nil)		;eldoc-like tooltip in minibuffer

;;; YaSnippet Settings
;;; Hook Python Snippets into Python-mode
(add-hook 'python-mode
	  (lambda ()
	    (yas-extra-modes 'python-mode)))

;;; Emulate R/ESS behavior
(setq py-split-windows-on-execute-p nil) ;prevent splitting windows
(setq py-switch-buffers-on-execute-p nil) ;prevent switching buffers

(defun execute-region-or-buffer-ipython () ;execute region or line and set ipython buffer to other window
  "Send active region or buffer to ipython interpreter and show interpreter buffer in other window"
  (interactive)
  (setq buf (current-buffer))
  (if (region-active-p)
      (py-execute-region-ipython (region-beginning) (region-end))
    (py-execute-buffer-ipython)		;else
    )
  (switch-to-buffer-other-window "*Ipython*") ;show interpreter in other window
  (other-window 1)
  )

(defun execute-line-ipython ()
  "Send line at point to ipython interpreter and show interpreter buffer in other window"
  (interactive)
  (setq buf (current-buffer))
  (setq p1 (point))
  (py-execute-line-ipython)
  (switch-to-buffer-other-window "*Ipython*")
  (other-window 1)
  (goto-char p1)
  )

(defun switch-to-ipython-interpreter ()
  "Switch to ipython interpreter buffer"
  (interactive)
  (setq buf (current-buffer))
  (switch-to-buffer-other-window "*Ipython*")
  (end-of-buffer)			;similar to 'ess-switch-to-end-of-proc-buffer t'
  )

(defun switch-to-script-buffer ()
  "Switch to script buffer"
  (interactive)
  (switch-to-buffer-other-window buf)
  )

(defun ipython-send-input ()
  "Send input in ipython interpreter and clear line"
  (interactive)
  (comint-send-input)
  (kill-line 1)				;bc sent code persists in new cells for me; depends on the python version afaik
  )

;;; Redefining Python-Mode Keys
;; Redefine default C-c C-c and C-c C-j keys
(add-hook 'python-mode-hook
          (lambda()
            (local-set-key (kbd "C-c C-c") 'execute-region-or-buffer-ipython)))
(add-hook 'python-mode-hook
          (lambda()
            (local-set-key (kbd "C-c C-j") 'execute-line-ipython)))
;;; and to jump back and forth between interpreter and script buffer with C-c C-z:
(define-key python-mode-map (kbd "C-c C-z") 'switch-to-ipython-interpreter)

;;; Autostart ipython-mode and Jedi in Py-shell
(add-hook 'py-shell-hook 'ipython-mode)
(add-hook 'py-shell-hook 'jedi:setup)
(add-hook 'py-shell-hook 'yas-minor-mode)

(define-minor-mode ipython-mode
  "Minor mode to emulate an ESS-like workflow with Python-mode & an ipython interpreter"
  :lighter " ipython" 
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c C-z") 'switch-to-script-buffer)
	    (define-key map (kbd "RET") 'ipython-send-input)
            map)
  )

(provide 'ipython-mode)
