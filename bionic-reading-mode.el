;;; bionic-reading-mode.el --- Speed reading minor mode  -*- lexical-binding: t -*-

;; Author: Yuri D'Elia <wavexx@thregr.org>
;; Version: 0.1
;; URL: https://gitlab.com/wavexx/bionic-reading-mode.el
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, wp

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Highlight word stems in text buffers, thereby providing artificial
;; fixation points to improve speed reading. Inspired by a website of
;; the same name.
;;
;; Use M-x bionic-reading-mode in a text buffer to turn on (or off) the
;; minor mode. Customize `bionic-reading-highlight-face' to modify the
;; default highlight.

;;; Code:

;; Customizable parameters
(defface bionic-reading-highlight-face
  '((t :weight bold))
  "Face used for highlighting word stems."
  :group 'faces)

(defcustom bionic-reading-overlay nil
  "Control the aggressiveness of the stem highlight.
When nil (default), only highlight words which are not already
highlighted by other modes. When t, highlight all words irregardless.
Requires a mode toggle to take effect."
  :type 'boolean)

(defvar bionic-reading--overlay-state nil
  "Overlay state since the last mode activation.")


;; Helper functions
(defun bionic-reading--keywords ()
  "Compute font-lock keywords for word stems."
  (let ((overlay (if bionic-reading--overlay-state 'append)))
    (let ((keywords `(("\\<\\(\\w\\)\\w\\{,2\\}"
		       (1 'bionic-reading-highlight-face ,overlay)))))
      (dotimes (c 16)
	(let ((n (+ 2 c)))
	  (push (list
		 (format
		  "\\<\\(\\w\\{%d\\}\\)\\w\\{%d,\\}"
		  n (ceiling (* n 0.6)))
		 `(1 'bionic-reading-highlight-face ,overlay))
		keywords)))
      keywords)))


;;;###autoload
(define-minor-mode bionic-reading-mode
  "Highlight word stems for speed-reading."
  :lighter " BR"
  (cond
   (bionic-reading-mode
    (setq bionic-reading--overlay-state bionic-reading-overlay)
    (font-lock-add-keywords nil (bionic-reading--keywords) 'append))
   (t
    (font-lock-remove-keywords nil (bionic-reading--keywords))))
  (font-lock-flush))


(provide 'bionic-reading-mode)

;;; bionic-reading-mode.el ends here
