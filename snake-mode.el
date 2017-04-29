;;; snake-mode.el --- minor mode for picture mode for fun

;; Copyright © 2017 Benedek Fazekas
;;
;; Author: Benedek Fazekas <benedek.fazekas@gmail.com>
;; Version: 0.0.1-snapshot
;; Package-Requires: ((emacs "24.3"))
;; Keywords: games

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:


(defvar-local snake--orientation :horizontal "stores the snake's current orientation")

(defcustom snake-turn-probability 10 "probability percentage of the snake turning at the current key press"
  :group 'snake-mode)

(defcustom snake-detect-collision t "detect collision if t otherwise ignore collisions"
 :group 'snake-mode)

(defcustom snake-collision-fn 'snake--collision-notify "Function that handles collision."
  :group 'snake-mode)

(defun snake--next-movement ()
  (if (not (snake--turn-p))
      (apply-partially 'picture-motion 0)
    (if (eq :horizontal snake--orientation)
        (progn
          (setq snake--orientation :vertical)
          (if (= 1 (line-number-at-pos))
              'picture-movement-down
            (aref [picture-movement-up picture-movement-down] (random 2))))
      (setq snake--orientation :horizontal)
      (if (= 0 (current-column))
          'picture-movement-right
        (aref [picture-movement-left picture-movement-right] (random 2))))))

(defun snake--turn-p ()
  (or
   (and (eq :horizontal snake--orientation) (= 0 (current-column)))
   (and (eq :vertical snake--orientation) (= 1 (line-number-at-pos)))
   (< (random 100) snake-turn-probability)))

(defun snake--collision-char ()
  (let ((next-char (progn
                     (picture-motion 1)
                     (char-after))))
    (picture-motion-reverse 1)
    (when (and next-char (string-match "\\w" (string next-char)))
      next-char)))

(defun snake--collision-notify (char)
  (message "Colliding with '%s'" (string char)))

(defun snake ()
  (let ((keys (this-command-keys)))
      (when (and (stringp keys)
                 (string-match-p "\\w" keys))
        (picture-motion-reverse 0)
        (let ((next-move  (snake--next-movement))
              (collision-char (when snake-detect-collision
                                (snake--collision-char))))
          (when collision-char
            (funcall snake-collision-fn collision-char))
          (funcall next-move)))))

(define-minor-mode snake-mode "Turns your lines into a snake in picture mode." nil "Snake" nil
  (if snake-mode
      (if (eq 'picture-mode major-mode)
          (add-hook 'post-command-hook 'snake nil :local)
        (setq snake-mode nil)
        (user-error "Snake mode is a minor mode designed for picture mode. Please enable picture mode."))
    (remove-hook 'post-command-hook 'snake :local))
  :group 'snake-mode)

(provide 'snake-mode)

;;; snake-mode.el ends here
