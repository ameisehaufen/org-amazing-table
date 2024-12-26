;;; org-amazing-table.el --- Replace org-table characters with box-drawing unicode glyphs -*- lexical-binding: t -*-

;; Copyright (C) 2013, 2023 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: faces
;; URL: https://github.com/Fuco1/org-amazing-table
;; Package-Requires: ((org "9") (emacs "24.1"))
;; Version: 1.0.0
;; Created: 29th November 2013

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This replaces the characters - | and + in `org-mode' tables with
;; appropriate unicode box-drawing glyphs, see
;; http://en.wikipedia.org/wiki/Box-drawing_character

;;; Code:

(require 'org)

(defconst org-amazing-table-regexp (regexp-opt '("-" "+" "|")))

(defgroup org-amazing-table ()
  "Replace org-table characters with box-drawing unicode glyphs."
  :group 'org)

(defcustom org-amazing-table-charset "┌┐└┘┬┤┴├┼─│"
  "Charset to draw the table.

The value is a string of length 11 with the characters used to
draw the table borders.

The order of the blocks is:

- upper left corner
- upper right corner
- lower left corner
- lower right corner
- down-facing T
- left-facing T
- up-facing T
- right-facing T
- cross
- horizontal bar
- vertical bar"
  :group 'org-amazing-table
  :type '(choice (const :tag "Single horizontal lines" "┌┐└┘┬┤┴├┼─│")
                 (const :tag "Double horizontal lines" "╒╕╘╛╤╡╧╞╪═│")
                 (string :tag "Custom")))

(defsubst org-amazing-table-ul-corner ()
  "Return upper left corner character as a string."
  (declare (pure t))
  (make-string 1 (aref org-amazing-table-charset 0)))

(defsubst org-amazing-table-ur-corner ()
  "Return upper right corner character as a string."
  (declare (pure t))
  (make-string 1 (aref org-amazing-table-charset 1)))

(defsubst org-amazing-table-ll-corner ()
  "Return lower left corner character as a string."
  (declare (pure t))
  (make-string 1 (aref org-amazing-table-charset 2)))

(defsubst org-amazing-table-lr-corner ()
  "Return lower right corner character as a string."
  (declare (pure t))
  (make-string 1 (aref org-amazing-table-charset 3)))

(defsubst org-amazing-table-df-t ()
  "Return down facing T character as a string."
  (declare (pure t))
  (make-string 1 (aref org-amazing-table-charset 4)))

(defsubst org-amazing-table-lf-t ()
  "Return left facing T character as a string."
  (declare (pure t))
  (make-string 1 (aref org-amazing-table-charset 5)))

(defsubst org-amazing-table-uf-t ()
  "Return up facing T character as a string."
  (declare (pure t))
  (make-string 1 (aref org-amazing-table-charset 6)))

(defsubst org-amazing-table-rf-t ()
  "Return right facing T character as a string."
  (declare (pure t))
  (make-string 1 (aref org-amazing-table-charset 7)))

(defsubst org-amazing-table-cross ()
  "Return cross character as a string."
  (declare (pure t))
  (make-string 1 (aref org-amazing-table-charset 8)))

(defsubst org-amazing-table-hb ()
  "Return horizontal bar character as a string."
  (declare (pure t))
  (make-string 1 (aref org-amazing-table-charset 9)))

(defsubst org-amazing-table-vb ()
  "Return vertical bar character as a string."
  (declare (pure t))
  (make-string 1 (aref org-amazing-table-charset 10)))

(defun org-amazing-table-at-table-p ()
  "Check if point is at table."
  (save-excursion
    (skip-syntax-forward " " (line-end-position))
    (eq (following-char) ?|)))

(defun org-amazing-table-propertize-region (start end)
  "Replace org-table characters with box-drawing glyphs between START and END.

Used by jit-lock for dynamic highlighting."
  (save-excursion
    (goto-char start)
    (let (table-end)
      (while (re-search-forward org-amazing-table-regexp end t)
        ;; reached the end of the current table
        (if (and table-end
                 (> (point) table-end))
            (setq table-end nil))

        ;; check if the current match is a table if we are not in a
        ;; table right now
        (unless (and (not table-end)
                     (not (save-match-data
                            (org-at-table-p))))

          ;; get the end of the table if we found a new table, so we
          ;; don't have to check (org-at-table-p) again until then
          (unless table-end
            (save-match-data
              (setq table-end (org-table-end))))

          ;; determine the context of the character
          (let ((match (match-string 0)))
            (cond
             ((equal "-" match)
              (backward-char 1)
              (re-search-forward "-+")
              (when (looking-at-p "[+|]")
                (put-text-property
                 (match-beginning 0) (match-end 0)
                 'display
                 (make-string (- (match-end 0) (match-beginning 0))
                              (aref (org-amazing-table-hb) 0))))
              t)
             ((equal "|" match)
              (cond
               ((and (eq (following-char) ?-)
                     (save-excursion
                       (forward-line 1)
                       (org-amazing-table-at-table-p))
                     (save-excursion
                       (backward-char 1)
                       (not (bobp)))
                     (save-excursion
                       (forward-line -1)
                       (and (not (bobp))
                            (org-amazing-table-at-table-p))))
                (put-text-property (match-beginning 0) (match-end 0) 'display (org-amazing-table-rf-t))
                t)
               ((and (save-excursion
                       (backward-char 1)
                       (eq (preceding-char) ?-))
                     (save-excursion
                       (forward-line 1)
                       (org-amazing-table-at-table-p))
                     (save-excursion
                       (forward-line -1)
                       (and (not (bobp))
                            (org-amazing-table-at-table-p))))
                (put-text-property (match-beginning 0) (match-end 0) 'display (org-amazing-table-lf-t))
                t)
               ((and (save-excursion
                       (backward-char 1)
                       (eq (preceding-char) ?-))
                     (save-excursion
                       (forward-line -1)
                       (or (bobp)
                           (not (org-amazing-table-at-table-p)))))
                (put-text-property (match-beginning 0) (match-end 0) 'display (org-amazing-table-ur-corner))
                t)
               ((and (save-excursion
                       (backward-char 1)
                       (eq (preceding-char) ?-))
                     (save-excursion
                       (forward-line 1)
                       (not (org-amazing-table-at-table-p))))
                (put-text-property (match-beginning 0) (match-end 0) 'display (org-amazing-table-lr-corner))
                t)
               ((and (eq (following-char) ?-)
                     (save-excursion
                       (forward-line -1)
                       (or (bobp)
                           (not (org-amazing-table-at-table-p)))))
                (put-text-property (match-beginning 0) (match-end 0) 'display (org-amazing-table-ul-corner))
                t)
               ((and (eq (following-char) ?-)
                     (save-excursion
                       (forward-line 1)
                       (not (org-amazing-table-at-table-p))))
                (put-text-property (match-beginning 0) (match-end 0) 'display (org-amazing-table-ll-corner))
                t)
               (t
                (put-text-property (match-beginning 0) (match-end 0) 'display (org-amazing-table-vb))
                t)))
             ((equal "+" match)
              (cond
               ((and (eq (following-char) ?-)
                     (save-excursion
                       (backward-char 1)
                       (eq (preceding-char) ?-))
                     (save-excursion
                       (forward-line -1)
                       (and (not (bobp))
                            (org-amazing-table-at-table-p)))
                     (save-excursion
                       (forward-line 1)
                       (org-amazing-table-at-table-p)))
                (put-text-property (match-beginning 0) (match-end 0) 'display (org-amazing-table-cross))
                t)
               ((and (eq (following-char) ?-)
                     (save-excursion
                       (backward-char 1)
                       (eq (preceding-char) ?-))
                     (save-excursion
                       (forward-line -1)
                       (or (bobp)
                           (not (org-amazing-table-at-table-p))))
                     (save-excursion
                       (forward-line 1)
                       (org-amazing-table-at-table-p)))
                (put-text-property (match-beginning 0) (match-end 0) 'display (org-amazing-table-df-t))
                t)
               ((and (eq (following-char) ?-)
                     (save-excursion
                       (backward-char 1)
                       (eq (preceding-char) ?-))
                     (save-excursion
                       (let ((char-pos (- (point) (line-beginning-position) 1)))
                         (forward-line -1)
                         (beginning-of-line)
                         (forward-char char-pos))
                       (eq (following-char) ?|))
                     (save-excursion
                       (backward-char 1)
                       (forward-line)
                       (not (eq (following-char) ?|))))
                (put-text-property (match-beginning 0) (match-end 0) 'display (org-amazing-table-uf-t))
                t))))))))))

(defun org-amazing-table-unpropertize-region (start end)
  "Remove box-drawing compositions between START and END."
  (remove-text-properties start end '(display)))

(defun org-amazing-table-unpropertize-table ()
  "Remove box-drawing compositions from table at point."
  (org-amazing-table-unpropertize-region (org-table-begin) (org-table-end)))

(defun org-amazing-table-align (oldfun &rest args)
  (unwind-protect
      (progn
        (org-amazing-table-mode -1)
        (org-amazing-table-unpropertize-table)
        (apply oldfun args))
    (org-amazing-table-mode 1)))

;;; Minor mode:

;;;###autoload
(define-minor-mode org-amazing-table-mode
  "Replace org-table characters with box-drawing unicode glyphs."
  :lighter " OPT"
  (if org-amazing-table-mode
      (progn
        (jit-lock-register 'org-amazing-table-propertize-region t)
        (advice-add 'org-table-align :around #'org-amazing-table-align))
    (jit-lock-unregister 'org-amazing-table-propertize-region)
    (advice-remove 'org-table-align #'org-amazing-table-align)
    (org-amazing-table-unpropertize-region (point-min) (point-max))))

;;;###autoload
(defun turn-on-org-amazing-table-mode ()
  "Turn on `org-amazing-table-mode'."
  (org-amazing-table-mode 1))

;;;###autoload
(defun turn-off-org-amazing-table-mode ()
  "Turn off `org-amazing-table-mode'."
  (org-amazing-table-mode 0))

;;;###autoload
(define-globalized-minor-mode global-org-amazing-table-mode
  org-amazing-table-mode turn-on-org-amazing-table-mode)

(provide 'org-amazing-table)
;;; org-amazing-table.el ends here
