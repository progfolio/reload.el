;;; reload.el --- Hot reload elisp libraries         -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nicholas Vollmer

;; Author:  Nicholas Vollmer
;; URL: https://github.com/progfolio/doct
;; Created: December 10, 2019
;; Keywords: convenience, lisp
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.0.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Hot reloader for elisp libraries

;;; Code:
(require 'cl-lib)

(defun reload--load-path ()
  "Return `load-path' suitable for finding hot-loaded packages."
  (append (mapcar (lambda (el) (file-name-directory (car el))) load-history)
          load-path))

(defun reload--library-file (library)
  "Return LIBRARY file."
  (locate-library (symbol-name library) nil (reload--load-path)))

(defun reload--loaded (library)
  "Return LIBRARY's loaded definitions."
  (or (alist-get (reload--library-file library) load-history nil nil #'equal)
      (user-error "Library not loaded: %S" library)))

(defun reload-read-library ()
  "Return loaded library symbol."
  (intern (completing-read "Reload library: "
                           (mapcar (lambda (f) (file-name-base (car f))) load-history)
                           nil t)))

;;@TODO: defface, define-type, cl-defmethod
(defun reload--library-symbols (library &optional type)
  "Return list of LIBRARY's symbols.
TYPE may be one of the following symbols:
  - defun:   function symbols
  - provide: feature symbols
  - var:     variable symbols
  - require: required features
If TYPE is nil, all types are returned."
  (let ((loaded (reload--loaded library)))
    (if (null type)
        (mapcar (lambda (el) (if (symbolp el) el (cdr el))) loaded)
      (delq nil (mapcar (lambda (el) (if (and (eq type 'var) (symbolp el))
                                         el
                                       (when (eq (car-safe el) type) (cdr el))))
                        loaded)))))

(defun reload--buffer-local-vars (library)
  "Return list of form (BUFFER . LOCALVARS) for LIBRARY."
  (cl-loop with vars = (reload--library-symbols library 'var)
           for buffer in (buffer-list)
           for locals =
           (cl-loop for var in vars
                    when (local-variable-p var buffer)
                    collect
                    (cons var (with-current-buffer buffer (symbol-value var))))
           when locals collect (cons buffer locals)))

(defun reload--buffer-modes (library)
  "Return list of buffers which need modes reloaded after LIBRARY is reloaded."
  (cl-loop with defuns = (reload--library-symbols library 'defun)
           for b in (buffer-list)
           collect (cons b (with-current-buffer b (cons major-mode local-minor-modes)))
           into buffers
           finally return
           (cl-remove-if-not (lambda (modes) (cl-intersection modes defuns))
                             buffers :key #'cdr)))

;;@TODO: FILE should be optional, so we can make this play nice with load-prefer-newer.
(defun reload--features (file &rest features)
  "Reload FILE's FEATURES."
  (cl-loop for feature in features do (progn (unload-feature feature t)
                                             (require feature file))))

(defun reload--restore-vars (pairs)
  "Restore variable PAIRS.
Each pair is a cons cell of form: (SYMBOL . VALUE)."
  (cl-loop for (var . val) in pairs
           do (funcall (or (get var 'custom-set) #'set-default) var val)))

(defun reload--restore-buffer-modes (library buffers)
  "Restore LIBRARY BUFFERS."
  (cl-loop with defuns = (reload--library-symbols library 'defun)
           for  (buffer major minors) in buffers
           do (with-current-buffer buffer
                (let ((majorp (memq major defuns)))
                  (if (not majorp)
                      (cl-loop for minor in minors
                               when (memq minor defuns)
                               do (funcall minor 1))
                    (funcall major)
                    (cl-loop for minor in minors do (funcall minor)))))))

;;;###autoload
(defun reload (library &optional clean)
  "Reload LIBRARY.
It's features are first forcibly unloaded.
If CLEAN is non-nil, previous variable bindings are not restored."
  (interactive (list (reload-read-library) current-prefix-arg))
  (let ((modes  (reload--buffer-modes library))
        (vars   (mapcar (lambda (it) (cons it (symbol-value it)))
                        (reload--library-symbols library 'var)))
        (locals (reload--buffer-local-vars library)))
    (apply #'reload--features (append (list (reload--library-file library))
                                      (reload--library-symbols library 'provide)))
    (reload--restore-buffer-modes library modes)
    (unless clean
      ;;@TODO: globalized-minors
      (let ((loaded (mapcar #'list (reload--library-symbols library 'var))))
        ;; vars is passed as last arg so `cl-intersection' keeps our symbol values.
        (setq vars (cl-intersection loaded vars :key #'car))
        (reload--restore-vars vars)
        (cl-loop for (buffer . pairs) in locals
                 do (with-current-buffer buffer
                      (reload--restore-vars (cl-intersection vars pairs :key #'car))))))
    ;;@TODO: clean up? Unbind symbols which were not restored?
    (message "Library %S reloaded" library)))

;;(defvar reload-test t "A test var.")

(provide 'reload)
;;; reload.el ends here
