;;; package-safe-delete.el --- Safely delete package.el packages -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/package-safe-delete
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (epl "0.7-cvs"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2014, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;; * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;; * Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; Delete package.el packages safely, without leaving unresolved dependencies.

;;; Code:
(require 'epl)

(defun package-safe-delete--installed-package-dependencies (excluded)
  "Get a dependency tree of the installed packages.
Dependencies of EXCLUDED packages are ignored.

The returned value is a hash table of the form package => list of packages
requiring it."
  (let ((package+descriptors (epl-installed-packages))
        (dependencies (make-hash-table :test #'eq)))
    (dolist (package+descriptor package+descriptors)
      (let ((package (epl-package-name package+descriptor)))
        (unless (memq package excluded)
          (dolist (requirement+descriptor (epl-package-requirements package+descriptor))
            (let ((requirement (epl-requirement-name requirement+descriptor)))
              (when (epl-package-installed-p requirement)
                (push package (gethash requirement dependencies))))))))
    dependencies))

(defun package-safe-delete--delete (packages force)
  "Delete PACKAGES.

PACKAGES is a list of package name symbols.
With FORCE non-nil, the user is not prompted for confirmation before the
packages are deleted."
  (when (or force
            (yes-or-no-p
             (pcase packages
               (`(,package)
                (format "Delete package `%s'? " package))
               (_
                (format "Delete these packages: %s? "
                        (mapconcat #'symbol-name packages ", "))))))
    (dolist (package packages)
      (mapc #'epl-package-delete (epl-find-installed-packages package)))))

(defun package-safe-delete--prompt-package-name (prompt)
  "Read a package name in the minibuffer.
PROMPT is a string to prompt with."
  (list
   (intern
    (completing-read prompt
                     (mapcar #'epl-package-name (epl-installed-packages))
                     nil
                     t))))

;;;###autoload
(defun package-safe-delete-packages (packages &optional force)
  "Delete PACKAGES.

PACKAGES is a list of package name symbols.
None of the PACKAGES are deleted when there's a package depending on one of
them, or if one of the PACKAGES is not installed.
With FORCE non-nil, the user is not prompted for confirmation before the
packages are deleted."
  (dolist (package packages)
    (unless (epl-package-installed-p package)
      (error "Package `%S' is not installed" package)))
  (let ((dependencies (package-safe-delete--installed-package-dependencies packages)))
    (dolist (package packages)
      (pcase (gethash package dependencies)
        (`nil)
        (`(,dependent+package)
         (error "Cannot delete `%S' because it's required by `%S'"
                package
                dependent+package))
        (dependent+packages
         (error "Cannot delete `%S' because it's required by: %s"
                package
                (mapconcat #'symbol-name dependent+packages ", "))))))
  (package-safe-delete--delete packages force))

;;;###autoload
(defun package-safe-delete (package)
  "Delete a PACKAGE.

PACKAGE is a package name symbol.
PACKAGE is not deleted when there are other packages requiring it.
Interactively, prompt for its name."
  (interactive (package-safe-delete--prompt-package-name "Delete package: "))
  (package-safe-delete-packages (list package)))

(provide 'package-safe-delete)
;;; package-safe-delete.el ends here
