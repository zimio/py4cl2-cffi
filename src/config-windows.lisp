(in-package :py4cl2-cffi/config)

;;; Windows-specific configuration.
;;;
;;; CPython on Windows does not ship `python3-config`, and its interpreter DLL
;;; is named like `python39.dll` (import library `python39.lib` under
;;; <base>/libs). We therefore discover the include and import-library paths
;;; from `sysconfig` (done in config.lisp via *python-ldflags* / *python-includes*)
;;; and here we assemble the gcc command lines that build the two helper DLLs
;;; (libpy4cl-utils.dll and libpy4cl-numpy-utils.dll) against that import lib.
;;;
;;; This mirrors what src/config-darwin.lisp does for macOS.

(defun %windows-link-flags ()
  "Return the `-L... -lpython..` portion for linking the helper DLLs, taken
from *PYTHON-LDFLAGS* as discovered in config.lisp."
  (format nil "~{~A~^ ~}"
          (remove-if-not
           (lambda (flag)
             (and (>= (length flag) 2)
                  (member (subseq flag 0 2) '("-L" "-l") :test #'string=)))
           py4cl2-cffi/config:*python-ldflags*)))

(defun configure ()
  (let ((link-flags (%windows-link-flags)))
    ;; ~A is filled with *python-includes* (e.g. -IC:/.../Include) by
    ;; shared-objects.lisp . The include flags already carry the directory, so
    ;; a single gcc invocation both compiles and links into a DLL.
    (setq py4cl2-cffi/config:*python-compile-command*
          (format nil
                  "gcc ~~A -shared -o libpy4cl-utils.dll py4cl-utils.c ~A"
                  link-flags))

    ;; The first ~A is *python-includes*, the second ~A is the numpy include
    ;; directory, both supplied by shared-objects.lisp .
    (setq py4cl2-cffi/config:*python-numpy-compile-command*
          (format nil
                  "gcc ~~A -I\"~~A\" -shared -o libpy4cl-numpy-utils.dll py4cl-numpy-utils.c ~A"
                  link-flags))))

(when (eq :win (uiop:operating-system))
  (configure))
