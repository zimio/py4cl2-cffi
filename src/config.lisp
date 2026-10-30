(defpackage :py4cl2-cffi/config
  (:use :cl)
  (:export #:+python-version-string+
           #:*python-ldflags*
           #:*python-ignore-ldflags*
           #:*python-includes*
           #:*python-compile-command*
           #:*python-numpy-compile-command*
           #:*python-executable-path*
           #:*python-site-packages-path*
           #:*disable-pystop*
           #:*python-call-mode*
           #:*pystart-verbosity*
           #:print-configuration
           #:shared-library-from-ldflag))

(in-package :py4cl2-cffi/config)

;; Basic principle: We aim to use python3-config or equivalent to discover these values
;; However, whenever there is a problem with getting the python-executable path, then we
;; set it *explicitly* externally using an environment variable, e.g., via including
;; something like
;;
;; (setf (uiop:getenv "PY4CL2_CONFIG_PYTHON_EXECUTABLE_PATH")	
;;      "/usr/local/Caskroom/miniconda/base/bin/python3")
;;
;; into .sbclrc
;;
;; Then, we use that path (if set) for inferring the other values

(declaim (type list
               *python-ldflags*
               *python-includes*))

(declaim (type string *python-compile-command*))

;; On Windows, CPython is normally built with MSVC. The helper C files below
;; only use the stable CPython C-API, so they can also be built with MinGW-w64
;; gcc linking against pythonXY.lib. These defaults target gcc; the exact
;; include/lib flags are filled in by config-windows.lisp (see below).
(defvar *python-compile-command*
  (if (eq :win (uiop:operating-system))
      ;; ~A -> *python-includes*, filled by config-windows.lisp with the
      ;; full command (it needs the python lib dir which is discovered here).
      "gcc ~A -shared -o libpy4cl-utils.dll py4cl-utils.c"
      (concatenate
       'string
       "gcc ~A -c -Wall -Werror -fpic py4cl-utils.c && "
       "gcc -shared -o libpy4cl-utils.so py4cl-utils.o"))
  "~A corresponds to the *python-includes*
")

(defvar *python-numpy-compile-command*
  (if (eq :win (uiop:operating-system))
      "gcc ~A -I\"~A\" -shared -o libpy4cl-numpy-utils.dll py4cl-numpy-utils.c"
      (concatenate
       'string
       "gcc ~A -I'~A' -c -Wall -Werror -fpic py4cl-numpy-utils.c && "
       "gcc -shared -o libpy4cl-numpy-utils.so py4cl-numpy-utils.o"))
  "The first~A corresponds to the *python-includes*
The second ~A corresponds to the numpy include files discovered
  in shared-objects.lisp
")


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun return-value-as-list (cmd)
    "Run CMD (a string passed to the shell, or a list of program + args)
and return its whitespace-separated output tokens as a list of strings."
    (remove ""
            (mapcar (lambda (value)
                      (string-trim '(#\newline #\return) value))
                    (uiop:split-string
                     (uiop:run-program cmd
                                       :output :string
                                       :error-output *error-output*)
                     :separator '(#\newline #\return #\tab #\space)))
            :test #'string=))

  (defun python-eval-to-string (python-executable code)
    "Run a one-liner of python CODE using PYTHON-EXECUTABLE and return the
raw string printed to stdout. CODE is passed as a separate argv element so
that no shell quoting (which differs between sh and cmd.exe) is required."
    (string-trim
     '(#\newline #\return #\space #\tab)
     (uiop:run-program (list python-executable "-c" code)
                       :output :string
                       :error-output *error-output*)))

  (defun %find-python-executable ()
    (cond
      ((uiop:getenv "PY4CL2_CONFIG_PYTHON_EXECUTABLE_PATH")
       (uiop:getenv "PY4CL2_CONFIG_PYTHON_EXECUTABLE_PATH"))
      ((eq :win (uiop:operating-system))
       ;; `which` does not exist on Windows; use `where` and take the first hit.
       (or (first (return-value-as-list (list "where" "python")))
           (first (return-value-as-list (list "where" "python3")))
           "python"))
      (t
       (first (return-value-as-list "which python3")))))

  (defvar *python-executable-path* (%find-python-executable)
    "The path to python executable. This will be used to set sys.path.
This is useful in cases such as venv when python3-config does not lead
to expected paths.")

  (alexandria:define-constant +python-version-string+
      ;; "python --version" prints e.g. "Python 3.9.5"; grab the version token.
      (let ((tokens (return-value-as-list
                     (list *python-executable-path* "--version"))))
        (or (find-if (lambda (tok) (digit-char-p (char tok 0))) tokens)
            (second tokens)))
    :test #'string=)

  (defun %windows-python-ldflags (python-executable)
    "Derive the equivalent of python3-config --embed --ldflags on Windows.
CPython on Windows does not ship python3-config, so we query sysconfig for
the import-library directory and version and build the flags ourselves."
    (let* ((libdir
             ;; On Windows the import libs live under <base>/libs .
             (python-eval-to-string
              python-executable
              "import os,sysconfig,sys;print(os.path.join(sysconfig.get_config_var('installed_base') or sys.base_prefix,'libs'))"))
           (version
             ;; e.g. "39" for Python 3.9
             (python-eval-to-string
              python-executable
              "import sysconfig;print(sysconfig.get_config_var('VERSION'))")))
      (list (format nil "-L~A" libdir)
            (format nil "-lpython~A" version))))

  (if (eq :win (uiop:operating-system))
      (defvar *python-ldflags*
        (%windows-python-ldflags *python-executable-path*))
      (if (uiop:version< +python-version-string+ "3.8.0")
          (defvar *python-ldflags*
            (return-value-as-list "python3-config --ldflags"))
          (defvar *python-ldflags*
            (return-value-as-list "python3-config --embed --ldflags")))))


(defvar *python-includes*
  (if (eq :win (uiop:operating-system))
      (list (format nil "-I~A"
                    (python-eval-to-string
                     *python-executable-path*
                     "import sysconfig;print(sysconfig.get_path('include'))")))
      (return-value-as-list "python3-config --includes")))

(defvar *python-site-packages-path*
  ;; Pass the python snippet as a single argv element (no shell quoting), so it
  ;; works the same under /bin/sh and cmd.exe .
  (return-value-as-list
   (list *python-executable-path*
         "-c" "import sys;print(chr(10).join(sys.path))")))

(defun print-configuration ()
  "Prints the ldflags and includes that will be used for the compilation
of the utility shared object/library that bridges the python C-API with lisp."
  (format t "Python ldflags: ~{~A~^ ~}~%Python includes: ~{~A~^ ~}~%"
          *python-ldflags*
          *python-includes*)
  (format t "Python site path (additional):~%  ~S~%" *python-site-packages-path*)
  (format t "  These are appended to sys.path after the embedded python starts.~%")
  (format t "Python executable used for site path:~%  ~A"
          *python-executable-path*))

(defvar *disable-pystop* nil
  "If non-NIL (PY4CL2-CFFI:PYSTOP) becomes a no-op.")

(defvar *python-ignore-ldflags*
  ;; python3-config of older versions of python (such as python3.6) includes
  ;; librt in --ldflags, but apparantly, older SBCL (SBCL 1.5.4) versions fail
  ;; to load it, and librt itself seems unnecessary.
  '("-lpthread" "-ldl" "-lutil" "-lanl" "-lm" "-lrt")
  "A list of ldflags that will be ignored during the compilation of
the utility shared object/library.")

(declaim (type (member :standard :dedicated-thread) *python-call-mode*))
(defvar *python-call-mode* :standard
  "PY4CL2-CFFI:+PYTHON-CALL-MODE+ is a constant assigned to the value of
PY4CL2-CFFI/CONFIG:*PYTHON-CALL-MODE* at compile time. Thus, if
PY4CL2-CFFI/CONFIG:*PYTHON-CALL-MODE* is changed, PY4CL2-CFFi must be recompiled.

Possible values are :STANDARD and :DEDICATED-THREAD

- When the value is :DEDICATED-THREAD, a dedicated thread is used for starting
  the embedded python interpreter. All calls to python from lisp are made
  through this thread. This may be required for using libraries like matplotlib
  that expect all calls from the single thread that loads the library. On the
  other hand, in emacs, each lisp buffer may communicate to the lisp process
  (and thus python) through separate threads. It is however possible to
  configure emacs and slime/swank to communicate using a single thread. See
  SWANK:*COMMUNICATION-STYLE*. Python call mode provides a way of sidestepping
  the slime/swank configuration.

- When the value is :STANDARD, no such thread is created. Calls to python can
  take place from any thread.")

(declaim (type boolean *pystart-verbosity*))
(defvar *pystart-verbosity* t
  "Default value of :VERBOSE argument to PY4CL2-CFFI:PYSTART")

(defun %shared-library-from-ldflag (ldflag)
  "Given a ldflag, for example, \"-lpython3.10\", return the shared library name
corresponding to it. In this case, on linux, it will return libpython3.10.so"
  (shared-library-from-ldflag ldflag (uiop:operating-system)))

(defgeneric shared-library-from-ldflag (ldflag operating-system)
  (:documentation "This is a generic function which takes in two arguments. The first argument is an ldflag (like `-lpython3.10`) and the second argument is the `(uiop:operating-system)` as a keyword to be used for specialization on the users systems. Each method should return the shared library name associated with that ldflag and software type. For example, when `(uiop:operating-system)` is `:linux`, the relevant method should return `python3.10.so`"))

(defmethod shared-library-from-ldflag (ldflag (operating-system (eql :linux)))
  (format nil "lib~A.so" (subseq ldflag 2)))

(defmethod shared-library-from-ldflag (ldflag (operating-system (eql :macosx)))
  (format nil "lib~A.dylib" (subseq ldflag 2)))

(defmethod shared-library-from-ldflag (ldflag (operating-system (eql :win)))
  ;; CPython on Windows ships its interpreter DLL as e.g. "python39.dll"
  ;; (no "lib" prefix, no dot between major and minor version), while the
  ;; helper libraries we build ourselves are named "libpy4cl-utils.dll" etc.
  ;; Only the python ldflag needs the special CPython naming; everything else
  ;; keeps the conventional "lib<name>.dll".
  (let ((name (subseq ldflag 2)))
    (cond
      ;; e.g. "-lpython3.9" -> "python39.dll"
      ((and (>= (length name) 6)
            (string-equal "python" (subseq name 0 6)))
       (format nil "~A.dll" (remove #\. name)))
      ;; e.g. "-lpython39" -> "python39.dll"
      (t
       (format nil "lib~A.dll" name)))))
