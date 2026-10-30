(in-package :py4cl2-cffi)

;;;; Windows named-pipe backed character input stream.
;;;;
;;;; On unix, py4cl2-cffi captures python's stdout/stderr by creating named
;;;; pipes (FIFOs) with mkfifo, pointing python's sys.stdout/sys.stderr at them
;;;; with open(path, 'w'), and reading them from lisp through an ordinary lisp
;;;; input stream. Windows has neither mkfifo nor /tmp, and a plain file cannot
;;;; reproduce the blocking "wait for a writer / wait for data" semantics the
;;;; synchronisation protocol in python-process.lisp relies on.
;;;;
;;;; Windows *does* have native named pipes under the \\.\pipe\ namespace, and
;;;; (verified empirically) CPython's builtin open() can open such a pipe for
;;;; writing as if it were a file. So on Windows we create the pipe as a server
;;;; here, let python connect as the client via open(path, 'w'), and expose the
;;;; read end as a Gray input stream that supports READ-CHAR, READ-CHAR-NO-HANG,
;;;; LISTEN and PEEK-CHAR -- exactly the operations read-output-with-sync uses.

#+(and win32 sbcl)
(progn

(defconstant +pipe-access-inbound+        #x00000001)
(defconstant +pipe-type-byte+             #x00000000)
(defconstant +pipe-wait+                  #x00000000)
(defconstant +pipe-unlimited-instances+   255)
(defconstant +invalid-handle-value+       -1)
(defconstant +error-pipe-connected+       535)
(defconstant +error-broken-pipe+          109)
(defconstant +error-no-data+              232)

(cffi:defcfun ("CreateNamedPipeA" %create-named-pipe) :pointer
  (name                :string)
  (open-mode           :uint32)
  (pipe-mode           :uint32)
  (max-instances       :uint32)
  (out-buffer-size     :uint32)
  (in-buffer-size      :uint32)
  (default-timeout     :uint32)
  (security-attributes :pointer))

(cffi:defcfun ("ConnectNamedPipe" %connect-named-pipe) :int
  (handle   :pointer)
  (overlapped :pointer))

(cffi:defcfun ("ReadFile" %read-file) :int
  (handle          :pointer)
  (buffer          :pointer)
  (bytes-to-read   :uint32)
  (bytes-read      :pointer)
  (overlapped      :pointer))

(cffi:defcfun ("PeekNamedPipe" %peek-named-pipe) :int
  (handle               :pointer)
  (buffer               :pointer)
  (buffer-size          :uint32)
  (bytes-read           :pointer)
  (total-bytes-avail    :pointer)
  (bytes-left-this-msg  :pointer))

(cffi:defcfun ("DisconnectNamedPipe" %disconnect-named-pipe) :int
  (handle :pointer))

(cffi:defcfun ("CloseHandle" %close-handle) :int
  (handle :pointer))

(cffi:defcfun ("GetLastError" %get-last-error) :uint32)

(defun %pipe-win32-name (lisp-path)
  "Map a FIFO path produced by %pipe-path to a \\\\.\\pipe\\ name.
Backslashes and the drive colon are stripped so the leftover characters form a
valid single pipe-name component."
  (let* ((base (file-namestring (pathname lisp-path))))
    (concatenate 'string "\\\\.\\pipe\\" base)))

(defclass windows-pipe-stream (sb-gray:fundamental-character-input-stream)
  ((handle    :initarg :handle    :accessor windows-pipe-stream-handle)
   (connected :initform nil        :accessor windows-pipe-stream-connected)
   (buffer    :initform (make-array 0 :element-type 'character
                                       :adjustable t :fill-pointer 0)
              :accessor windows-pipe-stream-buffer)
   (pos       :initform 0          :accessor windows-pipe-stream-pos)
   (eof       :initform nil        :accessor windows-pipe-stream-eof)))

(defun make-windows-named-pipe-server (lisp-path)
  "Create a named pipe server for LISP-PATH and return a WINDOWS-PIPE-STREAM.
The pipe is inbound (python writes, lisp reads). Connection to the client
(python) is established lazily on the first read."
  (let* ((name (%pipe-win32-name lisp-path))
         (handle (%create-named-pipe name
                                     +pipe-access-inbound+
                                     (logior +pipe-type-byte+ +pipe-wait+)
                                     +pipe-unlimited-instances+
                                     65536 65536 0
                                     (cffi:null-pointer))))
    (when (= (cffi:pointer-address handle)
             (ldb (byte 64 0) +invalid-handle-value+))
      (error "CreateNamedPipe failed for ~S (error ~D)" name (%get-last-error)))
    (make-instance 'windows-pipe-stream :handle handle)))

(defun %ensure-connected (stream)
  "Block until the python client connects to the pipe."
  (unless (windows-pipe-stream-connected stream)
    (let ((ok (%connect-named-pipe (windows-pipe-stream-handle stream)
                                   (cffi:null-pointer))))
      ;; ConnectNamedPipe returns 0 on failure; ERROR_PIPE_CONNECTED means the
      ;; client connected between CreateNamedPipe and ConnectNamedPipe, which is
      ;; still success.
      (when (and (zerop ok)
                 (/= (%get-last-error) +error-pipe-connected+))
        (error "ConnectNamedPipe failed (error ~D)" (%get-last-error))))
    (setf (windows-pipe-stream-connected stream) t)))

(defun %fill-from-pipe (stream &key (block t))
  "Read available bytes from the pipe into STREAM's buffer.
When BLOCK is true, performs one blocking ReadFile (used by READ-CHAR /
PEEK-CHAR). When BLOCK is nil, first peeks and only reads what is already
available (used by LISTEN / READ-CHAR-NO-HANG). Returns the number of new
characters appended, or NIL at end of pipe."
  (when (windows-pipe-stream-eof stream)
    (return-from %fill-from-pipe nil))
  (%ensure-connected stream)
  (let ((handle (windows-pipe-stream-handle stream)))
    (unless block
      ;; How many bytes can we read right now without blocking?
      (cffi:with-foreign-objects ((avail :uint32))
        (setf (cffi:mem-ref avail :uint32) 0)
        (let ((ok (%peek-named-pipe handle (cffi:null-pointer) 0
                                    (cffi:null-pointer) avail (cffi:null-pointer))))
          (when (zerop ok)
            (let ((err (%get-last-error)))
              (when (or (= err +error-broken-pipe+)
                        (= err +error-no-data+))
                (setf (windows-pipe-stream-eof stream) t)))
            (return-from %fill-from-pipe (if (windows-pipe-stream-eof stream) nil 0)))
          (when (zerop (cffi:mem-ref avail :uint32))
            (return-from %fill-from-pipe 0)))))
    (let ((want 4096))
      (cffi:with-foreign-objects ((buf :uint8 want)
                                  (nread :uint32))
        (setf (cffi:mem-ref nread :uint32) 0)
        (let ((ok (%read-file handle buf want nread (cffi:null-pointer))))
          (if (zerop ok)
              (let ((err (%get-last-error)))
                (if (= err +error-broken-pipe+)
                    (progn (setf (windows-pipe-stream-eof stream) t) nil)
                    (progn (setf (windows-pipe-stream-eof stream) t)
                           (error "ReadFile on pipe failed (error ~D)" err))))
              (let ((n (cffi:mem-ref nread :uint32)))
                (if (zerop n)
                    0
                    (let ((vec (windows-pipe-stream-buffer stream)))
                      (dotimes (i n)
                        (vector-push-extend
                         (code-char (cffi:mem-ref buf :uint8 i)) vec))
                      n)))))))))

(defun %available-chars (stream)
  (- (fill-pointer (windows-pipe-stream-buffer stream))
     (windows-pipe-stream-pos stream)))

#+sbcl
(progn
  (defmethod sb-gray:stream-read-char ((stream windows-pipe-stream))
    (loop
      (when (plusp (%available-chars stream))
        (let ((ch (aref (windows-pipe-stream-buffer stream)
                        (windows-pipe-stream-pos stream))))
          (incf (windows-pipe-stream-pos stream))
          (return ch)))
      (let ((n (%fill-from-pipe stream :block t)))
        (when (null n)
          (return :eof)))))

  (defmethod sb-gray:stream-read-char-no-hang ((stream windows-pipe-stream))
    (when (zerop (%available-chars stream))
      (let ((n (%fill-from-pipe stream :block nil)))
        (when (null n) (return-from sb-gray:stream-read-char-no-hang :eof))))
    (if (plusp (%available-chars stream))
        (let ((ch (aref (windows-pipe-stream-buffer stream)
                        (windows-pipe-stream-pos stream))))
          (incf (windows-pipe-stream-pos stream))
          ch)
        nil))

  (defmethod sb-gray:stream-peek-char ((stream windows-pipe-stream))
    ;; Blocking peek, matching PEEK-CHAR's wait-for-input behaviour.
    (loop
      (when (plusp (%available-chars stream))
        (return (aref (windows-pipe-stream-buffer stream)
                      (windows-pipe-stream-pos stream))))
      (let ((n (%fill-from-pipe stream :block t)))
        (when (null n) (return :eof)))))

  (defmethod sb-gray:stream-listen ((stream windows-pipe-stream))
    ;; Non-blocking "is data available?" check. There is an inherent race on
    ;; Windows named pipes: a PeekNamedPipe issued immediately after the writer
    ;; flushed may still report 0 bytes while the bytes are in flight. The
    ;; stdout/stderr capture protocol (see read-output-with-sync) relies on
    ;; LISTEN seeing the just-flushed output, so we retry the non-blocking peek
    ;; a few times with a tiny sleep. The budget is small and bounded, so this
    ;; cannot hang when there genuinely is no data.
    (when (zerop (%available-chars stream))
      (loop :repeat 50
            :until (or (plusp (%available-chars stream))
                       (windows-pipe-stream-eof stream))
            :do (let ((n (%fill-from-pipe stream :block nil)))
                  (when (and n (zerop n))
                    (sleep 0.001)))))
    (plusp (%available-chars stream)))

  (defmethod sb-gray:stream-unread-char ((stream windows-pipe-stream) char)
    (when (plusp (windows-pipe-stream-pos stream))
      (decf (windows-pipe-stream-pos stream))
      (setf (aref (windows-pipe-stream-buffer stream)
                  (windows-pipe-stream-pos stream))
            char))
    nil)

  (defmethod close ((stream windows-pipe-stream) &key abort)
    (declare (ignore abort))
    (when (windows-pipe-stream-handle stream)
      (ignore-errors (%disconnect-named-pipe (windows-pipe-stream-handle stream)))
      (ignore-errors (%close-handle (windows-pipe-stream-handle stream)))
      (setf (windows-pipe-stream-handle stream) nil))
    t))

) ; end #+(and win32 sbcl)
