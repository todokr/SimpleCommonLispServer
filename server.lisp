(setf *terminal-encoding* charset:utf-8)
(setf *default-file-encoding* charset:utf-8)

(defun http-char (c1 c2 &optional (default #\space))
  (let ((code (parse-integer
         (coerce (list c1 c2) 'string)
         :radix 16
         :junk-allowed t)))
    ;; ascii!
    (if code
  (code-char code)
        default)))

(defun http-byte (c1 c2 &optional (default #.(char-code #\space)))
  (let ((code (parse-integer
        (coerce (list (code-char c1) (code-char c2)) 'string)
        :radix 16
        :junk-allowed t)))
    (or code default)))

(defun decode-param (s)
  (labels ((f (lst)
        (when lst
    (case (car lst)
      ;; percent encoded 2byte character
      (#.(char-code #\%) (cons (http-byte (cadr lst) (caddr lst))
             (f (cdddr lst))))
      ;; `+` as space
      (#.(char-code #\+) (cons #.(char-code #\space) (f (cdr lst))))
      ;; other
      (t (cons (car lst) (f (cdr lst))))))))
    (ext:convert-string-from-bytes
     (coerce (f (coerce (ext:convert-string-to-bytes s charset:utf-8) 'list))
       'vector)
     charset:utf-8)))

(defun parse-params (s)
  ;; e.g. name=bob&age=25&gender=male => ((NAME . "bob") (AGE . "25") (GENDER . "male"))
  (let ((i1 (position #\= s))
  (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (decode-param (subseq s 0 i1))))
        (decode-param (subseq s (1+ i1) i2)))
        (and i2 (parse-params (subseq s (1+ i2))))))
    ((equal s "") nil)
    (t s))))

(defun parse-url (s)
  ;; e.g. "GET /lolcats.html?extra-funny=yes HTTP/1.1" => ("lolcats.html" (EXTRA-FUNNY . "yes"))
  (let* ((url (subseq s
          (+ 2 (position #\space s))
          (position #\space s :from-end t)))
   (x (position #\? url)))
    (if x
  (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))

(defun get-header (stream)
  ;;e.g. "bar: abc,123" => ((FOO . "1") (BAR . "abc,123"))
  (let* ((s (read-line stream))
   (h (let ((i (position #\: s)))
        (when i
    (cons (intern (string-upcase (subseq s 0 i)))
          (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
  (read-sequence content stream)
  (parse-params content)))))

(defun serve (request-handler)
  (let ((socket (socket-server 8080)))
    (unwind-protect
      (loop (with-open-stream (stream (socket-accept socket))
        (let* ((url    (parse-url (read-line stream)))
         (path   (car url))
         (header (get-header stream))
         (params (append (cdr url)
             (get-content-params stream header)))
         (*standard-output* stream))
    (funcall request-handler path header params))))
      (socket-server-close socket))))

(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
  (if (not name)
      (princ "<html><body><form>What is your name?<input name='name' /></form></body></html>")
      (format t "<html><body>Nice to meet you, ~a!</body></html>" (cdr name))))
      (princ "Sorry... I don't know that page.")))
