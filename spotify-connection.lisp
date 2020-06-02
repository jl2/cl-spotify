;;;; spotify-connection.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :cl-spotify)

;; Spotify client information
;; This file contains the client information received when registering the application
;; with Spotify.  It is required to access the Spotify API.
(defparameter *client-file* (asdf:system-relative-pathname :cl-spotify ".spotify-client")
  "Path to a JSON file containing an object with client_id and client_secret entries.")

;; Cached authorization
;; This file contains a cached version of the JSON object received during authorization.
(defparameter *auth-file* (asdf:system-relative-pathname :cl-spotify ".spotify-auth")
  "Path to the auth token cache file.")

;; HTML shown to the user after authorization.
(defparameter *close-html* (asdf:system-relative-pathname
                            :cl-spotify "html/close.html")
  "Path to the HTML file that the user is redirected to after authenticating.")

(defparameter *all-scopes*
  '(
    :ugc-image-upload
    :user-read-playback-state
    :user-modify-playback-state
    :user-read-currently-playing
    :streaming
    :app-remote-control
    :user-read-email
    :user-read-private
    :playlist-read-collaborative
    :playlist-modify-public
    :playlist-read-private
    :playlist-modify-private
    :user-library-modify
    :user-library-read
    :user-top-read
    :user-read-recently-played
    :user-follow-read
    :user-follow-modify
    )
  "A list of all available scopes.")

(defparameter *initializing-connection* nil
  "The spotify-connection that initiated authorization.  This object will be updated after \
access is granted and Spotify redirects the user to our local server.")

(defparameter *spotifyd* nil
  "The shared spotifyd process.")

(defun auth-request-header ()
  "Read client ID and secret from .spotify-client and return an authorization header."
  (with-input-from-file (ins *client-file*)
    (let ((json (read-json ins)))
      (cons
       "Authorization"
       (format
       nil
        "Basic ~a"
        (base64:string-to-base64-string
         (format nil "~a:~a"
                 (getjso "client_id" json)
                 (getjso "client_secret" json))))))))

(defun get-client-id ()
  "Return the client_id from .spotify-client."
  (with-input-from-file (ins *client-file*)
    (let ((json (read-json ins)))
      (getjso "client_id" json))))

(defclass spotify-connection ()
  ((auth-state :initarg :auth-state :initform (format nil "~a" (random 10000000000)) :accessor auth-state)
   (listen-port :initarg :listen-port :initform 4040 :accessor listen-port)
   (redirect-url :initarg :redirect-url :accessor redirect-url)
   (scope :initarg :scope :initform nil :accessor scope)
   (auth-server :initform nil :accessor auth-server)
   (stream :initform nil)
   (cookies :initform (make-instance 'drakma:cookie-jar) :accessor cookies)
   (expiration :initform nil :accessor expiration)
   (auth-header :initarg :auth-header :initform nil :accessor auth-header)
   (auth-token :initarg :auth-token :initform nil :accessor auth-token)
   (user-info :initform nil))
  (:documentation "A connection to the Spotify API."))

(defun create-auth-header (auth-token)
  "Create an HTTP authorization header from a JSON authorization token."
  (cons "Authorization"
        (format
         nil
         "~a ~a"
         (getjso "token_type" auth-token)
         (getjso "access_token" auth-token))))

(defun get-auth-url (connection)
  "Return the headers required to initiate authorization.  For more information: \
https://developer.spotify.com/documentation/general/guides/authorization-guide/"
  (let ((header-list (list (cons "response_type" "code")
                           (cons "client_id" (get-client-id))
                           (cons "scope" (scope-as-string connection))
                           (cons "state" (auth-state connection))
                           (cons "redirect_uri" (redirect-url connection)))))
    (format nil "https://accounts.spotify.com/authorize?~a"
            (drakma::alist-to-url-encoded-string header-list
                                                 :utf-8
                                                 #'drakma:url-encode))))

(defun scope-as-string (connection)
  (string-downcase (format nil "~{~a~^,~}" (scope connection))))

(defun init-new-connection (scope port)
  "Initialize a new connection and start the background server to listen \
for Spotify's redirect."

  (let* ((connection (make-instance 'spotify-connection
                                    :scope (ensure-list scope)
                                    :listen-port port
                                    :redirect-url (format nil "http://localhost:~a/" port))))

    ;; Set *initializing-connection* so that the HTTP callback can access the connection
    (setf *initializing-connection* connection)

    ;; Start HTTP listener to respond to callback
    (setf (auth-server connection)
          (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                            :port (listen-port connection)
                                            :message-log-destination nil
                                            :access-log-destination nil
                                            :persistent-connections-p nil
                                            :name "Spotify callback handler.")))

    ;; Open the URL authorization URL in the user's browser.
    ;; TODO: Don't use Swank...
    (uiop:run-program (format nil "xdg-open \"~a\"" (get-auth-url connection)))
    ;; (slynk:eval-in-emacs (list 'eww-browse-with-external-browser (get-auth-url connection)))

    ;; Return the new connection
    connection))


(defun connect (&key (scope nil) (port 4040) (use-cached-auth t))
  "Create an authenticated Spotify connection.  Initiate authorization, if necessary."

  (cond (;; Can't find client identification file
         (not (uiop:file-exists-p *client-file*))
         (error (format nil "Cannot read client information from ~s" *client-file*)))

        ;; Already authenticated, so create a connection using the cached token
        ((and use-cached-auth (uiop:file-exists-p *auth-file*))
         (let ((auth-token (read-auth-token)))
           (refresh-connection
            (make-instance 'spotify-connection
                           :scope (ensure-list scope)
                           :listen-port port
                           :redirect-url (format nil "http://localhost:~a/" port)
                           :auth-token auth-token
                           :auth-header (create-auth-header auth-token)))))

        ;; No cached token, so initiate authorization
        (t
         (init-new-connection scope port))))

(defun disconnect (connection)
  "Close network connections for this connection."
  (with-slots (auth-server stream) connection
    (when auth-server
      (hunchentoot:stop auth-server))
    (when stream
      (close  stream))))


(defun start-spotifyd ()
  (when (or (null *spotifyd*)
            (not (uiop:process-alive-p *spotifyd*)))
    (setf *spotifyd* (uiop:launch-program "spotifyd --no-daemon"))))

(defun stop-spotifyd ()
  (when (and *spotifyd* (uiop:process-alive-p *spotifyd*))
    (uiop:terminate-process *spotifyd*)
    (uiop:wait-process *spotifyd*)
    (setf *spotifyd* nil)))

(defun global-connect (&key
                         (scope *all-scopes*)
                         (port 4040)
                         (use-cached-auth t)
                         (start-spotifyd nil))
  "Create the default Spotify connection."
  (when start-spotifyd
    (start-spotifyd))
  (setf *global-connection* (connect :scope scope :port port :use-cached-auth use-cached-auth)))

(defun global-disconnect ()
  "Disconnect the default Spotify connection."
  (stop-spotifyd)
  (disconnect *global-connection*))

(define-condition authorization-error (error)
  ((error :initarg :error :reader auth-error)
   (error-description :initarg :description :reader description))
  (:report
   (lambda (condition stream)
     (with-slots (error error-description) condition
     (format stream
             "Authorization Error~%Error: ~a~%Description: ~a~%"
             error error-description))))
  (:documentation "An authorization error."))

(define-condition regular-error (error)
  ((status :initarg :status :reader status)
   (message :initarg :message :reader message))
  (:report
   (lambda (condition stream)
     (with-slots (status message) condition
     (format stream
             "Spotify Error~%Status: ~a~%Message: ~a~%"
             status message))))
  (:documentation "Generic Spotify API error."))

(define-condition http-error (error)
  ((code :initarg :code :reader code)
   (headers :initarg :headers :reader headers)
   (url :initarg :url :reader url)
   (message :initarg :message :reader message))
  (:report
   (lambda (condition stream)
     (with-slots (code headers url message) condition
     (format stream
             "HTTP Error~%~a~%~a~%~a~%~a~%"
             code headers url message))))
  (:documentation "An HTTP error from Spotify."))


(defun save-auth-token (json-token)
  "Save json-token to the authorization token cache file."
  (with-output-to-file (outs *auth-file* :if-exists :supersede)
    (format outs "~a" json-token))
  json-token)

(defun read-auth-token ()
  "Read a saved authorization token from the cache file."
  (with-input-from-file (ins *auth-file*)
    (st-json:read-json ins)))

(defun get-auth-token (code connection)
  "Request an authorization token from Spotify."
  (with-slots (redirect-url) connection
    (let ((content (drakma::alist-to-url-encoded-string
                    (list (cons "grant_type" "authorization_code")
                          (cons "code" code)
                          (cons "redirect_uri" redirect-url))
                    :utf-8
                    #'drakma:url-encode)))
      (spotify-get-json connection
                        "https://accounts.spotify.com/api/token"
                        :type :post
                        :extra-headers (list (auth-request-header))
                        :content content
                        :send-auth-header nil
                        :skip-refresh t
                        :skip-server-clean t))))

(hunchentoot:define-easy-handler (authorize :uri "/") (code state)
  "Spotify redirects the user here when authorization is granted.  This handler \
finishes the initialization of *initializing-connection* by filling in the \
authentication information and saving the auth token."

  (with-slots (redirect-url auth-state auth-token auth-header) *initializing-connection*

    ;; Check state variable, used to validate requests
    (when (not (string= auth-state state))
      (error "State does not match!"))
    (let ((json-token (get-auth-token code *initializing-connection*)))
    ;; Convert relative expires_in into absolute expire_time and store it in the JSON object

      (setf (getjso "expire_time" json-token)
            (format-timestring nil
                               (timestamp+
                                (local-time:now)
                                (getjso "expires_in" json-token)
                                :sec)))
      (save-auth-token json-token)
      (setf auth-token json-token)
      (setf auth-header (create-auth-header json-token)))
    (setf *initializing-connection* nil)

    ;; Return HTML telling the user they are authorized and can close the browser window.
    (setf (hunchentoot:content-type*) "text/html")
    (alexandria:read-file-into-string *close-html*)))

(defun http-error-lookup (code)
  "Map error codes to Spotify error messages."
  (assoc-value
   '(
     (400 . "Bad Request - The request could not be understood by the server \
due to malformed syntax. The message body will contain more information; \
see Response Schema.")

     (401 . "Unauthorized - The request requires user authentication or, if \
the request included authorization credentials, authorization has been \
refused for those credentials.")

     (403 . "Forbidden - The server understood the request, but is refusing \
to fulfill it.")

     (404 . "Not Found - The requested resource could not be found. This error \
can be due to a temporary or permanent condition.")

     (429 . "Too Many Requests - Rate limiting has been applied.")

     (500 . "Internal Server Error.")

     (502 . "Bad Gateway - The server was acting as a gateway or proxy and \
received an invalid response from the upstream server.")

     (503 . "Service Unavailable - The server is currently unable to handle \
the request due to a temporary condition which will be alleviated after some \
delay. You can choose to resend the request again."))

   code))

(defun check-error (json-response)
  "Raise an error for Spotify 'status' and 'error' JSON responses."
  (cond
    ((getjso "status" json-response)
     (error 'regular-error
            :status (getjso "status" json-response)
            :message (getjso "message" json-response)))
    ((getjso "error" json-response)
         (error 'authorization-error
                :error (getjso "error" json-response)
                :description (getjso "error_description" json-response)))
    (t
     json-response)))

(defun sget (url &key (connection *global-connection*))
  "Authenticated GET request for url."
  (to-object (spotify-get-json connection url :keep-alive t :type :get)))

;; TODO: Add content parameter
(defun sput (url &key (connection *global-connection*))
  "Authenticated PUT request for url."
  (spotify-get-json connection url :keep-alive nil :type :put))

;; TODO: Add content parameter
(defun spost (url &key (connection *global-connection*))
  "This function is stupid right now.  It needs content to post..."
  (spotify-get-json connection url :keep-alive nil :type :post))

(defun spotify-get-json (connection url &key
                                          extra-headers
                                          keep-alive
                                          (type :get)
                                          (content nil)
                                          (send-auth-header t)
                                          (skip-refresh nil)
                                          (skip-server-clean nil))
  "Very ugly function that handles all of the HTTP requests to Spotify \
services, including authentication, refreshing authentication, cookies, keepalive, \
JSON parsing, etc."

  ;; Refresh connection and clean up before making request
  (with-slots (auth-server stream) connection

    ;; Check if authentication needs to be refreshed, unless the check
    ;; is being skipped (probably because refresh is in progress and
    ;; this request is requesting a refresh token)

    ;; This is a race condition (the connection can expire between refresh-connection
    ;; and the HTTP request), but it isn't really necessary (the API will throw a 401,
    ;; the connection will be refreshed, and the request will be retried) but it saves
    ;; an HTTP request if the auth token has already expired.
    (when (not skip-refresh)
      (refresh-connection connection))

    ;; If there's a stream, but keep-alive is disabled then the stream
    ;; needs to be closed to free up resources.
    (when (and stream (not keep-alive))
      (close stream)
      (setf stream nil))

    ;; Shutdown the redirect listener server if it's still running, unless
    ;; the check is being skipped (probably because this request is coming
    ;; from the listener, for the auth token.)
    (when (and auth-server (not skip-server-clean))
      (hunchentoot:stop auth-server)
      (setf auth-server nil)))

  ;; Try multiple times to handle authentication refresh and keep alive timeouts
  (loop for attempts upto 3 do
       (with-slots (auth-header stream cookies retries) connection
         (handler-case
             (let ((headers (concatenate 'list
                                         (when send-auth-header (list auth-header))
                                         (ensure-list extra-headers)))
                   (content-string (if content
                                       (format nil "~a" content)
                                       "")))
               (when *debug-print-stream*
                 (format *debug-print-stream* "Method: ~a~%URL: ~a~%Content: ~a~%Headers: ~a~%~%"
                         type url content headers))
               (multiple-value-bind (body resp-code headers url req-stream must-close response)
                   ;; ugh
                   (drakma:http-request
                    url
                    :method type
                    :keep-alive keep-alive
                    :close (not keep-alive)
                    :accept "application/json"
                    :additional-headers headers
                    :user-agent "cl-spotify"
                    :want-stream nil
                    :content-length (length content-string)
                    :content content-string
                    :stream stream
                    :cookie-jar cookies)
                 (declare (ignorable body))
                 (when *debug-print-stream*
                   (format *debug-print-stream*
                           "Headers:~%~a~%Response Code: ~a~%Response: ~a~%URL: ~a~%Body ~a~%~%"
                           headers resp-code response url body))
                 (unwind-protect
                      (cond
                        ;; 200 -> Return JSON results
                        ((= resp-code 200)
                         (setf stream req-stream)
                         (let ((json-response
                                (read-json-from-string
                                 (flexi-streams:octets-to-string body :external-format :utf-8))))
                           (check-error json-response)
                           (return-from spotify-get-json json-response)))

                        ;; No results to read, but not an error, so return the response code
                        ((or (= resp-code 201)
                             (= resp-code 202)
                             (= resp-code 204)
                             (= resp-code 304))
                         (return-from spotify-get-json resp-code))

                        ((and (= resp-code 401) (< attempts 2))
                         ;; Authentication error, try to refresh and try again
                         (refresh-connection connection))

                        ;; Everything else is an error
                        (t
                         (error 'http-error
                                :code resp-code
                                :headers headers
                                :url url
                                :message (http-error-lookup resp-code))))
                   (when must-close
                     (when req-stream
                       (close req-stream))
                     (when stream
                       (close stream))
                     (setf stream nil)))))
           (drakma:drakma-error (err)
             (declare (ignorable err))
             (when *debug-print-stream*
               (format *debug-print-stream* "~a Keep alive expired!~%" err))
             (reset-connection connection))))))

(defun expired-p (connection)
  "Check if the connection has expired."
  (with-slots (auth-token) connection
    (let ((exp-time (parse-timestring (getjso "expire_time" auth-token))))
      (timestamp< exp-time (local-time:now)))))

(defun reset-connection (connection)
  "Close any open streams this connection has open."
  (with-slots (stream) connection
    (when stream
      (close stream)
      (setf stream nil))))

(defun refresh-connection (connection)
  "Request a refreshed authentication token and save it to disk."
  (with-slots (auth-token auth-header) connection

    (let ((exp-time (parse-timestring (getjso "expire_time" auth-token)))
          (ref-token (getjso "refresh_token" auth-token)))

      ;; Only refresh if the current auth is expired.
      (when (timestamp< exp-time (local-time:now))
        (reset-connection connection)
        (let* ((req-data (drakma::alist-to-url-encoded-string
                          (list
                           (cons "grant_type" "refresh_token")
                           (cons "refresh_token" ref-token))
                          :utf-8
                          #'drakma:url-encode))
               (json-token (spotify-get-json
                            connection
                            "https://accounts.spotify.com/api/token"
                            :type :post
                            :content req-data
                            :skip-refresh t
                            :keep-alive nil
                            :extra-headers (list (auth-request-header))
                            :send-auth-header nil)))
          (when-let (jtoken (getjso "refresh_token" json-token))
            (setf (getjso "refresh_token" auth-token) jtoken))

          (when *debug-print-stream*
            (format *debug-print-stream* "Refresh token: ~a~%" json-token))

          (setf (getjso "expire_time" auth-token)
                (format-timestring nil
                                   (timestamp+
                                    (local-time:now)
                                    (getjso "expires_in" json-token)
                                    :sec)))

          (when *debug-print-stream*
            (format *debug-print-stream* "json-token: ~a~%" json-token)
            (format *debug-print-stream* "auth-token: ~a~%" auth-token))
          (save-auth-token auth-token)
          (setf auth-header (create-auth-header json-token))))))
  connection)
