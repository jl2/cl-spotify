;;;; cl-spotify.lisp
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



(defparameter *debug-print-stream* nil
  "The stream to which debug messages are logged.  If nil, no debug logging.")

(defparameter *global-connection* nil
  "The global Spotify connection.  Connect using (cls:global-connect)")


;; Scope handline
;; A note about scopes...
;; Right now the scopes listed in (scope connection) may not match the scopes
;; that we are actually authenticated for.  The only guarantee is that we
;; are authenticated for *at least* the scopes that were requested in
;; (cls:connect) or (cls:global-connect)

(defun has-scope-p (scope &optional (connection *global-connection*))
  "Query whether the connection has the specified scope."
  (find scope (scope connection)))

(defun add-scope (new-scope &optional (connection *global-connection*))
  "Add new-scope to the connection.  Re-authenticate if necessary."
  (cond
    ;; Nothing to do if the scope's already there...
    ((has-scope-p new-scope connection )
     (format *debug-print-stream* "Already have scope ~a~%" new-scope)
     connection)

    (t
     ;; Need to ask permission
     (push new-scope (scope connection))
     (disconnect connection)
     ;; Set *initializing-connection* so that the HTTP callback can access the connection
     (setf *initializing-connection* connection)

     ;; Start HTTP listener to respond to callback
     (setf (auth-server connection)
           (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port (listen-port connection))))
     ;; Open the URL authorization URL in the user's browser.
     ;; TODO: Don't use Swank...
     (swank:eval-in-emacs '(require 'eww))
     (swank:eval-in-emacs (list 'eww-browse-with-external-browser (get-auth-url connection)))
     ;; Return the new connection
     connection)))

(defun rm-scope (rm-scope &key (connection *global-connection*))
  "Remove a scope from this connection.  This is broken."
  (cond
    ((not (has-scope-p connection rm-scope))
     (format *debug-print-stream* "Don't have scope ~a~%" rm-scope)
     connection)
    (t
     (setf (scope connection) (remove rm-scope (scope connection)))
     (disconnect connection)
     (connect))))


;; Basic queries
(defun user-id (&optional (connection *global-connection*))
  "Get the current user's user-id."
  (getjso "id" (user-info connection)))

(defun user-info (&optional (connection *global-connection*))
  "Return a JSON object describing the current user. For more information: \
https://developer.spotify.com/documentation/web-api/reference/object-model/#user-object-private"
  (with-slots (user-info) connection
    (when (null user-info)
      (setf user-info (spotify-get-json connection "https://api.spotify.com/v1/me")))
    user-info))

(defun get-playlists (&key (connection *global-connection*))
  "Get a list of the current user's playlists.  For more information: \
https://developer.spotify.com/documentation/web-api/reference/playlists/get-a-list-of-current-users-playlists/"
  (sget
   (format nil "https://api.spotify.com/v1/users/~a/playlists" (user-id connection))
   :connection connection))

(defun now-playing (&key (connection *global-connection*))
  "Return currently playing track.  For more information: \
https://developer.spotify.com/documentation/web-api/reference/player/get-the-users-currently-playing-track/"
  (sget
   "https://api.spotify.com/v1/me/player/currently-playing"
   :connection connection))


(defun devices (&key (connection *global-connection*))
  (sget "https://api.spotify.com/v1/me/player/devices" :connection connection))



;; Basic player controls
(defun play (&key (connection *global-connection*))
  "Resume playing the current track."
  (sput "https://api.spotify.com/v1/me/player/play" :connection connection))

(defun pause (&key (connection *global-connection*))
  "Pause the current track."
  (sput "https://api.spotify.com/v1/me/player/pause" :connection connection))

(defun next (&key (connection *global-connection*))
  "Skip to the next track."
  (spost "https://api.spotify.com/v1/me/player/next" :connection connection))

(defun previous (&key (connection *global-connection*))
  "Skip to the previous track."
  (spost "https://api.spotify.com/v1/me/player/previous" :connection connection))



;; Iterator handling
(defun iter-backward (iterator &key (connection *global-connection*))
  "Move backwards in the sequence and return the previous block of results."
  (when-let (previous (getjso "previous" iterator))
    (sget previous :connection connection)))

(defun iter-forward (iterator &key (connection *global-connection*))
  "Move forwards in the sequence and return the next block of results."
  (when-let (forward (getjso "next" iterator))
    (sget forward :connection connection)))

(defun iter-items (iterator)
  "Return the list of items in the iterator object."
  (getjso "items" iterator))

(global-connect)
