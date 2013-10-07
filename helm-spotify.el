;;; helm-spotify.el --- Control Spotify with Helm.
;; Copyright 2013 Kris Jenkins
;;
;; Author: Kris Jenkins <krisajenkins@gmail.com>
;; Maintainer: Kris Jenkins <krisajenkins@gmail.com>
;; Keywords: helm spotify
;; URL: https://github.com/krisajenkins/helm-spotify
;; Created: 6th October 2013
;; Version: 0.1.0
;; Package-Requires: ((helm "0.0.0") (json "0.0.0") (s "0.0.0"))

;;; Commentary:
;;
;; A search & play interface for Spotify.

;;; Code:

;;; API Reference: https://developer.spotify.com/technologies/web-api/
(require 'helm)
(require 'url)
(require 'json)
(require 's)

(defun spotify-search (search-term)
  "Search spotify for SEARCH-TERM, returning the results as a Lisp structure."
  (let ((a-url (format "http://ws.spotify.com/search/1/track.json?q=%s" search-term)))
    (with-current-buffer
	(url-retrieve-synchronously a-url)
      (re-search-forward "^$" (point-max))
      (forward-char)
      (json-read-object))))

(defun spotify-format-track (track)
  "Given a TRACK, return a (\"Formatted name\" . <href>) pair."
  (let ((track-name (cdr (assoc 'name track)))
	(track-length (cdr (assoc 'length track)))
	(album-name (cdr (assoc 'name (assoc 'album track))))
	(artist-names (mapcar (lambda (artist)
				(cdr (assoc 'name artist)))
			      (cdr (assoc 'artists track)))))
    (format "%s (%dm%0.2ds)\n%s - %s"
	    track-name
	    (/ track-length 60) (mod track-length 60)
	    (s-join "/" artist-names)
	    album-name)))

(defun spotify-search-formatted (search-term)
  (let ((results (spotify-search search-term)))
    (mapcar (lambda (track)
	      (cons (spotify-format-track track) track))
	    (cdr (assoc 'tracks results)))))

(defun spotify-play-href (href)
  "Get the Spotify app to play the object with the given HREF."
  (shell-command (format "osascript -e 'tell application %S to play track %S'"
			 "Spotify"
			 href)))

(defun spotify-play-track (track)
  "Get the Spotify app to play the TRACK."
  (spotify-play-href (cdr (assoc 'href track))))

(defun spotify-play-album (track)
  "Get the Spotify app to play the album for this TRACK."
  (spotify-play-href (cdr (assoc 'href (assoc 'album track)))))

(defun helm-spotify-search ()
  (spotify-search-formatted helm-pattern))

(defun helm-spotify-pprint (form &optional output-stream)
  (princ (with-temp-buffer
	   (cl-prettyprint form)
	   (buffer-string))
	 output-stream)
  nil)

;;;###autoload
(defvar helm-source-spotify-track-search 
  '((name . "Spotify")
    (volatile)
    (delayed)
    (multiline)
    (candidates-process . helm-spotify-search)
    (action . (("Play Track" . spotify-play-track)
	       ("Play Album" . spotify-play-album)
	       ("Show Track Metadata" . helm-spotify-pprint)))))

;;;###autoload
(defun helm-spotify ()
  "Bring up a Spotify search interface in helm."
  (interactive)
  (helm :sources '(helm-source-spotify-track-search)
	:buffer "*helm-spotify*"))

(provide 'helm-spotify)
;;; helm-spotify.el ends here
