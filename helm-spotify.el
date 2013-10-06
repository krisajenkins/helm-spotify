;;; See: https://developer.spotify.com/technologies/web-api/
(require 'url)
(require 'json)
(require 's)

(defun spotify-search (search-term)
  "Search spotify for term, returning the results as a Lisp structure."
  (let ((a-url (format "http://ws.spotify.com/search/1/track.json?q=%s" search-term)))
    (with-current-buffer
	(url-retrieve-synchronously a-url)
      (re-search-forward "^$" (point-max))
      (forward-char)
      (json-read-object))))

(defun spotify-format-track (track)
  "Given a track, return a (\"Formatted name\" . <href>) pair."
  (let ((track-name (cdr (assoc 'name track)))
	(album-name (cdr (assoc 'name (assoc 'album track))))
	(artist-names (mapcar (lambda (artist)
				(cdr (assoc (quote name) artist)))
			      (cdr (assoc (quote artists) track)))))
    (cons (format "%s\n%s\n%s"
		  (s-join "/" artist-names)
		  album-name
		  track-name)
	  track)))

(defun spotify-search-formatted (search-term)
  (let ((results (spotify-search search-term)))
    (mapcar #'spotify-format-track
	    (cdr (assoc 'tracks results)))))

(defun spotify-play-href (href)
  "Get the Spotify app to play the object with the given href."
  (shell-command (format "osascript -e 'tell application %S to play track %S'"
			 "Spotify"
			 href)))

(defun spotify-play-track (track)
  "Get the Spotify app to play the track."
  (spotify-play-href (cdr (assoc 'href track))))

(defun spotify-play-album (track)
  "Get the Spotify app to play the album for this track."
  (spotify-play-href (cdr (assoc 'href (assoc 'album track)))))

(defun helm-spotify-search ()
  (spotify-search-formatted helm-pattern))

;;;###autoload
(defvar helm-source-spotify-track-search 
  '((name . "Spotify")
    (volatile)
    (delayed)
    (multiline)
    (candidates-process . helm-spotify-search)
    (action . (("Play Track" . spotify-play-track)
	       ("Play Album" . spotify-play-album)
	       ("Show Track Metadata" . pprint)))))

;;;###autoload
(defun helm-spotify ()
  (interactive)
  (helm :sources '(helm-source-spotify-track-search)
	:buffer "*helm-spotify*"))
