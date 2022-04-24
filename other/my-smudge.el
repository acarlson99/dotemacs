(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'smudge)
;; Settings
(setq smudge-oauth2-client-secret "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
(setq smudge-oauth2-client-id "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
(define-key smudge-mode-map (kbd "C-c .") 'smudge-command-map)
(setq smudge-transport 'connect)
(global-smudge-remote-mode)

(oauth2-token-access-token (smudge-api-retrieve-oauth2-token))

(setq page 1)
(setq callback (lambda (x) (message x)))
(setq offset (* smudge-api-search-limit (1- page)))
(defun smudge-api-user-all-tracks (callback)
  "Call CALLBACK with the list of user liked songs."
	(smudge-api-call-async "GET" "/me/tracks" nil callback)
  )

(defun smudge-api-user-tracks (page callback)
  "Call CALLBACK with the list of user liked songs."
  (let ((offset (* smudge-api-search-limit (1- page))))
	(smudge-api-call-async
	 "GET"
	 (concat "/me/tracks?"
			 (url-build-query-string `((limit  ,smudge-api-search-limit)
									   (offset ,offset))
									 nil t))
	 nil callback))
  )

(smudge-api-user-tracks 1 (lambda (x) (message x)))
(smudge-api-user-all-tracks (lambda (x) (message x)))

(setq buffer (get-buffer-create "*Featured Playlists*"))
(let ((buffer (get-buffer-create "*Featured Playlists*")))
  (smudge-api-current-user
   (lambda (user)
	 (with-current-buffer buffer
	   (smudge-playlist-search-mode)
	   (pop-to-buffer buffer)
	   (smudge-api-user-tracks
		page
		(lambda (json)
		  (if-let ((songs (smudge-api-get-playlist-tracks json)))
			  (with-current-buffer buffer
				(setq-local smudge-user-id (smudge-api-get-item-id user))
				(setq-local smudge-current-page page)
				(setq resp json)
				(smudge-track-search-print (smudge-api-get-playlist-tracks json) page)
				(message "Track view updated"))
			(message "No more tracks"))))))))

(setq items (smudge-api-get-items resp))
(setq song (car items))
(hash-table-keys (gethash 'track song))
(setq tracks (smudge-api-get-playlist-tracks resp))

;; (defun smudge-track-search-print (songs page)
;;   "Append SONGS to the PAGE of track view."
;;   (let (entries)
;; 	(dolist (song songs)
;; 	  (push (list song)
;; 			entries))
;; 	(smudge-track-search-set-list-format)
;; 	(when (eq 1 page) (setq-local tabulated-list-entries nil))
;; 	(setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
;; 	(tabulated-list-init-header)
;; 	(tabulated-list-print t)))

;; (defun smudge-track-select-queue ()
;;   "Play the track under the cursor.
;; If the track list represents a playlist, the given track is played in the
;; context of that playlist; if the track list represents an album, the given
;; track is played in the context of that album.  Otherwise, it will be played
;; without a context."
;;   (interactive)
;;   (let* ((track (tabulated-list-get-id))
;;          (context (cond ((bound-and-true-p smudge-selected-playlist) smudge-selected-playlist)
;;                         ((bound-and-true-p smudge-selected-album) smudge-selected-album)
;;                         (t nil))))
;;     (smudge-controller-play-track track context)))
