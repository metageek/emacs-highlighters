;; Copyright (C) 2012 John Stracke.  Released under the GNU General Public
;;  License version 2; see included COPYING file.
;;
;; https://github.com/metageek/emacs-highlighters
;;
;; Instructions: put this in your load-path, and put (require 'highlighters)
;;  in your .emacs.  Or just put (load "/...path.../highlighters.el").
;;
;; Three colors of highlighter; saves and loads when you save and load
;;  the file; switch colors with meta-+; highlight the current region
;;  with meta-_; or press shift and drag the right mouse button. To
;;  erase, set a negative prefix argument; that is, meta-- meta-_
;;  erases all highlights in the current region. For a file
;;  /foo/bar/baz.txt, highlights are saved in
;;  /foo/bar/.highlights/baz.txt.el.
;;
;; One caveat: when you copy text, highlights do not go with it. This
;;  is because I didn't use the text-properties system (since it's
;;  already used by syntax highlighting, and I was concerned about
;;  interactions); I used the overlay system. Overlays *do* take care
;;  of moving when you insert and delete text; but you will see some
;;  odd behavior when you use transpose commands.

(require 'cl)

(defgroup highlighters ()
  "Faces for use in highlighter pens"
  :group 'faces
  :tag "Highlighter pens"
  :prefix 'highlighter-)

(defface highlighter-pink
  '((((class color) (min-colors 88))
     :background "pink")
    (((class color) (min-colors 8))
     :background "red" :foreground "black")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face for a pink highlighter")

(defface highlighter-blue
  '((((class color) (min-colors 88))
     :background "lightblue1")
    (((class color) (min-colors 8))
     :background "blue" :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face for a blue highlighter")

(defface highlighter-green
  '((((class color) (min-colors 88))
     :background "palegreen")
    (((class color) (min-colors 8))
     :background "green" :foreground "black")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face for a green highlighter")

(defvar all-highlighters '(highlighter-pink
                           highlighter-blue
                           highlighter-green))

(defun string-starts-with (prefix s &optional ignore-case-p)
  (let ((lp (length prefix)))
    (and (>= (length s) lp)
         (eq (compare-strings prefix 0 lp s 0 lp ignore-case-p)
             t))))
(defun highlighter-to-tag (highlighter-symbol)
  (let ((s (symbol-name highlighter-symbol))
        (prefix "highlighter-"))
    (assert (string-starts-with prefix s t))
    (substring s (length prefix))))

(defun tag-to-highlighter (tag-string)
  (let* ((prefix "highlighter-")
         (highlighter-symbol (intern (concat prefix tag-string))))
    (if (member highlighter-symbol all-highlighters)
        highlighter-symbol
      'highlighter-pink)))

(defvar highlighter-overlays nil)
(make-variable-buffer-local 'highlighter-overlays)

(defun apply-highlighter (start end face)
  "Apply the given highlighter face to the current region."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face face)
    (push overlay highlighter-overlays)))

(defun highlights-file-name (base-file-name)
  (let* ((dirname (file-name-directory base-file-name))
         (basename (file-name-nondirectory base-file-name))
         (highlights-dir (concat dirname ".highlights/"))
         )
    (condition-case error
      (make-directory highlights-dir t)
      (file-error nil))
    (concat highlights-dir basename ".el")))

(defun current-highlights-file-name ()
  (highlights-file-name (buffer-file-name (current-buffer))))

(defun highlighter-after-save-hook ()
  (let* ((highlights
          (mapcar (lambda (overlay)
                    (list (overlay-start overlay)
                          (overlay-end overlay)
                          (highlighter-to-tag
                           (overlay-get overlay 'face))))
                  (remove-if (lambda (overlay)
                               (>= (overlay-start overlay)
                                   (overlay-end overlay)))
                             highlighter-overlays)))
         (base-file-name (buffer-file-name (current-buffer)))
         (highlights-file-name (highlights-file-name base-file-name))
         )
    (if (or highlights (file-readable-p highlights-file-name))
        (with-temp-buffer
          (prin1 highlights (current-buffer))
          (write-region (point-min) (point-max) highlights-file-name)
          ))
    )
  )

(defun highlighter-overlays-in-current-buffer (&optional start end)
  (remove-if-not
   (lambda (overlay) (member (overlay-get overlay 'face) all-highlighters))
   (overlays-in (or start (point-min))
                (or end (point-max)))))

(defun highlighter-clear-all ()
  (interactive)
  (dolist (overlay (highlighter-overlays-in-current-buffer))
    (delete-overlay overlay))
  (setq highlighter-overlays nil))

(defun highlighter-find-file-hook ()
  (highlighter-clear-all) ; just in case
  (dolist (overlay-spec
           (let* ((base-file-name (buffer-file-name (current-buffer)))
                  (highlights-file-name (highlights-file-name base-file-name))
                  )
             (if (file-readable-p highlights-file-name)
                 (with-temp-buffer
                   (insert-file-contents highlights-file-name)
                   (beginning-of-buffer)
                   (read (current-buffer))))))
    (let ((start (car overlay-spec))
          (end (cadr overlay-spec))
          (face (tag-to-highlighter (caddr overlay-spec))))
      (apply-highlighter start end face))))

(pushnew 'highlighter-after-save-hook after-save-hook)
(pushnew 'highlighter-find-file-hook find-file-hook)

(defvar current-highlighter 'highlighter-pink)

(defun next-highlighter ()
  (interactive)
  (let ((hs all-highlighters))
    (while hs
      (if (eq current-highlighter (car hs))
          (progn
            (setq current-highlighter (if (cdr hs)
                                          (cadr hs)
                                        (car all-highlighters)))
            (setq hs nil))
        (setq hs (cdr hs)))))
  (message "New highlighter: %s" (symbol-name current-highlighter)))

(defun erase-highlighter (start end)
  (dolist (overlay (highlighter-overlays-in-current-buffer start end))
    (cond
     ((and (<= start (overlay-start overlay))
           (<= (overlay-end overlay) end))
      (delete-overlay overlay)
      (setq highlighter-overlays (delq overlay highlighter-overlays)))
     ((and (>= start (overlay-start overlay))
           (<= (overlay-end overlay) end))
      (move-overlay overlay (overlay-start overlay) start))
     ((and (<= start (overlay-start overlay))
           (>= (overlay-end overlay) end))
      (move-overlay overlay end (overlay-end overlay)))
     ((and (< (overlay-start overlay) start)
           (< end (overlay-end overlay)))
        (apply-highlighter end (overlay-end overlay)
                           (overlay-get overlay 'face))
        (move-overlay overlay (overlay-start overlay) start))
     (t (error "Missed a case.")))))

(defun apply-current-highlighter (start end prefix)
  (interactive "r\np")
  (if (< prefix 0)
      (erase-highlighter start end)
      (apply-highlighter start end current-highlighter))
  (set-buffer-modified-p t)
  (setq deactivate-mark t))

(defvar *drag-event* nil)

(defun drag-highlighter ()
  (interactive)
  (flet ((text-pos (position)
          (nth 5 position)))
    (let* ((drag-event last-command-event)
           (position1 (second drag-event))
           (position2 (third drag-event))
           (text-pos1 (text-pos position1))
           (text-pos2 (text-pos position2))
           (start (min text-pos1 text-pos2))
           (end (max text-pos1 text-pos2)))
      (if (> end start)
          (apply-current-highlighter start end 1)))))

(global-set-key "\M-_" 'apply-current-highlighter)
(global-set-key "\M-+" 'next-highlighter)
(global-set-key [S-drag-mouse-3] 'drag-highlighter)

(provide 'highlighters)
