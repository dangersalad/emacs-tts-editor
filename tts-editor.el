;;; tts-editor.el --- basic emacs customizations       -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Paul B Davis

;; Author:  <paul@dangersalad.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'json)
(require 'subr-x)

(defvar tts-editor/listen-name "tts-editor"
  "Name of the editor listener.")

(defvar tts-editor/connect-name "tts-connect"
  "Name of the TTS listener.")

(defvar tts-editor/listen-process nil
  "Process of the editor listener.")

(defvar tts-editor/connect-process nil
  "Process of the TTS listener.")

(defvar tts-editor/listen-port 39998
  "Port of the editor listener.")

(defvar tts-editor/connect-port 39999
  "Port of the TTS listener.")

(defvar tts-editor/listen-host "127.0.0.1"
  "Host of the editor listener.")

(defvar tts-editor/connect-host "127.0.0.1"
  "Host of the TTS listener.")

(defvar tts-editor/buffer-list ()
  "List of buffers being used by the editor.")

(defun tts-editor/listen-buffer-name ()
  "Get the tts editor buffer name."
  (format "*%s*" tts-editor/listen-name))

(defun tts-editor/connect-buffer-name ()
  "Get the tts connection buffer name."
  (format "*%s*" tts-editor/connect-name))

(defun tts-editor/listen-start nil
  "Start an Emacs tcp client listener for the TTS external editor API."
  (interactive)
  (setq tts-editor/listen-process (make-network-process
                                   :name tts-editor/listen-name
                                   :buffer (tts-editor/listen-buffer-name)
                                   :family 'ipv4
                                   :host tts-editor/listen-host
                                   :service tts-editor/listen-port
                                   :sentinel 'tts-editor/listen-sentinel
                                   :server 't)))

(defun tts-editor/listen-stop nil
  "Stop the Emacs tcp listener for the TTS external editor API."
  (interactive)
  (tts-editor/clear-buffers)
  (delete-process tts-editor/listen-name))

(defun tts-editor/write-to-editor-buf (&rest args)
  "Insert ARGS into the listen process buffer."
  (with-current-buffer (process-buffer tts-editor/listen-process)
    (goto-char (point-max))
    (insert (current-time-string) ": ")
    (apply 'insert args)
    (newline)
    (goto-char (point-max))))

(defun tts-editor/clear-buffers ()
  "Kill TTS script buffers."
  (dolist (buf tts-editor/buffer-list)
    (kill-buffer buf))
  (setq tts-editor/buffer-list nil))

(defun tts-editor/handle-load (scripts)
  "Handle SCRIPTS when a game save is loaded in TTS."
  (dolist (script scripts)
    (let* ((obj-name (gethash "name" script))
           
           (bufname-base (format "%s%s" obj-name (if (string= obj-name "Global") "" (format "-%s"(gethash "guid" script)))))
           (bufname-script (format "%s.lua" bufname-base))
           (bufname-ui (format "%s.xml" bufname-base))
           (data-script (gethash "script" script))
           (data-ui (gethash "ui" script)))
      
      (tts-editor/make-buffer bufname-script data-script)
      (tts-editor/make-buffer bufname-ui data-ui)
      (tts-editor/write-to-editor-buf (format "%s loaded" bufname-base)))))

(defun tts-editor/make-buffer (bufname content)
  "Make TTS editor buffer BUFNAME with CONTENT."
  (let ((script-buf (get-buffer-create (format "*tts-editor/%s*" bufname))))
    (add-to-list 'tts-editor/buffer-list script-buf t)
    (with-current-buffer script-buf
      (let ((pos (point)))
        (save-excursion
          (erase-buffer)
          (if (and content (stringp content))
              (insert content))
          (save-match-data
            (cond ((string-match-p "\\.lua$" bufname)
                   (lua-mode))
                  ((string-match-p "\\.xml$" bufname)
                   (xml-mode))))
          (tts-editor-mode))
        (goto-char pos)))))

(defun tts-editor/listen-sentinel (proc msg)
  "Handle closing the TTS external editor API listener with MSG from PROC."

  (let* ((status (process-status proc))
         (buf (process-buffer proc))
         (bufname (buffer-name buf)))
    (if (and
         (not (string= bufname (tts-editor/listen-buffer-name)))
         (equal status 'closed))
        (tts-editor/process-output-buffer buf))
    ;; (tts-editor/write-to-editor-buf bufname " " (symbol-name status))
    ))

(defun tts-editor/process-output-buffer (buf)
  "Process the contents of BUF."
  (with-current-buffer buf
    (let* ((clean-string (buffer-string))
           (json (json-parse-string clean-string
                                    :array-type 'list))
           (scripts (gethash "scriptStates" json))
           (message-id (gethash "messageID" json)))
      (cond
       ;; single object script
       ((= 0 message-id)
        (tts-editor/handle-load scripts)
        (tts-editor/write-to-editor-buf "Object Script Loaded"))
       ;; game save loaded
       ((= 1 message-id)
        (tts-editor/handle-load scripts)
        (tts-editor/write-to-editor-buf "Scripts Loaded"))
       ;; print/debug message
       ((= 2 message-id)
        (tts-editor/write-to-editor-buf (format "TTS Message: %s" (gethash "message" json))))
       ;; error message
       ((= 3 message-id)
        (tts-editor/write-to-editor-buf (format "TTS ERROR: [%s] %s %s"
                                                (gethash "guid" json)
                                                (gethash "errorMessagePrefix" json)
                                                (gethash "error" json))))
       ;; game saved
       ((= 6 message-id)
        (tts-editor/write-to-editor-buf "Game Saved"))
       
       (t
        (tts-editor/write-to-editor-buf clean-string)))
      (kill-buffer))))

(defun tts-editor/collect-scripts ()
  "Collect buffer contents as script data objects for sending to TTS."
  (let ((script-hash (make-hash-table :test 'equal)))
    (dolist (buf tts-editor/buffer-list)
      (let (obj-name
            obj-guid
            script-type
            script-data
            (bufname (buffer-name buf)))
        (when bufname
          (save-match-data
            (and (string-match "^\\*tts-editor/\\([^-.]+\\)-?\\([0-9a-f]+\\)?\\.\\(lua\\|xml\\)\\*$" bufname)
                 (setq obj-name (match-string 1 bufname)
                       obj-guid (or (match-string 2 bufname) "-1")
                       script-type (match-string 3 bufname))))
          (setq script-data (gethash obj-guid script-hash (make-hash-table :test 'equal)))
          (puthash "name" obj-name script-data)
          (puthash "guid" obj-guid script-data)
          (with-current-buffer buf
            (if (string= script-type "lua")
                (puthash "script" (buffer-string) script-data)
              (puthash "ui" (buffer-string) script-data)))
          (puthash obj-guid script-data script-hash))))
    (hash-table-values script-hash)))


(defun tts-editor/send-to-tts (data)
  "Send DATA to TTS."
  (let ((proc (open-network-stream
               tts-editor/connect-name
               (tts-editor/connect-buffer-name)
               tts-editor/connect-host
               tts-editor/connect-port)))
    (process-send-string proc (json-encode data))
    (delete-process proc)))

(defun tts-editor/save-and-play ()
  "Send scripts to TTS external and reload."
  (interactive)
  (tts-editor/write-to-editor-buf "Save and Play")
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string)
        (scripts (tts-editor/collect-scripts))
        (req (make-hash-table :test 'equal)))
    (puthash "messageID" 1 req)
    (puthash "scriptStates" scripts req)
    (tts-editor/send-to-tts req)))

(define-minor-mode tts-editor-mode
  "TTS External Editor mode."
  :lighter " TTS"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x C-s") 'tts-editor/save-and-play)
            map))


(provide 'tts-editor)
;;; tts-editor.el ends here
