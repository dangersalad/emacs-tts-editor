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

(defvar tts-editor/listen-name "tts-editor"
  "Name of the editor listener.")

(defvar tts-editor/listen-port 39998
  "Port of the editor listener.")

(defvar tts-editor/listen-host "127.0.0.1"
  "Host of the editor listener.")

(defvar tts-editor/buffer-list ()
  "List of buffers being used by the editor.")

(defun tts-editor/buffer-name ()
  "Get the tts editor buffer name."
  (format "*%s*" tts-editor/listen-name))

(defun tts-editor/listen-start nil
  "Start an Emacs tcp client listener for the TTS external editor API."
  (interactive)
  (make-network-process
   :name tts-editor/listen-name
   :buffer (tts-editor/buffer-name)
   :family 'ipv4
   :host tts-editor/listen-host
   :service tts-editor/listen-port
   :sentinel 'tts-editor/listen-sentinel
   :filter 'tts-editor/listen-filter
   :server 't))

(defun tts-editor/listen-stop nil
  "Stop the Emacs tcp listener for the TTS external editor API."
  (interactive)
  (delete-process tts-editor/listen-name))

(defun tts-editor/listen-filter (proc string)
  "Filter message STRING from PROC for the TTS editor API."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-from-string string))
         (scripts (gethash "scriptStates" json))
         (messageID (gethash "messageID" json)))
    (cond ((= 1 messageID)
           (tts-editor/handle-load scripts))
          ((= 0 messageID)
           (tts-editor/handle-load scripts)))
    )

  )

(defun tts-editor/handle-load (scripts)
  "Handle SCRIPTS when TTS loads a game save."
  (dolist (script scripts)
    (let* ((obj-name (gethash "name" script))
           
           (bufname-base (format "%s%s" obj-name (if (string= obj-name "Global") "" (format "-%s"(gethash "guid" script)))))
           (bufname-script (format "%s.lua" bufname-base))
           (bufname-ui (format "%s.xml" bufname-base))
           (data-script (gethash "script" script))
           (data-ui (gethash "ui" script)))
      (if (not (string= data-script ""))
          (tts-editor/make-buffer bufname-script data-script))
      (if (not (string= data-ui ""))
          (tts-editor/make-buffer bufname-ui data-ui)))
    ))

(defun tts-editor/make-buffer (bufname content)
  "Make TTS editor buffer BUFNAME with CONTENT."
  (let ((script-buf (get-buffer-create (format "*tts-editor/%s*" bufname))))
    (message "making buffer %s" bufname)
    (add-to-list 'tts-editor/buffer-list script-buf t)
    
    (with-current-buffer script-buf
      (erase-buffer)
      (insert content))))

(defun tts-editor/listen-sentinel (proc msg)
  "Handle closing the TTS external editor API listener with MSG from PROC."
  (when (string= msg "connection broken by remote peer\n")
    (message (format "client %s has quit" proc))))


(provide 'tts-editor)
;;; tts-editor.el ends here
