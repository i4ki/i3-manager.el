;; i3-manager
;; Author: Tiago Natel de Moura <natel@secplus.com.br>
;;
;; Trying to improve the i3 control for emacs-hackers
;;

(require 'cl)
(require 'i3)
(require 'i3-integration)
(require 'tree-widget)
(require 'ido)

(defvar i3-manager-buffer-name "*i3-manager*"
  "Name of the i3-manager buffer")


;; Helper functions

(defun print-wspace (wspaces index)
  "Print workspace by INDEX in list of workspace WSPACES"
  (if (and (>= index 0) (< index (length wspaces)))
      (let* ((workspace (elt wspaces index))
             (output (i3-alist-value 'output workspace))
             (name (i3-alist-value 'name workspace)))
        (message "name: %s, output: %s" name output))
    nil))

(defun print-windows (windows)
  "Print all WINDOWS names"
  (let ((window (car windows))
        (next (cdr windows)))
    (message "name: %s" (i3-alist-value 'name window))
    (if next
        (print-windows next))))

(defun i3-manager-get-wspace (wspaces index)
  "Get workspace by INDEX in list of workspaces WSPACES.
Returns `nil` if not found"
  (if (and (>= index 0)
           (< index (length wspaces)))
      (elt wspaces index)
    nil))

(defun i3-manager-get-root-tree ()
  "Get i3 root tree.
Returns a associated list (alist) of cons"
  (i3-get-tree-layout))

(defun i3-manager-get-output-tree (root)
  "Get i3 vector of active and inactive outputs of i3 ROOT tree."
  (i3-alist-value 'nodes root))

(defun i3-manager-get-workspaces ()
  "Get the workspaces tree. Returns a vector of alist's"
  (setq workspaces '())
  (let* ((root-tree (i3-manager-get-root-tree))
         (output-vector (i3-manager-get-output-tree root-tree))
         (counter 0))
    (while (< counter (length output-vector))
      (let* ((output (elt output-vector counter))
             (output-name (i3-alist-value 'name output)))
        (if (not (string-match "^\_\_" output-name))
            ;; i3 internal names starts with __, we avoid that
            (let ((content-areas (i3-alist-value 'nodes output))
                  (area-counter 0))
              ;; content-areas are almost always `topdock`, `bottomdock`
              ;; or `content`. The last one holds workspaces.
              (while (< area-counter (length content-areas))
                (let ((content-area (elt content-areas area-counter)))
                  (if (string-equal (i3-alist-value 'name content-area)
                                    "content")
                      (setq workspaces (i3-alist-value 'nodes
                                                       content-area))))
                (setq area-counter (1+ area-counter))))))
      (setq counter (1+ counter))))
  workspaces)

(defun i3-manager-get-windows ()
  "Get i3 windows in a vector of alist containing window-name and
workspace-name.
Example result:
\\[((name . \"emacs\") (\"workspace\" . \"1:dev\"))\\]"
  (setq window-vector nil)
  (let* ((workspace-tree (i3-manager-get-workspaces))
         (wspace-counter 0))
    (while (< wspace-counter (length workspace-tree))
      (let* ((workspace (elt workspace-tree wspace-counter))
             (windows (i3-alist-value 'nodes workspace))
             (win-counter 0))
        (while (< win-counter (length windows))
          (let* ((window (elt windows win-counter))
                 (window-name (i3-alist-value 'name window))
                 (window-workspace (i3-alist-value 'name workspace))
                 (obj (vector `((name . ,window-name)
                                (workspace . ,window-workspace)))))
            (if (vectorp window-vector)
                (setq window-vector (vconcat window-vector obj))
              (setq window-vector obj)))
          (setq win-counter (1+ win-counter))))
      (setq wspace-counter (1+ wspace-counter))))
  window-vector)

(defun i3-manager-get-windows-names (windows)
  "Get a list of window's title of WINDOWS.
Use `i3-manager-get-windows` to get the list of WINDOWS objects"
  (setq window-titles '())
  (setq counter 0)
  (while (< counter (length windows))
    (let* ((window (elt windows counter))
           (window-name (i3-alist-value 'name window)))
      (setq window-titles (append window-titles (list window-name))))
    (setq counter (1+ counter)))
  window-titles)

(defun i3-manager-get-wspace-names ()
  (setq wspace-names (list))
  (setq i 0)
  (let ((wspaces (i3-get-workspaces)))
    (while (< i (length wspaces))
      (let ((workspace (elt wspaces i)))
        (setq wspace-names
              (append wspace-names
                      (list (i3-alist-value 'name workspace)))))
      (setq i (+ i 1))))
  wspace-names)

(defun i3-manager-move-to-workspace (&optional wspace-name)
  (interactive)
  (setq res nil)
  (if (not wspace-name)
      (setq wspace-name
            (ido-completing-read "Choose workspace: "
                                 (i3-manager-get-wspace-names))))
  (if wspace-name
      (setq res (i3-command 0 (format "workspace %s"
                                      wspace-name))))
  (if res
      (princ res))
  res)

(defun i3-manager-find-window (&optional window-name)
  (interactive)
  (setq res nil)
  (setq windows (i3-manager-get-windows))
  (if (not window-name)
      (setq window-name
            (ido-completing-read
             "Choose a window: "
             (i3-manager-get-windows-names windows))))
  (if window-name
      (let* ((wspace (i3-manager-get-wspace-of-window windows
                                                      window-name)))
        (if wspace
            (setq res (i3-manager-move-to-workspace wspace)))))
  res)

(defun i3-manager-new-workspace (&optional wspace-name)
  (interactive)
  (setq res nil)
  (if (not wspace-name)
      (setq wspace-name
            (read-string "Workspace name: ")))

  (if wspace-name
      (setq res (i3-command 0
                            (format "workspace %s" wspace-name))))

  (if res
      (princ res))
  res)

(defun i3-manager-get-wspace-of-window (windows name)
  (setq wspace nil)
  (setq counter 0)
  (setq found nil)
  (while (and (not found)
              (< counter (length windows)))
    (let* ((obj (elt windows counter))
           (window-name (i3-alist-value 'name obj))
           (wspace-name (i3-alist-value 'workspace obj)))
      (if (string-equal window-name name)
          (progn
            (setq wspace wspace-name)
            (setq found t))))
    (setq counter (1+ counter)))
  wspace)

;;{{{  Widget tree
;; testing ...
(defun i3-manager-tree-widget (&optional theme)
  (interactive
   (list (if current-prefix-arg
             (completing-read "Theme name: "
                              '(("default" . "default")))
           nil)))
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    (mapcar #'delete-overlay (car all))
    (mapcar #'delete-overlay (cdr all)))
  (tree-widget-set-theme theme)
  (widget-insert (format "%s. \n\n"
                         "i3 Manager Tree view"))
  (widget-create
   ;; Open this level
   'tree-widget :open t
   ;; Use a push button for this code
   :node '(push-button
           :tag "Tree"
           :format "%[%t%]\n"
           :notify
           (lambda (&rest ignore)
             (message "This is the root node")))
   ;; Add subtrees (their nodes default to items)
   '(tree-widget :tag "1:main"
                 (tree-widget :tag "vertical"
                              (item :tag "i4k@secplus ~")
                              (item :tag "root@secplus ~")))
   '(tree-widget :tag "2:www"
                 (tree-widget :tag "horizontal"
                              (item :tag "Google Chrome")
                              (item :tag "Firefox")))))

;;;###autoload
(defun i3-manager ()
  "Start i3-mode buffer"
  (interactive)
  (switch-to-buffer i3-manager-buffer-name)
  (i3-manager-tree-widget))

(provide 'i3-manager)
