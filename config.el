;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "MesloLGS Nerd Font Mono" :size 20 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "MesloLGS Nerd Font Propo" :size 20)
      doom-unicode-font (font-spec :family "MesloLGS Nerd Font" :size 20))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'catppuccin)
(setq catppuccin-flavor 'frappe) ;; or 'latte, 'macchiato, or 'mocha

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Customisations to org-mode according to Rainer Konig's org-mode course
(after! org
  ;; Record keyword changes in logbook
  (setq org-todo-keywords
    '((sequence
      "TODO(t@/!)"  ; A task that needs doing & is ready to do
      "PROJ(p@/!)"  ; A project, which usually contains other tasks
      "WAIT(w@/!)"  ; Something external is holding up this task
      "HABIT(H)"    ; Habit
      "SOMEDAY(s)"  ; Someday/maybe
      "WEEKLY(W)"   ; Weekly review
      "MONTHLY(M)"  ; Monthly review
      "SEASONAL(S)" ; Seasonal review
      "ANNUAL(A)"   ; Annual review
      "DONE(d@/!)"  ; Task successfully completed
     )))
  ;; Leave quick notes in logbook
  (setq org-log-into-drawer 'LOGBOOK)
  ;; Leave note in logbook when task done
  (setq org-log-done 'note)
  ;; Leave note in logbook if task is rescheduled
  (setq org-log-reschedule 'note)
  (setq org-tags-exclude-from-inheritance '("journal")))

;; Refresh agenda folder
(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files '("~/OneDrive/emacs-PKM/pages/agenda")))

;; Build the agenda list the first time for the session
(my/org-roam-refresh-agenda-list)

(defun add-to-roam-file-hooks (FUNCTION)
  "Adds function to crud event hooks for org roam files only"

  ;; for file saving
  (advice-add
    #'org-roam-db-autosync--setup-file-h :after
    (lambda ()
      (add-hook 'after-save-hook
       (lambda ()
     (when org-roam-db-update-on-save
       (funcall FUNCTION))))
      nil t))

  ;; for file renaming
  (advice-add
    #'org-roam-db-autosync--rename-file-a :after
    (lambda (old-file new-file-or-dir &rest _args)
      (let ((new-file (if (directory-name-p new-file-or-dir)
              (expand-file-name (file-name-nondirectory old-file) new-file-or-dir)
                new-file-or-dir)))
        (setq new-file (expand-file-name new-file))
        (when (org-roam-file-p new-file)
      (funcall FUNCTION)))))

  ;; for file deleting
  (advice-add
    #'org-roam-db-autosync--delete-file-a :before
    (lambda (file &optional _trash)
        (when (and (not (auto-save-file-name-p file))
          (not (backup-file-name-p file))
          (org-roam-file-p file))
         (funcall FUNCTION)))))

;; Refresh agenda list automatically when new org roam files are added
(add-to-roam-file-hooks #'my/org-roam-refresh-agenda-list)

;; Custom agenda views
(setq org-agenda-custom-commands
  '(
    ("z" "Today"
        ((agenda "" ((org-agenda-span 'day)
                     (org-agenda-start-day "+0d")
                     (org-super-agenda-groups
                         '(
                           (:name "Today's schedule"
                            :time-grid t
                            :todo "TODO"
                            :order 0)
                           (:name "Habit tracker"
                            :todo "HABIT"
                            :order 1)
                           (:discard (:anything t))))))
        (todo "" ((org-agenda-overriding-header "")
                  (org-super-agenda-groups
                   '((:name "Waiting for"
                      :todo "WAIT"
                      :order 0)
                     (:name "Errands"
                      :tag "errands"
                      :order 1)
                     (:discard (:anything t))))))
        )
    )
    ("p" "Plan for tomorrow"
        ((agenda "" ((org-agenda-span 2)
                     (org-agenda-start-day "+0d")
                     (org-super-agenda-groups
                         '(
                           (:name "Schedule"
                            :time-grid t
                            :todo "TODO"
                            :discard (:todo "HABIT")
                            :order 0)
                           (:name "Habit tracker"
                            :todo "HABIT"
                            :order 1)
                           (:discard (:anything t))))))
        (todo "" ((org-agenda-overriding-header "")
                  (org-super-agenda-groups
                   '((:name "Waiting for"
                      :todo "WAIT"
                      :order 0)
                     (:name "Errands"
                      :tag "errands"
                      :order 1)
                     (:discard (:anything t))))))
        )
    )
    ("w" "Weekly review"
        ((agenda "" ((org-agenda-span '14)
                     (org-agenda-start-day "-7d")
                     (org-super-agenda-groups
                         '(
                           (:name ""
                            :time-grid t
                            :todo "TODO"
                            :discard (:todo "HABIT")
                            :order 0)
                           (:discard (:anything t))))))
        (todo "" ((org-agenda-overriding-header "")
                  (org-super-agenda-groups
                   '((:name "Waiting for"
                      :todo "WAIT"
                      :order 0)
                     (:name "Projects"
                      :todo "PROJ"
                      :order 1)
                     (:discard (:anything t))))))
        (todo "" ((org-agenda-overriding-header "")
                  (org-agenda-files '("~/OneDrive/emacs-PKM/pages/agenda/Single Actions.org"))
                  (org-super-agenda-groups
                   '((:auto-outline-path t)))))
        (todo "" ((org-agenda-overriding-header "")
                  (org-agenda-files '("~/OneDrive/emacs-PKM/pages/Someday Maybe.org"))
                  (org-super-agenda-groups
                   '((:name "Someday/Maybe"
                      :todo "SOMEDAY"
                      :order 0)
                     (:discard (:anything t))))))
        )
    )
  )
)

(org-super-agenda-mode)

;; Customisations to org-roam
(use-package! org-roam
  :custom
  ;; Set org roam directorry
  (org-roam-directory "~/OneDrive/emacs-PKM")
  (org-roam-dailies-directory "journals")
  (org-roam-completion-everywhere t)
  (org-roam-file-exclude-regexp "\\.st[^/]*\\|logseq/.*$")
  (org-roam-capture-templates
    '(("d" "default" plain "%?"
      :target (file+head "pages/${title}.org" "#+title: ${title}\n")
      :unnarrowed t)
      ("p" "project" plain
      (file "~/OneDrive/03 Resources/Org Roam Capture Templates/project-template.org")
      :target (file+head "pages/agenda/projects${title}.org" "#+title: ${title}\n")
      :unnarrowed t)
      ("w" "weekly review" plain
      (file "~/OneDrive/03 Resources/Org Roam Capture Templates/weekly-review-template.org")
      :target (file+head "pages/agenda/reviews/${title}.org" "#+title: ${title}\n")
      :unnarrowed t)
    )
  )
  (org-roam-dailies-capture-templates
    '(("d" "default" entry "* %?"
      :target (file+head "%<%Y_%m_%d>.org" "#+title: %<%Y-%m-%d>\n"))
      ("n" "new day" plain
      (file "~/OneDrive/03 Resources/Org Roam Capture Templates/daily-template.org")
      :target (file+head "%<%Y_%m_%d>.org" "#+title: %<%Y-%m-%d>\n"))
     )
  )
)

;; Allow refiling to non-agenda files
(defun ndk/org-refile-candidates ()
  (directory-files-recursively "~/OneDrive/emacs-PKM" "^[[:alnum:]].*\\.org\\'"))

(add-to-list 'org-refile-targets '(ndk/org-refile-candidates :maxlevel . 3))

;; org-roam-ql
(use-package! org-roam-ql
  ;; Simple configuration
  :after (org-roam)
  :bind ((:map org-roam-mode-map
               ;; Have org-roam-ql's transient available in org-roam-mode buffers
               ("v" . org-roam-ql-buffer-dispatch)
               :map minibuffer-mode-map
               ;; Be able to add titles in queries while in minibuffer.
               ;; This is similar to `org-roam-node-insert', but adds
               ;; only title as a string.
               ("C-c n i" . org-roam-ql-insert-node-title))))


;; dendroam
(use-package! dendroam
  :after org-roam)

;; Add dendroam to current org roam node display template
(setq org-roam-node-display-template
        (format "%s %s ${hierarchy}:${title}"
                (propertize "${doom-type:14}" 'face 'font-lock-keyword-face)
                (propertize "${tags:12}" 'face '(:inherit org-tag :box nil))))

;; org-roam-ui
(use-package! websocket
  :after org-roam)

;; org-roam-ui
(use-package! org-roam-ui
  :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; org-transclusion
(use-package! org-transclusion
  :after org
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

(cl-defmethod org-roam-node-related-nodes ((node org-roam-node))
  "Return the RELATED_NODES for NODE."
  (cdr (assoc-string "RELATED_NODES" (org-roam-node-properties node))))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
