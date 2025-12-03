;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")
(setq user-full-name "Dillon Draper"
      user-mail-address "dillonbdraper@gmail.com")

;; Fix: Reload envrc after startup (timing workaround)
;; Doom's direnv module should handle this, but restored sessions need a nudge
(add-hook 'window-setup-hook
          (lambda ()
            (run-with-timer
             0.5 nil
             (lambda ()
               (when (and (bound-and-true-p envrc-global-mode)
                          (fboundp 'envrc-reload-all))
                 (envrc-reload-all))))))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;; Blink cursor
(blink-cursor-mode 1)

(add-to-list 'default-frame-alist '(undecorated . t))

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(96 . 97))
(add-to-list 'default-frame-alist '(alpha . (96 . 97)))
;; Speed of which-key popup
(setq which-key-idle-delay 0.2)
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec :family "Fira Code" :size 16))
(setq doom-variable-pitch-font (font-spec :family "Alegreya" :size 18))
(setq  doom-big-font (font-spec :family "Fira Code" :size 22))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
(setq doom-theme 'modus-operandi-tinted)  ; TEMP DISABLED FOR DEBUG

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


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

(set-face-foreground 'line-number "#F0F0F0")
(set-face-foreground 'line-number-current-line "#6AE505")
(after! treemacs
  (setq treemacs-follow-mode t))

;; Function to get secrets from SOPS-encrypted YAML file
(defun my-get-sops-secret (file key)
  "Extract KEY from SOPS-encrypted YAML FILE."
  (string-trim
   (shell-command-to-string
    (format "sops -d --extract '[%s]' %s" key (shell-quote-argument file)))))

(setq gemini-key (my-get-sops-secret "/home/dillon/dbd_nixos_config/secrets/secrets.yaml" "\"gemini-key\""))

(setq mistral-key (my-get-sops-secret "/home/dillon/dbd_nixos_config/secrets/secrets.yaml" "\"mistral-key\""))

(use-package! aidermacs
  :config
  (map! :leader
        (:prefix ("o" . "Open")
                 (:prefix ("l" . "llm")
                  :desc "Open aidermacs menu" "i" #'aidermacs-transient-menu)))

  (setenv "GEMINI_API_KEY" gemini-key)
  :custom
  ;; See the Configuration section below
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "gemini/gemini-3-pro-preview")
  (aidermacs-extra-args '("--no-gitignore")))

(defun my/aider-add-current-buffer-read-only ()
  "Adds the current buffer to the running Aider session as read-only.
   Searches for a buffer starting with *aidermacs."
  (interactive)
  (let ((file-path (buffer-file-name))
        (aider-buf (seq-find (lambda (b) (string-prefix-p "*aidermacs" (buffer-name b)))
                             (buffer-list))))
    (unless file-path
      (error "Buffer is not visiting a file"))
    (if aider-buf
        (with-current-buffer aider-buf
          (process-send-string (get-buffer-process aider-buf)
                               (format "/read %s\n" file-path))
          (message "Added %s to Aider as read-only context." file-path))
      (error "No active Aider session found (looked for buffer starting with *aidermacs)."))))

(defun my/goto-paragraph-start ()
  "Move the cursor to the first character of the current paragraph."
  (interactive)
  (backward-paragraph)
  (skip-chars-forward " \t\n"))

(defun my/goto-paragraph-end ()
  "Move the cursor to the last character of the current paragraph."
  (interactive)
  (forward-paragraph)
  (skip-chars-backward " \t\n"))

(map! :nv "gk" #'my/goto-paragraph-start
      :nv "gj" #'my/goto-paragraph-end)

(use-package! gptel
  :config
  (let ((gptel-mistral (gptel-make-openai "mistral"
                         :host "api.mistral.ai"
                         :endpoint "/v1/chat/completions"
                         :protocol "https"
                         :key mistral-key
                         :models '("mistral-small")))
        (gptel-gemini (gptel-make-gemini "Gemini"
                        :key gemini-key
                        :stream t)))

    ;; Add them to the list of available backends
    (setq gptel-backends (list gptel-gemini gptel-mistral))

    ;; Set the default backend and model
    (setq gptel-backend gptel-gemini
          gptel-model 'gemini-pro-latest)))

(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(".*\\.heex$" . "elixir")))
(setq display-time-format '("%I:%M %p"))

;; --- TSX mode association ---
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))


;; --- LSP Configuration and Hooks ---
(after! lsp-mode
  ;; FIX 2: Ensure LSP client starts for tsx-ts-mode
  ;; Manually hook the lsp-mode activation to the TSX mode for reliability.
  (add-to-list 'lsp-language-id-configuration '(tsx-ts-mode . "typescriptreact"))



  ;; --- ESLint Configuration for Client Subdirectory ---
  ;; 1. Global setting: Tell LSP to use the ESLint plugin via the TypeScript Server
  ;; This assumes you have 'typescript-language-server' running and 'eslint' installed
  ;; in your 'client/node_modules'.
  (setq lsp-typescript-server-plugin-eslint-enable t)

  (defun my/set-client-root-for-lsp ()
    "When inside the client directory, set the LSP workspace root to the client directory.
     This allows ESLint and the TS Server to find the correct config files (client/eslint.config.js)."
    (let* ((root (projectile-project-root))
           (client-dir (concat root "client/"))
           ;; Use file-truename to resolve symlinks and ensure proper comparison
           (current-dir (file-truename default-directory)))

      ;; Check if the current file's directory is a subdirectory of 'client/'
      (when (string-prefix-p client-dir current-dir)
        ;; We are inside the client directory. Set it as the LSP workspace root.
        (setq-local lsp-session-workspace-root client-dir)

        (message "LSP root set to client directory for ESLint: %s" client-dir))))

  ;; 2. Attach the configuration function to the relevant hooks
  (add-hook 'typescript-mode-hook #'my/set-client-root-for-lsp)
  (add-hook 'tsx-ts-mode-hook #'my/set-client-root-for-lsp)
  (add-hook 'js-mode-hook #'my/set-client-root-for-lsp)
  (add-hook 'js-ts-mode-hook #'my/set-client-root-for-lsp))

(add-hook 'tsx-ts-mode-hook #'lsp-deferred)

(after! elixir-mode
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'elixir-mode-hook #'lsp-go-install-save-hooks))

(after! projectile
  (add-to-list 'projectile-globally-ignored-files ".aider.chat.history.md"))

;;; Helper function to spawn a named vterm buffer in a specific directory
(defun my/run-in-vterm (buffer-name dir cmd &optional background)
  "Checks for an existing vterm buffer. If missing, creates it and runs command.
   If BACKGROUND is non-nil, the buffer is created/started without switching focus."
  (let* ((buf-name (format "*%s*" buffer-name))
         (buffer (get-buffer buf-name)))
    (if (and buffer (buffer-live-p buffer))
        ;; Buffer exists: Switch to it unless background mode is requested
        (unless background
          (switch-to-buffer buffer))

      ;; Buffer missing: Create it
      (let ((default-directory dir))
        ;; vterm normally switches focus immediately.
        ;; If background is true, we wrap it to restore the window config afterwards.
        (if background
            (save-window-excursion
              (vterm buf-name))
          (vterm buf-name))

        ;; Send command to the newly created buffer
        (with-current-buffer (get-buffer buf-name)
          (vterm-send-string cmd)
          (vterm-send-return))

        (when background
          (message "Started %s in background" buf-name))))))

;;; 1. Start Coop Database
(defun my/dbstart ()
  "Start PostgreSQL server if it is not already running."
  (interactive)
  (let* ((db-dir (expand-file-name "~/Databases/co-op/"))
         (pid-file (concat db-dir "postmaster.pid")))
    (if (not (file-exists-p pid-file))
        (progn
          (message "Starting PostgreSQL...")
          (start-process "pg_ctl" nil "pg_ctl" "start"
                         "-D" db-dir
                         "-l" (concat db-dir "logfile")))
      (message "PostgreSQL is already running."))))

;;; 2. Function to start Elixir Server
(defun my/start-server (&optional background)
  "Starts the Elixir/Phoenix server in the project root."
  (interactive)
  (let ((root (projectile-project-root)))
    (my/run-in-vterm "elixir-server" root "iex -S mix phx.server" background)))

;;; 3. Function to start React Client
(defun my/start-client (&optional background)
  "Starts the React client in the /client subdirectory."
  (interactive)
  (let ((root (projectile-project-root))
        (client-dir "client/"))
    (my/run-in-vterm "react-client"
                     (concat root client-dir)
                     "npm run local"
                     background)))

;;; 4. Function to start full app in background
(defun my/start-full-stack ()
  "Starts both Elixir and React servers in the background without changing window layout."
  (interactive)
  (my/dbstart)
  (my/start-server t)
  (my/start-client t)
  (message "Full stack starting in background..."))




(map! :leader
      (:prefix ("d" . "Dillon")
       :desc "Start Coop Database" "d" #'my/dbstart
       :desc "Start Coop Server" "s" #'my/start-server
       :desc "Start Coop Client"   "c" #'my/start-client
       :desc "Start Fullstack" "f" #'my/start-full-stack))

(defun my/switch-to-elixir-server ()
  "Switch to the Elixir server vterm buffer."
  (interactive)
  (switch-to-buffer "*elixir-server*"))

(defun my/switch-to-react-client ()
  "Switch to the React client server vterm buffer."
  (interactive)
  (switch-to-buffer "*react-client*"))

(map! :leader
      (:prefix ("j" . "jump to")
       :desc "Jmp to React Client" "c" #'my/switch-to-react-client
       :desc "Jump To Elixir Server" "s" #'my/switch-to-elixir-server))



(after! lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("/home/dillon/expert/apps/expert/burrito_out/expert_linux_amd64" "--stdio"))
    :major-modes '(elixir-mode)
    :priority 10
    :server-id 'expert-elixir)))
;;; Org Mode Configuration

;; 1. General Org Settings (Run after 'org' loads)
(after! org
  ;; Enable Org Habit module
  (add-to-list 'org-modules 'org-habit)

  (setq org-read-date-force-compatible-dates nil)
  (setq org-habit-show-all-today t
        org-habit-graph-column 1)

  ;; Capture Templates
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline "~/org/inbox.org" "Inbox")
           "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")

          ("e" "Event" entry
           (file+headline "~/org/calendar.org" "Events")
           "* %^{Event}\n%^{SCHEDULED}T\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:CONTACT: %(org-capture-ref-link \"~/org/contacts.org\")\n:END:\n%?")

          ("d" "Deadline" entry
           (file+headline "~/org/calendar.org" "Deadlines")
           "* TODO %^{Task}\nDEADLINE: %^{Deadline}T\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")

          ("p" "Project" entry
           (file+headline "~/org/projects.org" "Projects")
           "* PROJ %^{Project name}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n** TODO %?")

          ("i" "Idea" entry
           (file+headline "~/org/ideas.org" "Ideas")
           "** IDEA %^{Idea}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?")

          ("b" "Bookmark" entry
           (file+headline "~/org/bookmarks.org" "Inbox")
           "** [[%^{URL}][%^{Title}]]\n:PROPERTIES:\n:CREATED: %U\n:TAGS: %(org-capture-bookmark-tags)\n:END:\n\n"
           :empty-lines 0)

          ("c" "Contact" entry
           (file+headline "~/org/contacts.org" "Inbox")
           "* %^{Name}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:EMAIL: %^{Email}\n:PHONE: %^{Phone}\n:BIRTHDAY: %^{Birthday +1y}u\n:LOCATION: %^{Address}\n:LAST_CONTACTED: %U\n:END:\n\\ *** Communications\n\\ *** Notes\n%?")

          ("n" "Note" entry
           (file+headline "~/org/notes.org" "Inbox")
           "* [%<%Y-%m-%d %a>] %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:CAPTURED: %a\n:END:\n%?"
           :prepend t)))

  ;; Babel Languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((elixir . t) (python . t) (javascript . t) (sql . t)))

  (setq org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window))

;; Auto-tangle configuration
(use-package! org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

;; 2. Agenda Settings (Run after 'org-agenda' loads)
(after! org-agenda
  (setq org-agenda-remove-tags t
        org-agenda-block-separator 32
        org-agenda-include-diary nil)

  ;; Agenda Visual Tweaks Hook
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (visual-line-mode -1)
              (setq truncate-lines 1)))

  ;; Custom Commands (The Dashboard)
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "\n HIGHEST PRIORITY")
                   (org-agenda-prefix-format "   %i %?-2 t%s")))
            (agenda ""
                    ((org-agenda-start-day "+0d")
                     (org-agenda-span 1)
                     (org-agenda-time)
                     (org-agenda-remove-tags t)
                     (org-agenda-todo-keyword-format "")
                     (org-agenda-scheduled-leaders '("" ""))
                     (org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈┈┈ NOW")
                     (org-agenda-overriding-header "\n TODAY'S SCHEDULE")
                     (org-agenda-prefix-format "   %i %?-2 t%s")))
            (tags-todo "-STYLE=\"habit\""
                       ((org-agenda-overriding-header "\n ALL TODO")
                        (org-agenda-sorting-strategy '(priority-down))
                        (org-agenda-remove-tags t)
                        (org-agenda-prefix-format "   %i %?-2 t%s"))))))))

;; 3. Helper functions (These can stay top-level)

(defun org-capture-bookmark-tags ()
  "Get tags from existing bookmarks and prompt for tags with completion."
  (save-window-excursion
    (let ((tags-list '()))
      ;; Collect existing tags
      (with-current-buffer (find-file-noselect "~/org/bookmarks.org")
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^:TAGS:\\s-*\\(.+\\)$" nil t)
            (let ((tag-string (match-string 1)))
              (dolist (tag (split-string tag-string "[,;]" t "[[:space:]]"))
                (push (string-trim tag) tags-list))))))
      ;; Remove duplicates and sort
      (setq tags-list (sort (delete-dups tags-list) 'string<))
      ;; Prompt user with completion
      (let ((selected-tags (completing-read-multiple "Tags (comma-separated): " tags-list)))
        ;; Return as a comma-separated string
        (mapconcat 'identity selected-tags ", ")))))

;; Helper function to select and link a contact
(defun org-capture-ref-link (file)
  "Create a link to a contact in contacts.org"
  (let* ((headlines (org-map-entries
                     (lambda ()
                       (cons (org-get-heading t t t t)
                             (org-id-get-create)))
                     t
                     (list file)))
         (contact (completing-read "Contact: "
                                   (mapcar #'car headlines)))
         (id (cdr (assoc contact headlines))))
    (format "[[id:%s][%s]]" id contact)))

;;; --- Org Roam Configuration ---

(use-package! org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  :config
  (org-roam-db-autosync-mode)

  ;; Create the directory if it doesn't exist
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))

  ;; -- Helper: Force Create Node if Missing --
  (defun my/org-roam-force-create-node (title &optional tag)
    "Creates a new Org-roam node with TITLE immediately and returns its ID.
     If TAG is provided, adds it to #+filetags."
    (let* ((slug (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" title)))
           (filename (expand-file-name (format "%s-%s.org"
                                               (format-time-string "%Y%m%d%H%M%S")
                                               slug)
                                       org-roam-directory))
           (id (org-id-new))
           (tag-str (if tag (format "#+filetags: :%s:\n" tag) "")))
      ;; Create the file
      (with-temp-file filename
        (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n#+title: %s\n%s" id title tag-str)))
      ;; Update DB
      (org-roam-db-update-file filename)
      id))

  ;; -- Helper: Read Multiple Links by Tag --
  (defun my/org-roam-read-nodes-by-tag (tag prompt)
    "Loops, prompting for nodes filtered by TAG. Creates them with TAG if missing.
     Returns a comma-separated string of links."
    (let ((links '())
          (node nil)
          (continue t)
          (filter-fn (lambda (node) (member tag (org-roam-node-tags node)))))
      (while continue
        (setq node (org-roam-node-read nil filter-fn nil nil (concat prompt " (RET to finish): ")))
        (let ((title (org-roam-node-title node)))
          (if (or (not title) (string-empty-p title))
              (setq continue nil)
            (let ((id (org-roam-node-id node)))
              (unless id (setq id (my/org-roam-force-create-node title tag)))
              (push (format "[[id:%s][%s]]" id title) links)))))
      (mapconcat 'identity (nreverse links) ", ")))

  ;; -- Helper: Read Orientation --
  (defun my/org-roam-read-orientation ()
    "Prompts for one or more orientations and returns them as a formatted tag string."
    (let ((choices (completing-read-multiple "Orientation (comma-separated): "
                                             '("Top" "Bottom" "Superior" "Inferior"))))
      (setq choices (delete "" choices))
      (if choices
          (concat ":" (mapconcat #'downcase choices ":"))
        "")))

  ;; -- Capture Templates --
  (setq org-roam-capture-templates
        `(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)

          ("p" "Position" plain
           ,(concat
             "*Description:* %^{Description}\n"
             "*Videos:*\n- \n"
             "*Notes:*\n%?")
           :target (file+head "positions/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :position:\n")
           :unnarrowed t)

          ("g" "Grip" plain
           ,(concat
             "*Description:* %^{Description}\n"
             "*Videos:*\n- \n"
             "*Notes:*\n%?")
           :target (file+head "grips/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :grip:\n")
           :unnarrowed t)

          ("t" "Technique" plain
           ,(concat
             "*Positions:* %(my/org-roam-read-nodes-by-tag \"position\" \"Link Position\")\n"
             "*Grips:* %(my/org-roam-read-nodes-by-tag \"grip\" \"Link Grip\")\n"
             "*Description:* %^{Description}\n"
             "*Teaching Points:*\n- \n"
             "*Videos:*\n- \n"
             "*Notes:*\n%?\n"
             "*Mastery:* %^{Mastery (1-10)|1|2|3|4|5|6|7|8|9|10}\n"
             "*Related Techniques:* \n")
           :target (file+head "techniques/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :technique%(my/org-roam-read-orientation):\n")
           :unnarrowed t)

          ("c" "Concept" plain
           ,(concat
             "*Description:* %^{Description}\n"
             "*Videos:*\n- \n"
             "*Connections:*\n%?")
           :target (file+head "concepts/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :concept:\n")
           :unnarrowed t))))
