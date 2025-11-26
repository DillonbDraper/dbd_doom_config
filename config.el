;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")
(setq user-full-name "Dillon Draper"
      user-mail-address "dillonbdraper@gmail.com")

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
(setq doom-theme 'modus-operandi-tinted)

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

(set-face-foreground 'line-number "F0F0F0")
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
                                        ; See the Configuration section below
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "gemini-2.5-pro"))

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
          gptel-model "gemini-pro-latest")))

(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(".*\\.heex$" . "elixir")))

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

;;; 1. Function to start Elixir Server
(defun my/start-server (&optional background)
  "Starts the Elixir/Phoenix server in the project root."
  (interactive)
  (let ((root (projectile-project-root)))
    (my/run-in-vterm "elixir-server" root "iex -S mix phx.server" background)))

;;; 2. Function to start React Client
(defun my/start-client (&optional background)
  "Starts the React client in the /client subdirectory."
  (interactive)
  (let ((root (projectile-project-root))
        (client-dir "client/"))
    (my/run-in-vterm "react-client"
                     (concat root client-dir)
                     "npm run local"
                     background)))

;;; 3. Function to start BOTH in background
(defun my/start-full-stack ()
  "Starts both Elixir and React servers in the background without changing window layout."
  (interactive)
  (my/start-server t)
  (my/start-client t)
  (message "Full stack starting in background..."))




(map! :leader
      (:prefix ("d" . "Dillon")
       :desc "Start Coop Server" "s" #'my/start-server
       :desc "Start Coop Client"   "c" #'my/start-client
       :desc "Start Fullstack" "b" #'my/start-full-stack))


(after! lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("/home/dillon/expert/apps/expert/burrito_out/expert_linux_amd64" "--stdio"))
    :major-modes '(elixir-mode)
    :priority 10
    :server-id 'expert-elixir)))
