(in-package #:autolith)

;;;; -- Built-in Provider Registrations --

(-> provider--codex-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--codex-registration-factory (configuration &key reasoning-summaries-p)
  "Create the built-in ChatGPT provider from registry metadata."
  (provider-family-create ':codex
                           configuration
                           :reasoning-summaries-p reasoning-summaries-p))

(-> provider--grok-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--grok-registration-factory (configuration &key reasoning-summaries-p)
  "Create the built-in Grok provider from registry metadata."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':grok configuration))

(-> provider--gemini-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--gemini-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in Gemini Code Assist subscription provider."
  (declare (ignore reasoning-summaries-p))
  (gemini-code-assist-provider-create configuration))

(-> provider--gemini-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--gemini-registration-authenticator
    (provider &key stream open-browser-p)
  "Run installed-application OAuth for the built-in Gemini provider."
  (gemini-oauth-login (provider-credential-manager provider)
                      :stream (or stream *standard-output*)
                      :open-browser-p open-browser-p)
  "Gemini authentication was saved by Autolith.")

(-> provider--nous-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--nous-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in Nous Research provider from registry metadata."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':nous configuration))

(-> provider--nous-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--nous-registration-authenticator
    (provider &key stream open-browser-p)
  "Run browser device login for the built-in Nous Research provider."
  (nous-provider-authenticate provider
                              :stream stream
                              :open-browser-p open-browser-p))

(-> provider--fireworks-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--fireworks-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in Fireworks provider from registry metadata."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':fireworks configuration))

(-> provider--opencode-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--opencode-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in OpenCode provider from registry metadata."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':opencode configuration))

(-> provider--opencode-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--opencode-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in OpenCode provider's API key."
  (declare (ignore open-browser-p))
  (opencode-api-key-login (provider-credential-manager provider)
                           :stream (or stream *standard-output*)))

(-> provider--openrouter-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--openrouter-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in OpenRouter provider from registry metadata."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':openrouter configuration))

(-> provider--openrouter-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--openrouter-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in OpenRouter provider's API key."
  (declare (ignore open-browser-p))
  (openrouter-api-key-login (provider-credential-manager provider)
                            :stream (or stream *standard-output*)))

(-> provider--mistral-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--mistral-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in Mistral provider from registry metadata."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':mistral configuration))

(-> provider--mistral-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--mistral-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in Mistral provider's API key."
  (declare (ignore open-browser-p))
  (mistral-api-key-login (provider-credential-manager provider)
                         :stream (or stream *standard-output*)))

(-> provider--fireworks-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--fireworks-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in Fireworks provider's API key."
  (declare (ignore open-browser-p))
  (fireworks-api-key-login (provider-credential-manager provider)
                           :stream (or stream *standard-output*)))

(-> provider--anthropic-registration-factory
    (configuration &key (:reasoning-summaries-p boolean))
    model-provider)
(defun provider--anthropic-registration-factory
    (configuration &key reasoning-summaries-p)
  "Create the built-in Anthropic provider from registry metadata."
  (declare (ignore reasoning-summaries-p))
  (provider-family-create ':anthropic configuration))

(-> provider--anthropic-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--anthropic-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in Anthropic provider's API key."
  (declare (ignore open-browser-p))
  (anthropic-api-key-login (provider-credential-manager provider)
                           :stream (or stream *standard-output*)))


;; ChatGPT subscription metadata: https://github.com/openai/codex,
;; codex-rs/models-manager/models.json at 444da310e108da16aaeb18fd790b0ac464f08aca.
(register-provider
 "chatgpt"
 :description "ChatGPT Codex subscription"
 :family ':codex
 :protocol ':responses
 :models '((:name "gpt-6.1-sol"
            :context-window 272000
            :reasoning-efforts ("low" "medium" "high" "xhigh" "max" "ultra"))
           (:name "gpt-6-astra"
            :context-window 272000
            :reasoning-efforts ("low" "medium" "high" "xhigh" "max" "ultra"))
           (:name "gpt-6-sol"
            :context-window 272000
            :reasoning-efforts ("low" "medium" "high" "xhigh" "max" "ultra"))
           (:name "gpt-6-luna"
            :context-window 272000
            :reasoning-efforts ("low" "medium" "high" "xhigh" "max"))
           "gpt-5.6-sol"
           "gpt-5.6-luna"
           "gpt-5.6-terra")
 :factory #'provider--codex-registration-factory
 :source ':builtin)

(register-provider
 "gemini"
 :description "Gemini Code Assist subscription"
 :family ':gemini-code-assist
 :protocol ':gemini-code-assist
 :models *gemini-code-assist-models*
 :factory #'provider--gemini-registration-factory
 :authenticator #'provider--gemini-registration-authenticator
 :endpoint *gemini-code-assist-endpoint*
 :source ':builtin)

(register-provider
 "grok"
 :description "Grok subscription"
 :family ':grok
 :protocol ':responses
 :models '((:name "grok-4.7" :context-window 500000
            :reasoning-efforts ("low" "medium" "high" "xhigh"))
           (:name "grok-4.6" :context-window 500000)
           (:name "grok-4.5" :context-window 500000))
 :factory #'provider--grok-registration-factory
 :source ':builtin)

(register-provider
 "nous"
 :description "Nous Research subscription"
 :family ':nous
 :protocol ':chat-completions+messages
 :endpoint (nous-chat-completions-endpoint)
 :factory #'provider--nous-registration-factory
 :authenticator #'provider--nous-registration-authenticator
 :model-discovery #'nous--fetch-models
 :model-discovery-endpoint (nous-models-endpoint)
 :model-discovery-endpoint-resolver #'nous-models-endpoint
 :source ':builtin)

(register-provider
 "fireworks"
 :description "Fireworks AI"
 :family ':fireworks
 :protocol ':responses
 :models '((:name "accounts/fireworks/models/kimi-k3"
            :context-window 1048576)
           ;; Fireworks advertises a 262k token context window.
           (:name "accounts/fireworks/models/qwen3p7-plus"
            :context-window 262144
            :reasoning-efforts ("none")))
 :factory #'provider--fireworks-registration-factory
 :authenticator #'provider--fireworks-registration-authenticator
 :endpoint *fireworks-responses-endpoint*
 :source ':builtin)

(register-provider
 "anthropic"
 :description "Anthropic API (pay-per-token)"
 :family ':anthropic
 :protocol ':messages
 :models '((:name "claude-opus-5" :context-window 200000
            :reasoning-efforts ("low" "medium" "high"))
           (:name "claude-sonnet-5" :context-window 200000
            :reasoning-efforts ("low" "medium" "high"))
           (:name "claude-haiku-4-5-20251001" :context-window 200000
            :reasoning-efforts ("low" "medium" "high")))
 :factory #'provider--anthropic-registration-factory
 :authenticator #'provider--anthropic-registration-authenticator
 :endpoint *anthropic-messages-endpoint*
 :source ':builtin)

(register-provider
 "opencode"
 :description "OpenCode (zen/go)"
 :family ':opencode
 :protocol ':chat-completions
 :endpoint *opencode-chat-completions-endpoint*
 :factory #'provider--opencode-registration-factory
 :authenticator #'provider--opencode-registration-authenticator
 :model-discovery #'opencode--fetch-models
 :model-discovery-endpoint *opencode-models-endpoint*
 :model-discovery-endpoint-resolver #'opencode-models-endpoint
 :source ':builtin)

(register-provider
 "openrouter"
 :description "OpenRouter (pay-per-token)"
 :family ':openrouter
 :protocol ':chat-completions
 :endpoint *openrouter-chat-completions-endpoint*
 :factory #'provider--openrouter-registration-factory
 :authenticator #'provider--openrouter-registration-authenticator
 :model-discovery #'openrouter--fetch-models
 :model-discovery-endpoint *openrouter-models-endpoint*
 :model-discovery-endpoint-resolver #'openrouter-models-endpoint
 :source ':builtin)

(register-provider
 "mistral"
 :description "Mistral AI"
 :family ':mistral
 :protocol ':chat-completions
 :endpoint *mistral-chat-completions-endpoint*
 :factory #'provider--mistral-registration-factory
 :authenticator #'provider--mistral-registration-authenticator
 :model-discovery #'mistral--fetch-models
 :model-discovery-endpoint *mistral-models-endpoint*
 :model-discovery-endpoint-resolver #'mistral-models-endpoint
 :source ':builtin)


;;;; -- Registered Chat Completions Lanes --
;;; Every lane below speaks the OpenAI Chat Completions wire at a bearer key,
;;; so each is a registration rather than a client implementation. The
;;; credential manager comes from the lane's definition beside the other
;;; static API-key providers, which is what lets the vendor's own environment
;;; variable bootstrap the key.

(-> provider--xiaomi-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--xiaomi-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in Xiaomi MiMo provider's API key."
  (declare (ignore open-browser-p))
  (xiaomi-api-key-login (provider-credential-manager provider)
                        :stream (or stream *standard-output*)))

(-> provider--cline-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--cline-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in Cline provider's API key."
  (declare (ignore open-browser-p))
  (cline-api-key-login (provider-credential-manager provider)
                       :stream (or stream *standard-output*)))

(-> provider--ollama-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--ollama-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for, validate, and save the built-in Ollama provider's API key."
  (declare (ignore open-browser-p))
  (ollama-api-key-login (provider-credential-manager provider)
                        :stream (or stream *standard-output*)))

(-> provider--google-vertex-registration-authenticator
    (model-provider &key (:stream stream) (:open-browser-p boolean))
    string)
(defun provider--google-vertex-registration-authenticator
    (provider &key stream open-browser-p)
  "Prompt for and save the built-in Google Vertex AI provider's API key."
  (declare (ignore open-browser-p))
  (google-vertex-api-key-login (provider-credential-manager provider)
                               :stream (or stream *standard-output*)))

;;; Xiaomi's models take a thinking object rather than a reasoning effort, so
;;; this lane declares no reasoning parameter and sends none. A Token Plan key
;;; is served by that plan's regional clusters instead of the shared base; a
;;; deployment using one names the cluster through
;;; AUTOLITH_XIAOMI_PROVIDER_ENDPOINT.

(register-openai-compatible-provider
 "xiaomi"
 :description "Xiaomi MiMo"
 :authenticator #'provider--xiaomi-registration-authenticator
 :display-name "Xiaomi MiMo"
 :endpoint *xiaomi-chat-completions-endpoint*
 :credential-manager #'xiaomi-credential-manager-create
 :models '((:name "mimo-v2.6-pro" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "mimo-v2.6-pro-ultraspeed" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "mimo-v2.6-flash" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "mimo-v2.5-pro" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "mimo-v2.5-pro-ultraspeed" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "mimo-v2.5" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "mimo-v2-pro" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "mimo-v2-omni" :context-window 262144
            :reasoning-efforts ("low" "medium" "high"))
           (:name "mimo-v2-flash" :context-window 262144
            :reasoning-efforts ("low" "medium" "high")))
 :source ':builtin)

;;; Cline's roster is named as its gateway expects it on the wire: a
;;; subscription model carries its plan prefix and a free model its own.
(register-openai-compatible-provider
 "cline"
 :description "Cline"
 :authenticator #'provider--cline-registration-authenticator
 :display-name "Cline"
 :endpoint *cline-chat-completions-endpoint*
 :headers (cline-client-headers)
 :credential-manager #'cline-credential-manager-create
 :models '((:name "cline-pass/deepseek-v4-pro" :context-window 1048576
            :reasoning-efforts ("high" "xhigh"))
           (:name "cline-pass/deepseek-v4.1-flash" :context-window 1000000
            :reasoning-efforts ("low" "medium" "high" "xhigh"))
           (:name "cline-pass/glm-5.3" :context-window 1048576
            :reasoning-efforts ("low" "high" "max"))
           (:name "cline-pass/glm-5.3-flash" :context-window 1000000
            :reasoning-efforts ("low" "high" "max"))
           (:name "cline-pass/kimi-k3" :context-window 1048576
            :reasoning-efforts ("low" "high" "max"))
           (:name "cline-pass/mimo-v2.5" :context-window 1050000)
           (:name "cline-pass/mimo-v2.5-pro" :context-window 1050000)
           (:name "cline-pass/mimo-v2.6-flash" :context-window 1048576)
           (:name "cline-pass/mimo-v2.6-pro" :context-window 1048576)
           (:name "cline-pass/minimax-m3" :context-window 1048576)
           (:name "cline-pass/muse-spark-1.3-contributor" :context-window 1048576)
           (:name "cline-pass/qwen3.7-max" :context-window 1000000
            :reasoning-efforts ("high" "xhigh"))
           (:name "cline-pass/qwen3.7-plus" :context-window 1000000
            :reasoning-efforts ("low" "medium" "high" "xhigh" "max"))
           (:name "cline-pass/qwen3.8-max" :context-window 1000000
            :reasoning-efforts ("low" "medium" "high" "xhigh"))
           (:name "cline-free/mimo-v2.6-flash" :context-window 1048576)
           (:name "cline-free/muse-spark-1.3-contributor" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high" "xhigh"))
           (:name "cline-free/solar-mini4" :context-window 524288))
 :source ':builtin)

;;; Ollama answers the OpenAI-compatible wire under /v1 on one host, hosted or
;;; self-hosted, and lists the models it serves there. Autolith always sends a
;;; credential, so a self-hosted server that asks for none takes any
;;; non-empty key value.
(register-openai-compatible-provider
 "ollama"
 :description "Ollama (cloud or self-hosted)"
 :authenticator #'provider--ollama-registration-authenticator
 :display-name "Ollama"
 :endpoint *ollama-chat-completions-endpoint*
 :models-endpoint (ollama-models-endpoint)
 :credential-manager #'ollama-credential-manager-create
 :source ':builtin)

;;; Vertex AI takes an API key raw in x-goog-api-key on the project-less
;;; partner route of its OpenAI-compatible wire, and
;;; AUTOLITH_GOOGLE_VERTEX_PROVIDER_ENDPOINT names a regional host or a
;;; gateway instead.
(register-openai-compatible-provider
 "google-vertex"
 :description "Google Vertex AI"
 :authenticator #'provider--google-vertex-registration-authenticator
 :display-name "Google Vertex AI"
 :endpoint (google-vertex-chat-completions-endpoint)
 :credential-manager #'google-vertex-credential-manager-create
 :wire-model-name #'google-vertex-wire-model-name
 :credential-header *google-vertex-credential-header*
 :credential-scheme *google-vertex-credential-scheme*
 :models '((:name "google-vertex/gemini-3.8-flash" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "google-vertex/gemini-3.7-flash" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "google-vertex/gemini-3.6-flash" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "google-vertex/gemini-3.5-flash" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "google-vertex/gemini-3.5-flash-lite" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "google-vertex/gemini-3.1-pro-preview" :context-window 1048576
            :reasoning-efforts ("low" "high"))
           (:name "google-vertex/gemini-3.1-pro-preview-customtools" :context-window 1048576
            :reasoning-efforts ("low" "high"))
           (:name "google-vertex/gemini-3.1-flash-lite" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "google-vertex/gemini-3-flash-preview" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "google-vertex/gemini-2.5-pro" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "google-vertex/gemini-2.5-flash" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "google-vertex/gemini-2.5-flash-lite" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "google-vertex/gemini-flash-latest" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high"))
           (:name "google-vertex/gemini-flash-lite-latest" :context-window 1048576
            :reasoning-efforts ("low" "medium" "high")))
 :source ':builtin)
