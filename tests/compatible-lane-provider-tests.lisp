(in-package #:autolith)

;;;; -- Compatible Lane Provider Tests --

;;; The lanes registered as built-ins in PROVIDER/BUILTINS.lisp share one wire
;;; and differ only in endpoint, credential source, headers, and model roster,
;;; so one table describes them and every check runs over every row.

(-> compatible-lane-provider-test--rows () list)
(defun compatible-lane-provider-test--rows ()
  "Return one (NAME DISPLAY-NAME FAMILY KEY-VARIABLE CREATE-FUNCTION) row per lane."
  (list (list "xiaomi" "Xiaomi MiMo" ':xiaomi
              *xiaomi-environment-variable* #'xiaomi-credential-manager-create)
        (list "cline" "Cline" ':cline
              *cline-environment-variable* #'cline-credential-manager-create)
        (list "ollama" "Ollama" ':ollama
              *ollama-environment-variable* #'ollama-credential-manager-create)
        (list "google-vertex" "Google Vertex AI" ':google-vertex
              *google-vertex-environment-variable*
              #'google-vertex-credential-manager-create)))

(-> compatible-lane-provider-test--configure (configuration) configuration)
(defun compatible-lane-provider-test--configure (configuration)
  "Return CONFIGURATION prepared for a lane provider."
  (setf (configuration-validation-deferred-p configuration) t
        (config :model configuration) *default-model*
        (config :reasoning-effort configuration) "medium")
  configuration)

(-> compatible-lane-provider-test--provider (string configuration) (option model-provider))
(defun compatible-lane-provider-test--provider (name configuration)
  "Return the provider NAME's registration builds for CONFIGURATION."
  (let ((registration (provider-registration-find name)))
    (and registration
         (funcall (provider-registration-factory registration)
                  configuration
                  :reasoning-summaries-p nil))))

(-> compatible-lane-provider-test--registration () null)
(defun compatible-lane-provider-test--registration ()
  "Test that every lane is registered as a built-in Chat Completions provider."
  (dolist (row (compatible-lane-provider-test--rows))
    (destructuring-bind (name display-name family key-variable create-function) row
      (declare (ignore display-name key-variable create-function))
      (let ((registration (provider-registration-find name)))
        (test-assert (not (null registration))
                     (format nil "~A is registered" name))
        (when registration
          (test-assert (eq (provider-registration-protocol registration)
                           ':chat-completions)
                       (format nil "~A speaks the Chat Completions wire" name))
          (test-assert (eq (provider-registration-family registration) family)
                       (format nil "~A keeps its own conversation family" name))
          (test-assert (eq (provider-registration-source registration) ':builtin)
                       (format nil "~A is a built-in provider" name))
          (test-assert (non-empty-string-p
                        (provider-registration-description registration))
                       (format nil "~A describes itself" name))
          (test-assert (non-empty-string-p
                        (provider-registration-endpoint registration))
                       (format nil "~A declares an endpoint" name))
          (test-assert (or (provider-registration-declared-models registration)
                           (provider-registration-model-discovery registration))
                       (format nil "~A declares models or model discovery" name))))))
  nil)

(-> compatible-lane-provider-test--credentials () null)
(defun compatible-lane-provider-test--credentials ()
  "Test each lane's environment bootstrap and private-store fallback."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (compatible-lane-provider-test--configure configuration)
    (dolist (row (compatible-lane-provider-test--rows))
      (destructuring-bind (name display-name family key-variable create-function) row
        (declare (ignore family))
        (let ((environment-key (format nil "~A-environment-key" name))
              (private-key (format nil "~A-private-key" name)))
          (with-test-environment ((key-variable environment-key))
            (let ((manager (funcall create-function configuration)))
              (test-assert (string= (credential-manager-provider-label manager)
                                    display-name)
                           (format nil "~A names itself ~A" name display-name))
              (api-key-credential-manager-save-key manager private-key))
            (test-assert
             (string= (oauth-credentials-access-token
                       (credential-manager-load
                        (funcall create-function configuration)))
                      environment-key)
             (format nil "~A environment credentials take precedence" name)))
          (with-test-environment ((key-variable nil))
            (test-assert
             (string= (oauth-credentials-access-token
                       (credential-manager-load
                        (funcall create-function configuration)))
                      private-key)
             (format nil "~A uses its private key when the environment is absent"
                     name)))))))
  nil)

(-> compatible-lane-provider-test--providers () null)
(defun compatible-lane-provider-test--providers ()
  "Test the provider every lane builds."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (compatible-lane-provider-test--configure configuration)
    (dolist (row (compatible-lane-provider-test--rows))
      (destructuring-bind (name display-name family key-variable create-function) row
        (declare (ignore key-variable create-function))
        (let ((provider (compatible-lane-provider-test--provider name configuration)))
          (test-assert (typep provider 'openai-compatible-provider)
                       (format nil "~A builds an OpenAI-compatible provider" name))
          (when provider
            (test-assert (eq (provider-family provider) family)
                         (format nil "~A keeps its provider family" name))
            (test-assert (string= (provider-account-label provider) display-name)
                         (format nil "~A reports its user-visible name" name))
            (test-assert (typep (provider-credential-manager provider)
                                'static-api-key-credential-manager)
                         (format nil "~A boots its key from the environment" name)))))))
  nil)

(-> compatible-lane-provider-test--cline-client-identity () null)
(defun compatible-lane-provider-test--cline-client-identity ()
  "Test the Cline client identity every Cline request carries."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (compatible-lane-provider-test--configure configuration)
    (let* ((platform-name (cline-client-platform-name))
           (provider (compatible-lane-provider-test--provider "cline"
                                                             configuration))
           (headers (openai-compatible-provider-headers provider))
           (value (lambda (name)
                    (rest (assoc name headers :test #'string=)))))
      (test-assert (member platform-name '("win32" "darwin" "linux")
                           :test #'string=)
                   "Cline reports a host platform name")
      (test-assert (string= (funcall value "X-CLIENT-TYPE") "cline-sdk")
                   "Cline identifies its client type")
      (test-assert (string= (funcall value "X-CLIENT-VERSION")
                            *cline-client-version*)
                   "Cline identifies its client version")
      (test-assert (string= (funcall value "User-Agent")
                            (format nil "Cline/~A" *cline-client-version*))
                   "Cline identifies its client user agent")
      (test-assert (string= (funcall value "X-PLATFORM") platform-name)
                   "Cline reports this host's platform name")
      (test-assert (string= (funcall value "X-PLATFORM-VERSION")
                            *cline-client-platform-version*)
                   "Cline identifies its client platform version")
      (test-assert (string= (funcall value "X-TITLE") "Cline")
                   "Cline identifies its client title")
      (test-assert (string= (funcall value "HTTP-Referer") "https://cline.bot")
                   "Cline identifies its client origin")
      (test-assert (string= (funcall value "X-IS-MULTIROOT") "false")
                   "Cline reports a single-root client")))
  nil)

(-> compatible-lane-provider-test--ollama-discovery () null)
(defun compatible-lane-provider-test--ollama-discovery ()
  "Test Ollama's model discovery endpoint and its override."
  (let ((registration (provider-registration-find "ollama")))
    (with-test-environment (("AUTOLITH_OLLAMA_MODELS_ENDPOINT" nil))
      (test-assert (string= (ollama-models-endpoint) *ollama-models-endpoint*)
                   "Ollama discovers models at its own endpoint")
      (test-assert (string= (provider-registration-model-discovery-endpoint
                             registration)
                            *ollama-models-endpoint*)
                   "Ollama's registration names the same discovery endpoint")
      (test-assert (not (null (provider-registration-model-discovery
                               registration)))
                   "Ollama declares model discovery"))
    (with-test-environment
        (("AUTOLITH_OLLAMA_MODELS_ENDPOINT" "https://ollama.invalid/v1/models"))
      (test-assert (string= (ollama-models-endpoint)
                            "https://ollama.invalid/v1/models")
                   "Ollama discovery honors its endpoint override")))
  nil)

(-> compatible-lane-provider-test--vertex-endpoint () null)
(defun compatible-lane-provider-test--vertex-endpoint ()
  "Test the Vertex AI API-key route, its override, and its independence."
  (with-test-environment
      (("AUTOLITH_GOOGLE_VERTEX_PROVIDER_ENDPOINT" nil)
       ("GOOGLE_CLOUD_PROJECT" "lane-project")
       ("GOOGLE_VERTEX_LOCATION" "europe-west1"))
    (test-assert
     (string= (google-vertex-chat-completions-endpoint)
              *google-vertex-chat-completions-endpoint*)
     "Vertex AI keeps the project-less API-key route")
    (test-assert
     (string= (provider-registration-endpoint
               (provider-registration-find "google-vertex"))
              *google-vertex-chat-completions-endpoint*)
     "Vertex AI registers the API-key route"))
  (with-test-environment
      (("AUTOLITH_GOOGLE_VERTEX_PROVIDER_ENDPOINT"
        "https://vertex.invalid/v1/endpoints/openapi/chat/completions"))
    (test-assert
     (string= (google-vertex-chat-completions-endpoint)
              "https://vertex.invalid/v1/endpoints/openapi/chat/completions")
     "Vertex AI honors its endpoint override"))
  nil)

(-> compatible-lane-provider-test--credential-headers () null)
(defun compatible-lane-provider-test--credential-headers ()
  "Test the header and scheme each lane carries its key in."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (compatible-lane-provider-test--configure configuration)
    (dolist (row (compatible-lane-provider-test--rows))
      (destructuring-bind (name display-name family key-variable create-function) row
        (declare (ignore display-name family))
        (let ((header (if (string= name "google-vertex")
                          "x-goog-api-key"
                          "Authorization"))
              (scheme (if (string= name "google-vertex") "" "Bearer")))
          (with-test-environment ((key-variable "lane-key"))
            (let* ((provider (compatible-lane-provider-test--provider
                              name configuration))
                   (credentials (credential-manager-load
                                 (funcall create-function configuration)))
                   (conversation
                     (conversation-create
                      configuration
                      :identifier (format nil "compatible-lane-headers-~A" name)))
                   (headers (openai-compatible--request-headers
                             provider credentials conversation))
                   (sent (rest (assoc header headers :test #'string=))))
              (test-assert
               (string= (openai-compatible-provider-credential-header provider)
                        header)
               (format nil "~A sends its key in ~A" name header))
              (test-assert
               (string= (openai-compatible-provider-credential-scheme provider)
                        scheme)
               (format nil "~A prefixes its key with ~S" name scheme))
              (test-assert
               (string= sent (if (plusp (length scheme))
                                 (format nil "~A ~A" scheme "lane-key")
                                 "lane-key"))
               (format nil "~A sends its key under ~A" name header))
              (when (string= name "google-vertex")
                (test-assert
                 (null (assoc "Authorization" headers :test #'string=))
                 "Vertex AI sends no bearer header"))))))))
  nil)

(-> compatible-lane-provider-test--effort-omission () null)
(defun compatible-lane-provider-test--effort-omission ()
  "Test that a lane without a reasoning parameter sends no effort field."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (compatible-lane-provider-test--configure configuration)
    (let* ((provider (compatible-lane-provider-test--provider "xiaomi"
                                                             configuration))
           (conversation
             (conversation-create configuration :identifier "xiaomi-request")))
      (conversation-append-user-message conversation "Hello Xiaomi.")
      (multiple-value-bind (request delivery)
          (provider-request-object provider conversation (json-array)
                                   :goal-context nil
                                   :compaction-p nil)
        (declare (ignore delivery))
        (test-assert (null (json-get request "reasoning_effort"))
                     "Xiaomi requests omit the reasoning effort field")
        (test-assert (null (json-get request "thinking"))
                     "Xiaomi requests omit the thinking field"))))
  nil)

(-> compatible-lane-provider-test--model-routing () null)
(defun compatible-lane-provider-test--model-routing ()
  "Test that every declared lane model routes back to its own lane."
  (dolist (row (compatible-lane-provider-test--rows))
    (destructuring-bind (name display-name family key-variable create-function) row
      (declare (ignore display-name family key-variable create-function))
      (let* ((registration (provider-registration-find name))
             (models (provider-registration-declared-models registration)))
        (test-assert (or models
                         (provider-registration-model-discovery registration))
                     (format nil "~A declares models or model discovery" name))
        (dolist (model models)
          (let* ((identifier (provider-model-name model))
                 (owner (provider-registration-for-model identifier)))
            (test-assert (and owner
                              (string= (provider-registration-name owner) name))
                         (format nil "~A serves its own model ~A" name
                                 identifier))))
        (when (string= name "google-vertex")
          (test-assert
           (every (lambda (model)
                    (uiop:string-prefix-p *google-vertex-model-prefix*
                                          (provider-model-name model)))
                  models)
           "Vertex AI models live in their own namespace")))))
  nil)

(-> compatible-lane-provider-test--vertex-namespace () null)
(defun compatible-lane-provider-test--vertex-namespace ()
  "Test the Vertex AI model namespace and its wire form."
  (test-assert (string= (google-vertex-wire-model-name
                         "google-vertex/gemini-2.5-pro")
                        "gemini-2.5-pro")
               "Vertex AI removes its model namespace for the wire")
  (test-assert (handler-case
                   (progn (google-vertex-wire-model-name "gemini-2.5-pro") nil)
                 (configuration-error () t))
               "Vertex AI refuses a model outside its namespace")
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (compatible-lane-provider-test--configure configuration)
    (setf (config :model configuration) "google-vertex/gemini-2.5-flash")
    (let* ((provider (compatible-lane-provider-test--provider "google-vertex"
                                                             configuration))
           (conversation
             (conversation-create configuration :identifier "vertex-request")))
      (conversation-append-user-message conversation "Hello Vertex.")
      (multiple-value-bind (request delivery)
          (provider-request-object provider conversation (json-array)
                                   :goal-context nil
                                   :compaction-p nil)
        (declare (ignore delivery))
        (test-assert (string= (json-get request "model") "gemini-2.5-flash")
                     "Vertex AI sends the model without its namespace"))))
  nil)

(-> test-compatible-lane-providers () null)
(defun test-compatible-lane-providers ()
  "Run the offline compatible lane provider tests."
  (let ((registry-snapshot (provider--registry-snapshot)))
    (unwind-protect
         (progn
           (compatible-lane-provider-test--registration)
           (compatible-lane-provider-test--credentials)
           (compatible-lane-provider-test--providers)
           (compatible-lane-provider-test--model-routing)
           (compatible-lane-provider-test--cline-client-identity)
           (compatible-lane-provider-test--ollama-discovery)
           (compatible-lane-provider-test--vertex-endpoint)
           (compatible-lane-provider-test--credential-headers)
           (compatible-lane-provider-test--vertex-namespace)
           (compatible-lane-provider-test--effort-omission))
      (provider--registry-restore registry-snapshot)))
  nil)
