(in-package #:autolith)

;;;; -- Google Vertex AI Provider --

;;; Vertex AI serves models that another Google lane serves under the same
;;; names, so this lane keeps a model namespace of its own and removes it at
;;; the wire boundary, exactly as the Copilot lane namespaces its models.

(defparameter *google-vertex-model-prefix* "google-vertex/"
  "The model namespace prefix every Vertex AI model identifier carries.")

(defparameter *google-vertex-credential-header* "x-goog-api-key"
  "The Vertex AI header carrying an API key.")

(defparameter *google-vertex-credential-scheme* ""
  "The scheme a Vertex AI API key carries, which is none.")

(-> google-vertex-wire-model-name (string) string)
(defun google-vertex-wire-model-name (model)
  "Return the Vertex AI model MODEL names on the wire."
  (unless (uiop:string-prefix-p *google-vertex-model-prefix* model)
    (error 'configuration-error
           :message
           (format nil "A Vertex AI request requires a ~A model."
                   *google-vertex-model-prefix*)))
  (subseq model (length *google-vertex-model-prefix*)))
