;;;; SPDX-FileCopyrightText: Pedro Delfino
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt-llm-cockpit/agent
  (:use :cl)
  (:export
   ;; JSON
   #:json-error
   #:parse-json
   #:write-json
   #:to-json
   #:jobject
   #:jarray
   #:jget
   #:jtrue-p
   ;; Entries: what a transcript is made of
   #:entry
   #:entry-id
   #:entry-parent
   #:user-entry
   #:assistant-entry
   #:thinking-entry
   #:tool-entry
   #:notice-entry
   #:entry-text
   #:entry-final-p
   #:tool-use-id
   #:tool-name
   #:tool-input
   #:tool-status
   #:tool-result
   #:tool-error-p
   #:notice-kind
   ;; Permission requests
   #:permission-request
   #:request-id
   #:request-tool-name
   #:request-input
   #:request-description
   #:request-suggestions
   #:request-blocked-path
   #:request-tool-use-id
   #:request-state
   ;; Sessions
   #:session
   #:session-program
   #:session-working-directory
   #:session-model
   #:session-permission-mode
   #:session-resume
   #:session-setting-sources
   #:session-extra-arguments
   #:session-state
   #:session-id
   #:session-info
   #:session-entries
   #:session-pending-permissions
   #:session-total-cost
   #:session-turns
   #:session-context-tokens
   #:session-context-window
   #:session-problems
   #:session-command
   #:find-program
   #:add-listener
   #:remove-listener
   #:start-session
   #:stop-session
   #:send-prompt
   #:interrupt-turn
   #:respond-permission
   #:session-alive-p
   #:session-busy-p
   #:wait-until
   #:tool-summary)
  (:documentation "Drive the Claude Code command line program from Lisp.

The program is started headless and spoken to over its stream-json protocol:
one JSON object per line on its standard input and output. This package
turns that stream into a `session' holding a list of `entry' objects, and
sends prompts, permission answers and interrupts back.

It depends on nothing but ASDF's UIOP and SBCL's threads. It knows nothing
about Nyxt, so it is tested without a browser."))
