;;;; SPDX-FileCopyrightText: Pedro Delfino
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Three layers, each its own system, so each is tested at its own cost:
;;;;
;;;;   nyxt-llm-cockpit/agent   the Claude Code program as a session    plain SBCL
;;;;   nyxt-llm-cockpit/view    a session as HTML and one status line   plain SBCL
;;;;   nyxt-llm-cockpit         the mode, the page and the commands     needs Nyxt

(defsystem "nyxt-llm-cockpit"
  :version "0.1.0"
  :author "Pedro Delfino"
  :license "BSD-3-Clause"
  :description "A cockpit for Claude Code inside the Nyxt browser."
  :depends-on ("nyxt" "nyxt-llm-cockpit/agent" "nyxt-llm-cockpit/view")
  :pathname "source/"
  :components ((:file "cockpit"))
  :in-order-to ((test-op (test-op "nyxt-llm-cockpit/tests"))))

(defsystem "nyxt-llm-cockpit/agent"
  :version "0.1.0"
  :author "Pedro Delfino"
  :license "BSD-3-Clause"
  :description "Drive the Claude Code program over its stream-json protocol.
No Nyxt and no libraries: ASDF's UIOP and SBCL's threads."
  :pathname "source/agent/"
  :serial t
  :components ((:file "package")
               (:file "json")
               (:file "process")
               (:file "session"))
  :in-order-to ((test-op (test-op "nyxt-llm-cockpit/plain-tests"))))

(defsystem "nyxt-llm-cockpit/view"
  :version "0.1.0"
  :author "Pedro Delfino"
  :license "BSD-3-Clause"
  :description "A session as strings: HTML for entries, the status line, the page script."
  :depends-on ("nyxt-llm-cockpit/agent")
  :pathname "source/"
  :components ((:file "view"))
  :in-order-to ((test-op (test-op "nyxt-llm-cockpit/plain-tests"))))

(defsystem "nyxt-llm-cockpit/plain-tests"
  :description "Tests that need no Nyxt and no network: a stand-in program plays Claude Code."
  :depends-on ("nyxt-llm-cockpit/agent" "nyxt-llm-cockpit/view")
  :pathname "tests/"
  :serial t
  :components ((:file "harness")
               (:file "agent-tests")
               (:file "view-tests"))
  :perform (test-op (operation component)
                    (unless (symbol-call :nyxt-llm-cockpit/tests :run-tests)
                      (error "The plain tests failed."))))

(defsystem "nyxt-llm-cockpit/tests"
  :description "Tests of the cockpit inside a Nyxt image with no renderer."
  :depends-on ("nyxt-llm-cockpit" "nyxt-llm-cockpit/plain-tests")
  :pathname "tests/"
  :serial t
  :components ((:file "cockpit-tests"))
  :perform (test-op (operation component)
                    (unless (symbol-call :nyxt-llm-cockpit/tests :run-tests)
                      (error "The cockpit tests failed."))))
