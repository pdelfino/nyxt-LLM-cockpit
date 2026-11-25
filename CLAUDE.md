# nyxt-LLM-cockpit: instructions for Claude Code

An LLM cockpit for the Nyxt browser, as a Nyxt extension in Common Lisp. The first agent it drives is Claude Code. Read `README.org` for what it is and `docs/architecture.org` before changing anything: the layers, the protocol and the three safety rules are there. The plan is issue #1; every unit of work is an issue with `## Verify` and `## Do not touch`.

## Layout (keep it)

```
source/agent/        the Claude Code program as a session. No Nyxt, no libraries
source/view.lisp     a session as strings: HTML, status line, the page script
source/cockpit.lisp  the only file that names Nyxt: mode, page, commands
tests/               harness, agent-tests, view-tests (plain SBCL), cockpit-tests (inside Nyxt)
tests/run-install.lisp  the README's install lines, in a Nyxt set up like the packaged app
tests/fake-claude    a shell script that plays Claude Code for the tests
.nyxt/               Nyxt's source at the pinned release, fetched by make nyxt-deps. Not committed
```

Lisp names are lowercase: the systems and packages are `nyxt-llm-cockpit`, `nyxt-llm-cockpit/agent` and `nyxt-llm-cockpit/view`. The repository keeps the capitals.

## Gate

`make check` before every commit: `make test` (plain SBCL), `make nyxt-test` (inside Nyxt's core, no window), then `make nyxt-install-test` (the README's install lines, in a Nyxt whose ASDF is set up like the packaged app's). A compile warning fails it. `make real` drives the real Claude Code on Haiku and spends a little: run it when `source/agent/` changes, never in a loop.

Nothing here can open a Nyxt window from a session. A change to what the page shows or how keys behave is not verified until Pedro has seen it in Nyxt; say so. The same holds for the packaged app: the tests load the cockpit into a Nyxt built from source, and the first run in the macOS app failed on something they could not see (`docs/architecture.org`, "What the first run in the app found"). When the install lines of `README.org` change, `make nyxt-install-test` must stay green, and the change is not proven until it has run in the app.

## Rules

- Lisp talks to the page, the page never talks to Lisp. No `lisp-url` callback, no `nyxt/ps:lisp-eval`, no `:nbutton`, no `onclick`, no custom scheme. The page shows text nobody here wrote, so it is given nothing to call.
- A tool runs only after an answer the user typed. The default of every permission path is deny. Never add an allow that a click, a URL parameter or a page could trigger.
- Text from the model, a tool, a page or the user is data: through `escape-html`, or as a `script-call` argument. Never `:raw`, never concatenated into markup or script.
- Opening the `claude` page starts nothing and takes no parameters.
- No API key, token or credential in code, settings, tests or logs. Claude Code uses its own login.
- `source/agent/` and `source/view.lisp` depend on nothing but ASDF's UIOP and SBCL. `source/cockpit.lisp` may use what the stock Nyxt binary already carries, and nothing else.
- One file knows the program's frames: `source/agent/session.lisp`. One file knows Nyxt: `source/cockpit.lisp`.
- New behavior gets a test in the cheapest layer that can hold it. A new frame gets a scenario in `tests/fake-claude`, cut down from what the real program printed.
- Keys are in Nyxt's notation: `S-` is super (the Command key on a Mac) and `s-` is shift, the reverse of Emacs. A new binding gets a press through `nyxt:dispatch-key-press`, as `return-sends-what-is-typed` does, not only a lookup in the keymap.
- The Nyxt release is pinned by `NYXT_VERSION` in the `Makefile`. Moving it is its own commit, with `make nyxt-test` green on the new release.
- Nothing is filed or pushed to `atlas-engineer` without Pedro's word.
- Docs are Org. Markdown only where a tool requires it (this file, GitHub issue bodies). No em-dashes anywhere, comments included. Dates for people carry the month name.
- Draft pull requests, opened after a green `make check`. Conventional Commits prefixes (`feat:`, `fix:`, `test:`, `docs:`, `chore:`). Explain why, not what.
