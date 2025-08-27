;;; Directory Local Variables -*- no-byte-compile: t -*-
((nil
  . ((emx/project-compile-commands
      . (;; ("run" . "cabal run client")
	 ("develop" . "ghcid -c 'cabal repl client' -T=Main.main")
	 ("run" . "wasm-build client; wasm-run client")
	 ("compile" . "wasm-build client")
	 )))))
