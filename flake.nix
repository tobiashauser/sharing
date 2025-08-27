{
  description = "A simple file sharing server.";

  inputs = {
    miso.url = "github:dmjio/miso";
    nixpkgs.follows = "miso/nixpkgs";
    ghc-wasm-meta.follows = "miso/ghc-wasm-meta";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ghc-wasm-meta, ... }:
    utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs { inherit system; };

      indexHTML = pkgs.writeText "index.html" ''
        <!DOCTYPE html>
        <html>
          <head>
            <meta charset="utf-8">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <title>Sample miso WASM counter app</title>
          </head>
          <body>
            <script src="index.js" type="module"></script>
          </body>
        </html>
      '';

      indexJS = pkgs.writeText "index.js" ''
        import { WASI, OpenFile, File, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/index.js";
        import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";
        
        const args = [];
        const env = ["GHCRTS=-H64m"];
        const fds = [
          new OpenFile(new File([])), // stdin
          ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ''${msg}`)),
          ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ''${msg}`)),
        ];
        const options = { debug: false };
        const wasi = new WASI(args, env, fds, options);
        
        const instance_exports = {};
        const { instance } = await WebAssembly.instantiateStreaming(fetch("<target>.wasm"), {
          wasi_snapshot_preview1: wasi.wasiImport,
          ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
        });
        Object.assign(instance_exports, instance.exports);
        
        wasi.initialize(instance);
        await instance.exports.hs_start(globalThis.example);
      '';

      wasm-init = pkgs.writeShellApplication {
        name = "wasm-init";
        runtimeInputs = [
          pkgs.cabal-install
          ghc-wasm-meta.packages.${system}.all_9_12
          pkgs.gnumake
        ];

        text = ''
          cd "$NIX_FLAKE_DIR"
          target="$1"
          out="$target.wasmexe"
          mkdir -p "$out"
  
          if [ -z "$target" ] || [ ! -d "$target" ] || [ ! -d "$out" ]; then
            echo "Usage: wasm-init <target>"
            exit 1
          fi
  
          wasm32-wasi-cabal update
          wasm32-wasi-cabal build "$target" --allow-newer
  
          # This command produces `ghc_wasm_jsffi.js`, which ensures our FFI
          # works properly.
          "$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
            --input "$(wasm32-wasi-cabal list-bin "$target" --allow-newer)" \
            --output "$out"/ghc_wasm_jsffi.js
  
          # This copies the .wasm executable into the output directory.
          cp -f "$(wasm32-wasi-cabal list-bin "$target" --allow-newer)" "$out"
  
          cat ${indexHTML} > "$out/index.html"
          cat ${indexJS} > "$out/index.js"

          # Inject the target into index.js.
          sed -i 's/<target>/'"$target"'/g' "$out/index.js"
        '';
      };

      wasm-run = pkgs.writeShellApplication {
        name = "wasm-run";
        runtimeInputs = [
          ghc-wasm-meta.packages.${system}.all_9_12
          pkgs.http-server
        ];

        text = ''
          cd "$NIX_FLAKE_DIR"
          target="$1"
          out="$target.wasmexe"
  
          if [ -z "$target" ] || [ ! -d "$target" ] || [ ! -d "$out" ]; then
            echo "Usage: wasm-run <target>"
            exit 1
          fi

          http-server "$out" -c-1
        '';
      };

      wasm-build = pkgs.writeShellApplication {
        name = "wasm-build";
        runtimeInputs = [
          pkgs.cabal-install
          ghc-wasm-meta.packages.${system}.all_9_12
          pkgs.gnumake
        ];

        text = ''
          cd "$NIX_FLAKE_DIR"
          target="$1"
          out="$target.wasmexe"
  
          if [ -z "$target" ] || [ ! -d "$target" ] || [ ! -d "$out" ]; then
            echo "Usage: wasm-run <target>"
            exit 1
          fi

          wasm32-wasi-cabal build "$target" --allow-newer
          cp -f "$(wasm32-wasi-cabal list-bin "$target" --allow-newer)" "$out"
        '';
      };
    in {
      devShell = with pkgs; mkShell {
        name = "miso";
        packages = [
          cabal-install
          ghcid
          haskell.packages.ghc9122.ghc
          haskell.packages.ghc9122.haskell-language-server
          wasm-build
          wasm-init
          wasm-run
        ];
        shellHook = ''
          export NIX_FLAKE_DIR=$(git rev-parse --show-toplevel)
        '';
      };
    });
}

# Local Variables:
# eval: (add-hook 'after-save-hook #'envrc-reload nil t)
# End:
