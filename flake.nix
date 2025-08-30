{
  description = "A simple file sharing server.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";

    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";

    mkSpagoDerivation.url = "github:jeslie0/mkSpagoDerivation";
  };

  outputs = inputs@{ self, nixpkgs, utils, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ] (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          inputs.purescript-overlay.overlays.default
          inputs.mkSpagoDerivation.overlays.default
        ];
      };

      # Create some convenient scripts to build and run the package.
      start-dev-server = pkgs.writeShellScriptBin "start-dev-server" ''
        ${pkgs.bun}/bin/bun i -D vite 2&>/dev/null
        ${pkgs.watchexec}/bin/watchexec -w src -e purs -- spago bundle --outfile public/index.js &
        ${pkgs.bun}/bin/bunx vite public 2&>/dev/null &
        wait
      '';
    in {
      devShells.default = pkgs.mkShell {
        name = "sharing";
        inputsFrom = builtins.attrValues self.packages.${system};
        packages = with pkgs.haskell.packages.ghc9122; [
          cabal-install
          ghc
          ghcid
          haskell-language-server
        ] ++ (with pkgs; [
          bun
          start-dev-server
          nodejs_24
          purescript-language-server
          purs-tidy
        ]);
        shellHook = ''
          export NIX_FLAKE_DIR=$(git rev-parse --show-toplevel)
        '';
      };

      # Still a work in progress.
      # `spago install` must have been run for this to build.
      packages.default = let
        indexHTML = pkgs.writeText "index.html" ''
          <!doctype html>
          <html>
            <head>
              <title>Sharing</title>
            </head>
            <body>
              <script src="./index.js"></script>
            </body>
          </html>
        '';
      in pkgs.mkSpagoDerivation {
        version = "0.1.0";
        src = ./.;
        
        nativeBuildInputs = with pkgs; [
          esbuild
          purs-backend-es
          purs
          spago-unstable
        ];

        # The option --no-build in purs-backend-es fails. However,
        # 'purs-backend-es' creates noticably smaller bundles and should be used
        # for production.
        buildPhase = ''
          spago bundle --minify
          # purs-backend-es bundle-app --minify 
        '';

        installPhase = ''
          mkdir $out
          cp -f index.js $out
          cat ${indexHTML} > $out/index.html
        '';
      };
    });
}

# Local Variables:
# eval: (add-hook 'after-save-hook #'envrc-reload nil t)
# End:
