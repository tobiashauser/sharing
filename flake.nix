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
          # bun
          # http-server
          nodejs_24
          purescript-language-server
          purs-tidy
        ]);
        shellHook = ''
          export NIX_FLAKE_DIR=$(git rev-parse --show-toplevel)
        '';
      };

      # spago install must have been run for this to build...
      packages.frontend = pkgs.mkSpagoDerivation {
        version = "0.1.0";
        src = ./.;
        
        nativeBuildInputs = with pkgs; [
          esbuild
          purs-backend-es
          purs
          spago-unstable
        ];

        # The option --no-build in purs-backend-es fails.
        buildPhase = ''
          spago bundle
          purs-backend-es bundle-app --minify 
        '';

        installPhase = ''
          mkdir $out
          cp index.js $out
        '';
      };
    });
}

# Local Variables:
# eval: (add-hook 'after-save-hook #'envrc-reload nil t)
# End:
