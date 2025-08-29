{
  description = "A simple file sharing server.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs { inherit system; };

    in {
      devShell = with pkgs; mkShell {
        name = "sharing";
        packages = with haskell.packages.ghc9122; [
          cabal-install
          ghc
          ghcid
          haskell-language-server
        ] ++ [
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
