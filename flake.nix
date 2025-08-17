{
  description = "A simple file sharing server.";

  inputs = {
    miso.url = "github:dmjio/miso";
    nixpkgs.follows = "miso/nixpkgs";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs { inherit system; };
    in {
      devShell = with pkgs; mkShell {
        name = "miso";
        packages = [
          cabal-install
          ghcid
          haskell.packages.ghc9122.ghc
          haskell.packages.ghc9122.haskell-language-server
        ];
      };
    });
}

# Local Variables:
# eval: (add-hook 'after-save-hook #'envrc-reload nil t)
# End:
