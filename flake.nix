{
  description = "Packages and tools for Common Lisp.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lisp = pkgs.sbcl.withPackages (ps: with ps; [
          # Add packages here:
          hunchentoot
        ]);
      in
        {
          devShell = with pkgs; mkShell {
            packages = [
              lisp
              bun
            ];
          };
        }
    );
}

# Local Variables:
# eval: (add-hook 'after-save-hook #'envrc-reload)
# End:
