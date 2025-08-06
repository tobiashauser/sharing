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
        inherit (nixpkgs.lib) strings map;
        sbclPackages = with pkgs.sbclPackages; [
          alexandria
          com_dot_inuoe_dot_jzon
          hunchentoot
        ];
        sbclWithPackages = pkgs.sbcl.withPackages (_: sbclPackages);
        loadPackage = package: "(asdf:load-system '${package.pname})";
        sbclPackagesLoadFile = pkgs.writeText "packages.lisp" ''
          (load (sb-ext:posix-getenv "ASDF"))
          ${strings.concatStringsSep "\n" (map loadPackage sbclPackages)}
        '';
      in
        {
          devShell = with pkgs; mkShell rec {
            buildInputs = [
              sbclWithPackages
              bun
            ];
            
            packages = [
              sbclWithPackages
              bun
            ];

            LD_LIBRARY_PATH = "${lib.makeLibraryPath buildInputs}";

            shellHook = ''
              ln -sf ${sbclPackagesLoadFile} "packages.lisp"
            '';
          };
        }
    );
}

# Local Variables:
# eval: (add-hook 'after-save-hook #'envrc-reload nil t)
# End:
