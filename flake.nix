{
  description = "A simple file sharing server.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        devShells.default = with pkgs; mkShell {
          name = "phoenix";
          packages = with pkgs.beam.packages.erlang; [
            elixir
            elixir-ls
            erlang-ls
          ] ++ [
            nodejs_24

            # Manually bundle with application
            rust-petname
            qrrs
          ];

          # E.g. install phx with `mix archive.install hex phx_new'.
          shellHook = ''
            # Use mix in the local directory.
            mkdir -p .nix-mix .nix-hex
            export MIX_HOME=$PWD/.nix-mix
            export HEX_HOME=$PWD/.nix-hex

            # Make hex from nixpkgs available.
            export MIX_PATH="${pkgs.beam.packages.erlang.hex}/lib/erlang/lib/hex/ebin"
            export PATH=$MIX_HOME/bin:$HEX_HOME/bin:$PATH

            # Keep the shell history in iex.
            export ERL_AFLAGS="-kernel shell_history enabled"

            # Dev or production?
            export MIX_ENV=dev
          '';
        };
      }
    );
}

# Local Variables:
# eval: (add-hook 'after-save-hook #'envrc-reload nil t)
# End:
