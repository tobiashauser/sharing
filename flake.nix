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
        erl = pkgs.beam.interpreters.erlang;
        erlangPackages = pkgs.beam.packagesWith erl;
        elixir = erlangPackages.elixir;
      in {
        devShells.default = with pkgs; mkShell {
          name = "phoenix";
          packages = with erlangPackages; [
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

        # Provide a package. This follows the tutorial at
        # https://www.curiosum.com/blog/packaging-elixir-application-with-nix.
        packages.default = let
          version = "0.1.0";    # synchronize with mix.exs
          src = ./.;
          mixFodDeps = erlangPackages.fetchMixDeps {
            inherit version src;
            pname = "sharing-deps";
            sha256 = "sha256-NEdvRaFXLQonuyFq95WTxUykjHs0Z1KHGGM6D/qSCuw=";
            mixEnv = "prod";
          };
          translatedPlatform = {
            aarch64-darwin = "macos-arm64";
            aarch64-linux = "linux-arm64";
            armv7l-linux = "linux-armv7";
            x86_64-darwin = "macos-x64";
            x86_64-linux = "linux-x64";
          }.${system};
        in erlangPackages.mixRelease {
          inherit version src mixFodDeps;
          pname = "sharing";

          preInstall = ''
            ln -s ${pkgs.tailwindcss_4}/bin/tailwindcss _build/tailwind-${translatedPlatform}
            ln -s ${pkgs.esbuild}/bin/esbuild _build/esbuild-${translatedPlatform}

            ${elixir}/bin/mix assets.deploy
            ${elixir}/bin/mix phx.gen.release
          '';
        };
      }
    ) // {

      # Create a service for deployment on nixOS. This is architecture
      # independent (well, only nixOS by design).
      nixosModules.default = { config, lib, ...}: let
        cfg = config.services.sharing;
      in {
        options.services.sharing = with lib; {
          enable = mkEnableOption "sharing";

          user = mkOption {
            description = "The user which runs the service.";
            type = types.str;
            default = "sharing";
          };

          group = mkOption {
            description = "The group the user belongs to.";
            type = types.str;
            default = config.services.sharing.user;
          };

          host = mkOption {
            description = "The adress of the webserver (regardless always on localhost).";
            type = with types; nullOr str;
            default = null;
          };

          port = mkOption {
            description = "The port on which the application runs.";
            type = types.number;
            default = 4000;
          };

          dataDir = mkOption {
            description = "The path to the directory where the uploads are stored.";
            type = with types; nullOr str;
            default = null;
          };

          package = mkOption {
            description = "Connect to the default package output.";
            type = types.package;
          };
        };

        config = lib.mkIf cfg.enable {
          # Create a new user.
          users.users.${cfg.user} = {
            isSystemUser = true;
            group = cfg.group;
          };
          users.groups.${cfg.group} = {};

          systemd.services.sharing = {
            description = "A simple file sharing server.";
            wantedBy = [ "multi-user.target" ];
            after = [ "network.target" ];

            serviceConfig = {
              User = cfg.user;
              Group = cfg.group;
              WorkingDirectory = cfg.dataDir;
              ExecStart = "${cfg.package}/bin/server";
              Restart = "always";
            };

            environment = {
              PHX_HOST = cfg.host;
              PORT = toString cfg.port;
            };
          };
        };
      };
    };
}

# Local Variables:
# eval: (add-hook 'after-save-hook #'envrc-reload nil t)
# End:
