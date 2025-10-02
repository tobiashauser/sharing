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
      in rec {
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
        packages = let
          version = "0.1.0";    # synchronize with mix.exs
          src = ./.;
          mixFodDeps = erlangPackages.fetchMixDeps {
            inherit version src;
            pname = "sharing-deps";
            sha256 = "sha256-NEdvRaFXLQonuyFq95WTxUykjHs0Z1KHGGM6D/qSCuw=";
            mixEnv = "prod";
          };
          translatedPlatform =
            {
              aarch64-darwin = "macos-arm64";
              aarch64-linux = "linux-arm64";
              armv7l-linux = "linux-armv7";
              x86_64-darwin = "macos-x64";
              x86_64-linux = "linux-x64";
            } .${system};
        in {
          default = erlangPackages.mixRelease {
            inherit version src mixFodDeps;
            pname = "sharing";

            preInstall = ''
              ln -s ${pkgs.tailwindcss_4}/bin/tailwindcss _build/tailwind-${translatedPlatform}
              ln -s ${pkgs.esbuild}/bin/esbuild _build/esbuild-${translatedPlatform}

              ${elixir}/bin/mix assets.deploy
              ${elixir}/bin/mix phx.gen.release
            '';
          };
        };

        # Create a module.
        nixosModules.default = { config, lib, ... }: let
          cfg = config.services.sharing;
        in {
          options.services.sharing = with lib; {
            enable = mkEnableOption "sharing";

            port = lib.mkOption {
              type = lib.types.port;
              default = 4000;
              description = "Port to listen on, 4000 by default.";
            };

            secretKeyBaseFile = lib.mkOption {
              type = lib.types.path;
              description = "A file containing the Phoenix Secret Key Base. This should be secret, and not kept in the nix store.";
            };

            host = lib.mkOption {
              type = lib.types.str;
              description = "The host to configure the router generation from.";
            };

            user = lib.mkOption {
              type = lib.types.str;
              description = "The user under which to run the service.";
              default = "sharing";
            };

            dataDir = lib.mkOption {
              type = lib.types.str;
              description = "The directory in which to store the data.";
            };
          };

          config = lib.mkIf cfg.enable {
            assertions = [
              {
                assertion = cfg.secretKeyBaseFile != "";
                message = "A base key file is necessary";
              }
            ];

            users.users.${cfg.user} = {
              isSystemUser = true;
              group = cfg.user;
            };
            users.groups.${cfg.user} = {};

            systemd.services = {
              sharing = {
                description = "A simple file sharing server.";
                wantedBy = ["multi-user.target"];

                script = ''
                  # Elixir does not start up if `RELEASE_COOKIE` is not set,
                  # even though we set `RELEASE_DISTRIBUTION=none` so the cookie should be unused.
                  # Thus, make a random one, which should then be ignored.
                  export RELEASE_COOKIE=$(tr -dc A-Za-z0-9 < /dev/urandom | head -c 20)
                  export SECRET_KEY_BASE="$(< $CREDENTIALS_DIRECTORY/SECRET_KEY_BASE )"
        
                  ${packages.default}/bin/server
                '';

                serviceConfig = {
                  User = cfg.user;
                  WorkingDirectory = "${cfg.dataDir}";
                  Group = cfg.user;
                  LoadCredential = [
                    "SECRET_KEY_BASE:${cfg.secretKeyBaseFile}"
                  ];
                };

                environment = {
                  PHX_HOST = cfg.host;
                  # Disable Erlang's distributed features
                  RELEASE_DISTRIBUTION = "none";
                  # Additional safeguard, in case `RELEASE_DISTRIBUTION=none` ever
                  # stops disabling the start of EPMD.
                  ERL_EPMD_ADDRESS = "127.0.0.1";
                  # Home is needed to connect to the node with iex
                  HOME = "${cfg.dataDir}";
                  PORT = toString cfg.port;
                };
              };
            };
          };
        };
      }
    );
}

# Local Variables:
# eval: (add-hook 'after-save-hook #'envrc-reload nil t)
# End:
