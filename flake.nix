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
            sha256 = "sha256-WF80osgi5lOwSXXJo5pbEl4tlJJT1CYXoiZ0+q879kw=";
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
      nixosModules.default = { config, lib, pkgs, ...}: let
        cfg = config.services.sharing;
      in {
        options.services.sharing = with lib; {
          enable = mkEnableOption "sharing";
          enableNginx = mkEnableOption "sharing-nginx";
          enableSSL = mkEnableOption "sharing-ssl";

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
            description = "The address of the webserver (regardless always on localhost).";
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

          secretKeyBaseFile = lib.mkOption {
            type = lib.types.path;
            description = "A file containing the Phoenix Secret Key Base. This should be secret, and not kept in the nix store.";
          };

          releaseCookieFile = lib.mkOption {
            type = lib.types.path;
            description = "A file containing a release cookie. This should be secret, and not kept in the nix store.";
          };

          address = mkOption {
            description = "The URL for the server.";
            type = types.str;
          };
        };

        config = lib.mkIf cfg.enable {
          # Create a new user.
          users.users.${cfg.user} = {
            isSystemUser = true;
            group = cfg.group;
            # home = cfg.dataDir;
            # createHome = true;
          };
          users.groups.${cfg.group} = {};

          systemd.services.sharing = {
            description = "Sharing";
            wantedBy = [ "multi-user.target" ];
            after = [ "network.target" ];

            script = ''
              # Elixir does not start up if `RELEASE_COOKIE` is not set,
              # even though we set `RELEASE_DISTRIBUTION=none` so the cookie should be unused.
              # Thus, make a random one, which should then be ignored.
              # export RELEASE_COOKIE=$(tr -dc A-Za-z0-9 < /dev/urandom | head -c 20)
              export RELEASE_COOKIE="$(< $CREDENTIALS_DIRECTORY/RELEASE_COOKIE )"
              export SECRET_KEY_BASE="$(< $CREDENTIALS_DIRECTORY/SECRET_KEY_BASE )"
    
              ${cfg.package}/bin/server
            '';

            serviceConfig = {
              User = cfg.user;
              Group = cfg.group;
              WorkingDirectory = cfg.dataDir;
              LoadCredential = [
                "SECRET_KEY_BASE:${cfg.secretKeyBaseFile}"
                "RELEASE_COOKIE:${cfg.releaseCookieFile}"
              ];
              Restart = "on-failure";
            };

            environment = {
              PHX_HOST = cfg.host;
              # Disable Erlang's distributed features.
              RELEASE_DISTRIBUTION = "none";
              # Additional safeguard, in case `RELEASE_DISTRIBUTION=none` ever
              # stops disabling the start of EPMD.
              ERL_EPMD_ADDRESS = "127.0.0.1";
              # Home is needed to connect to the node with iex.
              HOME = "${cfg.dataDir}";
              PORT = toString cfg.port;
            };
          };

          # A sample configuration for nginx.
          services.nginx = {
            enable = cfg.enableNginx;
            virtualHosts.sharing = {
              serverName = cfg.address;
              locations."/" = {
                proxyPass = "http://${cfg.host}:${toString cfg.port}";
                recommendedProxySettings = true;
              };
            } // (if cfg.enableSSL then {
              enableACME = true;
              forceSSL = true;
            } else { });
          };

          # We must configure the firewall to open ports 80 and 443.
          networking.firewall = {
            enable = true;
            # Port 80 needs to stay open to get ssl certificates.
            allowedTCPPorts = [ 80 443 ];
          };
        };
      };
    };
}

# Local Variables:
# eval: (add-hook 'after-save-hook #'envrc-reload nil t)
# End:
