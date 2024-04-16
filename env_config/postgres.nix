# This module automatically configures postgres when the user enters the
# devshell.
#
# To start the server, invoke `postgres` in one devshell. Then start a second
# devshell to run the clients.
{ lib, pkgs, config, ... }:
with lib;
let
  devshellSrc = fetchTarball
    "https://github.com/numtide/devshell/archive/cd4e2fda3150dd2f689caeac07b7f47df5197c31.tar.gz";

  # Because we want to be able to push pure JSON-like data into the
  # environment.
  strOrPackage =
    import (devshellSrc + "/nix/strOrPackage.nix") { inherit lib pkgs; };

  cfg = config.services.postgres;
  createDB = optionalString cfg.createUserDB ''
    echo "CREATE DATABASE ''${USER:-$(id -nu)};" | postgres --single -E postgres
  '';

  setup-postgres = pkgs.writeShellScriptBin "setup-postgres" ''
    set -euo pipefail
    export PATH=${cfg.package}/bin:${pkgs.coreutils}/bin

    # Abort if the data dir already exists
    [[ ! -d "$PGDATA" ]] || exit 0

    initdb ${concatStringsSep " " cfg.initdbArgs}

    cat >> "$PGDATA/postgresql.conf" <<EOF
      listen_addresses = '${cfg.listenAddresses}'
      unix_socket_directories = '$PGHOST'
    EOF

    ${createDB}
  '';

  start-postgres = pkgs.writeShellScriptBin "start-postgres" ''
    set -euo pipefail
    ${setup-postgres}/bin/setup-postgres
    exec ${cfg.package}/bin/postgres
  '';
in {
  options.services.postgres = {
    package = mkOption {
      type = strOrPackage;
      description = "Which version of postgres to use";
      default = pkgs.postgresql;
      defaultText = "pkgs.postgresql";
    };

    setupPostgresOnStartup = mkEnableOption "call setup-postgres on startup";

    createUserDB = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Create a database named like current user on startup.
        This option only makes sense when `setupPostgresOnStartup` is true.
      '';
    };

    listenAddresses = mkOption {
      type = types.str;
      default = "";
      description = ''
        Specifies the TCP/IP address(es) on which the server is to listen for connections from client applications
      '';
    };

    initdbArgs = mkOption {
      type = with types; listOf str;
      default = [ "--no-locale" ];
      example = [ "--data-checksums" "--allow-group-access" ];
      description = ''
        Additional arguments passed to <literal>initdb</literal> during data dir
        initialisation.
      '';
    };

  };
  config = {
    packages = [ cfg.package setup-postgres start-postgres ];

    env = [
      {
        name = "PGDATA";
        eval = "$PRJ_DATA_DIR/postgres";
      }
      {
        name = "PGHOST";
        eval = "$PGDATA";
      }
    ];

    devshell.startup.setup-postgres.text =
      lib.optionalString cfg.setupPostgresOnStartup ''
        ${setup-postgres}/bin/setup-postgres
      '';
  };
}
