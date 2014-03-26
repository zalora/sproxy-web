{ config, pkgs, lib, ... }:
let
  inherit (lib) mkOption mkIf types;

  cfg = config.services.sproxy-web;

  configFile = pkgs.writeText "sproxy-web.conf" cfg.config;
in {
  imports = [
    <zalora-nix-lib/daemon-users-module.nix>
  ];

  options = {
    services.sproxy-web = {
      enable = mkOption {
        default = false;

        type = types.bool;

        description = "Enable the sproxy web interface";
      };

      package = mkOption {
        type = types.package;

        description = "The built sproxy-web package";
      };

      port = mkOption {
        type = types.int;

        default = 8003;

        description = "The port to listen on";
      };

      config = mkOption {
        type = types.lines;

        description = "The sproxy-web configuration";
      };
    };
  };

  config = mkIf cfg.enable {
    users.extraUsers.sproxy-web = {
      description = "sproxy-web daemon user";

      uid = config.zalora.daemonUids.sproxy-web;
    };

    services.sproxy-web.config = "port = ${toString cfg.port}";

    systemd.services.sproxy-web = {
      description = "Sproxy web interface";

      wantedBy = [ "multi-user.target" ];

      serviceConfig.ExecStart = "${cfg.package}/bin/sproxy-web --config=${configFile}";

      serviceConfig.User = "sproxy-web";
    };
  };
}
