lfi:
{ config
, lib
, pkgs
, ...
}: let
  cfg = config.programs.literal-flake-input;
   inherit (lib) mkOption types optionals;
  inherit (types) ints;
in {
  options.programs.literal-flake-input = {
    enable = lib.mkEnableOption "literal-flake-input";
    port = mkOption {
      type = types.port;
      default = 8800;
      description = "HTTP port - service port";
    };
    ekg-port = mkOption {
      type = types.port;
      default = null;
      description = "EKG HTTP port - enable built-in monitoring if set";
    };
    cert = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "path to file with a certificate chain for HTTPS; HTTP is used if not set";
    };
    cert-key = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "path to file with a certificate private key for HTTPS; HTTP is used if not set";
    };
  };
  config = lib.mkIf cfg.enable {
    users = {
      groups.literal-flake-input = {};
      users.literal-flake-input = {
        group = "literal-flake-input";
        isSystemUser = true;
      };
    };
    systemd.services.literal-flake-input = {
      wantedBy = [ "network-online.target" ];
      requires = [ "network-online.target" ];
      enable = true;
      serviceConfig = {
        User = "literal-flake-input";
        Group = "literal-flake-input";
        Restart = "always";
        RestartSec = "8s";
        ExecStart =
          let
            ops = ["-p" (toString cfg.port)
                  ]
                  ++ optionals (cfg.ekg-port != null) [ "-e" (toString cfg.ekg-port) ]
                  ++ optionals (cfg.cert != null) [ "-g" cfg.cert ]
                  ++ optionals (cfg.cert-key != null) [ "-d" cfg.cert-key ];
          in "${lfi}/bin/literal-flake-input run ${lib.escapeShellArgs ops}";
      };
    };
  };
}
