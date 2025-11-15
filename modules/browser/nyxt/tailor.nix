{
  inputs,
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "nyxt"
    "tailor"
  ];
  options = {
    enableAutoSwitch = lib.mkEnableOption "automatic time-based switching of nx-tailor themes";
    extraConfig = lib.mkOption {
      type = lib.types.lines;
      description = "Extra configuration to add in nx-tailor config file.";
      default = '''';
    };
  };
  homeManager =
    { config, pkgs, ... }:
    {
      xdg.dataFile."nyxt/extensions/nx-tailor".source = inputs.nx-tailor;
      programs.nyxt = ordenada-lib.mkNyxtLispConfig pkgs {
        name = "ordenada-tailor";
        config =
          let
            inherit (config.ordenada.features.nyxt.appearance.defaultThemes) light dark;
            mkTheme = theme: ''
              (make-instance 'tailor:user-theme ${toString (lib.mapAttrsToList (n: v: ":${n} \"${v}\"") theme)})
            '';
          in
          with config.ordenada.features.nyxt;
          # lisp
          ''
              ${lib.optionalString tailor.enableAutoSwitch ''
                (local-time:reread-timezone-repository :timezone-repository "/etc/zoneinfo")
                (setf local-time:*default-timezone*
                      (local-time:find-timezone-by-location-name "${config.ordenada.features.hostInfo.timeZone}"))
              ''}

            (define-configuration web-buffer
              ((default-modes `(tailor:tailor-mode ,@%slot-value%))))

            (define-configuration tailor:tailor-mode
              ((tailor:auto-p ${ordenada-lib.lisp.toBoolean tailor.enableAutoSwitch})
               (tailor:main "${appearance.defaultThemes.${config.ordenada.features.theme.polarity}.name}")
               (tailor:themes (list ${mkTheme light} ${mkTheme dark}))))

            ${extraConfig}
          '';
        lispPackages = [ "nx-tailor" ];
      };
    };
}
