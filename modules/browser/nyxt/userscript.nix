{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "nyxt"
    "userscript"
  ];
  options = {
    userscripts = lib.mkOption {
      type = lib.types.nullOr lib.types.lines;
      description = ''
        List of userscripts to add. See `nyxt:manual#user-scripts` inside Nyxt
        to learn how to construct them.
      '';
      default = null;
    };
    userstyles = lib.mkOption {
      type = lib.types.nullOr lib.types.lines;
      description = "List of userstyles to add.";
      default = null;
    };
  };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.nyxt = ordenada-lib.mkNyxtLispConfig pkgs {
        name = "ordenada-userscript";
        config =
          with config.ordenada.features.nyxt.userscript; # lisp
          ''
            (define-configuration web-buffer
              ((default-modes `(nyxt/mode/user-script:user-script-mode
                                ,@%slot-value%))))

            ${lib.optionalString config.ordenada.features.nyxt.appearance.enableModeGlyphs ''
              (define-configuration nyxt/mode/user-script:user-script-mode
                ((glyph "★")))
            ''}

            (define-configuration nyxt/mode/user-script:user-script-mode
                (${
                  lib.optionalString (userstyles != null) ''
                    (nyxt/mode/user-script:user-styles (list ${userstyles}))
                  ''
                }
                ${
                  lib.optionalString (userscripts != null) ''
                    (nyxt/mode/user-script:user-scripts (list ${userscripts}))
                  ''
                }))
          '';
      };
    };
}
