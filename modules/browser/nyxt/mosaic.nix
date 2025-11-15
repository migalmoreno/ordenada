{
  lib,
  inputs,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "nyxt"
    "mosaic"
  ];
  options =
    { config, ... }:
    {
      greetingName = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        description = "Name to show in the nx-mosaic greeting.";
        default = builtins.elemAt (lib.splitString " " config.ordenada.features.userInfo.fullName) 0;
      };
      widgets = lib.mkOption {
        type = lib.types.listOf (
          lib.types.enum [
            "time"
            "greeting"
          ]
        );
        description = "List of nx-mosaic widgets.";
        default = [
          "time"
          "greeting"
        ];
      };
    };
  homeManager =
    { config, pkgs, ... }:
    {
      xdg.dataFile."nyxt/extensions/nx-mosaic".source = inputs.nx-mosaic;
      programs.nyxt = ordenada-lib.mkNyxtLispConfig pkgs {
        name = "ordenada-mosaic";
        config =
          with config.ordenada.features.nyxt.mosaic; # lisp
          ''
            (define-configuration browser
              ((default-new-buffer-url (quri:uri "nyxt:nx-mosaic:mosaic"))))

            (define-configuration mosaic:time-widget
              ((mosaic:timezone "${config.ordenada.features.hostInfo.timeZone}")))

            (define-configuration mosaic:greeting-widget
              ((mosaic:name ${ordenada-lib.lisp.toNilOr greetingName ''"${greetingName}"''})))

            (define-configuration mosaic:page
              ((mosaic:widgets (list ${
                toString (map (widget: "(make-instance 'mosaic:${widget}-widget)") widgets)
              }))))
          '';
        lispPackages = [ "nx-mosaic" ];
      };
    };
}
