{
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
  homeManager =
    { config, pkgs, ... }:
    {
      xdg.dataFile."nyxt/extensions/nx-mosaic".source = inputs.nx-mosaic;
      programs.nyxt = ordenada-lib.mkNyxtLispConfig pkgs {
        name = "ordenada-mosaic";
        config = # lisp
          ''
            (local-time:reread-timezone-repository :timezone-repository "/etc/zoneinfo")
            (setf local-time:*default-timezone*
                  (local-time:find-timezone-by-location-name "${config.ordenada.features.hostInfo.timeZone}"))

            (define-configuration browser
              ((default-new-buffer-url (quri:uri "nyxt:nx-mosaic:mosaic"))))

            (define-configuration mosaic:time-widget
              ((mosaic:timezone "${config.ordenada.features.hostInfo.timeZone}")))
          '';
        lispPackages = [ "nx-mosaic" ];
      };
    };
}
