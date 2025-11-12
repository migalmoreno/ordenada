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
    enableAutoSwitch = ordenada-lib.mkEnableTrueOption "switching Tailor themes automatically";
    extraConfig = lib.mkOption {
      type = lib.types.lines;
      description = "Extra configuration to be put in nx-router config file.";
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
          with config.ordenada.features.nyxt.tailor;
          # lisp
          ''
            (local-time:reread-timezone-repository :timezone-repository "/etc/zoneinfo")
            (setf local-time:*default-timezone*
                  (local-time:find-timezone-by-location-name "${config.ordenada.features.hostInfo.timeZone}"))

            (define-configuration web-buffer
              ((default-modes `(tailor:tailor-mode ,@%slot-value%))))

            (define-configuration tailor:tailor-mode
              ((tailor:auto-p ${ordenada-lib.lisp.toBoolean enableAutoSwitch})
               (tailor:themes (list ${mkTheme light} ${mkTheme dark}))))

            (define-configuration status-buffer
              ((style
                (tailor:with-style 'status-buffer
                  `(body
                    :font-family ,theme:monospace-font-family)
                  `("#container"
                    :background ,theme:background-)
                  `("#controls"
                    :color ,theme:on-background
                    :background inherit)
                  `("#controls button"
                    :padding "3px")
                  `("#url"
                    :background inherit
                    :color ,theme:on-background
                    :font-weight bold
                    :font-size "55vh"
                    :padding "0 5px"
                    :display flex
                    :align-items center
                    :box-sizing border-box
                    :flex-grow 6
                    :flex-shrink 3)
                  `("#url button"
                    :white-space nowrap
                    :text-overflow ellipsis
                    :overflow hidden)
                  `("#modes"
                    :background inherit
                    :color ,theme:on-background
                    :font-size "55vh"
                    :flex-grow 1)
                  `((:or .arrow-right .arrow-left)
                    :clip-path none
                    :margin-right 0)
                  `((:and (:or .button .tab "#url") :hover)
                    :background none
                    :color ,theme:on-background)))))

            (define-configuration prompt-buffer
              ((style
                (tailor:with-style 'prompt-buffer
                  `(body
                    :font-family ,theme:monospace-font-family
                    :color ,theme:on-background
                    :border none)
                  `("#input"
                    :padding-left "0 !important")
                  `("#input:focus"
                    :box-shadow none)
                  `("#prompt-area"
                    :background ,theme:background
                    :border none)
                  `("#prompt"
                    :padding-left "10px")
                  `("#suggestions"
                    :margin-right 0)
                  `("#selection"
                    :background ,(cl-colors2:print-hex theme:on-primary :alpha 0.5)
                    :color ,theme:on-background)
                  `(.source
                    :margin-left 0)
                  `(.source-name
                    :padding-left "10px"
                    :color ,theme:secondary
                    :background ,theme:background)
                  `(.source-content
                    :border-collapse collapse
                    :padding-left 0
                    :margin-left 0
                    (th
                     :padding "5px 10px"
                     :color ,theme:on-background
                     :background ,theme:background
                     :font-weight bold)
                    (td
                     :padding "5px 10px"
                     :text-overflow ellipsis))
                  `((:or "#prompt" "#prompt-extra")
                    :color ,theme:secondary
                    :background none)
                  `((:or .arrow-right .arrow-left)
                    :clip-path none
                    :margin-right 0)
                  `((:or "#prompt-modes" "#close-button" "#previous-source"
                         "#next-source" "#toggle-attributes")
                    :display none)))))

            ${extraConfig}
          '';
        lispPackages = [ "nx-tailor" ];
      };
    };
}
