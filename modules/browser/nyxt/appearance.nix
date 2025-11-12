{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

let
  getDefaultThemes =
    config: with config.ordenada.features.theme.defaultSchemes; {
      light = with light.withHashtag; {
        name = "ordenada-light";
        background-color = base00;
        background-color- = base01;
        on-background-color = base05;
        primary-color = base0C;
        on-primary-color = base01;
        secondary-color = base0B;
        on-secondary-color = base04;
        accent-color = base0D;
        on-accent-color = base00;
        font-family = config.ordenada.features.fontutils.fonts.sans.name;
        monospace-font-family = config.ordenada.features.fontutils.fonts.monospace.name;
      };
      dark = with dark.withHashtag; {
        name = "ordenada-dark";
        background-color = base00;
        background-color- = base01;
        on-background-color = base05;
        primary-color = base0C;
        on-primary-color = base01;
        secondary-color = base0B;
        on-secondary-color = base04;
        accent-color = base0D;
        on-accent-color = base00;
        font-family = config.ordenada.features.fontutils.fonts.sans.name;
        monospace-font-family = config.ordenada.features.fontutils.fonts.monospace.name;
      };
    };
in
mkFeature {
  name = [
    "nyxt"
    "appearance"
  ];
  options =
    with lib;
    { config, ... }:
    {
      statusBufferHeight = mkOption {
        type = types.int;
        description = "The height of the status buffer (message line below mode line).";
        default = 26;
      };
      enableModeGlyphs = mkEnableOption "showing mode glyphs in the mode line";
      defaultThemes = mkOption {
        type = types.attrs;
        description = "The themes to use for Nyxt.";
        default = getDefaultThemes config;
      };
    };
  homeManager =
    { config, pkgs, ... }:
    let
      nyxtTheme = with config.ordenada.features; nyxt.appearance.defaultThemes.${theme.polarity};
    in
    {
      programs.nyxt = ordenada-lib.mkNyxtLispConfig pkgs {
        name = "ordenada-appearance";
        config =
          with config.ordenada.features.nyxt.appearance; # lisp
          ''
            (define-configuration status-buffer
              ((glyph-mode-presentation-p ${ordenada-lib.lisp.toBoolean enableModeGlyphs})
               (height ${toString statusBufferHeight})))

            (define-configuration browser
              ((theme (make-instance 'theme:theme
                                     ${toString (lib.mapAttrsToList (n: v: ":${n} \"${v}\"") nyxtTheme)}))))

            (defmethod format-status-buttons :around ((status status-buffer))
              (spinneret:with-html-string
                  (:nbutton
                   :buffer status
                   :text (:raw (glyph-left status))
                   :title "Backwards"
                   '(nyxt/mode/history:history-backwards))
                (:nbutton
                 :buffer status
                 :text (:raw (glyph-reload status))
                 :title "Reload"
                 '(nyxt:reload-current-buffer))
                (:nbutton
                 :buffer status
                 :text (:raw (glyph-right status))
                 :title "Forwards"
                 '(nyxt/mode/history:history-forwards))
                (:nbutton
                 :buffer status
                 :text (:raw "${''
                   <svg width='5' height='5' viewBox='0 0 5 5' overflow='visible' stroke='currentColor' stroke-width='1.2'>
                     <line x2='5' y2='5' />
                     <line x1='5' y2='5' />
                   </svg>
                 ''}")
                 :title "Close"
                 '(nyxt:delete-current-buffer))))

            (defmethod format-status :around ((status status-buffer))
              (let ((buffer (current-buffer (window status))))
                (spinneret:with-html-string
                    (:div :id "container"
                          (:div :id "controls"
                                (:raw (format-status-buttons status)))
                          (:div :id "url"
                                (:raw
                                 (format-status-load-status status)
                                 (format-status-url status)))
                          (:div :id "modes"
                                :title (nyxt::modes-string buffer)
                                (:raw
                                 (format-status-modes status)))))))

            (define-configuration prompt-buffer
                ((mouse-support-p nil)))
          '';
      };
    };
}
