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
        description = "Height of the status buffer (i.e. mode line).";
        default = 26;
      };
      enableModeGlyphs = mkEnableOption "showing mode glyphs in the mode line";
      defaultThemes = mkOption {
        type = types.attrs;
        description = "Themes to use for Nyxt.";
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
          with config.ordenada.features.nyxt;
          let
            mkStyle = name: style: ''
              ${
                if tailor.enable then
                  "(tailor:with-style '${name} ${style})"
                else
                  "(str:concat %slot-default% (theme:themed-css (theme *browser*) ${style}))"
              }
            '';
          in
          # lisp
          ''
            (define-configuration status-buffer
              ((glyph-mode-presentation-p ${ordenada-lib.lisp.toBoolean appearance.enableModeGlyphs})
               (height ${toString appearance.statusBufferHeight})))

            (define-configuration browser
              ((theme (make-instance 'theme:theme
                                     ${toString (lib.mapAttrsToList (n: v: ":${n} \"${v}\"") nyxtTheme)}))))

            (define-configuration status-buffer
              ((style ${mkStyle "status-buffer" ''
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
                  :color ,theme:on-background)
              ''})))

            (define-configuration prompt-buffer
              ((style ${mkStyle "prompt-buffer" ''
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
                  :display none)
              ''})))

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
        lispPackages = lib.optional config.ordenada.features.nyxt.tailor.enable "nx-tailor";
      };
    };
}
