{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "nyxt"
    "blocker"
  ];
  options.hosts = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    description = "List of hosts to block.";
    default = [ ];
  };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.nyxt = ordenada-lib.mkNyxtLispConfig pkgs {
        name = "ordenada-blocker";
        config = # lisp
          ''
            (define-configuration web-buffer
              ((default-modes `(nyxt/mode/blocker:blocker-mode
                                ,@%slot-value%))))

            ${lib.optionalString config.ordenada.features.nyxt.appearance.enableModeGlyphs ''
              (define-configuration nyxt/mode/blocker:blocker-mode
                ((glyph "⨂")))
            ''}

            (define-configuration nyxt/mode/blocker:blocker-mode
              ((nyxt/mode/blocker:hostlists
                (cons
                 (nyxt/mode/blocker:make-hostlist
                  :hosts '(${toString config.ordenada.features.nyxt.blocker.hosts}))
                 %slot-default%))))
          '';
      };
    };
}
