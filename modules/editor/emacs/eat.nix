{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "eat"
  ];
  options.numberOfHistoryItemsToKeep = lib.mkOption {
    type = lib.types.number;
    default = 1024;
    description = "Number of input history items to keep.";
  };
  homeManager =
    { config, pkgs, ... }:
    {
      programs.bash.bashrcExtra = ''
        if [ -n "$EAT_SHELL_INTEGRATION_DIR" ]; then
          source "$EAT_SHELL_INTEGRATION_DIR/bash"
        fi
      '';
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-eat";
        config = with config.ordenada.features.emacs; ''
          (with-eval-after-load 'eat
            (setopt eat-line-input-ring-size ${toString eat.numberOfHistoryItemsToKeep})
            (setopt eat-term-scrollback-size nil)
            (setopt eat-kill-buffer-on-exit t)
            (setopt eat-enable-shell-prompt-annotation nil))
          ${lib.optionalString eshell.enable ''
            (add-hook 'eshell-load-hook #'eat-eshell-mode)
            (with-eval-after-load 'eshell
              (setopt eshell-visual-commands nil))
          ''}
          ${lib.optionalString project.enable ''
            (with-eval-after-load 'project
              (define-key project-prefix-map "E" #'eat-project))
          ''}
        '';
        elispPackages = with pkgs.emacsPackages; [ eat ];
      };
    };
}
