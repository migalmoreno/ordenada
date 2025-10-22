{
  lib,
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "all-the-icons"
  ];
  homeManager =
    { config, pkgs, ... }:
    {
      home.packages = with pkgs; [ emacs-all-the-icons-fonts ];
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-all-the-icons";
        config = with config.ordenada.features.emacs; ''
          (with-eval-after-load 'all-the-icons
            (setopt all-the-icons-scale-factor 1.0)
            (setopt all-the-icons-default-adjust 0)
            (setopt all-the-icons-octicon-scale-factor 0.9))
          ${lib.optionalString completion.enable ''
            (eval-when-compile (require 'compat))
            (autoload 'all-the-icons-completion-mode "all-the-icons-completion")
            (all-the-icons-completion-mode)
            (define-minor-mode all-the-icons-completion-mode
              "Add icons to completion candidates."
              :global t
              (if all-the-icons-completion-mode
                (progn
                  (advice-add (compat-function completion-metadata-get)
                              :around
                              #'all-the-icons-completion-completion-metadata-get)
                  (advice-add #'completion-metadata-get
                              :around
                              #'all-the-icons-completion-completion-metadata-get))
                (advice-remove #'completion-metadata-get
                               #'all-the-icons-completion-completion-metadata-get)
                (advice-remove (compat-function completion-metadata-get)
                               #'all-the-icons-completion-completion-metadata-get)))
          ''}
          ${lib.optionalString marginalia.enable ''
            (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
          ''}
        '';
        elispPackages =
          with pkgs.emacsPackages;
          [
            all-the-icons
            compat
          ]
          ++ lib.optional config.ordenada.features.emacs.completion.enable all-the-icons-completion;
      };
    };
}
