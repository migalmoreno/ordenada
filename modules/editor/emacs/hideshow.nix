{
  mkFeature,
  ordenada-lib,
  ...
}:

mkFeature {
  name = [
    "emacs"
    "hideshow"
  ];
  homeManager =
    { pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-hideshow";
        config = # elisp
          ''
            (add-hook 'prog-mode-hook 'hs-minor-mode)

            (setq hs-minor-mode-map (make-sparse-keymap)) ;; Deleting default bindings
            (defvar ordenada-hs-minor-mode-command-map nil
              "Map to bind `hs' commands under.")
            (define-prefix-command 'ordenada-hs-minor-mode-command-map)

            (let ((map ordenada-hs-minor-mode-command-map))
              (keymap-set map "@" '("Toggle hide / show" . hs-toggle-hiding))
              (keymap-set map "-" '("hide all" . hs-hide-all))
              (keymap-set map "+" '("show all" . hs-show-all))
              (keymap-set map "h" '("hide block" . hs-hide-block))
              (keymap-set map "s" '("show block" . hs-show-block))
              (keymap-set map "H" '("hide level" . hs-hide-level))
              (keymap-set map "f" '("show level" . hs-show-level))
              (keymap-set map "c" '("hide initial comment" . hs-hide-initial-initial-comment-block)))

            (keymap-set prog-mode-map "C-c @"
                        '("hide / show" . ordenada-hs-minor-mode-command-map))
          '';
        elispPackages = [];
      };
    };
}
