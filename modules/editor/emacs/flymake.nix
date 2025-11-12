{ mkFeature, ordenada-lib, ... }:

mkFeature {
  name = [
    "emacs"
    "flymake"
  ];
  homeManager =
    { pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-flymake";
        config = # elisp
          ''
            (with-eval-after-load 'flymake
              (let ((map flymake-mode-map))
                (keymap-set map "M-n" #'flymake-goto-next-error)
                (keymap-set map "M-p" #'flymake-goto-prev-error)))
          '';
      };
    };
}
