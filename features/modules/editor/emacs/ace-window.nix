{ mkFeature, ordenada-lib, ... }:

mkFeature {
  name = [
    "emacs"
    "ace-window"
  ];
  homeManager =
    { pkgs, ... }:
    {
      programs.emacs = ordenada-lib.mkElispConfig pkgs {
        name = "ordenada-ace-window";
        config = ''
          (keymap-global-set "M-o" #'ace-window)
          (with-eval-after-load 'ace-window
            (setopt aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
            (setopt aw-background nil)
            (setopt aw-scope 'frame)
            (setopt aw-ignore-current nil)
            (setopt aw-display-mode-overlay nil))
        '';
        elispPackages = with pkgs.emacsPackages; [ ace-window ];
      };
    };
}
