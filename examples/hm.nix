{ self, inputs, ... }:

{
  flake.homeConfigurations.example = inputs.home-manager.lib.homeManagerConfiguration {
    pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
    modules = [
      self.homeModules.ordenada
      (
        { config, ... }:
        {
          ordenada.features = {
            userInfo.enable = true;
            home.enable = true;
            bash.enable = true;
            theme.enable = true;
            fontutils.enable = true;
            keyboard.enable = true;
            emacs = {
              enable = true;
              keymaps.enable = true;
              appearance.enable = true;
              ace-window.enable = true;
              all-the-icons.enable = true;
              modus-themes.enable = true;
              org = {
                enable = true;
                orgModern = true;
              };
              completion.enable = true;
              embark.enable = true;
              marginalia.enable = true;
              orderless.enable = true;
              vertico.enable = true;
              dired.enable = true;
              project.enable = true;
              programming = {
                enable = true;
                apheleia.enable = true;
                flymake.enable = true;
                rainbow-delimiters.enable = true;
                eglot.enable = true;
              };
              shell.enable = true;
              eshell.enable = true;
              vterm.enable = true;
              tramp.enable = true;
              help.enable = true;
              which-key.enable = true;
            };
            firefox.enable = true;
            nix.enable = true;
            compile.enable = true;
            direnv.enable = true;
            sway.enable = true;
            scripts.screenshot.enable = true;
            xdg.enable = true;
          };
          home.stateVersion = "25.05";
        }
      )
    ];
  };
}
