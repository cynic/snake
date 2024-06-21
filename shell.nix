{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    # nativeBuildInputs is usually what you want -- tools you need to run
    nativeBuildInputs = with pkgs.buildPackages; [
      elmPackages.elm # for frontend
      nodejs_21 # for npm (task-runner, elm-watch, etc)
    ];
}
