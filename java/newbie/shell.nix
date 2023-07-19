with import <nixpkgs> {};
mkShell {
  packages = [
    jdt-language-server
    jre
    gradle
    vscode-extensions.vscjava.vscode-java-test
  ];
  shellHook = ''
    alias b="gradle build"
    alias r="gradle run"
  '';
}
