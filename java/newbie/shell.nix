with import <nixpkgs> {};
mkShell {
  packages = [
    jdt-language-server
    jre
    gradle
  ];
  shellHook = ''
    alias b="gradle build"
    alias r="gradle run"
  '';
}
