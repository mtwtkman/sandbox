with import <nixpkgs> {};
mkShell {
  packages = [
    jdt-language-server
    jre
    gradle
  ];
  shellHook = ''


  '';
}
