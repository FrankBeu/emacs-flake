{ ... }:
{
  environment = {
    shellAliases =
      {
        e    = "emacsclient -nc -a \"\" "; ### Emacs
        et   = "e           -tc -a \"\" "; ### EmacsTerminal
        enn  = "e            -c -a \"\" "; ### EmacsNoNowait

        ea   = "e ~/NOTES/AKTUELLES.org";  ### EmacsAktuelles
        er   = "e ~/ROUTINES/ROUTINES.org";   ### EmacsRoutinen

        emre = "pkill emacs && systemctl start emacs --user"; ### EMacsREstart
      };
  };
}

