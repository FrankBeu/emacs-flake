{ ... }:
{
  environment = {
    shellAliases =
      {
        e    = "emacsclient -cn -a \"\" "                   ;### Emacs
        et   = "emacsclient -ct -a \"\" "                   ;### EmacsTerminal
        enn  = "emacsclient -c  -a \"\" "                   ;### EmacsNoNowait

        ea   = "emacsclient ~/NOTES/AKTUELLES.org"          ;### EmacsAktuelles
        er   = "emacsclient ~/ROUTINES/ROUTINES.org"        ;### EmacsRoutinen

        emre = "pkill emacs && systemctl start emacs --user";### EMacsREstart
      };
  };
}

