(c-add-style "myg-c-style"
             '("python"
               (indent-tabs-mode . nil)
               (c-basic-offset . 4)
               (c-offsets-alist
                (arglist-close . c-lineup-close-paren))))
(setq-default c-default-style "myg-c-style")
