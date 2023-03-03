if status is-interactive
    alias fish_greeting /data/projects/sysfetch/sysfetch
    abbr a "abbr"
    abbr i "yay -Si"
    abbr R "yay -Rns"
    abbr S "yay -S"
    abbr v "nvim"
    abbr y "yay"
    abbr e "exit"
    abbr q "exit"
    abbr s "sudo"

    starship init fish | source
end
