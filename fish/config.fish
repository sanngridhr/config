if status is-interactive
   # aliases
    alias fish_greeting yass
    alias cat=bat
    alias ls="exa -lFh --git"
    alias rg="rg -ip"
    alias nvim-gtk="cd /data/projects && command nvim-gtk"

   # abbreviations
    abbr s "sudo"
    abbr i "yay -Si"
    abbr R "yay -Rns"
    abbr S "yay -S"
    abbr v "nvim"
    abbr y "yay"
    abbr e "exit"
    abbr q "exit"
    abbr sp "speedtest --secure"
    abbr r "rm -rfv"

   # variables
    set -x PAGER most
    set -x EDITOR nvim
    fish_add_path ~/.local/bin/

   # one dark palette
    set fish_color_command c678dd    #magenta
    set fish_color_quote 98c379      #green
    set fish_color_error be5046      #dark red
    set fish_color_param abb2bf      #white
    set fish_color_valid_path 61afef #blue
    set fish_color_option afbfbf     #light white
    set fish_color_comment 5c6370    #comment grey
    set fish_color_operator 61afef   #blue
    set fish_color_escape be5046     #dark red

   # starship
    starship init fish | source
end
