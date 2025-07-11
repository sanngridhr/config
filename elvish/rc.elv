# Starship
eval (starship init elvish)

# Abbreviations
set edit:abbr = [
  &cat='bat -n'
  &cp='cp -v'
  &ga='git add .'
  &gc='git commit -s -m'
  &grep='grep -ni --color'
  &ls='eza -F -Ghl --git --icons'
  &mv='mv -v'
  &nixos-rebuild='nixos-rebuild --log-format multiline-with-logs --use-remote-sudo'
  &q='exit'
]
