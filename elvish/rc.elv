# Starship
eval (starship init elvish)

# Abbreviations
set edit:small-word-abbr = [
  &cat='bat -n'
  &cp='cp -v'
  &ga='git add .'
  &gc='git commit -s -m'
  &gch='git checkout'
  &gp='git push -v'
  &gpl='git pull -v'
  &grep='grep -ni --color'
  &ls='eza -F -Ghl --git --icons'
  &mv='mv -v'
  &nrs='nixos-rebuild switch --log-format multiline-with-logs --use-remote-sudo'
  &q='exit'
  &r='trash-put -v'
]

# Functions
fn pdefrag { |directory|
  ls $directory | peach { |d|
    btrfs fi de -r -czstd $directory/$d
    echo $directory/$d finished
  }
}
