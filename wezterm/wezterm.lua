local wezterm = require 'wezterm'
local config = {}

config.color_scheme = "OneHalfDark"
config.font = wezterm.font("monospace")
config.font_size = 11

config.hide_tab_bar_if_only_one_tab = true

return config
