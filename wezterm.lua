-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
config.color_scheme = 'OneHalfDark'
config.font = wezterm.font 'Fira Code'
config.freetype_load_target = 'HorizontalLcd'
config.window_decorations = 'NONE'
config.enable_tab_bar = false
config.window_background_opacity = 0.8


-- and finally, return the configuration to wezterm
return config
