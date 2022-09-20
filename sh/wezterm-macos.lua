local wezterm = require 'wezterm';
local act = wezterm.action


return {
  font = wezterm.font("MonoLisa"),
  color_scheme = "Dracula+",
  font_size = 13.0,
  scrollback_lines = 100000,
  keys = {
	  -- Turn off the default CMD-m Hide action, allowing CMD-m to
	  -- be potentially recognized and handled by the tab
	  {key="=", mods="CMD", action=wezterm.action{SendString=":="}},
	  -- some hack
	  {key="n", mods="CMD", action=act.SendKey{key="n", mods="CTRL"}},
	  {key="p", mods="CMD", action=act.SendKey{key="p", mods="CTRL"}},
	  {key=".", mods="CMD", action=act.SendKey{key=".", mods="ALT"}},
  }
}
