local wezterm = require 'wezterm'
local act = wezterm.action

return {
    font = wezterm.font('Noto Sans Mono', { weight='Regular' }),
    font_size = 18,
    initial_rows = 30,
    initial_cols = 120,
    color_scheme = 'Dehydration (Gogh)',
    keys = {
        { key = '|', mods = 'CMD|SHIFT', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' } },
        { key = '-', mods = 'CMD', action = act.SplitVertical { domain = 'CurrentPaneDomain' } },
    },
}
