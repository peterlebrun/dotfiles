local wezterm = require 'wezterm'

return {
    font = wezterm.font('Fira Code', { weight='Regular' }),
    font_size = 18,
    initial_rows = 30,
    initial_cols = 120,
    colors = {
        background = '#333',
    },
}
