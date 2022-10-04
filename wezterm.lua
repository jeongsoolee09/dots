local wezterm = require "wezterm"
-- local fennel = require "fennel"

return {
    font = wezterm.font 'D2Coding Ligature',
    font_size = 18.0,
    enable_tab_bar = false,
    disable_default_mouse_bindings = true,
    audible_bell = "Disabled",
    window_close_confirmation = "NeverPrompt",
    color_scheme = "Cupertino (base16)",
    keys = {
        {
          key = 'f',
          mods = 'CTRL|CMD',
          action = wezterm.action.ToggleFullScreen,
        },
    },
    native_macos_fullscreen_mode = true
}
