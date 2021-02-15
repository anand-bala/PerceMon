with section("parse"):
    additional_commands = {
        "add_percemon_component": {
            "pargs": "1+",
        },
    }

with section("lint"):
    disabled_codes = [
        "C0113",
    ]

with section("format"):
    dangle_parens = True
    line_ending = "unix"
    line_width = 88
    tab_size = 2
    keyword_case = "upper"

with section("markup"):
    bullet_char = "-"
