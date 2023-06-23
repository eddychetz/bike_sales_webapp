library(bslib)
library(shiny)


# SPECIALIZED SPECS
TITLE <- "Sales Dashboard"

FONT_HEADING <- "Consolas"
FONT_BASE <- "Helvetica Neue"
PRIMARY <- "#2e3E50"
SUCCESS <- "#6ab5da"
INFO <- "#00F5FB"
WARNING <- "#ee9c0f"
DANGER <- "#f06698"
FG <- "#000000"
BG <- "#ffffff"

app_theme <- bs_theme(
    font_scale = 1.0,
    primary = PRIMARY,
    success = SUCCESS,
    info = INFO,
    warning = WARNING,
    danger = DANGER,
    fg = FG,
    bg = BG,
    "navbar-bg" = PRIMARY,
    "body-color" = PRIMARY,
    "accordio-button-active-bg" = SUCCESS,
    "accordion-button-active-color" = PRIMARY,
    "bs-accordion-color" = PRIMARY,
    "light" = BG
)