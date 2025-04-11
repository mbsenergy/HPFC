

# : ========================================================================================================================================================

suppressMessages(suppressWarnings(
    source(file.path('body', "00_settings.R"))
    ))

options(shiny.reactlog = TRUE)
options(warn = -1)
options(shiny.maxRequestSize = 200 * 1024^2)
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)
options(shiny.reactlog = TRUE)
options(shiny.trace = FALSE)


# App ===============================================================

shinyApp(
    ui = ui_app,
    server = server_app
)