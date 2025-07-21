# Main application loader
source("modules/data_management.R")
source("modules/ui_modules.R")
source("modules/server_modules.R")
source("modules/fiche_generation.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
