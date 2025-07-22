# Application Shiny de gestion des tablettes - Main loader
# --- Gestion automatique des packages ---
required_packages <- c(
  "shiny", "DT", "readxl", "shinyjs", "bslib", "shinyWidgets", "officer", "ggplot2", "reshape2"
)
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# Charger les modules
source("modules/data_management.R")
source("modules/ui_modules.R")
source("modules/server_modules.R")
source("modules/fiche_generation.R")
source("ui.R")
source("server.R")

# Variable globale pour le rôle utilisateur (NULL au démarrage)
global_user_role <- shiny::reactiveVal(NULL)

# Lancement de l'application
shinyApp(ui = app_ui(global_user_role), server = server)