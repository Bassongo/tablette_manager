# === UI PRINCIPAL COMPLET ===
# Protection : charger les modules UI si besoin
if (!exists("register_ui")) source("modules/ui_modules.R")
if (!exists("assignment_ui")) source("modules/ui_modules.R")
if (!exists("fiche_ui")) source("modules/ui_modules.R")
if (!exists("return_ui")) source("modules/ui_modules.R")
if (!exists("incident_ui")) source("modules/ui_modules.R")
if (!exists("administration_ui")) source("modules/ui_modules.R")

# Interface utilisateur principale avec tous les onglets
app_ui <- function() {
  navbarPage(
    id = "navbar",
    title = tagList(icon("tablet-alt"), "Gestion des Tablettes"),
    theme = bs_theme(version = 5, bootswatch = "minty"),
    header = tagList(
      useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
        tags$script(src = "https://cdn.jsdelivr.net/npm/jsqr@1.4.0/dist/jsQR.js"),
        tags$script(src = "qr_scanner.js"),
        app_css()
      )
    ),
    tabPanel(
      "Accueil",
      fluidRow(
        column(12,
               div(style = "padding:40px; text-align:center;", uiOutput("welcome_message"))
        )
      )
    ),
    tabPanel("Enregistrement", register_ui()),
    tabPanel("Affectation", assignment_ui()),
    tabPanel("Génération de fiches", fiche_ui()),
    tabPanel("Retour de tablette", return_ui()),
    tabPanel("Déclaration d'incident", incident_ui()),
    tabPanel("Administration", administration_ui())
  )
}
