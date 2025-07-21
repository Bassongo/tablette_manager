source("modules/ui_modules.R")

ui <- navbarPage(
  id = "navbar",
  title = tagList(icon("tablet-alt"), "Gestion des Tablettes"),
  theme = bs_theme(version = 5, bootswatch = "minty"),
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/jsqr@1.4.0/dist/jsQR.js"),
    tags$script(src = "qr_scanner.js"),
    tags$link(rel="stylesheet",type="text/css",href="style.css")
  ),
  tabPanel("Accueil", fluidRow(column(12, div(style="padding:40px; text-align:center;", uiOutput("welcome_message"))))),
  tabPanel("Enregistrement", register_ui()),
  tabPanel("Affectation", assignment_ui()),
  tabPanel("G\u00e9n\u00e9ration de fiches", fiche_ui()),
  tabPanel("Retour de tablette", return_ui()),
  tabPanel("D\u00e9claration d'incident", incident_ui())
)

