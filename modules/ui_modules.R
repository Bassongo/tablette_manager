# === MODULES UI COMPLETS ===
# S'assurer que shinyjs est chargé pour useShinyjs
if (!requireNamespace("shinyjs", quietly = TRUE)) install.packages("shinyjs")
library(shinyjs)
# S'assurer que shiny est chargé pour icon, modalDialog, useShinyjs, tags, etc.
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
library(shiny)
# S'assurer que bslib est chargé pour bs_theme
if (!requireNamespace("bslib", quietly = TRUE)) install.packages("bslib")
library(bslib)
# Helpers pour simuler des cards Bootstrap
card <- function(...) {
  div(class = "card", ...)
}
card_header <- function(..., class = NULL) {
  div(class = paste("card-header", class), ...)
}
card_body <- function(...) {
  div(class = "card-body", ...)
}
# S'assurer que shinyWidgets est chargé pour actionBttn
if (!requireNamespace("shinyWidgets", quietly = TRUE)) install.packages("shinyWidgets")
library(shinyWidgets)
# S'assurer que DT est chargé pour DTOutput, renderDT, datatable, etc.
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
library(DT)
# Tous les composants d'interface utilisateur

# === CSS ET STYLES ===
app_css <- function() {
  tags$style(HTML("
    /* Variables CSS personnalisées */
    :root {
      --primary-color: #007bff;
      --success-color: #28a745;
      --warning-color: #ffc107;
      --danger-color: #dc3545;
      --light-bg: #f8f9fa;
      --border-radius: 10px;
      --box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
    }
    
    /* Style général */
    body {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      min-height: 100vh;
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    }
    
    /* Navbar personnalisée */
    .navbar {
      background: linear-gradient(90deg, #667eea 0%, #764ba2 100%) !important;
      box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
      border: none;
    }
    
    .navbar-brand {
      color: white !important;
      font-weight: bold;
      font-size: 1.5rem;
    }
    
    .navbar-nav .nav-link {
      color: rgba(255, 255, 255, 0.9) !important;
      font-weight: 500;
      transition: all 0.3s ease;
    }
    
    .navbar-nav .nav-link:hover,
    .navbar-nav .nav-link.active {
      color: white !important;
      background-color: rgba(255, 255, 255, 0.1);
      border-radius: 5px;
    }
    
    /* Cards stylisées */
    .card {
      border: none;
      border-radius: var(--border-radius);
      box-shadow: var(--box-shadow);
      background: white;
      margin-bottom: 20px;
      transition: transform 0.3s ease, box-shadow 0.3s ease;
    }
    
    .card:hover {
      transform: translateY(-2px);
      box-shadow: 0 8px 15px rgba(0, 0, 0, 0.15);
    }
    
    .card-header {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      font-weight: bold;
      border-radius: var(--border-radius) var(--border-radius) 0 0 !important;
      border: none;
      padding: 15px 20px;
    }
    
    .card-body {
      padding: 25px;
    }
    
    /* Boutons personnalisés */
    .blue-btn {
      background: linear-gradient(135deg, var(--primary-color) 0%, #0056b3 100%) !important;
      border: none;
      border-radius: 25px;
      padding: 12px 30px;
      font-weight: 600;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      transition: all 0.3s ease;
      box-shadow: 0 4px 15px rgba(0, 123, 255, 0.3);
    }
    
    .blue-btn:hover {
      transform: translateY(-2px);
      box-shadow: 0 6px 20px rgba(0, 123, 255, 0.4);
    }
    
    /* Boutons d'action */
    .btn-success {
      background: linear-gradient(135deg, var(--success-color) 0%, #20c997 100%) !important;
      border: none;
      border-radius: 25px;
      box-shadow: 0 4px 15px rgba(40, 167, 69, 0.3);
    }
    
    .btn-primary {
      background: linear-gradient(135deg, var(--primary-color) 0%, #0056b3 100%) !important;
      border: none;
      border-radius: 25px;
      box-shadow: 0 4px 15px rgba(0, 123, 255, 0.3);
    }
    
    .btn-warning {
      background: linear-gradient(135deg, var(--warning-color) 0%, #e0a800 100%) !important;
      border: none;
      border-radius: 25px;
      box-shadow: 0 4px 15px rgba(255, 193, 7, 0.3);
    }
    
    /* Champs de saisie */
    .form-control {
      border-radius: 10px;
      border: 2px solid #e9ecef;
      padding: 12px 15px;
      transition: all 0.3s ease;
      background-color: #f8f9fa;
    }
    
    .form-control:focus {
      border-color: var(--primary-color);
      box-shadow: 0 0 0 0.2rem rgba(0, 123, 255, 0.25);
      background-color: white;
    }
    
    /* Titres stylisés */
    h4 {
      color: var(--primary-color);
      font-weight: 600;
      margin-bottom: 20px;
      text-align: center;
      font-size: 1.3rem;
    }
    
    h5 {
      color: var(--primary-color);
      font-weight: 600;
      margin-bottom: 15px;
      font-size: 1.1rem;
    }
    
    /* Tableaux */
    .dataTables_wrapper {
      background: white;
      border-radius: var(--border-radius);
      padding: 20px;
      box-shadow: var(--box-shadow);
    }
    
    /* Animations */
    @keyframes fadeIn {
      from { opacity: 0; transform: translateY(20px); }
      to { opacity: 1; transform: translateY(0); }
    }
    
    .card {
      animation: fadeIn 0.6s ease-out;
    }
    
    /* Responsive */
    @media (max-width: 768px) {
      .card-body {
        padding: 15px;
      }
      
      .btn {
        width: 100%;
        margin-bottom: 10px;
      }
    }
  "))
}

# === MODAL DE CONNEXION ===
login_ui <- function() {
  modalDialog(
    title = "Connexion",
    textInput("login_user", "Login"),
    passwordInput("login_pass", "Mot de passe"),
    footer = tagList(
      actionButton("login_btn", "Se connecter")
    ),
    easyClose = FALSE
  )
}

# === MODULE ENREGISTREMENT COMPLET ===
register_ui <- function() {
  tabsetPanel(
    tabPanel(
      "Scan QR",
      fluidRow(
        column(
          12,
          card(
            card_header("Enregistrement par Scan QR", class = "card-header"),
            card_body(
              fluidRow(
                column(6,
                       div(style = "text-align: center; margin-bottom: 20px;",
                           h4("📱 Scanner Tablette"),
                           actionBttn("scan_tablet_btn", "Scanner QR Tablette", 
                                      style = "fill", color = "success", size = "lg",
                                      icon = icon("qrcode")),
                           br(), br(),
                           textInput("reg_tab_num_qr", "Numéro tablette", placeholder = "Scanné automatiquement")
                       )
                ),
                column(6,
                       div(style = "text-align: center; margin-bottom: 20px;",
                           h4("🔌 Scanner Chargeur"),
                           actionBttn("scan_charger_btn", "Scanner QR Chargeur", 
                                      style = "fill", color = "primary", size = "lg",
                                      icon = icon("qrcode")),
                           br(), br(),
                           textInput("reg_charger_num_qr", "Numéro chargeur", placeholder = "Scanné automatiquement")
                       )
                )
              ),
              fluidRow(
                column(12,
                       div(style = "text-align: center; margin: 20px 0;",
                           h4("🔋 Powerbank"),
                           materialSwitch("reg_has_powerbank_qr", "Powerbank présent", status = "primary", width = "100%")
                       )
                )
              ),
              fluidRow(
                column(12,
                       div(style = "text-align: center;",
                           actionBttn("register_qr_btn", "Enregistrer", 
                                      style = "fill", color = "primary", size = "lg",
                                      class = "blue-btn")
                       )
                )
              )
            )
          )
        )
      ),
      fluidRow(
        column(12,
               card(
                 card_header("Tablettes enregistrées", class = "card-header"),
                 card_body(DTOutput("register_table"))
               )
        )
      )
    ),
    tabPanel(
      "Manuel",
      fluidRow(
        column(
          4,
          card(
            card_header("Enregistrement manuel", class = "card-header"),
            card_body(
              textInput("reg_tab_num", "Numéro de la tablette"),
              textInput("reg_charger_num", "Numéro de chargeur"),
              materialSwitch("reg_has_powerbank", "Powerbank présent", status = "primary"),
              div(style = "margin-top: 20px;",
                  actionBttn("register_btn", "Enregistrer", style = "fill", color = "primary", class = "blue-btn")
              )
            )
          )
        ),
        column(8, 
               card(
                 card_header("Tablettes enregistrées", class = "card-header"),
                 card_body(DTOutput("register_table"))
               )
        )
      )
    ),
    tabPanel(
      "En masse",
      fluidRow(
        column(
          4,
          card(
            card_header("Enregistrement en masse", class = "card-header"),
            card_body(
              fileInput(
                "tablets_register_file",
                "Liste des tablettes (Excel)",
                accept = c(".xlsx", ".xls")
              ),
              div(style = "margin-top: 20px;",
                  actionBttn("register_mass_btn", "Enregistrer en masse", style = "fill", color = "primary", class = "blue-btn")
              )
            )
          )
        ),
        column(8, 
               card(
                 card_header("Tablettes enregistrées", class = "card-header"),
                 card_body(DTOutput("register_table"))
               )
        )
      )
    )
  )
}

# === MODULE AFFECTATION COMPLET ===
assignment_ui <- function() {
  tabsetPanel(
    tabPanel(
      "Individuelle",
      fluidRow(
        column(
          4,
          card(
            card_header("Affectation individuelle", class = "card-header"),
            card_body(
              div(style = "margin-bottom: 15px;",
                  h5("Informations de la tablette", style = "color: var(--primary-color); font-weight: 600;"),
                  textInput("tab_num", "Numéro de la tablette"),
                  textInput("charger_num", "Numéro de chargeur"),
                  materialSwitch("has_powerbank", "Powerbank présent", status = "primary")
              ),
              div(style = "margin-bottom: 15px;",
                  h5("Informations de l'agent", style = "color: var(--primary-color); font-weight: 600;"),
                  textInput("agent_id", "ID de l'agent"),
                  textInput("agent_name", "Nom de l'agent"),
                  textInput("agent_group", "Groupe de l'agent"),
                  selectInput("agent_function", "Fonction", choices = c("Enquêteur", "Superviseur")),
                  textInput("agent_phone", "Numéro de téléphone"),
                  textInput("agent_class", "Classe")
              ),
              div(style = "margin-bottom: 15px;",
                  h5("Informations du superviseur", style = "color: var(--primary-color); font-weight: 600;"),
                  textInput("supervisor_name", "Nom du superviseur"),
                  textInput("supervisor_num", "Numéro du superviseur"),
                  dateInput("assign_date", "Date d'affectation")
              ),
              div(style = "margin-top: 20px;",
                  actionBttn("assign_btn", "Affecter", style = "fill", color = "primary", class = "blue-btn")
              )
            )
          )
        ),
        column(8, 
               card(
                 card_header("Affectations en cours", class = "card-header"),
                 card_body(DTOutput("assign_table"))
               )
        )
      )
    ),
    tabPanel(
      "En masse",
      fluidRow(
        column(
          4,
          card(
            card_header("Affectation en masse", class = "card-header"),
            card_body(
              div(style = "margin-bottom: 15px;",
                  h5("Fichiers requis", style = "color: var(--primary-color); font-weight: 600;"),
                  fileInput("agents_file", "Liste des agents (Excel)", accept = c(".xlsx", ".xls")),
                  fileInput("tablets_file", "Liste des tablettes (Excel)", accept = c(".xlsx", ".xls"))
              ),
              div(style = "margin-top: 20px;",
                  actionBttn("mass_assign_btn", "Affecter aléatoirement", style = "fill", color = "primary", class = "blue-btn")
              )
            )
          )
        ),
        column(8, 
               card(
                 card_header("Affectations en cours", class = "card-header"),
                 card_body(DTOutput("assign_table"))
               )
        )
      )
    )
  )
}

# === MODULE GÉNÉRATION DE FICHES COMPLET ===
fiche_ui <- function() {
  fluidRow(
    column(
      12,
      card(
        card_header("Génération de fiches d'affectation", class = "card-header"),
        card_body(
          fluidRow(
            column(6,
                   div(style = "margin-bottom: 20px;",
                       h5("Sélection des affectations", style = "color: var(--primary-color); font-weight: 600;"),
                       selectInput("fiche_assign_select", "Sélectionner une affectation", choices = NULL),
                       actionBttn("generate_fiche_btn", "Générer la fiche", style = "fill", color = "success", class = "blue-btn"),
                       actionBttn("reset_selection_btn", "Réinitialiser la sélection", style = "fill", color = "warning", class = "blue-btn")
                   )
            ),
            column(6,
                   div(style = "margin-bottom: 20px;",
                       h5("Génération en masse", style = "color: var(--primary-color); font-weight: 600;"),
                       actionBttn("generate_all_fiches_btn", "Générer toutes les fiches", style = "fill", color = "warning", class = "blue-btn")
                   )
            )
          ),
          fluidRow(
            column(12,
                   div(style = "margin-top: 20px;",
                       h5("Historique des fiches générées", style = "color: var(--primary-color); font-weight: 600;"),
                       DTOutput("fiches_history_table")
                   )
            )
          )
        )
      )
    )
  )
}

# === MODULE RETOUR COMPLET ===
return_ui <- function() {
  fluidRow(
    column(
      4,
      card(
        card_header("Retour de tablette", class = "card-header"),
        card_body(
          div(style = "margin-bottom: 15px;",
              h5("Vérification de l'identité", style = "color: var(--primary-color); font-weight: 600;"),
              textInput("return_agent_id", "ID de l'agent enquêteur")
          ),
          div(style = "margin-bottom: 15px;",
              h5("Tablette à retourner", style = "color: var(--primary-color); font-weight: 600;"),
              selectInput("return_tablet_select", "Sélectionner la tablette", choices = NULL)
          ),
          div(style = "margin-bottom: 15px;",
              h5("Équipements retournés", style = "color: var(--primary-color); font-weight: 600;"),
              textInput("return_charger_num", "Numéro de chargeur retourné"),
              div(style = "margin-top: 10px;",
                  materialSwitch("return_has_powerbank", "Powerbank retourné", status = "primary"),
                  div(style = "font-size: 0.8em; color: #666; margin-top: 5px;",
                      textOutput("powerbank_info")
                  )
              )
          ),
          div(style = "margin-bottom: 15px;",
              h5("Informations de retour", style = "color: var(--primary-color); font-weight: 600;"),
              textInput("return_reason", "Motif du retour"),
              selectInput("return_condition", "État de la tablette", 
                          choices = c("Bon état", "Légèrement endommagée", "Endommagée", "Hors service")),
              dateInput("return_date", "Date de retour", value = Sys.Date()),
              textAreaInput("return_notes", "Notes supplémentaires", rows = 3)
          ),
          div(style = "margin-top: 20px;",
              actionBttn("return_tablet_btn", "Enregistrer le retour", style = "fill", color = "warning", class = "blue-btn")
          )
        )
      )
    ),
    column(8,
           card(
             card_header("Historique des retours", class = "card-header"),
             card_body(DTOutput("returns_table"))
           )
    )
  )
}

# === MODULE INCIDENT COMPLET ===
incident_ui <- function() {
  fluidRow(
    column(
      4,
      card(
        card_header("Déclarer un incident", class = "card-header"),
        card_body(
          textInput("incident_agent_id", "ID de l'agent enquêteur"),
          selectInput("incident_tablet_select", "Sélectionner la tablette", choices = NULL),
          selectInput("incident_type", "Type d'incident", choices = c("Casse", "Vol", "Endommagé", "Autre")),
          selectInput("incident_state", "Nouvel état de la tablette", choices = c("En réparation", "Hors service")),
          materialSwitch("incident_charger_ok", "Chargeur utilisable", status = "primary", value = TRUE),
          materialSwitch("incident_powerbank_ok", "Powerbank utilisable", status = "primary", value = TRUE),
          dateInput("incident_date", "Date de l'incident", value = Sys.Date()),
          textAreaInput("incident_notes", "Notes", rows = 3),
          div(style = "margin-top: 20px;",
              actionBttn("declare_incident_btn", "Déclarer", style = "fill", color = "danger", class = "blue-btn")
          )
        )
      )
    ),
    column(8,
           card(
             card_header("Historique des incidents", class = "card-header"),
             card_body(DTOutput("incidents_table"))
           )
    )
  )
}

# === MODULE ADMINISTRATION COMPLET ===
administration_ui <- function() {
  tabsetPanel(
    tabPanel(
      "Tableau de bord",
      fluidRow(
        column(
          12,
          card(
            card_header("État général des tablettes", class = "card-header"),
            card_body(
              fluidRow(
                column(3,
                       div(style = "text-align: center; padding: 20px; background: linear-gradient(135deg, #28a745 0%, #20c997 100%); border-radius: 10px; color: white;",
                           h3(textOutput("available_tablets_count"), style = "margin: 0; font-size: 2.5rem;"),
                           p("Tablettes disponibles", style = "margin: 5px 0 0 0;")
                       )
                ),
                column(3,
                       div(style = "text-align: center; padding: 20px; background: linear-gradient(135deg, #007bff 0%, #0056b3 100%); border-radius: 10px; color: white;",
                           h3(textOutput("assigned_tablets_count"), style = "margin: 0; font-size: 2.5rem;"),
                           p("Tablettes affectées", style = "margin: 5px 0 0 0;")
                       )
                ),
                column(3,
                       div(style = "text-align: center; padding: 20px; background: linear-gradient(135deg, #ffc107 0%, #e0a800 100%); border-radius: 10px; color: white;",
                           h3(textOutput("returned_tablets_count"), style = "margin: 0; font-size: 2.5rem;"),
                           p("Tablettes en retour", style = "margin: 5px 0 0 0;")
                       )
                ),
                column(3,
                       div(style = "text-align: center; padding: 20px; background: linear-gradient(135deg, #dc3545 0%, #c82333 100%); border-radius: 10px; color: white;",
                           h3(textOutput("out_of_service_tablets_count"), style = "margin: 0; font-size: 2.5rem;"),
                           p("Tablettes hors service", style = "margin: 5px 0 0 0;")
                       )
                )
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          12,
          card(
            card_header("Tableau de suivi détaillé", class = "card-header"),
            card_body(
              fluidRow(
                column(12,
                       div(style = "margin-bottom: 15px;",
                           h5("Filtres", style = "color: var(--primary-color); font-weight: 600;"),
                           fluidRow(
                             column(3, selectInput("status_filter", "Statut", choices = c("Tous", "Disponible", "Affectée", "En retour", "Hors service"))),
                             column(3, selectInput("group_filter", "Groupe", choices = c("Tous"))),
                             column(3, selectInput("function_filter", "Fonction", choices = c("Tous", "Enquêteur", "Superviseur"))),
                             column(3, actionBttn("apply_filters_btn", "Appliquer les filtres", style = "fill", color = "primary", class = "blue-btn"))
                           )
                       )
                ),
                DTOutput("tracking_table")
              )
            )
          )
        )
      )
    ),
    tabPanel(
      "Gestion des Superviseurs",
      fluidRow(
        column(
          4,
          card(
            card_header("Import des Superviseurs", class = "card-header"),
            card_body(
              div(style = "margin-bottom: 15px;",
                  h5("Format requis", style = "color: var(--primary-color); font-weight: 600;"),
                  p("Le fichier Excel doit contenir les colonnes suivantes :"),
                  tags$ul(
                    tags$li("user_name : Nom complet du superviseur"),
                    tags$li("user_login : Identifiant de connexion"),
                    tags$li("user_password : Mot de passe")
                  )
              ),
              div(style = "margin-bottom: 20px; display: flex; gap: 8px; flex-wrap: wrap; align-items: flex-end;",
                  fileInput(
                    "supervisors_file",
                    "Fichier Excel des superviseurs",
                    accept = c(".xlsx", ".xls")
                  ),
                  actionButton("import_supervisors_btn", "Importer les superviseurs", 
                               class = "btn btn-success blue-btn", style = "margin-left: 8px; height: 38px;"),
                  actionBttn("clear_supervisors_btn", "Vider la base des superviseurs", 
                             style = "fill", color = "danger", class = "blue-btn", size = "sm"),
                  actionBttn("reset_all_btn", "Réinitialiser toute l'application", 
                             style = "fill", color = "danger", class = "blue-btn", size = "sm")
              )
            )
          )
        ),
        column(
          8,
          card(
            card_header("Base des Superviseurs", class = "card-header"),
            card_body(
              DTOutput("supervisors_table")
            )
          )
        )
      )
    ),
    tabPanel(
      "Suivi des superviseurs",
      conditionalPanel(
        condition = "output.isAdmin == true",
        fluidRow(
          column(12,
                 h2("Statistiques globales", style = "color: white;"),
                 fluidRow(
                   column(3, uiOutput("nb_superviseurs")),
                   column(3, uiOutput("nb_fiches_total")),
                   column(3, uiOutput("nb_fiches_transferees")),
                   column(3, uiOutput("nb_fiches_en_attente"))
                 )
          )
        ),
        fluidRow(
          column(12,
                 h2("Alertes", style = "color: white;"),
                 uiOutput("alertes_superviseurs")
          )
        ),
        fluidRow(
          column(12,
                 h2("Statistiques par superviseur", style = "color: white;"),
                 DT::dataTableOutput("stats_par_superviseur")
          )
        ),
        fluidRow(
          column(12,
                 h2("Visualisations", style = "color: white;"),
                 plotOutput("plot_fiches_par_superviseur"),
                 plotOutput("plot_evolution_fiches")
          )
        )
      )
    )
  )
}