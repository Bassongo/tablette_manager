# Application Shiny de gestion des tablettes
library(shiny)
library(DT)
library(readxl)
library(shinyjs)
library(bslib)
library(shinyWidgets)

# CSS personnalisé pour un design moderne et élégant
custom_css <- "
/* Variables CSS pour les couleurs */
:root {
  --primary-color: #4A90E2;
  --secondary-color: #7ED321;
  --accent-color: #F5A623;
  --danger-color: #D0021B;
  --success-color: #7ED321;
  --warning-color: #F5A623;
  --info-color: #4A90E2;
  --light-bg: #F8F9FA;
  --dark-text: #2C3E50;
  --border-radius: 12px;
  --box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  --transition: all 0.3s ease;
}

/* Style général de l'application */
body {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  color: var(--dark-text);
}

/* Navigation bar stylisée */
.navbar {
  background: linear-gradient(90deg, #4A90E2 0%, #357ABD 100%) !important;
  box-shadow: var(--box-shadow);
  border-radius: 0 0 var(--border-radius) var(--border-radius);
}

.navbar-brand {
  color: white !important;
  font-weight: bold;
  font-size: 1.5rem;
}

.navbar-nav .nav-link {
  color: rgba(255, 255, 255, 0.9) !important;
  font-weight: 500;
  transition: var(--transition);
  border-radius: var(--border-radius);
  margin: 0 5px;
}

.navbar-nav .nav-link:hover,
.navbar-nav .nav-link.active {
  color: white !important;
  background-color: rgba(255, 255, 255, 0.2);
  transform: translateY(-2px);
}

/* Cards stylisées */
.card {
  background: white;
  border: none;
  border-radius: var(--border-radius);
  box-shadow: var(--box-shadow);
  transition: var(--transition);
  margin-bottom: 20px;
  overflow: hidden;
}

.card:hover {
  transform: translateY(-5px);
  box-shadow: 0 8px 25px rgba(0, 0, 0, 0.15);
}

.card-header {
  background: linear-gradient(135deg, var(--primary-color) 0%, #357ABD 100%);
  color: white;
  font-weight: bold;
  border: none;
  padding: 15px 20px;
}

/* Boutons stylisés */
.blue-btn {
  background: linear-gradient(135deg, var(--primary-color) 0%, #357ABD 100%) !important;
  border: none !important;
  color: white !important;
  border-radius: var(--border-radius) !important;
  padding: 12px 24px !important;
  font-weight: 600 !important;
  font-size: 14px !important;
  text-transform: uppercase !important;
  letter-spacing: 0.5px !important;
  box-shadow: var(--box-shadow) !important;
  transition: var(--transition) !important;
  position: relative !important;
  overflow: hidden !important;
}

.blue-btn:hover {
  background: linear-gradient(135deg, #357ABD 0%, #2E6DA4 100%) !important;
  color: white !important;
  transform: translateY(-2px) !important;
  box-shadow: 0 6px 20px rgba(74, 144, 226, 0.4) !important;
}

.blue-btn:focus {
  background: linear-gradient(135deg, var(--primary-color) 0%, #357ABD 100%) !important;
  color: white !important;
  box-shadow: 0 0 0 3px rgba(74, 144, 226, 0.3) !important;
}

.blue-btn:active {
  transform: translateY(0) !important;
}

/* Inputs stylisés */
.form-control {
  border-radius: var(--border-radius);
  border: 2px solid #E9ECEF;
  padding: 12px 15px;
  font-size: 14px;
  transition: var(--transition);
  background-color: #FAFBFC;
}

.form-control:focus {
  border-color: var(--primary-color);
  box-shadow: 0 0 0 0.2rem rgba(74, 144, 226, 0.25);
  background-color: white;
}

/* Labels stylisés */
label {
  font-weight: 600;
  color: var(--dark-text);
  margin-bottom: 8px;
  font-size: 14px;
}

/* Material switches stylisés */
.material-switch {
  margin: 15px 0;
}

/* Value boxes stylisés */
.value-box {
  background: white;
  border-radius: var(--border-radius);
  box-shadow: var(--box-shadow);
  padding: 20px;
  text-align: center;
  transition: var(--transition);
}

.value-box:hover {
  transform: translateY(-3px);
  box-shadow: 0 8px 25px rgba(0, 0, 0, 0.15);
}

/* DataTables stylisés */
.dataTables_wrapper {
  background: white;
  border-radius: var(--border-radius);
  box-shadow: var(--box-shadow);
  padding: 20px;
  margin-top: 20px;
}

.dataTables_wrapper .dataTables_length,
.dataTables_wrapper .dataTables_filter {
  margin-bottom: 15px;
}

.dataTables_wrapper .dataTables_length select,
.dataTables_wrapper .dataTables_filter input {
  border-radius: var(--border-radius);
  border: 2px solid #E9ECEF;
  padding: 8px 12px;
}

/* Tabs stylisés */
.nav-tabs {
  border-bottom: 2px solid #E9ECEF;
  margin-bottom: 20px;
}

.nav-tabs .nav-link {
  border: none;
  border-radius: var(--border-radius) var(--border-radius) 0 0;
  color: var(--dark-text);
  font-weight: 500;
  padding: 12px 20px;
  transition: var(--transition);
}

.nav-tabs .nav-link:hover {
  background-color: #F8F9FA;
  border-color: transparent;
}

.nav-tabs .nav-link.active {
  background: linear-gradient(135deg, var(--primary-color) 0%, #357ABD 100%);
  color: white;
  border-color: transparent;
}

/* File inputs stylisés */
.file-input {
  background: #F8F9FA;
  border: 2px dashed #DEE2E6;
  border-radius: var(--border-radius);
  padding: 20px;
  text-align: center;
  transition: var(--transition);
}

.file-input:hover {
  border-color: var(--primary-color);
  background-color: rgba(74, 144, 226, 0.05);
}

/* Notifications stylisées */
.shiny-notification {
  border-radius: var(--border-radius);
  box-shadow: var(--box-shadow);
  font-weight: 500;
}

/* Modal stylisé */
.modal-content {
  border-radius: var(--border-radius);
  border: none;
  box-shadow: 0 10px 40px rgba(0, 0, 0, 0.2);
}

.modal-header {
  background: linear-gradient(135deg, var(--primary-color) 0%, #357ABD 100%);
  color: white;
  border-radius: var(--border-radius) var(--border-radius) 0 0;
  border: none;
}

.modal-footer {
  border-top: 1px solid #E9ECEF;
  padding: 15px 20px;
}

/* Responsive design */
@media (max-width: 768px) {
  .card {
    margin-bottom: 15px;
  }
  
  .blue-btn {
    width: 100%;
    margin-bottom: 10px;
  }
  
  .value-box {
    margin-bottom: 15px;
  }
}

/* Animations */
@keyframes fadeIn {
  from { opacity: 0; transform: translateY(20px); }
  to { opacity: 1; transform: translateY(0); }
}

.card, .value-box {
  animation: fadeIn 0.6s ease-out;
}

/* Scrollbar personnalisée */
::-webkit-scrollbar {
  width: 8px;
}

::-webkit-scrollbar-track {
  background: #F1F1F1;
  border-radius: 4px;
}

::-webkit-scrollbar-thumb {
  background: var(--primary-color);
  border-radius: 4px;
}

::-webkit-scrollbar-thumb:hover {
  background: #357ABD;
}
"

# Interface utilisateur principale
ui <- navbarPage(
  title = tagList(icon("tablet-alt"), "Gestion des Tablettes"),
  theme = bs_theme(version = 5, bootswatch = "minty"),
  useShinyjs(),
  tags$head(
    tags$style(HTML(custom_css)),
    tags$script(HTML("
      function downloadFile(filename) {
        var link = document.createElement('a');
        link.href = 'download/' + filename;
        link.download = filename;
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
      }
    "))
  ),
  tabPanel(
    "Enregistrement",
    tabsetPanel(
      tabPanel(
        "Individuel",
        fluidRow(
          column(
            4,
            card(
              card_header("Enregistrement individuel", class = "card-header"),
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
  ),
  tabPanel(
    "Affectation",
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
              card_header("Affectations en masse", class = "card-header"),
              card_body(DTOutput("mass_assign_table"))
            )
          )
        )
      )
    )
  ),
  tabPanel(
    "Retour de tablette",
    fluidRow(
      column(
        4,
        card(
          card_header("Retour de tablette", class = "card-header"),
          card_body(
          textInput("return_tab_num", "Numéro de la tablette"),
          textInput("return_agent", "Nom de l'agent"),
          textInput("return_charger", "Numéro de chargeur"),
          materialSwitch("return_powerbank", "Powerbank présent", status = "primary"),
            div(style = "margin-top: 20px;",
                actionBttn("return_btn", "Confirmer le retour", style = "fill", color = "primary", class = "blue-btn")
            )
          )
        )
      ),
      column(8, 
        card(
          card_header("Retours enregistrés", class = "card-header"),
          card_body(DTOutput("return_table"))
        )
      )
    )
  ),
  tabPanel(
    "Déclaration d'incident",
    fluidRow(
      column(
        4,
        card(
          card_header("Déclaration d'incident", class = "card-header"),
          card_body(
            div(style = "margin-bottom: 15px;",
                h5("Informations de l'incident", style = "color: var(--primary-color); font-weight: 600;"),
          textInput("incident_tab", "Numéro de la tablette"),
          selectInput("incident_type", "Type d'incident", choices = c("écran cassé", "perte", "autre")),
          textAreaInput("incident_comment", "Commentaire"),
                textInput("incident_agent", "Nom de l'agent")
            ),
            div(style = "margin-bottom: 15px;",
                h5("État des accessoires", style = "color: var(--primary-color); font-weight: 600;"),
          materialSwitch("incident_charger_ok", "Chargeur fonctionnel", value = TRUE, status = "primary"),
                materialSwitch("incident_powerbank_ok", "Powerbank fonctionnel", value = TRUE, status = "primary")
            ),
            div(style = "margin-top: 20px;",
                actionBttn("incident_btn", "Déclarer", style = "fill", color = "primary", class = "blue-btn")
            )
          )
        )
      ),
      column(8, 
        card(
          card_header("Incidents déclarés", class = "card-header"),
          card_body(DTOutput("incident_table"))
        )
      )
    )
  ),
  tabPanel(
    "Suivi des tablettes",
    fluidRow(
      column(4, 
        value_box(
          title = "Stock disponible", 
          value = textOutput("stock_txt"), 
          showcase = icon("warehouse", class = "fa-2x"),
          theme = "primary"
        )
      ),
      column(4,
        value_box(
          title = "Tablettes affectées", 
          value = textOutput("assigned_txt"), 
          showcase = icon("users", class = "fa-2x"),
          theme = "success"
        )
      ),
      column(4,
        value_box(
          title = "Incidents déclarés", 
          value = textOutput("incident_txt"), 
          showcase = icon("exclamation-triangle", class = "fa-2x"),
          theme = "warning"
        )
      )
    ),
    card(
      card_header("Inventaire complet des tablettes", class = "card-header"),
      card_body(DTOutput("dashboard_table"))
    )
  ),
  tabPanel(
    "Fiches d'affectation",
    fluidRow(
      column(12,
        card(
          card_header("Gestion des fiches d'affectation", class = "card-header"),
          card_body(
            div(style = "margin-bottom: 20px;",
                h5("Générer une fiche d'affectation", style = "color: var(--primary-color); font-weight: 600;"),
                selectInput("fiche_agent", "Sélectionner l'agent", choices = NULL),
                div(style = "margin-top: 15px;",
                    actionBttn("generate_fiche_btn", "Générer la fiche", style = "fill", color = "primary", class = "blue-btn")
                )
            ),
            div(style = "margin-top: 20px;",
                h5("Fiches générées", style = "color: var(--primary-color); font-weight: 600;"),
                DTOutput("fiches_table")
            )
          )
        )
      )
    )
  )
)


# Fonction pour générer une fiche d'affectation
generate_affectation_fiche <- function(assign_data) {
  library(officer)

  template_path <- "Fiche_Affectation_Materiel.docx"
  doc <- read_docx(template_path)

  replacements <- list(
    "{{groupe}}" = assign_data$groupe,
    "{{agent}}" = assign_data$agent,
    "{{fonction}}" = assign_data$fonction,
    "{{telephone}}" = assign_data$telephone,
    "{{tablette}}" = assign_data$tablette,
    "{{chargeur}}" = assign_data$chargeur,
    "{{superviseur}}" = assign_data$superviseur,
    "{{numero_superviseur}}" = assign_data$numero_superviseur,
    "{{date}}" = assign_data$date
  )

  for (v in names(replacements)) {
    doc <- body_replace_all_text(doc, v, replacements[[v]], ignore.case = FALSE)
  }

  filename <- paste0(
    "Fiche_",
    assign_data$agent,
    "_",
    assign_data$tablette,
    "_",
    Sys.Date(),
    ".docx"
  )
  print(doc, target = filename)
  return(filename)
}

server <- function(input, output, session) {
  # Liste des tablettes enregistr\u00e9es
  registered <- reactiveVal(
    data.frame(
      tablette = character(),
      chargeur = character(),
      powerbank = character(),
      etat = character(),
      stringsAsFactors = FALSE
    )
  )
  # Table des affectations individuelles
  assignments <- reactiveVal(
    data.frame(
      tablette = character(),
      chargeur = character(),
      powerbank = character(),
      groupe = character(),
      agent = character(),
      fonction = character(),
      telephone = character(),
      classe = character(),
      id_agent = character(),
      superviseur = character(),
      numero_superviseur = character(),
      date = character(),
      stringsAsFactors = FALSE
    )
  )
  # Table des affectations en masse
  mass_assignments <- reactiveVal(data.frame())
  # Table des retours de tablettes
  returns <- reactiveVal(
    data.frame(
      tablette = character(),
      chargeur = character(),
      powerbank = character(),
      agent = character(),
      date_retour = character(),
      stringsAsFactors = FALSE
    )
  )
  # Table des incidents d\u00e9clar\u00e9s
  incidents <- reactiveVal(
    data.frame(
      tablette = character(),
      type = character(),
      commentaire = character(),
      agent = character(),
      chargeur_ok = character(),
      powerbank_ok = character(),
      date = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # Table des fiches d'affectation générées
  fiches <- reactiveVal(
    data.frame(
      agent = character(),
      tablette = character(),
      date_generation = character(),
      fichier = character(),
      stringsAsFactors = FALSE
    )
  )

  # Valeurs temporaires pour la gestion des retours avec accessoires manquants
  vals <- reactiveValues(pending_return = NULL)

  finalize_return <- function(tab_num, charger_num, has_powerbank, agent) {
    new_entry <- data.frame(
      tablette = tab_num,
      chargeur = charger_num,
      powerbank = ifelse(has_powerbank, "Oui", "Non"),
      agent = agent,
      date_retour = Sys.Date(),
      stringsAsFactors = FALSE
    )
    returns(rbind(returns(), new_entry))
    current <- registered()
    row <- which(current$tablette == tab_num)
    current$etat[row] <- "en stock"
    if (length(row) == 1) {
      if (current$chargeur[row] != "endommag\u00e9") {
        current$chargeur[row] <- charger_num
      }
      current$powerbank[row] <- ifelse(has_powerbank, "Oui", "Non")
    }
    registered(current)
    updateTextInput(session, "return_tab_num", value = "")
    updateTextInput(session, "return_agent", value = "")
    updateTextInput(session, "return_charger", value = "")
    updateCheckboxInput(session, "return_powerbank", value = FALSE)
    vals$pending_return <- NULL
  }

  # Enregistrement manuel d'une tablette
  observeEvent(input$register_btn, {
    new_entry <- data.frame(
      tablette = input$reg_tab_num,
      chargeur = input$reg_charger_num,
      powerbank = ifelse(input$reg_has_powerbank, "Oui", "Non"),
      etat = "en stock",
      stringsAsFactors = FALSE
    )
    registered(rbind(registered(), new_entry))
    showNotification("Tablette enregistr\u00e9e avec succ\u00e8s", type = "message")
    updateTextInput(session, "reg_tab_num", value = "")
    updateTextInput(session, "reg_charger_num", value = "")
    updateCheckboxInput(session, "reg_has_powerbank", value = FALSE)
  })

  # Enregistrement en masse des tablettes via fichier Excel
  observeEvent(input$register_mass_btn, {
    req(input$tablets_register_file)
    tablets <- read_excel(input$tablets_register_file$datapath, col_types = "text")
    if (!"etat" %in% names(tablets)) {
      tablets$etat <- "en stock"
    }
    if ("powerbank" %in% names(tablets)) {
      tablets$powerbank <- ifelse(tablets$powerbank %in% c("TRUE", "1", "Oui", "yes", "YES", "oui"), "Oui", "Non")
    }
    registered(rbind(registered(), tablets))
    shinyjs::reset("tablets_register_file")
  })

  output$register_table <- renderDT(registered())

  observeEvent(input$assign_btn, {
    # V\u00e9rifie que la tablette est enregistr\u00e9e avant l'affectation
    if (!(input$tab_num %in% registered()$tablette)) {
      showNotification("Tablette non enregistr\u00e9e", type = "error")
      return()
    }

    current <- registered()
    tab_row <- which(current$tablette == input$tab_num)
    if (current$etat[tab_row] != "en stock") {
      showNotification("Tablette non disponible", type = "error")
      return()
    }

    if (current$chargeur[tab_row] != input$charger_num) {
      showNotification("Num\u00e9ro de chargeur incorrect", type = "error")
      return()
    }

    expected_pb <- current$powerbank[tab_row] == "Oui"
    if (expected_pb != input$has_powerbank) {
      showNotification("Powerbank non conforme", type = "error")
      return()
    }

    new_entry <- data.frame(
      tablette = input$tab_num,
      chargeur = input$charger_num,
      powerbank = ifelse(input$has_powerbank, "Oui", "Non"),
      groupe = input$agent_group,
      agent = trimws(input$agent_name),
      fonction = input$agent_function,
      telephone = input$agent_phone,
      classe = input$agent_class,
      id_agent = input$agent_id,
      superviseur = input$supervisor_name,
      numero_superviseur = input$supervisor_num,
      date = as.character(input$assign_date),
      stringsAsFactors = FALSE
    )
    assignments(rbind(assignments(), new_entry))
    current$etat[current$tablette == input$tab_num] <- "affect\u00e9"
    registered(current)
    showNotification("Tablette affect\u00e9e avec succ\u00e8s", type = "message")
    updateTextInput(session, "tab_num", value = "")
    updateTextInput(session, "charger_num", value = "")
    updateCheckboxInput(session, "has_powerbank", value = FALSE)
    updateTextInput(session, "agent_group", value = "")
    updateTextInput(session, "agent_id", value = "")
    updateTextInput(session, "agent_name", value = "")
    updateSelectInput(session, "agent_function", selected = "Enquêteur")
    updateTextInput(session, "agent_phone", value = "")
    updateTextInput(session, "agent_class", value = "")
    updateTextInput(session, "supervisor_name", value = "")
    updateTextInput(session, "supervisor_num", value = "")
    updateDateInput(session, "assign_date", value = Sys.Date())
  })

  output$assign_table <- renderDT(assignments())

  observeEvent(input$mass_assign_btn, {
    req(input$agents_file, input$tablets_file)
    agents <- read_excel(input$agents_file$datapath, col_types = "text")
    tablets <- read_excel(input$tablets_file$datapath, col_types = "text")

    # V\u00e9rifie que toutes les tablettes du fichier sont enregistr\u00e9es
    not_registered <- setdiff(tablets$tablette, registered()$tablette)
    if (length(not_registered) > 0) {
      showNotification(
        "Certaines tablettes ne sont pas enregistr\u00e9es",
        type = "error"
      )
      return()
    }

    n <- min(nrow(agents), nrow(tablets))
    shuffled <- sample(n)
    result <- cbind(agents[seq_len(n), ], tablets[shuffled, ])
    if ("powerbank" %in% names(result)) {
      result$powerbank <- ifelse(result$powerbank, "Oui", "Non")
    }
    mass_assignments(result)
    if (!"date" %in% names(result)) {
      result$date <- as.character(Sys.Date())
    }
    # Ajouter les colonnes manquantes avec NA
    for (col in setdiff(names(assignments()), names(result))) {
      result[[col]] <- NA
    }
    # Réordonner les colonnes
    result <- result[, names(assignments()), drop = FALSE]
    assignments(rbind(assignments(), result))
    current <- registered()
    current$etat[current$tablette %in% result$tablette] <- "affect\u00e9"
    registered(current)
    shinyjs::reset("agents_file")
    shinyjs::reset("tablets_file")
  })

  output$mass_assign_table <- renderDT(mass_assignments())

  observeEvent(input$return_btn, {
    if (!(input$return_tab_num %in% registered()$tablette)) {
      showNotification("Tablette non enregistr\u00e9e", type = "error")
      return()
    }

    current <- registered()
    tab_row <- which(current$tablette == input$return_tab_num)
    if (length(tab_row) == 0 || current$etat[tab_row] != "affect\u00e9") {
      showNotification("Tablette non affect\u00e9e", type = "error")
      return()
    }

    assign_row <- assignments()[assignments()$tablette == input$return_tab_num, ]
    if (nrow(assign_row) == 0) {
      showNotification("Tablette non affect\u00e9e", type = "error")
      return()
    }

    if (trimws(assign_row$agent) != trimws(input$return_agent)) {
      showNotification(paste(input$return_agent, "n'est pas le responsable de cette tablette, il ne peut donc pas la retourner."), type = "error")
      return()
    }

    pb_expected <- assign_row$powerbank == "Oui"
    charger_expected <- assign_row$chargeur
    pb_missing <- pb_expected && !input$return_powerbank
    charger_mismatch <- charger_expected != input$return_charger

    if (pb_missing || charger_mismatch) {
      vals$pending_return <- list(
        tab_num = input$return_tab_num,
        agent = trimws(input$return_agent),
        return_charger = input$return_charger,
        return_powerbank = input$return_powerbank,
        expected_charger = charger_expected,
        expected_powerbank = pb_expected
      )
      showModal(modalDialog(
        title = "Accessoires manquants",
        if (charger_mismatch) tagList(
          p("Ce chargeur n'est pas initialement affect\u00e9 avec cette tablette"),
          checkboxInput("lost_charger", "Avez-vous endommag\u00e9 ou perdu le chargeur initial ?", FALSE)
        ),
        if (pb_missing) checkboxInput("lost_powerbank", "Avez-vous endommag\u00e9 ou perdu la powerbank ?", FALSE),
        footer = tagList(modalButton("Annuler"), actionButton("confirm_missing", "Confirmer", class = "btn-primary blue-btn"))
      ))
      return()
    }

    finalize_return(input$return_tab_num, input$return_charger, input$return_powerbank, trimws(input$return_agent))
  })

  observeEvent(input$confirm_missing, {
    req(vals$pending_return)
    removeModal()
    params <- vals$pending_return

    pb_missing <- params$expected_powerbank && !params$return_powerbank
    charger_mismatch <- params$expected_charger != params$return_charger

    if ((charger_mismatch && !isTruthy(input$lost_charger)) || (pb_missing && !isTruthy(input$lost_powerbank))) {
      showNotification("Retour annul\u00e9: accessoires manquants", type = "error")
      vals$pending_return <- NULL
      return()
    }

    current <- registered()
    tab_row <- which(current$tablette == params$tab_num)
    if (charger_mismatch && isTruthy(input$lost_charger)) {
      current$chargeur[tab_row] <- "endommag\u00e9"
    }
    if (pb_missing && isTruthy(input$lost_powerbank)) {
      current$powerbank[tab_row] <- "Non"
    }
    registered(current)

    finalize_return(params$tab_num, params$return_charger, params$return_powerbank, params$agent)
  })

  output$return_table <- renderDT(returns())

  observeEvent(input$incident_btn, {
    if (!(input$incident_tab %in% registered()$tablette)) {
      showNotification("Tablette non enregistr\u00e9e", type = "error")
      return()
    }

    assign_row <- assignments()[assignments()$tablette == input$incident_tab, ]
    if (nrow(assign_row) == 0 || trimws(assign_row$agent) != trimws(input$incident_agent)) {
      showNotification("Cet agent n'est pas responsable de cette tablette", type = "error")
      return()
    }

    new_entry <- data.frame(
      tablette = input$incident_tab,
      type = input$incident_type,
      commentaire = input$incident_comment,
      agent = trimws(input$incident_agent),
      chargeur_ok = ifelse(input$incident_charger_ok, "Oui", "Non"),
      powerbank_ok = ifelse(input$incident_powerbank_ok, "Oui", "Non"),
      date = Sys.Date(),
      stringsAsFactors = FALSE
    )
    incidents(rbind(incidents(), new_entry))
    current <- registered()
    row <- which(current$tablette == input$incident_tab)
    current$etat[row] <- "endommag\u00e9"
    if (!input$incident_charger_ok) {
      current$chargeur[row] <- "endommag\u00e9"
    }
    if (!input$incident_powerbank_ok) {
      current$powerbank[row] <- "Non"
    }
    registered(current)

    updateTextInput(session, "incident_tab", value = "")
    updateTextInput(session, "incident_comment", value = "")
    updateTextInput(session, "incident_agent", value = "")
    updateSelectInput(session, "incident_type", selected = "\u00e9cran cass\u00e9")
    updateCheckboxInput(session, "incident_charger_ok", value = TRUE)
    updateCheckboxInput(session, "incident_powerbank_ok", value = TRUE)
  })

  output$incident_table <- renderDT(incidents())

  # Mise à jour des choix d'agents pour les fiches
  observe({
    if (nrow(assignments()) > 0) {
      agent_choices <- unique(paste(assignments()$agent, "-", assignments()$tablette))
      updateSelectInput(session, "fiche_agent", choices = c("", agent_choices))
    }
  })

  # Génération de fiche d'affectation
  observeEvent(input$generate_fiche_btn, {
    req(input$fiche_agent)
    if (input$fiche_agent == "") {
      showNotification("Veuillez sélectionner un agent", type = "error")
      return()
    }
    
    # Extraire l'agent et la tablette du choix
    agent_info <- strsplit(input$fiche_agent, " - ")[[1]]
    agent_name <- agent_info[1]
    tablette_num <- agent_info[2]
    
    # Trouver les informations de l'affectation
    assign_data <- assignments()[assignments()$agent == agent_name & assignments()$tablette == tablette_num, ]
    
    if (nrow(assign_data) == 0) {
      showNotification("Informations d'affectation non trouvées", type = "error")
      return()
    }
    
    # Générer la fiche
    filename <- generate_affectation_fiche(assign_data)
    
    # Ajouter à la table des fiches
    new_fiche <- data.frame(
      agent = agent_name,
      tablette = tablette_num,
      date_generation = as.character(Sys.Date()),
      fichier = filename,
      stringsAsFactors = FALSE
    )
    fiches(rbind(fiches(), new_fiche))
    
    showNotification(paste("Fiche d'affectation générée avec succès :", filename), type = "message")
  })

  output$fiches_table <- renderDT({
    if (nrow(fiches()) > 0) {
      # Ajouter une colonne avec bouton de téléchargement
      fiches_data <- fiches()
      fiches_data$actions <- paste0(
        '<button class="btn btn-sm btn-primary" onclick="downloadFile(\'', 
        fiches_data$fichier, '\')">Télécharger</button>'
      )
      datatable(fiches_data, escape = FALSE, options = list(pageLength = 10))
    } else {
      datatable(data.frame(Message = "Aucune fiche générée"), options = list(pageLength = 10))
    }
  })

  # Endpoint de téléchargement pour les fiches
  output$download_fiche <- downloadHandler(
    filename = function() {
      req(input$fiche_to_download)
      input$fiche_to_download
    },
    content = function(file) {
      req(input$fiche_to_download)
      file.copy(input$fiche_to_download, file)
    }
  )

  output$stock_txt <- renderText({
    stock <- sum(registered()$etat == "en stock")
    paste("Stock disponible:", stock)
  })

  output$assigned_txt <- renderText({
    paste("Tablettes affect\u00e9es:", sum(registered()$etat == "affect\u00e9"))
  })

  output$incident_txt <- renderText({
    paste("Incidents d\u00e9clar\u00e9s:", sum(registered()$etat == "endommag\u00e9"))
  })

  output$dashboard_table <- renderDT(registered())
}

shinyApp(ui, server)
