# Application Shiny de gestion des tablettes
library(shiny)
library(DT)
library(readxl)
library(shinyjs)
library(bslib)
library(shinyWidgets)

# Fonction pour analyser les placeholders dans le template
analyze_document_variables <- function(template_path) {
  library(officer)
  doc <- read_docx(template_path)
  doc_text <- docx_summary(doc)$text
  full_text <- paste(doc_text, collapse = " ")
  placeholder_pattern <- "\\{\\{[^}]+\\}\\}"
  placeholders <- regmatches(full_text, gregexpr(placeholder_pattern, full_text))[[1]]
  unique(placeholders)
}

# Fonction pour générer une fiche d'affectation
generate_affectation_fiche <- function(assign_data) {
  library(officer)
  
  # Lire le template
  doc <- read_docx("Fiche_Affectation_Materiel.docx")
  
  # Mapping direct des placeholders
  replacements <- list(
    "{{groupe}}" = as.character(ifelse(is.na(assign_data$agent_group) || assign_data$agent_group == "", "N/A", assign_data$agent_group)),
    "{{agent}}" = as.character(ifelse(is.na(assign_data$agent_name) || assign_data$agent_name == "", "N/A", assign_data$agent_name)),
    "{{fonction}}" = as.character(ifelse(is.na(assign_data$agent_function) || assign_data$agent_function == "", "N/A", assign_data$agent_function)),
    "{{Téléphone}}" = as.character(ifelse(is.na(assign_data$agent_phone) || assign_data$agent_phone == "", "N/A", assign_data$agent_phone)),
    "{{tablette}}" = as.character(ifelse(is.na(assign_data$tablette) || assign_data$tablette == "", "N/A", assign_data$tablette)),
    "{{chargeur}}" = as.character(ifelse(is.na(assign_data$chargeur) || assign_data$chargeur == "", "N/A", assign_data$chargeur)),
    "{{batterie}}" = as.character(ifelse(is.na(assign_data$powerbank) || assign_data$powerbank == "", "N/A", ifelse(as.logical(assign_data$powerbank), "Oui", "Non"))),
    "{{superviseur}}" = as.character(ifelse(is.na(assign_data$supervisor_name) || assign_data$supervisor_name == "", "N/A", assign_data$supervisor_name)),
    "{{adresse}}" = as.character(ifelse(is.na(assign_data$supervisor_num) || assign_data$supervisor_num == "", "N/A", assign_data$supervisor_num)),
    "{{date}}" = as.character(ifelse(is.na(assign_data$assign_date) || assign_data$assign_date == "", "N/A", assign_data$assign_date))
  )
  
  # Appliquer les remplacements
  for (placeholder in names(replacements)) {
    doc <- doc %>% 
      body_replace_all_text(placeholder, replacements[[placeholder]], fixed = TRUE)
  }
  
  # Générer le nom de fichier
  safe_agent_name <- gsub("[^a-zA-Z0-9]", "_", assign_data$agent_name)
  safe_tablet_name <- gsub("[^a-zA-Z0-9]", "_", assign_data$tablette)
  filename <- paste0("Fiche_", safe_agent_name, "_", safe_tablet_name, "_", Sys.Date(), ".docx")
  
  print(doc, target = filename)
  return(filename)
}

# Interface utilisateur principale
ui <- navbarPage(
  title = tagList(icon("tablet-alt"), "Gestion des Tablettes"),
  theme = bs_theme(version = 5, bootswatch = "minty"),
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
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
        background: linear-gradient(135deg, var(--success-color) 0%, #1e7e34 100%) !important;
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
      
      /* Switch personnalisé */
      .material-switch .switch {
        background-color: #ccc;
        border-radius: 20px;
      }
      
      .material-switch .switch.active {
        background-color: var(--primary-color);
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
  ),
  tabPanel(
    "Enregistrement",
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
              card_header("Affectations en cours", class = "card-header"),
              card_body(DTOutput("assign_table"))
            )
          )
        )
      )
    )
  ),
  tabPanel(
    "Génération de fiches",
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
                    actionBttn("generate_fiche_btn", "Générer la fiche", style = "fill", color = "success", class = "blue-btn")
                )
              ),
              column(6,
                div(style = "margin-bottom: 20px;",
                    h5("Génération en masse", style = "color: var(--primary-color); font-weight: 600;"),
                    actionBttn("generate_all_fiches_btn", "Générer toutes les fiches", style = "fill", color = "warning", class = "blue-btn")
                )
              )
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
  ),
  tabPanel(
    "Suivi des tablettes",
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
    )
  )

# Serveur
server <- function(input, output, session) {
  
  # Données réactives
  registered_tablets <- reactiveVal(data.frame(
    tablette = character(),
    chargeur = character(),
    powerbank = logical(),
    registration_date = character(),
    etat = character(),
    stringsAsFactors = FALSE
  ))
  
  assignments <- reactiveVal(data.frame(
    tablette = character(),
    chargeur = character(),
    powerbank = logical(),
    agent_id = character(),
    agent_name = character(),
    agent_group = character(),
    agent_function = character(),
    agent_phone = character(),
    agent_class = character(),
    supervisor_name = character(),
    supervisor_num = character(),
    assign_date = character(),
    stringsAsFactors = FALSE
  ))
  
  # Nouvelles données réactives pour retour et suivi
  tablet_returns <- reactiveVal(data.frame(
    tablette = character(),
    agent_name = character(),
    return_reason = character(),
    return_condition = character(),
    return_date = character(),
    return_notes = character(),
    stringsAsFactors = FALSE
  ))
  
  tablet_status <- reactiveVal(data.frame(
    tablette = character(),
    status = character(),
    current_agent = character(),
    assign_date = character(),
    return_date = character(),
    condition = character(),
    stringsAsFactors = FALSE
  ))
  
  # Observateurs pour les boutons de scan QR
  observeEvent(input$scan_tablet_btn, {
    print("=== BOUTON SCAN TABLETTE CLIQUÉ ===")
    print("Déclenchement du scan QR côté client...")
    runjs("alert('Test scan tablette - Fonctionnalité en développement');")
  })
  
  observeEvent(input$scan_charger_btn, {
    print("=== BOUTON SCAN CHARGEUR CLIQUÉ ===")
    print("Déclenchement du scan QR côté client...")
    runjs("alert('Test scan chargeur - Fonctionnalité en développement');")
  })
  
  # Enregistrement manuel
  observeEvent(input$register_btn, {
    req(input$reg_tab_num, input$reg_charger_num)
    
    new_tablet <- data.frame(
      tablette = input$reg_tab_num,
      chargeur = input$reg_charger_num,
      powerbank = input$reg_has_powerbank,
      registration_date = as.character(Sys.Date()),
      etat = "En stock",
      stringsAsFactors = FALSE
    )
    
    current_tablets <- registered_tablets()
    updated_tablets <- rbind(current_tablets, new_tablet)
    registered_tablets(updated_tablets)
    
    # Réinitialiser les champs
    updateTextInput(session, "reg_tab_num", value = "")
    updateTextInput(session, "reg_charger_num", value = "")
    updateMaterialSwitch(session, "reg_has_powerbank", value = FALSE)
    
    showNotification("Tablette enregistrée avec succès!", type = "default")
  })
  
  # Enregistrement en masse
  observeEvent(input$register_mass_btn, {
    req(input$tablets_register_file)
    
    tryCatch({
      data <- read_excel(input$tablets_register_file$datapath)
      # Conversion 'vrai'/'faux' en logique
      if (is.character(data$powerbank)) {
        data$powerbank <- tolower(data$powerbank)
        data$powerbank <- ifelse(data$powerbank %in% c('vrai', 'true'), TRUE, FALSE)
      }
      new_tablets <- data.frame(
        tablette = data$tablette,
        chargeur = data$chargeur,
        powerbank = data$powerbank,
        registration_date = as.character(Sys.Date()),
        etat = rep("En stock", nrow(data)),
        stringsAsFactors = FALSE
      )
      
      current_tablets <- registered_tablets()
      updated_tablets <- rbind(current_tablets, new_tablets)
      registered_tablets(updated_tablets)
      
      # Réinitialiser le champ de fichier
      reset("tablets_register_file")
      
      showNotification(paste(nrow(new_tablets), "tablettes enregistrées avec succès!"), type = "default")
    }, error = function(e) {
      showNotification("Erreur lors de l'enregistrement en masse", type = "error")
    })
  })
  
  # Affectation individuelle
  observeEvent(input$assign_btn, {
    req(input$tab_num, input$agent_name, input$supervisor_name)
    
    # Vérifier que la tablette est en stock
    current_tablets <- registered_tablets()
    idx <- which(current_tablets$tablette == input$tab_num)
    if (length(idx) == 0 || current_tablets$etat[idx] != "En stock") {
      showNotification("La tablette n'est pas en stock et ne peut pas être affectée.", type = "error")
      return()
    }
    new_assignment <- data.frame(
      tablette = input$tab_num,
      chargeur = input$charger_num,
      powerbank = input$has_powerbank,
      agent_id = input$agent_id,
      agent_name = input$agent_name,
      agent_group = input$agent_group,
      agent_function = input$agent_function,
      agent_phone = input$agent_phone,
      agent_class = input$agent_class,
      supervisor_name = input$supervisor_name,
      supervisor_num = input$supervisor_num,
      assign_date = as.character(input$assign_date),
      stringsAsFactors = FALSE
    )
    
    current_assignments <- assignments()
    updated_assignments <- rbind(current_assignments, new_assignment)
    assignments(updated_assignments)
    
    # Mettre à jour l'état de la tablette à "Affectée"
    current_tablets$etat[idx] <- "Affectée"
    registered_tablets(current_tablets)
    
    # Réinitialiser les champs
    updateTextInput(session, "tab_num", value = "")
    updateTextInput(session, "charger_num", value = "")
    updateMaterialSwitch(session, "has_powerbank", value = FALSE)
    updateTextInput(session, "agent_id", value = "")
    updateTextInput(session, "agent_name", value = "")
    updateTextInput(session, "agent_group", value = "")
    updateSelectInput(session, "agent_function", selected = "Enquêteur")
    updateTextInput(session, "agent_phone", value = "")
    updateTextInput(session, "agent_class", value = "")
    updateTextInput(session, "supervisor_name", value = "")
    updateTextInput(session, "supervisor_num", value = "")
    updateDateInput(session, "assign_date", value = Sys.Date())
    
    showNotification("Affectation créée avec succès!", type = "default")
  })
  
  # Affectation en masse
  observeEvent(input$mass_assign_btn, {
    req(input$agents_file, input$tablets_file)
    tryCatch({
      agents_data <- read_excel(input$agents_file$datapath)
      tablets_data <- read_excel(input$tablets_file$datapath)
      if (is.character(tablets_data$powerbank)) {
        tablets_data$powerbank <- tolower(tablets_data$powerbank)
        tablets_data$powerbank <- ifelse(tablets_data$powerbank %in% c('vrai', 'true'), TRUE, FALSE)
      }
      # Filtrer les tablettes en stock
      current_tablets <- registered_tablets()
      tablets_data <- tablets_data[tablets_data$tablette %in% current_tablets$tablette & current_tablets$etat[match(tablets_data$tablette, current_tablets$tablette)] == "En stock", ]
      n_agents <- nrow(agents_data)
      n_tablets <- nrow(tablets_data)
      if (n_tablets > n_agents) {
        showNotification("Plus de tablettes que d'agents disponibles", type = "warning")
        return()
      }
      shuffled_tablets <- tablets_data[sample(n_tablets), ]
      new_assignments <- data.frame(
        tablette = shuffled_tablets$tablette,
        chargeur = shuffled_tablets$chargeur,
        powerbank = shuffled_tablets$powerbank,
        agent_id = agents_data$id_agent[1:n_tablets],
        agent_name = agents_data$agent[1:n_tablets],
        agent_group = agents_data$groupe[1:n_tablets],
        agent_function = agents_data$fonction[1:n_tablets],
        agent_phone = agents_data$telephone[1:n_tablets],
        agent_class = agents_data$classe[1:n_tablets],
        supervisor_name = agents_data$superviseur[1:n_tablets],
        supervisor_num = agents_data$numero_superviseur[1:n_tablets],
        assign_date = as.character(Sys.Date()),
        stringsAsFactors = FALSE
      )
      current_assignments <- assignments()
      updated_assignments <- rbind(current_assignments, new_assignments)
      assignments(updated_assignments)
      # Mettre à jour l'état des tablettes à "Affectée"
      idxs <- match(shuffled_tablets$tablette, current_tablets$tablette)
      current_tablets$etat[idxs] <- "Affectée"
      registered_tablets(current_tablets)
      
      # Réinitialiser les champs de fichiers
      reset("agents_file")
      reset("tablets_file")
      
      showNotification(paste(nrow(new_assignments), "affectations créées avec succès!"), type = "default")
    }, error = function(e) {
      showNotification("Erreur lors de l'affectation en masse", type = "error")
    })
  })
  
  # Mise à jour des choix pour la génération de fiches
  observe({
    current_assignments <- assignments()
    if (nrow(current_assignments) > 0) {
      choices <- paste(current_assignments$agent_name, "-", current_assignments$tablette)
      updateSelectInput(session, "fiche_assign_select", choices = choices)
    }
  })
  
  # Génération de fiche individuelle
  observeEvent(input$generate_fiche_btn, {
    req(input$fiche_assign_select)
    
    current_assignments <- assignments()
    selected_index <- which(paste(current_assignments$agent_name, "-", current_assignments$tablette) == input$fiche_assign_select)
    
    if (length(selected_index) > 0) {
      assign_data <- current_assignments[selected_index, ]
      
      tryCatch({
        filename <- generate_affectation_fiche(assign_data)
        showNotification(paste("Fiche générée:", filename), type = "default")
      }, error = function(e) {
        showNotification("Erreur lors de la génération de la fiche", type = "error")
      })
    }
  })
  
  # Génération de toutes les fiches
  observeEvent(input$generate_all_fiches_btn, {
    current_assignments <- assignments()
    
    if (nrow(current_assignments) == 0) {
      showNotification("Aucune affectation à traiter", type = "warning")
      return()
    }
    
    tryCatch({
      for (i in 1:nrow(current_assignments)) {
        assign_data <- current_assignments[i, ]
        generate_affectation_fiche(assign_data)
      }
      showNotification(paste(nrow(current_assignments), "fiches générées avec succès!"), type = "default")
    }, error = function(e) {
      showNotification("Erreur lors de la génération des fiches", type = "error")
    })
  })
  
  # Observateur pour mettre à jour les choix de tablettes affectées et le powerbank
  observe({
    current_assignments <- assignments()
    if (nrow(current_assignments) > 0) {
      choices <- paste(current_assignments$tablette, "-", current_assignments$agent_name)
      updateSelectInput(session, "return_tablet_select", choices = choices)
    } else {
      updateSelectInput(session, "return_tablet_select", choices = "Aucune tablette affectée")
    }
  })
  
  # Observateur pour mettre à jour le powerbank selon la tablette sélectionnée
  observeEvent(input$return_tablet_select, {
    req(input$return_tablet_select)
    
    current_assignments <- assignments()
    if (nrow(current_assignments) > 0) {
      # Extraire le numéro de tablette de la sélection
      tablet_num <- strsplit(input$return_tablet_select, " - ")[[1]][1]
      
      # Trouver l'affectation de cette tablette
      tablet_idx <- which(current_assignments$tablette == tablet_num)
      
      if (length(tablet_idx) > 0) {
        assignment <- current_assignments[tablet_idx[1], ]
        
        # Mettre à jour le powerbank selon l'affectation
        updateMaterialSwitch(session, "return_has_powerbank", value = assignment$powerbank)
        
        # Désactiver/activer le powerbank selon l'affectation
        if (assignment$powerbank) {
          # Si affecté avec powerbank, permettre de le décocher
          shinyjs::enable("return_has_powerbank")
        } else {
          # Si affecté sans powerbank, désactiver la case
          shinyjs::disable("return_has_powerbank")
        }
        
        # Mettre à jour le chargeur
        updateTextInput(session, "return_charger_num", value = assignment$chargeur)
      }
    }
  })
  
  # Output pour afficher l'information sur le powerbank
  output$powerbank_info <- renderText({
    req(input$return_tablet_select)
    
    current_assignments <- assignments()
    if (nrow(current_assignments) > 0) {
      tablet_num <- strsplit(input$return_tablet_select, " - ")[[1]][1]
      tablet_idx <- which(current_assignments$tablette == tablet_num)
      
      if (length(tablet_idx) > 0) {
        assignment <- current_assignments[tablet_idx[1], ]
        if (assignment$powerbank) {
          "ℹ️ Cette tablette a été affectée avec un powerbank (vous pouvez le décocher si perdu)"
        } else {
          "ℹ️ Cette tablette a été affectée sans powerbank (case désactivée)"
        }
      } else {
        ""
      }
    } else {
      ""
    }
  })
  
  # Fonction pour traiter le retour de tablette
  process_tablet_return <- function(assignment, input_data) {
    # Créer l'enregistrement de retour
    new_return <- data.frame(
      tablette = assignment$tablette,
      agent_id = assignment$agent_id,
      agent_name = assignment$agent_name,
      charger_retourne = input_data$return_charger_num,
      powerbank_retourne = input_data$return_has_powerbank,
      return_reason = input_data$return_reason,
      return_condition = input_data$return_condition,
      return_date = as.character(input_data$return_date),
      return_notes = input_data$return_notes,
      stringsAsFactors = FALSE
    )
    
    # Ajouter au tableau des retours
    current_returns <- tablet_returns()
    updated_returns <- rbind(current_returns, new_return)
    tablet_returns(updated_returns)
    
    # Mettre à jour l'état de la tablette selon la condition de retour
    current_tablets <- registered_tablets()
    tablet_idx <- which(current_tablets$tablette == assignment$tablette)
    
    if (length(tablet_idx) > 0) {
      # Déterminer le nouvel état selon la condition de retour
      new_state <- switch(input_data$return_condition,
        "Bon état" = "En stock",
        "Légèrement endommagée" = "En réparation",
        "Endommagée" = "En réparation",
        "Hors service" = "Hors service",
        "En stock"  # par défaut
      )
      
      current_tablets$etat[tablet_idx] <- new_state
      registered_tablets(current_tablets)
    }
    
    # Supprimer l'affectation
    current_assignments <- assignments()
    assignment_idx <- which(current_assignments$tablette == assignment$tablette)
    if (length(assignment_idx) > 0) {
      updated_assignments <- current_assignments[-assignment_idx, ]
      assignments(updated_assignments)
    }
    
    # Réinitialiser les champs
    updateTextInput(session, "return_agent_id", value = "")
    updateSelectInput(session, "return_tablet_select", selected = "")
    updateTextInput(session, "return_charger_num", value = "")
    updateMaterialSwitch(session, "return_has_powerbank", value = FALSE)
    updateTextInput(session, "return_reason", value = "")
    updateSelectInput(session, "return_condition", selected = "Bon état")
    updateDateInput(session, "return_date", value = Sys.Date())
    updateTextAreaInput(session, "return_notes", value = "")
    
    showNotification("Retour de tablette enregistré avec succès!", type = "default")
  }
  
  # Observateur pour le retour de tablette
  observeEvent(input$return_tablet_btn, {
    req(input$return_agent_id, input$return_tablet_select)
    
    # Vérifier que l'agent existe et a une tablette affectée
    current_assignments <- assignments()
    if (nrow(current_assignments) == 0) {
      showNotification("Aucune affectation trouvée", type = "error")
      return()
    }
    
    # Extraire le numéro de tablette de la sélection
    tablet_num <- strsplit(input$return_tablet_select, " - ")[[1]][1]
    
    # Trouver l'affectation de cette tablette
    tablet_idx <- which(current_assignments$tablette == tablet_num)
    
    if (length(tablet_idx) == 0) {
      showNotification("Tablette non trouvée dans les affectations", type = "error")
      return()
    }
    
    assignment <- current_assignments[tablet_idx[1], ]
    
    # Vérifier que l'agent correspond
    if (assignment$agent_id != input$return_agent_id) {
      showNotification("L'ID de l'agent ne correspond pas à l'affectation", type = "error")
      return()
    }
    
    # Vérification de sécurité pour le chargeur uniquement
    charger_mismatch <- assignment$chargeur != input$return_charger_num
    
    # Si il y a une différence de chargeur, demander confirmation
    if (charger_mismatch) {
      # Créer une modal pour les questions
      showModal(modalDialog(
        title = "Chargeur manquant",
        "Attention: Le chargeur retourné ne correspond pas à celui affecté",
        br(), br(),
        div(
          h6("Chargeur manquant:"),
          radioButtons("charger_lost", "Avez-vous perdu ou endommagé le chargeur?",
                      choices = c("Non", "Oui"), selected = "Non")
        ),
        footer = tagList(
          modalButton("Annuler"),
          actionButton("confirm_return", "Confirmer le retour", class = "btn-warning")
        ),
        size = "m"
      ))
      return()
    }
    
    # Si tout est correct, procéder au retour
    process_tablet_return(assignment, input)
  })
  
  # Observateur pour la confirmation du retour avec chargeur manquant
  observeEvent(input$confirm_return, {
    req(input$return_agent_id)
    
    current_assignments <- assignments()
    agent_idx <- which(current_assignments$agent_id == input$return_agent_id)
    
    if (length(agent_idx) == 0) {
      showNotification("Erreur: affectation non trouvée", type = "error")
      removeModal()
      return()
    }
    
    assignment <- current_assignments[agent_idx[1], ]
    
    # Ajouter les informations sur le chargeur manquant
    notes <- input$return_notes
    if (assignment$chargeur != input$return_charger_num) {
      charger_status <- ifelse(input$charger_lost == "Oui", "perdu/endommagé", "non retourné")
      notes <- paste(notes, paste0("Chargeur ", charger_status, "."), sep = " ")
    }
    
    # Créer un objet input modifié
    modified_input <- list(
      return_charger_num = input$return_charger_num,
      return_has_powerbank = input$return_has_powerbank,
      return_reason = input$return_reason,
      return_condition = input$return_condition,
      return_date = input$return_date,
      return_notes = notes
    )
    
    # Traiter le retour
    process_tablet_return(assignment, modified_input)
    removeModal()
  })
  
  # Observateur pour mettre à jour le statut des tablettes (tableau de suivi)
  observe({
    reg_data <- registered_tablets()
    assign_data <- assignments()
    returns_data <- tablet_returns()
    if (nrow(reg_data) > 0) {
      suivi <- reg_data
      suivi$status <- reg_data$etat
      suivi$current_agent <- ""
      suivi$assign_date <- ""
      suivi$return_date <- ""
      suivi$condition <- ""
      # Pour chaque tablette affectée, renseigner l'agent et la date
      if (nrow(assign_data) > 0) {
        for (i in 1:nrow(assign_data)) {
          idx <- which(suivi$tablette == assign_data$tablette[i])
          if (length(idx) > 0 && suivi$etat[idx] == "Affectée") {
            suivi$current_agent[idx] <- assign_data$agent_name[i]
            suivi$assign_date[idx] <- assign_data$assign_date[i]
          }
        }
      }
      # Pour chaque retour, renseigner la date et l'état
      if (nrow(returns_data) > 0) {
        for (i in 1:nrow(returns_data)) {
          idx <- which(suivi$tablette == returns_data$tablette[i])
          if (length(idx) > 0) {
            suivi$return_date[idx] <- returns_data$return_date[i]
            suivi$condition[idx] <- returns_data$return_condition[i]
          }
        }
      }
      tablet_status(suivi)
    }
  })
  
  # Sorties des tableaux
  output$register_table <- renderDT({
    data <- registered_tablets()
    if (nrow(data) > 0) {
      data$powerbank <- ifelse(data$powerbank, "Oui", "Non")
    }
    datatable(
      data,
      options = list(
        pageLength = 10,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.24/i18n/French.json')
      ),
      rownames = FALSE
    )
  })
  
  output$assign_table <- renderDT({
    data <- assignments()
    if (nrow(data) > 0) {
      data$powerbank <- ifelse(data$powerbank, "Oui", "Non")
    }
    datatable(
      data,
      options = list(
        pageLength = 10,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.24/i18n/French.json')
      ),
      rownames = FALSE
    )
  })
  
  # Output pour le tableau des retours
  output$returns_table <- renderDT({
    returns_data <- tablet_returns()
    if (nrow(returns_data) == 0) {
      datatable(
        data.frame(Message = "Aucun retour enregistré"),
        options = list(pageLength = 10, dom = 't'),
        rownames = FALSE
      )
    } else {
      # Formater les données pour l'affichage
      display_data <- returns_data
      if (nrow(display_data) > 0) {
        display_data$powerbank_retourne <- ifelse(display_data$powerbank_retourne, "Oui", "Non")
      }
      datatable(
        display_data,
        options = list(
          pageLength = 10,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.24/i18n/French.json')
        ),
        rownames = FALSE,
        colnames = c("Tablette", "ID Agent", "Nom Agent", "Chargeur retourné", "Powerbank retourné", 
                    "Motif", "État", "Date", "Notes")
      )
    }
  })
  
  # Outputs pour les compteurs du tableau de bord
  
  output$available_tablets_count <- renderText({
    registered_data <- registered_tablets()
    if (nrow(registered_data) == 0) return("0")
    sum(registered_data$etat == "En stock", na.rm = TRUE)
  })
  
  output$assigned_tablets_count <- renderText({
    registered_data <- registered_tablets()
    if (nrow(registered_data) == 0) return("0")
    sum(registered_data$etat == "Affectée", na.rm = TRUE)
  })
  
  output$returned_tablets_count <- renderText({
    registered_data <- registered_tablets()
    if (nrow(registered_data) == 0) return("0")
    sum(registered_data$etat == "En réparation", na.rm = TRUE)
  })
  
  output$out_of_service_tablets_count <- renderText({
    registered_data <- registered_tablets()
    if (nrow(registered_data) == 0) return("0")
    sum(registered_data$etat == "Hors service", na.rm = TRUE)
  })
  
  # Output pour le tableau de suivi des tablettes
  output$tracking_table <- renderDT({
    status_data <- tablet_status()
    if (nrow(status_data) == 0) {
      datatable(
        data.frame(Message = "Aucune tablette enregistrée"),
        options = list(pageLength = 10, dom = 't'),
        rownames = FALSE
      )
    } else {
      # Formater les données pour l'affichage
      display_data <- status_data[, c("tablette", "status", "current_agent", "assign_date", "return_date", "condition")]
      colnames(display_data) <- c("Tablette", "État", "Agent actuel", "Date d'affectation", "Date de retour", "État retour")
      
      datatable(
        display_data,
        options = list(
          pageLength = 15,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.24/i18n/French.json')
        ),
        rownames = FALSE,
        filter = 'top'
      )
    }
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)