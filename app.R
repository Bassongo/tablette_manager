# Application Shiny de gestion des tablettes
library(shiny)
library(DT)
library(readxl)

# Interface utilisateur principale
ui <- navbarPage(
  "Gestion des Tablettes",
  # Onglet d'affectation des tablettes aux agents
  tabPanel(
    "Affectation des tablettes",
    tabsetPanel(
      tabPanel(
        "Affectation individuelle",
        sidebarLayout(
          sidebarPanel(
            textInput("tab_num", "Num\u00e9ro de la tablette"),
            textInput("charger_num", "Num\u00e9ro de chargeur"),
            checkboxInput("has_powerbank", "Powerbank pr\u00e9sent"),
            textInput("agent_group", "Groupe de l'agent"),
            textInput("agent_name", "Nom de l'agent"),
            textInput("agent_class", "Classe"),
            textInput("agent_num", "Num\u00e9ro de l'agent"),
            textInput("supervisor_name", "Nom du superviseur"),
            textInput("supervisor_num", "Num\u00e9ro du superviseur"),
            dateInput("assign_date", "Date d'affectation"),
            actionButton("assign_btn", "Affecter")
          ),
          mainPanel(DTOutput("assign_table"))
        )
      ),
      tabPanel(
        "Affectation en masse",
        sidebarLayout(
          sidebarPanel(
            fileInput("agents_file", "Liste des agents (Excel)", accept = c(".xlsx", ".xls")),
            fileInput("tablets_file", "Liste des tablettes (Excel)", accept = c(".xlsx", ".xls")),
            actionButton("mass_assign_btn", "Affecter al\u00e9atoirement")
          ),
          mainPanel(DTOutput("mass_assign_table"))
        )
      )
    )
  ),
  # Onglet d'enregistrement des nouvelles tablettes
  tabPanel(
    "Enregistrement des tablettes",
    tabsetPanel(
      tabPanel(
        "Enregistrement individuel",
        sidebarLayout(
          sidebarPanel(
            textInput("reg_tab_num", "Num\u00e9ro de la tablette"),
            textInput("reg_charger_num", "Num\u00e9ro de chargeur"),
            checkboxInput("reg_has_powerbank", "Powerbank pr\u00e9sent"),
            actionButton("register_btn", "Enregistrer")
          ),
          mainPanel(DTOutput("register_table"))
        )
      ),
      tabPanel(
        "Enregistrement en masse",
        sidebarLayout(
          sidebarPanel(
            fileInput(
              "tablets_register_file",
              "Liste des tablettes (Excel)",
              accept = c(".xlsx", ".xls")
            ),
            actionButton("register_mass_btn", "Enregistrer en masse")
          ),
          mainPanel(DTOutput("register_table"))
        )
      )
    )
  ),
  # Onglet pour enregistrer le retour d'une tablette
  tabPanel(
    "Retour de tablette",
    sidebarLayout(
      sidebarPanel(
        textInput("return_tab_num", "Num\u00e9ro de la tablette"),
        textInput("return_agent", "Nom de l'agent"),
        actionButton("return_btn", "Confirmer le retour")
      ),
      mainPanel(DTOutput("return_table"))
    )
  ),
  # Onglet de d\u00e9claration d'incident sur une tablette
  tabPanel(
    "D\u00e9claration d'incident",
    sidebarLayout(
      sidebarPanel(
        textInput("incident_tab", "Num\u00e9ro de la tablette"),
        selectInput("incident_type", "Type d'incident", choices = c("\u00e9cran cass\u00e9", "perte", "autre")),
        textAreaInput("incident_comment", "Commentaire"),
        textInput("incident_agent", "Nom de l'agent"),
        actionButton("incident_btn", "D\u00e9clarer")
      ),
      mainPanel(DTOutput("incident_table"))
    )
  ),
  # Tableau de bord r\u00e9capitulatif
  tabPanel(
    "Suivi des tablettes",
    fluidPage(
      fluidRow(
        column(4, verbatimTextOutput("stock_txt")),
        column(4, verbatimTextOutput("assigned_txt")),
        column(4, verbatimTextOutput("incident_txt"))
      ),
      DTOutput("dashboard_table")
    )
  )
)

server <- function(input, output, session) {
  # Liste des tablettes enregistr\u00e9es
  registered <- reactiveVal(
    data.frame(
      tablette = character(),
      chargeur = character(),
      powerbank = logical(),
      stringsAsFactors = FALSE
    )
  )
  # Table des affectations individuelles
  assignments <- reactiveVal(
    data.frame(
      tablette = character(),
      chargeur = character(),
      powerbank = logical(),
      groupe = character(),
      agent = character(),
      classe = character(),
      numero_agent = character(),
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
      date = character(),
      stringsAsFactors = FALSE
    )
  )

  # Enregistrement manuel d'une tablette
  observeEvent(input$register_btn, {
    new_entry <- data.frame(
      tablette = input$reg_tab_num,
      chargeur = input$reg_charger_num,
      powerbank = input$reg_has_powerbank,
      stringsAsFactors = FALSE
    )
    registered(rbind(registered(), new_entry))
  })

  # Enregistrement en masse des tablettes via fichier Excel
  observeEvent(input$register_mass_btn, {
    req(input$tablets_register_file)
    tablets <- read_excel(input$tablets_register_file$datapath)
    registered(rbind(registered(), tablets))
  })

  output$register_table <- renderDT(registered())

  observeEvent(input$assign_btn, {
    # V\u00e9rifie que la tablette est enregistr\u00e9e avant l'affectation
    if (!(input$tab_num %in% registered()$tablette)) {
      showNotification("Tablette non enregistr\u00e9e", type = "error")
      return()
    }

    new_entry <- data.frame(
      tablette = input$tab_num,
      chargeur = input$charger_num,
      powerbank = input$has_powerbank,
      groupe = input$agent_group,
      agent = input$agent_name,
      classe = input$agent_class,
      numero_agent = input$agent_num,
      superviseur = input$supervisor_name,
      numero_superviseur = input$supervisor_num,
      date = as.character(input$assign_date),
      stringsAsFactors = FALSE
    )
    assignments(rbind(assignments(), new_entry))
  })

  output$assign_table <- renderDT(assignments())

  observeEvent(input$mass_assign_btn, {
    req(input$agents_file, input$tablets_file)
    agents <- read_excel(input$agents_file$datapath)
    tablets <- read_excel(input$tablets_file$datapath)

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
    mass_assignments(result)
  })

  output$mass_assign_table <- renderDT(mass_assignments())

  observeEvent(input$return_btn, {
    new_entry <- data.frame(
      tablette = input$return_tab_num,
      agent = input$return_agent,
      date_retour = Sys.Date(),
      stringsAsFactors = FALSE
    )
    returns(rbind(returns(), new_entry))
  })

  output$return_table <- renderDT(returns())

  observeEvent(input$incident_btn, {
    new_entry <- data.frame(
      tablette = input$incident_tab,
      type = input$incident_type,
      commentaire = input$incident_comment,
      agent = input$incident_agent,
      date = Sys.Date(),
      stringsAsFactors = FALSE
    )
    incidents(rbind(incidents(), new_entry))
  })

  output$incident_table <- renderDT(incidents())

  output$stock_txt <- renderText({
    total <- nrow(assignments()) + nrow(incidents()) + nrow(returns())
    stock <- total - nrow(assignments())
    paste("Stock disponible:", stock)
  })

  output$assigned_txt <- renderText({
    paste("Tablettes affect\u00e9es:", nrow(assignments()))
  })

  output$incident_txt <- renderText({
    paste("Incidents d\u00e9clar\u00e9s:", nrow(incidents()))
  })

  output$dashboard_table <- renderDT(assignments())
}

shinyApp(ui, server)
