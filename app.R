# Application Shiny de gestion des tablettes
library(shiny)
library(DT)
library(readxl)
library(shinyjs)
library(shinythemes)

# Interface utilisateur principale
ui <- tagList(
  useShinyjs(),
  tags$head(
    tags$style(HTML(".btn-primary{background-color:#007bff;border-color:#007bff;}"))
  ),
  fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("Gestion des Tablettes"),
    navlistPanel(
      widths = c(2, 10),
      tabPanel(
        "Enregistrement des tablettes",
        tabsetPanel(
          tabPanel(
            "Enregistrement individuel",
            sidebarLayout(
              sidebarPanel(
                actionButton("register_btn", "Enregistrer", class = "btn-primary"),
                textInput("reg_tab_num", "Numéro de la tablette"),
                textInput("reg_charger_num", "Numéro de chargeur"),
                checkboxInput("reg_has_powerbank", "Powerbank présent")
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
                actionButton("register_mass_btn", "Enregistrer en masse", class = "btn-primary")
              ),
              mainPanel(DTOutput("register_table"))
            )
          )
        )
      ),
      tabPanel(
        "Affectation des tablettes",
        tabsetPanel(
          tabPanel(
            "Affectation individuelle",
            sidebarLayout(
              sidebarPanel(
                textInput("tab_num", "Numéro de la tablette"),
                textInput("charger_num", "Numéro de chargeur"),
                checkboxInput("has_powerbank", "Powerbank présent"),
                textInput("agent_group", "Groupe de l'agent"),
                textInput("agent_name", "Nom de l'agent"),
                textInput("agent_class", "Classe"),
                textInput("agent_num", "Numéro de l'agent"),
                textInput("supervisor_name", "Nom du superviseur"),
                textInput("supervisor_num", "Numéro du superviseur"),
                dateInput("assign_date", "Date d'affectation"),
                actionButton("assign_btn", "Affecter", class = "btn-primary")
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
                actionButton("mass_assign_btn", "Affecter aléatoirement", class = "btn-primary")
              ),
              mainPanel(DTOutput("mass_assign_table"))
            )
          )
        )
      ),
      tabPanel(
        "Retour de tablette",
        sidebarLayout(
          sidebarPanel(
            textInput("return_tab_num", "Numéro de la tablette"),
            textInput("return_agent", "Nom de l'agent"),
            textInput("return_charger", "Numéro de chargeur"),
            checkboxInput("return_powerbank", "Powerbank présent"),
            actionButton("return_btn", "Confirmer le retour", class = "btn-primary")
          ),
          mainPanel(DTOutput("return_table"))
        )
      ),
      tabPanel(
        "Déclaration d'incident",
        sidebarLayout(
          sidebarPanel(
            textInput("incident_tab", "Numéro de la tablette"),
            selectInput("incident_type", "Type d'incident", choices = c("écran cassé", "perte", "autre")),
            textAreaInput("incident_comment", "Commentaire"),
            textInput("incident_agent", "Nom de l'agent"),
            checkboxInput("incident_charger_ok", "Chargeur fonctionnel", TRUE),
            checkboxInput("incident_powerbank_ok", "Powerbank fonctionnel", TRUE),
            actionButton("incident_btn", "Déclarer", class = "btn-primary")
          ),
          mainPanel(DTOutput("incident_table"))
        )
      ),
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
  )
)

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
      classe = input$agent_class,
      numero_agent = input$agent_num,
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
    updateTextInput(session, "agent_name", value = "")
    updateTextInput(session, "agent_class", value = "")
    updateTextInput(session, "agent_num", value = "")
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
    assignments(rbind(assignments(), result[, names(assignments()), drop = FALSE]))
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
        footer = tagList(modalButton("Annuler"), actionButton("confirm_missing", "Confirmer", class = "btn-primary"))
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
