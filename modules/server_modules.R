# Server side modular logic

# Registration logic
register_server <- function(input, output, session, reactive_tablets) {
  observeEvent(input$register_qr_btn, {
    req(input$reg_tab_num_qr, input$reg_charger_num_qr)
    new_tab <- data.frame(tablette=input$reg_tab_num_qr,
                          chargeur=input$reg_charger_num_qr,
                          powerbank=input$reg_has_powerbank_qr,
                          chargeur_ok=TRUE,
                          powerbank_ok=input$reg_has_powerbank_qr,
                          registration_date=as.character(Sys.Date()),
                          etat="En stock",
                          user_login=session$userData$current_user,
                          stringsAsFactors = FALSE)
    updated <- rbind(reactive_tablets(), new_tab)
    reactive_tablets(updated)
    save_registered_tablets(updated)
    showNotification("Tablette enregistr\u00e9e")
  })

  observeEvent(input$register_btn, {
    req(input$reg_tab_num, input$reg_charger_num)
    new_tab <- data.frame(tablette=input$reg_tab_num,
                          chargeur=input$reg_charger_num,
                          powerbank=input$reg_has_powerbank,
                          chargeur_ok=TRUE,
                          powerbank_ok=input$reg_has_powerbank,
                          registration_date=as.character(Sys.Date()),
                          etat="En stock",
                          user_login=session$userData$current_user,
                          stringsAsFactors = FALSE)
    updated <- rbind(reactive_tablets(), new_tab)
    reactive_tablets(updated)
    save_registered_tablets(updated)
    showNotification("Tablette enregistr\u00e9e")
  })

  observeEvent(input$register_mass_btn, {
    req(input$tablets_register_file)
    tryCatch({
      data <- read_excel(input$tablets_register_file$datapath)
      required <- c("tablette","chargeur","powerbank")
      if (!all(required %in% names(data))) stop("Colonnes manquantes")
      data$powerbank <- as.logical(data$powerbank)
      new_tabs <- data.frame(tablette=data$tablette,
                             chargeur=data$chargeur,
                             powerbank=data$powerbank,
                             chargeur_ok=TRUE,
                             powerbank_ok=data$powerbank,
                             registration_date=as.character(Sys.Date()),
                             etat="En stock",
                             user_login=session$userData$current_user,
                             stringsAsFactors = FALSE)
      updated <- rbind(reactive_tablets(), new_tabs)
      reactive_tablets(updated)
      save_registered_tablets(updated)
      showNotification("Import r\u00e9ussi")
    }, error=function(e){
      showNotification(paste("Erreur:", e$message), type="error")
    })
  })

  output$register_table <- renderDT({ reactive_tablets() })
}

# Assignment logic
assignment_server <- function(input, output, session, reactive_assignments, reactive_tablets) {
  observeEvent(input$assign_btn, {
    req(input$tab_num, input$agent_id)
    idx <- which(reactive_tablets()$tablette == input$tab_num)
    if (length(idx) == 0) {
      showNotification("Tablette inconnue", type="error")
      return()
    }
    assign <- data.frame(tablette=input$tab_num,
                         chargeur=input$charger_num,
                         powerbank=input$has_powerbank,
                         agent_id=input$agent_id,
                         agent_name=input$agent_name,
                         agent_group=input$agent_group,
                         agent_function=input$agent_function,
                         agent_phone=input$agent_phone,
                         agent_class=input$agent_class,
                         supervisor_name=input$supervisor_name,
                         supervisor_num=input$supervisor_num,
                         assign_date=as.character(input$assign_date),
                         user_login=session$userData$current_user,
                         stringsAsFactors=FALSE)
    new_as <- rbind(reactive_assignments(), assign)
    reactive_assignments(new_as)
    save_assignments(new_as)
    t <- reactive_tablets()
    t$etat[idx] <- "Affect\u00e9e"
    reactive_tablets(t)
    save_registered_tablets(t)
    showNotification("Affectation enregistr\u00e9e")
  })

  output$assign_table <- renderDT({ reactive_assignments() })
}

# Fiche generation
fiche_server <- function(input, output, session, reactive_assignments, reactive_fiches) {
  observeEvent(input$generate_fiche_btn, {
    req(input$fiche_assign_select)
    idx <- as.numeric(input$fiche_assign_select)
    assign <- reactive_assignments()[idx,]
    tryCatch({
      res <- generate_affectation_fiche(assign)
      fiches <- rbind(reactive_fiches(), res$data)
      reactive_fiches(fiches)
      save_generated_fiches(fiches)
      showNotification(paste("Fiche", res$filename, "g\u00e9n\u00e9r\u00e9e"))
    }, error=function(e){
      showNotification(paste("Erreur fiche:", e$message), type="error")
    })
  })

  output$fiches_history_table <- renderDT({ reactive_fiches() })
}

# Return logic
return_server <- function(input, output, session, reactive_returns, reactive_tablets) {
  observeEvent(input$return_tablet_btn, {
    req(input$return_tablet_select)
    ret <- data.frame(
      tablette=input$return_tablet_select,
      chargeur=input$return_charger_num,
      powerbank=input$return_has_powerbank,
      agent_id=input$return_agent_id,
      return_reason=input$return_reason,
      condition=input$return_condition,
      return_date=as.character(input$return_date),
      notes=input$return_notes,
      user_login=session$userData$current_user,
      stringsAsFactors=FALSE)
    rets <- rbind(reactive_returns(), ret)
    reactive_returns(rets)
    save_tablet_returns(rets)
    t <- reactive_tablets()
    idx <- which(t$tablette == input$return_tablet_select)
    if (length(idx)>0) t$etat[idx] <- "En retour"
    reactive_tablets(t)
    save_registered_tablets(t)
    showNotification("Retour enregistr\u00e9")
  })

  output$returns_table <- renderDT({ reactive_returns() })
}

# Incident logic
incident_server <- function(input, output, session, reactive_incidents) {
  observeEvent(input$declare_incident_btn, {
    req(input$incident_tablet_select)
    inc <- data.frame(
      tablette=input$incident_tablet_select,
      incident_type=input$incident_type,
      description=input$incident_description,
      date=as.character(input$incident_date),
      user_login=session$userData$current_user,
      stringsAsFactors=FALSE)
    incs <- rbind(reactive_incidents(), inc)
    reactive_incidents(incs)
    save_tablet_incidents(incs)
    showNotification("Incident d\u00e9clar\u00e9")
  })

  output$incidents_table <- renderDT({ reactive_incidents() })
}

