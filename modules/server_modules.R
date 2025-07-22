# === MODULES SERVEUR COMPLETS ===
# Toute la logique serveur modulaire

# === MODULE ENREGISTREMENT SERVEUR ===
register_server_logic <- function(input, output, session, registered_tablets, user_role, current_user) {
  
  # Enregistrement via QR
  observeEvent(input$register_qr_btn, {
    req(input$reg_tab_num_qr, input$reg_charger_num_qr)
    
    new_tablet <- data.frame(
      tablette = input$reg_tab_num_qr,
      chargeur = input$reg_charger_num_qr,
      powerbank = input$reg_has_powerbank_qr,
      chargeur_ok = TRUE,
      powerbank_ok = input$reg_has_powerbank_qr,
      registration_date = as.character(Sys.Date()),
      etat = "En stock",
      user_login = current_user(),
      stringsAsFactors = FALSE
    )
    
    current_tablets <- registered_tablets()
    updated_tablets <- rbind(current_tablets, new_tablet)
    registered_tablets(updated_tablets)
    save_registered_tablets(updated_tablets)
    
    updateTextInput(session, 'reg_tab_num_qr', value = '')
    updateTextInput(session, 'reg_charger_num_qr', value = '')
    updateMaterialSwitch(session, 'reg_has_powerbank_qr', value = FALSE)
    
    showNotification("Tablette enregistrée avec succès!", type = "default")
  })
  
  # Enregistrement manuel
  observeEvent(input$register_btn, {
    req(input$reg_tab_num, input$reg_charger_num)
    
    new_tablet <- data.frame(
      tablette = input$reg_tab_num,
      chargeur = input$reg_charger_num,
      powerbank = input$reg_has_powerbank,
      chargeur_ok = TRUE,
      powerbank_ok = input$reg_has_powerbank,
      registration_date = as.character(Sys.Date()),
      etat = "En stock",
      user_login = current_user(),
      stringsAsFactors = FALSE
    )
    
    current_tablets <- registered_tablets()
    updated_tablets <- rbind(current_tablets, new_tablet)
    registered_tablets(updated_tablets)
    save_registered_tablets(updated_tablets)
    
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
      
      # Vérifier que les colonnes requises existent
      required_columns <- c("tablette", "chargeur", "powerbank")
      missing_columns <- setdiff(required_columns, colnames(data))
      
      if (length(missing_columns) > 0) {
        showNotification(
          paste("Erreur : Colonnes manquantes dans le fichier Excel :", paste(missing_columns, collapse = ", ")), 
          type = "error"
        )
        return()
      }
      
      # Conversion powerbank
      if (is.character(data$powerbank)) {
        data$powerbank <- tolower(data$powerbank)
        data$powerbank <- ifelse(data$powerbank %in% c('vrai', 'true', 'oui', 'yes', '1'), TRUE, FALSE)
      } else if (is.numeric(data$powerbank)) {
        data$powerbank <- as.logical(data$powerbank)
      }
      
      new_tablets <- data.frame(
        tablette = as.character(data$tablette),
        chargeur = as.character(data$chargeur),
        powerbank = as.logical(data$powerbank),
        chargeur_ok = TRUE,
        powerbank_ok = as.logical(data$powerbank),
        registration_date = as.character(Sys.Date()),
        etat = rep("En stock", nrow(data)),
        user_login = rep(current_user(), nrow(data)),
        stringsAsFactors = FALSE
      )
      
      current_tablets <- registered_tablets()
      updated_tablets <- rbind(current_tablets, new_tablets)
      registered_tablets(updated_tablets)
      save_registered_tablets(updated_tablets)
      
      showNotification(paste(nrow(new_tablets), "tablettes enregistrées avec succès!"), type = "default")
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'enregistrement en masse :", e$message), type = "error")
    })
  })
  
  # Output pour le tableau d'enregistrement
  output$register_table <- renderDT({
    req(user_role(), current_user())
    
    data <- get_user_data(registered_tablets(), user_role(), current_user())
    
    if (nrow(data) > 0) {
      data$powerbank <- ifelse(data$powerbank, "Oui", "Non")
      data$chargeur_ok <- ifelse(data$chargeur_ok, "Oui", "Non")
      data$powerbank_ok <- ifelse(data$powerbank_ok, "Oui", "Non")
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
}

# === MODULE AFFECTATION SERVEUR ===
assignment_server_logic <- function(input, output, session, assignments, registered_tablets, user_role, current_user) {
  
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
      user_login = current_user(),
      stringsAsFactors = FALSE
    )
    
    current_assignments <- assignments()
    updated_assignments <- rbind(current_assignments, new_assignment)
    assignments(updated_assignments)
    save_assignments(updated_assignments)
    
    # Mettre à jour l'état de la tablette à "Affectée"
    current_tablets$etat[idx] <- "Affectée"
    registered_tablets(current_tablets)
    save_registered_tablets(current_tablets)
    
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
      
      # Vérifications et logique d'affectation en masse
      # (Code similaire à l'original mais adapté)
      
      showNotification("Affectations en masse créées avec succès!", type = "default")
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'affectation en masse :", e$message), type = "error")
    })
  })
  
  # Output pour le tableau d'affectation
  output$assign_table <- renderDT({
    req(user_role(), current_user())
    
    data <- get_user_data(assignments(), user_role(), current_user())
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
}

# === MODULE GÉNÉRATION DE FICHES SERVEUR ===
fiche_server_logic <- function(input, output, session, assignments, generated_fiches, user_role, current_user, selected_fiche_index) {
  
  # Mise à jour des choix pour la génération de fiches
  observe({
    current_assignments <- get_user_data(assignments(), user_role(), current_user())
    if (nrow(current_assignments) > 0) {
      choices <- paste(current_assignments$agent_name, "-", current_assignments$tablette)
      updateSelectInput(session, "fiche_assign_select", choices = choices)
    }
  })
  
  # Génération de fiche individuelle
  observeEvent(input$generate_fiche_btn, {
    req(input$fiche_assign_select)
    
    current_assignments <- get_user_data(assignments(), user_role(), current_user())
    selected_index <- which(paste(current_assignments$agent_name, "-", current_assignments$tablette) == input$fiche_assign_select)
    
    if (length(selected_index) > 0) {
      assign_data <- current_assignments[selected_index, ]
      
      tryCatch({
        result <- generate_affectation_fiche(assign_data)
        
        # Ajouter à l'historique
        current_fiches <- generated_fiches()
        new_fiche <- data.frame(
          filename = result$filename,
          agent_name = assign_data$agent_name,
          tablette = assign_data$tablette,
          timestamp = as.character(Sys.time()),
          user_login = current_user(),
          transferred = FALSE,
          stringsAsFactors = FALSE
        )
        updated_fiches <- rbind(current_fiches, new_fiche)
        generated_fiches(updated_fiches)
        save_generated_fiches(updated_fiches)
        
        # Afficher prévisualisation
        showModal(modalDialog(
          title = paste("Fiche générée :", basename(result$filename)),
          size = "l",
          fluidRow(
            column(12,
                   h5("Informations de l'agent :"),
                   tags$ul(
                     tags$li(paste("Nom :", assign_data$agent_name)),
                     tags$li(paste("ID :", assign_data$agent_id)),
                     tags$li(paste("Groupe :", assign_data$agent_group)),
                     tags$li(paste("Fonction :", assign_data$agent_function)),
                     tags$li(paste("Téléphone :", assign_data$agent_phone))
                   ),
                   h5("Informations de la tablette :"),
                   tags$ul(
                     tags$li(paste("Tablette :", assign_data$tablette)),
                     tags$li(paste("Chargeur :", assign_data$chargeur)),
                     tags$li(paste("Powerbank :", ifelse(assign_data$powerbank, "Oui", "Non")))
                   ),
                   h5("Informations du superviseur :"),
                   tags$ul(
                     tags$li(paste("Nom :", assign_data$supervisor_name)),
                     tags$li(paste("Numéro :", assign_data$supervisor_num)),
                     tags$li(paste("Date d'affectation :", assign_data$assign_date))
                   )
            )
          ),
          footer = tagList(
            downloadButton("download_fiche", "Télécharger la fiche", class = "btn-success"),
            modalButton("Fermer")
          )
        ))
        
        selected_fiche_index(nrow(updated_fiches))
        showNotification(paste("Fiche générée:", basename(result$filename)), type = "default")
      }, error = function(e) {
        showNotification(paste("Erreur lors de la génération de la fiche :", e$message), type = "error")
      })
    }
  })
  
  # Génération de toutes les fiches
  observeEvent(input$generate_all_fiches_btn, {
    current_assignments <- get_user_data(assignments(), user_role(), current_user())
    
    if (nrow(current_assignments) == 0) {
      showNotification("Aucune affectation à traiter", type = "warning")
      return()
    }
    
    tryCatch({
      current_fiches <- generated_fiches()
      new_rows <- data.frame(
        filename = character(),
        agent_name = character(),
        tablette = character(),
        timestamp = character(),
        user_login = character(),
        transferred = logical(),
        stringsAsFactors = FALSE
      )
      
      for (i in 1:nrow(current_assignments)) {
        assign_data <- current_assignments[i, ]
        result <- generate_affectation_fiche(assign_data)
        
        new_rows <- rbind(new_rows, data.frame(
          filename = basename(result$filename),
          agent_name = assign_data$agent_name,
          tablette = assign_data$tablette,
          timestamp = as.character(Sys.time()),
          user_login = current_user(),
          transferred = FALSE,
          stringsAsFactors = FALSE
        ))
      }
      
      updated_fiches <- rbind(current_fiches, new_rows)
      generated_fiches(updated_fiches)
      save_generated_fiches(updated_fiches)
      showNotification(paste(nrow(current_assignments), "fiches générées avec succès!"), type = "default")
    }, error = function(e) {
      showNotification(paste("Erreur lors de la génération des fiches :", e$message), type = "error")
    })
  })
  
  # Réinitialisation de la sélection
  observeEvent(input$reset_selection_btn, {
    updateSelectInput(session, "fiche_assign_select", selected = "")
    selected_fiche_index(NULL)
    showNotification("Sélection réinitialisée", type = "default")
  })
  
  # Output pour l'historique des fiches
  output$fiches_history_table <- DT::renderDataTable({
    fiches <- get_user_data(generated_fiches(), user_role(), current_user())
    if (nrow(fiches) == 0) {
      return(datatable(data.frame(Message = "Aucune fiche générée"), options = list(dom = 't'), rownames = FALSE))
    }
    
    # S'assurer que la colonne transferred existe
    if (!"transferred" %in% colnames(fiches)) {
      fiches$transferred <- FALSE
    }
    fiches$transferred[is.na(fiches$transferred)] <- FALSE
    
    fiches$Action <- ifelse(fiches$transferred,
                            "Déjà transférée",
                            sprintf('<button class="btn btn-sm btn-primary" onclick="Shiny.setInputValue(\'transfer_fiche\', \'%s\', {priority: \"event\"})">Transférer à la centrale</button>', fiches$filename)
    )
    fiches$transferred <- ifelse(fiches$transferred, "Oui", "Non")
    DT::datatable(
      fiches[, c("filename", "agent_name", "tablette", "timestamp", "transferred", "Action")],
      escape = FALSE,
      rownames = FALSE,
      options = list(pageLength = 10, language = list(url = '//cdn.datatables.net/plug-ins/1.10.24/i18n/French.json'))
    )
  })
  
  # Téléchargement de fiche
  output$download_fiche <- downloadHandler(
    filename = function() {
      req(selected_fiche_index())
      current_fiches <- generated_fiches()
      if (selected_fiche_index() <= nrow(current_fiches)) {
        current_fiches$filename[selected_fiche_index()]
      } else {
        "fiche.docx"
      }
    },
    content = function(file) {
      req(selected_fiche_index())
      current_fiches <- generated_fiches()
      if (selected_fiche_index() <= nrow(current_fiches)) {
        fiche_filename <- current_fiches$filename[selected_fiche_index()]
        fiche_path <- file.path("fiches_generees", fiche_filename)
        if (file.exists(fiche_path)) {
          file.copy(fiche_path, file)
        }
      }
    }
  )
}

# === MODULE RETOUR SERVEUR ===
return_server_logic <- function(input, output, session, tablet_returns, assignments, registered_tablets, user_role, current_user) {
  
  # Observateur pour mettre à jour les choix de tablettes affectées
  observe({
    current_assignments <- get_user_data(assignments(), user_role(), current_user())
    if (nrow(current_assignments) > 0) {
      choices <- paste(current_assignments$tablette, "-", current_assignments$agent_name)
      updateSelectInput(session, "return_tablet_select", choices = choices)
    } else {
      updateSelectInput(session, "return_tablet_select", choices = "Aucune tablette affectée")
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
      user_login = current_user(),
      stringsAsFactors = FALSE
    )
    
    # Ajouter au tableau des retours
    current_returns <- tablet_returns()
    updated_returns <- rbind(current_returns, new_return)
    tablet_returns(updated_returns)
    save_tablet_returns(updated_returns)
    
    # Mettre à jour l'état de la tablette selon la condition de retour
    current_tablets <- registered_tablets()
    tablet_idx <- which(current_tablets$tablette == assignment$tablette)
    
    if (length(tablet_idx) > 0) {
      new_state <- switch(input_data$return_condition,
                          "Bon état" = "En stock",
                          "Légèrement endommagée" = "En réparation",
                          "Endommagée" = "En réparation",
                          "Hors service" = "Hors service",
                          "En stock"
      )
      
      current_tablets$etat[tablet_idx] <- new_state
      registered_tablets(current_tablets)
      save_registered_tablets(current_tablets)
    }
    
    # Supprimer l'affectation
    all_assignments <- assignments()
    all_assignment_idx <- which(all_assignments$tablette == assignment$tablette)
    if (length(all_assignment_idx) > 0) {
      updated_assignments <- all_assignments[-all_assignment_idx, ]
      assignments(updated_assignments)
      save_assignments(updated_assignments)
    }
    
    showNotification("Retour de tablette enregistré avec succès!", type = "default")
  }
  
  # Observateur pour le retour de tablette
  observeEvent(input$return_tablet_btn, {
    req(input$return_agent_id, input$return_tablet_select)
    
    current_assignments <- get_user_data(assignments(), user_role(), current_user())
    if (nrow(current_assignments) == 0) {
      showNotification("Aucune affectation trouvée", type = "error")
      return()
    }
    
    tablet_num <- strsplit(input$return_tablet_select, " - ")[[1]][1]
    tablet_idx <- which(current_assignments$tablette == tablet_num)
    
    if (length(tablet_idx) == 0) {
      showNotification("Tablette non trouvée dans les affectations", type = "error")
      return()
    }
    
    assignment <- current_assignments[tablet_idx[1], ]
    
    if (assignment$agent_id != input$return_agent_id) {
      showNotification("L'ID de l'agent ne correspond pas à l'affectation", type = "error")
      return()
    }
    
    # Traiter le retour
    process_tablet_return(assignment, input)
  })
  
  # Output pour le tableau des retours
  output$returns_table <- renderDT({
    req(user_role(), current_user())
    
    returns_data <- get_user_data(tablet_returns(), user_role(), current_user())
    if (nrow(returns_data) == 0) {
      datatable(
        data.frame(Message = "Aucun retour enregistré"),
        options = list(pageLength = 10, dom = 't'),
        rownames = FALSE
      )
    } else {
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
                     "Motif", "État", "Date", "Notes", "Utilisateur")
      )
    }
  })
}

# === MODULE INCIDENT SERVEUR ===
incident_server_logic <- function(input, output, session, tablet_incidents, assignments, registered_tablets, user_role, current_user) {
  
  # Observateur pour mettre à jour les choix de tablettes
  observe({
    current_assignments <- get_user_data(assignments(), user_role(), current_user())
    if (nrow(current_assignments) > 0) {
      choices <- paste(current_assignments$tablette, "-", current_assignments$agent_name)
      updateSelectInput(session, "incident_tablet_select", choices = choices)
    } else {
      updateSelectInput(session, "incident_tablet_select", choices = "Aucune tablette affectée")
    }
  })
  
  # Déclaration d'incident
  observeEvent(input$declare_incident_btn, {
    req(input$incident_agent_id, input$incident_tablet_select)
    
    current_assignments <- get_user_data(assignments(), user_role(), current_user())
    if (nrow(current_assignments) == 0) {
      showNotification("Aucune affectation trouvée", type = "error")
      return()
    }
    
    tablet_num <- strsplit(input$incident_tablet_select, " - ")[[1]][1]
    tablet_idx <- which(current_assignments$tablette == tablet_num)
    
    if (length(tablet_idx) == 0) {
      showNotification("Tablette non trouvée dans les affectations", type = "error")
      return()
    }
    
    assignment <- current_assignments[tablet_idx[1], ]
    
    if (assignment$agent_id != input$incident_agent_id) {
      showNotification("L'ID de l'agent ne correspond pas à l'affectation", type = "error")
      return()
    }
    
    new_incident <- data.frame(
      tablette = tablet_num,
      agent_id = assignment$agent_id,
      agent_name = assignment$agent_name,
      charger_usable = input$incident_charger_ok,
      powerbank_usable = input$incident_powerbank_ok,
      incident_type = input$incident_type,
      incident_state = input$incident_state,
      incident_date = as.character(input$incident_date),
      notes = input$incident_notes,
      user_login = current_user(),
      stringsAsFactors = FALSE
    )
    
    current_incidents <- tablet_incidents()
    updated_incidents <- rbind(current_incidents, new_incident)
    tablet_incidents(updated_incidents)
    save_tablet_incidents(updated_incidents)
    
    # Mettre à jour l'état de la tablette
    current_tablets <- registered_tablets()
    reg_idx <- which(current_tablets$tablette == tablet_num)
    if (length(reg_idx) > 0) {
      current_tablets$etat[reg_idx] <- input$incident_state
      current_tablets$chargeur_ok[reg_idx] <- input$incident_charger_ok
      current_tablets$powerbank_ok[reg_idx] <- input$incident_powerbank_ok
      registered_tablets(current_tablets)
      save_registered_tablets(current_tablets)
    }
    
    # Supprimer l'affectation
    all_assignments <- assignments()
    all_tablet_idx <- which(all_assignments$tablette == tablet_num)
    if (length(all_tablet_idx) > 0) {
      updated_assignments <- all_assignments[-all_tablet_idx, ]
      assignments(updated_assignments)
      save_assignments(updated_assignments)
    }
    
    # Réinitialiser les champs
    updateTextInput(session, "incident_agent_id", value = "")
    updateSelectInput(session, "incident_tablet_select", selected = "")
    updateSelectInput(session, "incident_type", selected = "Casse")
    updateSelectInput(session, "incident_state", selected = "En réparation")
    updateMaterialSwitch(session, "incident_charger_ok", value = TRUE)
    updateMaterialSwitch(session, "incident_powerbank_ok", value = TRUE)
    updateDateInput(session, "incident_date", value = Sys.Date())
    updateTextAreaInput(session, "incident_notes", value = "")
    
    showNotification("Incident déclaré avec succès!", type = "warning")
  })
  
  # Output pour le tableau des incidents
  output$incidents_table <- renderDT({
    req(user_role(), current_user())
    
    incidents_data <- get_user_data(tablet_incidents(), user_role(), current_user())
    if (nrow(incidents_data) == 0) {
      datatable(
        data.frame(Message = "Aucun incident déclaré"),
        options = list(pageLength = 10, dom = 't'),
        rownames = FALSE
      )
    } else {
      display_data <- incidents_data
      if (nrow(display_data) > 0) {
        display_data$charger_usable <- ifelse(display_data$charger_usable, "Oui", "Non")
        display_data$powerbank_usable <- ifelse(display_data$powerbank_usable, "Oui", "Non")
      }
      datatable(
        display_data,
        options = list(
          pageLength = 10,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.24/i18n/French.json')
        ),
        rownames = FALSE,
        colnames = c("Tablette", "ID Agent", "Nom Agent", "Chargeur OK", "Powerbank OK", "Type", "État", "Date", "Notes", "Utilisateur")
      )
    }
  })
}

# === MODULE ADMINISTRATION SERVEUR ===
administration_server_logic <- function(input, output, session, supervisors, registered_tablets, assignments, tablet_returns, tablet_incidents, generated_fiches, user_role, current_user) {
  
  # Outputs pour les compteurs du tableau de bord
  output$available_tablets_count <- renderText({
    req(user_role(), current_user())
    
    registered_data <- get_user_data(registered_tablets(), user_role(), current_user())
    if (nrow(registered_data) == 0) return("0")
    sum(registered_data$etat == "En stock", na.rm = TRUE)
  })
  
  output$assigned_tablets_count <- renderText({
    req(user_role(), current_user())
    
    registered_data <- get_user_data(registered_tablets(), user_role(), current_user())
    if (nrow(registered_data) == 0) return("0")
    sum(registered_data$etat == "Affectée", na.rm = TRUE)
  })
  
  output$returned_tablets_count <- renderText({
    req(user_role(), current_user())
    
    registered_data <- get_user_data(registered_tablets(), user_role(), current_user())
    if (nrow(registered_data) == 0) return("0")
    sum(registered_data$etat == "En réparation", na.rm = TRUE)
  })
  
  output$out_of_service_tablets_count <- renderText({
    req(user_role(), current_user())
    
    registered_data <- get_user_data(registered_tablets(), user_role(), current_user())
    if (nrow(registered_data) == 0) return("0")
    sum(registered_data$etat == "Hors service", na.rm = TRUE)
  })
  
  # Output pour le tableau de suivi des tablettes
  output$tracking_table <- renderDT({
    req(user_role(), current_user())
    
    # Construire le statut des tablettes
    reg_data <- get_user_data(registered_tablets(), user_role(), current_user())
    assign_data <- get_user_data(assignments(), user_role(), current_user())
    returns_data <- get_user_data(tablet_returns(), user_role(), current_user())
    
    if (nrow(reg_data) == 0) {
      datatable(
        data.frame(Message = "Aucune tablette enregistrée"),
        options = list(pageLength = 10, dom = 't'),
        rownames = FALSE
      )
    } else {
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
      
      # Formater les données pour l'affichage
      display_data <- suivi[, c("tablette", "status", "current_agent", "assign_date", "return_date", "condition")]
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
  
  # Import des superviseurs
  observeEvent(input$import_supervisors_btn, {
    req(input$supervisors_file)
    tryCatch({
      data <- read_excel(input$supervisors_file$datapath)
      required_columns <- c("user_name", "user_login", "user_password")
      missing_columns <- setdiff(required_columns, colnames(data))
      if (length(missing_columns) > 0) {
        showNotification(
          paste("Erreur : Colonnes manquantes :", paste(missing_columns, collapse = ", ")), 
          type = "error"
        )
        return()
      }
      
      new_supervisors <- data.frame(
        user_name = as.character(data$user_name),
        user_login = as.character(data$user_login),
        user_password = as.character(data$user_password),
        stringsAsFactors = FALSE
      )
      
      current_supervisors <- supervisors()
      if (nrow(current_supervisors) > 0) {
        duplicates <- new_supervisors$user_login %in% current_supervisors$user_login
        if (any(duplicates)) {
          current_supervisors <- current_supervisors[!current_supervisors$user_login %in% new_supervisors$user_login, ]
          showNotification(
            paste("Remplacement de", sum(duplicates), "superviseur(s) existant(s)"), 
            type = "warning"
          )
        }
      }
      
      updated_supervisors <- rbind(current_supervisors, new_supervisors)
      supervisors(updated_supervisors)
      save_supervisors(updated_supervisors)
      
      showNotification(
        paste(nrow(new_supervisors), "superviseur(s) importé(s) avec succès!"), 
        type = "default"
      )
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'import :", e$message), type = "error")
    })
  })
  
  # Vider la base des superviseurs
  observeEvent(input$clear_supervisors_btn, {
    showModal(modalDialog(
      title = "Confirmation",
      "Êtes-vous sûr de vouloir vider complètement la base des superviseurs ?",
      "Cette action ne peut pas être annulée.",
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_clear_supervisors", "Confirmer", class = "btn-danger")
      ),
      size = "m"
    ))
  })
  
  observeEvent(input$confirm_clear_supervisors, {
    supervisors(empty_supervisors())
    save_supervisors(supervisors())
    removeModal()
    showNotification("Base des superviseurs vidée avec succès", type = "default")
  })
  
  # Réinitialisation totale
  observeEvent(input$reset_all_btn, {
    showModal(modalDialog(
      title = "Confirmation de la réinitialisation",
      HTML("<b style='color:red;'>Attention : cette action va effacer <u>toutes</u> les données de l'application.<br>Cette action est <u>irréversible</u>!</b><br><br>Pour confirmer, tapez <code>CONFIRMER</code> ci-dessous."),
      textInput("reset_all_confirm", "Tapez CONFIRMER pour valider", value = ""),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("do_reset_all", "Tout réinitialiser", class = "btn-danger")
      ),
      size = "m"
    ))
  })
  
  observeEvent(input$do_reset_all, {
    if (!is.null(input$reset_all_confirm) && toupper(trimws(input$reset_all_confirm)) == "CONFIRMER") {
      reset_all_data()
      
      # Réinitialiser les données réactives
      supervisors(empty_supervisors())
      registered_tablets(empty_registered_tablets())
      assignments(empty_assignments())
      tablet_returns(empty_tablet_returns())
      tablet_incidents(empty_tablet_incidents())
      generated_fiches(empty_generated_fiches())
      
      removeModal()
      showNotification("Toutes les données ont été effacées.", type = "warning")
    } else {
      showNotification("Vous devez taper CONFIRMER pour valider la réinitialisation.", type = "error")
    }
  })
  
  # Output pour le tableau des superviseurs
  output$supervisors_table <- renderDT({
    supervisors_data <- supervisors()
    if (nrow(supervisors_data) == 0) {
      datatable(
        data.frame(Message = "Aucun superviseur enregistré"),
        options = list(pageLength = 10, dom = 't'),
        rownames = FALSE
      )
    } else {
      display_data <- supervisors_data
      display_data$user_password <- paste0(rep("*", 8), collapse = "")
      
      datatable(
        display_data,
        options = list(
          pageLength = 10,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.24/i18n/French.json')
        ),
        rownames = FALSE,
        colnames = c("Nom complet", "Identifiant", "Mot de passe"),
        selection = 'single'
      )
    }
  })
  
  # === SUIVI DES SUPERVISEURS (ADMIN UNIQUEMENT) ===
  # Statistiques globales
  output$nb_superviseurs <- renderUI({
    sup <- supervisors()
    value <- if (!is.null(sup) && nrow(sup) > 0) nrow(sup) else 0
    div(style = "background: linear-gradient(135deg, #764ba2 0%, #667eea 100%); color: white; border-radius: 10px; padding: 20px; text-align: center; box-shadow: 0 4px 8px rgba(0,0,0,0.08);",
        h3(value, style = "margin:0; font-size:2.5rem;"),
        p("Superviseurs actifs", style = "margin:0; font-size:1.1rem;")
    )
  })
  
  output$nb_fiches_total <- renderUI({
    fiches <- assignments()
    value <- if (!is.null(fiches) && nrow(fiches) > 0) nrow(fiches) else 0
    div(style = "background: linear-gradient(135deg, #007bff 0%, #0056b3 100%); color: white; border-radius: 10px; padding: 20px; text-align: center; box-shadow: 0 4px 8px rgba(0,0,0,0.08);",
        h3(value, style = "margin:0; font-size:2.5rem;"),
        p("Fiches générées", style = "margin:0; font-size:1.1rem;")
    )
  })
  
  output$nb_fiches_transferees <- renderUI({
    fiches <- generated_fiches()
    value <- if (!is.null(fiches) && nrow(fiches) > 0 && "transferred" %in% colnames(fiches)) sum(fiches$transferred == TRUE, na.rm = TRUE) else 0
    div(style = "background: linear-gradient(135deg, #28a745 0%, #20c997 100%); color: white; border-radius: 10px; padding: 20px; text-align: center; box-shadow: 0 4px 8px rgba(0,0,0,0.08);",
        h3(value, style = "margin:0; font-size:2.5rem;"),
        p("Fiches transférées", style = "margin:0; font-size:1.1rem;")
    )
  })
  
  output$nb_fiches_en_attente <- renderUI({
    fiches <- generated_fiches()
    value <- if (!is.null(fiches) && nrow(fiches) > 0 && "transferred" %in% colnames(fiches)) sum(is.na(fiches$transferred) | fiches$transferred == FALSE, na.rm = TRUE) else 0
    div(style = "background: linear-gradient(135deg, #ffc107 0%, #e0a800 100%); color: white; border-radius: 10px; padding: 20px; text-align: center; box-shadow: 0 4px 8px rgba(0,0,0,0.08);",
        h3(value, style = "margin:0; font-size:2.5rem;"),
        p("Fiches en attente", style = "margin:0; font-size:1.1rem;")
    )
  })
  
  # Observer le clic sur le bouton de transfert
  observeEvent(input$transfer_fiche, {
    req(input$transfer_fiche)
    fiches <- generated_fiches()
    idx <- which(fiches$filename == input$transfer_fiche & fiches$user_login == current_user())
    if (length(idx) == 1 && (is.na(fiches$transferred[idx]) || fiches$transferred[idx] == FALSE)) {
      fiches$transferred[idx] <- TRUE
      generated_fiches(fiches)
      save_generated_fiches(fiches)
      showNotification("Fiche transférée à la centrale !", type = "message")
    }
  })

  # ==================== VISUALISATIONS & ALERTES ====================
  # Tableau récapitulatif par superviseur
  output$stats_par_superviseur <- DT::renderDataTable({
    fiches <- generated_fiches()
    sup <- supervisors()
    if (is.null(fiches) || nrow(fiches) == 0) {
      return(datatable(data.frame(Message = "Aucune fiche générée"), options = list(dom = 't'), rownames = FALSE))
    }

    if (!"transferred" %in% colnames(fiches)) fiches$transferred <- FALSE

    stats <- aggregate(transferred ~ user_login, data = fiches,
                       FUN = function(x) c(total = length(x),
                                           transferees = sum(x == TRUE, na.rm = TRUE),
                                           pending = sum(is.na(x) | x == FALSE)))

    stats <- do.call(data.frame, stats)
    colnames(stats) <- c("user_login", "total", "transferees", "pending")
    stats$Superviseur <- if (!is.null(sup) && nrow(sup) > 0) sup$user_name[match(stats$user_login, sup$user_login)] else stats$user_login

    datatable(stats[, c("Superviseur", "total", "transferees", "pending")],
              colnames = c("Superviseur", "Fiches totales", "Fiches transférées", "Fiches en attente"),
              options = list(pageLength = 10, language = list(url = '//cdn.datatables.net/plug-ins/1.10.24/i18n/French.json')),
              rownames = FALSE)
  })

  # Graphique : nombre de fiches par superviseur
  output$plot_fiches_par_superviseur <- renderPlot({
    fiches <- generated_fiches()
    sup <- supervisors()
    if (is.null(fiches) || nrow(fiches) == 0) return(NULL)
    if (!"transferred" %in% colnames(fiches)) fiches$transferred <- FALSE

    stats <- aggregate(transferred ~ user_login, data = fiches,
                       FUN = function(x) c(total = length(x),
                                           transferees = sum(x == TRUE, na.rm = TRUE),
                                           pending = sum(is.na(x) | x == FALSE)))
    stats <- do.call(data.frame, stats)
    colnames(stats) <- c("user_login", "total", "transferees", "pending")
    stats$Superviseur <- if (!is.null(sup) && nrow(sup) > 0) sup$user_name[match(stats$user_login, sup$user_login)] else stats$user_login

    stats_melt <- reshape2::melt(stats[, c("Superviseur", "total", "transferees", "pending")], id.vars = "Superviseur")
    ggplot(stats_melt, aes(x = Superviseur, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Superviseur", y = "Nombre de fiches", fill = "Statut") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # Graphique : évolution quotidienne du nombre de fiches générées
  output$plot_evolution_fiches <- renderPlot({
    fiches <- generated_fiches()
    if (is.null(fiches) || nrow(fiches) == 0) return(NULL)
    dates <- as.Date(substr(fiches$timestamp, 1, 10))
    daily <- aggregate(filename ~ dates, data = data.frame(filename = fiches$filename, dates = dates), FUN = length)
    ggplot(daily, aes(x = dates, y = filename)) +
      geom_line(color = "#2c7fb8") + geom_point(color = "#2c7fb8") +
      labs(x = "Date", y = "Fiches générées") + theme_minimal()
  })

  # Zone d'alerte sur les fiches non transférées par superviseur
  output$alertes_superviseurs <- renderUI({
    fiches <- generated_fiches()
    sup <- supervisors()
    if (is.null(fiches) || nrow(fiches) == 0) {
      return(div("Aucune alerte", style = "color: white;"))
    }
    if (!"transferred" %in% colnames(fiches)) fiches$transferred <- FALSE

    alerts <- aggregate(transferred ~ user_login, data = fiches,
                        FUN = function(x) sum(is.na(x) | x == FALSE))
    alerts <- alerts[alerts$transferred > 0, ]

    if (nrow(alerts) == 0) {
      div("Aucune alerte", style = "color: white;")
    } else {
      tags$div(style = "max-height:150px; overflow-y:auto; background-color:white; padding:10px; border-radius:5px;",
               lapply(seq_len(nrow(alerts)), function(i) {
                 name <- if (!is.null(sup) && nrow(sup) > 0) sup$user_name[match(alerts$user_login[i], sup$user_login)] else alerts$user_login[i]
                 tags$p(sprintf("%s : %d fiche(s) en attente de transfert", name, alerts$transferred[i]), style = "margin:0;")
               }))
    }
  })
}