# === SERVEUR PRINCIPAL COMPLET ===
# Protection : charger la gestion des données si besoin
if (!exists("load_supervisors")) source("modules/data_management.R")
# Protection : charger la logique serveur modulaire si besoin
if (!exists("register_server_logic")) source("modules/server_modules.R")
# Serveur principal avec toute la logique

server <- function(input, output, session) {
  
  # === DONNÉES RÉACTIVES ===
  supervisors <- reactiveVal(load_supervisors())
  user_role <- reactiveVal(NULL)
  current_user <- reactiveVal(NULL)
  registered_tablets <- reactiveVal(load_registered_tablets())
  assignments <- reactiveVal(load_assignments())
  tablet_returns <- reactiveVal(load_tablet_returns())
  tablet_incidents <- reactiveVal(load_tablet_incidents())
  generated_fiches <- reactiveVal(load_generated_fiches())
  selected_fiche_index <- reactiveVal(NULL)
  
  # Synchroniser le rôle utilisateur global pour l'UI
  observe({
    if (exists("global_user_role", envir = .GlobalEnv)) {
      global_user_role <<- user_role
    }
  })
  
  # === GESTION DE LA CONNEXION ===
  observeEvent(TRUE, {
  showModal(login_ui())
  }, once = TRUE)

  observeEvent(input$login_btn, {
    req(input$login_user, input$login_pass)
    if (input$login_user == "admin" && input$login_pass == "admin") {
      user_role("admin")
      current_user("admin")
    } else {
      sup <- supervisors()
      idx <- which(sup$user_login == input$login_user & sup$user_password == input$login_pass)
      if (length(idx) == 1) {
        user_role("supervisor")
        current_user(input$login_user)
      } else {
        showNotification("Identifiants invalides", type = "error")
        return()
      }
    }
    removeModal()
    
    if (user_role() == "supervisor") {
      hideTab("navbar", "Administration")
    } else {
      showTab("navbar", "Administration")
    }
    
    # Ajouter le bouton de déconnexion
    removeUI(selector = "#logout_button", immediate = TRUE)
    insertUI(
      selector = ".navbar-nav",
      where = "beforeEnd",
      ui = tags$li(
        id = "logout_button",
        class = "nav-item",
        tags$a(
          class = "nav-link",
          href = "#",
          icon("sign-out-alt"),
          "Déconnexion",
          onclick = "Shiny.setInputValue('logout_click', Math.random())"
        )
      )
    )
    
    updateNavbarPage(session, "navbar", selected = "Accueil")
  })
  
  # Gestion de la déconnexion
  observeEvent(input$logout_click, {
    # Sauvegarder toutes les données avant déconnexion
    save_registered_tablets(registered_tablets())
    save_assignments(assignments())
    save_tablet_returns(tablet_returns())
    save_tablet_incidents(tablet_incidents())
    save_generated_fiches(generated_fiches())
    save_supervisors(supervisors())
    
    # Réinitialiser l'utilisateur et l'UI
    user_role(NULL)
    current_user(NULL)
    removeUI(selector = "#logout_button", immediate = TRUE)
    showModal(login_ui())
  })
  
  # === MESSAGE DE BIENVENUE ===
  output$welcome_message <- renderUI({
    req(current_user(), user_role())
    if (user_role() == "admin") {
      HTML(paste0("<h2 style='color: white;'>Bienvenue Administrateur</h2>",
                  "<p style='color: white; font-size: 1.2em;'>Vous êtes connecté en tant qu'administrateur. Vous voyez toutes les actions de tous les utilisateurs.</p>"))
    } else if (user_role() == "supervisor") {
      sup <- supervisors()
      user_idx <- which(sup$user_login == current_user())
      user_display_name <- if (length(user_idx) > 0) sup$user_name[user_idx] else current_user()
      HTML(paste0("<h2 style='color: white;'>Bienvenue ", user_display_name, "</h2>",
                  "<p style='color: white; font-size: 1.2em;'>Vous êtes connecté en tant que superviseur. Vous ne voyez que vos propres actions.</p>"))
    }
  })
  
  # === OBSERVATEURS POUR LES BOUTONS DE SCAN QR ===
  observeEvent(input$scan_tablet_btn, {
    showModal(modalDialog(
      title = "Scanner QR Tablette",
      tags$div(id = "qr-reader"),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
    session$sendCustomMessage('start-scan', list(target = 'scanned_tablet_code'))
  })
  
  observeEvent(input$scan_charger_btn, {
    showModal(modalDialog(
      title = "Scanner QR Chargeur",
      tags$div(id = "qr-reader"),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
    session$sendCustomMessage('start-scan', list(target = 'scanned_charger_code'))
  })
  
  observeEvent(input$scanned_tablet_code, {
    updateTextInput(session, 'reg_tab_num_qr', value = input$scanned_tablet_code)
  })
  
  observeEvent(input$scanned_charger_code, {
    updateTextInput(session, 'reg_charger_num_qr', value = input$scanned_charger_code)
  })
  
  # === OBSERVATEURS POUR MISE À JOUR DES CHOIX ===
  # Observateur pour mettre à jour les choix de tablettes affectées
  observe({
    current_assignments <- get_user_data(assignments(), user_role(), current_user())
    if (nrow(current_assignments) > 0) {
      choices <- paste(current_assignments$tablette, "-", current_assignments$agent_name)
      updateSelectInput(session, "return_tablet_select", choices = choices)
      updateSelectInput(session, "incident_tablet_select", choices = choices)
    } else {
      updateSelectInput(session, "return_tablet_select", choices = "Aucune tablette affectée")
      updateSelectInput(session, "incident_tablet_select", choices = "Aucune tablette affectée")
    }
  })
  
  # Observateur pour mettre à jour le powerbank selon la tablette sélectionnée
  observeEvent(input$return_tablet_select, {
    req(input$return_tablet_select)
    
    current_assignments <- get_user_data(assignments(), user_role(), current_user())
    if (nrow(current_assignments) > 0) {
      tablet_num <- strsplit(input$return_tablet_select, " - ")[[1]][1]
      tablet_idx <- which(current_assignments$tablette == tablet_num)
      
      if (length(tablet_idx) > 0) {
        assignment <- current_assignments[tablet_idx[1], ]
        updateMaterialSwitch(session, "return_has_powerbank", value = assignment$powerbank)
        
        if (assignment$powerbank) {
          shinyjs::enable("return_has_powerbank")
        } else {
          shinyjs::disable("return_has_powerbank")
        }
        
        updateTextInput(session, "return_charger_num", value = assignment$chargeur)
      }
    }
  })
  
  observeEvent(input$incident_tablet_select, {
    req(input$incident_tablet_select)
    
    current_assignments <- get_user_data(assignments(), user_role(), current_user())
    if (nrow(current_assignments) > 0) {
      tablet_num <- strsplit(input$incident_tablet_select, " - ")[[1]][1]
      tablet_idx <- which(current_assignments$tablette == tablet_num)
      
      if (length(tablet_idx) > 0) {
        assignment <- current_assignments[tablet_idx[1], ]
        updateMaterialSwitch(session, "incident_powerbank_ok", value = assignment$powerbank)
        
        if (assignment$powerbank) {
          shinyjs::enable("incident_powerbank_ok")
        } else {
          shinyjs::disable("incident_powerbank_ok")
        }
      }
    }
  })
  
  # Output pour afficher l'information sur le powerbank
  output$powerbank_info <- renderText({
    req(input$return_tablet_select)
    
    current_assignments <- get_user_data(assignments(), user_role(), current_user())
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
  
  # === OUTPUT POUR SAVOIR SI L'UTILISATEUR EST ADMIN ===
  output$isAdmin <- reactive({ 
    user_role() == "admin" 
  })
  outputOptions(output, "isAdmin", suspendWhenHidden = FALSE)
  
  # Masquer/afficher l'onglet Administration selon le rôle
  observe({
    if (!is.null(user_role()) && user_role() == "admin") {
      showTab("navbar", "Administration")
    } else {
      hideTab("navbar", "Administration")
    }
  })
  
  # === APPEL DES MODULES SERVEUR ===
  # Module Enregistrement
  register_server_logic(input, output, session, registered_tablets, user_role, current_user)
  
  # Module Affectation
  assignment_server_logic(input, output, session, assignments, registered_tablets, user_role, current_user)
  
  # Module Génération de fiches
  fiche_server_logic(input, output, session, assignments, generated_fiches, user_role, current_user, selected_fiche_index)
  
  # Module Retour
  return_server_logic(input, output, session, tablet_returns, assignments, registered_tablets, user_role, current_user)
  
  # Module Incident
  incident_server_logic(input, output, session, tablet_incidents, assignments, registered_tablets, user_role, current_user)
  
  # Module Administration
  administration_server_logic(input, output, session, supervisors, registered_tablets, assignments, tablet_returns, tablet_incidents, generated_fiches, user_role, current_user)
  
  # === OBSERVATEURS POUR LA CONFIRMATION DU RETOUR AVEC CHARGEUR MANQUANT ===
  # Fonction pour traiter le retour de tablette
  process_tablet_return <- function(assignment, input_data) {
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
    
    current_returns <- tablet_returns()
    updated_returns <- rbind(current_returns, new_return)
    tablet_returns(updated_returns)
    save_tablet_returns(updated_returns)
    
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
    
    all_assignments <- assignments()
    all_assignment_idx <- which(all_assignments$tablette == assignment$tablette)
    if (length(all_assignment_idx) > 0) {
      updated_assignments <- all_assignments[-all_assignment_idx, ]
      assignments(updated_assignments)
      save_assignments(updated_assignments)
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
  
  # Observateur pour la confirmation du retour avec chargeur manquant
  observeEvent(input$confirm_return, {
    req(input$return_agent_id)
    
    current_assignments <- get_user_data(assignments(), user_role(), current_user())
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
}
