# === MODULE GÉNÉRATION DE FICHES COMPLET ===
# Fonctions pour la génération de documents Word

# Fonction pour analyser les placeholders dans le template
analyze_document_variables <- function(template_path) {
  if (!file.exists(template_path)) {
    warning("Template file not found:", template_path)
    return(character(0))
  }
  
  tryCatch({
    doc <- officer::read_docx(template_path)
    doc_text <- officer::docx_summary(doc)$text
    full_text <- paste(doc_text, collapse = " ")
    placeholder_pattern <- "\\{\\{[^}]+\\}\\}"
    placeholders <- regmatches(full_text, gregexpr(placeholder_pattern, full_text))[[1]]
    unique(placeholders)
  }, error = function(e) {
    warning("Error analyzing document:", e$message)
    return(character(0))
  })
}

# Fonction principale de génération de fiche
generate_affectation_fiche <- function(assign_data, template = "Fiche_Affectation_Materiel.docx") {
  template_paths <- c(
    template,
    file.path(getwd(), template),
    file.path(dirname(getwd()), template),
    file.path("www", template)
  )
  template_path <- template_paths[file.exists(template_paths)][1]
  if (is.na(template_path)) {
    stop("Template de fiche non trouvé : ", template)
  }
  
  tryCatch({
    doc <- officer::read_docx(template_path)
    
    # Mapping sécurisé des placeholders
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
      doc <- officer::body_replace_all_text(doc, placeholder, replacements[[placeholder]], fixed = TRUE)
    }
    
    # Générer le nom de fichier sécurisé
    safe_agent_name <- gsub("[^a-zA-Z0-9]", "_", assign_data$agent_name)
    safe_tablet_name <- gsub("[^a-zA-Z0-9]", "_", assign_data$tablette)
    filename <- paste0("Fiche_", safe_agent_name, "_", safe_tablet_name, "_", Sys.Date(), ".docx")
    
    # Créer le dossier fiches s'il n'existe pas
    if (!dir.exists("fiches_generees")) dir.create("fiches_generees")
    filepath <- file.path("fiches_generees", filename)
    
    print(doc, target = filepath)
    return(list(filename = filename, filepath = filepath, data = assign_data))
    
  }, error = function(e) {
    stop(paste("Erreur lors de la génération de la fiche :", e$message))
  })
}