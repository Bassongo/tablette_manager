# Functions related to generation of word files

analyze_document_variables <- function(template_path) {
  doc <- read_docx(template_path)
  doc_text <- docx_summary(doc)$text
  full_text <- paste(doc_text, collapse = " ")
  placeholders <- regmatches(full_text, gregexpr("\{\{[^}]+\}\}", full_text))[[1]]
  unique(placeholders)
}

# main generation function
generate_affectation_fiche <- function(assign_data, template="Fiche_Affectation_Materiel.docx") {
  tryCatch({
    doc <- read_docx(template)
    replacements <- list(
      "{{groupe}}" = ifelse(is.na(assign_data$agent_group) || assign_data$agent_group=="","N/A",assign_data$agent_group),
      "{{agent}}" = ifelse(is.na(assign_data$agent_name) || assign_data$agent_name=="","N/A",assign_data$agent_name),
      "{{fonction}}" = ifelse(is.na(assign_data$agent_function) || assign_data$agent_function=="","N/A",assign_data$agent_function),
      "{{Téléphone}}" = ifelse(is.na(assign_data$agent_phone) || assign_data$agent_phone=="","N/A",assign_data$agent_phone),
      "{{tablette}}" = ifelse(is.na(assign_data$tablette) || assign_data$tablette=="","N/A",assign_data$tablette),
      "{{chargeur}}" = ifelse(is.na(assign_data$chargeur) || assign_data$chargeur=="","N/A",assign_data$chargeur),
      "{{batterie}}" = ifelse(is.na(assign_data$powerbank) || assign_data$powerbank=="","N/A", ifelse(assign_data$powerbank, "Oui", "Non")),
      "{{superviseur}}" = ifelse(is.na(assign_data$supervisor_name) || assign_data$supervisor_name=="","N/A",assign_data$supervisor_name),
      "{{adresse}}" = ifelse(is.na(assign_data$supervisor_num) || assign_data$supervisor_num=="","N/A",assign_data$supervisor_num),
      "{{date}}" = ifelse(is.na(assign_data$assign_date) || assign_data$assign_date=="","N/A",assign_data$assign_date)
    )
    for (p in names(replacements)) {
      doc <- body_replace_all_text(doc, p, replacements[[p]], fixed=TRUE)
    }
    safe_name <- gsub("[^a-zA-Z0-9]","_", paste0(assign_data$agent_name,"_",assign_data$tablette))
    filename <- file.path("fiches_generees", paste0("Fiche_", safe_name, "_", Sys.Date(), ".docx"))
    print(doc, target = filename)
    assign_data$filename <- filename
    assign_data$transferred <- FALSE
    list(filename=filename, data=assign_data)
  }, error=function(e){
    stop(paste("Erreur g\u00e9n\u00e9ration fiche:", e$message))
  })
}

