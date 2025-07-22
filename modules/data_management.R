# === GESTION DES DONNÉES - MODULE COMPLET ===
# Toutes les fonctions de persistance et structures par défaut

# Créer les dossiers nécessaires
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("fiches_generees")) dir.create("fiches_generees")

# === FONCTIONS GÉNÉRIQUES ===
load_data <- function(name) {
  path <- file.path("data", paste0(name, ".rds"))
  if (file.exists(path)) {
    readRDS(path)
  } else {
    # Retourner une structure par défaut selon le type
    switch(name,
           "supervisors" = empty_supervisors(),
           "registered_tablets" = empty_registered_tablets(),
           "assignments" = empty_assignments(),
           "tablet_returns" = empty_tablet_returns(),
           "tablet_incidents" = empty_tablet_incidents(),
           "generated_fiches" = empty_generated_fiches(),
           data.frame()
    )
  }
}

save_data <- function(df, name) {
  saveRDS(df, file.path("data", paste0(name, ".rds")))
}

# === STRUCTURES PAR DÉFAUT COMPLÈTES ===
empty_supervisors <- function() {
  data.frame(
    user_name = character(),
    user_login = character(),
    user_password = character(),
    stringsAsFactors = FALSE
  )
}

empty_registered_tablets <- function() {
  data.frame(
    tablette = character(),
    chargeur = character(),
    powerbank = logical(),
    chargeur_ok = logical(),
    powerbank_ok = logical(),
    registration_date = character(),
    etat = character(),
    user_login = character(),
    stringsAsFactors = FALSE
  )
}

empty_assignments <- function() {
  data.frame(
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
    user_login = character(),
    stringsAsFactors = FALSE
  )
}

empty_tablet_returns <- function() {
  data.frame(
    tablette = character(),
    agent_id = character(),
    agent_name = character(),
    charger_retourne = character(),
    powerbank_retourne = logical(),
    return_reason = character(),
    return_condition = character(),
    return_date = character(),
    return_notes = character(),
    user_login = character(),
    stringsAsFactors = FALSE
  )
}

empty_tablet_incidents <- function() {
  data.frame(
    tablette = character(),
    agent_id = character(),
    agent_name = character(),
    charger_usable = logical(),
    powerbank_usable = logical(),
    incident_type = character(),
    incident_state = character(),
    incident_date = character(),
    notes = character(),
    user_login = character(),
    stringsAsFactors = FALSE
  )
}

empty_generated_fiches <- function() {
  data.frame(
    filename = character(),
    agent_name = character(),
    tablette = character(),
    timestamp = character(),
    user_login = character(),
    transferred = logical(),  # COLONNE MANQUANTE DANS L'ORIGINAL
    stringsAsFactors = FALSE
  )
}

# === FONCTIONS DE CHARGEMENT/SAUVEGARDE SPÉCIFIQUES ===
load_supervisors <- function() load_data("supervisors")
save_supervisors <- function(data) save_data(data, "supervisors")

load_registered_tablets <- function() load_data("registered_tablets")
save_registered_tablets <- function(data) save_data(data, "registered_tablets")

load_assignments <- function() load_data("assignments")
save_assignments <- function(data) save_data(data, "assignments")

load_tablet_returns <- function() load_data("tablet_returns")
save_tablet_returns <- function(data) save_data(data, "tablet_returns")

load_tablet_incidents <- function() load_data("tablet_incidents")
save_tablet_incidents <- function(data) save_data(data, "tablet_incidents")

load_generated_fiches <- function() load_data("generated_fiches")
save_generated_fiches <- function(data) save_data(data, "generated_fiches")

# === GESTION COHÉRENTE DES PERMISSIONS ===
get_user_data <- function(data, user_role, current_user) {
  if (is.null(user_role) || is.null(current_user)) {
    return(data.frame())
  }
  
  if (user_role == "admin") {
    return(data)
  } else if (user_role == "supervisor") {
    if (nrow(data) == 0 || !"user_login" %in% colnames(data)) {
      return(data)
    }
    return(data[data$user_login == current_user, , drop = FALSE])
  }
  
  return(data.frame())
}

# === FONCTION DE RÉINITIALISATION COMPLÈTE ===
reset_all_data <- function() {
  # Supprimer tous les fichiers RDS
  files_to_remove <- c("supervisors.rds", "registered_tablets.rds", 
                       "assignments.rds", "tablet_returns.rds", 
                       "tablet_incidents.rds", "generated_fiches.rds")
  
  for (file in files_to_remove) {
    file_path <- file.path("data", file)
    if (file.exists(file_path)) {
      file.remove(file_path)
    }
  }
  
  # Supprimer les fichiers Word générés
  docx_files <- list.files(pattern = "^Fiche_.*\\.docx$")
  if (length(docx_files) > 0) {
    file.remove(docx_files)
  }
  
  # Supprimer le dossier fiches_generees s'il existe
  if (dir.exists("fiches_generees")) {
    unlink("fiches_generees", recursive = TRUE)
    dir.create("fiches_generees")
  }
}