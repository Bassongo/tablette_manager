# deploiement.R — Déploiement complet de l’application Shiny tablette_manager

cat("\n=== DÉPLOIEMENT DE L'APPLICATION SHINY ===\n")

# 1. Vérification du dossier de travail
projet_dir <- getwd()
cat("Répertoire courant :", projet_dir, "\n")

# 2. Vérification du dossier data/
if (!dir.exists("data")) {
  dir.create("data")
  cat("Dossier 'data/' créé.\n")
} else {
  cat("Dossier 'data/' déjà présent.\n")
}

# 3. Vérification des fichiers .rds nécessaires
fichiers_rds <- c(
  "supervisors.rds", "registered_tablets.rds", "assignments.rds",
  "tablet_returns.rds", "tablet_incidents.rds", "generated_fiches.rds"
)
for (f in fichiers_rds) {
  path <- file.path("data", f)
  if (!file.exists(path)) {
    saveRDS(data.frame(), path)
    cat("Fichier créé (vide) :", path, "\n")
  } else {
    cat("Fichier trouvé :", path, "\n")
  }
}

# 4. Vérification du .Rbuildignore
if (file.exists(".Rbuildignore")) {
  cat(".Rbuildignore présent.\n")
} else {
  cat("⚠️  .Rbuildignore manquant !\n")
}

# 5. Liste des fichiers > 10 Mo à surveiller
cat("\nFichiers > 10 Mo dans le projet :\n")
big_files <- list.files(projet_dir, recursive = TRUE, full.names = TRUE)
big_files <- big_files[file.info(big_files)$size > 10*1024*1024]
if (length(big_files) > 0) {
  print(big_files)
  cat("⚠️  Supprime ou exclue ces fichiers du déploiement si inutiles !\n")
} else {
  cat("Aucun fichier volumineux détecté.\n")
}

