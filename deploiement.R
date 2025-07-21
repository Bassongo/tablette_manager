# ===== DÉPLOIEMENT APPLICATION GESTION DES TABLETTES =====

# 1. Charger les bibliothèques nécessaires
library(rsconnect)
library(RSQLite)
library(DBI)

# 2. Vérifier que nous sommes dans le bon répertoire
cat("Répertoire actuel:", getwd(), "\n")
cat("Fichiers présents:", list.files(), "\n")

# 3. Vérifier la configuration shinyapps.io
cat("\n=== VÉRIFICATION CONFIGURATION ===\n")
accounts <- rsconnect::accounts()
if (length(accounts) > 0) {
  cat("✅ Compte(s) configuré(s):\n")
  print(accounts)
} else {
  cat("❌ Aucun compte configuré\n")
  cat("Utilisez: rsconnect::setAccountInfo() pour configurer\n")
}

# 4. Test rapide de l'application
cat("\n=== TEST RAPIDE ===\n")
tryCatch({
  source("app.R", echo = FALSE)
  cat("✅ Application chargée sans erreur\n")
}, error = function(e) {
  cat("❌ Erreur lors du chargement:", e$message, "\n")
  stop("Corriger l'erreur avant le déploiement")
})

# 5. Déploiement avec rsconnect::deployApp
cat("\n=== DÉPLOIEMENT ===\n")
tryCatch({
  cat("Déploiement en cours...\n")
  
  # Déployer avec un nom spécifique
  rsconnect::deployApp(
    appName = "E-tablette_manager",
    appTitle = "Gestion des Tablettes",
    appFiles = c("app.R", "www/", "Fiche_Affectation_Materiel.docx"),
    forceUpdate = TRUE
  )
  
  cat("✅ Déploiement réussi!\n")
  
}, error = function(e) {
  cat("❌ Erreur lors du déploiement:", e$message, "\n")
  
  # Essayer un déploiement simple
  cat("Tentative de déploiement simple...\n")
  tryCatch({
    rsconnect::deployApp(forceUpdate = TRUE)
    cat("✅ Déploiement simple réussi!\n")
  }, error = function(e2) {
    cat("❌ Échec du déploiement simple:", e2$message, "\n")
  })
})

# 6. Vérification post-déploiement
cat("\n=== VÉRIFICATION ===\n")
tryCatch({
  apps <- rsconnect::applications()
  if (nrow(apps) > 0) {
    cat("✅ Applications déployées:\n")
    print(apps[, c("name", "url", "status")])
  } else {
    cat("⚠️ Aucune application trouvée\n")
  }
}, error = function(e) {
  cat("❌ Erreur lors de la vérification:", e$message, "\n")
})

cat("\n=== FIN DU DÉPLOIEMENT ===\n")
cat("Si le déploiement est réussi, votre application est accessible sur shinyapps.io\n")