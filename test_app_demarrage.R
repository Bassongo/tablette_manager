# Test de démarrage de l'application
# Ce script vérifie que l'application peut démarrer sans erreur

cat("=== TEST DE DÉMARRAGE DE L'APPLICATION ===\n\n")

# Test 1: Vérification des bibliothèques
cat("1. Vérification des bibliothèques...\n")
required_packages <- c("shiny", "DT", "readxl", "shinyjs", "bslib", "shinyWidgets", "RSQLite", "DBI", "officer")

missing_packages <- c()
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) > 0) {
  cat("❌ Bibliothèques manquantes:", paste(missing_packages, collapse = ", "), "\n")
  cat("Installez-les avec: install.packages(c(", paste0('"', missing_packages, '"', collapse = ", "), "))\n")
} else {
  cat("✅ Toutes les bibliothèques sont installées\n")
}

# Test 2: Vérification du fichier app.R
cat("\n2. Vérification du fichier app.R...\n")
if (file.exists("app.R")) {
  cat("✅ Fichier app.R trouvé\n")
  
  # Test de syntaxe
  tryCatch({
    source("app.R", echo = FALSE)
    cat("✅ Syntaxe du fichier app.R correcte\n")
  }, error = function(e) {
    cat("❌ Erreur de syntaxe dans app.R:", e$message, "\n")
  })
  
} else {
  cat("❌ Fichier app.R non trouvé\n")
}

# Test 3: Vérification des fichiers requis
cat("\n3. Vérification des fichiers requis...\n")
required_files <- c("Fiche_Affectation_Materiel.docx", "www/qr_scanner.js")

for (file in required_files) {
  if (file.exists(file)) {
    cat("✅", file, "trouvé\n")
  } else {
    cat("⚠️", file, "non trouvé (optionnel)\n")
  }
}

# Test 4: Test de la base de données
cat("\n4. Test de la base de données...\n")
tryCatch({
  # Charger les fonctions de base de données
  source("app.R", local = TRUE)
  
  # Initialiser la base
  init_database()
  cat("✅ Base de données initialisée\n")
  
  # Tester une opération simple
  test_data <- data.frame(
    user_name = "Test",
    user_login = "test",
    user_password = "test",
    stringsAsFactors = FALSE
  )
  
  save_supervisors(test_data)
  loaded_data <- load_supervisors()
  
  if (nrow(loaded_data) == 1) {
    cat("✅ Opérations de base de données fonctionnelles\n")
  } else {
    cat("❌ Problème avec les opérations de base de données\n")
  }
  
  # Nettoyer
  save_supervisors(data.frame(user_name = character(), user_login = character(), user_password = character(), stringsAsFactors = FALSE))
  
}, error = function(e) {
  cat("❌ Erreur lors du test de la base de données:", e$message, "\n")
})

cat("\n=== RÉSUMÉ ===\n")
cat("Si tous les tests sont OK (✅), l'application devrait démarrer correctement.\n")
cat("Pour lancer l'application, exécutez: shiny::runApp()\n")
cat("Pour les tests complets, exécutez: source('test_sqlite.R')\n") 