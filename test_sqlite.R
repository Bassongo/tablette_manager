# Script de test pour vérifier les fonctionnalités SQLite
# Exécuter ce script dans RStudio pour tester l'application

cat("=== TEST DES FONCTIONNALITÉS SQLITE ===\n\n")

# Test 1: Vérification des bibliothèques
cat("1. Test des bibliothèques...\n")
tryCatch({
  library(shiny)
  library(RSQLite)
  library(DBI)
  library(readxl)
  cat("✅ Bibliothèques chargées avec succès\n")
}, error = function(e) {
  cat("❌ Erreur lors du chargement des bibliothèques:", e$message, "\n")
  stop("Impossible de continuer les tests")
})

# Test 2: Test de la base de données
cat("\n2. Test de la base de données...\n")
tryCatch({
  # Charger les fonctions de l'application
  source("app.R", local = TRUE)
  
  # Initialiser la base
  init_database()
  cat("✅ Base de données initialisée\n")
  
  # Tester la connexion
  db <- get_db_connection()
  tables <- dbListTables(db)
  dbDisconnect(db)
  cat("✅ Connexion à la base réussie\n")
  cat("   Tables créées:", paste(tables, collapse = ", "), "\n")
  
}, error = function(e) {
  cat("❌ Erreur lors du test de la base:", e$message, "\n")
})

# Test 3: Test des fonctions de sauvegarde/chargement
cat("\n3. Test des fonctions de sauvegarde/chargement...\n")

# Test superviseurs
tryCatch({
  # Données de test
  test_supervisors <- data.frame(
    user_name = c("Test User 1", "Test User 2"),
    user_login = c("test1", "test2"),
    user_password = c("pass1", "pass2"),
    stringsAsFactors = FALSE
  )
  
  # Sauvegarder
  save_supervisors(test_supervisors)
  cat("✅ Sauvegarde superviseurs OK\n")
  
  # Charger
  loaded_supervisors <- load_supervisors()
  if (nrow(loaded_supervisors) == 2) {
    cat("✅ Chargement superviseurs OK\n")
  } else {
    cat("❌ Erreur: données superviseurs corrompues\n")
  }
  
}, error = function(e) {
  cat("❌ Erreur test superviseurs:", e$message, "\n")
})

# Test tablettes enregistrées
tryCatch({
  test_tablets <- data.frame(
    tablette = c("TAB001", "TAB002"),
    chargeur = c("CHG001", "CHG002"),
    powerbank = c(TRUE, FALSE),
    chargeur_ok = c(TRUE, TRUE),
    powerbank_ok = c(TRUE, FALSE),
    registration_date = c("2024-01-01", "2024-01-02"),
    etat = c("En stock", "En stock"),
    user_login = c("admin", "admin"),
    stringsAsFactors = FALSE
  )
  
  save_registered_tablets(test_tablets)
  cat("✅ Sauvegarde tablettes OK\n")
  
  loaded_tablets <- load_registered_tablets()
  if (nrow(loaded_tablets) == 2) {
    cat("✅ Chargement tablettes OK\n")
  } else {
    cat("❌ Erreur: données tablettes corrompues\n")
  }
  
}, error = function(e) {
  cat("❌ Erreur test tablettes:", e$message, "\n")
})

# Test affectations
tryCatch({
  test_assignments <- data.frame(
    tablette = c("TAB001"),
    chargeur = c("CHG001"),
    powerbank = c(TRUE),
    agent_id = c("AG001"),
    agent_name = c("Agent Test"),
    agent_group = c("Groupe A"),
    agent_function = c("Enquêteur"),
    agent_phone = c("0123456789"),
    agent_class = c("Classe 1"),
    supervisor_name = c("Superviseur Test"),
    supervisor_num = c("SUP001"),
    assign_date = c("2024-01-01"),
    user_login = c("admin"),
    stringsAsFactors = FALSE
  )
  
  save_assignments(test_assignments)
  cat("✅ Sauvegarde affectations OK\n")
  
  loaded_assignments <- load_assignments()
  if (nrow(loaded_assignments) == 1) {
    cat("✅ Chargement affectations OK\n")
  } else {
    cat("❌ Erreur: données affectations corrompues\n")
  }
  
}, error = function(e) {
  cat("❌ Erreur test affectations:", e$message, "\n")
})

# Test 4: Test de persistance entre sessions
cat("\n4. Test de persistance...\n")
tryCatch({
  # Vérifier que les données sont toujours là
  final_supervisors <- load_supervisors()
  final_tablets <- load_registered_tablets()
  final_assignments <- load_assignments()
  
  if (nrow(final_supervisors) == 2 && nrow(final_tablets) == 2 && nrow(final_assignments) == 1) {
    cat("✅ Persistance des données OK\n")
  } else {
    cat("❌ Erreur: données perdues\n")
  }
  
}, error = function(e) {
  cat("❌ Erreur test persistance:", e$message, "\n")
})

# Test 5: Test de l'application complète
cat("\n5. Test de l'application...\n")
cat("Pour tester l'application complète, exécutez:\n")
cat("shiny::runApp()\n\n")

# Test 6: Nettoyage des données de test
cat("6. Nettoyage des données de test...\n")
tryCatch({
  # Vider les tables de test
  save_supervisors(data.frame(user_name = character(), user_login = character(), user_password = character(), stringsAsFactors = FALSE))
  save_registered_tablets(data.frame(tablette = character(), chargeur = character(), powerbank = logical(), chargeur_ok = logical(), powerbank_ok = logical(), registration_date = character(), etat = character(), user_login = character(), stringsAsFactors = FALSE))
  save_assignments(data.frame(tablette = character(), chargeur = character(), powerbank = logical(), agent_id = character(), agent_name = character(), agent_group = character(), agent_function = character(), agent_phone = character(), agent_class = character(), supervisor_name = character(), supervisor_num = character(), assign_date = character(), user_login = character(), stringsAsFactors = FALSE))
  
  cat("✅ Données de test nettoyées\n")
  
}, error = function(e) {
  cat("❌ Erreur nettoyage:", e$message, "\n")
})

cat("\n=== TESTS TERMINÉS ===\n")
cat("Si tous les tests sont OK (✅), l'application est prête pour le déploiement.\n")
cat("En cas d'erreurs (❌), corriger les problèmes avant le déploiement.\n") 