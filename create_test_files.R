# Script pour créer les fichiers Excel de test
# Exécuter ce script pour générer les fichiers de test

library(writexl)

cat("=== CRÉATION DES FICHIERS DE TEST ===\n\n")

# 1. Fichier Superviseurs de test
cat("1. Création du fichier superviseurs_test.xlsx...\n")
superviseurs_test <- data.frame(
  user_name = c("Test User 1", "Test User 2", "Mamadou Ndiaye"),
  user_login = c("test1", "test2", "mamadou"),
  user_password = c("pass1", "pass2", "password123"),
  stringsAsFactors = FALSE
)

write_xlsx(superviseurs_test, "superviseurs_test.xlsx")
cat("✅ Fichier superviseurs_test.xlsx créé\n")

# 2. Fichier Tablettes de test
cat("\n2. Création du fichier tablettes_test.xlsx...\n")
tablettes_test <- data.frame(
  tablette = c("TAB001", "TAB002", "TAB003", "TAB004"),
  chargeur = c("CHG001", "CHG002", "CHG003", "CHG004"),
  powerbank = c("vrai", "faux", "vrai", "faux"),
  stringsAsFactors = FALSE
)

write_xlsx(tablettes_test, "tablettes_test.xlsx")
cat("✅ Fichier tablettes_test.xlsx créé\n")

# 3. Fichier Agents de test
cat("\n3. Création du fichier agents_test.xlsx...\n")
agents_test <- data.frame(
  id_agent = c("AG001", "AG002", "AG003"),
  agent = c("Agent Test 1", "Agent Test 2", "Agent Test 3"),
  groupe = c("Groupe A", "Groupe B", "Groupe A"),
  fonction = c("Enquêteur", "Enquêteur", "Superviseur"),
  telephone = c("0123456789", "0987654321", "0555666777"),
  classe = c("Classe 1", "Classe 2", "Classe 1"),
  superviseur = c("Superviseur Test", "Superviseur Test", "Admin"),
  numero_superviseur = c("SUP001", "SUP001", "ADMIN"),
  stringsAsFactors = FALSE
)

write_xlsx(agents_test, "agents_test.xlsx")
cat("✅ Fichier agents_test.xlsx créé\n")

cat("\n=== FICHIERS DE TEST CRÉÉS ===\n")
cat("Les fichiers suivants ont été créés :\n")
cat("- superviseurs_test.xlsx\n")
cat("- tablettes_test.xlsx\n")
cat("- agents_test.xlsx\n\n")
cat("Vous pouvez maintenant utiliser ces fichiers pour tester l'application.\n") 