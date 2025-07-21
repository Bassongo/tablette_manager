# Guide de Test - Application Gestion des Tablettes

## 🎯 Objectif
Vérifier que toutes les fonctionnalités SQLite fonctionnent correctement avant le déploiement.

## 📋 Plan de Test

### **Étape 1 : Test Automatique**
1. Ouvrir RStudio
2. Exécuter le script `test_sqlite.R`
3. Vérifier que tous les tests passent (✅)

### **Étape 2 : Test Manuel de l'Application**

#### **2.1 Test de Connexion**
- [ ] Se connecter en tant qu'admin (admin/admin)
- [ ] Vérifier que l'onglet Administration est visible
- [ ] Se déconnecter
- [ ] Se reconnecter en tant que superviseur (si existant)
- [ ] Vérifier que l'onglet Administration est masqué

#### **2.2 Test des Superviseurs (Admin uniquement)**
- [ ] Aller dans Administration > Gestion des Superviseurs
- [ ] Importer un fichier Excel de superviseurs
- [ ] Vérifier l'affichage dans le tableau
- [ ] Supprimer un superviseur individuellement
- [ ] Vider complètement la base des superviseurs
- [ ] Se déconnecter et se reconnecter
- [ ] Vérifier que les données persistent

#### **2.3 Test d'Enregistrement de Tablettes**
- [ ] Aller dans Enregistrement > Manuel
- [ ] Enregistrer une tablette avec powerbank
- [ ] Enregistrer une tablette sans powerbank
- [ ] Vérifier l'affichage dans le tableau
- [ ] Tester l'enregistrement en masse avec un fichier Excel
- [ ] Se déconnecter et se reconnecter
- [ ] Vérifier que les tablettes sont toujours là

#### **2.4 Test d'Affectation**
- [ ] Aller dans Affectation > Individuelle
- [ ] Affecter une tablette à un agent
- [ ] Vérifier que l'état de la tablette passe à "Affectée"
- [ ] Vérifier l'affichage dans le tableau des affectations
- [ ] Tester l'affectation en masse
- [ ] Se déconnecter et se reconnecter
- [ ] Vérifier que les affectations persistent

#### **2.5 Test de Génération de Fiches**
- [ ] Aller dans Génération de fiches
- [ ] Sélectionner une affectation
- [ ] Générer une fiche individuelle
- [ ] Vérifier la prévisualisation
- [ ] Télécharger la fiche
- [ ] Vérifier l'historique des fiches
- [ ] Générer toutes les fiches
- [ ] Se déconnecter et se reconnecter
- [ ] Vérifier que l'historique persiste

#### **2.6 Test de Retour de Tablettes**
- [ ] Aller dans Retour de tablette
- [ ] Sélectionner une tablette affectée
- [ ] Remplir les informations de retour
- [ ] Enregistrer le retour
- [ ] Vérifier que l'état de la tablette change
- [ ] Vérifier l'historique des retours
- [ ] Se déconnecter et se reconnecter
- [ ] Vérifier que les retours persistent

#### **2.7 Test de Déclaration d'Incidents**
- [ ] Aller dans Déclaration d'incident
- [ ] Sélectionner une tablette affectée
- [ ] Déclarer un incident
- [ ] Vérifier que l'état de la tablette change
- [ ] Vérifier l'historique des incidents
- [ ] Se déconnecter et se reconnecter
- [ ] Vérifier que les incidents persistent

#### **2.8 Test du Tableau de Bord (Admin)**
- [ ] Aller dans Administration > Tableau de bord
- [ ] Vérifier les compteurs (disponibles, affectées, etc.)
- [ ] Vérifier le tableau de suivi détaillé
- [ ] Tester les filtres
- [ ] Se déconnecter et se reconnecter
- [ ] Vérifier que les données sont cohérentes

### **Étape 3 : Test de Persistance**

#### **3.1 Test Multi-Sessions**
- [ ] Ouvrir l'application dans deux onglets différents
- [ ] Se connecter en tant qu'admin dans les deux
- [ ] Modifier des données dans le premier onglet
- [ ] Vérifier que les modifications apparaissent dans le second
- [ ] Fermer complètement le navigateur
- [ ] Rouvrir l'application
- [ ] Vérifier que toutes les données sont présentes

#### **3.2 Test de Déconnexion/Reconnexion**
- [ ] Effectuer plusieurs actions (enregistrement, affectation, etc.)
- [ ] Se déconnecter
- [ ] Se reconnecter
- [ ] Vérifier que toutes les données sont intactes

### **Étape 4 : Test des Cas d'Erreur**

#### **4.1 Validation des Données**
- [ ] Essayer d'enregistrer une tablette sans numéro
- [ ] Essayer d'affecter une tablette inexistante
- [ ] Essayer d'importer un fichier Excel mal formaté
- [ ] Vérifier que les messages d'erreur s'affichent correctement

#### **4.2 Test de Robustesse**
- [ ] Fermer brutalement l'application pendant une opération
- [ ] Rouvrir et vérifier l'intégrité des données
- [ ] Tester avec des données volumineuses

## 🚨 Points d'Attention

### **Avant le Déploiement**
- [ ] Tous les tests automatiques passent
- [ ] Tous les tests manuels sont validés
- [ ] Aucune erreur dans la console R
- [ ] Les données persistent correctement
- [ ] L'interface utilisateur fonctionne bien

### **Problèmes Courants à Vérifier**
- [ ] Les valeurs booléennes s'affichent en français (Oui/Non)
- [ ] Les dates sont au bon format
- [ ] Les caractères spéciaux s'affichent correctement
- [ ] Les tableaux se mettent à jour en temps réel
- [ ] Les notifications s'affichent correctement

## 📝 Fichiers de Test

### **Fichier Superviseurs de Test**
Créer un fichier Excel `superviseurs_test.xlsx` avec :
- user_name | user_login | user_password
- Test User 1 | test1 | pass1
- Test User 2 | test2 | pass2

### **Fichier Tablettes de Test**
Créer un fichier Excel `tablettes_test.xlsx` avec :
- tablette | chargeur | powerbank
- TAB001 | CHG001 | vrai
- TAB002 | CHG002 | faux

### **Fichier Agents de Test**
Créer un fichier Excel `agents_test.xlsx` avec :
- id_agent | agent | groupe | fonction | telephone | classe | superviseur | numero_superviseur
- AG001 | Agent Test | Groupe A | Enquêteur | 0123456789 | Classe 1 | Superviseur Test | SUP001

## ✅ Critères de Validation

L'application est prête pour le déploiement si :
- [ ] Tous les tests automatiques passent
- [ ] Tous les tests manuels sont validés
- [ ] Aucune erreur critique n'est détectée
- [ ] Les performances sont acceptables
- [ ] L'expérience utilisateur est fluide

## 🚀 Déploiement

Une fois tous les tests validés :
1. Créer un fichier `.Rbuildignore` si nécessaire
2. Créer un fichier `.rsconnectignore` si nécessaire
3. Déployer sur shinyapps.io
4. Tester l'application déployée
5. Documenter les accès et configurations 