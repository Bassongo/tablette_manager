# tablette_manager

Cette application R Shiny permet de g\u00e9rer l'affectation des tablettes aux agents enqu\u00eateurs, de les enregistrer en stock, d'enregistrer les retours et de suivre les incidents.

Fonctionnalit\u00e9s principales :
* Enregistrement des tablettes (manuel ou via un fichier Excel)
* Affectation individuelle ou en masse des tablettes aux agents
* Gestion des retours et d\u00e9claration d'incidents

## Lancer l'application

Assurez-vous d'avoir R et les packages `shiny`, `DT`, `readxl`, `shinyjs` et `officer` install\u00e9s, puis ex\u00e9cutez :

```R
shiny::runApp('app.R')
```
