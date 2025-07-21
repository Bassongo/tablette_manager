# Modular UI components

login_ui <- function() {
  modalDialog(
    title = "Connexion",
    textInput("login_user", "Login"),
    passwordInput("login_pass", "Mot de passe"),
    footer = tagList(actionButton("login_btn", "Se connecter")),
    easyClose = FALSE
  )
}

register_ui <- function() {
  tabsetPanel(
    tabPanel("Scan QR",
             fluidRow(
               column(6,
                      actionBttn("scan_tablet_btn","Scanner QR Tablette",style="fill",color="success"),
                      textInput("reg_tab_num_qr","Num\u00e9ro tablette"),
                      actionBttn("scan_charger_btn","Scanner QR Chargeur",style="fill",color="primary"),
                      textInput("reg_charger_num_qr","Num\u00e9ro chargeur"),
                      materialSwitch("reg_has_powerbank_qr","Powerbank pr\u00e9sent",status="primary"),
                      actionBttn("register_qr_btn","Enregistrer",style="fill",color="primary",class="blue-btn")
               ),
               column(6,DTOutput("register_table"))
             )
    ),
    tabPanel("Manuel",
             fluidRow(
               column(6,
                      textInput("reg_tab_num","Num\u00e9ro tablette"),
                      textInput("reg_charger_num","Num\u00e9ro chargeur"),
                      materialSwitch("reg_has_powerbank","Powerbank pr\u00e9sent",status="primary"),
                      actionBttn("register_btn","Enregistrer",style="fill",color="primary",class="blue-btn")
               ),
               column(6,DTOutput("register_table"))
             )
    ),
    tabPanel("En masse",
             fluidRow(
               column(4,fileInput("tablets_register_file","Liste des tablettes",accept=c(".xlsx",".xls")),
                      actionBttn("register_mass_btn","Enregistrer en masse",style="fill",color="primary",class="blue-btn")),
               column(8,DTOutput("register_table"))
             )
    )
  )
}

assignment_ui <- function() {
  tabsetPanel(
    tabPanel("Individuelle",
             fluidRow(
               column(4,
                      textInput("tab_num","Num\u00e9ro de la tablette"),
                      textInput("charger_num","Num\u00e9ro de chargeur"),
                      materialSwitch("has_powerbank","Powerbank pr\u00e9sent",status="primary"),
                      textInput("agent_id","ID de l'agent"),
                      textInput("agent_name","Nom"),
                      textInput("agent_group","Groupe"),
                      selectInput("agent_function","Fonction",choices=c("Enqu\u00eateur","Superviseur")),
                      textInput("agent_phone","T\u00e9l"),
                      textInput("agent_class","Classe"),
                      textInput("supervisor_name","Nom superviseur"),
                      textInput("supervisor_num","Num superviseur"),
                      dateInput("assign_date","Date"),
                      actionBttn("assign_btn","Affecter",style="fill",color="primary",class="blue-btn")
               ),
               column(8,DTOutput("assign_table"))
             )
    ),
    tabPanel("En masse",
             fluidRow(
               column(4,
                      fileInput("agents_file","Liste des agents",accept=c(".xlsx",".xls")),
                      fileInput("tablets_file","Liste des tablettes",accept=c(".xlsx",".xls")),
                      actionBttn("mass_assign_btn","Affecter al\u00e9atoirement",style="fill",color="primary",class="blue-btn")
               ),
               column(8,DTOutput("assign_table"))
             )
    )
  )
}

fiche_ui <- function() {
  fluidRow(
    column(6,
           selectInput("fiche_assign_select","S\u00e9lectionner une affectation",choices=NULL),
           actionBttn("generate_fiche_btn","G\u00e9n\u00e9rer la fiche",style="fill",color="success",class="blue-btn"),
           actionBttn("generate_all_fiches_btn","G\u00e9n\u00e9rer toutes",style="fill",color="warning",class="blue-btn"),
           actionBttn("reset_selection_btn","R\u00e9initialiser",style="fill",color="warning",class="blue-btn")
    ),
    column(6,DTOutput("fiches_history_table"))
  )
}

return_ui <- function() {
  fluidRow(
    column(4,
           textInput("return_agent_id","ID de l'agent"),
           selectInput("return_tablet_select","S\u00e9lectionner la tablette",choices=NULL),
           textInput("return_charger_num","Num\u00e9ro chargeur"),
           materialSwitch("return_has_powerbank","Powerbank",status="primary"),
           textInput("return_reason","Motif"),
           selectInput("return_condition","\u00c9tat",choices=c("Bon \u00e9tat","L\u00e9g\u00e8rement endommag\u00e9e","Endommag\u00e9e","Hors service")),
           dateInput("return_date","Date",value=Sys.Date()),
           textAreaInput("return_notes","Notes",rows=3),
           actionBttn("return_tablet_btn","Enregistrer le retour",style="fill",color="warning",class="blue-btn")
    ),
    column(8,DTOutput("returns_table"))
  )
}

incident_ui <- function() {
  fluidRow(
    column(4,
           textInput("incident_agent_id","ID de l'agent"),
           selectInput("incident_tablet_select","Tablette",choices=NULL),
           selectInput("incident_type","Type",choices=c("Casse","Perte","Autre")),
           textAreaInput("incident_description","Description"),
           dateInput("incident_date","Date",value=Sys.Date()),
           actionBttn("declare_incident_btn","D\u00e9clarer",style="fill",color="danger",class="blue-btn")
    ),
    column(8,DTOutput("incidents_table"))
  )
}

