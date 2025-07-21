source("modules/data_management.R")
source("modules/server_modules.R")
source("modules/fiche_generation.R")

server <- function(input, output, session) {
  supervisors <- reactiveVal(load_supervisors())
  registered_tablets <- reactiveVal(load_registered_tablets())
  assignments <- reactiveVal(load_assignments())
  tablet_returns <- reactiveVal(load_tablet_returns())
  tablet_incidents <- reactiveVal(load_tablet_incidents())
  generated_fiches <- reactiveVal(load_generated_fiches())

  user_role <- reactiveVal(NULL)
  current_user <- reactiveVal(NULL)
  session$userData$current_user <- NULL

  showModal(login_ui())

  observeEvent(input$login_btn, {
    req(input$login_user, input$login_pass)
    if (input$login_user == "admin" && input$login_pass == "admin") {
      user_role("admin")
      current_user("admin")
    } else {
      sup <- supervisors()
      idx <- which(sup$user_login == input$login_user & sup$user_password == input$login_pass)
      if (length(idx) == 1) {
        user_role("supervisor")
        current_user(input$login_user)
      } else {
        showNotification("Identifiants invalides", type="error")
        return()
      }
    }
    session$userData$current_user <- current_user()
    removeModal()
  })

  output$welcome_message <- renderUI({
    req(current_user())
    if (user_role() == "supervisor") {
      HTML(paste0("<h2 style='color: white;'>Bienvenue ", current_user(), "</h2>"))
    } else {
      HTML("<h2 style='color: white;'>Bienvenue Administrateur</h2>")
    }
  })

  # Modules server
  callModule(register_server, "reg", reactive_tablets = registered_tablets)
  callModule(assignment_server, "assign", reactive_assignments = assignments, reactive_tablets = registered_tablets)
  callModule(fiche_server, "fiche", reactive_assignments = assignments, reactive_fiches = generated_fiches)
  callModule(return_server, "return", reactive_returns = tablet_returns, reactive_tablets = registered_tablets)
  callModule(incident_server, "incident", reactive_incidents = tablet_incidents)

}

