# Data management functions
# All datasets stored as RDS in data/ directory

# Load required libraries
library(shiny)
library(DT)
library(readxl)
library(shinyjs)
library(bslib)
library(shinyWidgets)
library(officer)
library(ggplot2)
library(reshape2)

# ensure directories exist
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("fiches_generees")) dir.create("fiches_generees")

# generic loader/saver
load_data <- function(name, default_df = data.frame()) {
  path <- file.path("data", paste0(name, ".rds"))
  if (file.exists(path)) {
    readRDS(path)
  } else {
    default_df
  }
}

save_data <- function(df, name) {
  saveRDS(df, file.path("data", paste0(name, ".rds")))
}

# default structures
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
    chargeur = character(),
    powerbank = logical(),
    agent_id = character(),
    return_reason = character(),
    condition = character(),
    return_date = character(),
    notes = character(),
    user_login = character(),
    stringsAsFactors = FALSE
  )
}

empty_tablet_incidents <- function() {
  data.frame(
    tablette = character(),
    incident_type = character(),
    description = character(),
    date = character(),
    user_login = character(),
    stringsAsFactors = FALSE
  )
}

empty_supervisors <- function() {
  data.frame(
    user_name = character(),
    user_login = character(),
    user_password = character(),
    stringsAsFactors = FALSE
  )
}

empty_generated_fiches <- function() {
  data.frame(
    filename = character(),
    tablette = character(),
    agent_name = character(),
    assign_date = character(),
    transferred = logical(),
    stringsAsFactors = FALSE
  )
}

# wrappers
load_supervisors <- function() load_data("supervisors", empty_supervisors())
save_supervisors <- function(data) save_data(data, "supervisors")

load_registered_tablets <- function() load_data("registered_tablets", empty_registered_tablets())
save_registered_tablets <- function(data) save_data(data, "registered_tablets")

load_assignments <- function() load_data("assignments", empty_assignments())
save_assignments <- function(data) save_data(data, "assignments")

load_tablet_returns <- function() load_data("tablet_returns", empty_tablet_returns())
save_tablet_returns <- function(data) save_data(data, "tablet_returns")

load_tablet_incidents <- function() load_data("tablet_incidents", empty_tablet_incidents())
save_tablet_incidents <- function(data) save_data(data, "tablet_incidents")

load_generated_fiches <- function() load_data("generated_fiches", empty_generated_fiches())
save_generated_fiches <- function(data) save_data(data, "generated_fiches")

# reset all data
reset_all_data <- function() {
  save_supervisors(empty_supervisors())
  save_registered_tablets(empty_registered_tablets())
  save_assignments(empty_assignments())
  save_tablet_returns(empty_tablet_returns())
  save_tablet_incidents(empty_tablet_incidents())
  save_generated_fiches(empty_generated_fiches())
  if (dir.exists("fiches_generees")) {
    unlink(list.files("fiches_generees", full.names = TRUE), force = TRUE)
  }
}

# unified user data filtering
get_user_data <- function(data, user_role, current_user) {
  if (is.null(user_role) || user_role == "admin") return(data)
  if ("user_login" %in% names(data)) {
    data[data$user_login == current_user, , drop = FALSE]
  } else {
    data
  }
}

