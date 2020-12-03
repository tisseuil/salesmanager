#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
#' 



app_ui <- function(request) {

  ## UI PANEL
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    navbarPage(
      "SalesManager !",

      tabPanel(
        "Clients",
        mod_client_ui("client_ui_1"),
      ),
      tabPanel(
        "Tickets",
        mod_ticket_ui("ticket_ui_1"),
      ),
      
      tabPanel(
        "About",
        mod_about_ui("about_ui_1")
      )
    )
  )


}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$title("2020-11-21"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'salesmanager'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    
  )
}

