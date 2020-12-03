#' client_edit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_client_edit_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}


#' client Add & Edit Module
#'
#' Module to add & edit clients in the client database file
#'
#' @importFrom shiny observeEvent showModal modalDialog removeModal fluidRow column textInput numericInput selectInput modalButton actionButton reactive eventReactive
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback showToast
#' @importFrom shinyjs enable disable
#' @importFrom lubridate with_tz
#' @importFrom uuid UUIDgenerate
#' @importFrom DBI dbExecute
#' @importFrom stats runif setNames
#' @importFrom dplyr "%>%" 
#' @importFrom htmlwidgets JS
#'  
#' @param modal_title string - the title for the modal
#' @param client_to_edit reactive returning a 1 row data frame of the client to edit
#' from the "mt_client" table
#' @param modal_trigger reactive trigger to open the modal (Add or Edit buttons)
#'
#' @return None
#' @noRd 
#'
# client_edit_module 
mod_client_edit_server <- function(input, output, session, modal_title, client_to_edit, modal_trigger) {
  ns <- session$ns
  
  observeEvent(modal_trigger(), {
    hold <- client_to_edit()
    
    showModal(
      modalDialog(
        fluidRow(
          column(
            width = 6,
            textInput(
              ns("num_client"),
              'Client ID',
              value = ifelse(is.null(hold), "", hold$num_client)
            ),
            textInput(
              ns('first'),
              'First name',
              value = ifelse(is.null(hold), "", hold$first),
            ),
            textInput(
              ns('last'),
              'Last name',
              value = ifelse(is.null(hold), "", hold$last),
            ),
            textInput(
              ns('name'),
              'Full Name',
              value = ifelse(is.null(hold), "", hold$name),
            ),
            
            textInput(
              ns('job'),
              'Job',
              value = ifelse(is.null(hold), "", hold$job),
            ),
            
            numericInput(
              ns('age'),
              'Age',
              value = ifelse(is.null(hold), "", hold$age),
            ),
            textInput(
              ns('region'),
              'Region',
              value = ifelse(is.null(hold), "", hold$region),
            ),
            textInput(
              ns('departement'),
              'Region',
              value = ifelse(is.null(hold), "", hold$departement),
            ),
            textInput(
              ns('cb_provider'),
              'Credit Card provider',
              value = ifelse(is.null(hold), "", hold$cb_provider),
            ),
            numericInput(
              ns('fidelity_points'),
              'Fidelity Points',
              value = ifelse(is.null(hold), "", hold$fidelity_points),
            )
          )
        ),
        title = modal_title,
        size = 'm',
        footer = list(
          modalButton('Cancel'),
          actionButton(
            ns('submit'),
            'Submit',
            class = "btn btn-primary",
            style = "color: white"
          )
        )
      )
    )
    
    # Observe event for "Model" text input in Add/Edit client Modal
    # `shinyFeedback`
    observeEvent(input$model, {
      if (input$model == "") {
        shinyFeedback::showFeedbackDanger(
          "model",
          text = "Must enter model of client!"
        )
        shinyjs::disable('submit')
      } else {
        shinyFeedback::hideFeedback("model")
        shinyjs::enable('submit')
      }
    })
    
  })
  
  
  
  
  
  edit_client_dat <- reactive({
    hold <- client_to_edit()
    
    out <- list(
      num_client = if (is.null(hold)) NA else hold$num_client,
      data = list(
        "first" = input$first,
        "last" = input$last, 
        "name"=input$name,
        "job"=input$job,
        "age"=input$age,
        "region"=input$region,
        "departement"=input$departement,
        "cb_provider"=input$cb_provider,
        "fidelity_points"=input$fidelity_points
      )
    )
    
    time_now <- as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))
    
    # if (is.null(hold)) {
    #   # adding a new client
    #   
    #   out$data$created_at <- time_now
    #   out$data$created_by <- session$userData$email
    # } else {
    #   # Editing existing client
    #   
    #   out$data$created_at <- as.character(hold$created_at)
    #   out$data$created_by <- hold$created_by
    # }
    # 
    # out$data$modified_at <- time_now
    # out$data$modified_by <- session$userData$email
    
    out
  })
  
  validate_edit <- eventReactive(input$submit, {
    dat <- edit_client_dat()
    
    # Logic to validate inputs...
    
    dat
  })
  
  observeEvent(validate_edit(), {
    removeModal()
    dat <- validate_edit()
    
    tryCatch({
      
      if (is.na(dat$num_client)) {
        # creating a new client
        num_client <- round(runif(1,500,1000)) ##uuid::UUIDgenerate()

        dbExecute(
          conn,
          "INSERT INTO client_tbl (num_client, first, last, name, job, age, region, departement, cb_provider,
          fidelity_points) VALUES
          ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)",
          # "INSERT INTO clients (uid, model, mpg, cyl, disp, hp, drat, wt, qsec, vs, am,
          # gear, clientb, created_at, created_by, modified_at, modified_by) VALUES
          # ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17)",
          params = c(
            list(num_client),
            unname(dat$data)
          )
        )
      } else {
        # editing an existing client
        dbExecute(
          conn,
          "UPDATE client_tbl SET first=$1, last=$2, name=$3, job=$4, age=$5,
          region=$6, departement=$7, cb_provider=$8, fidelity_points=$9 WHERE num_client=$10",
          # "UPDATE mtcars SET model=$1, mpg=$2, cyl=$3, disp=$4, hp=$5, drat=$6,
          # wt=$7, qsec=$8, vs=$9, am=$10, gear=$11, carb=$12, created_at=$13, created_by=$14,
          # modified_at=$15, modified_by=$16 WHERE uid=$17",
          params = c(
            unname(dat$data),
            list(dat$num_client)
          )
        )
      }
      
      session$userData$client_trigger(session$userData$client_trigger() + 1)
      showToast("success", paste0(modal_title, " Successs"))
    }, error = function(error) {
      
      msg <- paste0(modal_title, " Error")
      
      
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })
  })
  
}


## To be copied in the UI
# mod_client_edit_ui("client_edit_ui_1")

## To be copied in the server
# callModule(mod_client_edit_server, "client_edit_ui_1")

