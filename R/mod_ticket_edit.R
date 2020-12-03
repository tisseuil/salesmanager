#' ticket_edit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ticket_edit_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}


#' ticket Add & Edit Module
#'
#' Module to add & edit tickets in the ticket database file
#'
#' @importFrom shiny observeEvent showModal modalDialog removeModal fluidRow column textInput numericInput selectInput modalButton actionButton reactive eventReactive
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback showToast
#' @importFrom shinyjs enable disable
#' @importFrom lubridate with_tz
#' @importFrom uuid UUIDgenerate
#' @importFrom DBI dbExecute
#'
#' @param modal_title string - the title for the modal
#' @param ticket_to_edit reactive returning a 1 row data frame of the ticket to edit
#' from the "mt_ticket" table
#' @param modal_trigger reactive trigger to open the modal (Add or Edit buttons)
#'
#' @return None
#' @noRd 
#'
# ticket_edit_module 


mod_ticket_edit_server <- function(input, output, session, modal_title, ticket_to_edit, 
                                   modal_trigger) {
  ns <- session$ns
  
  observeEvent(modal_trigger(), {
    hold <- ticket_to_edit()
    
    showModal(
      modalDialog(
        fluidRow(
          column(
            width = 6,
            textInput(
              ns('num_ticket'),
              'ticket Reference',
              value = ifelse(is.null(hold), "", hold$num_ticket)
            ),
            textInput(
              ns('num_client'),
              'Client ID',
              value = ifelse(is.null(hold), "", hold$num_client)
            ),
            textInput(
              ns('name'),
              'Client name',
              value = ifelse(is.null(hold), "", hold$name),
            ),
            numericInput(
              ns('year'),
              'Year',
              value = ifelse(is.null(hold), "", hold$year),
            ),
            numericInput(
              ns('month'),
              'Month',
              value = ifelse(is.null(hold), "", hold$month),
            ),
            numericInput(
              ns('day'),
              'Day',
              value = ifelse(is.null(hold), "", hold$day),
            ),
            
            # dateInput(
            #   ns('timestamp'),
            #   'Timestamp',
            #   value = ifelse(is.null(hold), "", hold$timestamp),
            #   format="yyyy-mm-dd"
            # ),
            
            # numericInput(
            #   ns('timestamp'),
            #   'Timestamp',
            #   value = ifelse(is.null(hold), "", hold$timestamp),
            # ),
            
            textInput(
              ns('type'),
              'Type',
              value = ifelse(is.null(hold), "", hold$type),
            ),
            textInput(
              ns('supported'),
              'Supported',
              value = ifelse(is.null(hold), "", hold$supported),
            ),
            textInput(
              ns('state'),
              'State',
              value = ifelse(is.null(hold), "", hold$state),
            ),
            textInput(
              ns('source_call'),
              'Source Call',
              value = ifelse(is.null(hold), "", hold$source_call),
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
    
    # Observe event for "Model" text input in Add/Edit ticket Modal
    # `shinyFeedback`
    observeEvent(input$model, {
      if (input$model == "") {
        shinyFeedback::showFeedbackDanger(
          "model",
          text = "Must enter model of ticket!"
        )
        shinyjs::disable('submit')
      } else {
        shinyFeedback::hideFeedback("model")
        shinyjs::enable('submit')
      }
    })
    
  })
  
  
  
  edit_ticket_dat <- reactive({
    hold <- ticket_to_edit()
    # timestamp=as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))
    
    out <- list(
      num_ticket = if (is.null(hold)) NA else hold$num_ticket,
      data = list(
        "num_client" = input$num_client, 
        "name" = input$name,
        "year" = input$year,
        "month"=input$month,
        "day"=input$day,
        ##"timestamp"=input$timestamp,
        "type"=input$type,
        "supported"=input$supported,
        "state"=input$state,
        "source_call"=input$source_call
      )
    )
    
    time_now <- as.character(lubridate::with_tz(Sys.time(), tzone = "UTC"))
    
    # if (is.null(hold)) {
    #   # adding a new ticket
    #   
    #   out$data$created_at <- time_now
    #   out$data$created_by <- session$userData$email
    # } else {
    #   # Editing existing ticket
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
    dat <- edit_ticket_dat()
    
    # Logic to validate inputs...
    
    dat
  })
  
  observeEvent(validate_edit(), {
    removeModal()
    dat <- validate_edit()
    
    tryCatch({
      
      if (is.na(dat$num_ticket)) {
        # creating a new ticket
        num_ticket <- uuid::UUIDgenerate(FALSE)
        
        dbExecute(
          conn,
          "INSERT INTO ticket_tbl (num_ticket, num_client, name, year, month, day, 
          type, supported, state, source_call) 
          VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)",
          # "INSERT INTO ticket_tbl (num_ticket, num_client, name, year, month, day, timestamp,
          # type, supported, state, source_call) 
          # VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)",

          params = c(
            list(num_ticket),
            unname(dat$data)
          )
        )
      } else {
        # editing an existing ticket
        dbExecute(
          conn,
          "UPDATE ticket_tbl SET num_client=$1, name=$2, year=$3, month=$4, day=$5,
          type=$6, supported=$7, state=$8, source_call=$9 WHERE num_ticket=$10",
          # "UPDATE ticket_tbl SET num_client=$1, name=$2, year=$3, month=$4, day=$5,
          # timestamp=$6, type=$7, supported=$8, state=$9, source_call=$10 WHERE num_ticket=$11",

          params = c(
            unname(dat$data),
            list(dat$num_ticket)
          )
        )
      }
      
      session$userData$ticket_trigger(session$userData$ticket_trigger() + 1)
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
# mod_ticket_edit_ui("ticket_edit_ui_1")

## To be copied in the server
# callModule(mod_ticket_edit_server, "ticket_edit_ui_1")

