#' ticket_delete UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
#' 
#' 

mod_ticket_delete_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}

#' ticket Delete Server Module
#'
#' This module is for deleting a row's information from the tickets database file
#'
#' @importFrom shiny observeEvent req showModal h3 modalDialog removeModal actionButton modalButton
#' @importFrom DBI dbExecute
#' @importFrom shinyFeedback showToast
#'
#' @param modal_title string - the title for the modal
#' @param ticket_to_delete string - the model of the ticket to be deleted
#' @param modal_trigger reactive trigger to open the modal (ticket button)
#'
#' @return None


#' @noRd 
mod_ticket_delete_server <- function(input, output, session, modal_title, ticket_to_delete, modal_trigger){
  ns <- session$ns
  # Observes trigger for this module (here, the Delete Button)
  observeEvent(modal_trigger(), {
    # Authorize who is able to access particular buttons (here, modules)
    # req(session$userData$email == 'clement.tisseuil@gmail.com')
    
    showModal(
      modalDialog(
        div(
          style = "padding: 30px;",
          class = "text-center",
          h2(
            style = "line-height: 1.75;",
            paste0(
              'Are you sure you want to delete the "',
              ticket_to_delete()$num_ticket,
              '"?'
            )
          )
        ),
        title = modal_title,
        size = "m",
        footer = list(
          modalButton("Cancel"),
          actionButton(
            ns("submit_delete"),
            "Delete ticket",
            class = "btn-danger",
            style="color: #fff;"
          )
        )
      )
    )
  })
  
  observeEvent(input$submit_delete, {
    req(ticket_to_delete())
    
    removeModal()
    
    tryCatch({
      
      num_ticket <- ticket_to_delete()$num_ticket
      
      DBI::dbExecute(
        conn,
        "DELETE FROM ticket_tbl WHERE num_ticket=$1",
        params = c(num_ticket)
      )
      
      session$userData$ticket_trigger(session$userData$ticket_trigger() + 1)
      showToast("success", "tickets Successfully Deleted")
    }, error = function(error) {
      
      msg <- "Error Deleting ticket"
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
# mod_ticket_delete_ui("ticket_delete_ui_1")

## To be copied in the server
# callModule(mod_ticket_delete_server, "ticket_delete_ui_1")

