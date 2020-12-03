#' client_delete UI Function
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

mod_client_delete_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}

#' Client Delete Server Module
#'
#' This module is for deleting a row's information from the clients database file
#'
#' @importFrom shiny observeEvent req showModal h3 modalDialog removeModal actionButton modalButton
#' @importFrom DBI dbExecute
#' @importFrom shinyFeedback showToast
#'
#' @param modal_title string - the title for the modal
#' @param client_to_delete string - the model of the client to be deleted
#' @param modal_trigger reactive trigger to open the modal (client button)
#'
#' @return None


#' @noRd 
mod_client_delete_server <- function(input, output, session, modal_title, client_to_delete, modal_trigger){
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
              client_to_delete()$name,
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
            "Delete Client",
            class = "btn-danger",
            style="color: #fff;"
          )
        )
      )
    )
  })
  
  observeEvent(input$submit_delete, {
    req(client_to_delete())
    
    removeModal()
    
    tryCatch({
      
      num_client <- client_to_delete()$num_client
      
      DBI::dbExecute(
        conn,
        "DELETE FROM client_tbl WHERE num_client=$1",
        params = c(num_client)
      )
      
      session$userData$client_trigger(session$userData$client_trigger() + 1)
      showToast("success", "Clients Successfully Deleted")
    }, error = function(error) {
      
      msg <- "Error Deleting Client"
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
# mod_client_delete_ui("client_delete_ui_1")

## To be copied in the server
# callModule(mod_client_delete_server, "client_delete_ui_1")

