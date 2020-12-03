#' The UI portion of the module for displaying the Client datatable
#' @title   mod_client_ui and mod_client_server
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_client 
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom DT DTOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom htmltools tags


#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' 
#' 



mod_client_ui <- function(id){
  ns <- NS(id)
  
  # CLASSICAL UI
  tagList(
    fluidRow(
      column(
        width = 2,
        actionButton(
          ns("add_client"),
          "Add",
          class = "btn-success",
          style = "color: #fff;",
          icon = icon('plus'),
          width = '100%'
        ),
        tags$br(),
        tags$br()
      )
    ),
    fluidRow(
      column(
        width = 12,
        title = "Motor Trend Car Road Tests",
        DTOutput(ns('client_table')) %>%
          withSpinner(),
        ##tags$br(),
        tags$br()
      )
    ),
    tags$script(src = "mod_client.js"),
    tags$script(paste0("mod_client_js('", ns(''), "')"))
  )
  
  
  
  
}



#' client Server Function
#'
#' The Server portion of the module for displaying the client datatable
#'
#' @importFrom shiny reactive reactiveVal observeEvent req callModule eventReactive
#' @importFrom DT renderDT datatable replaceData dataTableProxy
#' @importFrom dplyr tbl collect mutate arrange select filter pull "%>%" 
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#' @importFrom htmlwidgets JS
#'
#' @param None
#'
#' @return None
#'
#' @noRd 
#' 
mod_client_server <- function(input, output, session){
  ns <- session$ns
  
  # trigegr to reload data from the "clients" table
  session$userData$client_trigger <- reactiveVal(0)
  
  # Read in "mtclients" table from the database
  client <- reactive({
    session$userData$client_trigger()
    
    out <- NULL
    tryCatch({ 
      out <- conn %>%
        tbl('client_tbl') %>%
        collect()}, 
      error = function(err) {
        msg <- "Database Connection Error"
        # print `msg` so that we can find it in the logs
        print(msg)
        # print the actual error to log it
        print(error)
        # show error `msg` to user.  User can then tell us about error and we can
        # quickly identify where it cam from based on the value in `msg`
        showToast("error", msg)
      }
    )
    out
  })
  
  client_table_prep <- reactiveVal(NULL)
  
  
  ## output$client_table <- renderTable(client())
  
  
  observeEvent(client(), {
    out <- client()
    
    ids <- out$num_client
    
    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
      )
    })
    
    # # Remove the `uid` column. We don't want to show this column to the user
    out <- out %>%
      select(-num_client)
    
    # Set the Action Buttons row to the first column of the `client` table
    out <- cbind(
      tibble(" " = actions),
      out
    )
    
    if (is.null(client_table_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      client_table_prep(out)
      
    } else {
      
      # table has already rendered, so use DT proxy to update the data in the
      # table without rerendering the entire table
      replaceData(client_table_proxy, out, resetPaging = FALSE, rownames = FALSE)
      
    }
  })
  
  output$client_table <- renderDT({
    req(client_table_prep())
    out <- client_table_prep()
    
    datatable(
      out,
      rownames = FALSE,
      # colnames = c('Model', 'Miles/Gallon', 'Cylinders', 'Displacement (cu.in.)',
      #              'Horsepower', 'Rear Axle Ratio', 'Weight (lbs)', '1/4 Mile Time',
      #              'Engine', 'Transmission', 'Forward Gears', 'clientburetors', 'Created At',
      #              'Created By', 'Modified At', 'Modified By'),
      selection = "none",
      class = "compact stripe row-border nowrap",
      # Escape the HTML in all except 1st column (which has the buttons)
      escape = -1,
      extensions = c("Buttons"),
      options = list(
        scrollX = TRUE,
        dom = 'Bftip',
        buttons = list(
          list(
            extend = "excel",
            text = "Download",
            title = paste0("client-", Sys.Date()),
            exportOptions = list(
              columns = 1:(length(out) - 1)
            )
          )
        ),
        columnDefs = list(
          list(targets = 0, orderable = FALSE)
        ),
        drawCallback = JS("function(settings) {
          // removes any lingering tooltips
          $('.tooltip').remove()
        }")
      )
    ) 
    # %>%
    # formatDate(
    #   columns = c("created_at", "modified_at"),
    #   method = 'toLocaleString'
    # )
    
  })
  
  client_table_proxy <- DT::dataTableProxy('client_table')
  
  callModule(
    mod_client_edit_server,
    "add_client",
    modal_title = "Add client",
    client_to_edit = function() NULL,
    modal_trigger = reactive({input$add_client})
  )
  
  client_to_edit <- eventReactive(input$client_id_to_edit, {
    
    client() %>%
      filter(num_client == input$client_id_to_edit)
  })
  
  callModule(
    mod_client_edit_server,
    "edit_client",
    modal_title = "Edit client",
    client_to_edit = client_to_edit,
    modal_trigger = reactive({input$client_id_to_edit})
  )
  
  client_to_delete <- eventReactive(input$client_id_to_delete, {
    
    out <- client() %>%
      filter(num_client == input$client_id_to_delete) %>%
      as.list()
  })
  
  callModule(
    mod_client_delete_server,
    "delete_client",
    modal_title = "Delete client",
    client_to_delete = client_to_delete,
    modal_trigger = reactive({input$client_id_to_delete})
  )
  
  
  
}

## To be copied in the UI
# mod_client_ui("client_ui_1")

## To be copied in the server
# callModule(mod_client_server, "client_ui_1")

