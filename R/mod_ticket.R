#' The UI portion of the module for displaying the ticket datatable
#' @title   mod_ticket_ui and mod_ticket_server
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_ticket 
#'
#' @importFrom shiny NS tagList fluidRow column actionButton tags
#' @importFrom DT DTOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom htmltools tags
#' @importFrom stats setNames
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' 
#' 


mod_ticket_ui <- function(id){
  ns <- NS(id)
  
  # CLASSICAL UI
  tagList(
    
    fluidRow(
      column(
        width = 10,
        selectInput(ns("num_client"),
                    "Nom du client",
                    choices = setNames(client_tbl$num_client,client_tbl$name)
                    ## client %>% select("num_client", "name")),
                    ##  selectInput("territory", "Territory", choices = unique(sales$TERRITORY)),
        )
      )
    ),
    
    
    fluidRow(
      column(
        width = 2,
        actionButton(
          ns("add_ticket"),
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
        title = "Ticket sales",
        DT::DTOutput(ns("ticket_table")) %>%
          withSpinner(), 
        ##tags$br(),
        tags$br()
      )
    ),
    tags$script(src = "mod_ticket.js"),
    tags$script(paste0("mod_ticket_js('", ns(''), "')"))
    
    
  )
  
}



#' ticket Server Function
#'
#' The Server portion of the module for displaying the ticket datatable
#'
#' @importFrom shiny reactive reactiveVal observeEvent req callModule eventReactive
#' @importFrom DT renderDT datatable replaceData dataTableProxy
#' @importFrom dplyr tbl collect mutate arrange select filter pull "%>%"
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#' @importFrom DBI dbGetQuery
#' @importFrom htmlwidgets JS
#'
#' @param None
#'
#' @return None
#'
#' @noRd 
#' 
mod_ticket_server <- function(input, output, session){
  ns <- session$ns
  
  # trigegr to reload data from the "tickets" table
  session$userData$ticket_trigger <- reactiveVal(0)
  
  ##num_client_id = reactive({ input$num_client })
  
  # Read in "mttickets" table from the database
  ticket <- reactive({
    session$userData$ticket_trigger()
    
    out <- NULL
    tryCatch({
      # out <- conn %>%
      #   tbl("ticket_tbl") %>%
      #   filter(num_client == {input$num_client}) %>%
      #   collect()},
      query <- paste0("SELECT * FROM ticket_tbl WHERE num_client = '", input$num_client, "';")
      out <- dbGetQuery(conn, query)},
      # out <- conn %>% tbl(ticket_tbl) },
      # out <- ticket_tbl},
      # out <- ticket_tbl %>% filter(num_client == input$num_client)},
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
  
  ## ticket <- reactive(ticket_tbl)
  ## output$ticket_table <- renderDT(ticket())
  
  ticket_table_prep <- reactiveVal(NULL)



  observeEvent(ticket(), {
    out <- ticket()

    ids <- out$num_ticket

    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
      )
    })

    # # Remove the `uid` column. We don't want to show this column to the user
    out <- out %>% select(-num_ticket)

    # Set the Action Buttons row to the first column of the `ticket` table
    out <- cbind(
      tibble(" " = actions),
      out
    )

    if (is.null(ticket_table_prep())) {
      # loading data into the table for the first time, so we render the entire table
      # rather than using a DT proxy
      ticket_table_prep(out)

    } else {

      # table has already rendered, so use DT proxy to update the data in the
      # table without rerendering the entire table
      replaceData(ticket_table_proxy, out, resetPaging = FALSE, rownames = FALSE)

    }
  })

  output$ticket_table <- renderDT({
    req(ticket_table_prep())
    out <- ticket_table_prep()

    datatable(
      out,
      rownames = FALSE,
      # colnames = c('Model', 'Miles/Gallon', 'Cylinders', 'Displacement (cu.in.)',
      #              'Horsepower', 'Rear Axle Ratio', 'Weight (lbs)', '1/4 Mile Time',
      #              'Engine', 'Transmission', 'Forward Gears', 'ticketburetors', 'Created At',
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
            title = paste0("ticket-", Sys.Date()),
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

  ticket_table_proxy <- DT::dataTableProxy('ticket_table')
  
  callModule(
    mod_ticket_edit_server,
    "add_ticket",
    modal_title = "Add ticket",
    ticket_to_edit = function() NULL,
    modal_trigger = reactive({input$add_ticket})
  )

  ticket_to_edit <- eventReactive(input$ticket_id_to_edit, {

    ticket() %>%
      filter(num_ticket == input$ticket_id_to_edit)
  })

  callModule(
    mod_ticket_edit_server,
    "edit_ticket",
    modal_title = "Edit ticket",
    ticket_to_edit = ticket_to_edit,
    modal_trigger = reactive({input$ticket_id_to_edit})
  )

  ticket_to_delete <- eventReactive(input$ticket_id_to_delete, {
    out <- ticket() %>%
      filter(num_ticket == input$ticket_id_to_delete) %>%
      as.list()
  })

  callModule(
    mod_ticket_delete_server,
    "delete_ticket",
    modal_title = "Delete ticket",
    ticket_to_delete = ticket_to_delete,
    modal_trigger = reactive({input$ticket_id_to_delete})
  )
  
  
  
}

## To be copied in the UI
# mod_ticket_ui("ticket_ui_1")

## To be copied in the server
# callModule(mod_ticket_server, "ticket_ui_1")

