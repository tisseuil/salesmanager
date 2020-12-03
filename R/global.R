# Library in packages used in this application
library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(shinyjs)
library(shinycssloaders)
library(lubridate)
library(shinyFeedback)
library(dplyr)
library(dbplyr)
library(fakir)

fake_ticket_client(vol = 10)
tickets_db <- fake_ticket_client(vol = 100, split = TRUE)
names(tickets_db)

client_tbl <- tickets_db$clients
ticket_tbl <- tickets_db$tickets %>%
  inner_join(
    select(client_tbl,num_client,name)
  ) %>%
  select(ref,num_client,name,year,month,day,type,supported, state,source_call) %>%
  #select(ref,num_client,name,year,month,day,timestamp,type,supported, state,source_call) %>%
  rename(num_ticket = ref)


# save database into Sqlite database
conn <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")
dbListTables(conn)
dbWriteTable(conn, "client_tbl",client_tbl,overwrite=T)
dbWriteTable(conn, "ticket_tbl",ticket_tbl,overwrite=T)


# Stop database connection when application stops
shiny::onStop(function() {
  dbDisconnect(conn)
})

# Turn off scientific notation
options(scipen = 999)

# Set spinner type (for loading)
options(spinner.type = 8)

utils::globalVariables(c("error","num_client","num_ticket"))
