# nuclear explosions tidy tuesday

# required packages -------------------------------------------------------
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(shiny)
library(shinythemes)
library(DT)
library(timevis)
library(shinybusy)


# import data -------------------------------------------------------------
ne <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

# correct format for date_long
ne <- ne %>% mutate(date_long = ymd(date_long))

# to avoid NA showing up in the time line, impute a message where there is no name or upper yield estimation
ne <- ne %>% mutate(name = replace_na(name, "No name provided"))
ne <- ne %>% mutate(yield_upper = replace_na(yield_upper, "No estimation provided"))

# impute corrections for some region's names
# France
ne <- ne %>% mutate(region = mapvalues(region, from = c("FANGATAUFAA", "MUEUEOA"), to = c("FANGATAUFA", "MURUROA")))

# USA
ne <- ne %>% mutate(region = mapvalues(region, from = "S.ATLANTIC", to = "S. ATLANTIC"))

# USSR
ne <- ne %>% mutate(region = mapvalues(region, from = "UKEAINE", to = "UKRAINE"))
ne <- ne %>% mutate(region = mapvalues(region, from = "TUYMEN RUSS", to = "TYUMEN RUSS"))
ne <- ne %>% mutate(region = mapvalues(region, from = "JAKUTS RUSE", to = "JAKUTS RUSS"))
ne <- ne %>% mutate(region = mapvalues(region, from = "AZGIR", to = "AZGIR KAZAKH"))
ne <- ne %>% mutate(region = mapvalues(region, from = "AZGIE KAZAKH" , to = "AZGIR KAZAKH"))
ne <- ne %>% mutate(region = mapvalues(region, from = "BASHKI RUSS" , to = "BASHKIR RUSS"))
ne <- ne %>% mutate(region = mapvalues(region, from = "KAZAKH" , to = "KAZAKHSTAN"))



# shiny app ---------------------------------------------------------------

# ui section --------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  headerPanel("Nuclear Explosions Shiny App"),
  add_busy_spinner(spin = "fading-circle"),
  fluidRow( 
    column(4,
           
           selectInput("countryInput", "Country",
                       choices = sort(unique(ne$country)),
                       selected = "USA"
                       ),
           
           selectInput("regionInput", "Region where nuclear device was deployed",
                       choices = "",
                       selected = "ALAMOGORDO"
                       ),
           
           dateRangeInput("dateInput", "Date",
                          start = min(ne$date_long), 
                          end = max(ne$date_long),
                          min = min(ne$date_long), 
                          max = max(ne$date_long), 
                          startview = "decade"
                          ),
           
           actionButton(inputId = "resetInput", 
                        label = "Reset dates", 
                        icon = icon("undo")
                        ),
           
           h2("Purpose of detonation:"),
           h5(strong("COMBAT:"), "WWII bombs dropped over Japan"),
           h5(strong("FMS:"), "Soviet test, study phenomenon of nuclear explosion"),
           h5(strong("ME:"), "Military Exercise"),
           h5(strong("PNE:"), "Peaceful nuclear explosion"),
           h5(strong("SAM:"), "Soviet test, accidental mode/emergency"),
           h5(strong("SSE:"), "French/US tests - testing safety of nuclear weapons in case of accident"),
           h5(strong("TRANSP:"), "Transportation-storage purposes"),
           h5(strong("WE:"), "British, French, US, evaluate effects of nuclear detonation on various targets"),
           h5(strong("WR:"), "Weapons development program")
           
           ),
    
    column(8, 
           DT::dataTableOutput("table"), # alias: DTOutput()
           
           br(),
           tags$hr(),
           br(),
           
           timevisOutput("timeline")
           )
    
  )
)




# server section ----------------------------------------------------------
server <- function(input, output, session) {
  
  # update select input options for region
  observeEvent(
    eventExpr = input$countryInput,
    handlerExpr = updateSelectInput(session, 
                                    inputId = "regionInput", 
                                    label = "Region",
                                    choices = sort(unique(ne$region[ne$country == input$countryInput])))
    )
  
  # update date input: last date cannot be earlier than starting date
  observeEvent(
    eventExpr = input$dateInput[[1]],
    handlerExpr = updateDateRangeInput(session,
                                       inputId = "dateInput",
                                       min = input$dateInput[[1]])
    )
  
  # reset button
  observeEvent(
    eventExpr = input$resetInput,
    handlerExpr = updateDateRangeInput(session,
                                       inputId = "dateInput",
                                       start = min(ne$date_long), 
                                       end = max(ne$date_long),
                                       min = min(ne$date_long), 
                                       max = max(ne$date_long))
    )
  
  # reactive expression to filter data
  ne_filtered <- reactive(ne %>%
                            filter(country == input$countryInput,
                                   region == input$regionInput,
                                   date_long >= input$dateInput[[1]],
                                   date_long <= input$dateInput[[2]])
                          )
  
  # table
  output$table <- DT::renderDataTable({ # alias renderDT()
    ne_filtered() %>%
      select(c("date_long", "id_no", "country", "region", "source", "purpose", "name")) %>%
      datatable(colnames = c("Date", "Explosion ID", "Country", "Region", "Source", "Purpose", "Name"), 
                options = list(pageLength = 5,
                               order = list(2, "asc")))
  })
  
  # timeline
  output$timeline <- renderTimevis({
    temp_df <- ne_filtered() %>% select(c("date_long", "name", "yield_upper"))
    timevis(data.frame(temp_df,
                       start = temp_df$date_long,
                       content = paste(temp_df$name, " - Upper yield:", temp_df$yield_upper, "kT")))
    })
 }

shinyApp(ui, server)