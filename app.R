## libraries ----
suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(shinydashboard)
})

## Functions ----

# source("R/func.R") # put long functions in external files

# display debugging messages in R if local, 
# or in the console log if remote
debug_msg <- function(...) {
    is_local <- Sys.getenv('SHINY_PORT') == ""
    txt <- paste(...)
    if (is_local) {
        message(txt)
    } else {
        shinyjs::logjs(txt)
    }
}

## Tabs ----

# you can put complex tabs in separate files and source them
source("ui/header.R")
source("ui/sidebar.R")
source("ui/main_tab.R")
source("ui/info_tab.R")



# if the header and/or sidebar get too complex, 
# put them in external files and uncomment below 
# source("ui/header.R") # defines the `header`
# source("ui/sidebar.R") # defines the `sidebar`


## UI ----
ui <- dashboardPage(
    skin = "purple",
    header = header, # if sourced above
    #dashboardHeader(title = "Template"),
    sidebar = sidebar, # if sourced above
    #dashboardSidebar(
        # https://fontawesome.com/icons?d=gallery&m=free
    #    sidebarMenu(
    #        id = "tabs",
    #        menuItem("Main", tabName = "main_tab",
    #                 icon = icon("home")),
    #        menuItem("Info", tabName = "info_tab",
    #                 icon = icon("info"))
        
    
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), # links to www/custom.css
            tags$script(src = "custom.js") # links to www/custom.js
        ),
        tabItems(
            main_tab,
            sim_tab,
            info_tab
        )
    )
)



## server ----
server <- function(input, output, session) {
    observeEvent(input$show_flower, {
        debug_msg("show_flower", input$show_flower)
        runjs("openBox('flower_box');")
    })
    
    observeEvent(input$hide_flower, {
        debug_msg("hide_flower", input$hide_flower)
        runjs("closeBox('flower_box');")
    })
    # simulate
    observeEvent(input$simulate, {
        debug_msg("simulate", input$simulate)
        # input check
        is_integer <- as.integer(input$sample_size) == input$sample_size
        is_pos <- input$sample_size >= 1
        is_bounded <- (input$corr >= -1) & (input$corr <= 1)
        
        if(!is_integer || !is_pos){
            shiny::showNotification("Please fix sample size value. Choose a whole number greater than zero")
        }
        if(!is_bounded){
            shiny::showNotification("Please choose a correlation value between -1 and 1")
        }
        
    })
} 

shinyApp(ui, server)