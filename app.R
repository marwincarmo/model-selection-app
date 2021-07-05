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
            return()
        }
        if(!is_bounded){
            shiny::showNotification("Please choose a correlation value between -1 and 1")
            return()
        }
        
        # simulate data
        p <- input$n_param
        SNR <- input$snr
        Sigma <- matrix(input$corr, p, p)
        diag(Sigma) <- 1
        n = input$sample_size
        b0 <- input$intercept
        beta <- rep(input$coefs, p)
        names(beta) <- paste0("x", 1:p)
        reps = input$simulations
        coefs <- cover <- matrix(NA, nrow = reps, ncol = p)
        sigma_error <-  sqrt(as.numeric(crossprod(beta, Sigma %*% beta) / SNR))
        colnames(coefs) <- paste0("x", 1:p)
        colnames(cover) <- paste0("x", 1:p)
        
        for (i in seq(reps)) {
            
            X <-  MASS::mvrnorm(n = n, rep(0, p) , Sigma)
            y <- as.numeric(cbind(1, X) %*% c(b0, beta) + rnorm(n, 0, sigma_error))
            Xy <- as.data.frame( cbind(X, y))
            colnames(Xy) <- c(paste0("x", 1:p), "y")
            fit <- lm(y ~ ., data = Xy)
            coefs[i, ] <- coef(fit)[-1]
            cis <- confint(fit)[-1,]
            cover[i,] <- ifelse(cis[,1] < 1 & cis[,2] > 1, 1, 0)
        }
        
        res <- data.frame(
            pred = paste0("x", 1:p),
            mean_coef = colMeans(coefs),
            cover = colMeans(cover),
            mse = colMeans((coefs - beta)^2)
            
        )
    })
    
    output$results <- renderTable(res)
} 

shinyApp(ui, server)