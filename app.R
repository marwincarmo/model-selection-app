## libraries ----
suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(shinydashboard)
    library(shinyWidgets)
    library(ggplot2)
    library(dplyr)
    library(MASS)
    library(purrr)
    library(reactable)
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

source("ui/sidebar.R")
source("ui/main_tab.R")
source("ui/info_tab2.R")



# if the header and/or sidebar get too complex, 
# put them in external files and uncomment below 
# source("ui/header.R") # defines the `header`
# source("ui/sidebar.R") # defines the `sidebar`


## UI ----
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Model selection simulation"),
    sidebar = sidebar, # if sourced above
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), # links to www/custom.css
             tags$script(src = "custom.js") # links to www/custom.js
        ),
        tabItems(
            sim_tab,
            info_tab
        )
    )
)



## server ----
server <- function(input, output, session) {
    
    # reactive variables ----
    df <- reactiveValues()
    
    # input the predictors coefficients
    predictors <- reactive(paste0("x", seq_len(input$n_pred)))
    
    # update predictors boxes
    output$preds <- renderUI({
        purrr::map(predictors(), ~numericInput(.x, label = paste0("True coefficient value for ", .x), 
                                               value = 1, step = .1))
    })
    
    
    # simulate
    observeEvent(input$simulate, {
        debug_msg("simulate", input$simulate)
        # input check
        is_integer <- as.integer(input$sample_size) == input$sample_size
        is_pos <- input$sample_size >= 1
        is_greater_zero <- input$snr > 0
        non_zero_pred <- input$n_pred > 0
        is_bounded <- (input$corr >= -1) & (input$corr <= 1)
        
        if(!is_integer || !is_pos){
            shiny::showNotification("Please fix sample size value. Choose a whole number greater than zero.")
            return()
        }
        if(!is_bounded){
            shiny::showNotification("Please choose a correlation value between -1 and 1.")
            return()
        }
        if(!is_greater_zero){
            shiny::showNotification("The SNR must be greater than 0!")
            return()
        }
        if(!non_zero_pred){
            shiny::showNotification("Number of predictors must be greater than zero.")
            return()
        }

        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        
        progress$set(message = "Simulating datasets", value = 0)
        
        # simulation ----
        reps <- input$simulations
        p <- input$n_pred
        n <- input$sample_size
        # SNR can't be 0
        SNR <- input$snr
        Sigma <- matrix(input$corr, p, p)
        diag(Sigma) <- 1
        b0 <- input$intercept
        beta <- purrr::map_dbl(predictors(), ~input[[.x]])
        selection <- input$fit_crit
        names(beta) <- paste0("X", 1:p)
        coefs <- tvals <- matrix(NA, nrow = reps, ncol = p)
        cover <- matrix(0, nrow = reps, ncol = p)
        rsq <- NULL
        sigma_error <-  sqrt(as.numeric(crossprod(beta, Sigma %*% beta) / SNR))
        colnames(coefs) <- paste0("X", 1:p)
        colnames(cover) <- paste0("X", 1:p)
        colnames(tvals) <- paste0("X", 1:p)
        
        # simulating model selection
        for (i in seq(reps)) {
            
            X <-  MASS::mvrnorm(n = n, rep(0, p) , Sigma)
            y <- as.numeric(cbind(1, X) %*% c(b0, beta) + rnorm(n, 0, sigma_error))
            Xy <- as.data.frame(cbind(X, y))
            colnames(Xy) <- c(paste0("X", 1:p), "y")
            fit <- lm(y ~ ., data = Xy)
            if (selection == "AIC") {
                sel <- step(fit, k = 2, trace = FALSE)
            } else {
                sel <- step(fit, k = log(n) , trace = FALSE)
            }
            s <- summary(sel)
            tval <- s$coefficients[,3][-1]
            tvals[i, names(tval)] <-  tval
            coefs[i, names(tval)] <- coef(sel)[-1]
            cis <- confint(sel)[-1,]
            rsq[i] <- s$r.squared
            # avoids error if there is only one predictor selected
            if (length(cis) < 3) {
                cover[i,names(tval)] <- ifelse(cis[1] < beta[names(tval)] & cis[2] > beta[names(tval)], 1, 0)
            } else {
                cover[i,names(tval)] <- ifelse(cis[names(tval),1] < beta[names(tval)] & cis[names(tval),2] > beta[names(tval)], 1, 0)
            }
            # Increment the progress bar, and update the detail text.
            progress$inc(1/reps, detail = paste("Doing part", i))
            
        }
        
        # results dataframe ----
        df$res <- data.frame(
            Predictor = paste0("X", 1:p),
            Estimate = colMeans(coefs, na.rm = TRUE),
            Coverage = colMeans(cover),
            Bias = colMeans((coefs - beta), na.rm = TRUE),
            MSE = colMeans((coefs - beta)^2, na.rm = TRUE)
            
        )
        
        # simulating estimation from full model
        
        tvals_full <- coefs_full <- matrix(NA, nrow = reps, ncol = p)
        colnames(tvals_full) <- paste0("X", 1:p)
        colnames(coefs_full) <- paste0("X", 1:p)
        
        for (i in seq(reps)) {
            X <-  MASS::mvrnorm(n = n, rep(0, p) , Sigma)
            y <- as.numeric(cbind(1, X) %*% c(b0, beta) + rnorm(n, 0, sigma_error))
            Xy <- as.data.frame(cbind(X, y))
            colnames(Xy) <- c(paste0("X", 1:p), "y")
            fit <- lm(y ~ ., data = Xy)
            s <- summary(fit)
            tval <- s$coefficients[,3][-1]
            tvals_full[i, names(tval)] <-  tval
            coefs_full[i, names(tval)] <- coef(fit)[-1]
        }
        
        # bind estimates for the full and selected model
        # t-values
        df$tvals_complete <- dplyr::bind_rows("step" =as.data.frame(tvals), 
                                     "full" = as.data.frame(tvals_full),
                                     .id = "model")
        # regression coefficients
        df$coefs_complete <- dplyr::bind_rows("step" =as.data.frame(coefs), 
                                              "full" = as.data.frame(coefs_full),
                                              .id = "model")
        
        # rsq_output ----
        
        output$rsq_output <- renderUI({
            withMathJax(
                helpText(
                paste0("The mean \\(R^2\\) value is ", round(mean(rsq), 3))))
        })

        # res_table ----
        output$res_table <- renderReactable({
                reactable(df$res,
                        rownames = FALSE,
                        defaultPageSize = 10,
                        defaultColDef = colDef(format = colFormat(digits = 3)),
                        columns = list(
                            Predictor = colDef(width = 75),
                            R2 = colDef(width = 50)
                        ))
        })
    
        observe({
            # update predictor choices
            updateSelectInput(session, "pred_plot",
                              choices = names(tval))
        })
        # sim_plot ----
        output$sim_plot <- renderPlot({
            
            predictor <- input$pred_plot
            choice <- input$estimate_plot
            
            if (choice == "Parameter estimates") {
                estimate <- df$coefs_complete
            } else {
                estimate <- df$tvals_complete
            }
            
            plot_pred(estimate, predictor, choice)
            
        })
})
    
}
shinyApp(ui, server)

## To do:
## Diagram explaining what is happening: generate data, select a model, then compute CIs (info tab)
## Describe what is happening and illustrate with a flowchart
## blank table if no data is generated yet