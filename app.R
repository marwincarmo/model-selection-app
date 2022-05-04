## Shiny template by @LisaDeBruine -- https://github.com/PsyTeachR/shiny-tutorials

## libraries ----
suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(shinydashboard)
    library(shinyWidgets)
    library(ggplot2)
    library(dplyr)
    library(MASS)
    library(leaps)
    library(purrr)
    library(reactable)
    library(rmarkdown)
    library(markdown)
})

## Functions ----

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

source("ui/sidebar.R")
source("ui/main_tab.R")
source("ui/info_tab.R")


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

periscope::downloadablePlotUI("sim_plot", 
                              downloadtypes = c("png", "csv"), 
                              download_hovertext = "Download the plot and data here!",
                              height = "500px", 
                              btn_halign = "left")


## server ----
server <- function(input, output, session) {
    
    # reactive variables ----
    df <- reactiveValues()
    
    stored_values <- reactiveValues()
    
    # set the fields to input the predictors coefficients values
    predictors <- reactive(paste0("x", seq_len(input$n_pred)))
    
    # randomly generated values predictors coefficients values
    # (Huge thanks to Julio Trecenti for helping me with the code for this feature)
    observeEvent(input$randval, {
        
        # update stored values
        for (ii in seq_along(names(stored_values))) {
            nm <- paste0("x", ii)
            stored_values[[nm]] <- runif(1)
            # update numeric input
            updateNumericInput(
                session,
                paste0("x", ii),
                paste0("True coefficient value for x", ii),
                stored_values[[nm]], 0, 1
            )
        }
        
    })
    
    observe({
        ids <- seq_len(input$n_pred)
        for (ii in ids) {
            nm <- paste0("x", ii)
            stored_values[[nm]] <- input[[nm]]
        }
    })
    
    
    output$preds <- shiny::renderUI({
        
        randList <- purrr::map(seq_len(input$n_pred), ~{
            
            # this function will keep the previous values when
            # the number of inputs is changed

            nm <- paste0("x", .x)
            # if the value doesn't exists, one is created and stored in the reactiveValues object
            # i.e. newly added predictors always start with value of 1
            if (is.null(stored_values[[nm]])) {
                stored_values[[nm]] <- 1
            }
            
            shiny::numericInput(
                nm, 
                paste0("True coefficient value for x", .x), 
                stored_values[[nm]],
                step = .1
            )
        })
        
        randList
    })
    

    # simulate
    observeEvent(input$simulate, {
        debug_msg("simulate", input$simulate)
        # input checks
        is_integer <- as.integer(input$sample_size) == input$sample_size
        is_pos <- input$sample_size >= 1
        is_greater_zero <- input$snr > 0
        non_zero_pred <- input$n_pred > 0
        is_bounded <- (input$corr >= -1) & (input$corr <= 1)
        
        if(!is_integer || !is_pos){
            shiny::showNotification("Please fix sample size value. Choose an integer greater than zero.")
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
        # N_sims, N_predictors and sample size
        reps <- input$simulations
        p <- input$n_pred
        n <- input$sample_size
        # SNR can't be 0
        SNR <- input$snr
        # Correlation matrix
        cormat <- ifelse(input$cormatrix == "", "0.5", input$cormatrix)
        corvals <- as.numeric(unlist(strsplit(cormat,",")))
        Sigma <- matrix(rep(0,p*p), nrow=p)
        Sigma[lower.tri(Sigma)] <-  corvals
        Sigma <- t(Sigma)
        Sigma[lower.tri(Sigma)] <-  corvals
        diag(Sigma) <- 1
        # Coefficients
        b0 <- input$intercept
        beta <- purrr::map_dbl(predictors(), ~input[[.x]])
        selection <- input$fit_crit
        names(beta) <- paste0("x", 1:p)
        # Record results
        coefs <- tvals <- matrix(NA, nrow = reps, ncol = p)
        cover <- matrix(0, nrow = reps, ncol = p)
        rsq <- NULL
        sigma_error <-  sqrt(as.numeric(crossprod(beta, Sigma %*% beta) / SNR))
        colnames(coefs) <- paste0("x", 1:p)
        colnames(cover) <- paste0("x", 1:p)
        colnames(tvals) <- paste0("x", 1:p)
        
        # simulating model selection
        for (i in seq(reps)) {
          
          X <-  MASS::mvrnorm(n = n, rep(0, p) , Sigma)
          y <- as.numeric(cbind(1, X) %*% c(b0, beta) + rnorm(n, 0, sigma_error))
          Xy <- as.data.frame(cbind(X, y))
          colnames(Xy) <- c(paste0("x", 1:p), "y")
          fit <- lm(y ~ ., data = Xy)
          if (selection == "AIC") {
            sel <- step(fit, k = 2, trace = FALSE)
          } else if (selection == "BIC") {
            sel <- step(fit, k = log(n) , trace = FALSE)
          } else if (selection == "Mallows's Cp") {
            
            models <- regsubsets(y ~ ., data = Xy, nvmax = p)
            res.sum <- summary(models)
            cpmod <- which.min(abs(res.sum$cp - p))
            sel <- lm(get_model_formula(cpmod, models, "y"), data=Xy)
          }
          s <- summary(sel)
          tval <- s$coefficients[,3][-1]
          tvals[i, names(tval)] <-  tval
          coefs[i, names(tval)] <- coef(sel)[-1]
          cis <- confint(sel)[-1,]
          rsq[i] <- s$r.squared
          # prevents failure if there is only one predictor selected
          if (length(cis) < 3) {
            cover[i,names(tval)] <- ifelse(cis[1] < beta[names(tval)] & 
                                             cis[2] > beta[names(tval)], 1, 0)
          } else {
            cover[i,names(tval)] <- ifelse(cis[names(tval),1] < beta[names(tval)] & 
                                             cis[names(tval),2] > beta[names(tval)], 1, 0)
          }
          # Increment the progress bar
          progress$inc(1/reps, detail = paste("Doing part", i))
          
        }
        
        # results dataframe ----
        df$res <- data.frame(
            Predictor = paste0("x", 1:p),
            Estimate = colMeans(coefs, na.rm = TRUE),
            Coverage = colMeans(cover),
            Bias = colMeans((coefs - beta), na.rm = TRUE),
            MSE = colMeans((coefs - beta)^2, na.rm = TRUE)
        )
        
        # simulating estimates from full model
        
        tvals_full <- coefs_full <- matrix(NA, nrow = reps, ncol = p)
        colnames(tvals_full) <- paste0("x", 1:p)
        colnames(coefs_full) <- paste0("x", 1:p)
        
        for (i in seq(reps)) {
            X <-  MASS::mvrnorm(n = n, rep(0, p) , Sigma)
            y <- as.numeric(cbind(1, X) %*% c(b0, beta) + rnorm(n, 0, sigma_error))
            Xy <- as.data.frame(cbind(X, y))
            colnames(Xy) <- c(paste0("x", 1:p), "y")
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
                paste0("The mean \\(R^2\\) value was ", round(mean(rsq), 3), 
                       ". The true model was captured by the final model ", 
                       (length(tvals[complete.cases(tvals),])/p)/reps*100,
                       "% of the times.")))
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
## blank table if no data is generated yet
## download results button
## interactions