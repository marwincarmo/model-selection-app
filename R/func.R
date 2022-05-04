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

# plot estimates for the full and selected models
plot_pred <- function(estimate, predictor, choice) {
  ggplot(aes(x = .data[[predictor]], fill= model, color = model), data = estimate) +
    geom_density(alpha=0.4, adjust = 3) +
    theme_minimal(base_size = 14) +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),
          panel.grid.minor = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 12)
          ) +
    labs(x = paste0(choice, " for ", predictor), 
         y = "Density",
         fill = NULL, #paste0(choice, " in"),
         color = NULL) + #paste0(choice, " in")) +
    scale_fill_manual(labels = c("Full model", "Predictor included in a model"),
                        values = c("red3", "dodgerblue3")) + 
    scale_color_manual(labels = c("Full model", "Predictor included in a model"),
                         values = c("red3", "dodgerblue3")) 
  
}

# get model formula
get_model_formula <- function(id, object, outcome){
  
  # get models data
  models <- summary(object)$which[id,-1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}