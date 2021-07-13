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
  ggplot(aes(x = estimate[[predictor]], fill= model, color = model), data = estimate) +
    geom_density(alpha=0.6, adjust = 3) +
    theme_minimal(12) +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),
          panel.grid.minor = element_blank(),
          legend.position = "top") +
    labs(x = paste0(choice, " for ", predictor), 
         y = "Density",
         fill = paste0(choice, " in"),
         color = paste0(choice, " in")) +
    scale_fill_discrete(labels = c("Full model", "Predictor included in model")) + 
    scale_color_discrete(labels = c("Full model", "Predictor included in model")) 
  
}