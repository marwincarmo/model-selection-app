# sim_tab ----
sim_tab <- tabItem(
  tabName = "sim_tab",
  fluidRow(
    column(width = 6,
        box(width = NULL,
            title = "Generate the model",
            status = "primary",
            solidHeader = TRUE,
            numericInput(inputId = "simulations",
                         label = "Simulations",
                         value = 1000,
                         min = 1,
                         step = 50),
            textAreaInput(inputId = "cormatrix", label = "Correlation matrix (defaults to 0.5 if blank)", value = "", 
                          width = NULL, height = NULL,
                          cols = NULL, rows = 7, resize = "vertical",
                          placeholder = "Enter the predictor's correlation matrix off-diagonal entries separated by commas. Use a single value if you want to make all relationships equal.
Ex: .4, .5, .3 will output:
     [,1] [,2] [,3]
[1,]  1.0  0.4  0.5
[2,]  0.4  1.0  0.3
[3,]  0.5  0.3  1.0"),
            numericInput(inputId = "sample_size",
                         label = "Sample size",
                         value = 200,
                         min = 1,
                         step = 10),
            numericInput(inputId = "intercept",
                         label = "Intercept",
                         value = 1),
            numericInput(inputId = "snr",
                         label = "Signal-to-Noise Ratio",
                         min = 0,
                         value = 0.5,
                         step = .1),
            selectInput(inputId = "fit_crit",
                        label = "Fit criterion",
                        choices = c("AIC", "BIC", "Mallows' Cp")
            ),
            actionButton("simulate", "Simulate"))),
    column(width = 6,
           box(width = NULL,
               title = "",
               solidHeader = TRUE,
               status = "primary",
               collapsible = TRUE,
               numericInput(inputId = "n_pred",
                            label = "Number of predictors",
                            value = 1,
                            min = 1),
               shiny::actionButton("randval", "Generate random values"),
               uiOutput("preds")
           )
            )
    ),
  fluidRow(
    column(width = 6,
           box(width = NULL,
               title = "Results",
               solidHeader = TRUE,
               status = "primary",
               uiOutput("rsq_output"),
               reactableOutput("res_table"))
           ),
    column(width = 6,
      box(width = NULL,
          title = "Sampling distribution",
          solidHeader = TRUE,
          status = "primary",
          radioGroupButtons(inputId = "estimate_plot",
                      #label = "Sampling distribution of",
                      choices = c("Parameter estimates", "t-values"),
                      justified = TRUE),
          selectInput(inputId = "pred_plot",
                      label = "For regressor",
                      choices = NULL,
          ),
          plotOutput("sim_plot")))
  )
)