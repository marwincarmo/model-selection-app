# main_tab ----
main_tab <- tabItem(
  tabName = "main_tab",
  p("Some explanation about the app"),
  box(id = "flower_box", title = "Flower", collapsible = T,
      HTML("<img src='img/flower.jpg' width = '100%'>")
  ),
  actionButton("show_flower", "Show Flower"),
  actionButton("hide_flower", "Hide Flower")
)

# sim_tab ----
sim_tab <- tabItem(
  tabName = "sim_tab",
  
    box(title = "Parameters setting",
        solidHeader = TRUE,
        numericInput(inputId = "simulations",
                    label = "Simulations",
                    value = 1000,
                    min = 1),
        numericInput(inputId = "corr",
                     label = "Correlation",
                     value = 0.5,
                     min = -1,
                     max = 1,
                     step = 0.1),
        numericInput(inputId = "sample_size",
                     label = "Sample size",
                     value = 250,
                     min = 1,
                     step = 10),
        numericInput(inputId = "intercept",
                     label = "Intercept",
                     value = 0),
        numericInput(inputId = "n_param",
                     label = "Number of parameters",
                     value = 1,
                     min = 1),
        numericInput(inputId = "coefs",
                     label = "True coefficient value",
                     value = 0),
        numericInput(inputId = "snr",
                     label = "Signal Noise Ratio",
                     value = 0.5),
        actionButton("simulate", "Simulate")
        
  ),
  tableOutput("res_table"),
  plotOutput("sim_plot")
)