info_tab <- tabItem(
  tabName = "info_tab",
  h2("Using the app"), 
  p("This Shiny app is designed for performing simuations of statistical inferences
    after model selection procedures. Its purpose is to illustrate the problems 
    that arise when model selection, parameters estimation and statistical inferences 
    are undertaken with the same data set."),
  p("When performing model selection, a best model is chosen for each random sample
    drawn. This model can be thought as an estimate of what the \"correct\" model is,
    and the regression parameters dependent on the model beign estimated.
    The parameters estimates distribution will, therefore, contain estimates"),
  h3("Step 1: Generate the data"),
  p("First the correct regression model must be specified. The user can choose the number
    of predictors included and its respective parameter values. By default, each 
    parameter is set to 1, but new values can be set individually.
    Note that the more predictors included, the longer it will take to run each simulation."),
  p("An intercept value of 1 is also set by default. Although this value can be changed, 
    the output will only provide estimates for predictors."),
  p("The predictors are drawn at random from a multivariate normal distribution.
    A sample correlation coefficient can be set. This value represents the correlation
    between each variable included in the model. The sampling variability
    is expressed in terms of the signal-to-noise-ratio (SNR)"),
  p(""),
  h2("References"),
  p("Berk, R., Brown, L., & Zhao, L. (2010). Statistical Inference After Model Selection. 
    Journal of Quantitative Criminology, 26(2), 217-236. https://doi.org/10.1007/s10940-009-9077-7
")
)