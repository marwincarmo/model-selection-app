info_tab <- tabItem(
  tabName = "info_tab",
  box(title = "Using the app",
      width = NULL,
      solidHeader = TRUE,
      status = "primary",
      p("This Shiny app is designed for performing simuations of statistical inferences
    after model selection procedures. Its purpose is to illustrate the problems 
    that arise when model selection, parameters estimation and statistical inferences 
    are undertaken with the same data set."),
    p("The procedure works in three main steps, as shown by the diagram below:"),
    img(src='img/flowchart.png', align = "center"),
    # p("When performing model selection, a best model is chosen for each random sample
    #   drawn. This model can be thought as an estimate of what the \"correct\" model is,
    #   and the regression parameters dependent on the model beign estimated.
    #   The parameters estimates distribution will, therefore, contain estimates"),
    h3("Step 1: Generate the data"),
    p("First the preferred regression model must be specified. The user can choose the number
    of predictors included and its respective parameter values. By default, each 
    parameter is set to 1, but new values can be set individually.
    Note that the more predictors included, the longer it will take to run each simulation."),
    p("An intercept value of 1 is also set by default. Although this value can be modified, 
    the output will only provide estimates for predictors."),
    p("The predictors are drawn at random from a multivariate normal distribution.
    The correlation matrix is set with 1's on diagonal and the
    correlation value on the off-diagonal."),
    p("The error term variability is expressed in terms of the signal-to-noise-ratio (SNR).
    This term equals the mean divided by the standard deviation."),
    #p("Finally set a number of simulations to be run"),
    h3("Step 2: Model Selection"),
    p("Foward stepwise regression is applied to each sample simulated. Model selection
    will be performed with the preferred procedure (AIC or BIC)."),
    h3("Step 3: Results"),
    p("A mean R2 value is calculated from all estimated models."),
    p("For each specified regression coefficient the results table offer the mean
    estimate value, the confidence interval coverage, the mean bias and mean squared error."),
    p("Plots for the sampling distributions of t-values and parameter estimates
    are provided for each regressor. These are compared against the theoretical distribution of n samples
    where no model selection procedure is performed."),
    h2("References"),
    p("Berk, R., Brown, L., & Zhao, L. (2010). Statistical Inference After Model Selection. 
    Journal of Quantitative Criminology, 26(2), 217-236.", 
    a("https://doi.org/10.1007/s10940-009-9077-7",
      href="https://doi.org/10.1007/s10940-009-9077-7"))
))