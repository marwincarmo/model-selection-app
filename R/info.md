# Using the app

This Shiny app is designed for performing simuations of statistical inferences after model selection procedures. Its purpose is to illustrate the problems that arise when model selection, parameters estimation and statistical inferences are undertaken with the same data set.

The procedure works in three main steps, as shown by the diagram below:

<center>

---
<img src="img/flowchart2.png";"/>
---

</center>

## Step 1: Generate data

First the preferred regression model must be specified. The user can choose the number of predictors included and its respective parameter values. By default each parameter is set to 1 but new values can be set individually. Note that the more predictors included, the longer it will take to run each simulation.

An intercept value of 1 is also set by default. Although it can be modified, the output will only provide estimates for predictors.

The predictors are drawn at random from a multivariate normal distribution. The variance-covariance matrix is set with 1's on diagonal and the covariance values on the off-diagonal.

The error term variability is expressed in terms of the signal-to-noise-ratio (SNR). This term equals the mean divided by the standard deviation.

<center>
<img src="https://latex.codecogs.com/gif.latex?SNR&space;=&space;\frac{\mu}{\sigma}">
</center>

## Step 2: Select a model

Foward stepwise regression is applied to each sample simulated. Model selection will be performed with the preferred procedure (AIC or BIC).

## Step 3: Results

A mean R<sup>2</sup> value is calculated from all estimated models.

For each specified regression coefficient the results table offer the mean estimate value, the confidence interval coverage, the mean bias and mean squared error.

Plots for the sampling distributions of t-values and parameter estimates are provided for each regressor. These are compared against the theoretical distribution of n samples where no model selection procedure is performed.

<center>
<img src="https://latex.codecogs.com/gif.latex?\begin{align*}&space;bias(\hat{\theta})&space;=&space;\mathbf{E}(\theta)&space;-&space;\theta\\&space;MSE(\hat{\theta})&space;=&space;\mathbf{E}[(\hat{\theta}&space;-&space;\theta)^2]&space;\end{align*}">
</center>


# References

Berk, R., Brown, L., & Zhao, L. (2010). Statistical Inference After Model Selection. Journal of Quantitative Criminology, 26(2), 217-236. https://doi.org/10.1007/s10940-009-9077-7

Bias of an estimator. (2021, May 6). In Wikipedia. https://en.wikipedia.org/wiki/Bias_of_an_estimator


Lohninger, H. (n.d.). Signal and Noise. Fundamentals of Statistics. Retrieved August 2, 2021, from http://www.statistics4u.com/fundstat_eng/cc_signal_noise.html