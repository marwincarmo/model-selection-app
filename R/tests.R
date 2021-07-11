library(magrittr)
reps <- 1
p <- 3
n <- 200
# SNR can't be 0
SNR <- .2
Sigma <- matrix(c(5,4,5,
                  4,6,5,
                  5,5,7), p, p)
#diag(Sigma) <- 1
b0 <- 3
beta <- c(0, 1, 2)
selection <- "AIC"
names(beta) <- paste0("x", 1:p)
coefs <- tvals <- matrix(NA, nrow = reps, ncol = p)
cover <- matrix(0, nrow = reps, ncol = p)
rsq <- NULL
sigma_error <- 10
#sigma_error <-  sqrt(as.numeric(crossprod(beta, Sigma %*% beta) / SNR))
colnames(coefs) <- paste0("x", 1:p)
colnames(cover) <- paste0("x", 1:p)
colnames(tvals) <- paste0("x", 1:p)

for (i in seq(reps)) {
  #print(i)
  X <-  MASS::mvrnorm(n = n, rep(0, p) , Sigma)
  y <- as.numeric(cbind(1, X) %*% c(b0, beta) + rnorm(n, 0, sigma_error))
  Xy <- as.data.frame(cbind(X, y))
  colnames(Xy) <- c(paste0("x", 1:p), "y")
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
}

# results dataframe ----
res <- data.frame(
  pred = paste0("x", 1:p),
  rsq = mean(rsq),
  mean_coef = colMeans(coefs, na.rm = TRUE),
  cover = colMeans(cover, na.rm = TRUE),
  bias = colMeans((coefs - beta), na.rm = TRUE),
  mse = colMeans((coefs - beta)^2, na.rm = TRUE))
res

# !is.na(tvals[,1])
# plot(density(tvals[,1][!is.na(tvals[,1])]))


# Full model --------------------------------------------------------------


tvals_full <- matrix(NA, nrow = reps, ncol = p)
colnames(tvals_full) <- paste0("x", 1:p)

for (i in seq(reps)) {
  #print(i)
  X <-  MASS::mvrnorm(n = n, rep(0, p) , Sigma)
  y <- as.numeric(cbind(1, X) %*% c(b0, beta) + rnorm(n, 0, sigma_error))
  Xy <- as.data.frame(cbind(X, y))
  colnames(Xy) <- c(paste0("x", 1:p), "y")
  fit <- lm(y ~ ., data = Xy)
  s <- summary(fit)
  tval <- s$coefficients[,3][-1]
  tvals_full[i, names(tval)] <-  tval
}

tstatval <- dplyr::bind_rows("step" =as.data.frame(tvals), 
                             "full" = as.data.frame(tvals_full),
                             .id = "model")

tstatval %>% 
  ggplot(aes(x = x2, fill= model, color = model)) +
  geom_density(alpha=0.6, adjust = 3) +
  theme_minimal(12) +
  #theme(panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.minor = element_blank(),
        legend.position = c(.8,.9)) +
  labs(x = "t-values for Regressor X1", 
       y = "Density",
       fill = "t-values in",
       color = "t-values in") +
  scale_fill_discrete(labels = c("Full model", "Predictor included in model")) + 
  scale_color_discrete(labels = c("Full model", "Predictor included in model"))
  
  
plot_models <- function(df, predictor) {
  df %>% 
    ggplot(aes(x = predictor, fill= model, color = model)) +
    geom_density(alpha=0.6, adjust = 3) +
    theme_minimal(12) +
    #theme(panel.grid.minor = element_blank()) +
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),
          panel.grid.minor = element_blank(),
          legend.position = c(.8,.9)) +
    labs(x = paste0("t-values for Regressor ", predictor), 
         y = "Density",
         fill = "t-values in",
         color = "t-values in") +
    scale_fill_discrete(labels = c("Full model", "Predictor included")) + 
    scale_color_discrete(labels = c("Full model", "Predictor included"))
}
