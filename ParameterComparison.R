plot_beta1_comparison <- function(SS_model, BRMS_model, normal_model) {
  # Extract the samples for beta[1] from each model
  beta1_SS <- rstan::extract(SS_model)$beta[, 1]
  beta1_BRMS <- rstan::extract(BRMS_model)$beta[, 1]
  beta1_normal <- rstan::extract(normal_model)$beta[, 1]
  
  # Set up the plot with the first density plot
  plot(density(beta1_SS),
       main = "Comparison of beta[1] Distributions",
       xlab = "beta[1]",
       ylab = "Density",
       col = rgb(0, 0, 1, 0.5),  # Blue with transparency
       lwd = 2,
       ylim = c(0, max(density(beta1_SS)$y, density(beta1_BRMS)$y, density(beta1_normal)$y)))
  
  # Overlay the other density plots with transparency
  lines(density(beta1_BRMS), col = rgb(1, 0, 0, 0.5), lwd = 2)  # Red with transparency
  lines(density(beta1_normal), col = rgb(0, 1, 0, 0.5), lwd = 2)  # Green with transparency
  
  # Add a legend
  legend("topright", legend = c("SS_model", "BRMS_model", "normal_model"),
         col = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5), rgb(0, 1, 0, 0.5)),
         lwd = 2)
  
  # Add histograms with breaks = 50 for each model (optional)
  hist(beta1_SS, probability = TRUE, breaks = 50, add = TRUE, col = rgb(0, 0, 1, 0.3))
  hist(beta1_BRMS, probability = TRUE, breaks = 50, add = TRUE, col = rgb(1, 0, 0, 0.3))
  hist(beta1_normal, probability = TRUE, breaks = 50, add = TRUE, col = rgb(0, 1, 0, 0.3))
}

# To use the function, call it with your models:
 plot_beta1_comparison(SS_model, BRMS_model, normal_model)
