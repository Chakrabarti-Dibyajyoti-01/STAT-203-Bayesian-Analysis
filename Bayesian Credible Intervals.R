library(bayestestR)
library(dplyr)
library(ggplot2)
# Generate a beta distribution
ci=function(a,b){
  posterior <- distribution_beta(1000, a, b)

# Compute HDI and Quantile CI
ci_hdi <- hdi(posterior,ci=0.95)
ci_eti <- eti(posterior,ci=0.95)
}
#1st choice
ci(4,4)
# Plot the distribution and add the limits of the two CIs
posterior %>% 
  estimate_density(extend = TRUE) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_area(fill = "powderblue") +
  theme_classic() +
  # HDI
  geom_vline(xintercept = ci_hdi$CI_low, color = "navy", size = 3) +
  geom_vline(xintercept = ci_hdi$CI_high, color = "navy", size = 3) +
  # ETI
  geom_vline(xintercept = ci_eti$CI_low, color = "slateblue", size = 1) +
  geom_vline(xintercept = ci_eti$CI_high, color = "slateblue", size = 1)

#2nd choice
ci(3,16)
# Plot the distribution and add the limits of the two CIs
posterior %>% 
  estimate_density(extend = TRUE) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_area(fill = "wheat4") +
  theme_classic() +
  # HDI
  geom_vline(xintercept = ci_hdi$CI_low, color = "red4", size = 3) +
  geom_vline(xintercept = ci_hdi$CI_high, color = "red4", size = 3) +
  # ETI
  geom_vline(xintercept = ci_eti$CI_low, color = "pink4", size = 1) +
  geom_vline(xintercept = ci_eti$CI_high, color = "pink4", size = 1)

#3rd choice 
ci(22,4)
# Plot the distribution and add the limits of the two CIs
posterior %>% 
  estimate_density(extend = TRUE) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_area(fill = "steelblue1") +
  theme_classic() +
  # HDI
  geom_vline(xintercept = ci_hdi$CI_low, color = "maroon4", size = 3) +
  geom_vline(xintercept = ci_hdi$CI_high, color = "maroon4", size = 3) +
  # ETI
  geom_vline(xintercept = ci_eti$CI_low, color = "navy", size = 1) +
  geom_vline(xintercept = ci_eti$CI_high, color = "navy", size = 1)
