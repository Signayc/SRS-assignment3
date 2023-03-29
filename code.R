# Load package
library(MASS)

# Read in the data
data <- c(703,1006,76,464,19,62,4,54,3,19,0,15,1,1,1)
# S1- Local authority
S1 <- c(0,0,0,0,0,0,0,1,1,1,1,1,1,1,1)
# S2 - Non-government organisation
S2 <- c(0,0,0,1,1,1,1,0,0,0,0,1,1,1,1)
# S3 - Police force
S3 <- c(0,1,1,0,0,1,1,0,0,1,1,0,0,1,1)
# S4 - Government organisation
S4 <- c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1)

# Initial Model
# fit the initial model without any interaction terms
initial.model <- glm(data~S1+S2+S3+S4,
             family = poisson(link = "log"))
summary(initial.model)
# todo: interpret coefficients

# Adding interaction terms
# fit the full model with interaction terms between each two lists
full.model <- glm(data~S1+S2+S3+S4+S1:S2+S1:S3+S1:S4+S2:S3+S2:S4+S3:S4, 
             family = poisson(link = "log"))
summary(full.model)
# todo : interpret coefficients, which interaction terms are significant?

# Model selection
# Using AIC value as relative goodness of fit in model selection
# stepwise regression in both direction to figure out the best performed model 
# according to the AIC value
step.model <-stepAIC(full.model, direction = "both", trace = TRUE)
summary(step.model)
# todo: interpret coefficients

# todo: decide which absolute goodness of fit should be used for final model
#       and interpret

# Checking model assumptions
# some model diagnostic charts including residual plot and normal q-q plot
plot(step.model)

# Attain the estimate of total population size N
# use intercept to achieve MLE for y0000 and calculate the estimate of the total 
# population size N
beta0 <- step.model$coefficients[1]
y0000 <- exp(beta0)
y0000
N <- sum(data)+y0000
N

# Attain the confidence interval for N
lowerbond <- summary(step.model)$coefficients[1,1]-1.96*summary(step.model)$coefficients[1,2]
upperbond <- summary(step.model)$coefficients[1,1]+1.96*summary(step.model)$coefficients[1,2]
lb_exp <- exp(lowerbond)+sum(data)
ub_exp <- exp(upperbond)+sum(data)
cat("The 95% confidence interval for N is [",lb_exp,",",ub_exp,"].",sep = "")
