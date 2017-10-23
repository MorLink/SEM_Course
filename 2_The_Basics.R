### STRUCTURAL EQUATION MODELING: THE BASICS ###

### Author: Jon Lefcheck
### Last updated: 17 October 2017

# Load required libraries
library(piecewiseSEM)

# Generate random data

set.seed(1) # Generates identical values to the Powerpoint

data <- data.frame(x1 = rnorm(100))

data$x2 <- data$x1 + runif(100, 0, 3)

data$y1 <- data$x2 + runif(100, 0, 6)

data$y2 <- data$x2 + runif(100, 0, 9)

# Standardized coefficients: Bxy * sd(x) / sd(y)
mod <- lm(y2 ~ y1, data)

# `coefs` returns the coefficient table (both standardized and unstandardized)
coefs(mod)

# `unstdCoefs` and `stdCoefs` return unrounded coefficients
stdCoefs(mod)

BetaStd <- stdCoefs(mod)$Estimate * sd(data$y1) / sd(data$y2)

# Compare manually standardized to automatically standardized output
BetaStd; stdCoefs(mod)$Std.Estimate

# The same as scaling the data beforehand and retrieving the raw coefficients
data.scaled <- as.data.frame(apply(data, 2, scale))

mod2 <- lm(y2 ~ y1, data.scaled)

stdCoefs(mod2) # Estimte and Std.Estimate are the same

### RULE 1: PATH COEFFICIENTS FOR UNANALYZED RELATIONSHIPS (BIDIRECTIONAL ARROWS) AMONG
### EXOGENOUS VARIABLES ARE THEIR BIVARIATE CORRELATION

cor(data[, -4])

### RULE 2: WHEN VARIABLES ARE CONNECTED BY A SINGLE PATH, THE (STANDARDIZED) PATH COEFFICIENT IS
### SIMPLY THE CORRELATION COEFFICIENT

cor(data[, -2])

# Path 1
mody1.x1 <- lm(y1 ~ x1, data)

stdCoefs(mody1.x1)$Std.Estimate; cor(data[, c("y1", "x1")])[2, 1]

# Path 2
mody2.y1 <- lm(y2 ~ y1, data)

stdCoefs(mody2.y1)$Std.Estimate; cor(data[, c("y2", "y1")])[2, 1]

### RULE 3: STRENGTH OF A COMPOOUND PATH IS THE PRODUCT OF THE (STANDARDIZED) COEFFICIENTS ALONG
### THE PATH

stdCoefs(mody1.x1)$Std.Estimate * stdCoefs(mody2.y1)$Std.Estimate; cor(data[, c("y2", "x1")])[2, 1]

# Wait a minute...!

### RULE 4: WHEN VARIABLES ARE CONNECTED BY MORE THAN ONE CAUSAL PATHWAY, THE PATH COEFFICIENTS
### ARE "PARTIAL" REGRESSION COEFFICIENTS

# Path 1
mody2.x1 <- lm(y2 ~ y1 + x1, data)

Gamma.y2.x1 <- (cor(data$y2, data$x1) - (cor(data$y2, data$y1) * cor(data$y1, data$x1))) /
  (1 - cor(data$y1, data$x1) ^ 2)

stdCoefs(mody2.x1)[2, 8]; Gamma.y2.x1

# Path 2
Gamma.y2.y1 <- (cor(data$y2, data$y1) - (cor(data$y2, data$x1) * cor(data$y1, data$x1))) /
  (1 - cor(data$y1, data$x1) ^ 2)

stdCoefs(mody2.x1)[1, 8]; Gamma.y2.y1

### RULE 5: PATHS FROM ERROR VARIABLES REPRESENT PREDICTION ERROR

# Path 1
(Zeta.y1 <- 1 - summary(mody1.x1)$r.squared)

sqrt(Zeta.y1)

# Path 2
(Zeta.y2 <- 1 - summary(mody2.x1)$r.squared)

sqrt(Zeta.y2)

### RULE 6: UNANALYZED RESIDUAL CORRELATIONS BETWEEN ENDOGENOUS VARIABLES ARE PARTIAL CORRELATIONS

# Get residuals from models and compute correlation
(pcor <- cor(
  # effect of x1 on y2
  resid(mody2.x1),
  # effect of x1 on y1
  resid(mody1.x1)
))

# Can also use function from piecewiseSEM
partialCorr(y1 %~~% y2, list(mody2.x1, mody1.x1))

# Get residual correlation
mody2.x1 <- lm(y2 ~ x1, data)

(Zeta.y2 = sqrt(1 - summary(mody2.x1)$r.squared))

mody1.x1 <- lm(y1 ~ x1, data)

(Zeta.y1 = sqrt(1 - summary(mody1.x1)$r.squared))

# Total correlation between y1 and y2 equals sum of direct and indirect paths (Also Rule 8)
stdCoefs(mody2.x1)$Std.Estimate * stdCoefs(mody1.x1)$Std.Estimate +
  Zeta.y2 *
  pcor *
  Zeta.y1; cor(data$y1, data$y2)

### RULE 7: TOTAL EFFECT ONE VARIABLE HAS ANOTHER IS THE SUM OF ITS DIRECT AND INDIRECT EFFECTS

mody2 <- lm(y2 ~ x1 + y1, data)

mody1 <- lm(y1 ~ x1, data)

Gamma.y2.x1 <- stdCoefs(mody2)$Std.Estimate[1] + (stdCoefs(mody1)$Std.Estimate[1] * stdCoefs(mody2)$Std.Estimate[2])

Gamma.y2.x1

### RULE 8: SUM OF ALL PATHWAYS BETWEEN TWO VARIABLES (DIRECT AND INDIRECT) EQUALS THE CORRELATION

Gamma.y2.x1; cor(data$y2, data$x1)

# Suppression effect: when the estimated coefficients deviates strongly from its correlation
mody1.1 <- lm(y1 ~ x1 + x2, data)

stdCoefs(mody1.1)$Std.Estimate[2]; cor(data$y1, data$x2)
