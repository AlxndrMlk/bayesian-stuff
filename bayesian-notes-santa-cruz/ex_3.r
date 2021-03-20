### CHALLENGER ###

# Get the data
library(ggplot2)
library(GGally)
library(dplyr)

data_1 = read.delim('challenger_data.txt')

ggplot(data_1, aes(T, I)) + 
  geom_point(color = 'dark blue', alpha = .2) + 
  xlab('Temperature') +
  ylab('Damage') +
  ggtitle('Challenger - temperature vs damage')


# Regression
model_1 = lm(I ~ T, data)

summary(model_1)

# Plot with reg line
ggplot(data_1, aes(T, I)) + 
  geom_point(color = 'dark blue', alpha = .2) + 
  xlab('Temperature') +
  ylab('Damage') +
  ggtitle('Challenger - temperature vs damage') +
  geom_line(aes(T, fitted(model_1)), color = 'red')

# Predict for 31 degrees
predict(model_1, data.frame(T = 31), interval = 'predict')

# Posterior prediction interval (same as frequentist)
pred_1 = predict(model_1, data.frame(T = 31))
 
k   = length(model_1$coefficients) - 1 # Subtract one to ignore intercept
SSE = sum(model_1$residuals**2)
n   = length(model_1$residuals)

resid_std_error = sqrt(SSE / (n-(1+k))) # Residual Standard Error


pred_1 - resid_std_error * qt(.975, model_1$df.residual) * sqrt(1 + 1/23)
# 10.82052 - 2.102*qt(.975,21)*sqrt(1+1/23+((31-mean(T))^2/22/var(T)))

# posterior probability that damage index is greater than zero
1-pt((0-10.82052)/(2.102*sqrt(1+1/23+((31-mean(T))^2/22/var(T)))),21)


### HEIGHT ###

# Get the data
data_2 = read.delim('http://www.randomservices.org/random/data/Galton.txt')

# Plot
data_2 %>% select(-c('Family')) %>% ggpairs(., lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)))

# Regress
model_2 = lm(Height ~ Father + Mother + Gender, data_2)

summary(model_2)


### GOLFERS ###

# Get the data
data_3 = read.delim('http://www.stat.ufl.edu/~winner/data/pgalpga2008.dat', header = FALSE, sep = ' ')
data_3 = data_3[, c(4, 8, 15)]

colnames(data_3) <- c('dist', 'accuracy', 'gender')   # 1 = F, 2 = M

head(data_3)

data_3 %>% filter(gender == 1) %>% ggplot(., aes(dist, accuracy)) + 
                                     geom_point(alpha = .5) + geom_smooth(method = 'lm') +
                                     ggtitle('Mean distance vs accuracy - Females')

data_3 %>% filter(gender == 2) %>% ggplot(., aes(dist, accuracy)) + 
                                     geom_point(alpha = .5) + geom_smooth(method = 'lm') +
                                     ggtitle('Mean distance vs accuracy - Males')


# Fit a linear regression model to the female golfer data only with 
# drive distance as the explanatory variable x and accuracy as the response variable y. 
# Use the standard reference (non-informative) prior.

# Recall that in a linear regression, we are modeling E(y) = b0 + b1x.

# In this particular model, the intercept term is not interpretable, 
# as we would not expect to see a 0-yard drive (but it is still necessary). 
# Predictions should generally be made only within the range of the observed data.

# Report the posterior mean estimate of the slope parameter b relating drive distance to accuracy. 
# Round your answer to two decimal places.

data_3_F = data_3 %>% filter(gender == 1)

model_3 = lm(accuracy ~ dist, data_3_F)

summary(model_3)

# Use the posterior mean estimates of the model coefficients to obtain a posterior predictive 
# mean estimate of driving accuracy for a new female golfer whose average driving distance 
# is x = 260 yards. Round your answer to one decimal place.


predict(model_3, data.frame(dist = 260), interval = 'predict')



