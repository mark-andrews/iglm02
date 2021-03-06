library(tidyverse)

weight_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm02/master/data/weight.csv")
weight_male_df <- weight_df %>% filter(gender == 'male')

# model weight as normal linear model, with predictor height
M_1 <- lm(weight ~ height, data = weight_male_df)

# the coefficients
coef(M_1)
sigma(M_1)

# model weight as normal linear model, 
# with predictors height and age
M_2 <- lm(weight ~ height + age, data = weight_male_df)
coef(M_2)
sigma(M_2)

# model weight as normal linear model, 
# with predictors height and gender
M_3 <- lm(weight ~ height + gender, data = weight_df)
coef(M_3)
sigma(M_3)

M_4 <- lm(weight ~ height + race, data = weight_df)
coef(M_4)

# Log odds ----------------------------------------------------------------

# Let's look at the log odds, or logit function
theta <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# what are the "odds" corresponding to the theta 
theta/(1-theta)

# logit of the theta
# log of the odds
log(theta/(1-theta))

data_df <- tibble(theta = seq(0.01, 0.99, length.out = 100),
                  phi = log(theta/(1-theta)))

ggplot(data_df, 
       aes(x = theta, y = phi)) + geom_line()

ilogit <- function(phi) {1/(1+exp(-phi))}

data_df2 <- tibble(phi = seq(-5, 5, length.out = 100),
                   theta = ilogit(phi))
ggplot(data_df2, 
       aes(x = phi, y = theta)) + geom_line()

# Logistic regression -----------------------------------------------------

affairs_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm02/master/data/affairs.csv")

affairs_df <- mutate(affairs_df, cheater = affairs > 0)

M_5 <- glm(cheater ~ yearsmarried,
           family = binomial(link = 'logit'),
           data = affairs_df)

# M_5 <- lm(yearsmarried ~ cheater, data = affairs_df)

# What are the predicted probabilities of having an affair
# for different values of yearsmarried ...
data_df3 <- tibble(yearsmarried = seq(0.5, 50, length.out = 100))

predict(M_5, newdata = data_df3)

library(modelr)
data_df3 %>% 
  add_predictions(M_5) %>% 
  mutate(p = ilogit(pred))

summary(M_5)
predict(M_5, newdata = data_df3, type = 'response')

data_df3 %>% 
  add_predictions(M_5, type = 'response') %>% 
  ggplot(aes(x = yearsmarried, y = pred)) + geom_line()



# Inference ---------------------------------------------------------------

summary(M_5)
confint.default(M_5)
confint(M_5)

# deviance ----------------------------------------------------------------

logLik(M_5) * -2
deviance(M_5)

M_6 <- glm(cheater ~ yearsmarried + age,
           family = binomial(link = 'logit'),
           data = affairs_df)


delta_deviance <- deviance(M_5) - deviance(M_6)
pchisq(delta_deviance, df = 1, lower.tail = F)
anova(M_5, M_6, test = 'Chisq')


M_7 <- glm(cheater ~ yearsmarried + age + gender + children + rating,
           family = binomial(link = 'logit'),
           data = affairs_df)

anova(M_6, M_7, test = 'Chisq')
# Akaike


# Ordinal logistic regression ---------------------------------------------

library(pscl)
library(MASS)

M_8 <- polr(score ~ gre.quant, data = admit)

admit_df2 <- tibble(gre.quant = seq(300, 800, by = 100))

admit_df2 %>% 
  add_predictions(M_8, type = 'prob')


# What's the probability of being below first cutpoint
# in a logistic distribution with mean mu
# We use the cumulative distribution function of the logistic distribution.
mu <- coef(M) * 600


plogis(M$zeta[1], location = mu)

# What's the probability of being below second cutpoint
plogis(M$zeta[2], location = mu)

# What the probability of being *between* first and second cutpoints
plogis(M$zeta[2], location = mu) - plogis(M$zeta[1], location = mu)


# categorical -------------------------------------------------------------

library(nnet)
M_9 <- multinom(score ~ gre.quant, data = admit)

summary(M_9)

admit_df2 %>% 
  add_predictions(M_9, type = 'prob')

z <- coef(M_9) %*% c(1, 600)
z <- c(0, z)
exp(z)/sum(exp(z))

z <- coef(M_9) %*% c(1, 400)
z <- c(0, z)
exp(z)/sum(exp(z))


prob <- admit_df2 %>% add_predictions(M_9, type = 'prob')
