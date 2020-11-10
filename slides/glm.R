## ---- echo=F------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = F, prompt = F, warning = F, message = F, comment='#>')
# Thanks to 
# https://github.com/ramnathv/slidify/issues/189#issuecomment-15850008
hook1 <- function(x){ gsub("```\n*```r*\n*", "", x) }
hook2 <- function(x){ gsub("```\n+```\n", "", x) }
knitr::knit_hooks$set(document = hook1)


## -----------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(here)
library(knitr)
theme_set(theme_classic())

set.seed(10101)


## ---- out.width='0.67\\textwidth', fig.align='center'-------------------------------------------------------------------
n <- 10
tibble(x = rnorm(n), 
       y = x + rnorm(n)) %>% 
  ggplot(aes(x = x, y = y)) + geom_point() + stat_smooth(method = 'lm', se = F)


## ---- echo = T----------------------------------------------------------------------------------------------------------
weight_df <- read_csv(here('data/weight.csv'))
weight_male_df <- weight_df %>% filter(gender == 'male')

M_1 <- lm(weight ~ height, data = weight_male_df)

M_2 <- lm(weight ~ height + age, data = weight_male_df)



## -----------------------------------------------------------------------------------------------------------------------
tribble(~race, ~x1, ~x2,
        'black', 0, 0,
        'white', 0, 1,
        'hispanic', 1, 0) %>% 
  kable()


## ---- echo = T----------------------------------------------------------------------------------------------------------
M_3 <- lm(weight ~ height + gender, data = weight_df)

M_3 <- lm(weight ~ height + race, data = weight_male_df)



## -----------------------------------------------------------------------------------------------------------------------

logit <- function(p) log(p/(1-p))

tibble(x = seq(0, 1, length.out = 1000),
       y = logit(x)) %>% 
  ggplot(aes(x = x, y = y)) + geom_line() +
  xlab('Probability') + 
  ylab('Log odds')



## -----------------------------------------------------------------------------------------------------------------------
N <- 25000
tibble(x = runif(N),
       y = logit(x)) %>% 
  ggplot(aes(x=y)) + geom_histogram(bins=50, col='white')


## -----------------------------------------------------------------------------------------------------------------------
ilogit <- function(x) 1/(1 + exp(-x))
tibble(x = seq(-5, 5, length.out = 100),
       y = ilogit(x)) %>% 
  ggplot(aes(x = x, y = y)) + geom_line() +
  ylab('Probability') + 
  xlab('Log odds')



## ---- results='hide', echo=T--------------------------------------------------------------------------------------------
affairs_df <- read_csv(here('data/affairs.csv')) %>% 
  mutate(cheater = affairs > 0)

M <- glm(cheater ~ yearsmarried, 
         family = binomial(link = 'logit'),
         data = affairs_df)


## ---- results = 'hide', echo=T------------------------------------------------------------------------------------------
affairs_df_new <- tibble(yearsmarried = c(1, 5, 10, 20, 25))

library(modelr)

# predicted log odds
affairs_df_new %>% 
  add_predictions(M)

# predicted probabilities
affairs_df_new %>% 
  add_predictions(M, type = 'response')


## ---- echo = T, results = 'hide'----------------------------------------------------------------------------------------
M_1 <- glm(cheater ~ yearsmarried + age + gender, 
           family = binomial(link = 'logit'),
           data = affairs_df)

# The "null" model
M_0 <- glm(cheater ~ yearsmarried, 
           family = binomial(link = 'logit'),
           data = affairs_df)

anova(M_0, M, test = 'Chisq')


## ---- out.width="0.75\\textwidth", fig.align="center"-------------------------------------------------------------------
tibble(x = seq(-10, 10, length.out = 10000),
       y = dlogis(x)) %>% 
  ggplot(aes(x = x, y = y)) + geom_line()


## ---- out.width="0.75\\textwidth", fig.align="center"-------------------------------------------------------------------
df_1 <- tibble(x = seq(-10, 10, length.out = 10000),
               y = dlogis(x, location = 1.25)) 
df_2 <- tibble(x = seq(-10, 0, length.out = 10000),
               y = dlogis(x, location = 1.25)) 

df_3 <- bind_rows(c(x = -10, y= 0),
          df_2)

ggplot(data = df_1, aes(x = x, y = y)) + geom_line() +
  geom_polygon(data = rbind(c(0,0), subset(df_1, x > 0)), aes(x = x, y=y), fill = 'grey') +
  geom_polygon(data = rbind(c(0,0), subset(df_1, x < 0)), aes(x = x, y=y), fill = 'grey61')


## ---- echo=T, results='hide'--------------------------------------------------------------------------------------------
library(pscl)
library(MASS)
M <- polr(score ~ gre.quant, data=admit)

admit_df_new <- tibble(gre.quant = c(600, 700, 800))
add_predictions(admit_df_new, M, type = 'probs')

# compare to
plogis(q = M$zeta, location = M$coefficients * 600)




## ---- echo=T, results='hide'--------------------------------------------------------------------------------------------
library(nnet)
M <- multinom(score ~ gre.quant, data=admit)


add_predictions(admit_df_new, M, type = 'probs')

# Compare to
z <- coef(M) %*% c(1, 600)
c(1, exp(z)) / sum(c(1, exp(z)))

