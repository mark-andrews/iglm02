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
library(cowplot)
library(modelr)
library(pscl)
library(MASS)
theme_set(theme_classic())

set.seed(10101)

biochemists_Df <- read_csv(here('data/biochemist.csv'))

theme_set(theme_classic())

set.seed(10101)

data("bioChemists")
publications <- bioChemists$art




## ---- out.width='0.75\\textwidth', fig.align='center'-------------------------------------------------------------------
lambda <- 3.5
tibble(x = seq(0, 25),
       y = dpois(x, lambda = lambda)
) %>% ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_segment(aes(x = x, xend = x, y = 0, yend = y), color = "grey50") +
  ylab('P(x)')


## ---- out.width='0.75\\textwidth', fig.align='center'-------------------------------------------------------------------
lambda_0 <- 3.5
lambda_1 <- 5.0
lambda_2 <- 10.0
lambda_3 <- 15.0
tibble(x = seq(0, 25),
       y_0 = dpois(x, lambda = lambda_0),
       y_1 = dpois(x, lambda = lambda_1),
       y_2 = dpois(x, lambda = lambda_2),
       y_3 = dpois(x, lambda = lambda_3)
) %>% gather(lambda, density, -x) %>% 
  ggplot(aes(x = x, y = density, col = lambda)) +
  geom_line() + 
  geom_point() +
  ylab('P(x)') +
  guides(col = FALSE)


## ---- out.width='0.8\\textwidth', fig.align='center'--------------------------------------------------------------------
lambda <- 7.5
N <- 10^3

Df <- tibble(x = seq(0, 25),
             y_bin = dbinom(x, size = N, prob = lambda/N),
             y_pois = dpois(x, lambda = lambda)
)

p1 <- ggplot(Df, aes(x = x, y_bin)) + geom_line() + geom_point() + ylab('P(x)') + guides(col = FALSE)
p2 <- ggplot(Df, aes(x = x, y_pois)) + geom_line() + geom_point() + ylab('P(x)') + guides(col = FALSE)


plot_grid(p1, p2, labels = c("A", "B"))




## ---- echo=T, results='hide'--------------------------------------------------------------------------------------------
doc_df <- read_csv(here('data/DoctorAUS.csv')) %>%
  mutate(gender = ifelse(sex == 1, 'female', 'male'))

M <- glm(doctorco ~ gender,
         data = doc_df,
         family = poisson)

doc_df_new <- tibble(gender = c('female', 'male'))

doc_df_new %>% 
  add_predictions(M)

doc_df_new %>% 
  add_predictions(M, type='response')



## ---- echo=T, results='hide'--------------------------------------------------------------------------------------------
M_1 <- glm(doctorco ~ gender + insurance,
           data = doc_df,
           family = poisson)

anova(M, M_1, test='Chisq')


## ---- echo=T, results='hide'--------------------------------------------------------------------------------------------
insur_df <- read_csv(here('data/Insurance.csv')) %>%
  mutate(District = factor(District))

M <- glm(Claims ~ District + Group + Age + offset(log(Holders)),
         data = insur_df,
         family = poisson)



## ---- out.width='0.75\\textwidth', fig.align='center'-------------------------------------------------------------------
lambda_0 <- 3.5
lambda_1 <- 5.0
lambda_2 <- 10.0
lambda_3 <- 15.0
tibble(x = seq(0, 25),
       y_0 = dpois(x, lambda = lambda_0),
       y_1 = dpois(x, lambda = lambda_1),
       y_2 = dpois(x, lambda = lambda_2),
       y_3 = dpois(x, lambda = lambda_3)
) %>% gather(lambda, density, -x) %>% 
  ggplot(aes(x = x, y = density, col = lambda)) +
  geom_line() + 
  geom_point() +
  ylab('P(x)') +
  guides(col = FALSE)


## ---- echo=T------------------------------------------------------------------------------------------------------------
x <- rpois(25, lambda = 5)
c(mean(x), var(x), var(x)/mean(x))


## ---- echo=T------------------------------------------------------------------------------------------------------------
x <- rpois(25, lambda = 5)
c(mean(x), var(x), var(x)/mean(x))


## ---- out.width='0.7\\textwidth', fig.align='center'--------------------------------------------------------------------
ggplot(bioChemists, aes(x=art)) + geom_histogram(col='white', binwidth = 1)


## ---- echo=T------------------------------------------------------------------------------------------------------------
var(publications)/mean(publications)


## ---- echo=T------------------------------------------------------------------------------------------------------------
M <- glm(publications ~ 1, family=poisson)
summary(M)$coefficients


## ---- echo=T------------------------------------------------------------------------------------------------------------
M <- glm(publications ~ 1, family=quasipoisson)
summary(M)$coefficients


## ---- out.width='0.9\\textwidth', fig.align='center'--------------------------------------------------------------------

n <- 25

tibble(x = seq(0, n),
       p = dbinom(x, size=n, prob=0.75)
) %>% ggplot(aes(x = x, y = p)) + geom_bar(stat = 'identity')



## ---- out.width='0.8\\textwidth', fig.align='center'--------------------------------------------------------------------

n <- 25

tibble(x = seq(0, n),
       p = dnbinom(x, size=2, prob=0.75)
) %>% ggplot(aes(x = x, y = p)) + geom_bar(stat = 'identity')



## ---- out.width='0.9\\textwidth', fig.align='center'--------------------------------------------------------------------
n <- 25

tibble(x = seq(0, n),
       p = dnbinom(x, size=3, prob=0.5)
) %>% ggplot(aes(x = x, y = p)) + geom_bar(stat = 'identity')



## ---- echo=T------------------------------------------------------------------------------------------------------------
dnbinom(2, 3, 0.75)


## ---- out.width='0.75\\textwidth', fig.align='center'-------------------------------------------------------------------
lambda_0 <- 3.5
lambda_1 <- 5.0
lambda_2 <- 7.0
lambda_3 <- 10.0
tibble(x = seq(0, 25),
       y_0 = dpois(x, lambda = lambda_0),
       y_1 = dpois(x, lambda = lambda_1),
       y_2 = dpois(x, lambda = lambda_2),
       y_3 = dpois(x, lambda = lambda_3),
       y_sum = (y_0 + y_1 + y_2 + y_3)/2
) %>% gather(lambda, density, -x) %>% 
  ggplot(aes(x = x, y = density, col = lambda)) +
  geom_line() + 
  geom_point() +
  ylab('P(x)') +
  guides(col = FALSE)


## ---- echo=T, results='hide'--------------------------------------------------------------------------------------------
M <- glm.nb(publications ~ gender, data = biochemists_Df)
M1 <- glm.nb(publications ~ gender + married + I(children > 0), data = biochemists_Df)



## ---- echo=T------------------------------------------------------------------------------------------------------------
golf_df <- read_csv(here('data/golf_putts.csv')) %>% 
  mutate(failure = attempts - success,
         p = success/attempts)

M <- glm(cbind(success, failure) ~ distance,
         family = binomial(link = 'logit'),
         data = golf_df)


## ----echo=F-------------------------------------------------------------------------------------------------------------
lambda <- 5.5


## ---- out.width='0.75\\textwidth', fig.align='center'-------------------------------------------------------------------
tibble(x = seq(0, 25),
       y = dpois(x, lambda = lambda)
) %>% ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_segment(aes(x = x, xend = x, y = 0, yend = y), color = "grey50") +
  ylab('P(x)')


## ----echo=F-------------------------------------------------------------------------------------------------------------
lambda <- 5.5
n <- 25
z <- 0.2


## ---- out.width='0.75\\textwidth', fig.align='center'-------------------------------------------------------------------
tibble(x = seq(0, n),
       y = ((1-z)*dpois(x, lambda = lambda)) + (z * c(1, rep(0, n)))
) %>% ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_segment(aes(x = x, xend = x, y = 0, yend = y), color = "grey50") +
  ylab('P(x)')


## ---- echo=T, results='hide'--------------------------------------------------------------------------------------------
smoking_df <- read_csv(here('data/smoking.csv'))
M <- glm(cigs ~ educ, data = smoking_df)
M_zip <- zeroinfl(cigs ~ educ, data=smoking_df)

Df_new <- data.frame(educ = seq(20))
# Predited average smoking rate
Df_new %>% 
  add_predictions(M_zip, type='response')

# Predicted average smoking rate of "smokers" 
Df_new %>%
  add_predictions(M_zip, type='count')

# Predicted probability of being a "smoker" 
Df_new %>% 
  add_predictions(M_zip, type='zero') %>% 
  mutate(pred = 1-pred)

