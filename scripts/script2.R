library(tidyverse)

doctor_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm02/master/data/DoctorAUS.csv")

M_10 <- glm(doctorco ~ sex, 
            family = poisson(link = 'log'),
            data = doctor_df)

summary(M_10)

betas <- coef(M_10)

# prediction of log of rate (log of lambda) for female (sex = 1), we have
betas[1] + betas[2] * 1

# prediction of rate (lambda) for female (sex = 1), we have
exp(betas[1] + betas[2] * 1)


# prediction of log of rate (log of lambda) for male (sex = 0), we have
betas[1] + betas[2] * 0

# prediction of rate (lambda) for male (sex = 0), we have
exp(betas[1] + betas[2] * 0)

# meaning of the coefficient
exp(betas[2])

tibble(sex = c(0, 1)) %>% 
  add_predictions(M_10, type = 'response')


# modify data -------------------------------------------------------------

doctor_df <- mutate(doctor_df, age = age * 100)

M_11 <- glm(doctorco ~ age + sex,
            family = poisson(link = 'log'),
            data = doctor_df)

summary(M_11)

coef(M_11)[2:3] %>% exp()


# model comparison --------------------------------------------------------


M_12 <- glm(doctorco ~ age + sex + insurance,
            family = poisson(link = 'log'),
            data = doctor_df)

anova(M_11, M_12, test = 'Chisq')


# exposure and offset -----------------------------------------------------

insurance_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm02/master/data/Insurance.csv")

M_13 <- glm(Claims ~ Age + offset(log(Holders)),
            family = poisson(link = 'log'),
            data = insurance_df)

tibble(Age = unique(insurance_df$Age),
       Holders = 100) %>% 
  add_predictions(M_13, type = 'response')


# Means and variances of Poissons -----------------------------------------

x <- rpois(10000, lambda = 5)
c(mean(x), var(x), mean(x)/var(x))


# overdispersion ----------------------------------------------------------

biochem_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglm02/master/data/biochemist.csv")

ggplot(biochem_df, aes(x = publications)) + geom_bar()

biochem_df %>% summarize(mean(publications),
                         var(publications))

M_14 <- glm(publications ~ 1, 
            data = biochem_df, 
            family = poisson(link = 'log'))

M_15 <- glm(publications ~ 1, 
            data = biochem_df,
            family = quasipoisson(link = 'log'))

M_16 <- glm.nb(publications ~ 1, data = biochem_df)

M_17 <- glm.nb(publications ~ gender + married + prestige,
               data = biochem_df)

summary(M_17)
