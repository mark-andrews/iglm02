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
