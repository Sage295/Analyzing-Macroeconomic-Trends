#coded by Sejal Mogalgiddi, Tony Tran, Ryan Jones,and Gabriel Alvarado
#loading the dataset into the compiler
df <- data.frame(Economic_Indicators_And_Inflation_1_)
#removing years 2024, and 2-025 since they are predictions
df2 <- subset(df, Year < 2024)
summary(df2)


#building the model we use to test for model diagnostics
model_interactions <- lm(Unemployment.Rate.... ~ 
                           GDP..in.billion.USD. * Inflation.Rate.... +
                           Inflation.Rate.... * Economic.Growth.... +
                           GDP..in.billion.USD. * Economic.Growth....,
                     data = df2)
# View model summary
summary(model_interactions)

# for correlation matrix, testing for collinearity
cor_matrix <- cor(df2[, c("Unemployment.Rate....", 
                          "GDP..in.billion.USD.", 
                          "Inflation.Rate....", 
                          "Economic.Growth....")], 
                  use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)

# this was when we finihsed testing for outliers, we ended up removing 182 and 245 since they were obvious outliers

df_adjusted_cleaned <- df_adjusted[-c(182, 245), ]
dim(df2)
dim(df_adjusted)
dim(df_adjusted_cleaned)


library(car)
#cooks distance

cooks_d <- cooks.distance(model_interactions)
cooks_d[cooks_d > 1]

#leverage
n <- nrow(df2)  
p <- length(coefficients(model_interactions)) 

# Compute leverage
leverage <- hatvalues(model_interactions)

threshold <- (2 * p) / n


high_leverage <- leverage[leverage > threshold]
print(high_leverage)


#jackknife residual
stud_res <- rstudent(model_interactions) 

plot(model_interactions$fitted.values, stud_res)
abline(h = 0)

t_crit <- qt(0.975, df.residual(model_interactions))
print(t_crit)

head(sort(stud_res), 20)
tail(sort(stud_res), 20)
# print out all the values that were shown as outliers by the 3 outlier tests
df[c(43, 182, 260, 261, 262, 263, 267, 269, 268, 270), ] 
# remove Turkey
df_adjusted_cleaned <- subset(df_adjusted_cleaned, Country != "Turkey")

#building the model we use to test for model diagnostics
full_model <- lm(Unemployment.Rate.... ~ 
                           GDP..in.billion.USD. * Inflation.Rate.... +
                           Inflation.Rate.... * Economic.Growth.... +
                           GDP..in.billion.USD. * Economic.Growth....,
                         data = df_adjusted_cleaned)
plot(full_model )
#testing outliers again
#cooks distance

cooks_d <- cooks.distance(full_model)
cooks_d[cooks_d > 1]

#leverage
n <- nrow(df_adjusted_cleaned)  
p <- length(coefficients(full_model)) 

# Compute leverage
leverage <- hatvalues(full_model)

threshold <- (2 * p) / n


high_leverage <- leverage[leverage > threshold]
print(high_leverage)


#jackknife residual
stud_res <- rstudent(full_model) 

plot(full_model$fitted.values, stud_res)
abline(h = 0)

t_crit <- qt(0.975, df.residual(full_model))
print(t_crit)

head(sort(stud_res), 20)
tail(sort(stud_res), 20)
# print out the outlier that the outlier tests shows
df[c(252), ] 
library(car)
#box cox transformation
boxCox(full_model)


#sqrt transformation just to double check
full_model_sqrt <- lm(sqrt(Unemployment.Rate....) ~ 
                        GDP..in.billion.USD. * Inflation.Rate.... +
                        Inflation.Rate.... * Economic.Growth.... +
                        GDP..in.billion.USD. * Economic.Growth....,
                      data = df_adjusted_cleaned)

plot(full_model_sqrt)

#inverse transformation just to double check
full_model_inv_sqrt <- lm(I((Unemployment.Rate....)^(-1/2)) ~ GDP..in.billion.USD. +
                            Inflation.Rate....,
                          data = df_adjusted_cleaned)


#helps with the selection
df_adjusted_cleaned$log_unemp <- log(df_adjusted_cleaned$Unemployment.Rate....)

#log transformation suggested by box cox, this is what we went with
model_log <- lm(log_unemp ~ 
                  GDP..in.billion.USD. * Inflation.Rate.... +
                  Inflation.Rate.... * Economic.Growth.... +
                  GDP..in.billion.USD. * Economic.Growth....,
                data = df_adjusted_cleaned)

# backwards selection
library(olsrr)
model_backward_log <- ols_step_backward_p(model_log, prem = 0.1, details = TRUE)
summary(model_backward_log$model)  # View final model
print(model_backward_log) 
summary(model_backward)

#stepwise selection
model_step2 <- ols_step_both_aic(model_log, details=T)
print(model_step2)
summary(model_step2$model)

#forward selection
model_forward <- ols_step_forward_p(model_log, penter = 0.1, details=TRUE)
print(model_forward)
summary(model_forward$model)








