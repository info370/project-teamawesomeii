if(!require(mlbench)){install.packages("mlbench"); require(mlbench)} # common datasets to use
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)} 
if(!require(modelr)){install.packages("modelr"); library(modelr)} 

# some dependencies for caret that aren't automatically installed
if(!require(ModelMetrics)){install.packages("ModelMetrics"); require(ModelMetrics)}
if(!require(recipes)){install.packages("recipes"); require(recipes)}
if(!require(DEoptimR)){install.packages("DEoptimR"); require(DEoptimR)}

if(!require(caret)){install.packages("caret"); require(caret)} # ML package WITHOUT its dependencies. Should not take as long
if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}
if(!require(broom)){install.packages("broom"); require(broom)}
set.seed(370)

if(!require(caret)){install.packages("caret", dependencies = c("Depends", "Suggests")); require(caret)}



# load data
sleep_data <- read.csv("./Sleep Drinking - Sheet1.csv")

sleep_data <- sleep_data %>% dplyr::select(-LarkOwl, -DepressionStatus, -AnxietyStatus, -Stress)

# Scatter plot

p <- ggplot(sleep_data, aes(x = Drinks, y = GPA))  +
  geom_point()


# Feature selection
control <- trainControl(method="repeatedcv", number = 10, repeats = 3)

model <- train(GPA ~., data=sleep_data, method = "knn", trControl = control)

importance <- varImp(model)

ggplot(importance)

linear_model <- lm (GPA ~ Drinks, data = sleep_data)

parameters <- tidy(linear_model)$estimate

sleep_data_small <- sleep_data %>%
  #mutate(GPA_negation = 4.0 - sleep_data$GPA) %>%
  dplyr::select(Drinks, GPA) %>%
  mutate(linear_model_prediction = parameters[1] + parameters[2] * sleep_data$Drinks)

#head(sleep_data)

ggplot(sleep_data_small , aes(Drinks)) +
  geom_point(aes(y= GPA), size = 1) +
  geom_line(aes(y= linear_model_prediction), size = 1, colour = "grey30")

model_linear_function <- function(a, data) {
  a[1] + data$Drinks * a[2]
}

model_function = model_linear_function
measure_distance <- function(mod_params, data) {
  diff <- data$GPA - model_function(mod_params, data)
  sqrt(mean(diff ^ 2))
}



# Here's a line fit with the objective function that lm uses - squares of the residuals
best <- optim(c(3.4, 0), measure_distance, data = sleep_data_small)


# values for the coefficients
parameters
best$par

measure_distance_mad <- function(mod_params, data) {
  diff <- data$GPA - model_function(mod_params, data)
  mean(abs(diff))
}

# make use of a different distance function here (such as mean absolute)
mad_fit <- optim(c(3.4, 0), measure_distance_mad, data = sleep_data_small)
mad_fit$par

sleep_data_small <- sleep_data_small %>%
  mutate(lm_mad = mad_fit$par[1] + mad_fit$par[2] * sleep_data$Drinks)

ggplot(sleep_data_small , aes(Drinks)) +
  geom_point(aes(y= GPA), size = 1) +
  geom_line(aes(y= lm_mad), size = 1, colour = "grey30")


sleep_data_small <- sleep_data_small %>% 
  mutate(
    residuals_mad = GPA - lm_mad,
    residuals = GPA - linear_model_prediction
  )


# plot the residuals to compare them
ggplot(sleep_data_small, aes(ClassesMissed)) +
  geom_point(aes(y=residuals)) + 
  geom_point(aes(y=residuals_mad), color = "red") +
  geom_abline(intercept=0, slope = 0)

summary(linear_model)
cor(sleep_data$ClassesMissed, sleep_data$GPA)
cor(sleep_data$Drinks, sleep_data$GPA)
cor(sleep_data$ClassesMissed, sleep_data$Drinks)




#CLass_skipped analysis

linear_model <- lm (GPA ~ ClassesMissed, data = sleep_data)

parameters <- tidy(linear_model)$estimate

sleep_data_small <- sleep_data %>%
  #mutate(GPA_negation = 4.0 - sleep_data$GPA) %>%
  dplyr::select(ClassesMissed, GPA) %>%
  mutate(linear_model_prediction = parameters[1] + parameters[2] * sleep_data$ClassesMissed)

#head(sleep_data)

ggplot(sleep_data_small , aes(ClassesMissed)) +
  geom_point(aes(y= GPA), size = 1) +
  geom_line(aes(y= linear_model_prediction), size = 1, colour = "grey30")

model_linear_function <- function(a, data) {
  a[1] + data$ClassesMissed * a[2]
}

model_function = model_linear_function
measure_distance <- function(mod_params, data) {
  diff <- data$GPA - model_function(mod_params, data)
  sqrt(mean(diff ^ 2))
}

# Here's a line fit with the objective function that lm uses - squares of the residuals
best <- optim(c(3.4, 0), measure_distance, data = sleep_data_small)


# values for the coefficients
parameters
best$par

measure_distance_mad <- function(mod_params, data) {
  diff <- data$GPA - model_function(mod_params, data)
  mean(abs(diff))
}

# make use of a different distance function here (such as mean absolute)
mad_fit <- optim(c(3.4, 0), measure_distance_mad, data = sleep_data_small)
mad_fit$par

sleep_data_small <- sleep_data_small %>%
  mutate(lm_mad = mad_fit$par[1] + mad_fit$par[2] * sleep_data$ClassesMissed)

sleep_data_small <- sleep_data_small %>% 
  mutate(
    residuals_mad = GPA - lm_mad,
    residuals = GPA - linear_model_prediction
  )


# plot the residuals to compare them
ggplot(sleep_data_small, aes(ClassesMissed)) +
  geom_point(aes(y=residuals)) + 
  geom_point(aes(y=residuals_mad), color = "red") +
  geom_abline(intercept=0, slope = 0)

summary(linear_model)








