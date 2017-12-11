if(!require(mlbench)){install.packages("mlbench"); require(mlbench)} # common datasets to use
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)} 
if(!require(modelr)){install.packages("modelr"); library(modelr)} 

if(!require(caret)){install.packages("caret"); require(caret)} # ML package WITHOUT its dependencies. Should not take as long
if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}
if(!require(broom)){install.packages("broom"); require(broom)}
set.seed(370)

if(!require(caret)){install.packages("caret", dependencies = c("Depends", "Suggests")); require(caret)}

# load data
uci_data <- read.csv("./student-mat.csv", stringsAsFactors = FALSE)

# Violin plots

# Estimate a linear regression model
dalc_linear_model <- lm(G3 ~ Dalc, data=uci_data)
cf <- coef(dalc_linear_model)

# Dalc data
uci_data$Dalc <- as.factor(uci_data$Dalc)
p <- ggplot(uci_data, aes(x = Dalc, y = G3)) + 
  geom_violin(trim = FALSE) +
  stat_summary(aes(group=1),fun.y=mean, geom="point", color="red", size=3) +
  geom_abline(slope=cf[2], intercept=cf[1], lwd=.8)
p + geom_boxplot(width=0.1)

summary(dalc_linear_model)
cor(as.numeric(uci_data$Dalc), uci_data$G3)

# Walc data
uci_data$Walc <- as.factor(uci_data$Walc)
p <- ggplot(uci_data, aes(x = Walc, y = G3)) + 
  geom_violin(trim = FALSE)
p + geom_boxplot(width=0.1)

# Feature selection for whole dataset
uci_data <- uci_data %>% dplyr::select(-failures)
final_grade_features <- uci_data %>% dplyr::select(-G1, -G2)
control <- trainControl(method="repeatedcv", number = 10, repeats = 3)

model <- train(G3 ~., data=final_grade_features, method = "knn", trControl = control)

importance <- varImp(model)

ggplot(importance)
# absences is the most important feature

# model for absences
linear_model <- lm(G3 ~ absences, data=uci_data)
parameters = tidy(linear_model)$estimate
absences_model_data <- uci_data %>%
  mutate(
    linear_model_prediction = parameters[1] + parameters[2] * uci_data$absences
  )
<<<<<<< HEAD
summary(linear_model)
ggplot(uci_data , aes(absences)) +
=======

ggplot(absences_model_data , aes(absences)) +
>>>>>>> 26c2f04daeacb63acf09a7dad1c583f8e128a57a
  geom_point(aes(y= G3)) +
  geom_line(aes(y= linear_model_prediction), color = "red")

model_linear_function <- function(a, data) {
  a[1] + data$absences * a[2]
}

model_function = model_linear_function
measure_distance <- function(mod_params, data) {
  diff <- data$G3 - model_function(mod_params, data)
  sqrt(mean(diff ^ 2))
}

# Here's a line fit with the objective function that lm uses - squares of the residuals
best <- optim(c(0, 0), measure_distance, data = absences_model_data)

# values for the coefficients
best$par
# compare to coefficients of lm()
parameters

measure_distance_mad <- function(mod_params, data) {
  diff <- data$G3 - model_function(mod_params, data)
  mean(abs(diff))
}

# make use of a different distance function here (such as mean absolute)
mad_fit <- optim(c(0, 0), measure_distance_mad, data = absences_model_data)
mad_fit$par

absences_model_data <- absences_model_data %>%
  mutate(lm_mad = mad_fit$par[1] + mad_fit$par[2] * uci_data$absences)

absences_model_data <- absences_model_data %>% 
  mutate(
    residuals_mad = G3 - lm_mad,
    residuals = G3 - linear_model_prediction
  )

# plot residuals
ggplot(absences_model_data, aes(absences)) +
  geom_point(aes(y=residuals)) + 
  geom_point(aes(y=residuals_mad), color = "red") +
  geom_abline(intercept=0, slope = 0)

summary(linear_model)
cor(as.numeric(uci_data$absences), uci_data$G3)


# create dummy variables for Dalc
uci_data$Dalc <- as.factor(uci_data$Dalc)
dmy <- dummyVars("~ Dalc", data=uci_data)
dmy_df <- data.frame(predict(dmy, newdata = uci_data), uci_data$G3)
colnames(dmy_df)[6] <- "G3"

predict(dmy, uci_data)

# feature selection using dummy variables
control <- trainControl(method = "repeatedcv", number=10, repeats = 3)
model <- train(G3 ~ ., data=dmy_df, method = "knn", trControl = control)
importance <- varImp(model)
ggplot(importance)

G3_features <- uci_data %>% 
  mutate(Dalc.1 = dmy_df$Dalc.1, Dalc.2 = dmy_df$Dalc.2, Dalc.3 = dmy_df$Dalc.3,
         Dalc.4 = dmy_df$Dalc.4, Dalc.5 = dmy_df$Dalc.5) %>% 
  dplyr::select(-G1, -G2)
control <- trainControl(method = "repeatedcv", number=10, repeats = 3)
model <- train(G3 ~ ., data=G3_features, method = "knn", trControl = control)
importance <- varImp(model)
ggplot(importance)
