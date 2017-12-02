if(!require(mlbench)){install.packages("mlbench"); require(mlbench)} # common datasets to use
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)} 
if(!require(modelr)){install.packages("modelr"); library(modelr)} 

# some dependencies for caret that aren't automatically installed
if(!require(ModelMetrics)){install.packages("ModelMetrics"); require(ModelMetrics)}
if(!require(recipes)){install.packages("recipes"); require(recipes)}
if(!require(DEoptimR)){install.packages("DEoptimR"); require(DEoptimR)}

if(!require(caret)){install.packages("caret"); require(caret)} # ML package WITHOUT its dependencies. Should not take as long
if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}
set.seed(370)
if(!require(glmnet)){install.packages("glmnet"); require(glmnet)}
if(!require(caret)){install.packages("caret", dependencies = c("Depends", "Suggests")); require(caret)}

# load data
mich_data <- read.csv("./gpa1.csv")
mich_data <- mich_data %>% dplyr::select(-X)
low_drink <- mich_data %>% filter(alcohol < 3)
high_drink <- mich_data %>% filter(alcohol >= 3)

# Scatterplot

all_plot <- ggplot(mich_data, aes(x = alcohol, y = colGPA))  +
  geom_point()
low_plot <- ggplot(low_drink, aes(x = alcohol, y = colGPA))  +
  geom_point()
high_plot <- ggplot(high_drink, aes(x = alcohol, y = colGPA))  +
  geom_point()

# Feature dplyr::selection

# all data
control <- trainControl(method="repeatedcv", number = 10, repeats = 3)
colGPA_features <- mich_data %>% dplyr::select(-hsGPA)

model1 <- train(colGPA ~., data=mich_data, method = "knn", trControl = control)

importance1 <- varImp(model1)

ggplot(importance1)

# low
control <- trainControl(method="repeatedcv", number = 10, repeats = 3)
colGPA_features_low <- low_drink %>% dplyr::select(-hsGPA)

model_low <- train(colGPA ~., data=colGPA_features_low, method = "knn", trControl = control)

importance_low <- varImp(model_low)

ggplot(importance_low)

# high
control <- trainControl(method="repeatedcv", number = 10, repeats = 3)
colGPA_features_high <- high_drink %>% dplyr::select(-hsGPA)

model_high <- train(colGPA ~., data=colGPA_features_high, method = "knn", trControl = control)

importance_high <- varImp(model_high)

ggplot(importance_high)

# tests

# try t-test for low vs high groups

#model

p <- ggplot(data=mich_data) + geom_point(aes(x=alcohol, y=colGPA)) 
p + geom_abline(slope=0.004655, intercept=3.047889)

#basic linear model

linearMod <- lm(colGPA ~ alcohol, data=mich_data)
linearMod

alc <- mich_data$alcohol
gpa <- mich_data$colGPA
cor(alc,gpa)

# glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
#                            lambda = c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1,1))
# glmnet_ctrl <- trainControl(method="cv", number=5)
# glmnet_fit <- train(colGPA ~ ., data=mich_data,
#                     method="glmnet",
#                     tuneGrid=glmnet_grid,
#                     trControl=glmnet_ctrl)
