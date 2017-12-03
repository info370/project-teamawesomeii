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
if(!require(splines)){install.packages("splines"); require(splines)}
if(!require(DAAG)){install.packages("DAAG"); require(DAAG)}


# load data
mich_data <- read.csv("./gpa1.csv")
mich_data <- mich_data %>% dplyr::select(-X)
low_drink <- mich_data %>% filter(alcohol < 3)
high_drink <- mich_data %>% filter(alcohol >= 3)

# Scatterplot

all_plot <- ggplot(mich_data, aes(x = alcohol, y = colGPA))  +
  geom_point()
all_plot
low_plot <- ggplot(low_drink, aes(x = alcohol, y = colGPA))  +
  geom_point()
high_plot <- ggplot(high_drink, aes(x = alcohol, y = colGPA))  +
  geom_point()

# Feature dplyr::selection

# all data
control <- trainControl(method="repeatedcv", number = 10, repeats = 3)
colGPA_features <- mich_data %>% dplyr::select(-hsGPA)

model1 <- train(colGPA ~., data=colGPA_features, method = "knn", trControl = control)

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
predictors(importance_high)
ggplot(importance_high)
selected_features_high <- c("ACT", "greek", "PC", "siblings", "bgfriend","voluntr")
# tests

# try t-test for low vs high groups

#model

# skipped classes plot
skip_plot <- ggplot(mich_data, aes(x = skipped, y = colGPA, size=alcohol))  +
  geom_point()
skip_plot
linearMod_skip <- lm(colGPA ~ skipped, data=mich_data)
linearMod_skip
summary(linearMod_skip)
skip_plot + geom_abline(slope=-0.08952, intercept = 3.15308)

p <- ggplot(data=mich_data) + geom_point(aes(x=alcohol, y=colGPA)) 
p + geom_abline(slope=0.004655, intercept=3.047889)

#basic linear model

linearMod <- lm(colGPA ~ alcohol, data=mich_data)
linearMod

#correlation
alc <- mich_data$alcohol
gpa <- mich_data$colGPA
skip <- mich_data$skipped
cor(skip,gpa)

#residual plot
mich_data$predAlc <- .004655*mich_data$alcohol + 3.047889
mich_data$residGPA <- mich_data$colGPA - mich_data$predAlc
p3 <- ggplot(data=mich_data) + geom_point(aes(x=alcohol, y=residGPA))
p3+geom_abline(slope=0, intercept = 0, color="red")

#splines modeling
# mod2 <- lm(colGPA ~ ns(alcohol, 2), data=mich_data)
# mod3 <- lm(colGPA ~ ns(alcohol, 3), data=mich_data)
# mod4 <- lm(colGPA ~ ns(alcohol, 4), data=mich_data)
# mod5 <- lm(colGPA ~ ns(alcohol, 5), data=mich_data)
# mod10 <- lm(colGPA ~ ns(alcohol, 10), data=mich_data)
# mod20 <- lm(colGPA ~ ns(alcohol, 20), data=mich_data)

# k-fold cross validation


 # glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
 #                            lambda = c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1,1))
 # glmnet_ctrl <- trainControl(method="cv", number=5)
 # glmnet_fit <- train(colGPA ~ ., data=mich_data[,c(selected_features_high, "colGPA")],
 #                     method="glmnet",
 #                    tuneGrid=glmnet_grid,
 #                    trControl=glmnet_ctrl)
