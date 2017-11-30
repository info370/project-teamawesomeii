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

if(!require(caret)){install.packages("caret", dependencies = c("Depends", "Suggests")); require(caret)}

# load data
student_data <- read.csv("./student-mat.csv")

# Violin plot

# Dalc data
student_data$Dalc <- as.factor(student_data$Dalc)
p <- ggplot(student_data, aes(x = Dalc, y = G3)) + 
  geom_violin(trim = FALSE)
p + geom_boxplot(width=0.1)

# Walc data
student_data$Walc <- as.factor(student_data$Walc)
p <- ggplot(student_data, aes(x = Walc, y = G3)) + 
  geom_violin(trim = FALSE)
p + geom_boxplot(width=0.1)


# Feature selection
student_data <- student_data %>% select(-failures)
G1_features <- student_data %>% select(-G2, -G3)
G2_features <- student_data %>% select(-G1, -G3)
G3_features <- student_data %>% select(-G1, -G2)
Dalc_features <- student_data %>% select(-Walc)
control <- trainControl(method="repeatedcv", number = 10, repeats = 3)

model1 <- train(G1 ~., data=G1_features, method = "knn", trControl = control)
model2 <- train(G2 ~., data=G2_features, method = "knn", trControl = control)
model3 <- train(G3 ~., data=G3_features, method = "knn", trControl = control)

alc_model <- train(Dalc ~., data=Dalc_features, method = "knn", trControl = control)

importance1 <- varImp(model1)
importance2 <- varImp(model2)
importance3 <- varImp(model3)
importance_alc <- varImp(alc_model)

ggplot(importance1)
ggplot(importance2)
ggplot(importance3)
ggplot(importance_alc)


# splitting boston data into train+validate and test sets

split_proportion = 0.8 # specify proportion of data used for training

# select outcome variable
outcome <- student_data %>% dplyr::select(G3)

# randomly select indices for train/validate set
train_ind <- createDataPartition(outcome$G3, p = split_proportion, list = FALSE)
student_data_train <- student_data[train_ind,] # get training data
student_data_test <- student_data[-train_ind,] # get test data

student_test_x <- student_data %>% dplyr::select(-G3) # select predictor data for test set
student_test_y <- student_data %>% dplyr::select(G3) # select outcome data for test set

ctrl <- trainControl(method = "cv", number=5)