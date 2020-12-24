
library(dplyr)   ## For data transformation
library(caret)   ## Provides a nice modelling framework
library(rattle)  ## Used to create nice tree plots


# Load in the processed data
cancer_data <- readr::read_csv("data/processed/processed_data.csv")


# First, lets change our class attribute to a factor
cancer_data <- cancer_data %>% mutate(class = as.factor(case_when(class == 0 ~ "Benign", class == 1 ~ "Malignant")))

# Set a seed to ensure reproducibility
set.seed(101)

# Creating a 10% holdout (by default this is a stratified sample)
train_ind <- createDataPartition(cancer_data$class, p = 0.9, list = FALSE)

# Create training and test data sets
train_data <- cancer_data[train_ind,]
holdout_data <- cancer_data[-train_ind,]


# Create an object with the settings I want for cross validation
# I've opted for a 5-fold cross validation
my_training_settings <- trainControl(method = "cv", number = 5)

# Now I train my model, predicting class based on all the variables using the rpart model
my_model <- train(class ~ ., 
                  data = train_data,
                  method = "rpart",
                  trControl = my_training_settings,
                  tuneLength = 5,
                  metric = "Accuracy")

# Lets have a look at the output from my training
my_model
# This shows that models with a low complexity parameter performs best
# A low cp chooses a more complex model (i.e it allows more splits)

# Now I'll visualise the best tree
fancyRpartPlot(my_model$finalModel, 
               main = "Breast Cancer Classification", 
               sub = "")


# To double check that I'm not overfitting, do a final check against the holdout data
confusionMatrix(predict(my_model, holdout_data), 
                holdout_data$class)
# The model performs similar here compared to the CV accuracy
# Getting ~ 92% accuracy on the holdout data

