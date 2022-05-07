# Install the ms excel reading package.
if (!require("readxl"))
  install.packages("readxl")

if (!require("neuralnet"))
  install.packages("neuralnet")

if (!require("useful"))
  install.packages("useful")

library("readxl")
library("neuralnet")
library("useful")
library(caret)

# Load the data.
load_data <- read_excel("UoW_load.xlsx")
load_data$Dates <- as.Date(load_data$Dates)

# Let's also rename the columns for convenience.
names(load_data)[names(load_data) == "09:00"] <- "Nine"
names(load_data)[names(load_data) == "10:00"] <- "Ten"
names(load_data)[names(load_data) == "11:00"] <- "Eleven"
load_data

# Also make sure to set the seed!
set.seed(123)

# Get all the class information from the data set.
sapply(load_data, class)
summary(load_data)
as.numeric(as.POSIXct(load_data$Dates))

# New data frame so that we don't have to worry about the date-time format.
homogeneous_data <- load_data
homogeneous_data$Dates = as.numeric(load_data$Dates)
sapply(homogeneous_data, class)
summary(homogeneous_data)

# Normalize the data.
normalized = as.data.frame(sapply(homogeneous_data, function(x)
  (x - min(x)) / (max(x) - min(x))))
summary(normalized)

# Now let's split the data into two parts, training and testing.
training_data <- as.data.frame(normalized[1:430,])
testing_data <- as.data.frame(normalized[431:500,])

# Utility function to generate training and testing data based on the t-prior.
generate_training_testing <-
  function(prior, training_set, testing_set) {
    for (x in 1:(prior - 1)) {
      training_set = shift.column(
        data = training_set,
        columns = "Eleven",
        newNames = paste("T", as.character(x), sep = ""),
        up = FALSE,
        len = x
      )
      
      testing_set = shift.column(
        data = testing_set,
        columns = "Eleven",
        newNames = paste("T", as.character(x), sep = ""),
        up = FALSE,
        len = x
      )
    }
    
    training_set = shift.column(
      data = training_set,
      columns = "Eleven",
      newNames = "Predict",
      up = TRUE
    )
    
    testing_set = shift.column(
      data = testing_set,
      columns = "Eleven",
      newNames = "Predict",
      up = TRUE
    )
    
    training_set = as.data.frame(training_set[4:ncol(training_set)])
    testing_set = as.data.frame(testing_set[4:ncol(testing_set)])
    
    return(list("training" = training_set, "testing" = testing_set))
  }

# Create a function to train a NN model and test it.
train_predict_plot_nn <- function(training, testing) {
  # Calculate the best number of hidden layers.
  # https://medium.com/geekculture/introduction-to-neural-network-2f8b8221fbd3
  hidden_layer_one = (ncol(training) + 1) / 2
  hidden_layer_two = 3
  
  # Prepare data for the neural network.
  column_names <- names(training)
  column_formula <-
    as.formula(paste("Predict ~", paste(column_names[!column_names %in% "Predict"], collapse = " + ")))
  
  # Construct the neural network.
  nn <- neuralnet(
    column_formula,
    data = training,
    hidden = c(hidden_layer_one, hidden_layer_two),
    linear.output = T
  )
  
  plot(nn)
  # result <- predict(nn, testing)
  
  #Test the resulting output
  temp_test <- subset(testing, select = names(testing))
  head(temp_test)
  nn.results <- compute(nn, temp_test)
  results <- data.frame(actual = testing$Predict, prediction = nn.results$net.result)
  
  rounded_results <- sapply(results, round, digits=0)
  rounded_results = data.frame(rounded_results)
  attach(rounded_results)
  confusionMatrix(as.factor(prediction), as.factor(actual))
}

# Calculate and show t-1
# data_set <-
#   generate_training_testing(1, training_data, testing_data)
# train_predict_plot_nn(data_set$training,
#                       data_set$testing)

# Calculate and show t-2
data_set <-
  generate_training_testing(2, training_data, testing_data)
train_predict_plot_nn(data_set$training,
                      data_set$testing)

# Calculate and show t-3
data_set <-
  generate_training_testing(3, training_data, testing_data)
train_predict_plot_nn(data_set$training,
                      data_set$testing)

# Calculate and show t-4
data_set <-
  generate_training_testing(4, training_data, testing_data)
train_predict_plot_nn(data_set$training,
                      data_set$testing)

# Calculate and show t-7
data_set <-
  generate_training_testing(7, training_data, testing_data)
train_predict_plot_nn(data_set$training,
                      data_set$testing)