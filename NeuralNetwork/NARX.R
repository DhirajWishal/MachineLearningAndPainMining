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
summary(load_data)

# Also make sure to set the seed!
set.seed(123)

# Get all the class information from the data set.
sapply(load_data, class)
summary(load_data)

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
    # Iteratively create the prior records if the count is more than 1.
    if (prior > 1) {
      for (x in 1:(prior - 1)) {
        training_set = shift.column(
          data = training_set,
          columns = c("Nine", "Ten", "Eleven"),
          newNames = c(
            paste("N", as.character(x + 1), sep = ""),
            paste("T", as.character(x + 1), sep = ""),
            paste("E", as.character(x + 1), sep = "")
          ),
          len = x
        )
        
        testing_set = shift.column(
          data = testing_set,
          columns = c("Nine", "Ten", "Eleven"),
          newNames = c(
            paste("N", as.character(x + 1), sep = ""),
            paste("T", as.character(x + 1), sep = ""),
            paste("E", as.character(x + 1), sep = "")
          ),
          len = x
        )
      }
    }
    
    # Generate the predicted/ output column.
    training_set = shift.column(
      data = training_set,
      columns = "Eleven",
      newNames = "Tomorrow",
      len = prior
    )
    
    testing_set = shift.column(
      data = testing_set,
      columns = "Eleven",
      newNames = "Tomorrow",
      len = prior
    )
    
    # Rename the first columns to fit the naming scheme.
    names(training_set)[names(training_set) == "Nine"] <- "N1"
    names(training_set)[names(training_set) == "Ten"] <- "T1"
    names(training_set)[names(training_set) == "Eleven"] <- "E1"
    
    names(testing_set)[names(testing_set) == "Nine"] <- "N1"
    names(testing_set)[names(testing_set) == "Ten"] <- "T1"
    names(testing_set)[names(testing_set) == "Eleven"] <- "E1"
    
    # Strip and remove the first column, and use only the required last columns.
    training_set = as.data.frame(training_set[2:ncol(training_set)])
    testing_set = as.data.frame(testing_set[2:ncol(testing_set)])
    
    return(list("training" = training_set, "testing" = testing_set))
  }

# Create a function to train a NN model and test it.
# https://medium.com/geekculture/introduction-to-neural-network-2f8b8221fbd3
train_predict_plot_nn <-
  function(prior,
           training,
           testing,
           hidden_layers = c(5, 3),
           reps = 100) {
    # Generate the training and testing data.
    data_set <- generate_training_testing(prior, training, testing)
    
    # Prepare data for the neural network.
    column_names <- names(data_set$training)
    column_formula <-
      as.formula(paste("Tomorrow ~", paste(column_names[!column_names %in% "Tomorrow"], collapse = " + ")))
    
    # Construct the neural network.
    model <- neuralnet(
      column_formula,
      data = data_set$training,
      hidden = hidden_layers,
      linear.output = T,
      rep = reps
    )
    
    result <- predict(model, data_set$testing)
    
    # Plot the predicted results.
    plot(
      result,
      data_set$testing$Tomorrow,
      xlab = "Predicted Values",
      ylab = "Observed Values",
      main = paste("T-", as.character(prior), sep = ""),
      abline(a = 0, b = 1)
    )
    
    # Evaluate the model.
    predicted <- result * abs(diff(range(data_set$testing$E1))) + min(data_set$testing$E1)
    actual <- data_set$testing * abs(diff(range(data_set$testing$E1))) + min(data_set$testing$E1)
    actual <- actual[!(actual$Tomorrow == 0), ]
    
    return(list("RMSE" = RMSE(predicted, actual$Tomorrow), "MAE" = MAE(predicted, actual$Tomorrow), "MAPE" = MAPE(predicted, actual$Tomorrow)))
  }

layers <- c(3)

# Calculate and show t-1
train_predict_plot_nn(1, training_data, testing_data, hidden_layers = layers, reps = 10)

# Calculate and show t-2
train_predict_plot_nn(2, training_data, testing_data, hidden_layers = layers, reps = 10)

# Calculate and show t-3
train_predict_plot_nn(3, training_data, testing_data, hidden_layers = layers, reps = 10)

# Calculate and show t-4
train_predict_plot_nn(4, training_data, testing_data, hidden_layers = layers, reps = 10)

# Calculate and show t-7
train_predict_plot_nn(7, training_data, testing_data, hidden_layers = layers, reps = 1)