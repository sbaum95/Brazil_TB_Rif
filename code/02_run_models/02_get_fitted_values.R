# Author: Sarah Baum
# Created: 2024-03-22

# Description: Get fitted values from all models

# Create function that store fitted values --------------------------------
get_fitted_values <- function (model_name) {
  
  # Fit values 
  fitted_values <- gratia::fitted_values(fitted_models[[model_name]][[1]], 
                                 data = fitted_models[[model_name]][[2]], 
                                 scale = "response")
  
  return(fitted_values)
  
}


# Get fitted values from models -------------------------------------------
fitted_values <- list()

model_name <- names(fitted_models)

fitted_values_list <- lapply(model_name, get_fitted_values) 

fitted_values <- setNames(fitted_values_list, model_name)
