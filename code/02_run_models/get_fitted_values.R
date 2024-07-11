# Description: Get fitted values from all models

# Create function that stores fitted values --------------------------------
get_fitted_values <- function (model_name) {
  
  # Get fitted values and store original data
  fitted_values <- gratia::fitted_values(fitted_models[[model_name]][[1]], 
                                 data = fitted_models[[model_name]][[2]], 
                                 scale = "response")
  
  return(fitted_values)
  
}
