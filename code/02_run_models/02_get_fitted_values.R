# Author: Sarah Baum
# Created: 2024-03-22
# Updated:

# Description: Get fitted values from all models




# Load data and model output ----------------------------------------------
load("data/mdf_mun_new_grp.Rdata")
load("data/mdf_prev_ind.Rdata")
load("output/fitted_models.Rdata")


# Create function that store fitted values --------------------------------
get_fitted_values <- function (model_name) {
  
  # Pull data frame that was fed into model 
  data <- parse_expr(fitted_models[[model_name]]$data_call)
  
  # Fit values 
  fitted_values <- fitted_values(fitted_models[[model_name]], data = eval(data), scale = "response")
  
  return(fitted_values)
  
}

# Get fitted values from models -------------------------------------------
fitted_values <- list()

model_name <- names(fitted_models)

fitted_values_list <- lapply(model_name, get_fitted_values) 

fitted_values <- setNames(fitted_values_list, model_name)


# Store fitted values -----------------------------------------------------
save(fitted_values, file = "output/fitted_values.Rdata")



