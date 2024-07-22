source("code/dependencies.R")


## Write function that produces univariate coefficients for each covariate 
get_reg_output <- function(case_type, var, covariates, data){
  

# Probability of being tested ---------------------------------------------
  list <- list()
  
  # Univariate
  uni_tested_string <- paste("tested ~", paste(var))

  uni_tested <- glm(as.formula(uni_tested_string), 
                  family = binomial (link = "logit"), 
                  data = data)
  
  df_name <- paste0("uni_tested", var)
  
  list[[df_name]] <- uni_tested
  
  
  # Probability of RR-TB ----------------------------------------------------
  
  # Univariate
  uni_result_string <- paste("result ~", paste(var))
  
  uni_result <- glm(as.formula(uni_result_string), 
                    family = binomial (link = "logit"), 
                    data = data %>% filter(tested == "tested"))
  
  df_name <- paste0("uni_result", var)
  
  list[[df_name]] <- uni_result
  
  return(list(uni_tested,uni_result))

}


## Output to stargazer that is built for ORs
stargazer2 <- function(model, odd.ratio = F, ...) {
  
  coefOR2 <- lapply(model, function(x) exp(coef(x)))
  seOR2 <- lapply(model, function(x) exp(coef(x)) * summary(x)$coef[, 2])
  p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
  ciOR2 <-  lapply(model, function(x) as.matrix(exp(confint.default(x))))
  
  stargazer(model, type = "text", digits = 2, coef = coefOR2, se = seOR2, p = p2, ci.custom = ciOR2)
  
  
}






# Run functions -----------------------------------------------------------

## Get model results for new cases 
var = c("sex", "hiv_status", "age_cat", "health_unit")

new_output <- lapply(var, get_reg_output, 
                 data = mdf_new_ind, 
                 case_type = "New", 
                 covariates = c("sex", "hiv_status", "age_cat", "health_unit"))




## Get model results for previous cases 
var = c("sex", "hiv_status", "age_cat", "health_unit", "tratamento")

prev_output <- lapply(var, get_reg_output, 
                     data = mdf_prev_ind, 
                     case_type = "Previous", 
                     covariates = c("sex", "hiv_status", "age_cat", "health_unit", "tratamento"))




## Prepare results for stargazer
model_names <- c("uni_tested", "uni_result")

new_output <- lapply(new_output, function(sublist) {setNames(sublist, model_names)})
prev_output <- lapply(prev_output, function(sublist) {setNames(sublist, model_names)})


## Pull and store model results only
new_model_list <- unlist(new_output, recursive = FALSE)

new_model_list[["multi_tested"]] <-  glm(tested ~ sex + hiv_status + age_cat + health_unit, 
                                   family = binomial (link = "logit"), 
                                   data = mdf_new_ind)

new_model_list[["multi_result"]] <-  glm(result ~ sex + hiv_status + age_cat + health_unit, 
                                   family = binomial (link = "logit"), 
                                   data = mdf_new_ind %>% filter(tested == "tested"))


prev_model_list <- unlist(prev_output, recursive = FALSE)

prev_model_list[["multi_tested"]] <-  glm(tested ~ sex + tratamento + hiv_status + age_cat + health_unit, 
                                         family = binomial (link = "logit"), 
                                         data = mdf_prev_ind)

prev_model_list[["multi_result"]] <-  glm(result ~ sex + tratamento + hiv_status + age_cat + health_unit, 
                                         family = binomial (link = "logit"), 
                                         data = mdf_prev_ind %>% filter(tested == "tested"))



## Other
# new_model_list[["multi_tested_prison"]] <-  glm(tested ~ sex + hiv_status + age_cat + health_unit + pop_liber + state,
#                                                 family = binomial (link = "logit"),
#                                                 data = mdf_new_ind)
# 
# new_model_list[["multi_result_prison"]] <-  glm(result ~ sex + hiv_status + age_cat + health_unit + pop_liber + state,
#                                                 family = binomial (link = "logit"),
#                                                 data = mdf_new_ind %>% filter(tested == "tested"))
