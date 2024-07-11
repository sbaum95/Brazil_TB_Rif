
# Description/Decisions: Takes sinan and prepare it for model analysis. 
# Filter to desired time period. Select only new and previously treated case types. Remove individuals misdiagnosed with TB. 

create_analytic_dataset <- function(first_quarter, last_quarter, covariates, tratamento) {
  
  mdf <- sinan_tmp %>%
    # Remove individuals misdiagnosed with TB (situa_ence == 06)
    filter(diag_qrt >= first_quarter & diag_qrt <= last_quarter & situa_ence != "06") %>%
    select(all_of(covariates))
  
  # Order the dates in ascending order
  sorted_dates <- sort(unique(mdf$diag_qrt))
  
  # Create a named vector to store the rank for each date
  time <- setNames(1:length(sorted_dates), format(sorted_dates, "%Y-%m-%d"))
  dates <- cbind(as.data.frame(sorted_dates), time)
  mdf <- left_join(mdf, dates, by = c("diag_qrt" = "sorted_dates"))
  mdf$time <- as.numeric(as.character(mdf$time))
  

  if (tratamento == "new") {
    mdf_new_ind <- mdf %>%
      filter(tratamento == "1")
    
  } else {
    mdf_prev_ind <- mdf %>%
      filter(tratamento %in% c("2", "3")) %>%
      mutate(tratamento = if_else(tratamento == "2", "relapse", "reentry") %>% as.factor())
  }
}
