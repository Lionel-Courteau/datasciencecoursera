best <- function(state, outcome) {
  
  # Read outcome data
  outcome_data <- data.table::fread('outcome-of-care-measures.csv')
  
  outcome <- tolower(outcome)
  
  # Column name is same as variable so changing it 
  my_state <- state 
  
  # Check that state and outcome are valid
  if (!my_state %in% unique(outcome_data[["State"]])) {
    stop('invalid state')
  }
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop('invalid outcome')
  }
  
  # Renaming Columns to be less verbose and lowercase
  setnames(outcome_data,
           tolower(sapply(colnames(outcome_data), 
                                        gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" ))
  )
  
  #Filter by state
  outcome_data <- outcome_data[state == my_state]
  
  # Columns indices to keep
  col_indices <- grep(paste0("hospital name|state|^",outcome), 
                      colnames(outcome_data))
  
  # Filtering out unnessecary data 
  outcome_data <- outcome_data[, .SD ,.SDcols = col_indices]
  
  # Find out what class each column is 
  # sapply(out_dt,class)
  outcome_data[, outcome] <- outcome_data[,  as.numeric(get(outcome))]
  
  
  # Removing Missing Values for numerical datatype (outcome column)
  outcome_data <- outcome_data[complete.cases(outcome_data),]
  
  # Order Column to Top 
  outcome_data <- outcome_data[order(get(outcome), `hospital name`)]
  
  return(outcome_data[, "hospital name"][1])
  
}