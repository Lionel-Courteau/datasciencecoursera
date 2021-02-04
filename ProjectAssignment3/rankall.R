rankall <- function(outcome, num = "best") {
  
  # Read outcome data
  outcome_data <- data.table::fread('outcome-of-care-measures.csv')
  
  outcome <- tolower(outcome)
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop('invalid outcome')
  }
  
  # Renaming Columns to be less verbose and lowercase
  setnames(outcome_data, 
           tolower(sapply(colnames(outcome_data), 
                          gsub, pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", replacement = "" ))
  )
  
  # Columns indices to keep
  col_indices <- grep(paste0("hospital name|state|^",outcome), colnames(outcome_data))
  
  # Filtering out unnessecary data 
  outcome_data <- outcome_data[, .SD ,.SDcols = col_indices]
  
  # Change outcome column class
  outcome_data[, outcome] <- outcome_data[,  as.numeric(get(outcome))]
  
  if (num == "best"){
    return(outcome_data[order(state, get(outcome), `hospital name`), 
                        .(hospital = head(`hospital name`, 1)), 
                        by = state])
  }
  
  if (num == "worst"){
    return(outcome_data[order(get(outcome), `hospital name`), 
                        .(hospital = tail(`hospital name`, 1)), 
                        by = state])
  }
  
  return(outcome_data[order(state, get(outcome), `hospital name`), 
                      head(.SD,num), 
                      by = state, .SDcols = c("hospital name") ])
  
}