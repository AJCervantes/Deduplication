case_merge<-function(rows){
  sink("outputC.txt")
  cat("Cases to Merge\n", append = TRUE)
  
  for (i in 1:(rows)){
    box <- which(patient_id == patient_id[i])
    if (length(box) == 1){
      next
    }
    
    for (j in box){
      if (i <= j || disease[i] == "hepatitiscperinatal" || disease[j] == "hepatitiscperinatal"){
        next
      }
      merge_case <- FALSE
      id_comp <- compare_ids(case_id[i], case_id[j])
      
      if(disease[i] == disease[j] && case_status[i] == case_status[j]){
        if(case_status[i] == "outofstate" && state_1[i] != state_1[j]){
          next
        }else{
          merge_case <- TRUE
        }
      }else{
        if(disease[i] == disease[j]){
          if((case_status[i] == "notacase" && id_comp == 1 && case_status[j] != "outofstate") || (case_status[j] == "notacase" && id_comp == 2 && case_status[i] != "outofstate")){
            merge_case <- TRUE
          }
          if(case_status[i] == "confirmed" && case_status[j] == "probable" && id_comp == 2){
            merge_case <- TRUE
          }
          if(case_status[j] == "confirmed" && case_status[i] == "probable" && id_comp == 1){
            merge_case <- TRUE
          }
        }
      }
      if(disease[i] != "hepatitiscacute" && disease[j] != "hepatitiscacute" && merge_case == FALSE){
        if(case_status[j] == "confirmed" && id_comp == 1 && case_status[i] != "outofstate"){
          merge_case <- TRUE
        }
        if(case_status[i] == "confirmed" && id_comp == 2 && case_status[j] != "outofstate"){
          merge_case <- TRUE
        }
      }
      
      if (merge_case == TRUE){
        cat(case_id[i], append = TRUE)
        cat(",", append = TRUE) 
        cat(case_id[j], append = TRUE)
        cat("\n\n", append = TRUE) 
      }
    }
  }
  sink()
}

#HELPER FUNCTIONS

#Function: compare_ids
#Does: returns the greater of the two ids
compare_ids<-function(one, two){
  if(as.numeric(one) > as.numeric(two)){
    return(1)
  }
  if(as.numeric(one) < as.numeric(two)){
    return(2)
  }
  return(0)
}

