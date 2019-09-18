#Function: case_merge
#Does: Determines cases to merge for Hepatits B
#By: Andrew Cervantes
case_merge<-function(rows){
  sink("outputC.txt")
  cat("Cases to Merge\n", append = TRUE)
  for (i in 1:(rows)){
    box <- which(patient_id == patient_id[i])
    if (length(box) == 1){
      next
    }      
    merge_case <- FALSE
    for (j in box){
      if (i <= j){
        next
      }

      if(disease[i] == disease[j]){
        merge_case <- TRUE
        break
      }
      if (disease[i] != "hepatitisbacute" && disease[j] != "hepatitisbacute"){
        merge_case <- TRUE
        break
      }
    }
    #Prints cases that should be merged
    if (merge_case == TRUE){
      cat("Case IDs: ", append = TRUE)
      cat(case_id[i], append = TRUE)
      cat(",", append = TRUE) 
      cat(case_id[j], append = TRUE)
      cat("\n\n", append = TRUE) 
    }
  }
  sink()
}
