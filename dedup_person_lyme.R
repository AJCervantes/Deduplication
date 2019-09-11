person_merge<-function(cutoff){
  #Setting up/cleaning variables
  sink("outputP.txt")
  cat("People to Merge\n", append = TRUE)
  
  cases <- zeros(4000)
  case_index <- 1
  counter <- 0
  for (i in 1:(length(case_id) - 1)){
    if(is_match[i] == TRUE){
      next
    }
    for (j in (box)){
      
      percent = 0
      #checking skip conditions
      if(j <= i || is_match[j] == TRUE || patient_id[i] == patient_id[j]){
        next
      }
      if ((dob[i] != dob[j] && name_first[i] != name_first[j] && name_last[i] != name_last[j] && phone[i] != phone[j])){
        next
      }
      
      #Date of birth check
      if (dob[i] == dob[j] && dob[i] != ""){
        split = strsplit(dob[i], "/")[[1]]
        if (split[1] == "01"&& split[2] == "01"){
          percent = 0.35
        }else{
          percent = 0.39
        }
      }else{
        percent_dob = percent_birth(dob[i], dob[j])
        if(percent_dob > 0.8){
          percent = 0.32
        }else{
          if(percent_dob > 0.7){
            percent = 0.3
          }
        }
      } 
      #Name and address check if DOB gives any percent
      if (percent > 0){
        #Address and phone number check
        add_same = address_same(i, j, percent)
        
        if (add_same >= 0.9 || (phone[i] == phone[j] && phone[i] != "0" && phone[i] != "" && phone[i] != "9999999999" && phone[i] != "9733456000")){
          percent = percent + 0.11
        }else{
          if(add_same == -1){
            perent = percent + 0.03
          }else{
            if(check_pars(gender[i], gender[j], race[i], race[j], ethnicity[i], ethnicity[j]) == TRUE){
              percent = percent + 0.01
            }
          }
        }
        #Name check
        if(percent >= (cutoff - 0.50) && name_first[i] != "sr" && name_first[j] != "sr"){
          if (name_first[i] == name_first[j] && name_last[i] == name_last[j] && name_last[i] != "doe"){
            percent = percent + 0.5
          }else{
            nick <- is_nickname(name_first[i], name_first[j])
            if (nick == TRUE && name_last[i] == name_last[j]){
              percent = percent + 0.47
            }
            else{
              if(nick == TRUE){
                first_same = 1
              }
              else{
                first_same = percent_same(name_first[i], name_first[j])
              }
              last_same = percent_same(name_last[i], name_last[j])
              
              if (first_same == 1 && last_same == 1){
                percent = percent + 0.46
              }
              else{
                if (((first_same == -1 || first_same > 0.8) && last_same == 1) || ((last_same == -1 || last_same > 0.8) && first_same == 1)){
                  percent = percent + 0.45
                }else{
                  if ((first_same == -1 && last_same > 0.8) || (first_same > 0.8 && last_same == -1)){
                    percent = percent + 0.4
                  }
                  else{
                    if (first_same > 0.8 && last_same > 0.8){
                      percent = percent + 0.39
                    }
                    else{
                      if(first_same == 1 && gender[i] == "female"){
                        percent = percent + 0.37
                      }
                      else{
                        if (name_first[i] == name_last[j] && name_last[i] == name_first[j]){
                          percent = percent + 0.44
                        }
                        else{
                          first_same = percent_same(name_first[i], name_last[j])
                          last_same = percent_same(name_first[j], name_last[i])
                          if (first_same == 1 && last_same == 1){
                            percent = percent + 0.44 
                          }
                          else{
                            if ((first_same == -1 && last_same > 0.8) || (first_same > 0.8 && last_same == -1)){
                              percent = percent + 0.35
                            }
                            else{
                              if (first_same > 0.8 && last_same > 0.8){
                                percent = percent + 0.33
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      #adds case IDs to output if it passes given percent
      if (percent >= cutoff){
        cat("Percent: ", append = TRUE)
        cat (percent, append = TRUE)
        cat ("\n", append = TRUE)
        cat("Case IDs: ", append = TRUE)
        cat(case_id[i], append = TRUE)
        cat(",", append = TRUE)
        cat(case_id[j], append = TRUE)
        cat("\n\n", append = TRUE) 

        is_match[i] <- TRUE
        is_match[j] <- TRUE
        if(as.numeric(case_id[i]) > 1487952 && as.numeric(case_id[j]) > 1487952){
          counter <- counter + 1
          cases[case_index] <- case_id[i]
          case_index <- case_index + 1
        }
        break
      }
    }
  }
  sink()
  
  sink("2019cases.txt")
  for(i in 1:(case_index - 1)){
    cat(cases[i], append = TRUE)
    cat("\n\n", append = TRUE)
  }
  sink()
}

#HELPER FUNCTIONS

#Function: percent_same
#Does: gives the best perent of the two strings that 
#      are the same, fowards or backwards
percent_same <- function(str1, str2){
  if (str1 == "" || str1 == "unknown" || str1 == "unk" || str1 == "0" || str1 == "nj" || str2 == "" || str2 == "unknown" || str2 == "unk" || str2 == "0" || str2 == "nj"){
    return(0)
  }
  
  len1 <- str_length(str1)
  len2 <- str_length(str2)
  length <- len1
  if (len1 >= len2){
    length <- len2
  }
  
  total1 = 0
  for (i in  1:length){
    char1 = substr(str1, i, i)
    char2 = substr(str2, i, i)
    if (char1 == char2){
      total1 = total1 + 1
    }
  }
  
  total2 = 0
  for (j in  1:length){
    char1 = str_sub(str1, length - j, length - (j))
    char2 = str_sub(str2, length - j, length - (j))
    if (char1 == char2){
      total2 = total2 + 1
    }
  }
  if (((length - 6) < 0) && (length - total1 == 1) || (length - total2 == 1)){
    return(-1)
  }else{
    if (total1 >= total2){
      return(total1 / length)
    }else{
      return(total2 / length)
    }
  }
}

#Function: percent_birth
#Does: Checks the percent of the 2 DOBs that are the same
percent_birth <-function(dob1, dob2){
  if(dob1 == "" || dob2 == ""){
    return(0)
  }
  
  check1 <- strsplit(dob1, "/")[[1]]
  check2 <- strsplit(dob2, "/")[[1]]
  if (check1[3] == "2100" || check2[3] == "2100" || check1[3] == "1888" || check2[3] == "1888"){
    return(0)
  }
  
  char1 <- strsplit(dob1, "")[[1]]
  char2 <- strsplit(dob2, "")[[1]]
  percentd = 0
  nums <- c(1, 2, 4, 5, 7, 8, 9, 10)
  for(i in nums){
    if(char1[i] == char2[i]){
      percentd = percentd + 0.125
    }
  }
  
  diff = as.numeric(check1[3]) - as.numeric(check2[3])
  if (diff > 10 || diff < -10){
    percentd = percentd - .10
  } 
  if (diff > 30 || diff < -30){
    percentd = percentd - 0.20  
  }
  return(percentd)
}

#Function: address_same
#Does:Checks if two addresses are the same
address_same <-function(i, j, percent){
  #Checks for jail address matches
  is_jail1 <- which(jail_add == address_1[i])  
  is_jail3 <- which(jail_add == address_2[i])  
  
  if (address_1[i] == address_1[j]){
    if (address_1[i] == "" || address_1[i] == "unknown" || address_1[i] == "unk" || address_1[i] == "0" || address_1[i] == "nj"){
      return(0)
    }
    if(length(is_jail1) != 0 && percent < 0.31){
      return(0)
    }
    return(1)
  }
  if (address_1[i] == address_2[j]){
    if (address_1[i] == "" || address_1[i] == "unknown" || address_1[i] == "unk" || address_1[i] == "0" || address_1[i] == "nj"){
      return(0)
    }
    if(length(is_jail1) != 0 && percent < 0.31){
      return(0)
    }
    return(1)
  }
  if (address_2[i] == address_1[j]){
    if (address_2[i] == "" || address_2[i] == "unknown" || address_2[i] == "unk" || address_2[i] == "0" || address_2[i] == "nj"){
      return(0)
    }
    if(length(is_jail3) != 0 && percent < 0.31){
      return(0)
    }
    return(1)
  }
  if (address_2[i] == address_2[j]){
    if (address_2[i] == "" || address_2[i] == "unknown" || address_2[i] == "unk" || address_2[i] == "0" || address_2[i] == "nj"){
      return(0)
    }
    if(length(is_jail3) != 0 && percent < 0.31){
      return(0)
    }
    return(1)
  }
  
  is_jail2 <- which(jail_add == address_1[j]) 
  is_jail4 <- which(jail_add == address_2[j])  
  
  if (length(is_jail1) != 0 && length(is_jail2) == 0){
    return(-1)
  }
  if (length(is_jail1) != 0 && length(is_jail4) == 0){
    return(-1)
  }
  if (length(is_jail3) != 0 && length(is_jail2) == 0){
    return(-1)
  }
  if (length(is_jail3) != 0 && length(is_jail4) == 0){
    return(-1)
  }
  return(0)
}

#Function: check_pars
#Does:Checks if the gender, ethnicity, or race are the same
check_pars <- function(gen1, gen2, race1, race2, eth1, eth2){
  if (gen1 == gen2 && gen1 != "other/unknown"){
    return(TRUE)
  }
  if (race1 == race2 && race1 != "other/unknown" && race1 != "unknown"){
    return(TRUE)
  }
  if (eth1 == eth2 && eth1 != "other/unknown" && eth1 != "unknown"){
    return(TRUE)
  }
  return(FALSE)
}

#Function: is_nickname
#Does: checks if there is a nickname/fullname
#      combo that is in the nickname file
is_nickname<-function(name1, name2){
  c <- which(nicknames == name1)
  if (length(c) != 0){
    d <- which(fullnames == name2)
    if (anyDuplicated(c(c, d)) != 0){
      return(TRUE)
    }
  }
  e <- which(nicknames == name2)
  if (length(e) != 0){
    f <- which(fullnames == name1)
    if (anyDuplicated(c(e, f)) != 0){
      return(TRUE)
    }
  }
  return(FALSE)
}





