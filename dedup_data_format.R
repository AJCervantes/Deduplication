#reading in data
data1 <- read.csv("LD DeDUP_aug819.csv", sep=",", skip = 1, header =F,col.names = c("case_id", "patient_id","case_status", "report_status" , "disease", "date_for_report", "illness_onset", "lastname", "firstname", "gender", "date_of_birth", "race", "ethnicity", "phone", "address1", "county1", "state1", "address2", "county2", "state2", "address3", "county3", "state3", "address4", "county4", "state4", "address5", "county5", "state5", "address6", "county6", "state6", "address7", "county7", "state7"))           
#format_date <- read_csv("DEDUP_7Y_072219.csv", skip = 1, col_types = cols(date_of_birth = col_date(format = "%m/%d/%Y")))
#data1 <- arrange(format_date,date_of_birth)  

jails <- read.csv("jail_addresses.csv", sep=",", skip=0, header=F, col.names=c("addresses"))
data_nicknames <- read.csv("nicknames_list.csv", sep=",", skip=0, header=F, col.names=c("nickname", "fullname"))
exclusions <- read.csv("Exclusions.csv", sep=",", skip=1, header=F, col.names=c("IDs"))

#creating and formatting the columns
exclusion_id <- exclusions$IDs
case_id <- data1$case_id
patient_id <- data1$patient_id
name_first <- data1$firstname
name_last <- data1$lastname
disease <- data1$disease
case_status <- data1$case_status
report_status <- data1$report_status
gender <- data1$gender
dob <- data1$date_of_birth
ethnicity <- data1$ethnicity
race <- data1$race
address_1 <- data1$address1
county_1 <- data1$county1
state_1 <- data1$state1
phone <- data1$phone
address_2 <- data1$address2
county_2 <- data1$county2

#Formatting columns
case_id <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", case_id))))
patient_id <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", patient_id))))
name_first <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", name_first))))
name_last <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", name_last))))
disease <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", disease))))
case_status <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", case_status))))
report_status <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", report_status))))
gender <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", gender))))
dob <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "/", dob))))
ethnicity <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", ethnicity))))
race <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", race))))
address_1 <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", address_1))))
county_1 <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", county_1))))
state_1 <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", state_1))))
phone <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", phone))))
address_2 <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", address_2))))
county_2 <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", county_2))))
gsub("street", "st", gsub("road", "rd", gsub("avenue", "ave", gsub("lane", "ln", gsub("place", "pl", address_1)))))
gsub("street", "st", gsub("road", "rd", gsub("avenue", "ave", gsub("lane", "ln", gsub("place", "pl", address_2)))))
gsub("west", "w", gsub("east", "e", gsub("north", "n", gsub("south", "s", address_1 ))))
gsub("west", "w", gsub("east", "e", gsub("north", "n", gsub("south", "s", address_2 ))))

#getting rid of NA blocks
index <- is.na(name_first)
name_first[index] = ""
index1 <- is.na(dob)
dob[index1] = "1/1/2100" 
index2 <- is.na(phone)
phone[index2] = 0
index3 <- is.na(address_1)
address_1[index3] = 0
index4 <- is.na(address_2)
address_2[index4] = 0
index5 <- is.na(name_last)
name_last[index5] = ""

is_match <- logical(length(case_id))

#formatting for nicknames and jail addresses
nicknames <- data_nicknames$nickname
fullnames <- data_nicknames$fullname

jail_add <- jails$addresses
jail_add <- tolower(gsub(" ", "", gsub(",", "", gsub("-", "", gsub(".", "", jail_add)))))
gsub("street", "st", gsub("road", "rd", gsub("avenue", "ave", gsub("lane", "ln", gsub("place", "pl", jail_add)))))
gsub("west", "w", gsub("east", "e", gsub("north", "n", gsub("south", "s", jail_add ))))


#freeing up some extra space
rm(data1)
rm(jails)
rm(data_nicknames)
rm(exclusions)
rm(index)
rm(index1)
rm(index2)
rm(index3)
rm(index4)
rm(index5)
