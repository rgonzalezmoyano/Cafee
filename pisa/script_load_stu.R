### libraries
#library(readr)

### read file
# INT_STU12_DEC03 <- read_table (
#   "pisa/data/1_student_questionannaire_data_file/INT_STU12_DEC03.txt",
#   col_names = FALSE)

# dataset <- read_csv(
#   "pisa/data/1_student_questionannaire_data_file/INT_STU12_DEC03.txt",
#   col_names = FALSE
#   )

# save file in new format
#save(dataset, file = "pisa/data/1_student_questionannaire_data_file/INT_STU12_DEC03.RData")

# PC CIO
load("C:/Users/Ricardo/Desktop/cafee/pisa/data/1_student_questionannaire_data_file/INT_STU12_DEC03.RData")

# Surface
load("C:/Users/Ricardo/Desktop/Data Pisa 2012/INT_STU12_DEC03.RData")

sortBD <- function(
    data) {
  
  # delete spaces
  #data <- apply(dataset, 2, function(x) gsub(" ", "", x))
  data <- as.data.frame(dataset[1:9,])
  
  # create new data.frame to save correct data
  db <- as.data.frame( 
    matrix(nrow = nrow(data))
  )
  
  # table to find extra information
  names <- data.frame(
    vble = rep(NA, length(data)),
    name = rep(NA, length(data))
  )
  
  #############
  # variables #
  #############
  
  # Country code 3-character
  db$CNT <- substr(data[, 1], start = 1, stop = 3)
  db$V1 <- NULL # delete first column of NA
  index <- which(names(db) == "CNT")
  
  names[index, 1] <- "CNT"
  names[index, 2] <- "Country code"
  
  # Adjudicated sub-region code 7-digit code (3-digit country code + region ID + stratum ID)
  db$SUBNATIO <- substr(data[, 1], start = 4, stop = 10) 
  index <- which(names(db) == "SUBNATIO")
  
  names[index, 1] <- "SUBNATIO"
  names[index, 2] <- "Adjudicated sub-region code"
  
  # Stratum ID 7-character (cnt + region ID + original stratum ID)
  db$STRATUM <- substr(data[, 1], start = 11, stop = 17)  
  index <- which(names(db) == "STRATUM")
  
  names[index, 1] <- "STRATUM"
  names[index, 2] <- "Stratum ID"
  
  # OECD country
  db$OECD <- c(substr(data[, 1], start = 18, stop = 18)) 
  index <- which(names(db) == "OECD")
  
  names[index, 1] <- "OECD"
  names[index, 2] <- "OECD countr"
  
  #  National Centre 6-digit Code
  db$NC <- substr(data[, 1], start = 19, stop = 24)
  index <- which(names(db) == "NC")
  
  names[index, 1] <- "NC"
  names[index, 2] <- "National Centre"
  
  #  School ID 7-digit (region ID + stratum ID + 3-digit schoolID)
  db$SCHOOLID <- substr(data[, 1], start = 25, stop = 31)
  index <- which(names(db) == "SCHOOLID")
  
  names[index, 1] <- "SCHOOLID"
  names[index, 2] <- "School ID"
  
  # Student ID
  db$STIDSTD <- substr(data[, 1], start = 32, stop = 36)
  index <- which(names(db) == "STIDSTD")
  
  names[index, 1] <- "STIDSTD"
  names[index, 2] <- "Student ID"
  
  # International Grade
  db$ST01Q01 <- substr(data[, 1], start = 37, stop = 38)
  index <- which(names(db) == "ST01Q01")
  
  names[index, 1] <- "ST01Q01"
  names[index, 2] <- "International Grade"
  
  # National Study Programme
  db$ST02Q01 <- substr(data[, 1], start = 39, stop = 40)
  index <- which(names(db) == "ST02Q01")
  
  names[index, 1] <- "ST02Q01"
  names[index, 2] <- "National Study Programme"
  
  # Birth - Month
  db$ST03Q01<- substr(data[, 1], start = 41, stop = 42)
  index <- which(names(db) == "ST03Q01")
  
  names[index, 1] <- "ST03Q01"
  names[index, 2] <- "Birth - Month"
  
  # Birth -Year
  db$ST03Q02 <- substr(data[, 1], start = 43, stop = 46) 
  index <- which(names(db) == "ST03Q02")
  
  names[index, 1] <- "ST03Q02"
  names[index, 2] <- "Birth -Year"
  
  # Gender
  db$ST04Q01 <- substr(data[, 1], start = 47, stop = 47) 
  index <- which(names(db) == "ST04Q01")
  
  names[index, 1] <- "ST04Q01"
  names[index, 2] <- "Gender"
  
  # Attend <ISCED 0>
  db$ST05Q01 <- substr(data[, 1], start = 48, stop = 48)
  index <- which(names(db) == "ST05Q01")
  
  names[index, 1] <- "ST05Q01"
  names[index, 2] <- "Attend <ISCED 0>"
  
  #  Age at <ISCED 1> 
  db$ST06Q01 <- substr(data[, 1], start = 49, stop = 52)
  index <- which(names(db) == "ST06Q01")
  
  names[index, 1] <- "ST06Q01"
  names[index, 2] <- "Age at <ISCED 1>"
  
  # Repeat - <ISCED 1>
  db$ST07Q01 <- substr(data[, 1], start = 53, stop = 53)
  index <- which(names(db) == "ST07Q01")
  
  names[index, 1] <- "ST07Q01"
  names[index, 2] <- "Repeat - <ISCED 1>"
  
  # Repeat - <ISCED 2>
  db$ST07Q02 <- substr(data[, 1], start = 54, stop = 54)
  index <- which(names(db) == "ST07Q02")
  
  names[index, 1] <- "ST07Q02"
  names[index, 2] <- "Repeat - <ISCED 2>"
  
  # Repeat - <ISCED 3>
  db$ST07Q03 <- substr(data[, 1], start = 55, stop = 55)
  index <- which(names(db) == "ST07Q03")
  
  names[index, 1] <- "ST07Q03"
  names[index, 2] <- "Repeat - <ISCED 3>"
  
  # Truancy - Late for School
  db$ST08Q01 <- substr(data[, 1], start = 56, stop = 56)
  index <- which(names(db) == "ST08Q01")
  
  names[index, 1] <- "ST08Q01"
  names[index, 2] <- "Truancy - Late for School"
  
  # Truancy - Skip whole school day
  db$ST09Q01 <- substr(data[, 1], start = 57, stop = 57)
  index <- which(names(db) == "ST09Q01")
  
  names[index, 1] <- "ST09Q01"
  names[index, 2] <- "Truancy - Skip whole school day"
  
  db$ST115Q01 <- substr(data[, 1], start = 58, stop = 58)
  index <- which(names(db) == "ST115Q01")
  
  names[index, 1] <- "ST115Q01"
  names[index, 2] <- "Truancy - Skip classes within school day"
  
  db$ST11Q01 <- substr(data[, 1], start = 59, stop = 59)
  index <- which(names(db) == "ST11Q01")
  
  names[index, 1] <- "ST11Q01"
  names[index, 2] <- "At Home - Mother"
  
  db$ST11Q02 <- substr(data[, 1], start = 60, stop = 60)
  index <- which(names(db) == "ST11Q02")
  
  names[index, 1] <- "ST11Q02"
  names[index, 2] <- "At Home - Father"
  
  db$ST11Q03 <- substr(data[, 1], start = 61, stop = 61)
  index <- which(names(db) == "ST11Q03")
  
  names[index, 1] <- "ST11Q03"
  names[index, 2] <- "At Home - Brothers"
  
  db$ST11Q04 <- substr(data[, 1], start = 62, stop = 62)
  index <- which(names(db) == "ST11Q04")
  
  names[index, 1] <- "ST11Q04"
  names[index, 2] <- "At Home - Sisters"
  
  db$ST11Q05 <- substr(data[, 1], start = 63, stop = 63)
  index <- which(names(db) == "ST11Q05")
  
  names[index, 1] <- "ST11Q05"
  names[index, 2] <- "At Home - Grandparents"
  
  db$ST11Q06 <- substr(data[, 1], start = 64, stop = 64)
  index <- which(names(db) == "ST11Q06")
  
  names[index, 1] <- "ST11Q06"
  names[index, 2] <- "At Home - Others"
  
  
    
    
  
  return(list(data = db, names = names))
  
}

sortBD(data = dataset)
