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
load("C:/Users/Ricardo/OneDrive - UMH/Escritorio/Data Pisa 2012/INT_STU12_DEC03.RData")

# Surface
load("C:/Users/Ricardo/Desktop/Data Pisa 2012/INT_STU12_DEC03.RData")

sortBD <- function(
    data) {
  
  # delete spaces
  #data <- apply(dataset, 2, function(x) gsub(" ", "", x))
  #data <- as.data.frame(dataset[1:9,])
  
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
  
  db$CNT <- substr(data[, 1], start = 1, stop = 3)
  db$V1 <- NULL # delete first column of NA
  index <- which(names(db) == "CNT")
  
  names[index, 1] <- "CNT"
  names[index, 2] <- "Country code 3-character"
  
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
  
  db$ST13Q01 <- substr(data[, 1], start = 65, stop = 65)
  index <- which(names(db) == "ST13Q01")
  
  names[index, 1] <- "ST13Q01"
  names[index, 2] <- "Mother<Highest Schooling>"
  
  db$ST14Q01 <- substr(data[, 1], start = 66, stop = 66)
  index <- which(names(db) == "ST14Q01")
  
  names[index, 1] <- "ST14Q01"
  names[index, 2] <- "Mother Qualifications - <ISCED level 6>"
  
  db$ST14Q02 <- substr(data[, 1], start = 67, stop = 67)
  index <- which(names(db) == "ST14Q02")
  
  names[index, 1] <- "ST14Q02"
  names[index, 2] <- "Mother Qualifications - <ISCED level 5A>"
  
  db$ST14Q03 <- substr(data[, 1], start = 68, stop = 68)
  index <- which(names(db) == "ST14Q03")
  
  names[index, 1] <- "ST14Q03"
  names[index, 2] <- "Mother Qualifications - <ISCED level 5B>"
  
  db$ST14Q04 <- substr(data[, 1], start = 69, stop = 69)
  index <- which(names(db) == "ST14Q04")
  
  names[index, 1] <- "ST14Q04"
  names[index, 2] <- "Mother Qualifications - <ISCED level 4>"
  
  db$ST15Q01 <- substr(data[, 1], start = 70, stop = 70)
  index <- which(names(db) == "ST15Q01")
  
  names[index, 1] <- "ST15Q01"
  names[index, 2] <- "Mother Current Job Status"
  
  db$ST17Q01 <- substr(data[, 1], start = 71, stop = 71)
  index <- which(names(db) == "ST17Q01")
  
  names[index, 1] <- "ST17Q01"
  names[index, 2] <- "Father<Highest Schooling>"
  
  db$ST18Q01 <- substr(data[, 1], start = 72, stop = 72)
  index <- which(names(db) == "ST18Q01")
  
  names[index, 1] <- "ST18Q01"
  names[index, 2] <- "Father Qualifications - <ISCED level 6>"
  
  db$ST18Q02 <- substr(data[, 1], start = 73, stop = 73)
  index <- which(names(db) == "ST18Q02")
  
  names[index, 1] <- "ST18Q02"
  names[index, 2] <- "Father Qualifications - <ISCED level 5A>"
  
  db$ST18Q03 <- substr(data[, 1], start = 74, stop = 74)
  index <- which(names(db) == "ST18Q03")
  
  names[index, 1] <- "ST18Q03"
  names[index, 2] <- "Father Qualifications - <ISCED level 5B>"
  
  db$ST18Q04 <- substr(data[, 1], start = 75, stop = 75)
  index <- which(names(db) == "ST18Q04")
  
  names[index, 1] <- "ST18Q04"
  names[index, 2] <- "Father Qualifications - <ISCED level 4>"
  
  db$ST19Q01 <- substr(data[, 1], start = 76, stop = 76)
  index <- which(names(db) == "ST19Q01")
  
  names[index, 1] <- "ST19Q01"
  names[index, 2] <- "Father Current Job Status"
  
  db$ST20Q01 <- substr(data[, 1], start = 77, stop = 77)
  index <- which(names(db) == "ST20Q01")
  
  names[index, 1] <- "ST20Q01"
  names[index, 2] <- "Father Current Job Status"
  
  db$ST20Q02 <- substr(data[, 1], start = 78, stop = 78)
  index <- which(names(db) == "ST20Q02")
  
  names[index, 1] <- "ST20Q02"
  names[index, 2] <- "Country of Birth International - Mother"
  
  db$ST20Q03 <- substr(data[, 1], start = 79, stop = 79)
  index <- which(names(db) == "ST20Q03")
  
  names[index, 1] <- "ST20Q03"
  names[index, 2] <- "Country of Birth International - Father"
  
  db$ST21Q01 <- substr(data[, 1], start = 80, stop = 83)
  index <- which(names(db) == "ST21Q01")
  
  names[index, 1] <- "ST21Q01"
  names[index, 2] <- "Age of arrival in <country of test>"
  
  db$ST25Q01 <- substr(data[, 1], start = 84, stop = 84)
  index <- which(names(db) == "ST25Q01")
  
  names[index, 1] <- "ST25Q01"
  names[index, 2] <- "International Language at Home"
  
  db$ST26Q01 <- substr(data[, 1], start = 85, stop = 85)
  index <- which(names(db) == "ST26Q01")
  
  names[index, 1] <- "ST26Q01"
  names[index, 2] <- "Possessions - desk"
  
  db$ST26Q02 <- substr(data[, 1], start = 86, stop = 86)
  index <- which(names(db) == "ST26Q02")
  
  names[index, 1] <- "ST26Q02"
  names[index, 2] <- "Possessions - own room"
  
  db$ST26Q03 <- substr(data[, 1], start = 87, stop = 87)
  index <- which(names(db) == "ST26Q03")
  
  names[index, 1] <- "ST26Q03"
  names[index, 2] <- "Possessions - study place"
  
  db$ST26Q04 <- substr(data[, 1], start = 88, stop = 88)
  index <- which(names(db) == "ST26Q04")
  
  names[index, 1] <- "ST26Q04"
  names[index, 2] <- "Possessions - computer"
  
  db$ST26Q05 <- substr(data[, 1], start = 89, stop = 89)
  index <- which(names(db) == "ST26Q05")
  
  names[index, 1] <- "ST26Q05"
  names[index, 2] <- "Possessions - software"
  
  db$ST26Q06 <- substr(data[, 1], start = 90, stop = 90)
  index <- which(names(db) == "ST26Q06")
  
  names[index, 1] <- "ST26Q06"
  names[index, 2] <- "Possessions - software"
  
  db$ST26Q07 <- substr(data[, 1], start = 91, stop = 91)
  index <- which(names(db) == "ST26Q07")
  
  names[index, 1] <- "ST26Q07"
  names[index, 2] <- "Possessions - literature"
  
  db$ST26Q07 <- substr(data[, 1], start = 91, stop = 91)
  index <- which(names(db) == "ST26Q07")
  
  names[index, 1] <- "ST26Q07"
  names[index, 2] <- "Possessions - literature"
  
  db$ST26Q08 <- substr(data[, 1], start = 92, stop = 92)
  index <- which(names(db) == "ST26Q08")
  
  names[index, 1] <- "ST26Q08"
  names[index, 2] <- "Possessions - poetry"
  
  db$ST26Q09 <- substr(data[, 1], start = 93, stop = 93)
  index <- which(names(db) == "ST26Q09")
  
  names[index, 1] <- "ST26Q09"
  names[index, 2] <- "Possessions - art"
  
  db$ST26Q10 <- substr(data[, 1], start = 94, stop = 94)
  index <- which(names(db) == "ST26Q10")
  
  names[index, 1] <- "ST26Q10"
  names[index, 2] <- "Possessions - textbooks"
  
  db$ST26Q11 <- substr(data[, 1], start = 95, stop = 95)
  index <- which(names(db) == "ST26Q11")
  
  names[index, 1] <- "ST26Q11"
  names[index, 2] <- "Possessions - <technical reference books>"
  
  db$ST26Q12 <- substr(data[, 1], start = 96, stop = 96)
  index <- which(names(db) == "ST26Q12")
  
  names[index, 1] <- "ST26Q12"
  names[index, 2] <- "Possessions - dictionary"
  
  db$ST26Q13 <- substr(data[, 1], start = 97, stop = 97)
  index <- which(names(db) == "ST26Q13")
  
  names[index, 1] <- "ST26Q13"
  names[index, 2] <- "Possessions - dishwasher"
  
  db$ST26Q14 <- substr(data[, 1], start = 98, stop = 98)
  index <- which(names(db) == "ST26Q14")
  
  names[index, 1] <- "ST26Q14"
  names[index, 2] <- "Possessions - <DVD>"
  
  db$ST26Q15 <- substr(data[, 1], start = 99, stop = 105)
  index <- which(names(db) == "ST26Q15")
  
  names[index, 1] <- "ST26Q15"
  names[index, 2] <- "Possessions - <Country item 1>"
  
  db$ST26Q16 <- substr(data[, 1], start = 106, stop = 112)
  index <- which(names(db) == "ST26Q16")
  
  names[index, 1] <- "ST26Q16"
  names[index, 2] <- "Possessions - <Country item 2>"
  
  db$ST26Q17 <- substr(data[, 1], start = 113, stop = 119)
  index <- which(names(db) == "ST26Q17")
  
  names[index, 1] <- "ST26Q17"
  names[index, 2] <- "Possessions - <Country item 3>"
  
  db$ST27Q01 <- substr(data[, 1], start = 120, stop = 120)
  index <- which(names(db) == "ST27Q01")
  
  names[index, 1] <- "ST27Q01"
  names[index, 2] <- "How many - cellular phones"
  
  db$ST27Q02 <- substr(data[, 1], start = 121, stop = 121)
  index <- which(names(db) == "ST27Q02")
  
  names[index, 1] <- "ST27Q02"
  names[index, 2] <- "How many - televisions"
  
  db$ST27Q03 <- substr(data[, 1], start = 122, stop = 122)
  index <- which(names(db) == "ST27Q03")
  
  names[index, 1] <- "ST27Q03"
  names[index, 2] <- "How many - computers"
  
  db$ST27Q04 <- substr(data[, 1], start = 123, stop = 123)
  index <- which(names(db) == "ST27Q04")
  
  names[index, 1] <- "ST27Q04"
  names[index, 2] <- "How many - cars"
  
  db$ST27Q05 <- substr(data[, 1], start = 124, stop = 124)
  index <- which(names(db) == "ST27Q05")
  
  names[index, 1] <- "ST27Q05"
  names[index, 2] <- "How many - rooms bath or shower"
  
  db$ST28Q01 <- substr(data[, 1], start = 125, stop = 125)
  index <- which(names(db) == "ST28Q01")
  
  names[index, 1] <- "ST28Q01"
  names[index, 2] <- "How many books at home"
  
  db$ST29Q01 <- substr(data[, 1], start = 126, stop = 126)
  index <- which(names(db) == "ST29Q01")
  
  names[index, 1] <- "ST29Q01"
  names[index, 2] <- "Maths Interest - Enjoy Reading"
  
  db$ST29Q02 <- substr(data[, 1], start = 127, stop = 127)
  index <- which(names(db) == "ST29Q02")
  
  names[index, 1] <- "ST29Q02"
  names[index, 2] <- "Instrumental Motivation - Worthwhile for Work"
  
  db$ST29Q03 <- substr(data[, 1], start = 128, stop = 128)
  index <- which(names(db) == "ST29Q03")
  
  names[index, 1] <- "ST29Q03"
  names[index, 2] <- "Maths Interest - Look Forward to Lessons"
  
  db$ST29Q04 <- substr(data[, 1], start = 129, stop = 129)
  index <- which(names(db) == "ST29Q04")
  
  names[index, 1] <- "ST29Q04"
  names[index, 2] <- "Maths Interest - Enjoy Maths"
  
  db$ST29Q05 <- substr(data[, 1], start = 130, stop = 130)
  index <- which(names(db) == "ST29Q05")
  
  names[index, 1] <- "ST29Q05"
  names[index, 2] <- "Instrumental Motivation - Worthwhile for Career Chances"
  
  db$ST29Q06 <- substr(data[, 1], start = 131, stop = 131)
  index <- which(names(db) == "ST29Q06")
  
  names[index, 1] <- "ST29Q06"
  names[index, 2] <- "Maths Interest - Interested"
  
  db$ST29Q07 <- substr(data[, 1], start = 132, stop = 132)
  index <- which(names(db) == "ST29Q07")
  
  names[index, 1] <- "ST29Q07"
  names[index, 2] <- "Instrumental Motivation - Important for Future Study"
  
  db$ST29Q08 <- substr(data[, 1], start = 133, stop = 133)
  index <- which(names(db) == "ST29Q08")
  
  names[index, 1] <- "ST29Q08"
  names[index, 2] <- "Instrumental Motivation - Helps to Get a Job"
  
  db$ST35Q01 <- substr(data[, 1], start = 134, stop = 134)
  index <- which(names(db) == "ST35Q01")
  
  names[index, 1] <- "ST35Q01"
  names[index, 2] <- "Subjective Norms - Friends Do Well in Mathematics"
  
  db$ST35Q02 <- substr(data[, 1], start = 135, stop = 135)
  index <- which(names(db) == "ST35Q02")
  
  names[index, 1] <- "ST35Q02"
  names[index, 2] <- "Subjective Norms - Friends Work Hard on Mathematics"
  
  db$ST35Q03 <- substr(data[, 1], start = 136, stop = 136)
  index <- which(names(db) == "ST35Q03")
  
  names[index, 1] <- "ST35Q03"
  names[index, 2] <- "Subjective Norms - Friends Enjoy Mathematics Tests"
  
  db$ST35Q04 <- substr(data[, 1], start = 137, stop = 137)
  index <- which(names(db) == "ST35Q04")
  
  names[index, 1] <- "ST35Q04"
  names[index, 2] <- "Subjective Norms - Parents Believe Studying Mathematics Is Important"
  
  db$ST35Q05 <- substr(data[, 1], start = 138, stop = 138)
  index <- which(names(db) == "ST35Q05")
  
  names[index, 1] <- "ST35Q05"
  names[index, 2] <- "Subjective Norms - Parents Believe Mathematics Is Important for Career"
  
  db$ST35Q06 <- substr(data[, 1], start = 139, stop = 139)
  index <- which(names(db) == "ST35Q06")
  
  names[index, 1] <- "ST35Q06"
  names[index, 2] <- "Subjective Norms - Parents Like Mathematics"
  
  db$ST37Q01 <- substr(data[, 1], start = 140, stop = 140)
  index <- which(names(db) == "ST37Q01")
  
  names[index, 1] <- "ST37Q01"
  names[index, 2] <- "Maths Self-Efficacy - Using a <Train Timetable>"
  
  db$ST37Q02 <- substr(data[, 1], start = 141, stop = 141)
  index <- which(names(db) == "ST37Q02")
  
  names[index, 1] <- "ST37Q02"
  names[index, 2] <- "Maths Self-Efficacy - Calculating TV Discount"
  
  db$ST37Q03 <- substr(data[, 1], start = 142, stop = 142)
  index <- which(names(db) == "ST37Q03")
  
  names[index, 1] <- "ST37Q03"
  names[index, 2] <- "Maths Self-Efficacy - Calculating Square Metres of Tiles"
  
  db$ST37Q04 <- substr(data[, 1], start = 143, stop = 143)
  index <- which(names(db) == "ST37Q04")
  
  names[index, 1] <- "ST37Q04"
  names[index, 2] <- "Maths Self-Efficacy - Understanding Graphs in Newspapers"
  
  db$ST37Q05 <- substr(data[, 1], start = 144, stop = 144)
  index <- which(names(db) == "ST37Q05")
  
  names[index, 1] <- "ST37Q05"
  names[index, 2] <- "Maths Self-Efficacy - Solving Equation 1"
  
  db$ST37Q06 <- substr(data[, 1], start = 145, stop = 145)
  index <- which(names(db) == "ST37Q06")
  
  names[index, 1] <- "ST37Q06"
  names[index, 2] <- "Maths Self-Efficacy - Distance to Scale"
  
  db$ST37Q07 <- substr(data[, 1], start = 146, stop = 146)
  index <- which(names(db) == "ST37Q07")
  
  names[index, 1] <- "ST37Q07"
  names[index, 2] <- "Maths Self-Efficacy - Solving Equation 2"
  
  db$ST37Q08 <- substr(data[, 1], start = 147, stop = 147)
  index <- which(names(db) == "ST37Q08")
  
  names[index, 1] <- "ST37Q08"
  names[index, 2] <- "Maths Self-Efficacy - Calculate Petrol Consumption Rate"
  
  db$ST42Q01 <- substr(data[, 1], start = 148, stop = 148)
  index <- which(names(db) == "ST42Q01")
  
  names[index, 1] <- "ST42Q01"
  names[index, 2] <- "Maths Anxiety - Worry That It Will Be Difficult"
  
  db$ST42Q02 <- substr(data[, 1], start = 149, stop = 149)
  index <- which(names(db) == "ST42Q02")
  
  names[index, 1] <- "ST42Q02"
  names[index, 2] <- "Maths Self-Concept - Not Good at Maths"
  
  db$ST42Q03 <- substr(data[, 1], start = 150, stop = 150)
  index <- which(names(db) == "ST42Q03")
  
  names[index, 1] <- "ST42Q03"
  names[index, 2] <- "Maths Anxiety - Get Very Tense"
  
  db$ST42Q04 <- substr(data[, 1], start = 151, stop = 151)
  index <- which(names(db) == "ST42Q04")
  
  names[index, 1] <- "ST42Q04"
  names[index, 2] <- "Maths Self-Concept - Get Good <Grades>"
  
  db$ST42Q05 <- substr(data[, 1], start = 152, stop = 152)
  index <- which(names(db) == "ST42Q05")

  names[index, 1] <- "ST42Q05"
  names[index, 2] <- "Maths Anxiety - Get Very Nervous"
  
  db$ST42Q06 <- substr(data[, 1], start = 153, stop = 153)
  index <- which(names(db) == "ST42Q06")
  
  names[index, 1] <- "ST42Q06"
  names[index, 2] <- "Maths Self-Concept - Learn Quickly"

  db$ST42Q07 <- substr(data[, 1], start = 154, stop = 154)
  index <- which(names(db) == "ST42Q07")
  
  names[index, 1] <- "ST42Q07"
  names[index, 2] <- "Maths Self-Concept - One of Best Subjects"
  
  db$ST42Q08 <- substr(data[, 1], start = 155, stop = 155)
  index <- which(names(db) == "ST42Q08")
  
  names[index, 1] <- "ST42Q08"
  names[index, 2] <- "Maths Anxiety - Feel Helplesss"
  
  db$ST42Q09 <- substr(data[, 1], start = 156, stop = 156)
  index <- which(names(db) == "ST42Q09")
  
  names[index, 1] <- "ST42Q09"
  names[index, 2] <- "Maths Self-Concept - Understand Difficult Work"
  
  db$ST42Q10 <- substr(data[, 1], start = 157, stop = 157)
  index <- which(names(db) == "ST42Q10")
  
  names[index, 1] <- "ST42Q10"
  names[index, 2] <- "Maths Anxiety - Worry About Getting Poor <Grades>"
  
  db$ST43Q01 <- substr(data[, 1], start = 158, stop = 158)
  index <- which(names(db) == "ST43Q01")
  
  names[index, 1] <- "ST43Q01"
  names[index, 2] <- "Perceived Control - Can Succeed with Enough Effort"
  
  db$ST43Q02 <- substr(data[, 1], start = 159, stop = 159)
  index <- which(names(db) == "ST43Q02")
  
  names[index, 1] <- "ST43Q02"
  names[index, 2] <- "Perceived Control - Doing Well is Completely Up to Me"
  
  db$ST43Q03 <- substr(data[, 1], start = 160, stop = 160)
  index <- which(names(db) == "ST43Q03")
  
  names[index, 1] <- "ST43Q03"
  names[index, 2] <- "Perceived Control - Family Demands and Problems"
  
  db$ST43Q04 <- substr(data[, 1], start = 161, stop = 161)
  index <- which(names(db) == "ST43Q04")
  
  names[index, 1] <- "ST43Q04"
  names[index, 2] <- "Perceived Control - Different Teachers"
  
  db$ST43Q05 <- substr(data[, 1], start = 162, stop = 162)
  index <- which(names(db) == "ST43Q05")
  
  names[index, 1] <- "ST43Q05"
  names[index, 2] <- "Perceived Control - If I Wanted I Could Perform Well"
  
  db$ST43Q06 <- substr(data[, 1], start = 163, stop = 163)
  index <- which(names(db) == "ST43Q06")
  
  names[index, 1] <- "ST43Q06"
  names[index, 2] <- "Perceived Control - Perform Poorly Regardless"
  
  db$ST44Q01 <- substr(data[, 1], start = 164, stop = 164)
  index <- which(names(db) == "ST44Q01")
  
  names[index, 1] <- "ST44Q01"
  names[index, 2] <- "Attributions to Failure - Not Good at Maths Problems"
  
  db$ST44Q03 <- substr(data[, 1], start = 165, stop = 165)
  index <- which(names(db) == "ST44Q03")
  
  names[index, 1] <- "ST44Q03"
  names[index, 2] <- "Attributions to Failure - Teacher Did Not Explain Well"
  
  db$ST44Q04 <- substr(data[, 1], start = 166, stop = 166)
  index <- which(names(db) == "ST44Q04")
  
  names[index, 1] <- "ST44Q04"
  names[index, 2] <- "Attributions to Failure - Teacher Did Not Explain Well"
  
  db$ST44Q05 <- substr(data[, 1], start = 167, stop = 167)
  index <- which(names(db) == "ST44Q05")
  
  names[index, 1] <- "ST44Q05"
  names[index, 2] <- "Attributions to Failure - Material Too Hard"
  
  db$ST44Q07 <- substr(data[, 1], start = 168, stop = 168)
  index <- which(names(db) == "ST44Q07")
  
  names[index, 1] <- "ST44Q07"
  names[index, 2] <- "Attributions to Failure - Teacher Didnt Get Students Interested"
  
  db$ST44Q08 <- substr(data[, 1], start = 169, stop = 169)
  index <- which(names(db) == "ST44Q08")
  
  names[index, 1] <- "ST44Q08"
  names[index, 2] <- "Attributions to Failure - Unlucky"
  
  db$ST44Q08 <- substr(data[, 1], start = 169, stop = 169)
  index <- which(names(db) == "ST44Q08")
  
  names[index, 1] <- "ST44Q08"
  names[index, 2] <- "Attributions to Failure - Unlucky"
  
  db$ST46Q01 <- substr(data[, 1], start = 170, stop = 170)
  index <- which(names(db) == "ST46Q01")
  
  names[index, 1] <- "ST46Q01"
  names[index, 2] <- "Attributions to Failure - Unlucky"
  
  db$ST46Q02 <- substr(data[, 1], start = 171, stop = 171)
  index <- which(names(db) == "ST46Q02")
  
  names[index, 1] <- "ST46Q02"
  names[index, 2] <- "Maths Work Ethic - Work Hard on Homework"
  
  db$ST46Q03 <- substr(data[, 1], start = 172, stop = 172)
  index <- which(names(db) == "ST46Q03")
  
  names[index, 1] <- "ST46Q03"
  names[index, 2] <- "Maths Work Ethic - Prepared for Exams"
  
  db$ST46Q04 <- substr(data[, 1], start = 173, stop = 173)
  index <- which(names(db) == "ST46Q04")
  
  names[index, 1] <- "ST46Q04"
  names[index, 2] <- "Maths Work Ethic - Study Hard for Quizzes"
  
  db$ST46Q05 <- substr(data[, 1], start = 174, stop = 174)
  index <- which(names(db) == "ST46Q05")
  
  names[index, 1] <- "ST46Q05"
  names[index, 2] <- "Maths Work Ethic - Study Until I Understand Everything"
  
  db$ST46Q06 <- substr(data[, 1], start = 175, stop = 175)
  index <- which(names(db) == "ST46Q06")
  
  names[index, 1] <- "ST46Q06"
  names[index, 2] <- "Maths Work Ethic - Pay Attention in Classes"
  
  db$ST46Q07 <- substr(data[, 1], start = 176, stop = 176)
  index <- which(names(db) == "ST46Q07")
  
  names[index, 1] <- "ST46Q07"
  names[index, 2] <- "Maths Work Ethic - Listen in Classes"
  
  db$ST46Q08 <- substr(data[, 1], start = 177, stop = 177)
  index <- which(names(db) == "ST46Q08")
  
  names[index, 1] <- "ST46Q08"
  names[index, 2] <- "Maths Work Ethic - Avoid Distractions When Studying"
  
  db$ST46Q09 <- substr(data[, 1], start = 178, stop = 178)
  index <- which(names(db) == "ST46Q09")
  
  names[index, 1] <- "ST46Q09"
  names[index, 2] <- "Maths Work Ethic - Keep Work Organized"
  
  db$ST48Q01 <- substr(data[, 1], start = 179, stop = 179)
  index <- which(names(db) == "ST48Q01")
  
  names[index, 1] <- "ST48Q01"
  names[index, 2] <- "Maths Work Ethic - Keep Work Organized"
  
  db$ST48Q02 <- substr(data[, 1], start = 180, stop = 180)
  index <- which(names(db) == "ST48Q02")
  
  names[index, 1] <- "ST48Q02"
  names[index, 2] <- "Maths Intentions - Mathematics vs. Science Related Major in College"
  
  db$ST48Q03 <- substr(data[, 1], start = 181, stop = 181)
  index <- which(names(db) == "ST48Q03")
  
  names[index, 1] <- "ST48Q03"
  names[index, 2] <- "Maths Intentions - Study Harder in Mathematics vs. Language Classes"
  
  db$ST48Q04 <- substr(data[, 1], start = 182, stop = 182)
  index <- which(names(db) == "ST48Q04")
  
  names[index, 1] <- "ST48Q04"
  names[index, 2] <- "Maths Intentions - Take Maximum Number of Mathematics vs. Science Classes"
  
  db$ST48Q05 <- substr(data[, 1], start = 183, stop = 183)
  index <- which(names(db) == "ST48Q05")
  
  names[index, 1] <- "ST48Q05"
  names[index, 2] <- "Maths Intentions - Pursuing a Career That Involves Mathematics vs. Science"
  
  db$ST49Q01 <- substr(data[, 1], start = 184, stop = 184)
  index <- which(names(db) == "ST49Q01")
  
  names[index, 1] <- "ST49Q01"
  names[index, 2] <- "Maths Behaviour - Talk about Maths with Friends"
  
  db$ST49Q02 <- substr(data[, 1], start = 185, stop = 185)
  index <- which(names(db) == "ST49Q02")
  
  names[index, 1] <- "ST49Q02"
  names[index, 2] <- "Maths Behaviour - Help Friends with Maths"
  
  db$ST49Q03 <- substr(data[, 1], start = 186, stop = 186)
  index <- which(names(db) == "ST49Q03")
  
  names[index, 1] <- "ST49Q03"
  names[index, 2] <- "Maths Behaviour - <Extracurricular> Activity"
  
  db$ST49Q04 <- substr(data[, 1], start = 187, stop = 187)
  index <- which(names(db) == "ST49Q04")
  
  names[index, 1] <- "ST49Q04"
  names[index, 2] <- "Maths Behaviour - Participate in Competitions"
  
  db$ST49Q05 <- substr(data[, 1], start = 188, stop = 188)
  index <- which(names(db) == "ST49Q05")
  
  names[index, 1] <- "ST49Q05"
  names[index, 2] <- "Maths Behaviour - Study More Than 2 Extra Hours a Day"
  
  db$ST49Q06 <- substr(data[, 1], start = 189, stop = 189)
  index <- which(names(db) == "ST49Q06")
  
  names[index, 1] <- "ST49Q06"
  names[index, 2] <- "Maths Behaviour - Play Chess"
  
  db$ST49Q07 <- substr(data[, 1], start = 190, stop = 190)
  index <- which(names(db) == "ST49Q07")
  
  names[index, 1] <- "ST49Q07"
  names[index, 2] <- "Maths Behaviour - Computer programming"
  
  db$ST49Q09 <- substr(data[, 1], start = 191, stop = 191)
  index <- which(names(db) == "ST49Q09")
  
  names[index, 1] <- "ST49Q09"
  names[index, 2] <- "Maths Behaviour - Participate in Maths Club"
  
  db$ST53Q01 <- substr(data[, 1], start = 192, stop = 192)
  index <- which(names(db) == "ST53Q01")
  
  names[index, 1] <- "ST53Q01"
  names[index, 2] <- "Learning Strategies - Important Parts vs. Existing Knowledge vs. Learn by Heart"
  
  db$ST53Q02 <- substr(data[, 1], start = 193, stop = 193)
  index <- which(names(db) == "ST53Q02")
  
  names[index, 1] <- "ST53Q02"
  names[index, 2] <- "Learning Strategies - Improve Understanding vs. New Ways vs. Memory"
  
  db$ST53Q03 <- substr(data[, 1], start = 194, stop = 194)
  index <- which(names(db) == "ST53Q03")
  
  names[index, 1] <- "ST53Q03"
  names[index, 2] <- "Learning Strategies - Improve Understanding vs. New Ways vs. Memory"
  
  db$ST53Q04 <- substr(data[, 1], start = 195, stop = 195)
  index <- which(names(db) == "ST53Q04")
  
  names[index, 1] <- "ST53Q04"
  names[index, 2] <- "Learning Strategies - Improve Understanding vs. New Ways vs. Memory"
  
  db$ST55Q01 <- substr(data[, 1], start = 196, stop = 196)
  index <- which(names(db) == "ST55Q01")
  
  names[index, 1] <- "ST55Q01"
  names[index, 2] <- "Out of school lessons - <test lang>"
  
  db$ST55Q02 <- substr(data[, 1], start = 197, stop = 197)
  index <- which(names(db) == "ST55Q02")
  
  names[index, 1] <- "ST55Q02"
  names[index, 2] <- "Out of school lessons - <maths>"
  
  db$ST55Q03 <- substr(data[, 1], start = 198, stop = 198)
  index <- which(names(db) == "ST55Q03")
  
  names[index, 1] <- "ST55Q03"
  names[index, 2] <- "Out of school lessons - <maths>"
  
  db$ST55Q04 <- substr(data[, 1], start = 199, stop = 199)
  index <- which(names(db) == "ST55Q04")
  
  names[index, 1] <- "ST55Q04"
  names[index, 2] <- "Out of school lessons - other"
  
  db$ST57Q01 <- substr(data[, 1], start = 200, stop = 203)
  index <- which(names(db) == "ST57Q01")
  
  names[index, 1] <- "ST57Q01"
  names[index, 2] <- "Out-of-School Study Time - Homework"
  
  db$ST57Q02 <- substr(data[, 1], start = 204, stop = 207)
  index <- which(names(db) == "ST57Q02")
  
  names[index, 1] <- "ST57Q02"
  names[index, 2] <- "Out-of-School Study Time - Guided Homework"
  
  db$ST57Q03 <- substr(data[, 1], start = 208, stop = 211)
  index <- which(names(db) == "ST57Q03")
  
  names[index, 1] <- "ST57Q03"
  names[index, 2] <- "Out-of-School Study Time - Guided Homework"
  
  db$ST57Q04 <- substr(data[, 1], start = 212, stop = 215)
  index <- which(names(db) == "ST57Q04")
  
  names[index, 1] <- "ST57Q04"
  names[index, 2] <- "Out-of-School Study Time - Commercial Company"
  
  db$ST57Q05 <- substr(data[, 1], start = 216, stop = 219)
  index <- which(names(db) == "ST57Q05")
  
  names[index, 1] <- "ST57Q05"
  names[index, 2] <- "Out-of-School Study Time - With Parent"
  
  db$ST57Q06 <- substr(data[, 1], start = 220, stop = 223)
  index <- which(names(db) == "ST57Q06")
  
  names[index, 1] <- "ST57Q06"
  names[index, 2] <- "Out-of-School Study Time - Computer"
  
  db$ST61Q01 <- substr(data[, 1], start = 224, stop = 224)
  index <- which(names(db) == "ST61Q01")
  
  names[index, 1] <- "ST61Q01"
  names[index, 2] <- "Experience with Applied Maths Tasks - Use <Train Timetable>"
  
  db$ST61Q02 <- substr(data[, 1], start = 225, stop = 225)
  index <- which(names(db) == "ST61Q02")
  
  names[index, 1] <- "ST61Q02"
  names[index, 2] <- "Experience with Applied Maths Tasks - Calculate Price including Tax"
  
  db$ST61Q03 <- substr(data[, 1], start = 226, stop = 226)
  index <- which(names(db) == "ST61Q03")
  
  names[index, 1] <- "ST61Q03"
  names[index, 2] <- "Experience with Applied Maths Tasks - Calculate Square Metres"
  
  db$ST61Q04 <- substr(data[, 1], start = 227, stop = 227)
  index <- which(names(db) == "ST61Q04")
  
  names[index, 1] <- "ST61Q04"
  names[index, 2] <- "Experience with Applied Maths Tasks - Understand Scientific Tables"
  
  db$ST61Q05 <- substr(data[, 1], start = 228, stop = 228)
  index <- which(names(db) == "ST61Q05")
  
  names[index, 1] <- "ST61Q05"
  names[index, 2] <- "Experience with Pure Maths Tasks - Solve Equation 1"
  
  db$ST61Q06 <- substr(data[, 1], start = 229, stop = 229)
  index <- which(names(db) == "ST61Q06")
  
  names[index, 1] <- "ST61Q06"
  names[index, 2] <- "Experience with Applied Maths Tasks - Use a Map to Calculate Distance"
  
  db$ST61Q07 <- substr(data[, 1], start = 230, stop = 230)
  index <- which(names(db) == "ST61Q07")
  
  names[index, 1] <- "ST61Q07"
  names[index, 2] <- "Experience with Pure Maths Tasks - Solve Equation 2"
  
  db$ST61Q08 <- substr(data[, 1], start = 231, stop = 231)
  index <- which(names(db) == "ST61Q08")
  
  names[index, 1] <- "ST61Q08"
  names[index, 2] <- "Experience with Applied Maths Tasks - Calculate Power Consumption Rate"
  
  db$ST61Q09 <- substr(data[, 1], start = 232, stop = 232)
  index <- which(names(db) == "ST61Q09")
  
  names[index, 1] <- "ST61Q09"
  names[index, 2] <- "Experience with Applied Maths Tasks - Solve Equation 3"
  
  db$ST62Q01 <- substr(data[, 1], start = 233, stop = 233)
  index <- which(names(db) == "ST62Q01")
  
  names[index, 1] <- "ST62Q01"
  names[index, 2] <- "Familiarity with Maths Concepts - Exponential Function"
  
  db$ST62Q02 <- substr(data[, 1], start = 234, stop = 234)
  index <- which(names(db) == "ST62Q02")
  
  names[index, 1] <- "ST62Q02"
  names[index, 2] <- "Familiarity with Maths Concepts - Divisor"
  
  db$ST62Q03 <- substr(data[, 1], start = 235, stop = 235)
  index <- which(names(db) == "ST62Q03")
  
  names[index, 1] <- "ST62Q03"
  names[index, 2] <- "Familiarity with Maths Concepts - Quadratic Function"
  
  db$ST62Q04 <- substr(data[, 1], start = 236, stop = 236)
  index <- which(names(db) == "ST62Q04")
  
  names[index, 1] <- "ST62Q04"
  names[index, 2] <- "Overclaiming - Proper Number"
  
  db$ST62Q06 <- substr(data[, 1], start = 237, stop = 237)
  index <- which(names(db) == "ST62Q06")
  
  names[index, 1] <- "ST62Q06"
  names[index, 2] <- "Familiarity with Maths Concepts - Linear Equation"
  
  db$ST62Q07 <- substr(data[, 1], start = 238, stop = 238)
  index <- which(names(db) == "ST62Q07")
  
  names[index, 1] <- "ST62Q07"
  names[index, 2] <- "Familiarity with Maths Concepts - Vectors"
  
  db$ST62Q08 <- substr(data[, 1], start = 239, stop = 239)
  index <- which(names(db) == "ST62Q08")
  
  names[index, 1] <- "ST62Q08"
  names[index, 2] <- "Familiarity with Maths Concepts - Complex Number"
  
  db$ST62Q09 <- substr(data[, 1], start = 240, stop = 240)
  index <- which(names(db) == "ST62Q09")
  
  names[index, 1] <- "ST62Q09"
  names[index, 2] <- "Familiarity with Maths Concepts - Rational Number"
  
  db$ST62Q10 <- substr(data[, 1], start = 241, stop = 241)
  index <- which(names(db) == "ST62Q10")
  
  names[index, 1] <- "ST62Q10"
  names[index, 2] <- "Familiarity with Maths Concepts - Radicals"
  
  db$ST62Q11 <- substr(data[, 1], start = 242, stop = 242)
  index <- which(names(db) == "ST62Q11")
  
  names[index, 1] <- "ST62Q11"
  names[index, 2] <- "Overclaiming - Subjunctive Scaling"
  
  db$ST62Q12 <- substr(data[, 1], start = 243, stop = 243)
  index <- which(names(db) == "ST62Q12")
  
  names[index, 1] <- "ST62Q12"
  names[index, 2] <- "Familiarity with Maths Concepts - Polygon"
  
  db$ST62Q13 <- substr(data[, 1], start = 244, stop = 244)
  index <- which(names(db) == "ST62Q13")
  
  names[index, 1] <- "ST62Q13"
  names[index, 2] <- "Overclaiming - Declarative Fraction"
  
  db$ST62Q15 <- substr(data[, 1], start = 245, stop = 245)
  index <- which(names(db) == "ST62Q15")
  
  names[index, 1] <- "ST62Q15"
  names[index, 2] <- "Familiarity with Maths Concepts - Congruent Figure"
  
  db$ST62Q16 <- substr(data[, 1], start = 246, stop = 246)
  index <- which(names(db) == "ST62Q16")
  
  names[index, 1] <- "ST62Q16"
  names[index, 2] <- "Familiarity with Maths Concepts - Cosine"
  
  db$ST62Q17 <- substr(data[, 1], start = 247, stop = 247)
  index <- which(names(db) == "ST62Q17")
  
  names[index, 1] <- "ST62Q17"
  names[index, 2] <- "Familiarity with Maths Concepts - Arithmetic Mean"
  
  db$ST62Q19 <- substr(data[, 1], start = 248, stop = 248)
  index <- which(names(db) == "ST62Q19")
  
  names[index, 1] <- "ST62Q19"
  names[index, 2] <- "Familiarity with Maths Concepts - Probability"
  
  db$ST69Q01 <- substr(data[, 1], start = 249, stop = 252)
  index <- which(names(db) == "ST69Q01")
  
  names[index, 1] <- "ST69Q01"
  names[index, 2] <- "Min in <class period> - <test lang>"
  
  db$ST69Q02 <- substr(data[, 1], start = 253, stop = 256)
  index <- which(names(db) == "ST69Q02")
  
  names[index, 1] <- "ST69Q02"
  names[index, 2] <- "Min in <class period> - <Maths>"
  
  db$ST69Q03 <- substr(data[, 1], start = 257, stop = 260)
  index <- which(names(db) == "ST69Q03")
  
  names[index, 1] <- "ST69Q03"
  names[index, 2] <- "Min in <class period> - <Science>"
  
  db$ST70Q01 <- substr(data[, 1], start = 261, stop = 264)
  index <- which(names(db) == "ST70Q01")
  
  names[index, 1] <- "ST70Q01"
  names[index, 2] <- "No of <class period> p/wk - <test lang>"
  
  db$ST70Q02 <- substr(data[, 1], start = 265, stop = 268)
  index <- which(names(db) == "ST70Q02")
  
  names[index, 1] <- "ST70Q02"
  names[index, 2] <- "No of <class period> p/wk - <Maths>"
  
  db$ST70Q03 <- substr(data[, 1], start = 269, stop = 272)
  index <- which(names(db) == "ST70Q03")
  
  names[index, 1] <- "ST70Q03"
  names[index, 2] <- "No of <class period> p/wk - <Science>"
  
  db$ST71Q01 <- substr(data[, 1], start = 273, stop = 276)
  index <- which(names(db) == "ST71Q01")
  
  names[index, 1] <- "ST71Q01"
  names[index, 2] <- "No of ALL <class period> a week"
  
  db$ST72Q01 <- substr(data[, 1], start = 277, stop = 280)
  index <- which(names(db) == "ST72Q01")
  
  names[index, 1] <- "ST72Q01"
  names[index, 2] <- "Class Size - No of Students in <Test Language> Class"
  
  db$ST73Q01 <- substr(data[, 1], start = 281, stop = 281)
  index <- which(names(db) == "ST73Q01")
  
  names[index, 1] <- "ST73Q01"
  names[index, 2] <- "OTL - Algebraic Word Problem in Maths Lesson"
  
  db$ST73Q02 <- substr(data[, 1], start = 282, stop = 282)
  index <- which(names(db) == "ST73Q02")
  
  names[index, 1] <- "ST73Q02"
  names[index, 2] <- "OTL - Algebraic Word Problem in Tests"
  
  db$ST74Q01 <- substr(data[, 1], start = 283, stop = 283)
  index <- which(names(db) == "ST74Q01")
  
  names[index, 1] <- "ST74Q01"
  names[index, 2] <- "OTL - Procedural Task in Maths Lesson"
  
  db$ST74Q02 <- substr(data[, 1], start = 284, stop = 284)
  index <- which(names(db) == "ST74Q02")
  
  names[index, 1] <- "ST74Q02"
  names[index, 2] <- "OTL - Procedural Task in Tests"
  
  db$ST75Q01 <- substr(data[, 1], start = 285, stop = 285)
  index <- which(names(db) == "ST75Q01")
  
  names[index, 1] <- "ST75Q01"
  names[index, 2] <- "OTL - Pure Maths Reasoning in Maths Lesson"
  
  db$ST75Q02 <- substr(data[, 1], start = 286, stop = 286)
  index <- which(names(db) == "ST75Q02")
  
  names[index, 1] <- "ST75Q02"
  names[index, 2] <- "OTL - Pure Maths Reasoning in Tests"
  
  db$ST76Q01 <- substr(data[, 1], start = 287, stop = 287)
  index <- which(names(db) == "ST76Q01")
  
  names[index, 1] <- "ST76Q01"
  names[index, 2] <- "OTL - Applied Maths Reasoning in Maths Lesson"
  
  db$ST76Q02 <- substr(data[, 1], start = 288, stop = 288)
  index <- which(names(db) == "ST76Q02")
  
  names[index, 1] <- "ST76Q02"
  names[index, 2] <- "OTL - Applied Maths Reasoning in Tests"
  
  db$ST77Q01 <- substr(data[, 1], start = 289, stop = 289)
  index <- which(names(db) == "ST77Q01")
  
  names[index, 1] <- "ST77Q01"
  names[index, 2] <- "Maths Teaching - Teacher shows interest"
  
  db$ST77Q02 <- substr(data[, 1], start = 290, stop = 290)
  index <- which(names(db) == "ST77Q02")
  
  names[index, 1] <- "ST77Q02"
  names[index, 2] <- "Maths Teaching - Extra help"
  
  db$ST77Q04 <- substr(data[, 1], start = 291, stop = 291)
  index <- which(names(db) == "ST77Q04")
  
  names[index, 1] <- "ST77Q04"
  names[index, 2] <- "Maths Teaching - Teacher helps"
  
  db$ST77Q05 <- substr(data[, 1], start = 292, stop = 292)
  index <- which(names(db) == "ST77Q05")
  
  names[index, 1] <- "ST77Q05"
  names[index, 2] <- "Maths Teaching - Teacher continues"

  db$ST77Q06 <- substr(data[, 1], start = 293, stop = 293)
  index <- which(names(db) == "ST77Q06")
  
  names[index, 1] <- "ST77Q06"
  names[index, 2] <- "Maths Teaching - Express opinions"
  
  db$ST79Q01 <- substr(data[, 1], start = 294, stop = 294)
  index <- which(names(db) == "ST79Q01")
  
  names[index, 1] <- "ST79Q01"
  names[index, 2] <- "Teacher-Directed Instruction - Sets Clear Goals"
  
  db$ST79Q02 <- substr(data[, 1], start = 295, stop = 295)
  index <- which(names(db) == "ST79Q02")
  
  names[index, 1] <- "ST79Q02"
  names[index, 2] <- "Teacher-Directed Instruction - Encourages Thinking and Reasoning"
  
  db$ST79Q03 <- substr(data[, 1], start = 296, stop = 296)
  index <- which(names(db) == "ST79Q03")
  
  names[index, 1] <- "ST79Q03"
  names[index, 2] <- "Student Orientation - Differentiates Between Students When Giving Tasks"
  
  db$ST79Q04 <- substr(data[, 1], start = 297, stop = 297)
  index <- which(names(db) == "ST79Q04")
  
  names[index, 1] <- "ST79Q04"
  names[index, 2] <- "Student Orientation - Assigns Complex Projects"
  
  db$ST79Q05 <- substr(data[, 1], start = 298, stop = 298)
  index <- which(names(db) == "ST79Q05")
  
  names[index, 1] <- "ST79Q05"
  names[index, 2] <- "Formative Assessment - Gives Feedback"
  
  db$ST79Q06 <- substr(data[, 1], start = 299, stop = 299)
  index <- which(names(db) == "ST79Q06")
  
  names[index, 1] <- "ST79Q06"
  names[index, 2] <- "Teacher-Directed Instruction - Checks Understanding"
  
  db$ST79Q07 <- substr(data[, 1], start = 300, stop = 300)
  index <- which(names(db) == "ST79Q07")
  
  names[index, 1] <- "ST79Q07"
  names[index, 2] <- "Student Orientation - Has Students Work in Small Groups"
  
  db$ST79Q08 <- substr(data[, 1], start = 301, stop = 301)
  index <- which(names(db) == "ST79Q08")
  
  names[index, 1] <- "ST79Q08"
  names[index, 2] <- "Teacher-Directed Instruction - Summarizes Previous Lessons"
  
  db$ST79Q10 <- substr(data[, 1], start = 302, stop = 302)
  index <- which(names(db) == "ST79Q10")
  
  names[index, 1] <- "ST79Q10"
  names[index, 2] <- "Student Orientation - Plans Classroom Activities"
  
  db$ST79Q11 <- substr(data[, 1], start = 303, stop = 303)
  index <- which(names(db) == "ST79Q11")
  
  names[index, 1] <- "ST79Q11"
  names[index, 2] <- "Formative Assessment - Gives Feedback on Strengths and Weaknesses"
  
  db$ST79Q12 <- substr(data[, 1], start = 304, stop = 304)
  index <- which(names(db) == "ST79Q12")
  
  names[index, 1] <- "ST79Q12"
  names[index, 2] <- "Formative Assessment - Informs about Expectations"
  
  db$ST79Q15 <- substr(data[, 1], start = 305, stop = 305)
  index <- which(names(db) == "ST79Q15")
  
  names[index, 1] <- "ST79Q15"
  names[index, 2] <- "Teacher-Directed Instruction - Informs about Learning Goals"
  
  db$ST79Q17 <- substr(data[, 1], start = 306, stop = 306)
  index <- which(names(db) == "ST79Q17")
  
  names[index, 1] <- "ST79Q17"
  names[index, 2] <- "Formative Assessment - Tells How to Get Better"
  
  db$ST80Q01 <- substr(data[, 1], start = 307, stop = 307)
  index <- which(names(db) == "ST80Q01")
  
  names[index, 1] <- "ST80Q01"
  names[index, 2] <- "Cognitive Activation - Teacher Encourages to Reflect Problems"
  
  db$ST80Q04 <- substr(data[, 1], start = 308, stop = 308)
  index <- which(names(db) == "ST80Q04")
  
  names[index, 1] <- "ST80Q04"
  names[index, 2] <- "Cognitive Activation - Gives Problems that Require to Think"
  
  db$ST80Q05 <- substr(data[, 1], start = 309, stop = 309)
  index <- which(names(db) == "ST80Q05")
  
  names[index, 1] <- "ST80Q05"
  names[index, 2] <- "Cognitive Activation - Asks to Use Own Procedures"
  
  db$ST80Q06 <- substr(data[, 1], start = 310, stop = 310)
  index <- which(names(db) == "ST80Q06")
  
  names[index, 1] <- "ST80Q06"
  names[index, 2] <- "Cognitive Activation - Presents Problems with No Obvious Solutions"
  
  db$ST80Q07 <- substr(data[, 1], start = 311, stop = 311)
  index <- which(names(db) == "ST80Q07")
  
  names[index, 1] <- "ST80Q07"
  names[index, 2] <- "Cognitive Activation - Presents Problems in Different Contexts"
  
  db$ST80Q08 <- substr(data[, 1], start = 312, stop = 312)
  index <- which(names(db) == "ST80Q08")
  
  names[index, 1] <- "ST80Q08"
  names[index, 2] <- "Cognitive Activation - Helps Learn from Mistakes"
  
  db$ST80Q09 <- substr(data[, 1], start = 313, stop = 313)
  index <- which(names(db) == "ST80Q09")
  
  names[index, 1] <- "ST80Q09"
  names[index, 2] <- "Cognitive Activation - Asks for Explanations"
  
  db$ST80Q10 <- substr(data[, 1], start = 314, stop = 314)
  index <- which(names(db) == "ST80Q10")
  
  names[index, 1] <- "ST80Q10"
  names[index, 2] <- "Cognitive Activation - Apply What We Learned"
  
  db$ST80Q11 <- substr(data[, 1], start = 315, stop = 315)
  index <- which(names(db) == "ST80Q11")
  
  names[index, 1] <- "ST80Q11"
  names[index, 2] <- "Cognitive Activation - Problems with Multiple Solutions"
  
  db$ST81Q01 <- substr(data[, 1], start = 316, stop = 316)
  index <- which(names(db) == "ST81Q01")
  
  names[index, 1] <- "ST81Q01"
  names[index, 2] <- "Disciplinary Climate - Students Don’t Listen"
  
  db$ST81Q02 <- substr(data[, 1], start = 317, stop = 317)
  index <- which(names(db) == "ST81Q02")
  
  names[index, 1] <- "ST81Q02"
  names[index, 2] <- "Disciplinary Climate - Noise and Disorder"
  
  db$ST81Q03 <- substr(data[, 1], start = 318, stop = 318)
  index <- which(names(db) == "ST81Q03")
  
  names[index, 1] <- "ST81Q03"
  names[index, 2] <- "Disciplinary Climate - Teacher Has to Wait Until its Quiet"
  
  db$ST81Q04 <- substr(data[, 1], start = 319, stop = 319)
  index <- which(names(db) == "ST81Q04")
  
  names[index, 1] <- "ST81Q04"
  names[index, 2] <- "Disciplinary Climate - Students Don’t Work Well"
  
  db$ST81Q05 <- substr(data[, 1], start = 320, stop = 320)
  index <- which(names(db) == "ST81Q05")
  
  names[index, 1] <- "ST81Q05"
  names[index, 2] <- "Disciplinary Climate - Students Start Working Late"
  
  db$ST82Q01 <- substr(data[, 1], start = 321, stop = 321)
  index <- which(names(db) == "ST82Q01")
  
  names[index, 1] <- "ST82Q01"
  names[index, 2] <- "Vignette Teacher Support - Homework Every Other Day/Back in Time"
  
  db$ST82Q02 <- substr(data[, 1], start = 322, stop = 322)
  index <- which(names(db) == "ST82Q02")
  
  names[index, 1] <- "ST82Q02"
  names[index, 2] <- "Vignette Teacher Support - Homework Once a Week/Back in Time"
  
  db$ST82Q03 <- substr(data[, 1], start = 323, stop = 323)
  index <- which(names(db) == "ST82Q03")
  
  names[index, 1] <- "ST82Q03"
  names[index, 2] <- "Vignette Teacher Support - Homework Once a Week/Not Back in Time"
  
  db$ST83Q01 <- substr(data[, 1], start = 324, stop = 324)
  index <- which(names(db) == "ST83Q01")
  
  names[index, 1] <- "ST83Q01"
  names[index, 2] <- "Teacher Support - Lets Us Know We Have to Work Hard"
  
  db$ST83Q02 <- substr(data[, 1], start = 325, stop = 325)
  index <- which(names(db) == "ST83Q02")
  
  names[index, 1] <- "ST83Q02"
  names[index, 2] <- "Teacher Support - Provides Extra Help When Needed"
  
  db$ST83Q03 <- substr(data[, 1], start = 326, stop = 326)
  index <- which(names(db) == "ST83Q03")
  
  names[index, 1] <- "ST83Q03"
  names[index, 2] <- "Teacher Support - Helps Students with Learning"
  
  db$ST83Q04 <- substr(data[, 1], start = 327, stop = 327)
  index <- which(names(db) == "ST83Q04")
  
  names[index, 1] <- "ST83Q04"
  names[index, 2] <- "Teacher Support - Gives Opportunity to Express Opinions"
  
  db$ST84Q01 <- substr(data[, 1], start = 328, stop = 328)
  index <- which(names(db) == "ST84Q01")
  
  names[index, 1] <- "ST84Q01"
  names[index, 2] <- "Vignette Classroom Management - Students Frequently Interrupt/Teacher Arrives Early"
  
  db$ST84Q02 <- substr(data[, 1], start = 329, stop = 329)
  index <- which(names(db) == "ST84Q02")
  
  names[index, 1] <- "ST84Q02"
  names[index, 2] <- "Vignette Classroom Management - Students Are Calm/Teacher Arrives on Time"
  
  db$ST84Q03 <- substr(data[, 1], start = 330, stop = 330)
  index <- which(names(db) == "ST84Q03")
  
  names[index, 1] <- "ST84Q03"
  names[index, 2] <- "Vignette Classroom Management - Students Frequently Interrupt/Teacher Arrives Late"
  
  db$ST85Q01 <- substr(data[, 1], start = 331, stop = 331)
  index <- which(names(db) == "ST85Q01")
  
  names[index, 1] <- "ST85Q01"
  names[index, 2] <- "Classroom Management - Students Listen"
  
  db$ST85Q02 <- substr(data[, 1], start = 332, stop = 332)
  index <- which(names(db) == "ST85Q02")
  
  names[index, 1] <- "ST85Q02"
  names[index, 2] <- "Classroom Management - Teacher Keeps Class Orderly"
  
  db$ST85Q03 <- substr(data[, 1], start = 333, stop = 333)
  index <- which(names(db) == "ST85Q03")
  
  names[index, 1] <- "ST85Q03"
  names[index, 2] <- "Classroom Management - Teacher Starts On Time"
  
  db$ST85Q04 <- substr(data[, 1], start = 334, stop = 334)
  index <- which(names(db) == "ST85Q04")
  
  names[index, 1] <- "ST85Q04"
  names[index, 2] <- "Classroom Management - Wait Long to <Quiet Down>"
  
  db$ST86Q01 <- substr(data[, 1], start = 335, stop = 335)
  index <- which(names(db) == "ST86Q01")
  
  names[index, 1] <- "ST86Q01"
  names[index, 2] <- "Student-Teacher Relations - Get Along with Teachers"
  
  db$ST86Q02 <- substr(data[, 1], start = 336, stop = 336)
  index <- which(names(db) == "ST86Q02")
  
  names[index, 1] <- "ST86Q02"
  names[index, 2] <- "Student-Teacher Relations - Teachers Are Interested"
  
  db$ST86Q03 <- substr(data[, 1], start = 337, stop = 337)
  index <- which(names(db) == "ST86Q03")
  
  names[index, 1] <- "ST86Q03"
  names[index, 2] <- "Student-Teacher Relations - Teachers Listen to Students"
  
  db$ST86Q04 <- substr(data[, 1], start = 338, stop = 338)
  index <- which(names(db) == "ST86Q04")
  
  names[index, 1] <- "ST86Q04"
  names[index, 2] <- "Student-Teacher Relations - Teachers Help Students"
  
  db$ST86Q05 <- substr(data[, 1], start = 339, stop = 339)
  index <- which(names(db) == "ST86Q05")
  
  names[index, 1] <- "ST86Q05"
  names[index, 2] <- "Student-Teacher Relations - Teachers Treat Students Fair"
  
  db$ST87Q01 <- substr(data[, 1], start = 340, stop = 340)
  index <- which(names(db) == "ST87Q01")
  
  names[index, 1] <- "ST87Q01"
  names[index, 2] <- "Sense of Belonging - Feel Like Outsider"
  
  db$ST87Q02 <- substr(data[, 1], start = 341, stop = 341)
  index <- which(names(db) == "ST87Q02")
  
  names[index, 1] <- "ST87Q02"
  names[index, 2] <- "Sense of Belonging - Make Friends Easily"
  
  db$ST87Q03 <- substr(data[, 1], start = 342, stop = 342)
  index <- which(names(db) == "ST87Q03")
  
  names[index, 1] <- "ST87Q03"
  names[index, 2] <- "Sense of Belonging - Belong at School"
  
  db$ST87Q04 <- substr(data[, 1], start = 343, stop = 343)
  index <- which(names(db) == "ST87Q04")
  
  names[index, 1] <- "ST87Q04"
  names[index, 2] <- "Sense of Belonging - Feel Awkward at School"
  
  db$ST87Q05 <- substr(data[, 1], start = 344, stop = 344)
  index <- which(names(db) == "ST87Q05")
  
  names[index, 1] <- "ST87Q05"
  names[index, 2] <- "Sense of Belonging - Liked by Other Students"
  
  db$ST87Q06 <- substr(data[, 1], start = 345, stop = 345)
  index <- which(names(db) == "ST87Q06")
  
  names[index, 1] <- "ST87Q06"
  names[index, 2] <- "Sense of Belonging - Feel Lonely at School"
  
  db$ST87Q07 <- substr(data[, 1], start = 346, stop = 346)
  index <- which(names(db) == "ST87Q07")
  
  names[index, 1] <- "ST87Q07"
  names[index, 2] <- "Sense of Belonging - Feel Happy at School"
  
  db$ST87Q08 <- substr(data[, 1], start = 347, stop = 347)
  index <- which(names(db) == "ST87Q08")
  
  names[index, 1] <- "ST87Q08"
  names[index, 2] <- "Sense of Belonging - Things Are Ideal at School"
  
  db$ST87Q09 <- substr(data[, 1], start = 348, stop = 348)
  index <- which(names(db) == "ST87Q09")
  
  names[index, 1] <- "ST87Q09"
  names[index, 2] <- "Sense of Belonging - Satisfied at School"
  
  db$ST88Q01 <- substr(data[, 1], start = 349, stop = 349)
  index <- which(names(db) == "ST88Q01")
  
  names[index, 1] <- "ST88Q01"
  names[index, 2] <- "Attitude towards School - Does Little to Prepare Me for Life"
  
  db$ST88Q02 <- substr(data[, 1], start = 350, stop = 350)
  index <- which(names(db) == "ST88Q02")
  
  names[index, 1] <- "ST88Q02"
  names[index, 2] <- "Attitude towards School - Waste of Time"
  
  db$ST88Q03 <- substr(data[, 1], start = 351, stop = 351)
  index <- which(names(db) == "ST88Q03")
  
  names[index, 1] <- "ST88Q03"
  names[index, 2] <- "Attitude towards School - Gave Me Confidence"
  
  db$ST88Q04 <- substr(data[, 1], start = 352, stop = 352)
  index <- which(names(db) == "ST88Q04")
  
  names[index, 1] <- "ST88Q04"
  names[index, 2] <- "Attitude towards School - Useful for Job"
  
  db$ST89Q02 <- substr(data[, 1], start = 353, stop = 353)
  index <- which(names(db) == "ST89Q02")
  
  names[index, 1] <- "ST89Q02"
  names[index, 2] <- "Attitude toward School - Helps to Get a Job"
  
  db$ST89Q03 <- substr(data[, 1], start = 354, stop = 354)
  index <- which(names(db) == "ST89Q03")
  
  names[index, 1] <- "ST89Q03"
  names[index, 2] <- "Attitude toward School - Prepare for College"
  
  db$ST89Q04 <- substr(data[, 1], start = 355, stop = 355)
  index <- which(names(db) == "ST89Q04")
  
  names[index, 1] <- "ST89Q04"
  names[index, 2] <- "Attitude toward School - Enjoy Good Grades"
  
  db$ST89Q05 <- substr(data[, 1], start = 356, stop = 356)
  index <- which(names(db) == "ST89Q05")
  
  names[index, 1] <- "ST89Q05"
  names[index, 2] <- "Attitude toward School - Trying Hard is Important"
  
  db$ST91Q01 <- substr(data[, 1], start = 357, stop = 357)
  index <- which(names(db) == "ST91Q01")
  
  names[index, 1] <- "ST91Q01"
  names[index, 2] <- "Perceived Control - Can Succeed with Enough Effort"
  
  db$ST91Q02 <- substr(data[, 1], start = 358, stop = 358)
  index <- which(names(db) == "ST91Q02")
  
  names[index, 1] <- "ST91Q02"
  names[index, 2] <- "Perceived Control - My Choice Whether I Will Be Good"
  
  db$ST91Q03 <- substr(data[, 1], start = 359, stop = 359)
  index <- which(names(db) == "ST91Q03")
  
  names[index, 1] <- "ST91Q03"
  names[index, 2] <- "Perceived Control - Problems Prevent from Putting Effort into School"
  
  db$ST91Q04 <- substr(data[, 1], start = 360, stop = 360)
  index <- which(names(db) == "ST91Q04")
  
  names[index, 1] <- "ST91Q04"
  names[index, 2] <- "Perceived Control - Different Teachers Would Make Me Try Harder"
  
  db$ST91Q05 <- substr(data[, 1], start = 361, stop = 361)
  index <- which(names(db) == "ST91Q05")
  
  names[index, 1] <- "ST91Q05"
  names[index, 2] <- "Perceived Control - Could Perform Well if I Wanted"
  
  db$ST91Q06 <- substr(data[, 1], start = 362, stop = 362)
  index <- which(names(db) == "ST91Q06")
  
  names[index, 1] <- "ST91Q06"
  names[index, 2] <- "Perceived Control - Perform Poor Regardless"
  
  db$ST93Q01 <- substr(data[, 1], start = 363, stop = 363)
  index <- which(names(db) == "ST93Q01")
  
  names[index, 1] <- "ST93Q01"
  names[index, 2] <- "Perseverance - Give up easily"
  
  db$ST93Q03 <- substr(data[, 1], start = 364, stop = 364)
  index <- which(names(db) == "ST93Q03")
  
  names[index, 1] <- "ST93Q03"
  names[index, 2] <- "Perseverance - Put off difficult problems"
  
  db$ST93Q04 <- substr(data[, 1], start = 365, stop = 365)
  index <- which(names(db) == "ST93Q04")
  
  names[index, 1] <- "ST93Q04"
  names[index, 2] <- "Perseverance - Remain interested"
  
  db$ST93Q06 <- substr(data[, 1], start = 366, stop = 366)
  index <- which(names(db) == "ST93Q06")
  
  names[index, 1] <- "ST93Q06"
  names[index, 2] <- "Perseverance - Remain interested"
  
  db$ST93Q07 <- substr(data[, 1], start = 367, stop = 367)
  index <- which(names(db) == "ST93Q07")
  
  names[index, 1] <- "ST93Q07"
  names[index, 2] <- "Perseverance - Exceed expectations"
  
  db$ST94Q05 <- substr(data[, 1], start = 368, stop = 368)
  index <- which(names(db) == "ST94Q05")
  
  names[index, 1] <- "ST94Q05"
  names[index, 2] <- "Openness for Problem Solving - Can Handle a Lot of Information"
  
  db$ST94Q06 <- substr(data[, 1], start = 369, stop = 369)
  index <- which(names(db) == "ST94Q06")
  
  names[index, 1] <- "ST94Q06"
  names[index, 2] <- "Openness for Problem Solving - Quick to Understand"
  
  db$ST94Q09 <- substr(data[, 1], start = 370, stop = 370)
  index <- which(names(db) == "ST94Q09")
  
  names[index, 1] <- "ST94Q09"
  names[index, 2] <- "Openness for Problem Solving - Seek Explanations"
  
  db$ST94Q10 <- substr(data[, 1], start = 371, stop = 371)
  index <- which(names(db) == "ST94Q10")
  
  names[index, 1] <- "ST94Q10"
  names[index, 2] <- "Openness for Problem Solving - Can Link Facts"
  
  db$ST94Q14 <- substr(data[, 1], start = 372, stop = 372)
  index <- which(names(db) == "ST94Q14")
  
  names[index, 1] <- "ST94Q14"
  names[index, 2] <- "Openness for Problem Solving - Like to Solve Complex Problems"
  
  db$ST96Q01 <- substr(data[, 1], start = 373, stop = 373)
  index <- which(names(db) == "ST96Q01")
  
  names[index, 1] <- "ST96Q01"
  names[index, 2] <- "Problem Text Message - Press every button"
  
  db$ST96Q02 <- substr(data[, 1], start = 374, stop = 374)
  index <- which(names(db) == "ST96Q02")
  
  names[index, 1] <- "ST96Q02"
  names[index, 2] <- "Problem Text Message - Trace steps"
  
  db$ST96Q03 <- substr(data[, 1], start = 375, stop = 375)
  index <- which(names(db) == "ST96Q03")
  
  names[index, 1] <- "ST96Q03"
  names[index, 2] <- "Problem Text Message - Manual"
  
  db$ST96Q05 <- substr(data[, 1], start = 376, stop = 376)
  index <- which(names(db) == "ST96Q05")
  
  names[index, 1] <- "ST96Q05"
  names[index, 2] <- "Problem Text Message - Ask a friend"
  
  db$ST101Q01 <- substr(data[, 1], start = 377, stop = 377)
  index <- which(names(db) == "ST101Q01")
  
  names[index, 1] <- "ST101Q01"
  names[index, 2] <- "Problem Route Selection - Read brochure"
  
  db$ST101Q02 <- substr(data[, 1], start = 378, stop = 378)
  index <- which(names(db) == "ST101Q02")
  
  names[index, 1] <- "ST101Q02"
  names[index, 2] <- "Problem Route Selection - Study map"
  
  db$ST101Q03 <- substr(data[, 1], start = 379, stop = 379)
  index <- which(names(db) == "ST101Q03")
  
  names[index, 1] <- "ST101Q03"
  names[index, 2] <- "Problem Route Selection - Leave it to brother"
  
  db$ST101Q05 <- substr(data[, 1], start = 380, stop = 380)
  index <- which(names(db) == "ST101Q05")
  
  names[index, 1] <- "ST101Q05"
  names[index, 2] <- "Problem Route Selection - Just drive"
  
  db$ST104Q01 <- substr(data[, 1], start = 381, stop = 381)
  index <- which(names(db) == "ST104Q01")
  
  names[index, 1] <- "ST104Q01"
  names[index, 2] <- "Problem Ticket Machine - Similarities"
  
  db$ST104Q04 <- substr(data[, 1], start = 382, stop = 382)
  index <- which(names(db) == "ST104Q04")
  
  names[index, 1] <- "ST104Q04"
  names[index, 2] <- "Problem Ticket Machine - Try buttons"
  
  db$ST104Q05 <- substr(data[, 1], start = 383, stop = 383)
  index <- which(names(db) == "ST104Q05")
  
  names[index, 1] <- "ST104Q05"
  names[index, 2] <- "Problem Ticket Machine - Ask for help"
  
  db$ST104Q06 <- substr(data[, 1], start = 384, stop = 384)
  index <- which(names(db) == "ST104Q06")
  
  names[index, 1] <- "ST104Q06"
  names[index, 2] <- "Problem Ticket Machine - Find ticket office"
  
  db$IC01Q01 <- substr(data[, 1], start = 385, stop = 385)
  index <- which(names(db) == "IC01Q01")
  
  names[index, 1] <- "IC01Q01"
  names[index, 2] <- "At Home - Desktop Computer"
  
  db$IC01Q02 <- substr(data[, 1], start = 386, stop = 386)
  index <- which(names(db) == "IC01Q02")
  
  names[index, 1] <- "IC01Q02"
  names[index, 2] <- "At Home - Portable laptop"
  
  db$IC01Q03 <- substr(data[, 1], start = 387, stop = 387)
  index <- which(names(db) == "IC01Q03")
  
  names[index, 1] <- "IC01Q03"
  names[index, 2] <- "At Home - Tablet computerp"
  
  db$IC01Q04 <- substr(data[, 1], start = 388, stop = 388)
  index <- which(names(db) == "IC01Q04")
  
  names[index, 1] <- "IC01Q04"
  names[index, 2] <- "At Home - Internet connection"
  
  db$IC01Q05 <- substr(data[, 1], start = 389, stop = 389)
  index <- which(names(db) == "IC01Q05")
  
  names[index, 1] <- "IC01Q05"
  names[index, 2] <- "At Home - Video games console"
  
  db$IC01Q06 <- substr(data[, 1], start = 390, stop = 390)
  index <- which(names(db) == "IC01Q06")
  
  names[index, 1] <- "IC01Q06"
  names[index, 2] <- "At Home - Cell phone w/o Internet"
  
  db$IC01Q07 <- substr(data[, 1], start = 391, stop = 391)
  index <- which(names(db) == "IC01Q07")
  
  names[index, 1] <- "IC01Q07"
  names[index, 2] <- "At Home - Cell phone with Internet"
  
  db$IC01Q08 <- substr(data[, 1], start = 392, stop = 392)
  index <- which(names(db) == "IC01Q08")
  
  names[index, 1] <- "IC01Q08"
  names[index, 2] <- "At Home - Mp3/Mp4 player"
  
  db$IC01Q09 <- substr(data[, 1], start = 393, stop = 393)
  index <- which(names(db) == "IC01Q09")
  
  names[index, 1] <- "IC01Q09"
  names[index, 2] <- "At Home - Printer"
  
  db$IC01Q10 <- substr(data[, 1], start = 394, stop = 394)
  index <- which(names(db) == "IC01Q10")
  
  names[index, 1] <- "IC01Q10"
  names[index, 2] <- "At Home - USB (memory) stick"
  
  db$IC01Q11 <- substr(data[, 1], start = 395, stop = 395)
  index <- which(names(db) == "IC01Q11")
  
  names[index, 1] <- "IC01Q11"
  names[index, 2] <- "At Home - Ebook reader"
  
  db$IC02Q01 <- substr(data[, 1], start = 396, stop = 396)
  index <- which(names(db) == "IC02Q01")
  
  names[index, 1] <- "IC02Q01"
  names[index, 2] <- "At school - Desktop Computer"
  
  db$IC02Q02 <- substr(data[, 1], start = 397, stop = 397)
  index <- which(names(db) == "IC02Q02")
  
  names[index, 1] <- "IC02Q02"
  names[index, 2] <- "At school - Portable laptop"
  
  db$IC02Q03 <- substr(data[, 1], start = 398, stop = 398)
  index <- which(names(db) == "IC02Q03")
  
  names[index, 1] <- "IC02Q03"
  names[index, 2] <- "At school - Tablet computer"
  
  db$IC02Q04 <- substr(data[, 1], start = 399, stop = 399)
  index <- which(names(db) == "IC02Q04")
  
  names[index, 1] <- "IC02Q04"
  names[index, 2] <- "At school - Internet connection"
  
  db$IC02Q05 <- substr(data[, 1], start = 400, stop = 400)
  index <- which(names(db) == "IC02Q05")
  
  names[index, 1] <- "IC02Q05"
  names[index, 2] <- "At school - Printer"
  
  db$IC02Q06 <- substr(data[, 1], start = 401, stop = 401)
  index <- which(names(db) == "IC02Q06")
  
  names[index, 1] <- "IC02Q06"
  names[index, 2] <- "At school - USB (memory) stick"
  
  db$IC02Q07 <- substr(data[, 1], start = 402, stop = 402)
  index <- which(names(db) == "IC02Q07")
  
  names[index, 1] <- "IC02Q07"
  names[index, 2] <- "At school - Ebook reader"
  
  db$IC03Q01 <- substr(data[, 1], start = 403, stop = 403)
  index <- which(names(db) == "IC03Q01")
  
  names[index, 1] <- "IC03Q01"
  names[index, 2] <- "First use of computers"
  
  db$IC04Q01 <- substr(data[, 1], start = 404, stop = 404)
  index <- which(names(db) == "IC04Q01")
  
  names[index, 1] <- "IC04Q01"
  names[index, 2] <- "First access to Internet"
  
  db$IC05Q01 <- substr(data[, 1], start = 405, stop = 406)
  index <- which(names(db) == "IC05Q01")
  
  names[index, 1] <- "IC05Q01"
  names[index, 2] <- "Internet at School"
  
  db$IC06Q01 <- substr(data[, 1], start = 407, stop = 408)
  index <- which(names(db) == "IC06Q01")
  
  names[index, 1] <- "IC06Q01"
  names[index, 2] <- "Internet out-of-school - Weekday"
  
  db$IC07Q01 <- substr(data[, 1], start = 409, stop = 410)
  index <- which(names(db) == "IC07Q01")
  
  names[index, 1] <- "IC07Q01"
  names[index, 2] <- "Internet out-of-school - Weekend"
  
  db$IC08Q01 <- substr(data[, 1], start = 411, stop = 411)
  index <- which(names(db) == "IC08Q01")
  
  names[index, 1] <- "IC08Q01"
  names[index, 2] <- "Out-of-school 8 - One player games"
  
  db$IC08Q02 <- substr(data[, 1], start = 412, stop = 412)
  index <- which(names(db) == "IC08Q02")
  
  names[index, 1] <- "IC08Q02"
  names[index, 2] <- "Out-of-school 8 - ColLabourative games"
  
  db$IC08Q03 <- substr(data[, 1], start = 413, stop = 413)
  index <- which(names(db) == "IC08Q03")
  
  names[index, 1] <- "IC08Q03"
  names[index, 2] <- "Out-of-school 8 - Use email"
  
  db$IC08Q04 <- substr(data[, 1], start = 414, stop = 414)
  index <- which(names(db) == "IC08Q04")
  
  names[index, 1] <- "IC08Q04"
  names[index, 2] <- "Out-of-school 8 - Chat on line"
  
  db$IC08Q05 <- substr(data[, 1], start = 415, stop = 415)
  index <- which(names(db) == "IC08Q05")
  
  names[index, 1] <- "IC08Q05"
  names[index, 2] <- "Out-of-school 8 - Social networks"
  
  db$IC08Q06 <- substr(data[, 1], start = 416, stop = 416)
  index <- which(names(db) == "IC08Q06")
  
  names[index, 1] <- "IC08Q06"
  names[index, 2] <- "Out-of-school 8 - Browse the Internet for fun"
  
  db$IC08Q07 <- substr(data[, 1], start = 417, stop = 417)
  index <- which(names(db) == "IC08Q07")
  
  names[index, 1] <- "IC08Q07"
  names[index, 2] <- "Out-of-school 8 - Read news"
  
  db$IC08Q08 <- substr(data[, 1], start = 418, stop = 418)
  index <- which(names(db) == "IC08Q08")
  
  names[index, 1] <- "IC08Q08"
  names[index, 2] <- "Out-of-school 8 - Obtain practical information from the Internet"
  
  db$IC08Q09 <- substr(data[, 1], start = 419, stop = 419)
  index <- which(names(db) == "IC08Q09")
  
  names[index, 1] <- "IC08Q09"
  names[index, 2] <- "Out-of-school 8 - Download music"
  
  db$IC08Q11 <- substr(data[, 1], start = 420, stop = 420)
  index <- which(names(db) == "IC08Q11")
  
  names[index, 1] <- "IC08Q11"
  names[index, 2] <- "Out-of-school 8 - Upload content"
  
  db$IC09Q01 <- substr(data[, 1], start = 421, stop = 421)
  index <- which(names(db) == "IC09Q01")
  
  names[index, 1] <- "IC09Q01"
  names[index, 2] <- "Out-of-school 9 - Internet for school"
  
  db$IC09Q02 <- substr(data[, 1], start = 422, stop = 422)
  index <- which(names(db) == "IC09Q02")
  
  names[index, 1] <- "IC09Q02"
  names[index, 2] <- "Out-of-school 9 - Email students"
  
  db$IC09Q03 <- substr(data[, 1], start = 423, stop = 423)
  index <- which(names(db) == "IC09Q03")
  
  names[index, 1] <- "IC09Q03"
  names[index, 2] <- "Out-of-school 9 - Email teachers"
  
  db$IC09Q04 <- substr(data[, 1], start = 424, stop = 424)
  index <- which(names(db) == "IC09Q04")
  
  names[index, 1] <- "IC09Q04"
  names[index, 2] <- "Out-of-school 9 - Download from School"
  
  db$IC09Q05 <- substr(data[, 1], start = 425, stop = 425)
  index <- which(names(db) == "IC09Q05")
  
  names[index, 1] <- "IC09Q05"
  names[index, 2] <- "Out-of-school 9 - Announcements"
  
  db$IC09Q06 <- substr(data[, 1], start = 426, stop = 426)
  index <- which(names(db) == "IC09Q06")
  
  names[index, 1] <- "IC09Q06"
  names[index, 2] <- "Out-of-school 9 - Homework"
  
  db$IC09Q07 <- substr(data[, 1], start = 427, stop = 427)
  index <- which(names(db) == "IC09Q07")
  
  names[index, 1] <- "IC09Q07"
  names[index, 2] <- "Out-of-school 9 - Share school material"
  
  db$IC10Q01 <- substr(data[, 1], start = 428, stop = 428)
  index <- which(names(db) == "IC10Q01")
  
  names[index, 1] <- "IC10Q01"
  names[index, 2] <- "At School - Chat on line"
  
  db$IC10Q02 <- substr(data[, 1], start = 428, stop = 428)
  index <- which(names(db) == "IC10Q02")
  
  names[index, 1] <- "IC10Q02"
  names[index, 2] <- "At School - Email"
  
  db$IC10Q02 <- substr(data[, 1], start = 429, stop = 429)
  index <- which(names(db) == "IC10Q02")
  
  names[index, 1] <- "IC10Q02"
  names[index, 2] <- "At School - Email"
  
  db$IC10Q03 <- substr(data[, 1], start = 430, stop = 430)
  index <- which(names(db) == "IC10Q03")
  
  names[index, 1] <- "IC10Q03"
  names[index, 2] <- "At School - Browse for schoolwork"
  
  db$IC10Q04 <- substr(data[, 1], start = 431, stop = 431)
  index <- which(names(db) == "IC10Q04")
  
  names[index, 1] <- "IC10Q04"
  names[index, 2] <- "At School - Download from website"
  
  db$IC10Q05 <- substr(data[, 1], start = 432, stop = 432)
  index <- which(names(db) == "IC10Q05")
  
  names[index, 1] <- "IC10Q05"
  names[index, 2] <- "At School - Post on website"
  
  db$IC10Q06 <- substr(data[, 1], start = 433, stop = 433)
  index <- which(names(db) == "IC10Q06")
  
  names[index, 1] <- "IC10Q06"
  names[index, 2] <- "At School - Simulations"
  
  db$IC10Q07 <- substr(data[, 1], start = 434, stop = 434)
  index <- which(names(db) == "IC10Q07")
  
  names[index, 1] <- "IC10Q07"
  names[index, 2] <- "At School - Practice and drilling"
  
  db$IC10Q08 <- substr(data[, 1], start = 435, stop = 435)
  index <- which(names(db) == "IC10Q08")
  
  names[index, 1] <- "IC10Q08"
  names[index, 2] <- "At School - Homework"
  
  db$IC10Q09 <- substr(data[, 1], start = 436, stop = 436)
  index <- which(names(db) == "IC10Q09")
  
  names[index, 1] <- "IC10Q09"
  names[index, 2] <- "At School - Group work"
  
  db$IC11Q01 <- substr(data[, 1], start = 437, stop = 437)
  index <- which(names(db) == "IC11Q01")
  
  names[index, 1] <- "IC11Q01"
  names[index, 2] <- "Maths lessons - Draw graph"
  
  db$IC11Q02 <- substr(data[, 1], start = 438, stop = 438)
  index <- which(names(db) == "IC11Q02")
  
  names[index, 1] <- "IC11Q02"
  names[index, 2] <- "Maths lessons - Calculation with numbers"
  
  db$IC11Q03 <- substr(data[, 1], start = 439, stop = 439)
  index <- which(names(db) == "IC11Q03")
  
  names[index, 1] <- "IC11Q03"
  names[index, 2] <- "Maths lessons - Geometric figures"
  
  db$IC11Q04 <- substr(data[, 1], start = 440, stop = 440)
  index <- which(names(db) == "IC11Q04")
  
  names[index, 1] <- "IC11Q04"
  names[index, 2] <- "Maths lessons - Spreadsheet"
  
  db$IC11Q05 <- substr(data[, 1], start = 441, stop = 441)
  index <- which(names(db) == "IC11Q05")
  
  names[index, 1] <- "IC11Q05"
  names[index, 2] <- "Maths lessons - Algebra"
  
  db$IC11Q06 <- substr(data[, 1], start = 442, stop = 442)
  index <- which(names(db) == "IC11Q06")
  
  names[index, 1] <- "IC11Q06"
  names[index, 2] <- "Maths lessons - Histograms"
  
  db$IC11Q07 <- substr(data[, 1], start = 443, stop = 443)
  index <- which(names(db) == "IC11Q07")
  
  names[index, 1] <- "IC11Q07"
  names[index, 2] <- "Maths lessons - Change in graphs"
  
  db$IC22Q01 <- substr(data[, 1], start = 444, stop = 444)
  index <- which(names(db) == "IC22Q01")
  
  names[index, 1] <- "IC22Q01"
  names[index, 2] <- "Attitudes - Useful for schoolwork"
  
  db$IC22Q02 <- substr(data[, 1], start = 445, stop = 445)
  index <- which(names(db) == "IC22Q02")
  
  names[index, 1] <- "IC22Q02"
  names[index, 2] <- "Attitudes - Homework more fun"
  
  db$IC22Q04 <- substr(data[, 1], start = 446, stop = 446)
  index <- which(names(db) == "IC22Q04")
  
  names[index, 1] <- "IC22Q04"
  names[index, 2] <- "Attitudes - Source of information"
  
  db$IC22Q06 <- substr(data[, 1], start = 447, stop = 447)
  index <- which(names(db) == "IC22Q06")
  
  names[index, 1] <- "IC22Q06"
  names[index, 2] <- "Attitudes - Troublesome"
  
  db$IC22Q07 <- substr(data[, 1], start = 448, stop = 448)
  index <- which(names(db) == "IC22Q07")
  
  names[index, 1] <- "IC22Q07"
  names[index, 2] <- "Attitudes - Not suitable for schoolwork"
  
  db$IC22Q08 <- substr(data[, 1], start = 449, stop = 449)
  index <- which(names(db) == "IC22Q08")
  
  names[index, 1] <- "IC22Q08"
  names[index, 2] <- "Attitudes - Too unreliable"
  
  db$EC01Q01 <- substr(data[, 1], start = 450, stop = 450)
  index <- which(names(db) == "EC01Q01")
  
  names[index, 1] <- "EC01Q01"
  names[index, 2] <- "Miss 2 months of <ISCED 1>"
  
  db$EC02Q01 <- substr(data[, 1], start = 451, stop = 451)
  index <- which(names(db) == "EC02Q01")
  
  names[index, 1] <- "EC02Q01"
  names[index, 2] <- "Miss 2 months of <ISCED 2>"
  
  db$EC03Q01 <- substr(data[, 1], start = 452, stop = 452)
  index <- which(names(db) == "EC03Q01")
  
  names[index, 1] <- "EC03Q01"
  names[index, 2] <- "Future Orientation - Internship"
  
  db$EC03Q02 <- substr(data[, 1], start = 453, stop = 453)
  index <- which(names(db) == "EC03Q02")
  
  names[index, 1] <- "EC03Q02"
  names[index, 2] <- "Future Orientation - Work-site visits"
  
  db$EC03Q03 <- substr(data[, 1], start = 454, stop = 454)
  index <- which(names(db) == "EC03Q03")
  
  names[index, 1] <- "EC03Q03"
  names[index, 2] <- "Future Orientation - Job fair"
  
  db$EC03Q04 <- substr(data[, 1], start = 455, stop = 455)
  index <- which(names(db) == "EC03Q04")
  
  names[index, 1] <- "EC03Q04"
  names[index, 2] <- "Future Orientation - Career advisor at school"
  
  db$EC03Q05 <- substr(data[, 1], start = 456, stop = 456)
  index <- which(names(db) == "EC03Q05")
  
  names[index, 1] <- "EC03Q05"
  names[index, 2] <- "Future Orientation - Career advisor outside school"
  
  db$EC03Q06 <- substr(data[, 1], start = 457, stop = 457)
  index <- which(names(db) == "EC03Q06")
  
  names[index, 1] <- "EC03Q06"
  names[index, 2] <- "Future Orientation - Questionnaire"
  
  db$EC03Q07 <- substr(data[, 1], start = 458, stop = 458)
  index <- which(names(db) == "EC03Q07")
  
  names[index, 1] <- "EC03Q07"
  names[index, 2] <- "Future Orientation - Internet search"
  
  db$EC03Q08 <- substr(data[, 1], start = 459, stop = 459)
  index <- which(names(db) == "EC03Q08")
  
  names[index, 1] <- "EC03Q08"
  names[index, 2] <- "Future Orientation - Tour<ISCED 3-5> institution"
  
  db$EC03Q09 <- substr(data[, 1], start = 460, stop = 460)
  index <- which(names(db) == "EC03Q09")
  
  names[index, 1] <- "EC03Q09"
  names[index, 2] <- "Future Orientation - web search <ISCED 3-5> prog"
  
  db$EC03Q10 <- substr(data[, 1], start = 461, stop = 461)
  index <- which(names(db) == "EC03Q10")
  
  names[index, 1] <- "EC03Q10"
  names[index, 2] <- "Future Orientation - <country specific item>"
  
  db$EC04Q01A <- substr(data[, 1], start = 462, stop = 462)
  index <- which(names(db) == "EC04Q01A")
  
  names[index, 1] <- "EC04Q01A"
  names[index, 2] <- "Acquired skills - Find job info - Yes, at school"
  
  db$EC04Q01B <- substr(data[, 1], start = 463, stop = 463)
  index <- which(names(db) == "EC04Q01B")
  
  names[index, 1] <- "EC04Q01B"
  names[index, 2] <- "Acquired skills - Find job info - Yes, out of school"
  
  db$EC04Q01C <- substr(data[, 1], start = 464, stop = 464)
  index <- which(names(db) == "EC04Q01C")
  
  names[index, 1] <- "EC04Q01C"
  names[index, 2] <- "Acquired skills - Find job info - No, never"
  
  db$EC04Q02A <- substr(data[, 1], start = 465, stop = 465)
  index <- which(names(db) == "EC04Q02A")
  
  names[index, 1] <- "EC04Q02A"
  names[index, 2] <- "Acquired skills - Search for job - Yes, at school"
  
  db$EC04Q02B <- substr(data[, 1], start = 466, stop = 466)
  index <- which(names(db) == "EC04Q02B")
  
  names[index, 1] <- "EC04Q02B"
  names[index, 2] <- "Acquired skills - Search for job - Yes, out of school"
  
  db$EC04Q02C <- substr(data[, 1], start = 467, stop = 467)
  index <- which(names(db) == "EC04Q02C")
  
  names[index, 1] <- "EC04Q02C"
  names[index, 2] <- "Acquired skills - Search for job - No, never"
  
  db$EC04Q03A <- substr(data[, 1], start = 468, stop = 468)
  index <- which(names(db) == "EC04Q03A")
  
  names[index, 1] <- "EC04Q03A"
  names[index, 2] <- "Acquired skills - Write resume - Yes, at school"
  
  db$EC04Q03B <- substr(data[, 1], start = 469, stop = 469)
  index <- which(names(db) == "EC04Q03B")
  
  names[index, 1] <- "EC04Q03B"
  names[index, 2] <- "Acquired skills - Write resume - Yes, out of school"
  
  db$EC04Q03C <- substr(data[, 1], start = 470, stop = 470)
  index <- which(names(db) == "EC04Q03C")
  
  names[index, 1] <- "EC04Q03C"
  names[index, 2] <- "Acquired skills - Write resume - No, never"
  
  db$EC04Q04A <- substr(data[, 1], start = 471, stop = 471)
  index <- which(names(db) == "EC04Q04A")
  
  names[index, 1] <- "EC04Q04A"
  names[index, 2] <- "Acquired skills - Job interview - Yes, at school"
  
  db$EC04Q04B <- substr(data[, 1], start = 472, stop = 472)
  index <- which(names(db) == "EC04Q04B")
  
  names[index, 1] <- "EC04Q04B"
  names[index, 2] <- "Acquired skills - Job interview - Yes, out of school"
  
  db$EC04Q04C <- substr(data[, 1], start = 473, stop = 473)
  index <- which(names(db) == "EC04Q04C")
  
  names[index, 1] <- "EC04Q04C"
  names[index, 2] <- "Acquired skills - Job interview - No, never"
  
  db$EC04Q05A <- substr(data[, 1], start = 474, stop = 474)
  index <- which(names(db) == "EC04Q05A")
  
  names[index, 1] <- "EC04Q05A"
  names[index, 2] <- "Acquired skills - ISCED 3-5 programs - Yes, at school"
  
  db$EC04Q05B <- substr(data[, 1], start = 475, stop = 475)
  index <- which(names(db) == "EC04Q05B")
  
  names[index, 1] <- "EC04Q05B"
  names[index, 2] <- "Acquired skills - ISCED 3-5 programs - Yes, out of school"
  
  db$EC04Q05C <- substr(data[, 1], start = 476, stop = 476)
  index <- which(names(db) == "EC04Q05C")
  
  names[index, 1] <- "EC04Q05C"
  names[index, 2] <- "Acquired skills - ISCED 3-5 programs - No, never"
  
  db$EC04Q06A <- substr(data[, 1], start = 477, stop = 477)
  index <- which(names(db) == "EC04Q06A")
  
  names[index, 1] <- "EC04Q06A"
  names[index, 2] <- "Acquired skills - Student financing - Yes, at school"
  
  db$EC04Q06B <- substr(data[, 1], start = 478, stop = 478)
  index <- which(names(db) == "EC04Q06B")
  
  names[index, 1] <- "EC04Q06B"
  names[index, 2] <- "Acquired skills - Student financing - Yes, out of school"
  
  db$EC04Q06C <- substr(data[, 1], start = 479, stop = 479)
  index <- which(names(db) == "EC04Q06C")
  
  names[index, 1] <- "EC04Q06C"
  names[index, 2] <- "Acquired skills - Student financing - No, never"
  
  db$EC05Q01 <- substr(data[, 1], start = 480, stop = 480)
  index <- which(names(db) == "EC05Q01")
  
  names[index, 1] <- "EC05Q01"
  names[index, 2] <- "First language learned"
  
  db$EC06Q01 <- substr(data[, 1], start = 481, stop = 481)
  index <- which(names(db) == "EC06Q01")
  
  names[index, 1] <- "EC06Q01"
  names[index, 2] <- "Age started learning <test language>"
  
  db$EC07Q01 <- substr(data[, 1], start = 482, stop = 482)
  index <- which(names(db) == "EC07Q01")
  
  names[index, 1] <- "EC07Q01"
  names[index, 2] <- "Language spoken - Mother"
  
  db$EC07Q02 <- substr(data[, 1], start = 483, stop = 483)
  index <- which(names(db) == "EC07Q02")
  
  names[index, 1] <- "EC07Q02"
  names[index, 2] <- "Language spoken - Father"
  
  db$EC07Q03 <- substr(data[, 1], start = 484, stop = 484)
  index <- which(names(db) == "EC07Q03")
  
  names[index, 1] <- "EC07Q03"
  names[index, 2] <- "Language spoken - Siblings"
  
  db$EC07Q04 <- substr(data[, 1], start = 485, stop = 485)
  index <- which(names(db) == "EC07Q04")
  
  names[index, 1] <- "EC07Q04"
  names[index, 2] <- "Language spoken - Best friend"
  
  db$EC07Q05 <- substr(data[, 1], start = 486, stop = 486)
  index <- which(names(db) == "EC07Q05")
  
  names[index, 1] <- "EC07Q05"
  names[index, 2] <- "Language spoken - Schoolmates"
  
  db$EC08Q01 <- substr(data[, 1], start = 487, stop = 487)
  index <- which(names(db) == "EC08Q01")
  
  names[index, 1] <- "EC08Q01"
  names[index, 2] <- "Activities language - Reading"
  
  db$EC08Q02 <- substr(data[, 1], start = 488, stop = 488)
  index <- which(names(db) == "EC08Q02")
  
  names[index, 1] <- "EC08Q02"
  names[index, 2] <- "Activities language - Watching TV"
  
  db$EC08Q03 <- substr(data[, 1], start = 489, stop = 489)
  index <- which(names(db) == "EC08Q03")
  
  names[index, 1] <- "EC08Q03"
  names[index, 2] <- "Activities language - Internet surfing"
  
  db$EC08Q04 <- substr(data[, 1], start = 490, stop = 490)
  index <- which(names(db) == "EC08Q04")
  
  names[index, 1] <- "EC08Q04"
  names[index, 2] <- "Activities language - Writing emails"
  
  db$EC09Q03 <- substr(data[, 1], start = 491, stop = 491)
  index <- which(names(db) == "EC09Q03")
  
  names[index, 1] <- "EC09Q03"
  names[index, 2] <- "Types of support <test language> - remedial lessons"
  
  db$EC10Q01 <- substr(data[, 1], start = 492, stop = 492)
  index <- which(names(db) == "EC10Q01")
  
  names[index, 1] <- "EC10Q01"
  names[index, 2] <- "Amount of support <test language>"
  
  db$EC11Q02 <- substr(data[, 1], start = 493, stop = 493)
  index <- which(names(db) == "EC11Q02")
  
  names[index, 1] <- "EC11Q02"
  names[index, 2] <- "Attend lessons <heritage language> - focused"
  
  db$EC11Q03 <- substr(data[, 1], start = 494, stop = 494)
  index <- which(names(db) == "EC11Q03")
  
  names[index, 1] <- "EC11Q03"
  names[index, 2] <- "Attend lessons <heritage language> - school subjects"
  
  db$EC12Q01 <- substr(data[, 1], start = 495, stop = 495)
  index <- which(names(db) == "EC12Q01")
  
  names[index, 1] <- "EC12Q01"
  names[index, 2] <- "Instruction in <heritage language>"
  
  db$ST22Q01 <- substr(data[, 1], start = 496, stop = 496)
  index <- which(names(db) == "ST22Q01")
  
  names[index, 1] <- "ST22Q01"
  names[index, 2] <- "Acculturation - Mother Immigrant (Filter)"
  
  db$ST23Q01 <- substr(data[, 1], start = 497, stop = 497)
  index <- which(names(db) == "ST23Q01")
  
  names[index, 1] <- "ST23Q01"
  names[index, 2] <- "Acculturation - Enjoy <Host Culture> Friends"
  
  db$ST23Q02 <- substr(data[, 1], start = 498, stop = 498)
  index <- which(names(db) == "ST23Q02")
  
  names[index, 1] <- "ST23Q02"
  names[index, 2] <- "Acculturation - Enjoy <Heritage Culture> Friends"
  
  db$ST23Q03 <- substr(data[, 1], start = 499, stop = 499)
  index <- which(names(db) == "ST23Q03")
  
  names[index, 1] <- "ST23Q03"
  names[index, 2] <- "Acculturation - Enjoy <Host Culture> Celebrations"
  
  db$ST23Q04 <- substr(data[, 1], start = 500, stop = 500)
  index <- which(names(db) == "ST23Q04")
  
  names[index, 1] <- "ST23Q04"
  names[index, 2] <- "Acculturation - Enjoy <Heritage Culture> Celebrations"
  
  db$ST23Q05 <- substr(data[, 1], start = 501, stop = 501)
  index <- which(names(db) == "ST23Q05")
  
  names[index, 1] <- "ST23Q05"
  names[index, 2] <- "Acculturation - Spend Time with <Host Culture> Friends"
  
  db$ST23Q06 <- substr(data[, 1], start = 502, stop = 502)
  index <- which(names(db) == "ST23Q06")
  
  names[index, 1] <- "ST23Q06"
  names[index, 2] <- "Acculturation - Spend Time with <Heritage Culture> Friends"
  
  db$ST23Q07 <- substr(data[, 1], start = 503, stop = 503)
  index <- which(names(db) == "ST23Q07")
  
  names[index, 1] <- "ST23Q07"
  names[index, 2] <- "Acculturation - Participate in <Host Culture> Celebrations"
  
  db$ST23Q08 <- substr(data[, 1], start = 504, stop = 504)
  index <- which(names(db) == "ST23Q08")
  
  names[index, 1] <- "ST23Q08"
  names[index, 2] <- "Acculturation - Participate in <Heritage Culture> Celebrations"
  
  db$ST24Q01 <- substr(data[, 1], start = 505, stop = 505)
  index <- which(names(db) == "ST24Q01")
  
  names[index, 1] <- "ST24Q01"
  names[index, 2] <- "Acculturation - Perceived Host-Heritage Cultural Differences - Values"
  
  db$ST24Q02 <- substr(data[, 1], start = 506, stop = 506)
  index <- which(names(db) == "ST24Q02")
  
  names[index, 1] <- "ST24Q02"
  names[index, 2] <- "Acculturation - Perceived Host-Heritage Cultural Differences - Mother Treatment"
  
  db$ST24Q03 <- substr(data[, 1], start = 507, stop = 507)
  index <- which(names(db) == "ST24Q03")
  
  names[index, 1] <- "ST24Q03"
  names[index, 2] <- "Acculturation - Perceived Host-Heritage Cultural Differences - Teacher Treatment"
  
  db$CLCUSE1 <- substr(data[, 1], start = 508, stop = 508)
  index <- which(names(db) == "CLCUSE1")
  
  names[index, 1] <- "CLCUSE1"
  names[index, 2] <- "Calculator Use"
  
  db$CLCUSE301 <- substr(data[, 1], start = 509, stop = 510)
  index <- which(names(db) == "CLCUSE301")
  
  names[index, 1] <- "CLCUSE301"
  names[index, 2] <- "Effort-real 1"
  
  db$CLCUSE302 <- substr(data[, 1], start = 511, stop = 512)
  index <- which(names(db) == "CLCUSE302")
  
  names[index, 1] <- "CLCUSE302"
  names[index, 2] <- "Effort-real 2"
  
  db$DEFFORT <- substr(data[, 1], start = 513, stop = 514)
  index <- which(names(db) == "DEFFORT")
  
  names[index, 1] <- "DEFFORT"
  names[index, 2] <- "Difference in Effort"
  
  db$QUESTID <- substr(data[, 1], start = 515, stop = 515)
  index <- which(names(db) == "QUESTID")
  
  names[index, 1] <- "QUESTID"
  names[index, 2] <- "Student Questionnaire Form"
  
  db$BOOKID <- substr(data[, 1], start = 516, stop = 517)
  index <- which(names(db) == "BOOKID")
  
  names[index, 1] <- "BOOKID"
  names[index, 2] <- "Booklet ID"
  
  db$EASY <- substr(data[, 1], start = 518, stop = 518)
  index <- which(names(db) == "EASY")
  
  names[index, 1] <- "EASY"
  names[index, 2] <- "Standard or simplified set of booklets"
  
  db$AGE <- substr(data[, 1], start = 519, stop = 526)
  index <- which(names(db) == "AGE")
  
  names[index, 1] <- "AGE"
  names[index, 2] <- "Standard or simplified set of booklets"
  
  db$GRADE <- substr(data[, 1], start = 527, stop = 528)
  index <- which(names(db) == "GRADE")
  
  names[index, 1] <- "GRADE"
  names[index, 2] <- "Grade compared to modal grade in country"
  
  db$PROGN <- substr(data[, 1], start = 529, stop = 536)
  index <- which(names(db) == "PROGN")
  
  names[index, 1] <- "PROGN"
  names[index, 2] <- "Unique national study programme code"
  
  db$ANXMAT <- substr(data[, 1], start = 537, stop = 544)
  index <- which(names(db) == "ANXMAT")
  
  names[index, 1] <- "ANXMAT"
  names[index, 2] <- "Mathematics Anxiety"
  
  db$ATSCHL <- substr(data[, 1], start = 545, stop = 552)
  index <- which(names(db) == "ATSCHL")
  
  names[index, 1] <- "ATSCHL"
  names[index, 2] <- "Attitude towards School: Learning Outcomes"
  
  db$ATTLNACT <- substr(data[, 1], start = 553, stop = 561)
  index <- which(names(db) == "ATTLNACT")
  
  names[index, 1] <- "ATTLNACT"
  names[index, 2] <- "Attitude towards School: Learning Activities"
  
  db$BELONG <- substr(data[, 1], start = 562, stop = 569)
  index <- which(names(db) == "BELONG")
  
  names[index, 1] <- "BELONG"
  names[index, 2] <- "Sense of Belonging to School"
  
  db$BFMJ2 <- substr(data[, 1], start = 570, stop = 574)
  index <- which(names(db) == "BFMJ2")
  
  names[index, 1] <- "BFMJ2"
  names[index, 2] <- "Father SQ ISEI"
  
  db$BMMJ1 <- substr(data[, 1], start = 575, stop = 579)
  index <- which(names(db) == "BMMJ1")
  
  names[index, 1] <- "BMMJ1"
  names[index, 2] <- "Mother SQ ISEI"
  
  db$CLSMAN <- substr(data[, 1], start = 580, stop = 588)
  index <- which(names(db) == "CLSMAN")
  
  names[index, 1] <- "CLSMAN"
  names[index, 2] <- "Mathematics Teacher's Classroom Management"
  
  db$COBN_F <- substr(data[, 1], start = 589, stop = 594)
  index <- which(names(db) == "COBN_F")
  
  names[index, 1] <- "COBN_F"
  names[index, 2] <- "Country of Birth National Categories-Father"
  
  db$COBN_M <- substr(data[, 1], start = 595, stop = 600)
  index <- which(names(db) == "COBN_M")
  
  names[index, 1] <- "COBN_M"
  names[index, 2] <- "Country of Birth National Categories-Father"
  
  db$COBN_S <- substr(data[, 1], start = 601, stop = 606)
  index <- which(names(db) == "COBN_S")
  
  names[index, 1] <- "COBN_S"
  names[index, 2] <- "Country of Birth National Categories- Self"
  
  db$COGACT <- substr(data[, 1], start = 607, stop = 615)
  index <- which(names(db) == "COGACT")
  
  names[index, 1] <- "COGACT"
  names[index, 2] <- "Cognitive Activation in Mathematics Lessons"
  
  db$CULTDIST <- substr(data[, 1], start = 616, stop = 624)
  index <- which(names(db) == "CULTDIST")
  
  names[index, 1] <- "CULTDIST"
  names[index, 2] <- "Cultural Distance between Host and Heritage Culture"
  
  db$CULTPOS <- substr(data[, 1], start = 625, stop = 632)
  index <- which(names(db) == "CULTPOS")
  
  names[index, 1] <- "CULTPOS"
  names[index, 2] <- "Cultural Possessions"
  
  db$DISCLIMA <- substr(data[, 1], start = 633, stop = 640)
  index <- which(names(db) == "DISCLIMA")
  
  names[index, 1] <- "DISCLIMA"
  names[index, 2] <- "Disciplinary Climate"
  
  db$ENTUSE <- substr(data[, 1], start = 641, stop = 649)
  index <- which(names(db) == "ENTUSE")
  
  names[index, 1] <- "ENTUSE"
  names[index, 2] <- "ICT Entertainment Use"
  
  db$ESCS <- substr(data[, 1], start = 650, stop = 657)
  index <- which(names(db) == "ESCS")
  
  names[index, 1] <- "ESCS"
  names[index, 2] <- "Index of economic, social and cultural status"
  
  db$EXAPPLM <- substr(data[, 1], start = 658, stop = 666)
  index <- which(names(db) == "EXAPPLM")
  
  names[index, 1] <- "EXAPPLM"
  names[index, 2] <- "Experience with Applied Mathematics Tasks at School"
  
  db$EXPUREM <- substr(data[, 1], start = 667, stop = 675)
  index <- which(names(db) == "EXPUREM")
  
  names[index, 1] <- "EXPUREM"
  names[index, 2] <- "Experience with Pure Mathematics Tasks at School"
  
  db$FAILMAT <- substr(data[, 1], start = 676, stop = 684)
  index <- which(names(db) == "FAILMAT")
  
  names[index, 1] <- "FAILMAT"
  names[index, 2] <- "Attributions to Failure in Mathematics"
  
  db$FAMCON <- substr(data[, 1], start = 685, stop = 693)
  index <- which(names(db) == "FAMCON")
  
  names[index, 1] <- "FAMCON"
  names[index, 2] <- "Familiarity with Mathematical Concepts"
  
  db$FAMCONC <- substr(data[, 1], start = 694, stop = 701)
  index <- which(names(db) == "FAMCONC")
  
  names[index, 1] <- "FAMCONC"
  names[index, 2] <- "Familiarity with Mathematical Concepts (Signal Detection Adjusted)"
  
  
  db$FAMSTRUC <- substr(data[, 1], start = 702, stop = 702)
  index <- which(names(db) == "FAMSTRUC")
  
  names[index, 1] <- "FAMSTRUC"
  names[index, 2] <- "Family Structure"
  
  db$FISCED <- substr(data[, 1], start = 703, stop = 703)
  index <- which(names(db) == "FISCED")
  
  names[index, 1] <- "FISCED"
  names[index, 2] <- "Educational level of father (ISCED)"
  
  db$HEDRES <- substr(data[, 1], start = 704, stop = 711)
  index <- which(names(db) == "HEDRES")
  
  names[index, 1] <- "HEDRES"
  names[index, 2] <- "Home educational resources"
  
  db$HERITCUL <- substr(data[, 1], start = 712, stop = 720)
  index <- which(names(db) == "HERITCUL")
  
  names[index, 1] <- "HERITCUL"
  names[index, 2] <- "Acculturation: Heritage Culture Oriented Strategies"
  
  db$HISCED <- substr(data[, 1], start = 721, stop = 721)
  index <- which(names(db) == "HISCED")
  
  names[index, 1] <- "HISCED"
  names[index, 2] <- "Highest educational level of parents"
  
  db$HISEI <- substr(data[, 1], start = 722, stop = 729)
  index <- which(names(db) == "HISEI")
  
  names[index, 1] <- "HISEI"
  names[index, 2] <- "Highest parental occupational status"
  
  db$HOMEPOS <- substr(data[, 1], start = 730, stop = 737)
  index <- which(names(db) == "HOMEPOS")
  
  names[index, 1] <- "HOMEPOS"
  names[index, 2] <- "Home Possessions"
  
  db$HOMSCH <- substr(data[, 1], start = 738, stop = 746)
  index <- which(names(db) == "HOMSCH")
  
  names[index, 1] <- "HOMSCH"
  names[index, 2] <- "ICT Use at Home for School-related Tasks"
  
  db$HOSTCUL <- substr(data[, 1], start = 747, stop = 755)
  index <- which(names(db) == "HOSTCUL")
  
  names[index, 1] <- "HOSTCUL"
  names[index, 2] <- "Acculturation: Host Culture Oriented Strategies"
  
  db$ICTATTNEG <- substr(data[, 1], start = 756, stop = 764)
  index <- which(names(db) == "ICTATTNEG")
  
  names[index, 1] <- "ICTATTNEG"
  names[index, 2] <- "Attitudes Towards Computers: Limitations of the Computer as a Tool for School Learning"
  
  db$ICTATTPOS <- substr(data[, 1], start = 765, stop = 773)
  index <- which(names(db) == "ICTATTPOS")
  
  names[index, 1] <- "ICTATTPOS"
  names[index, 2] <- "Attitudes Towards Computers: Computer as a Tool for School Learning"
  
  db$ICTHOME <- substr(data[, 1], start = 774, stop = 782)
  index <- which(names(db) == "ICTHOME")
  
  names[index, 1] <- "ICTHOME"
  names[index, 2] <- "ICT Availability at Home"
  
  db$ICTSCH <- substr(data[, 1], start = 783, stop = 791)
  index <- which(names(db) == "ICTSCH")
  
  names[index, 1] <- "ICTSCH"
  names[index, 2] <- "ICT Availability at School"
  
  db$IMMIG <- substr(data[, 1], start = 792, stop = 792)
  index <- which(names(db) == "IMMIG")
  
  names[index, 1] <- "IMMIG"
  names[index, 2] <- "Immigration status"
  
  db$INFOCAR <- substr(data[, 1], start = 793, stop = 801)
  index <- which(names(db) == "INFOCAR")
  
  names[index, 1] <- "INFOCAR"
  names[index, 2] <- "Information about Careers"
  
  db$INFOCAR <- substr(data[, 1], start = 793, stop = 801)
  index <- which(names(db) == "INFOCAR")
  
  names[index, 1] <- "INFOCAR"
  names[index, 2] <- "Information about Careers"
  
  db$INFOJOB1 <- substr(data[, 1], start = 802, stop = 810)
  index <- which(names(db) == "INFOJOB1")
  
  names[index, 1] <- "INFOJOB1"
  names[index, 2] <- "Information about the Labour Market provided by the School"
  
  db$INFOJOB2 <- substr(data[, 1], start = 811, stop = 819)
  index <- which(names(db) == "INFOJOB2")
  
  names[index, 1] <- "INFOJOB2"
  names[index, 2] <- "Information about the Labour Market provided outside of School"
  
  db$INSTMOT <- substr(data[, 1], start = 820, stop = 827)
  index <- which(names(db) == "INSTMOT")
  
  names[index, 1] <- "INSTMOT"
  names[index, 2] <- "Instrumental Motivation for Mathematics"
  
  db$INTMAT <- substr(data[, 1], start = 828, stop = 835)
  index <- which(names(db) == "INTMAT")
  
  names[index, 1] <- "INTMAT"
  names[index, 2] <- "Mathematics Interest"
  
  db$ISCEDD <- substr(data[, 1], start = 836, stop = 836)
  index <- which(names(db) == "ISCEDD")
  
  names[index, 1] <- "ISCEDD"
  names[index, 2] <- "ISCED designation"
  
  db$ISCEDL <- substr(data[, 1], start = 837, stop = 837)
  index <- which(names(db) == "ISCEDL")
  
  names[index, 1] <- "ISCEDL"
  names[index, 2] <- "ISCED level"
  
  db$ISCEDO <- substr(data[, 1], start = 838, stop = 838)
  index <- which(names(db) == "ISCEDO")
  
  names[index, 1] <- "ISCEDO"
  names[index, 2] <- "ISCED orientation"
  
  db$LANGCOMM <- substr(data[, 1], start = 839, stop = 839)
  index <- which(names(db) == "LANGCOMM")
  
  names[index, 1] <- "LANGCOMM"
  names[index, 2] <- "Preference for Heritage Language in Conversations with Family and Friends"
  
  db$LANGN <- substr(data[, 1], start = 840, stop = 842)
  index <- which(names(db) == "LANGN")
  
  names[index, 1] <- "LANGN"
  names[index, 2] <- "Language at home (3-digit code)"
  
  db$LANGRPPD <- substr(data[, 1], start = 843, stop = 843)
  index <- which(names(db) == "LANGRPPD")
  
  names[index, 1] <- "LANGRPPD"
  names[index, 2] <- "Preference for Heritage Language in Language Reception and Production"
  
  db$LMINS <- substr(data[, 1], start = 844, stop = 847)
  index <- which(names(db) == "LMINS")
  
  names[index, 1] <- "LMINS"
  names[index, 2] <- "Learning time (minutes per week) - <test language>"
  
  db$MATBEH <- substr(data[, 1], start = 848, stop = 856)
  index <- which(names(db) == "MATBEH")
  
  names[index, 1] <- "MATBEH"
  names[index, 2] <- "Mathematics Behaviour"
  
  db$MATHEFF <- substr(data[, 1], start = 857, stop = 864)
  index <- which(names(db) == "MATHEFF")
  
  names[index, 1] <- "MATHEFF"
  names[index, 2] <- "Mathematics Self-Efficacy"
  
  db$MATINTFC <- substr(data[, 1], start = 865, stop = 873)
  index <- which(names(db) == "MATINTFC")
  
  names[index, 1] <- "MATINTFC"
  names[index, 2] <- "Mathematics Intentions"
  
  db$MATWKETH <- substr(data[, 1], start = 874, stop = 882)
  index <- which(names(db) == "MATWKETH")
  
  names[index, 1] <- "MATWKETH"
  names[index, 2] <- "Mathematics Work Ethic"
  
  db$MISCED <- substr(data[, 1], start = 883, stop = 883)
  index <- which(names(db) == "MISCED")
  
  names[index, 1] <- "MISCED"
  names[index, 2] <- "Educational level of mother (ISCED)"
  
  db$MMINS <- substr(data[, 1], start = 884, stop = 887)
  index <- which(names(db) == "MMINS")
  
  names[index, 1] <- "MMINS"
  names[index, 2] <- "Learning time (minutes per week)- <Mathematics>"
  
  db$MTSUP <- substr(data[, 1], start = 888, stop = 896)
  index <- which(names(db) == "MTSUP")
  
  names[index, 1] <- "MTSUP"
  names[index, 2] <- "Mathematics Teacher's Support"
  
  db$OCOD1 <- substr(data[, 1], start = 897, stop = 900)
  index <- which(names(db) == "OCOD1")
  
  names[index, 1] <- "OCOD1"
  names[index, 2] <- "ISCO-08 Occupation code - Mother"
  
  db$OCOD2 <- substr(data[, 1], start = 901, stop = 904)
  index <- which(names(db) == "OCOD2")
  
  names[index, 1] <- "OCOD2"
  names[index, 2] <- "ISCO-08 Occupation code - Father"
  
  db$OPENPS <- substr(data[, 1], start = 905, stop = 913)
  index <- which(names(db) == "OPENPS")
  
  names[index, 1] <- "OPENPS"
  names[index, 2] <- "Openness for Problem Solving"
  
  db$OUTHOURS <- substr(data[, 1], start = 914, stop = 921)
  index <- which(names(db) == "OUTHOURS")
  
  names[index, 1] <- "OUTHOURS"
  names[index, 2] <- "Out-of-School Study Time"
  
  db$PARED <- substr(data[, 1], start = 922, stop = 929)
  index <- which(names(db) == "PARED")
  
  names[index, 1] <- "PARED"
  names[index, 2] <- "Highest parental education in years"
  
  db$PERSEV <- substr(data[, 1], start = 930, stop = 938)
  index <- which(names(db) == "PERSEV")
  
  names[index, 1] <- "PERSEV"
  names[index, 2] <- "Perseverance"
  
  db$REPEAT <- substr(data[, 1], start = 939, stop = 939)
  index <- which(names(db) == "REPEAT")
  
  names[index, 1] <- "REPEAT"
  names[index, 2] <- "Grade Repetition"
  
  db$SCMAT <- substr(data[, 1], start = 940, stop = 947)
  index <- which(names(db) == "SCMAT")
  
  names[index, 1] <- "SCMAT"
  names[index, 2] <- "Mathematics Self-Concept"
  
  db$SMINS <- substr(data[, 1], start = 948, stop = 951)
  index <- which(names(db) == "SMINS")
  
  names[index, 1] <- "SMINS"
  names[index, 2] <- "Learning time (minutes per week) - <Science>"
  
  db$STUDREL <- substr(data[, 1], start = 952, stop = 959)
  index <- which(names(db) == "STUDREL")
  
  names[index, 1] <- "STUDREL"
  names[index, 2] <- "Teacher Student Relations"
  
  db$SUBNORM <- substr(data[, 1], start = 960, stop = 968)
  index <- which(names(db) == "SUBNORM")
  
  names[index, 1] <- "SUBNORM"
  names[index, 2] <- "Subjective Norms in Mathematics"
  
  db$TCHBEHFA <- substr(data[, 1], start = 969, stop = 977)
  index <- which(names(db) == "TCHBEHFA")
  
  names[index, 1] <- "TCHBEHFA"
  names[index, 2] <- "Teacher Behaviour: Formative Assessment"
  
  db$TCHBEHSO <- substr(data[, 1], start = 978, stop = 986)
  index <- which(names(db) == "TCHBEHSO")
  
  names[index, 1] <- "TCHBEHSO"
  names[index, 2] <- "Teacher Behaviour: Student Orientation"
  
  db$TCHBEHTD <- substr(data[, 1], start = 987, stop = 995)
  index <- which(names(db) == "TCHBEHTD")
  
  names[index, 1] <- "TCHBEHTD"
  names[index, 2] <- "Teacher Behaviour: Teacher-directed Instruction"
  
  db$TEACHSUP <- substr(data[, 1], start = 996, stop = 1003)
  index <- which(names(db) == "TEACHSUP")
  
  names[index, 1] <- "TEACHSUP"
  names[index, 2] <- "Teacher Support"
  
  db$TESTLANG <- substr(data[, 1], start = 1004, stop = 1006)
  index <- which(names(db) == "TESTLANG")
  
  names[index, 1] <- "TESTLANG"
  names[index, 2] <- "Language of the test"
  
  db$TIMEINT <- substr(data[, 1], start = 1007, stop = 1015)
  index <- which(names(db) == "TIMEINT")
  
  names[index, 1] <- "TIMEINT"
  names[index, 2] <- "Time of computer use (mins)"
  
  db$USEMATH <- substr(data[, 1], start = 1016, stop = 1024)
  index <- which(names(db) == "USEMATH")
  
  names[index, 1] <- "USEMATH"
  names[index, 2] <- "Use of ICT in Mathematic Lessons"
  
  db$USESCH <- substr(data[, 1], start = 1025, stop = 1033)
  index <- which(names(db) == "USESCH")
  
  names[index, 1] <- "USESCH"
  names[index, 2] <- "Use of ICT at School"
  
  db$WEALTH <- substr(data[, 1], start = 1034, stop = 1041)
  index <- which(names(db) == "WEALTH")
  
  names[index, 1] <- "WEALTH"
  names[index, 2] <- "Wealth"
  
  db$ANCATSCHL <- substr(data[, 1], start = 1042, stop = 1050)
  index <- which(names(db) == "ANCATSCHL")
  
  names[index, 1] <- "ANCATSCHL"
  names[index, 2] <- "Attitude towards School: Learning Outcomes (Anchored)"
  
  db$ANCATTLNACT <- substr(data[, 1], start = 1051, stop = 1059)
  index <- which(names(db) == "ANCATTLNACT")
  
  names[index, 1] <- "ANCATTLNACT"
  names[index, 2] <- "Attitude towards School: Learning Activities (Anchored)"
  
  db$ANCBELONG <- substr(data[, 1], start = 1060, stop = 1068)
  index <- which(names(db) == "ANCBELONG")
  
  names[index, 1] <- "ANCBELONG"
  names[index, 2] <- "Sense of Belonging to School (Anchored)"
  
  db$ANCCLSMAN <- substr(data[, 1], start = 1069, stop = 1077)
  index <- which(names(db) == "ANCCLSMAN")
  
  names[index, 1] <- "ANCCLSMAN"
  names[index, 2] <- "Mathematics Teacher's Classroom Management (Anchored)"
  
  db$ANCCOGACT <- substr(data[, 1], start = 1078, stop = 1086)
  index <- which(names(db) == "ANCCOGACT")
  
  names[index, 1] <- "ANCCOGACT"
  names[index, 2] <- "Cognitive Activation in Mathematics Lessons (Anchored)"
  
  db$ANCINSTMOT <- substr(data[, 1], start = 1087, stop = 1095)
  index <- which(names(db) == "ANCINSTMOT")
  
  names[index, 1] <- "ANCINSTMOT"
  names[index, 2] <- "Instrumental Motivation for Mathematics (Anchored)"
  
  db$ANCINTMAT <- substr(data[, 1], start = 1096, stop = 1104)
  index <- which(names(db) == "ANCINTMAT")
  
  names[index, 1] <- "ANCINTMAT"
  names[index, 2] <- "Mathematics Interest (Anchored)"
  
  db$ANCMATWKETH <- substr(data[, 1], start = 1105, stop = 1113)
  index <- which(names(db) == "ANCMATWKETH")
  
  names[index, 1] <- "ANCMATWKETH"
  names[index, 2] <- "Mathematics Work Ethic (Anchored)"
  
  db$ANCMTSUP <- substr(data[, 1], start = 1114, stop = 1122)
  index <- which(names(db) == "ANCMTSUP")
  
  names[index, 1] <- "ANCMTSUP"
  names[index, 2] <- "Mathematics Teacher's Support (Anchored)"
  
  db$ANCSCMAT <- substr(data[, 1], start = 1123, stop = 1131)
  index <- which(names(db) == "ANCSCMAT")
  
  names[index, 1] <- "ANCSCMAT"
  names[index, 2] <- "Mathematics Self-Concept (Anchored)"
  
  db$ANCSTUDREL <- substr(data[, 1], start = 1132, stop = 1140)
  index <- which(names(db) == "ANCSTUDREL")
  
  names[index, 1] <- "ANCSTUDREL"
  names[index, 2] <- "Teacher Student Relations (Anchored)"
  
  db$ANCSUBNORM <- substr(data[, 1], start = 1141, stop = 1149)
  index <- which(names(db) == "ANCSUBNORM")
  
  names[index, 1] <- "ANCSUBNORM"
  names[index, 2] <- "Subjective Norms in Mathematics (Anchored)"
  
  db$PV1MATH <- substr(data[, 1], start = 1150, stop = 1158)
  index <- which(names(db) == "PV1MATH")
  
  names[index, 1] <- "PV1MATH"
  names[index, 2] <- "Plausible value 1 in mathematics"
  
  db$PV2MATH <- substr(data[, 1], start = 1159, stop = 1167)
  index <- which(names(db) == "PV2MATH")
  
  names[index, 1] <- "PV2MATH"
  names[index, 2] <- "Plausible value 2 in mathematics"
  
  db$PV3MATH <- substr(data[, 1], start = 1168, stop = 1176)
  index <- which(names(db) == "PV3MATH")
  
  names[index, 1] <- "PV3MATH"
  names[index, 2] <- "Plausible value 3 in mathematics"
  
  db$PV4MATH <- substr(data[, 1], start = 1177, stop = 1185)
  index <- which(names(db) == "PV4MATH")
  
  names[index, 1] <- "PV4MATH"
  names[index, 2] <- "Plausible value 4 in mathematics"
  
  db$PV5MATH <- substr(data[, 1], start = 1186, stop = 1194)
  index <- which(names(db) == "PV5MATH")
  
  names[index, 1] <- "PV5MATH"
  names[index, 2] <- "Plausible value 5 in mathematics"
  
  db$PV1MACC <- substr(data[, 1], start = 1195, stop = 1203)
  index <- which(names(db) == "PV1MACC")
  
  names[index, 1] <- "PV1MACC"
  names[index, 2] <- "Plausible value 1 in content subscale of Maths - Change and Relationships"
  
  db$PV2MACC <- substr(data[, 1], start = 1204, stop = 1212)
  index <- which(names(db) == "PV2MACC")
  
  names[index, 1] <- "PV2MACC"
  names[index, 2] <- "Plausible value 2 in content subscale of Maths - Change and Relationships"
  
  db$PV3MACC <- substr(data[, 1], start = 1213, stop = 1221)
  index <- which(names(db) == "PV3MACC")
  
  names[index, 1] <- "PV3MACC"
  names[index, 2] <- "Plausible value 3 in content subscale of Maths - Change and Relationships"
  
  db$PV4MACC <- substr(data[, 1], start = 1222, stop = 1230)
  index <- which(names(db) == "PV4MACC")
  
  names[index, 1] <- "PV4MACC"
  names[index, 2] <- "Plausible value 4 in content subscale of Maths - Change and Relationships"
  
  db$PV5MACC <- substr(data[, 1], start = 1231, stop = 1239)
  index <- which(names(db) == "PV5MACC")
  
  names[index, 1] <- "PV5MACC"
  names[index, 2] <- "Plausible value 5 in content subscale of Maths - Change and Relationships"
  
  db$PV1MACQ <- substr(data[, 1], start = 1240, stop = 1248)
  index <- which(names(db) == "PV1MACQ")
  
  names[index, 1] <- "PV1MACQ"
  names[index, 2] <- "Plausible value 1 in content subscale of Maths - Quantity"
  
  db$PV2MACQ <- substr(data[, 1], start = 1249, stop = 1257)
  index <- which(names(db) == "PV2MACQ")
  
  names[index, 1] <- "PV2MACQ"
  names[index, 2] <- "Plausible value 2 in content subscale of Maths - Quantity"
  
  db$PV3MACQ <- substr(data[, 1], start = 1258, stop = 1266)
  index <- which(names(db) == "PV3MACQ")
  
  names[index, 1] <- "PV3MACQ"
  names[index, 2] <- "Plausible value 3 in content subscale of Maths - Quantity"
  
  db$PV4MACQ <- substr(data[, 1], start = 1267, stop = 1275)
  index <- which(names(db) == "PV4MACQ")
  
  names[index, 1] <- "PV4MACQ"
  names[index, 2] <- "Plausible value 4 in content subscale of Maths - Quantity"
  
  db$PV5MACQ <- substr(data[, 1], start = 1276, stop = 1284)
  index <- which(names(db) == "PV5MACQ")
  
  names[index, 1] <- "PV5MACQ"
  names[index, 2] <- "Plausible value 5 in content subscale of Maths - Quantity"
  
  db$PV1MACS <- substr(data[, 1], start = 1285, stop = 1293)
  index <- which(names(db) == "PV1MACS")
  
  names[index, 1] <- "PV1MACS"
  names[index, 2] <- "Plausible value 1 in content subscale of Maths - Space and Shape"
  
  db$PV2MACS <- substr(data[, 1], start = 1294, stop = 1302)
  index <- which(names(db) == "PV2MACS")
  
  names[index, 1] <- "PV2MACS"
  names[index, 2] <- "Plausible value 2 in content subscale of Maths - Space and Shape"
  
  db$PV3MACS <- substr(data[, 1], start = 1303, stop = 1311)
  index <- which(names(db) == "PV3MACS")
  
  names[index, 1] <- "PV3MACS"
  names[index, 2] <- "Plausible value 3 in content subscale of Maths - Space and Shape"
  
  db$PV4MACS <- substr(data[, 1], start = 1312, stop = 1320)
  index <- which(names(db) == "PV4MACS")
  
  names[index, 1] <- "PV4MACS"
  names[index, 2] <- "Plausible value 4 in content subscale of Maths - Space and Shape"
  
  db$PV5MACS <- substr(data[, 1], start = 1321, stop = 1329)
  index <- which(names(db) == "PV5MACS")
  
  names[index, 1] <- "PV5MACS"
  names[index, 2] <- "Plausible value 5 in content subscale of Maths - Space and Shape"
  
  db$PV1MACU <- substr(data[, 1], start = 1330, stop = 1338)
  index <- which(names(db) == "PV1MACU")
  
  names[index, 1] <- "PV1MACU"
  names[index, 2] <- "Plausible value 1 in content subscale of Maths - Uncertainty and Data"
  
  db$PV2MACU <- substr(data[, 1], start = 1339, stop = 1347)
  index <- which(names(db) == "PV2MACU")
  
  names[index, 1] <- "PV2MACU"
  names[index, 2] <- "Plausible value 2 in content subscale of Maths - Uncertainty and Data"
  
  db$PV3MACU <- substr(data[, 1], start = 1348, stop = 1356)
  index <- which(names(db) == "PV3MACU")
  
  names[index, 1] <- "PV3MACU"
  names[index, 2] <- "Plausible value 3 in content subscale of Maths - Uncertainty and Data"
  
  db$PV4MACU <- substr(data[, 1], start = 1357, stop = 1365)
  index <- which(names(db) == "PV4MACU")
  
  names[index, 1] <- "PV4MACU"
  names[index, 2] <- "Plausible value 4 in content subscale of Maths - Uncertainty and Data"
  
  db$PV5MACU <- substr(data[, 1], start = 1366, stop = 1374)
  index <- which(names(db) == "PV5MACU")
  
  names[index, 1] <- "PV5MACU"
  names[index, 2] <- "Plausible value 5 in content subscale of Maths - Uncertainty and Data"
  
  db$PV1MAPE <- substr(data[, 1], start = 1375, stop = 1383)
  index <- which(names(db) == "PV1MAPE")
  
  names[index, 1] <- "PV1MAPE"
  names[index, 2] <- "Plausible value 1 in process subscale of Maths - Employ"
  
  db$PV2MAPE <- substr(data[, 1], start = 1384, stop = 1392)
  index <- which(names(db) == "PV2MAPE")
  
  names[index, 1] <- "PV2MAPE"
  names[index, 2] <- "Plausible value 2 in process subscale of Maths - Employ"
  
  db$PV3MAPE <- substr(data[, 1], start = 1393, stop = 1401)
  index <- which(names(db) == "PV3MAPE")
  
  names[index, 1] <- "PV3MAPE"
  names[index, 2] <- "Plausible value 3 in process subscale of Maths - Employ"
  
  db$PV4MAPE <- substr(data[, 1], start = 1402, stop = 1410)
  index <- which(names(db) == "PV4MAPE")
  
  names[index, 1] <- "PV4MAPE"
  names[index, 2] <- "Plausible value 4 in process subscale of Maths - Employ"
  
  db$PV5MAPE <- substr(data[, 1], start = 1411, stop = 1419)
  index <- which(names(db) == "PV5MAPE")
  
  names[index, 1] <- "PV5MAPE"
  names[index, 2] <- "Plausible value 5 in process subscale of Maths - Employ"
  
  db$PV1MAPF <- substr(data[, 1], start = 1420, stop = 1428)
  index <- which(names(db) == "PV1MAPF")
  
  names[index, 1] <- "PV1MAPF"
  names[index, 2] <- "Plausible value 1 in process subscale of Maths - Formulate"
  
  db$PV2MAPF <- substr(data[, 1], start = 1429, stop = 1437)
  index <- which(names(db) == "PV2MAPF")
  
  names[index, 1] <- "PV2MAPF"
  names[index, 2] <- "Plausible value 2 in process subscale of Maths - Formulate"
  
  db$PV3MAPF <- substr(data[, 1], start = 1438, stop = 1446)
  index <- which(names(db) == "PV3MAPF")
  
  names[index, 1] <- "PV3MAPF"
  names[index, 2] <- "Plausible value 3 in process subscale of Maths - Formulate"
  
  db$PV4MAPF <- substr(data[, 1], start = 1447, stop = 1455)
  index <- which(names(db) == "PV4MAPF")
  
  names[index, 1] <- "PV4MAPF"
  names[index, 2] <- "Plausible value 4 in process subscale of Maths - Formulate"
  
  db$PV5MAPF <- substr(data[, 1], start = 1456, stop = 1464)
  index <- which(names(db) == "PV5MAPF")
  
  names[index, 1] <- "PV5MAPF"
  names[index, 2] <- "Plausible value 5 in process subscale of Maths - Formulate"
  
  db$PV1MAPI <- substr(data[, 1], start = 1465, stop = 1473)
  index <- which(names(db) == "PV1MAPI")
  
  names[index, 1] <- "PV1MAPI"
  names[index, 2] <- "Plausible value 1 in process subscale of Maths - Interpret"
  
  db$PV2MAPI <- substr(data[, 1], start = 1474, stop = 1482)
  index <- which(names(db) == "PV2MAPI")
  
  names[index, 1] <- "PV2MAPI"
  names[index, 2] <- "Plausible value 2 in process subscale of Maths - Interpret"
  
  db$PV3MAPI <- substr(data[, 1], start = 1483, stop = 1491)
  index <- which(names(db) == "PV3MAPI")
  
  names[index, 1] <- "PV3MAPI"
  names[index, 2] <- "Plausible value 3 in process subscale of Maths - Interpret"
  
  db$PV4MAPI <- substr(data[, 1], start = 1492, stop = 1500)
  index <- which(names(db) == "PV4MAPI")
  
  names[index, 1] <- "PV4MAPI"
  names[index, 2] <- "Plausible value 4 in process subscale of Maths - Interpret"
  
  db$PV5MAPI <- substr(data[, 1], start = 1501, stop = 1509)
  index <- which(names(db) == "PV5MAPI")
  
  names[index, 1] <- "PV5MAPI"
  names[index, 2] <- "Plausible value 5 in process subscale of Maths - Interpret"
  
  db$PV1READ <- substr(data[, 1], start = 1510, stop = 1518)
  index <- which(names(db) == "PV1READ")
  
  names[index, 1] <- "PV1READ"
  names[index, 2] <- "Plausible value 1 in reading"
  
  db$PV2READ <- substr(data[, 1], start = 1519, stop = 1527)
  index <- which(names(db) == "PV2READ")
  
  names[index, 1] <- "PV2READ"
  names[index, 2] <- "Plausible value 2 in reading"
  
  db$PV3READ <- substr(data[, 1], start = 1528, stop = 1536)
  index <- which(names(db) == "PV3READ")
  
  names[index, 1] <- "PV3READ"
  names[index, 2] <- "Plausible value 3 in reading"
  
  db$PV4READ <- substr(data[, 1], start = 1537, stop = 1545)
  index <- which(names(db) == "PV4READ")
  
  names[index, 1] <- "PV4READ"
  names[index, 2] <- "Plausible value 4 in reading"
  
  db$PV5READ <- substr(data[, 1], start = 1546, stop = 1554)
  index <- which(names(db) == "PV5READ")
  
  names[index, 1] <- "PV5READ"
  names[index, 2] <- "Plausible value 5 in reading"
  
  db$PV1SCIE <- substr(data[, 1], start = 1555, stop = 1563)
  index <- which(names(db) == "PV1SCIE")
  
  names[index, 1] <- "PV1SCIE"
  names[index, 2] <- "Plausible value 1 in science"
  
  db$PV2SCIE <- substr(data[, 1], start = 1564, stop = 1572)
  index <- which(names(db) == "PV2SCIE")
  
  names[index, 1] <- "PV2SCIE"
  names[index, 2] <- "Plausible value 2 in science"
  
  db$PV3SCIE <- substr(data[, 1], start = 1573, stop = 1581)
  index <- which(names(db) == "PV3SCIE")
  
  names[index, 1] <- "PV3SCIE"
  names[index, 2] <- "Plausible value 3 in science"
  
  db$PV4SCIE <- substr(data[, 1], start = 1582, stop = 1590)
  index <- which(names(db) == "PV4SCIE")
  
  names[index, 1] <- "PV4SCIE"
  names[index, 2] <- "Plausible value 4 in science"
  
  db$PV5SCIE <- substr(data[, 1], start = 1591, stop = 1599)
  index <- which(names(db) == "PV5SCIE")
  
  names[index, 1] <- "PV5SCIE"
  names[index, 2] <- "Plausible value 5 in science"
  
  db$W_FSTUWT <- substr(data[, 1], start = 1600, stop = 1608)
  index <- which(names(db) == "W_FSTUWT")
  
  names[index, 1] <- "W_FSTUWT"
  names[index, 2] <- "FINAL STUDENT WEIGHT"
  
  db$W_FSTR1 <- substr(data[, 1], start = 1609, stop = 1617)
  index <- which(names(db) == "W_FSTR1")
  
  names[index, 1] <- "W_FSTR1"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT1"
  
  db$W_FSTR2 <- substr(data[, 1], start = 1618, stop = 1626)
  index <- which(names(db) == "W_FSTR2")
  
  names[index, 1] <- "W_FSTR2"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT2"
  
  db$W_FSTR3 <- substr(data[, 1], start = 1627, stop = 1635)
  index <- which(names(db) == "W_FSTR3")
  
  names[index, 1] <- "W_FSTR3"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT3"
  
  db$W_FSTR4 <- substr(data[, 1], start = 1636, stop = 1644)
  index <- which(names(db) == "W_FSTR4")
  
  names[index, 1] <- "W_FSTR4"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT4"
  
  db$W_FSTR5 <- substr(data[, 1], start = 1645, stop = 1653)
  index <- which(names(db) == "W_FSTR5")
  
  names[index, 1] <- "W_FSTR5"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT5"
  
  db$W_FSTR6 <- substr(data[, 1], start = 1654, stop = 1662)
  index <- which(names(db) == "W_FSTR6")
  
  names[index, 1] <- "W_FSTR6"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT6"
  
  db$W_FSTR7 <- substr(data[, 1], start = 1663, stop = 1671)
  index <- which(names(db) == "W_FSTR7")
  
  names[index, 1] <- "W_FSTR7"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT7"
  
  db$W_FSTR8 <- substr(data[, 1], start = 1672, stop = 1680)
  index <- which(names(db) == "W_FSTR8")
  
  names[index, 1] <- "W_FSTR8"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT8"
  
  db$W_FSTR9 <- substr(data[, 1], start = 1681, stop = 1689)
  index <- which(names(db) == "W_FSTR9")
  
  names[index, 1] <- "W_FSTR9"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT9"
  
  db$W_FSTR10 <- substr(data[, 1], start = 1690, stop = 1698)
  index <- which(names(db) == "W_FSTR10")
  
  names[index, 1] <- "W_FSTR10"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT10"
  
  db$W_FSTR11 <- substr(data[, 1], start = 1699, stop = 1707)
  index <- which(names(db) == "W_FSTR11")
  
  names[index, 1] <- "W_FSTR11"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT11"
  
  db$W_FSTR12 <- substr(data[, 1], start = 1708, stop = 1716)
  index <- which(names(db) == "W_FSTR12")
  
  names[index, 1] <- "W_FSTR12"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT12"
  
  db$W_FSTR13 <- substr(data[, 1], start = 1717, stop = 1725)
  index <- which(names(db) == "W_FSTR13")
  
  names[index, 1] <- "W_FSTR13"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT13"
  
  db$W_FSTR14 <- substr(data[, 1], start = 1726, stop = 1734)
  index <- which(names(db) == "W_FSTR14")
  
  names[index, 1] <- "W_FSTR14"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT14"
  
  db$W_FSTR15 <- substr(data[, 1], start = 1735, stop = 1743)
  index <- which(names(db) == "W_FSTR15")
  
  names[index, 1] <- "W_FSTR15"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT15"
  
  db$W_FSTR16 <- substr(data[, 1], start = 1744, stop = 1752)
  index <- which(names(db) == "W_FSTR16")
  
  names[index, 1] <- "W_FSTR16"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT16"
  
  db$W_FSTR17 <- substr(data[, 1], start = 1753, stop = 1761)
  index <- which(names(db) == "W_FSTR17")
  
  names[index, 1] <- "W_FSTR17"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT17"
  
  db$W_FSTR18 <- substr(data[, 1], start = 1762, stop = 1770)
  index <- which(names(db) == "W_FSTR18")
  
  names[index, 1] <- "W_FSTR18"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT18"
  
  db$W_FSTR19 <- substr(data[, 1], start = 1771, stop = 1779)
  index <- which(names(db) == "W_FSTR19")
  
  names[index, 1] <- "W_FSTR19"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT19"
  
  db$W_FSTR20 <- substr(data[, 1], start = 1780, stop = 1788)
  index <- which(names(db) == "W_FSTR20")
  
  names[index, 1] <- "W_FSTR20"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT20"
  
  db$W_FSTR21 <- substr(data[, 1], start = 1789, stop = 1797)
  index <- which(names(db) == "W_FSTR21")
  
  names[index, 1] <- "W_FSTR21"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT21"
  
  db$W_FSTR22 <- substr(data[, 1], start = 1798, stop = 1806)
  index <- which(names(db) == "W_FSTR22")
  
  names[index, 1] <- "W_FSTR22"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT22"
  
  db$W_FSTR23 <- substr(data[, 1], start = 1807, stop = 1815)
  index <- which(names(db) == "W_FSTR23")
  
  names[index, 1] <- "W_FSTR23"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT23"
  
  db$W_FSTR24 <- substr(data[, 1], start = 1816, stop = 1824)
  index <- which(names(db) == "W_FSTR24")
  
  names[index, 1] <- "W_FSTR24"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT24"
  
  db$W_FSTR25 <- substr(data[, 1], start = 1825, stop = 1833)
  index <- which(names(db) == "W_FSTR25")
  
  names[index, 1] <- "W_FSTR25"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT25"
  
  db$W_FSTR26 <- substr(data[, 1], start = 1834, stop = 1842)
  index <- which(names(db) == "W_FSTR26")
  
  names[index, 1] <- "W_FSTR26"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT26"
  
  db$W_FSTR27 <- substr(data[, 1], start = 1843, stop = 1851)
  index <- which(names(db) == "W_FSTR27")
  
  names[index, 1] <- "W_FSTR27"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT27"
  
  db$W_FSTR28 <- substr(data[, 1], start = 1852, stop = 1860)
  index <- which(names(db) == "W_FSTR28")
  
  names[index, 1] <- "W_FSTR28"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT28"
  
  db$W_FSTR29 <- substr(data[, 1], start = 1861, stop = 1869)
  index <- which(names(db) == "W_FSTR29")
  
  names[index, 1] <- "W_FSTR29"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT29"
  
  db$W_FSTR30 <- substr(data[, 1], start = 1870, stop = 1878)
  index <- which(names(db) == "W_FSTR30")
  
  names[index, 1] <- "W_FSTR30"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT30"
  
  db$W_FSTR31 <- substr(data[, 1], start = 1879, stop = 1887)
  index <- which(names(db) == "W_FSTR31")
  
  names[index, 1] <- "W_FSTR31"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT31"
  
  db$W_FSTR32 <- substr(data[, 1], start = 1888, stop = 1896)
  index <- which(names(db) == "W_FSTR32")
  
  names[index, 1] <- "W_FSTR32"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT32"
  
  db$W_FSTR33 <- substr(data[, 1], start = 1897, stop = 1905)
  index <- which(names(db) == "W_FSTR33")
  
  names[index, 1] <- "W_FSTR33"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT33"
  
  db$W_FSTR34 <- substr(data[, 1], start = 1906, stop = 1914)
  index <- which(names(db) == "W_FSTR34")
  
  names[index, 1] <- "W_FSTR34"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT34"
  
  db$W_FSTR35 <- substr(data[, 1], start = 1915, stop = 1923)
  index <- which(names(db) == "W_FSTR35")
  
  names[index, 1] <- "W_FSTR35"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT35"
  
  db$W_FSTR36 <- substr(data[, 1], start = 1924, stop = 1932)
  index <- which(names(db) == "W_FSTR36")
  
  names[index, 1] <- "W_FSTR36"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT36"
  
  db$W_FSTR37 <- substr(data[, 1], start = 1933, stop = 1941)
  index <- which(names(db) == "W_FSTR37")
  
  names[index, 1] <- "W_FSTR37"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT37"
  
  db$W_FSTR38 <- substr(data[, 1], start = 1942, stop = 1950)
  index <- which(names(db) == "W_FSTR38")
  
  names[index, 1] <- "W_FSTR38"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT38"
  
  db$W_FSTR39 <- substr(data[, 1], start = 1951, stop = 1959)
  index <- which(names(db) == "W_FSTR39")
  
  names[index, 1] <- "W_FSTR39"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT39"
  
  db$W_FSTR40 <- substr(data[, 1], start = 1960, stop = 1968)
  index <- which(names(db) == "W_FSTR40")
  
  names[index, 1] <- "W_FSTR40"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT40"
  
  db$W_FSTR41 <- substr(data[, 1], start = 1969, stop = 1977)
  index <- which(names(db) == "W_FSTR41")
  
  names[index, 1] <- "W_FSTR41"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT41"
  
  db$W_FSTR42 <- substr(data[, 1], start = 1978, stop = 1986)
  index <- which(names(db) == "W_FSTR42")
  
  names[index, 1] <- "W_FSTR42"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT42"
  
  db$W_FSTR43 <- substr(data[, 1], start = 1987, stop = 1995)
  index <- which(names(db) == "W_FSTR43")
  
  names[index, 1] <- "W_FSTR43"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT43"
  
  db$W_FSTR44 <- substr(data[, 1], start = 1996, stop = 2004)
  index <- which(names(db) == "W_FSTR44")
  
  names[index, 1] <- "W_FSTR44"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT44"
  
  db$W_FSTR45 <- substr(data[, 1], start = 2005, stop = 2013)
  index <- which(names(db) == "W_FSTR45")
  
  names[index, 1] <- "W_FSTR45"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT45"
  
  db$W_FSTR46 <- substr(data[, 1], start = 2014, stop = 2022)
  index <- which(names(db) == "W_FSTR46")
  
  names[index, 1] <- "W_FSTR46"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT46"
  
  db$W_FSTR47 <- substr(data[, 1], start = 2023, stop = 2031)
  index <- which(names(db) == "W_FSTR47")
  
  names[index, 1] <- "W_FSTR47"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT47"
  
  db$W_FSTR48 <- substr(data[, 1], start = 2032, stop = 2040)
  index <- which(names(db) == "W_FSTR48")
  
  names[index, 1] <- "W_FSTR48"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT48"
  
  db$W_FSTR49 <- substr(data[, 1], start = 2041, stop = 2049)
  index <- which(names(db) == "W_FSTR49")
  
  names[index, 1] <- "W_FSTR49"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT49"
  
  db$W_FSTR50 <- substr(data[, 1], start = 2050, stop = 2058)
  index <- which(names(db) == "W_FSTR50")
  
  names[index, 1] <- "W_FSTR50"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT50"
  
  db$W_FSTR51 <- substr(data[, 1], start = 2059, stop = 2067)
  index <- which(names(db) == "W_FSTR51")
  
  names[index, 1] <- "W_FSTR51"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT51"
  
  db$W_FSTR52 <- substr(data[, 1], start = 2068, stop = 2076)
  index <- which(names(db) == "W_FSTR52")
  
  names[index, 1] <- "W_FSTR52"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT52"
  
  db$W_FSTR53 <- substr(data[, 1], start = 2077, stop = 2085)
  index <- which(names(db) == "W_FSTR53")
  
  names[index, 1] <- "W_FSTR53"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT53"
  
  db$W_FSTR54 <- substr(data[, 1], start = 2086, stop = 2094)
  index <- which(names(db) == "W_FSTR54")
  
  names[index, 1] <- "W_FSTR54"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT54"
  
  db$W_FSTR55 <- substr(data[, 1], start = 2095, stop = 2103)
  index <- which(names(db) == "W_FSTR55")
  
  names[index, 1] <- "W_FSTR55"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT55"
  
  db$W_FSTR56 <- substr(data[, 1], start = 2104, stop = 2112)
  index <- which(names(db) == "W_FSTR56")
  
  names[index, 1] <- "W_FSTR56"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT56"
  
  db$W_FSTR57 <- substr(data[, 1], start = 2113, stop = 2121)
  index <- which(names(db) == "W_FSTR57")
  
  names[index, 1] <- "W_FSTR57"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT57"
  
  db$W_FSTR58 <- substr(data[, 1], start = 2122, stop = 2130)
  index <- which(names(db) == "W_FSTR58")
  
  names[index, 1] <- "W_FSTR58"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT58"
  
  db$W_FSTR59 <- substr(data[, 1], start = 2131, stop = 2139)
  index <- which(names(db) == "W_FSTR59")
  
  names[index, 1] <- "W_FSTR59"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT59"
  
  db$W_FSTR60 <- substr(data[, 1], start = 2140, stop = 2148)
  index <- which(names(db) == "W_FSTR60")
  
  names[index, 1] <- "W_FSTR60"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT60"
  
  db$W_FSTR61 <- substr(data[, 1], start = 2149, stop = 2157)
  index <- which(names(db) == "W_FSTR61")
  
  names[index, 1] <- "W_FSTR61"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT61"
  
  db$W_FSTR62 <- substr(data[, 1], start = 2158, stop = 2166)
  index <- which(names(db) == "W_FSTR62")
  
  names[index, 1] <- "W_FSTR62"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT62"
  
  db$W_FSTR63 <- substr(data[, 1], start = 2167, stop = 2175)
  index <- which(names(db) == "W_FSTR63")
  
  names[index, 1] <- "W_FSTR63"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT63"
  
  db$W_FSTR64 <- substr(data[, 1], start = 2176, stop = 2184)
  index <- which(names(db) == "W_FSTR64")
  
  names[index, 1] <- "W_FSTR64"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT64"
  
  db$W_FSTR65 <- substr(data[, 1], start = 2185, stop = 2193)
  index <- which(names(db) == "W_FSTR65")
  
  names[index, 1] <- "W_FSTR65"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT65"
  
  db$W_FSTR66 <- substr(data[, 1], start = 2194, stop = 2202)
  index <- which(names(db) == "W_FSTR66")
  
  names[index, 1] <- "W_FSTR66"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT66"
  
  db$W_FSTR67 <- substr(data[, 1], start = 2203, stop = 2211)
  index <- which(names(db) == "W_FSTR67")
  
  names[index, 1] <- "W_FSTR67"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT67"
  
  db$W_FSTR68 <- substr(data[, 1], start = 2212, stop = 2220)
  index <- which(names(db) == "W_FSTR68")
  
  names[index, 1] <- "W_FSTR68"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT68"
  
  db$W_FSTR69 <- substr(data[, 1], start = 2221, stop = 2229)
  index <- which(names(db) == "W_FSTR69")
  
  names[index, 1] <- "W_FSTR69"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT69"
  
  db$W_FSTR70 <- substr(data[, 1], start = 2230, stop = 2238)
  index <- which(names(db) == "W_FSTR70")
  
  names[index, 1] <- "W_FSTR70"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT70"
  
  db$W_FSTR71 <- substr(data[, 1], start = 2239, stop = 2247)
  index <- which(names(db) == "W_FSTR71")
  
  names[index, 1] <- "W_FSTR71"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT71"
  
  db$W_FSTR72 <- substr(data[, 1], start = 2248, stop = 2256)
  index <- which(names(db) == "W_FSTR72")
  
  names[index, 1] <- "W_FSTR72"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT72"
  
  db$W_FSTR73 <- substr(data[, 1], start = 2257, stop = 2265)
  index <- which(names(db) == "W_FSTR73")
  
  names[index, 1] <- "W_FSTR73"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT73"
  
  db$W_FSTR74 <- substr(data[, 1], start = 2266, stop = 2274)
  index <- which(names(db) == "W_FSTR74")
  
  names[index, 1] <- "W_FSTR74"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT74"
  
  db$W_FSTR75 <- substr(data[, 1], start = 2275, stop = 2283)
  index <- which(names(db) == "W_FSTR75")
  
  names[index, 1] <- "W_FSTR75"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT75"
  
  db$W_FSTR76 <- substr(data[, 1], start = 2284, stop = 2292)
  index <- which(names(db) == "W_FSTR76")
  
  names[index, 1] <- "W_FSTR76"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT76"
  
  db$W_FSTR77 <- substr(data[, 1], start = 2293, stop = 2301)
  index <- which(names(db) == "W_FSTR77")
  
  names[index, 1] <- "W_FSTR77"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT77"
  
  db$W_FSTR78 <- substr(data[, 1], start = 2302, stop = 2310)
  index <- which(names(db) == "W_FSTR78")
  
  names[index, 1] <- "W_FSTR78"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT78"
  
  db$W_FSTR79 <- substr(data[, 1], start = 2311, stop = 2319)
  index <- which(names(db) == "W_FSTR79")
  
  names[index, 1] <- "W_FSTR79"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT79"
  
  db$W_FSTR80 <- substr(data[, 1], start = 2320, stop = 2328)
  index <- which(names(db) == "W_FSTR80")
  
  names[index, 1] <- "W_FSTR80"
  names[index, 2] <- "FINAL STUDENT REPLICATE BRR-FAY WEIGHT80"
  
  db$WVARSTRR <- substr(data[, 1], start = 2329, stop = 2330)
  index <- which(names(db) == "WVARSTRR")
  
  names[index, 1] <- "WVARSTRR"
  names[index, 2] <- "RANDOMIZED FINAL VARIANCE STRATUM (1-80)"
  
  db$VAR_UNIT <- substr(data[, 1], start = 2331, stop = 2332)
  index <- which(names(db) == "VAR_UNIT")
  
  names[index, 1] <- "VAR_UNIT"
  names[index, 2] <- "RANDOMLY ASSIGNED VARIANCE UNIT"
  
  db$SENWGT_STU <- substr(data[, 1], start = 2333, stop = 2341)
  index <- which(names(db) == "SENWGT_STU")
  
  names[index, 1] <- "SENWGT_STU"
  names[index, 2] <- "Senate weight - sum of weight within the country is 1000"
  
  db$VER_STU <- substr(data[, 1], start = 2342, stop = 2348)
  index <- which(names(db) == "VER_STU")
  
  names[index, 1] <- "VER_STU"
  names[index, 2] <- "Date of the database creation"
  
  return(list(data = db, names = names))
  
}

a <- sortBD(data = dataset)
b <- a$data
c <- a$names

save(a, file = "PISA_2012.RData")
