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
  
  db$ST61Q02 <- substr(data[, 1], start = 226, stop = 226)
  index <- which(names(db) == "ST61Q02")
  
  names[index, 1] <- "ST61Q02"
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
  
  return(list(data = db, names = names))
  
  
}

sortBD(data = dataset)
