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
  
  db$EC07Q01 <- substr(data[, 1], start = 483, stop = 483)
  index <- which(names(db) == "EC07Q01")
  
  names[index, 1] <- "EC07Q01"
  names[index, 2] <- "Language spoken - Mother"
  
  
  return(list(data = db, names = names))
  
  
}

sortBD(data = dataset)
