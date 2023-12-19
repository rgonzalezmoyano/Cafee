### libraries
library(readr)

# INT_STU12_DEC03 <- read_table (
#   "data/1_student_questionannaire_data_file/INT_STU12_DEC03.txt",
#   col_names = FALSE)

#save(INT_STU12_DEC03, file = "data/1_student_questionannaire_data_file/INT_STU12_DEC03.RData")

load("C:/Users/Ricardo/Desktop/cafee/pisa/data/1_student_questionannaire_data_file/INT_STU12_DEC03.RData")

data <- INT_STU12_DEC03

primera_fila <- data[1,]
any(is.na(primera_fila))
names(primera_fila)[1] <- "ID"

# find if each row has 184 columns
in.which()
ncol()

