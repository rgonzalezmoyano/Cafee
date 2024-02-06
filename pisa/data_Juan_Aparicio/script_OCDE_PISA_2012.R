library(readxl)
library(dplyr)

data_original <- read_excel("pisa/data_Juan_Aparicio/pisa_2012_JA.xlsx")

dataframe <- a$data
names <- a$names

to_search <- names(data_original)

names_to_search <- names(dataframe)

indices <- which(names_to_search %in% to_search)

merge_names <- names_to_search[indices]

# mismas variables
data <- dataframe[merge_names]

copy_data <- data


# try to find agrupation
library(dplyr)

# database PISA
# Pruebas con AUS
data_new <- data[data$CNT == "AUS", ]
data_new <- data_new[, -1]

data_new <- data_new %>% 
  group_by(SCHOOLID) %>% 
  summarise(
    mean_DISCLIMA = mean(as.numeric(DISCLIMA)),
    mean_ESCS = mean(as.numeric(ESCS)),
    mean_PERSEV = mean(as.numeric(PERSEV))
    )

# Juan Aparicio
data_JA <- data_original[data_original$CNT == "AUS", ]
data_JA <- data_JA[merge_names]
data_JA <- data_JA[, -1]  
data_new$SCHOOLID <- as.numeric(data_new$SCHOOLID)
data_new
prueba <- data_original[data_original$CNT == "AUS", ]
  
  
data_JA_new <- data_JA %>% 
  group_by(SCHOOLID) %>% 
  summarise(
    mean_DISCLIMA = mean(as.numeric(DISCLIMA)),
    mean_ESCS = mean(as.numeric(ESCS)),
    mean_PERSEV = mean(as.numeric(PERSEV))
  )
data_JA_new

head(data_JA)
head(data_new)
