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

# Cordero
data_JA <- data_original[data_original$CNT == "AUS", ]
data_JA <- data_JA[merge_names]
data_JA <- data_JA[, -1] 
data_JA

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

# puntuaciones
puntuaciones_PISA <- grep("^PV", names(dataframe), value = TRUE)

data_result <- dataframe[puntuaciones_PISA]
ID <- dataframe[, 1:7]
data_result <- cbind(ID, data_result)

data_result <- data_result[data_result$CNT == "AUS", ]

math <- grep("MATH", names(dataframe), value = TRUE)[3:7]
math_df <- data_result[math]

math_df <- mutate_all(math_df, as.numeric)

data_result$PVMATH <- rowMeans(math_df) 
prueba_coin_pvmath <- cbind(ID[ID$CNT == "AUS",], data_result$PVMATH)

prueba_coin_pvmath <- prueba_coin_pvmath[, c(6,8)]

names(prueba_coin_pvmath) <- c("SCHOOLID", "PVMATH")

math_coin <- prueba_coin_pvmath %>% 
  group_by(SCHOOLID) %>% 
  summarise(
    mean_PVMATH = mean(PVMATH)
  )
math_coin
