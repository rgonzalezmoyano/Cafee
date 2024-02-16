library(readxl)
library(dplyr)

data_original <- read_excel("pisa_2012_JA.xlsx")

load("C:/Users/Ricardo/Downloads/PISA_2012.RData")
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



### Function JOIN DATA

# ========= #
# Libreries #
# ========= #
library(readxl)
library(dplyr)

# ========= #
# Load data #
# ========= #
load("C:/Users/Ricardo/Downloads/PISA_2012.RData")
data_PISA2012 <- a$data

# ============ #
# Puntuaciones #
# ============ #
name_PV <- grep("^PV", names(data_PISA2012), value = TRUE)

data_PV <- data_PISA2012[name_PV]

# ID
ID <- data_PISA2012[, 1:7]

# join ID with data_PV
data_PV <- cbind(ID, data_PV)

# how many contries are there 
num_CNT <- length(unique(data_PV$CNT))
names_CNT <- unique(data_PV$CNT)

math <- grep("MATH", names(data_PISA2012), value = TRUE)[3:7]
read <- grep("READ", names(data_PISA2012), value = TRUE)
scie <- grep("SCIE", names(data_PISA2012), value = TRUE)

# change name; only PV values of math, read and science
data_PV <- data_PISA2012[c(math, read, scie)]
data_PV <- as.data.frame(apply(data_PV, 2, as.numeric)) 

data_PV$PVMATH <- rowMeans(data_PV[math])
data_PV$PVREAD <- rowMeans(data_PV[read])
data_PV$PVSCIE <- rowMeans(data_PV[scie])

data_PV <- cbind(ID, data_PV[c("PVMATH", "PVREAD", "PVSCIE")])

data_PV$SCHOOLID <- as.numeric(data_PV$SCHOOLID)
#data_PV[1:7] <- apply(data_PV[1:7], 2, as.factor) 

dataframe <- as.data.frame(
  matrix(
    data = NA,
    nrow = 0,
    ncol = 11
  )
)

for (i in 1:num_CNT) {
  
  # filter data
  data_PV_CNT <- data_PV %>%
    filter(CNT == names_CNT[i])
  
  data_PV_CNT_mean <- data_PV_CNT %>% 
    group_by(SCHOOLID) %>%
    summarise(
      mean_PVMATH = mean(PVMATH),
      mean_PVREAD = mean(PVREAD),
      mean_SCIE = mean(PVSCIE)
    )
  
  name_CNT <- as.data.frame(
    matrix(
      data = unique(data_PV_CNT$CNT),
      nrow = nrow(data_PV_CNT_mean),
      ncol = 1
    )
  )
  
  names(name_CNT) <- c("CNT")
  
  data_PV_CNT_mean <- cbind(name_CNT, data_PV_CNT_mean)
  
  dataframe <- rbind(dataframe, data_PV_CNT_mean)
}

dataframe[, 3:ncol(dataframe)] <- round(dataframe[, 3:ncol(dataframe)], digits = 2)

# ============ #
# Data Cordero #
# ============ #
data_original <- read_excel("pisa/data_Juan_Aparicio/pisa_2012_JA.xlsx")

# make ID connection
data_original$id_join <- paste(data_original$CNT, data_original$PVMATH, data_original$PVREAD, data_original$PVSCIE)
dataframe$id_join <- paste(dataframe$CNT, dataframe$mean_PVMATH, dataframe$mean_PVREAD, dataframe$mean_SCIE)

data_original$id_join[1] == dataframe$id_join[889]

# ========= #
# join data #
# ========= #

data_final <- inner_join(data_original, dataframe, by = "id_join")

nrow(data_original)
nrow(dataframe)
nrow(dataframe) - nrow(data_original)

