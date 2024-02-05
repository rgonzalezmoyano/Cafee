library(readxl)
library(dplyr)

data_original <- read_excel("pisa_example/Data PISA2012 ordenado2.xlsx")

# index
colums_ID <- c("CNT", "IDCNT", "UNIT", "SCHOOLID")
colums_out <- c("PVSCIE", "PVMATH", "PVREAD")
colums_inp <- c("ESCS", "EDUSHORT2", "TSRATIO", "SCHLTYPE", "OBS")
# input recomendados: 
#  ESCSmean (media del nivel socioeconómico de los alumnos)
#  STRATIO (ratio profesor por cada 100 alumnos)
#  EDUSHORT (proxy de la calidad de los recursos educativos, está ya transformada para poder ser incluida como input, ya que la variable original reflejaba la escasez de material educativo)

data <- data_original %>% 
  select(colums_ID, colums_out, colums_inp)

names <- names(data_original)

any(names == "TSRATIO")
