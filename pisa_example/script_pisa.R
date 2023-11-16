devtools::document() # actualizar manuales de ayuda
devtools::load_all() # actualizar el codigo

# libraries
library(caret)
library(readxl)

# setting a seed 
set.seed(123)

# ========= #
# Load data #
#========== #
data_original <- read_excel("pisa_example/Data_Spain_PISA_2018.xlsx")

region <- read_excel (
  "pisa_example/Data_Spain_PISA_2018.xlsx", 
  sheet = "codificacion",
  range = "A1:B18")

info_SCHLTYPE <- read_excel (
  "pisa_example/Data_Spain_PISA_2018.xlsx", 
  sheet = "codificacion",
  range = "D1:E4")

# ========== #
# PreProcess #
#=========== #

# remove accents
colnames(region) <- c("Codigo", "Region")

colnames(info_SCHLTYPE) <- c("Codigo", "Tipo_escuela")

# determinate na values
colSums(is.na(data_original))

# remove na DMUs
data <- data_original[complete.cases(data_original), ]
  
# Filter and iterate over the list, extracting DataFrames with dynamic names
datasets_region <- split(data, data$Region)

# 
# for (d in 1:length(datasets_region)) {
#   assign(paste0("df_region_", d), datasets_region[[d]])
# }

# ====================== #
# Machine Learning model #
# ====================== #

# determinate input and output index
x <- c(6, 7, 10) # no SCHLTYPE; no OBS
y <- c(3, 4 , 5)

# efficiency orientation
orientation <- "output"

# metrics for model evaluation
MySummary <- function (data, lev = NULL, model = NULL) {
  
  # accuracy and kappa
  acc_kpp <- defaultSummary(data, lev, model)
  
  # AUC, sensitivity and specificity
  auc_sen_spe <- twoClassSummary(data, lev, model)
  
  # precision and recall
  pre_rec <- prSummary(data, lev, model)
  
  c(acc_kpp, auc_sen_spe, pre_rec)
} 

# Parameters for controlling the training process
trControl <- trainControl (
  method = "cv",
  number = 5,
  summaryFunction = MySummary,
  classProbs = TRUE,
  savePredictions = "all"
)

hold_out <- 0.15

methods <- list (
  "svmLinear" = list(
    "C" = c(0.01, 0.1, 1, 10, 100)
  ),
  "svmRadial" = list (
    "C" = c(0.01, 0.1, 1, 10, 100),
    "sigma" = c(0.001, 0.01, 0.1, 1)
    ),
  "svmPoly" = list(
    "degree" = c(1, 2, 3, 4, 5),
    "scale" = c(0.1, 1, 10),
    "C" = c(0.01, 0.1, 1, 10, 100)
  )
)

# https://topepo.github.io/caret/train-models-by-tag.html

metric = "F1"

# ========= #
# Execution #
# ========= #

# region results
result_region <- vector (
  "list", length(datasets_region)
)

result_region_model <- vector (
  "list", length(datasets_region)
)

for (region in 1:length(datasets_region)) {
  
  # best ML model
  result_region_model[[region]] <- efficiency_estimation (
    data = datasets_region[[region]],
    x = x,
    y = y,
    orientation = orientation,
    trControl = trControl,
    method = methods,
    metric = "F1",
    hold_out = hold_out
  )
    
  # calculate scores
  result_region[[region]] <- compute_scores (
    data = datasets_region[[region]],
    x = x,
    y = y,
    final_model = result_region_model[[region]],
    orientation = orientation
  )
  
  print(region)
  
}

# metafrontier spain result
result_spain <- data.frame (
  DMU = c(1:nrow(data))
)

final_model_spain <- efficiency_estimation (
  data = data,
  x = x,
  y = y,
  orientation = orientation,
  trControl = trControl,
  method = methods,
  metric = "F1",
  hold_out = hold_out
)

result_spain$scores <- compute_scores (
  data = data,
  x = x,
  y = y,
  final_model = final_model_spain,
  orientation = orientation
)

# ============ #
# Save results #
# ============ #

directory <- getwd()

# Nombre de la carpeta en la que deseas guardar el objeto
folder <- paste("/pisa_example/results", sep = "")

new_directory <- paste(directory, folder, sep ="")

setwd(new_directory)

save(result_region, file = "result_region.RData")
save(result_region_model, file = "result_region_model.RData")
save(final_model_spain, file = "final_model_spain.RData")
save(result_spain, file = "result_spain.RData")

setwd(directory)


# ========= #
# DEA - BCC #
# ========= #

# determinate input and output index
x <- c(6, 7, 8, 10) # SI SCHLTYPE; no OBS
y <- c(3, 4 , 5)

# region results
result_region_DEA <- vector (
  "list", length(datasets_region)
)

for (region in 1:length(datasets_region)) {
  
  df <- datasets_region[[region]]
  
  # determinate dmus for technology and for evaluate
  tech_xmat <- as.matrix(df[, x])
  tech_ymat <- as.matrix(df[, y])
  eval_xmat <- as.matrix(df[, x])
  eval_ymat <- as.matrix(df[, y])
  
  result_region_DEA[[region]] <- rad_out (
    tech_xmat = tech_xmat,
    tech_ymat = tech_ymat,
    eval_xmat = eval_xmat,
    eval_ymat = eval_ymat,
    convexity = TRUE,
    returns = "variable"
  )
  
}

# spain results
# determinate dmus for technology and for evaluate
tech_xmat <- as.matrix(data[, x])
tech_ymat <- as.matrix(data[, y])
eval_xmat <- as.matrix(data[, x])
eval_ymat <- as.matrix(data[, y])

result_spain_DEA <- rad_out (
  tech_xmat = tech_xmat,
  tech_ymat = tech_ymat,
  eval_xmat = eval_xmat,
  eval_ymat = eval_ymat,
  convexity = TRUE,
  returns = "variable"
)

comparation_spain <- data.frame(
  DMU = c(1:nrow(data)),
  DEA_BCC = result_spain_DEA,
  cafee = result_spain$scores
)
