# ========= #
# libraries #
# ========= #

devtools::load_all()
library("ggplot2")

# ==== #
# seed #
# ==== #

set.seed(314)

# ========================= #
# function to generate plot #
# ========================= #

# technique: name of the technique

# N: sample size

# std_dev: standard deviation for random inefficiency

# metric: performance metric to evaluate models. Options:
  # Sensitivity
  # Specificity
  # Pos Pred Value
  # Neg Pred Value
  # Precision
  # Recall
  # F1
  # Prevalence
  # Detection Prevalence
  # Balanced Accuracy

# Fixed: cross-validation with 10 folds and 10% of DMUs as hold out data

generate_plot <- function (techique, N, std_dev, metric) {
  
  # ============== #
  # Simulated data #
  # ============== #
  
  data <- reffcy (
    DGP = "add_scenario_XnY1",
    parms = list (
      N = N,
      scenario = "A"
    )
  )
  
  x <- 1
  y <- 2
  
  # compute random error
  random_error <- rnorm(n = N, mean = 0, sd = std_dev)
    
  # compute new vector of outputs
  data[, y] <- data[, y] * exp(random_error)
  
  # ================== #
  # compute DEA scores #
  # ================== #
  
  rad_out <- function (
    tech_xmat, tech_ymat, eval_xmat, eval_ymat, convexity, returns
  ) {
    
    # number of DMUs in the technology
    tech_dmu <- nrow(tech_xmat)
    
    # number of DMUs to be evaluated
    eval_dmu <- nrow(eval_xmat)
    
    # initialize vector of scores
    scores <- matrix(nrow = eval_dmu, ncol = 1)
    
    # number of inputs and outputs
    nX <- ncol(tech_xmat)
    nY <- ncol(tech_ymat)
    
    for (d in 1:eval_dmu) {
      
      objVal <- matrix(ncol = 1 + tech_dmu, nrow = 1)
      objVal[1] <- 1
      
      lps <- make.lp(nrow = 0, ncol = 1 + tech_dmu)
      lp.control(lps, sense = 'max')
      set.objfn(lps, objVal)
      
      # inputs
      for (xi in 1:nX) {
        add.constraint(lps, xt = c(0, tech_xmat[, xi]), "<=",  rhs = eval_xmat[d, xi])
      }
      
      # outputs
      for (yi in 1:nY) {
        add.constraint(lps, xt = c(- eval_ymat[d, yi], tech_ymat[, yi]), ">=", rhs = 0)
      }
      
      # technology
      if (returns == "variable") {
        if (convexity) {
          add.constraint(lprec = lps, xt = c(0, rep(1, tech_dmu)), type = "=", rhs = 1)
        } else {
          add.constraint(lprec = lps, xt = c(0, rep(1, tech_dmu)), type = "=", rhs = 1)
          set.type(lps, columns = 1:tech_dmu + 1, type = c("binary"))
        }
      }
      
      solve(lps)
      scores[d, ] <- get.objective(lps)
    }
    
    return(scores)
  }
  
  tech_xmat <- as.matrix(data[, x])
  tech_ymat <- as.matrix(data[, y])
  eval_xmat <- as.matrix(data[, x])
  eval_ymat <- as.matrix(data[, y])
  
  bcc_scores <- rad_out (
    tech_xmat = tech_xmat,
    tech_ymat = tech_ymat,
    eval_xmat = eval_xmat,
    eval_ymat = eval_ymat,
    convexity = TRUE,
    returns = "variable"
  )
  
  dea_proj <- as.data.frame(data$y * bcc_scores)
  data$dea <- dea_proj$V1
  
  # ======== #
  # ML model #
  # ======== #
  
  # efficiency orientation
  orientation <- "output"
  
  # Parameters for controlling the training process
  trControl <- trainControl (
    method = "cv",
    number = 10,
    summaryFunction = twoClassSummary,
    classProbs = TRUE,
    savePredictions = "all"
  )
  
  hold_out <- 0.1
  
  methods <- list (
    "gbm" = list (
      n.trees = c(50, 100, 150),
      interaction.depth = c(1, 2, 3),
      shrinkage = c(0.01, 0.1, 0.2),
      n.minobsinnode = c(1, 3, 5)
    )
  )
  
  # https://topepo.github.io/caret/train-models-by-tag.html
  
  # Result
  model <- efficiency_estimation (
    data = data,
    x = x,
    y = y,
    orientation = orientation,
    trControl = trControl,
    method = methods,
    metric = metric,
    hold_out = hold_out
  )
  
  # ============= #
  # Generate plot #
  # ============= #
  
  # make a grid of the predictors
  rng.x <- range(data[1])
  
  if (max(data[3] > max(data[2]))) {
    top <- max(data[3])
  } else {
    top <- max(data[2])
  }
  
  if (min(data[3] < min(data[2]))) {
    bottom <- min(data[3])
  } else {
    bottom <- min(data[2])
  }
  
  rng.y <- range(bottom, top)
  
  grid <- expand.grid (
    x1 = seq(rng.x[1], rng.x[2], length = 150),
    y = seq(rng.y[1], rng.y[2], length = 150)
  )
  
  grid$decision <- predict(model, grid, type = "raw")
  
  img <- ggplot(data = data) +
    geom_point(data = grid, aes(x = x1, y = y, color = decision), size = 0.75, alpha = 0.5) +
    geom_line(aes(x = x1, y = yD), linewidth = 1, linetype = "dotted") +
    geom_line(aes(x = x1, y = dea), linewidth = 1) +
    scale_color_manual(values = c("not_efficient" = "pink", "efficient" = "lightgreen")) +
    ggtitle(paste("Frontera gbm", " | ", "e ~ N(0, ", std_dev, ")", sep = "")) +
    theme_bw() +
    theme(
      axis.title.x = element_text (
        size = 12, face = "bold", color = "#921F30",
        margin = margin(t = 10)),
      axis.title.y = element_text (
        size = 12, face = "bold", color = "#921F30",
        margin = margin(r = 10)),
      axis.text = element_text (
        size = 12, color = "black"),
      plot.margin = unit(c(1.25, 1.25, 1.25, 1.25), "lines"),
      plot.title = element_text (
        size = 12, face = "bold", color = "#921F30",
        margin = margin(b = 10)
      ),
      legend.position = "none"
    )
  
  # ========= #
  # Save plot #
  # ========= #
  
  if (N == 25) {
    N <- "025"
  } else if (N == 50) {
    N <- "050"
  } else {
    N <- N
  }
  
  if (std_dev == 0) {
    std_dev <- "0.00"
  } else if (std_dev == 0.1) {
    std_dev <- "0.10"
  } else {
    std_dev <- std_dev
  }
  
  file <- paste(technique, "_", N, "_", std_dev, ".png", sep = "")
  
  ggsave(file = file, plot = img, dpi = 600, width = 10, heigh = 6)
}

# Generate plots

technique <- "gbm"
N <- c(25, 50, 100, 200, 500)
std_dev <- c(0, 0.005, 0.01)
metric <- "F1"

for (n in N) {
  for (std in std_dev) {
    generate_plot(techique = techique, N = n, std_dev = std, metric = metric)
  }
}
