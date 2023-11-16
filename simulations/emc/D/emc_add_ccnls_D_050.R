library(dplyr)
library(Rglpk)
library(tictoc)
library(lpSolveAPI)
library(quadprog)

source("/home/PI/vespana/aces/R/ACES.R")
source("/home/PI/vespana/aces/R/backward_algorithm.R")
source("/home/PI/vespana/aces/R/C2NLS.R")
source("/home/PI/vespana/aces/R/efficiency_scores.R")
source("/home/PI/vespana/aces/R/estimate_coefficients.R")
source("/home/PI/vespana/aces/R/forward_algorithm.R")
source("/home/PI/vespana/aces/R/predictions.R")
source("/home/PI/vespana/aces/R/simulations.R")
source("/home/PI/vespana/aces/R/smoothing_algorithm.R")

repl <- 100

simulaciones <- data.frame(
  id = rep(NA, repl),
  scenario = rep(NA, repl),
  monotonicity = rep(TRUE, repl),
  concavity = rep(TRUE, repl),
  N = rep(NA, repl),
  metric = rep(NA, repl),
  degree = rep(NA, repl),
  hd_cost = rep(NA, repl),
  err_red = rep(NA, repl),
  d = rep(NA, repl),
  turbo = rep(NA, repl),
  time = rep(NA, repl),
  mse_aces_forward = rep(NA, repl),
  bias_aces_forward = rep(NA, repl),
  bias_absolute_aces_forward = rep(NA, repl),
  mse_aces = rep(NA, repl),
  bias_aces = rep(NA, repl),
  bias_absolute_aces = rep(NA, repl),
  mse_aces_cubic = rep(NA, repl),
  bias_aces_cubic = rep(NA, repl),
  bias_absolute_aces_cubic = rep(NA, repl),
  mse_aces_quintic = rep(NA, repl),
  bias_aces_quintic = rep(NA, repl),
  bias_absolute_aces_quintic = rep(NA, repl),
  mse_dea = rep(NA, repl),
  bias_dea = rep(NA, repl),
  bias_absolute_dea = rep(NA, repl),
  time_dea = rep(NA, repl),
  mse_ccnls = rep(NA, repl),
  bias_ccnls = rep(NA, repl),
  bias_absolute_ccnls = rep(NA, repl),
  time_ccnls = rep(NA, repl)
  )

set.seed(314)

for (s in c("D")) {
  for (N in c(50)) {
    for(id in 1:100) {

      simulaciones[id, c(1, 2, 5)] <- c(id, s, N)

      while (TRUE) {

        # ==
        # Data Generation
        # ==

        data <- reffcy (
          DGP = "add_scenario_XnY1",
          parms = list (
            N = N,
            scenario = s
          ))

        if (nrow(data) < N) next

        # input and output indexes
        if (s %in% c("A", "B")) {
          x <- 1
          y <- 2
        } else if (s %in% c("C", "E")) {
          x <- 1:2
          y <- 3
        } else {
          x <- 1:3
          y <- 4
        }

        # ==
        # DEA
        # ==

        tic()

        dea <- rad_out (
          tech_xmat = as.matrix(data[, x]),
          tech_ymat = as.matrix(data[, y]),
          eval_xmat = as.matrix(data[, x]),
          eval_ymat = as.matrix(data[, y]),
          convexity = TRUE,
          returns = "variable"
        ) * data[, y]

        time <- toc()

        dif <- dea - data[, "yD"]

        # mse | bias | bias absolute
        simulaciones[id, 25] <- round(mean(dif ^ 2), 3)
        simulaciones[id, 26] <- round(mean(dif), 3)
        simulaciones[id, 27] <- round(mean(abs(dif)), 3)
        simulaciones[id, 28] <- unname(time$toc - time$tic)

        if (round(sum(dif ^ 2) / N, 3) < 1000) break
      }

      print(paste(" ### !!! -- > Réplica número", id, "/", 100, "<-- ¡¡¡ ###"))

      # ==
      # CCNLS
      # ==

      tic()

      CCNLS <- C2NLS (
        data, x, y
        )

      CCNLS <- predict(CCNLS, data, x)$f

      time <- toc()

      dif <- CCNLS - data[, "yD"]

      # mse | bias | bias absolute
      simulaciones[id, 29] <- round(mean(dif ^ 2), 3)
      simulaciones[id, 30] <- round(mean(dif), 3)
      simulaciones[id, 31] <- round(mean(abs(dif)), 3)
      simulaciones[id, 32] <- unname(time$toc - time$tic)

      # ==
      # ACES
      # ==

      if (s %in% c("A", "B")) {
        degree  <- 1
        hd_cost <- 1
      } else if (s %in% c("C", "E")) {
        degree  <- 2
        hd_cost <- c(0.15, 0.3, 0.5)
      } else {
        degree  <- 3
        hd_cost <- c(0.15, 0.3, 0.5)
      }

      grid <- expand.grid (
        err_red = c(0),
        d = c(1, 2, 3),
        hd_cost = hd_cost,
        degree = degree,
        turbo = c(Inf),
        metric = c("mse")
      )

      # mse for k-fold cross-validation
      grid$mse1 <- rep(NA, nrow(grid))
      grid$mse2 <- rep(NA, nrow(grid))
      grid$mse3 <- rep(NA, nrow(grid))

      # times for k-fold cross-validation
      grid$time1 <- rep(NA, nrow(grid))
      grid$time2 <- rep(NA, nrow(grid))
      grid$time3 <- rep(NA, nrow(grid))

      # mean error in k-fold cross-validation
      grid$mse <- rep(NA, nrow(grid))

      # mean time in k-fold cross-validation
      grid$time <- rep(NA, nrow(grid))

      # ================ #
      # Cross validation #
      # ================ #

      data_shuffle <- data[sample(1:nrow(data)), ]

      # number of folds
      kfold <- 3
      # size of the fold
      ksize <- floor(N / 3)
      # list for each fold
      kindex <- vector("list", kfold)

      for (k in 1:kfold) {
        kindex[[k]] <- ((k - 1) * ksize + 1):(k * ksize)
      }

      # last fold
      kindex[[kfold]] <- (((kfold - 1) * ksize) + 1):N

      for (k in 1:kfold) {

        # training data
        train <- data_shuffle[- kindex[[k]], ]
        row.names(train) <- 1:nrow(train)

        # test data
        test <- data_shuffle[kindex[[k]] , ]

        for (i in 1:nrow(grid)) {

          tic()

          model <- aces (
            data = train,
            x = x,
            y = y,
            y_type = "individual",
            degree = grid[i, "degree"],
            metric = "mse",
            turbo = grid[i, "turbo"],
            monotonicity = T,
            concavity = T,
            nterms = 50,
            err_red = grid[i, "err_red"],
            hd_cost = grid[i, "hd_cost"],
            minspan = - 1,
            endspan = - 1,
            knots_grid = - 1,
            d = grid[i, "d"],
            wc = seq(1, 2, 0.2),
            wq = seq(8 / 7, 1.5, length.out = 5)
            )

          time <- toc()

          # predictions
          mse <- c()
          for (m in c("aces", "aces_cubic", "aces_quintic")) {
            y_hat_test <- predict(model, test, x, m)
            dif_test <- y_hat_test - test[, "y"]
            mse[m] <- round(sum(dif_test ^ 2) / nrow(dif_test), 3)
          }

          grid[i, 6 + k] <- min(mse)
          grid[i, 9 + k] <- unname(time$toc - time$tic)
        }
    }

    # mean error in k-fold cross-validation
    grid$mse <- apply(grid[, 07:09], 1, mean)

    # mean time in k-fold cross-validation
    grid$time <- apply(grid[, 10:12], 1, mean)

    # Best set of hyperparameters
    best_hyp <- grid %>%
      top_n(- 1, mse) %>%
      top_n(- 1, time) %>%
      sample_n(1)

    simulaciones[id, 06] <- "mse"
    simulaciones[id, 07] <- best_hyp[, "degree"]
    simulaciones[id, 08] <- best_hyp[, "hd_cost"]
    simulaciones[id, 09] <- best_hyp[, "err_red"]
    simulaciones[id, 10] <- best_hyp[, "d"]
    simulaciones[id, 11] <- "Inf"

    # Model with all the observations
    tic()

    model <- aces (
      data = data,
      x = x,
      y = y,
      y_type = "individual",
      degree = best_hyp[, "degree"],
      metric = "mse",
      turbo = Inf,
      monotonicity = T,
      concavity = T,
      nterms = 50,
      err_red = best_hyp[, "err_red"],
      hd_cost = best_hyp[, "hd_cost"],
      minspan = - 1,
      endspan = - 1,
      knots_grid = - 1,
      d = best_hyp[, "d"],
      wc = seq(1, 2, 0.2),
      wq = seq(8 / 7, 1.5, length.out = 5)
    )

    time <- toc()

    # time
    simulaciones[id, 12] <- unname(time$toc - time$tic)

    # ==
    # aces predictions
    # ==

    cols <- c(12, 15, 18, 21)
       j <- 0

    for (m in c("aces_forward", "aces", "aces_cubic", "aces_quintic")) {

      j <- j + 1

      y_hat <- aces_scores (
        tech_data = data,
        eval_data = data,
        x = x,
        y = y,
        object = model,
        method = m,
        measure = "rad_out",
        convexity = TRUE,
        returns = "variable",
        direction = NULL,
        weights = NULL,
        digits = 3
      ) * data[, y]

      dif <- y_hat[, 1] - data[, "yD"]

      # mse | bias | bias absolute
      simulaciones[id, cols[j] + 1] <- round(mean(dif ^ 2), 3)
      simulaciones[id, cols[j] + 2] <- round(mean(dif), 3)
      simulaciones[id, cols[j] + 3] <- round(mean(abs(dif)), 3)

      }
    }
  }
}

if (N == 50) {
  N <- "050"
} else {
  N <- N
}

file <- paste("emc_add_ccnls_", s, "_", N, ".RData", sep = "")
save(simulaciones, file = file)
