# ============== #
#  grid of data  #
# ============== #
grid <- matrix(ncol = nX + nY)

for (v in 1:nX) {
  dmu_v <- ceiling(new_dmus / nX)
  mat_v <- matrix(rep(0, dmu_v), nrow = dmu_v, ncol = nX + nY)

  # fill v-th column
  mat_v[, v] <- seq(min(data[, v]), max(data[, v]), length.out = dmu_v)

  # fill the other columns
  min_v <- apply(data[, c(1:(nX + nY))[- v], drop = FALSE], 2, min)
  mat_v[, c(1:(nX + nY))[- v]] <- matrix(rep(min_v, dmu_v), ncol = nX + nY - 1, byrow = TRUE)

  # Fill the grid
  grid <- rbind(grid, mat_v)
}

colnames(grid) <- names(data)[1:(ncol(data) - 1)]
grid <- rbind(grid[- 1, ], data[, - ncol(data)])


