Multiplicative_RNG <- function(a, z0, m, n) {
  xj <- matrix(NA, n, 3)
  colnames(xj) <- c("aZ", "Xj", "Uj")
  for (j in 1:n) {
    xj[j, 1] <- (a * z0)
    xj[j, 2] <- xj[j, 1] %% m
    xj[j, 3] <- xj[j, 2] / m
    z0 <- xj[j, 2]
  }
  hist(xj[, 3])
  return(xj)
}

Binom <- function(x, p) {
  if (x < p) {
    return(0)
  } else {
    return(1)
  }
}

Gabungan_RNG_ber <- function(a, z0, m, n, p) {
  xj <- matrix(NA, n, 3)
  dj <- matrix(NA, n, 3)
  colnames(xj) <- c("aZ", "Xj", "Uj")  
  colnames(dj) <- c("Uj", "0", "1")
  for (j in 1:n) {
    xj[j, 1] <- (a * z0)
    xj[j, 2] <- xj[j, 1] %% m
    xj[j, 3] <- xj[j, 2] / m
    z0 <- xj[j, 2]
    dj[j, 1] <- xj[j, 3]
    dj[j, 2:3] <- Binom(xj[j, 2], p)
  }
  hist(xj[, 3])
  View(xj)
  View(dj)
  return(dj)
}

set.seed(123) # set seed agar hasil random dapat direproduksi
xj <- Multiplicative_RNG(45, 21139, 417, 150)
dj <- Gabungan_RNG_ber(45, 21139, 417, 150, 0.5)
Binom <- as.numeric(di[, 3])
tabel <- table(Binom) / length(Binom)
View(tabel)
runif(di[, 1]) # melakukan pemanggilan fungsi runif dengan menggunakan nilai dari kolom "Uj"