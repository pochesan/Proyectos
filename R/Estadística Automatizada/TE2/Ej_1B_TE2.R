
# Definicion de parametros
theta <- 2
n <- 20
B <- 10000

#### a. Monto Carlo ####
set.seed(123)

# Realizamos B muestras de aleatorias de tamaño n de una distribucion Poisson(theta)
sample_mc <- matrix(rpois(n*B, lambda = theta), ncol = B)

# Estimamos tau para cada una de las B muestras aleatorias
sample_mc_tau <- apply(sample_mc, 2, function(x, n) {((n-1)/n) ^ sum(x)}, n)

# Estimamos E[tau_hat] y V(tau_hat)
(E_tau_hat_mc <- mean(sample_mc_tau))
(V_tau_hat_mc <- var(sample_mc_tau))

# Realizamos un histograma de las B estimaciones
x11()
hist(sample_mc_tau, breaks = 20, main = "Histograma de tau_hat con el método de Monte Carlo")
abline(v = E_tau_hat_mc, col = "red")
legend("topright", legend = c(paste("E_tau_hat =", round(E_tau_hat_mc, 4)),
                              paste("V_tau_hat =", round(V_tau_hat_mc, 4))))


#### b. Bootstrap ####
# Tamaño de la muestra
n_b <- 20

# Muestra sobre la que vamos a realizar el bootstrap
sample_S <- rpois(n_b, lambda = theta)

# Realizamos B remuestreos
sample_b <- replicate(B, sample(sample_S, n_b, replace = TRUE))

# Estimamos tau para cada una de las B muestras aleatorias
sample_b_tau <- apply(sample_b, 2, function(x, n) {((n-1)/n) ^ sum(x)}, n)

# Estimamos E[tau_hat] y V(tau_hat)
(E_tau_hat_b <- mean(sample_b_tau))
(V_tau_hat_b <- var(sample_b_tau))

# Realizamos un histograma de las B estimaciones
x11()
hist(sample_b_tau, breaks = 20, main = "Histograma de tau_hat con el método de Bootstrap")
abline(v = E_tau_hat_b, col = "red")
legend("topright", legend = c(paste("E_tau_hat =", round(E_tau_hat_b, 4)),
                              paste("V_tau_hat =", round(V_tau_hat_b, 4))))


dpois(0, lambda = 2)
