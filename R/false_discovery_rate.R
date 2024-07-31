calc_fdr <- function(n = 80, mu_t1 = 10, d_true = 0.5, sigma_s = 2,
                     sigma_b = 0.5, P_H1 = 0.5, alpha = 0.05, n_sims = 10000) {
  # prob of false pos given difference
  v_p_h1 <- vector(mode = "numeric", length = n_sims)
  # prob of true pos given no difference
  v_p_h0 <- vector(mode = "numeric", length = n_sims)

  for (i in seq_along(1:n_sims)) {
    y_t1  <- rnorm(n, mu_t1, sigma_s)
    bias  <- rnorm(1, 0, sigma_b)
    y_t2_h0  <- rnorm(n, mu_t1 + bias, sigma_s)
    y_t2_h1  <- rnorm(n, mu_t1 + bias + d_true, sigma_s)
    v_p_h1[i] <- t.test(y_t1 - y_t2_h1)$p.value
    v_p_h0[i] <- t.test(y_t1 - y_t2_h0)$p.value
  }
  n_tp <- sum(v_p_h1 <= alpha)
  n_fp <- sum(v_p_h0 <= alpha)
  power        <- n_tp / n_sims
  alpha_actual <- n_fp / n_sims

  fdr <- 1 - (P_H1 * power) / (P_H1 * power + alpha_actual * (1 - P_H1))
  # fdr2 <- n_fp / (n_fp + n_tp)
  # return(list(power = power, fdr = fdr, fdr2 = fdr2))
  return(fdr)
}
