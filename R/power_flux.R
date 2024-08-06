#' @import ggplot2
#' @import ggforce
#' @import units
#' @importFrom caTools trapz

# constants
# define the conversion unit between g N and moles of N2O
units::install_unit("mol_n2o", "28 g", "mol wt of N in N2O")

secs_per_day <- 24 * 60 * 60
rho <- set_units(41.742, mol / m^3) # air density at STP, mol m-3

# Function that returns Root Mean Squared Error
rmse <- function(error) {
  sqrt(mean(error^2))
}

get_v_t <- function(x_max, n_gas) {
  v_t <- seq(from = x_max / n_gas, to = x_max, length.out = n_gas)
  return(v_t)
}

get_sigma_t <- function(v_t) {
  sigma_t <- sd(v_t)
  units(sigma_t) <- units(v_t)
  return(sigma_t)
}


# calculates the confidence interval
# the lmits are flux ± ci_flux
#' sigma_t <- sapply(n_gas, get_sigma_t, x_max = t_max)
get_ci_flux <- function(sigma_chi = set_units(25, nmol_n2o / mol),
                        height   = set_units(0.23, m),
                        sigma_t  = set_units(90, s),
                        n_gas = 10) {  # no. samples
  ci_b <- sqrt(sigma_chi^2 / ((n_gas - 1) * sigma_t^2)) * 1.96
  # convert ci_b from mol/mol/s to flux units mol/m2/s - check correct
  ci_flux <- ci_b * rho * height
  return(ci_flux)
}

get_dt_ci <- function(sigma_chi = set_units(25, nmol_n2o / mol),
                      height   = set_units(0.23, m),
                      sigma_t  = set_units(90, s),
                      n_gas = 10,        # no. samples
                      meanlog = 0,
                      sdlog = 1,
                      max_flux = set_units(5, nmol_n2o / m^2 / s)) {
  zero_flux <- max_flux - max_flux # get zero in flux units
  v_flux <- seq(zero_flux, max_flux, by = max_flux / 100)
  v_prob <- drop_units(dlnorm(v_flux, meanlog, sdlog))
  ci_flux <- get_ci_flux(
    sigma_chi = sigma_chi, height = height,
    sigma_t = sigma_t, n_gas = n_gas
  )
  snr <- v_flux^2 / ci_flux^2
  dt <- data.table(
    prob = v_prob, flux = v_flux, ci_flux = ci_flux, snr = snr,
    sigma_chi = sigma_chi, sigma_t = sigma_t, height = height
  )
  return(dt)
}

get_percent_detectable <- function(sigma_chi = set_units(25, nmol_n2o / mol),
                                   height   = set_units(0.23, m),
                                   sigma_t  = set_units(90, s),
                                   n_gas = 10,        # no. samples
                                   meanlog = 0,
                                   sdlog = 1
) {
  ci_flux <- get_ci_flux(
    sigma_chi = sigma_chi, height = height,
    sigma_t = sigma_t, n_gas = n_gas)
  percent_detectable <- (1 - drop_units(plnorm(ci_flux, meanlog, sdlog))) * 100
  return(percent_detectable)
}

get_sigma_spatial <- function(n_samples = 10, n_sims = 1e5, location = 0,
                              scale = 1.5, dist_lognormal = TRUE) {
  n <- rep(n_samples, n_sims)
  # assume same mean for either distribution (cannot have CV with mu = 0)
  mu_true <- exp(location + scale^2 / 2)
  if (dist_lognormal) {
    df <- as.data.frame(sapply(n, function(x) {
      rlnorm(x, location, scale)
    }))
  } else { # assume normal
    df <- as.data.frame(sapply(n, function(x) {
      rnorm(x, mu_true, exp(scale))
    }))
  }
  x <- colMeans(df)
  error <- x - mu_true
  sigma <- sd(error)
  cv <- sigma / mu_true * 100
  return(c(
    mu = mean(x), sigma = sigma, bias = mean(error),
    cv = cv, n_samples = n_samples, dist_lognormal = dist_lognormal
  ))
}

get_omega_ci <- function(
                         sigma_chi = set_units(10, nmol_n2o / mol),
                         height   = set_units(0.23, m),
                         # flux measurement time length, s
                         t_max = set_units(30 * 60, s),
                         n_gas = 4,        # no. gas samples per mmnt
                         #            enter as mol/m2 * 1e9 = nmol/m2
                         N_appl = set_units(0.1489069 * 1e9, nmol_n2o / m^2),
                         omega = 0.01,
                         n_days = 28,
                         n_mmnt_per_day = 10,
                         n_sims = 3,
                         # time length over which measurements are taken,
                         # ususally two weeks to two months but given in secs
                         d_max = set_units(28 * secs_per_day, s),
                         delta = 11.8,
                         k = 0.86,
                         sigma_s = 1.5,
                         rel_error = FALSE,
                         plot_graph = FALSE,
                         plot_histogram = FALSE,
                         get_v_omega = FALSE,
                         trt_diff_obs = 0.005) {

  df_params <- data.frame(omega   = omega,
    N_appl = N_appl,
    sigma_chi = sigma_chi,
    n_days = n_days,
    n_mmnt_per_day = n_mmnt_per_day,
    n_sims = n_sims,
    # time length over which measurements are taken, ~ 25 days but in secs
    d_max = d_max,
    delta = delta,
    k = k,
    sigma_s = sigma_s,
    height = height,
    t_max = t_max, # flux measurement time length, s
    n_gas = n_gas)

  n_mmnts <- n_days * n_mmnt_per_day
  # vector of measurement times
  v_times <- rep(seq(set_units(1, s), d_max, length.out = n_days),
    times = n_mmnt_per_day)
  # vector of true instantaneous mean flux
  # / set_units(1, s) is shortcut to make units correct
  v_F_mean <- drop_units(dlnorm(v_times, delta, k)) * omega * N_appl /
    set_units(1, s)

  # true cumulative flux
  F_cum <- drop_units(plnorm(d_max, delta, k)) * omega * N_appl

  F_mean <- plnorm(d_max, delta, k) * omega * N_appl  / d_max

  # add error from chamber flux mmnt
  v_t <- get_v_t(t_max, n_gas)
  sigma_t <- get_sigma_t(v_t)
  ci_flux <- get_ci_flux(
    sigma_chi = sigma_chi,
    height = height,
    sigma_t = sigma_t,
    n_gas = n_gas
  )
  sd_flux <- ci_flux / 1.96

  # add spatial variation
  # calculate mu_log from arithmetic mean mu
  v_F_meanlog <- drop_units(log(v_F_mean)) - 0.5 * sigma_s^2
  m_F_sample <- sapply(rep(n_mmnts, n_sims), rlnorm, v_F_meanlog, sigma_s)

  # add error from chamber flux mmnt
  m_F_obs <- sapply(1:n_sims, function(x) {
    rnorm(n_mmnts, mean = m_F_sample[, x], sd = sd_flux)
  })

  # get arithmetic mean of simulated flux at each time point
  # df_F_obs_mean <- aggregate(m_F_obs, by = list(v_times), FUN = mean)
  # v_F_cum_obs <- sapply(1 + 1:n_sims, function(x) {
    # trapz(df_F_obs_mean$Group.1, df_F_obs_mean[, x])
  # })

  # v_F_cum_obs <- set_units(v_F_cum_obs, nmol_n2o / m^2)
  # v_omega_obs <- drop_units(v_F_cum_obs / N_appl)
  # v_F_cum_error <- v_F_cum_obs - F_cum

  # if (rel_error) {
    # v_omega_error <- v_omega_obs - mean(v_omega_obs)
  # } else {
    # v_omega_error <- v_omega_obs - omega
  # }
  # not quite zero when sigma_chi and sigma_s are both zero bcos
  # trapz integration doesn't capture peak flux well

  # F_cum_rmsd <- rmse(v_F_cum_error)
  # omega_rmsd <- rmse(v_omega_error)

  if (plot_graph) {
    df <- data.frame(
      time = v_times / secs_per_day, F_mean = v_F_mean,
      F_sample = m_F_sample[, 1], F_obs = m_F_obs[, 1]
    )

    t <- seq(set_units(0, s), d_max, by = d_max / 1000)
    flux <- drop_units(dlnorm(t, delta, k)) * N_appl * omega / set_units(1, s)
    df_hr <- data.frame(t = t / secs_per_day, flux = flux)
    cols <- c("True mean" = "black", "With spatial varn" = "blue",
      "With spatial varn + mmnt noise" = "red")
    p <- ggplot(df, aes(time, F_mean))
    p <- p + scale_colour_manual(name = "", values = cols)
    p <- p + geom_line(aes(colour = "True mean"))
    p <- p + geom_point(aes(y = F_sample, colour = "With spatial varn"))
    p <- p + geom_point(aes(y = F_obs,
      colour = "With spatial varn + mmnt noise"))
    # p <- p + geom_line(data = df_hr, aes(t, flux, colour = "True mean"))
    # p <- p + geom_line(data = df_F_obs_mean, aes(Group.1 / secs_per_day, V1,
      # colour = "With spatial varn + mmnt noise"))
    p <- p + scale_y_unit(unit = "nmol_n2o / m^2 / s", limits = c(-2.5, 15))
    print(p)
  }

  return(n_gas)
}
