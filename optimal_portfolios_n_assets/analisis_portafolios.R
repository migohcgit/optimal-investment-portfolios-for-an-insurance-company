# Modelos de Markowitz para N activos

#Esta es la configuracion
DATA_DIR <- "data"
OUTPUT_DIR <- "output"
FREQUENCY <- "monthly"       # "daily", "weekly", "monthly" o "annual" esto ya se decide
RISK_FREE_ANNUAL <- 0.0628    # CETES 28 dias % modificar si se ocupa
N_RANDOM_PORTFOLIOS <- 20000 
SEED <- 123
ALLOW_SHORT_SELLING <- FALSE # for now just long positions

if (!requireNamespace("quadprog", quietly = TRUE)) {
  stop("'quadprog' Missing. Install with install.packages('quadprog') and try again.")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("'ggplot2' Missing. Install with install.packages('ggplot2') and try again.")
}

if (!dir.exists(DATA_DIR)) stop("can't find 'data' directory. Execute the script from project's root.")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

periods_per_year <- switch(FREQUENCY, daily = 252, weekly = 52, monthly = 12, annual = 1,
                           stop("FREQUENCY must be daily, weekly, monthly o annual."))
risk_free_period <- (1 + RISK_FREE_ANNUAL)^(1 / periods_per_year) - 1

# Reads every CSV file in data/ that contains Date y Price.
# Every file represents an asset; file name is its ticker/name.
files <- list.files(DATA_DIR, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
if (length(files) < 2) stop("Include at least two CSV assets files in data/.")

read_asset <- function(path) {
  raw <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  names(raw) <- trimws(names(raw))
  if (!all(c("Date", "Price") %in% names(raw))) {
    stop(sprintf("%s file must include Date y Price named columns", basename(path)))
  }
  # Example data uses mm/dd/yyyy date type. Price must be a number
  date <- as.Date(raw$Date, format = "%m/%d/%Y")
  price <- suppressWarnings(as.numeric(gsub(",", "", raw$Price, fixed = TRUE)))
  valid <- !is.na(date) & is.finite(price) & price > 0
  out <- data.frame(Date = date[valid], Price = price[valid])
  out <- out[order(out$Date), ]
  out <- out[!duplicated(out$Date, fromLast = TRUE), ]
  if (nrow(out) < 3) stop(sprintf("%s not contains enough valid observations.", basename(path)))
  names(out)[2] <- tools::file_path_sans_ext(basename(path))
  out
}

assets <- lapply(files, read_asset)
names(assets) <- vapply(files, function(x) tools::file_path_sans_ext(basename(x)), character(1))

# this part aligns by date and ensures no merge non coincidence dates in the rows
prices <- Reduce(function(x, y) merge(x, y, by = "Date", all = FALSE), assets)
prices <- prices[order(prices$Date), ]
asset_names <- setdiff(names(prices), "Date")
if (nrow(prices) < 3) stop("Assets don't have enough common dates")

# Simple return, chronological order 
price_matrix <- as.matrix(prices[, asset_names, drop = FALSE])
returns <- price_matrix[-1, , drop = FALSE] / price_matrix[-nrow(price_matrix), , drop = FALSE] - 1
return_dates <- prices$Date[-1]
colnames(returns) <- asset_names
rownames(returns) <- as.character(return_dates)
if (any(!is.finite(returns))) stop("Non finites returns, review for price and dates, may a character in a number variable")

# Expected return vector and covariance matrix
mu <- colMeans(returns)
Sigma <- stats::cov(returns)
# symmetrize by numerical stability and check for a positive definite matrix.
Sigma <- (Sigma + t(Sigma)) / 2
if (any(!is.finite(Sigma))) stop("Can't calculate covariance matrix. Finite or character error")

write.csv(data.frame(Date = return_dates, returns, check.names = FALSE),
          file.path(OUTPUT_DIR, "rendimientos.csv"), row.names = FALSE)
write.csv(data.frame(Activo = names(mu), Rendimiento_esperado_periodico = as.numeric(mu),
                     Volatilidad_periodica = sqrt(diag(Sigma))),
          file.path(OUTPUT_DIR, "estadisticas_activos.csv"), row.names = FALSE)
write.csv(data.frame(Activo = rownames(Sigma), Sigma, check.names = FALSE),
          file.path(OUTPUT_DIR, "matriz_covarianzas.csv"), row.names = FALSE)
write.csv(data.frame(Activo = rownames(cor(returns)), cor(returns), check.names = FALSE),
          file.path(OUTPUT_DIR, "matriz_correlaciones.csv"), row.names = FALSE)

# Random portfolios (only for comparation)
set.seed(SEED)
n_assets <- length(asset_names)
raw_weights <- matrix(rexp(N_RANDOM_PORTFOLIOS * n_assets), ncol = n_assets)
weights_random <- raw_weights / rowSums(raw_weights)
colnames(weights_random) <- asset_names

portfolio_return <- as.vector(weights_random %*% mu)
portfolio_variance <- rowSums((weights_random %*% Sigma) * weights_random)
portfolio_sd <- sqrt(pmax(portfolio_variance, 0))
sharpe <- ifelse(portfolio_sd > 0, (portfolio_return - risk_free_period) / portfolio_sd, NA_real_)

portfolios <- data.frame(
  id = seq_len(N_RANDOM_PORTFOLIOS),
  rendimiento_esperado = portfolio_return,
  varianza = portfolio_variance,
  volatilidad = portfolio_sd,
  sharpe = sharpe,
  weights_random,
  check.names = FALSE
)

#optimization
solve_min_variance <- function(S) {
  n <- ncol(S)
  D <- S + diag(1e-10, n) # number regularization
  Amat <- cbind(rep(1, n), diag(n))
  bvec <- c(1, rep(0, n))
  fit <- quadprog::solve.QP(Dmat = D, dvec = rep(0, n), Amat = Amat,
                            bvec = bvec, meq = 1)
  w <- as.numeric(fit$solution)
  w[w < 1e-10] <- 0
  w / sum(w)
}

w_min <- solve_min_variance(Sigma)
ret_min <- sum(w_min * mu)
sd_min <- sqrt(as.numeric(t(w_min) %*% Sigma %*% w_min))

# Max sharp ratio porfolio and lower risk
best_idx <- which.max(portfolios$sharpe)
w_sharpe <- as.numeric(weights_random[best_idx, ])
if (is.finite((ret_min - risk_free_period) / sd_min) &&
    (ret_min - risk_free_period) / sd_min > portfolios$sharpe[best_idx]) {
  w_sharpe <- w_min
}

special_weights <- rbind(Minima_varianza = w_min, Maximo_Sharpe_simulado = w_sharpe)
colnames(special_weights) <- asset_names
special_return <- as.vector(special_weights %*% mu)
special_variance <- rowSums((special_weights %*% Sigma) * special_weights)
special_sd <- sqrt(pmax(special_variance, 0))
special_sharpe <- ifelse(special_sd > 0, (special_return - risk_free_period) / special_sd, NA_real_)
special <- data.frame(
  portafolio = rownames(special_weights),
  rendimiento_esperado = special_return,
  varianza = special_variance,
  volatilidad = special_sd,
  sharpe = special_sharpe,
  special_weights,
  row.names = NULL,
  check.names = FALSE
)

# Efficient Frontier: best sharp ratio
# keep the lower variance portfolio as a reference
frontier <- portfolios[order(portfolios$volatilidad), ]
frontier <- frontier[!duplicated(round(frontier$volatilidad, 6)), ]
frontier <- frontier[order(frontier$volatilidad), ]
frontier <- frontier[!duplicated(cummax(frontier$rendimiento_esperado)), ]

write.csv(portfolios, file.path(OUTPUT_DIR, "portafolios_simulados.csv"), row.names = FALSE)
write.csv(special, file.path(OUTPUT_DIR, "portafolios_optimos.csv"), row.names = FALSE)
weights_output <- data.frame(
  Activo = rownames(t(special_weights)),
  t(special_weights),
  check.names = FALSE,
  row.names = NULL
)

write.csv(
  weights_output,
  file.path(OUTPUT_DIR, "pesos_optimos.csv"),
  row.names = FALSE
)
write.csv(frontier[, c("rendimiento_esperado", "volatilidad", "sharpe")],
          file.path(OUTPUT_DIR, "frontera_eficiente_aproximada.csv"), row.names = FALSE)

# this is the graph
cat("Número de portafolios:", nrow(portfolios), "\n")
cat("Número de puntos de la frontera:", nrow(frontier), "\n")
cat("Número de portafolios especiales:", nrow(special), "\n")

print(head(portfolios))

p <- ggplot2::ggplot(portfolios, ggplot2::aes(x = volatilidad, y = rendimiento_esperado)) +
  ggplot2::geom_point(ggplot2::aes(color = sharpe), alpha = 0.45, size = 1.2) +
  ggplot2::geom_line(data = frontier,
                     ggplot2::aes(x = volatilidad, y = rendimiento_esperado),
                     inherit.aes = FALSE, linewidth = 0.8, color = "#007F7F") +
  ggplot2::geom_point(data = special,
                      ggplot2::aes(x = volatilidad, y = rendimiento_esperado, shape = portafolio),
                      inherit.aes = FALSE, size = 3.2, color = "#B23A48") +
  ggplot2::labs(title = "Portafolios de Markowitz para N activos",
                subtitle = sprintf("%d activos | frecuencia %s | tasa libre de riesgo anual %.2f%%",
                                   n_assets, FREQUENCY, RISK_FREE_ANNUAL * 100),
                x = "Volatilidad por periodo", y = "Rendimiento esperado por periodo",
                color = "Sharpe", shape = "Portafolio") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(OUTPUT_DIR, "frontera_eficiente.png"), p, width = 10, height = 6, dpi = 160)

cat("\nAnalisis terminado.\n")
cat("Activos:", paste(asset_names, collapse = ", "), "\n")
cat("Observaciones de rendimiento:", nrow(returns), "\n")
cat("Frecuencia:", FREQUENCY, "| tasa libre de riesgo por periodo:", round(risk_free_period, 6), "\n\n")
print(special)
cat("\nArchivos generados en:", normalizePath(OUTPUT_DIR), "\n")

