#' GOF
#'
#' Good of fitting
#'
#' @param obs Numeric vector, observations
#' @param sim Numeric vector, corresponding simulated values
#' @param w Numeric vector, weights of every points. If w included, when
#' calculating mean, Bias, MAE, RMSE and NSE, w will be taken into considered.
#' @param include.cv If true, cv will be included.
#' @param include.r If true, r and R2 will be included.
#' 
#' @return
#' * `RMSE` root mean square error
#' * `NSE` NASH coefficient
#' * `MAE` mean absolute error
#' * `AI` Agreement index (only good points (w == 1)) participate to
#' calculate. See details in Zhang et al., (2015).
#' * `Bias` bias
#' * `Bias_perc` bias percentage
#' * `n_sim` number of valid obs
#' * `cv` Coefficient of variation
#' * `R2` correlation of determination
#' * `R` pearson correlation
#' * `pvalue` pvalue of `R`
#'
#' @references
#' 1. https://en.wikipedia.org/wiki/Coefficient_of_determination
#' 2. https://en.wikipedia.org/wiki/Explained_sum_of_squares
#' 3. https://en.wikipedia.org/wiki/Nash%E2%80%93Sutcliffe_model_efficiency_coefficient
#' 4. Zhang Xiaoyang (2015), http://dx.doi.org/10.1016/j.rse.2014.10.012
#'
#' @examples
#' obs = rnorm(100)
#' sim = obs + rnorm(100)/4
#' GOF(obs, sim)
#' 
#' @importFrom dplyr tibble
#' @export
GOF <- function(obs, sim, w, include.cv = FALSE, include.r = TRUE){
    if (missing(w)) w <- rep(1, length(obs))

    # remove NA_real_ and Inf values in sim, obs and w
    valid <- function(x) !is.na(x) & is.finite(x)

    I <- which(valid(sim) & valid(obs) & valid(w))
    # n_obs <- length(obs)
    n_sim <- length(I)

    sim <- sim[I]
    obs <- obs[I]
    w     <- w[I]

    if (include.cv) {
        CV_obs <- cv_coef(obs, w)
        CV_sim <- cv_coef(sim, w)
    }
    if (is_empty(obs)){
        out <- c(RMSE = NA_real_, 
            KGE = NA_real_,
            NSE = NA_real_, MAE = NA_real_, AI = NA_real_,
            Bias = NA_real_, Bias_perc = NA_real_, n_sim = NA_real_)

        if (include.r) out <- c(out, R2 = NA_real_, R = NA_real_, pvalue = NA_real_)
        if (include.cv) out <- c(out, obs = CV_obs, sim = CV_sim)
        return(out)
    }

    # R2: the portion of regression explained variance, also known as
    # coefficient of determination
    KGE = KGE(sim, obs)
    # https://en.wikipedia.org/wiki/Coefficient_of_determination
    # https://en.wikipedia.org/wiki/Explained_sum_of_squares
    y_mean <- sum(obs * w) / sum(w)

    SSR    <- sum( (sim - y_mean)^2 * w)
    SST    <- sum( (obs - y_mean)^2 * w)
    # R2     <- SSR / SST

    RE     <- sim - obs
    Bias   <- sum ( w*RE)     /sum(w)                     # bias
    Bias_perc <- Bias/y_mean                              # bias percentage
    MAE    <- sum ( w*abs(RE))/sum(w)                     # mean absolute error
    RMSE   <- sqrt( sum(w*(RE)^2)/sum(w) )                # root mean sqrt error

    # https://en.wikipedia.org/wiki/Nash%E2%80%93Sutcliffe_model_efficiency_coefficient
    NSE  <- 1  - sum( (RE)^2 * w) / SST # NSE coefficient

    # Observations number are not same, so comparing correlation coefficient
    # was meaningless.
    # In the current, I have no idea how to add weights `R`.
    if (include.r){
        R      <- NA_real_
        pvalue <- NA_real_
        
        tryCatch({
            cor.obj <- cor.test(obs, sim, use = "complete.obs")
            R       <- cor.obj$estimate[[1]]
            pvalue  <- cor.obj$p.value
        }, error = function(e){
            message(e$message)
        })
        R2 = R^2
    }
    # In Linear regression, R2 = R^2 (R is pearson cor)
    # R2     <- summary(lm(sim ~ obs))$r.squared # low efficient

    # AI: Agreement Index (only good values(w==1) calculate AI)
    AI <- NA_real_
    I2 <- which(w == 1)
    if (length(I2) >= 2) {
        obs = obs[I2]
        sim = sim[I2]
        y_mean = mean(obs)
        AI = 1 - sum( (sim - obs)^2 ) / sum( (abs(sim - y_mean) + abs(obs - y_mean))^2 )
    }

    out <- tibble(R, pvalue, R2, NSE, KGE, RMSE, MAE, 
             Bias, Bias_perc, AI = AI, n_sim = n_sim)
    if (include.cv) out <- cbind(out, CV_obs, CV_sim)
    return(out)
}


#' @param ... ignored
#' @rdname GOF
#' @export
KGE <- function(obs, sim, w = c(1, 1, 1), ...) {
  length(sim) <= 2 && (return(-999.0))
  ## Check inputs and select timesteps

  ## calculate components
  c1 = cor(obs, sim) # r: linear correlation
  c2 = sd(sim) / sd(obs) # alpha: ratio of standard deviations
  c3 = mean(sim) / mean(obs) # beta: bias

  ## calculate value
  1 - sqrt((w[1] * (c1 - 1))^2 + (w[2] * (c2 - 1))^2 + (w[3] * (c3 - 1))^2) # weighted KGE
}

#' @rdname GOF
#' @export
NSE <- function(obs, sim, w, ...) {
    if (missing(w)) w <- rep(1, length(obs))

    ind <- valindex(obs, sim)
    w <- w[ind]

    y_mean <- sum(obs[ind] * w) / sum(w)
    # R2: the portion of regression explained variance, also known as
    # coefficient of determination

    # SSR <- sum((sim - y_mean)^2 * w)
    SST <- sum((obs[ind] - y_mean)^2 * w)
    # R2     <- SSR / SST
    RE <- sim[ind] - obs[ind]
    # Bias <- sum(w * RE) / sum(w) # bias
    # Bias_perc <- Bias / y_mean # bias percentage
    # MAE <- sum(w * abs(RE)) / sum(w) # mean absolute error
    RMSE <- sqrt(sum(w * (RE)^2) / sum(w)) # root mean sqrt error

    NSE <- 1 - sum((RE)^2 * w) / SST # NSE coefficient
    NSE
}

#' weighted CV
#' @param x Numeric vector
#' @param w weights of different point
#'
#' @return Named numeric vector, (mean, sd, cv).
#' @examples
#' x <- rnorm(100)
#' coefs <- cv_coef(x)
#' @keywords internal
#' @export
cv_coef <- function(x, w) {
    if (missing(w)) w <- rep(1, length(x))
    if (length(x) == 0) {
        return(c(mean = NA_real_, sd = NA_real_, cv = NA_real_))
    }
    # rm NA_real_
    I <- is.finite(x)
    x <- x[I]
    w <- w[I]

    mean <- sum(x * w) / sum(w)
    sd <- sqrt(sum((x - mean)^2 * w) / sum(w))
    cv <- sd / mean
    c(mean = mean, sd = sd, cv = cv) # quickly return
}

#' Critical value of determined correlation
#'
#' @param n length of observation.
#' @param NumberOfPredictor Number of predictor, including constant.
#' @param alpha significant level.
#'
#' @return `F` statistic and `R2` at significant level.
#'
#' @keywords internal
#' @references
#' Chen Yanguang (2012), Geographical Data analysis with MATLAB.
#' @examples
#' R2_critical <- R2_sign(30, NumberOfPredictor = 2, alpha = 0.05)
#' @export
R2_sign <- function(n, NumberOfPredictor = 2, alpha = 0.05) {
    freedom_r <- NumberOfPredictor - 1 # regression
    freedom_e <- n - NumberOfPredictor # error

    F <- qf(1 - alpha, freedom_r, freedom_e)
    R2 <- 1 - 1 / (1 + F * freedom_r / freedom_e)

    # F = 485.1
    # F = R2/freedom_r/((1-R2)/freedom_e)
    # Rc = sqrt(/(qf(1 - alpha, 1, freedom) + freedom)) %TRUE>% print  # 0.11215
    return(list(F = F, R2 = R2))
}

# remove NA_real_ and Inf values in Y_sim, Y_obs and w
valid <- function(x) !is.na(x) & is.finite(x)

valindex <- function(obs, sim, ...) {
    if (length(obs) != length(sim)) {
        stop("Invalid argument: 'length(sim) != length(obs)' !! (", length(sim), "!=", length(obs), ") !!")
    } else {
        index <- which(valid(obs) & valid(sim))
        if (length(index) == 0) {
            warning("'sim' and 'obs' are empty or they do not have any common pair of elements with data !!")
        }
        return(index)
    }
}

is_empty <- function(x) {
    is.null(x) || (is.data.frame(x) && nrow(x) == 0) || length(x) == 0
}


