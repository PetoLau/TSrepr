# Model-based methods of representation of time series ----
# devtools::use_package("MASS")
# devtools::use_package("quantreg")

# Linear regression model

#' @rdname coef_comp
#' @name coefComp
#' @title Functions for linear regression model coefficients extraction
#'
#' @description The functions computes regression coefficients from a linear model.
#'
#' @return The numeric vector of regression coefficients
#'
#' @param X the model (design) matrix of independent variables
#' @param Y the vector of dependent variable (time series)
#'
#' @seealso \code{\link[stats]{lm}, \link[MASS]{rlm}, \link[quantreg]{rq}}
#'
#' @examples
#' design_matrix <- matrix(rnorm(10), ncol = 2)
#' lmCoef(design_matrix, rnorm(5))
#'
#' @export lmCoef
lmCoef <- function(X, Y) {

  beta <- solve(t(X) %*% X) %*% t(X) %*% as.vector(Y) # OLS

  return(as.vector(beta))
}

# Robust Linear regression model (MASS package)

#' @rdname coef_comp
#' @name coefComp
#' @title Functions for linear regression model coefficients extraction
#'
#' @examples
#' rlmCoef(design_matrix, rnorm(5))
#'
#' @importFrom MASS rlm psi.huber
#' @export rlmCoef
rlmCoef <- function(X, Y) {

  rlm.reg <- MASS::rlm(X, Y, method = "M", psi = MASS::psi.huber, k = 2.5)$coefficients

  return(as.vector(rlm.reg))
}

# L1 Linear regression model (quantreg package)

#' @rdname coef_comp
#' @name coefComp
#' @title Functions for linear regression model coefficients extraction
#'
#' @examples
#' l1Coef(design_matrix, rnorm(5))
#'
#' @importFrom quantreg rq
#' @export l1Coef
l1Coef <- function(X, Y) {

  X_Y <- as.data.frame(cbind(Y, X))

  rlm.reg <- quantreg::rq(Y ~ 0 + .,
                data = X_Y, method = "fn", model = FALSE)$coefficients

  return(as.vector(rlm.reg))
}

#' @rdname repr_lm
#' @name repr_lm
#' @title Regression coefficients from linear model as representation
#'
#' @description The \code{repr_lm} computes seasonal regression coefficients from a linear model. Additional exogenous variables can be also added.
#'
#' @return the numeric vector of regression coefficients
#'
#' @param x the numeric vector (time series)
#' @param freq the frequency of the time series. Can be vector of two frequencies (seasonalities) or just an integer of one frequency.
#' @param method the linear regression method to use. It can be "lm", "rlm" or "l1".
#' @param xreg the data.frame with additional exogenous regressors or the single numeric vector
#'
#' @details TODO. lm rlm and l1. Frequencies.
#'
#' @seealso \code{\link[TSrepr]{repr_gam}, \link[TSrepr]{repr_exp}}
#'
#' @examples
#' # Extracts 24 seasonal regression coefficients from the time series by linear model
#' repr_lm(rnorm(96), freq = 24, method = "lm")
#'
#' # Try also robust linear models ("rlm" and "l1")
#' repr_lm(rnorm(96), freq = 24, method = "rlm")
#' repr_lm(rnorm(96), freq = 24, method = "l1")
#'
#' @importFrom stats as.formula model.matrix
#' @export repr_lm
repr_lm <- function(x, freq = NULL, method = "lm", xreg = NULL) {

  x <- as.numeric(x)

  # creates model matrix
  N <- length(x)

  if(is.null(freq) == F) {

    n_freq <- length(freq)

    if (n_freq > 2) {
      stop("Number of seasonalities must be less than 3!")
    }

    if (n_freq == 2 & freq[1] >= freq[2]) {
      stop("First seasonality must be less than second one!")
    }

    param_list <- list()
    j <- 1
    for (i in freq) {

      if (j == 1) {
        n_times <- N / i

        if ((N %% i) == 0) {
          freq_only <- rep(1:i, n_times)
        }

        if ((N %% i) != 0) {
          freq_only <- rep(1:i, floor(n_times))
          remainder <- N - (floor(N / i) * i)
          freq_only <- c(freq_only, 1:remainder)
        }

        param_list[[j]] <- as.factor(freq_only)
        j <- j + 1
      } else {

        sec_freq <- i / freq[1]
        n_times_freq2 <- N / i

        if ((N %% i) == 0) {
          freq_only <- rep(rep(1:sec_freq, each = freq[1]), n_times_freq2)
        }

        if ((N %% i) != 0) {
          freq_only <- rep(rep(1:sec_freq, each = freq[1]), floor(n_times_freq2))
          remainder <- N - (floor(N / i) * i)

          if((remainder %% freq[1]) == 0) {
            freq_only <- c(freq_only, rep(1:(remainder / freq[1]), each = freq[1]))
          } else {
            remainder_2 <- remainder - (floor(remainder / freq[1]) * freq[1])
            if(remainder_2 < freq[1] & remainder < freq[1]) {
              freq_only <- c(freq_only, rep(1, remainder_2))
            } else {
               freq_only <- c(freq_only, rep(1:floor(remainder / freq[1]), each = freq[1]))
               freq_only <- c(freq_only, rep(floor(remainder / freq[1])+1, remainder_2))
            }
          }
        }
        param_list[[j]] <- as.factor(freq_only)
      }
    }

    param_list <- data.frame(param_list)
    mat_model_freq <- model.matrix(as.formula(paste("~ 0 +", paste(names(param_list), collapse = "+"))), data = param_list)
  }

  if (is.null(xreg) == F & is.null(freq) == F) {
    mat_model_freq <- cbind(mat_model_freq, xreg)
  }

  if (is.null(xreg) == F & is.null(freq) == T) {
    mat_model_freq <- model.matrix(as.formula(paste("~ 0 +", paste(names(xreg), collapse = "+"))), data = xreg)
  }

  if (method == "lm") {
    repr <- lmCoef(mat_model_freq, x)
  }

  if (method == "rlm") {
    repr <- rlmCoef(mat_model_freq, x)
  }

  if (method == "l1") {
    repr <- l1Coef(mat_model_freq, x)
  }

  return(repr)
}

# GAM

#' @rdname repr_gam
#' @name repr_gam
#' @title GAM regression coefficients as representation
#'
#' @description The \code{repr_gam} computes seasonal GAM regression coefficients. Additional exogenous variables can be also added.
#'
#' @return the numeric vector of GAM regression coefficients
#'
#' @param x the numeric vector (time series)
#' @param freq the frequency of the time series. Can be vector of two frequencies (seasonalities) or just an integer of one frequency.
#' @param xreg the numeric vector or the data.frame with additional exogenous regressors
#'
#' @seealso \code{\link[TSrepr]{repr_lm}, \link[TSrepr]{repr_exp}, \link[mgcv]{gam}}
#'
#' @examples
#' repr_gam(rnorm(96), freq = 24)
#'
#' @importFrom stats as.formula
#' @importFrom mgcv gam s
#' @export repr_gam
repr_gam <- function(x, freq = NULL, xreg = NULL) {

  x <- as.numeric(x)

  # creates model matrix
  N <- length(x)

  if(is.null(freq) == F) {

    n_freq <- length(freq)

    if (n_freq > 2) {
      stop("Number of seasonalities must be less than 3!")
    }

    if (n_freq == 2 & freq[1] >= freq[2]) {
      stop("First seasonality must be less than second one!")
    }

    param_list <- list()
    j <- 1
    for (i in freq) {

      if (j == 1) {
        n_times <- N / i

        if ((N %% i) == 0) {
          freq_only <- rep(1:i, n_times)
        }

        if ((N %% i) != 0) {
          freq_only <- rep(1:i, floor(n_times))
          remainder <- N - (floor(N / i) * i)
          freq_only <- c(freq_only, 1:remainder)
        }

        param_list[[j]] <- freq_only
        j <- j + 1
      } else {

        sec_freq <- i / freq[1]
        n_times_freq2 <- N / i

        if ((N %% i) == 0) {
          freq_only <- rep(rep(1:sec_freq, each = freq[1]), n_times_freq2)
        }

        if ((N %% i) != 0) {
          freq_only <- rep(rep(1:sec_freq, each = freq[1]), floor(n_times_freq2))
          remainder <- N - (floor(N / i) * i)

          if((remainder %% freq[1]) == 0) {
            freq_only <- c(freq_only, rep(1:(remainder / freq[1]), each = freq[1]))
          } else {
            remainder_2 <- remainder - (floor(remainder / freq[1]) * freq[1])
            if(remainder_2 < freq[1] & remainder < freq[1]) {
              freq_only <- c(freq_only, rep(1, remainder_2))
            } else {
              freq_only <- c(freq_only, rep(1:floor(remainder / freq[1]), each = freq[1]))
              freq_only <- c(freq_only, rep(floor(remainder / freq[1])+1, remainder_2))
            }
          }
        }
        param_list[[j]] <- freq_only

        freq[2] <- freq[2]/freq[1]
      }
    }

    data_train <- data.frame(param_list)
    m_form <- paste("Y ~ 1 +",paste(c(sapply(1:length(freq), function(i)
                                 paste("s(", names(data_train)[i], ", bs = \"cr\", k = ", freq[i], ")",sep = ""))),
                                 collapse = "+"))

    data_train$Y <- x
  }

  if (is.null(xreg) == F & is.null(freq) == F) {
    xreg <- as.data.frame(xreg)
    data_train <- data.frame(cbind(data_train, xreg))
    m_form <- paste(m_form, paste(c(sapply(1:ncol(xreg), function(i)
                                               paste("s(", names(xreg)[i], ")",sep = ""))),
                                               collapse = "+"), sep = "+")

  }

  if (is.null(xreg) == F & is.null(freq) == T) {
    data_train <- as.data.frame(xreg)
    m_form <- paste("Y ~ 1 +",paste(c(sapply(1:ncol(data_train), function(i)
                                 paste("s(", names(data_train)[i], ")",sep = ""))),
                                 collapse = "+"))
    data_train$Y <- x
  }

  m_gam <- mgcv::gam(as.formula(m_form), data = data_train, optimizer = "outer")

  return(as.vector(m_gam$coefficients[-1]))
}

# Exponential-smoothing

#' @rdname repr_exp
#' @name repr_exp
#' @title Exponential smoothing seasonal coefficients as representation
#'
#' @description The \code{repr_exp} computes exponential smoothing seasonal coefficients.
#'
#' @return the numeric vector of seasonal coefficients
#'
#' @param x the numeric vector (time series)
#' @param freq the frequency of the time series
#' @param alpha the smoothing factor (default to TRUE - automatic determination of smoothing factor), or number between 0 to 1
#' @param gamma the seasonal smoothing factor (default to TRUE - automatic determination of seasonal smoothing factor), or number between 0 to 1
#'
#' @seealso \code{\link[TSrepr]{repr_lm}, \link[TSrepr]{repr_gam}, \link[TSrepr]{repr_seas_profile},
#' \link[stats]{HoltWinters}}
#'
#' @examples
#' repr_exp(rnorm(96), freq = 24)
#'
#' @importFrom stats HoltWinters ts
#' @export repr_exp
repr_exp <- function(x, freq, alpha = TRUE, gamma = TRUE) {

  x <- as.numeric(x)

  repr <- HoltWinters(ts(x, frequency = freq), alpha = alpha, beta = FALSE, gamma = gamma)$coefficients[-1]

  return(as.vector(repr))
}
