library(MASS)
library(mvtnorm)   # for drawing from mvrnorm()
library(car)       # for scatterplotMatrix() visualization
library(MCMCpack)  # for procrustes() rotation
library(plyr)      # for ldply() summarization
library(RColorBrewer)
library(plotrix)

library(abind)
## library(foreach)
## library(doMC)
## registerDoMC(detectCores())

'%.%' <- paste0

## function for recombining matrices in parallelized tensor derivatives
abind.combine <- function(...){
  abind(..., along = 3)
}

## functions for matrix powers
tr.mat <- function(X){
  sum(diag(X))
}
sqrt.mat <- function(X){
  X.eigen <- eigen(X, symmetric = TRUE)
  X.eigen$vectors %*% diag(sqrt(X.eigen$values)) %*% t(X.eigen$vectors)
}
pow.mat <- function(X, pow){
  if (nrow(X) != ncol(X)){
    stop('only square matrices are supported')
  }
  if (pow %% 1 != 0){
    stop('only integer powers are supported')
  }
  if (pow == 0){
    return(diag(nrow(X)))  # identity matrix
  }
  if (pow < 0){
    X <- solve(X)
    pow <- -pow
  }
  if (pow == 1){
    return(X)
  }
  out <- X
  for (i in 2:pow){
    out <- out %*% X
  }
  return(out)
}

## function for plotting vcov matrices, adapted from car::ellipse
ellipse <- function(mu,
                    Sigma,
                    radius = 1  # standard deviations,
                    ){
  segments <- 50
  angles <- (0:segments) * 2 * pi/segments
  unit.circle <- cbind(cos(angles), sin(angles))
  Q <- chol(Sigma, pivot = TRUE)
  order <- order(attr(Q, 'pivot'))
  coord <- t(mu + radius * t(unit.circle %*% Q))
  return(coord)
}



#############################################################
## helper functions for computing various parts of hessian ##
#############################################################

## set up functions for computing various parts of hessian
d2L.dmu.dmuT.fn <- function (D, Q, N, Sigma.hat.inv){
  message('  calculating d^2 L / dmu dmu^T')
  out <- -N * Sigma.hat.inv
  dimnames(out) <- list(mu.d0 = 'mu.d0.' %.% 1:D, mu.d1 = 'mu.d1.' %.% 1:D)
  return(out)
}

d2L.dsigmasq2.fn <- function (D, Q, N, T.demean.hat, Sigma.hat.inv){
  message('  calculating d^2 L / dsigmasq^2')
  out <- as.matrix(
    ## description of optimizations:
    ## - single-obs llh uses A_i %*% B_i * t(A_i), multiple-obs
    ##   version is A %*% B * t(A) but only diagonal needed
    ## - note that diag(A %*% B * t(A)) == rowSums(A %*% B * A)
    ##   and the latter is much faster
    ## - then sum individual llhs over all obs: sum(A %*% B * A)
    ## - scalar quantities multiplied by N instead of summing N copies
    .5 * N * tr.mat(pow.mat(Sigma.hat.inv, 2)) -
      sum(T.demean.hat %*% pow.mat(Sigma.hat.inv, 3) * T.demean.hat)
  )
  dimnames(out) <- list(sigmasq = 'sigmasq', sigmasq = 'sigmasq')
  return(out)
}

## ## pre-optimization version for reference
## d2L.dW.dWT.fn.old <- function (D, Q, N, T.demean.hat.crossprod, W.hat, Sigma.hat.inv){
##     ## outer loop over W, using index variables q1, d3
##     message('  calculating d^2 L / dW dWT')
##     out <- sapply(1:Q, function(q1){
##         message('    q1 = ', q1, sep = '')
##         sapply(1:D, function(d3){
##             message('      d3 = ', d3, sep = '')
##             ## derivative of Sigma w.r.t q1d3-th element of W
##             J.d3q1 <- matrix(0, D, Q)
##             J.d3q1[d3, q1] <- 1
##             dSigma.dW.d3q1 <- J.d3q1 %*% t(W.hat) + W.hat %*% t(J.d3q1)
##             ## inner loop over W, using index variables q0, d2
##             out <- sapply(1:Q, function(q0){
##                 ## message('        q0 = ', q0, sep = '')
##                 sapply(1:D, function(d2){
##                     ## derivative of Sigma w.r.t d2q0-th element of W
##                     J.d2q0 <- matrix(0, D, Q)
##                     J.d2q0[d2, q0] <- 1
##                     dSigma.dW.d2q0 <- J.d2q0 %*% t(W.hat) + W.hat %*% t(J.d2q0)
##                     sum((
##                         .5 * N * (
##                             Sigma.hat.inv %*% dSigma.dW.d3q1 %*% Sigma.hat.inv
##                         ) - .5 * (
##                             Sigma.hat.inv %*% dSigma.dW.d3q1 %*% Sigma.hat.inv %*%
##                             T.demean.hat.crossprod %*% Sigma.hat.inv
##                         ) - .5 * (
##                             Sigma.hat.inv %*% T.demean.hat.crossprod %*%
##                             Sigma.hat.inv %*% dSigma.dW.d3q1 %*% Sigma.hat.inv
##                         )
##                     ) * dSigma.dW.d2q0
##                     ) +
##                         sum((
##                             -.5 * N * Sigma.hat.inv +
##                             .5 * (
##                                 Sigma.hat.inv %*% T.demean.hat.crossprod %*% Sigma.hat.inv
##                             )
##                         ) * (J.d2q0 %*% t(J.d3q1) + J.d3q1 %*% t(J.d2q0))
##                         )
##                 })
##             })
##             dimnames(out) <- list(W.d2 = 'W.d2.' %.% 1:D,
##                                   W.q0 = 'W.q0.' %.% 1:Q
##                                   )
##             out
##         }, simplify = 'array')
##     }, simplify = 'array')
##     dimnames(out)[3:4] <- list('W.d3.' %.% 1:D,
##                                'W.q1.' %.% 1:Q)
##     names(dimnames(out))[3:4] <- c('W.d3', 'W.q1')
##     return(
##         matrix(
##             out,
##             nrow = D * Q,
##             ncol = D * Q,
##             dimnames = list(
##                 as.character(
##                     outer('W.d2.' %.% 1:D,
##                           'W.q0.' %.% 1:Q,
##                           paste,
##                           sep = ', '
##                           )
##                 ),
##                 as.character(
##                     outer('W.d3.' %.% 1:D,
##                           'W.q1.' %.% 1:Q,
##                           paste,
##                           sep = ', '
##                           )
##                 )
##             )
##         )
##     )
## }

## optimized version
d2L.dW.dWT.fn <- function (D, Q, N, T.demean.hat.crossprod, W.hat, Sigma.hat.inv){
  ## outer loop over W, using index variables q1, d3
  message('  calculating d^2 L / dW dWT')
  out1 <- sapply(1:Q, function(q1){
    ## message('    q1 = ', q1, sep = '')
    sapply(1:D, function(d3){
      ## message('      d3 = ', d3, sep = '')
      ## fast deriv of Sigma w.r.t q1d3-th element of W, equiv to
      ##   J.d3q1 <- matrix(0, D, Q)
      ##   J.d3q1[d3, q1] <- 1
      ##   dSigma.dW.d3q1 <- J.d3q1 %*% t(W.hat) + W.hat %*% t(J.d3q1)
      dSigma.dW.d3q1 <- matrix(0, D, D)
      dSigma.dW.d3q1[d3,] <- dSigma.dW.d3q1[,d3] <- W.hat[,q1]
      dSigma.dW.d3q1[d3, d3] <- 2 * W.hat[d3, q1]
      ## precompute various quantities; note that
      ##   isSymmetric(T.demean.hat.crossprod)
      ##   isSymmetric(Sigma.hat.inv)
      ##   T.demean.hat.crossprod %*% Sigma.hat.inv ==
      ##     Sigma.hat.inv %*% T.demean.hat.crossprod
      Sigma.hat.inv.X.dSigma.dW.d3q1.X.Sigma.hat.inv <-
        Sigma.hat.inv %*% dSigma.dW.d3q1 %*% Sigma.hat.inv
      Sigma.hat.inv.X.T.demean.hat.crossprod <-
        Sigma.hat.inv %*% T.demean.hat.crossprod
      why.was.this.clusterfuck.in.inner.loop <-
        .5 * N * (
          Sigma.hat.inv.X.dSigma.dW.d3q1.X.Sigma.hat.inv
        ) - .5 * (
          Sigma.hat.inv.X.dSigma.dW.d3q1.X.Sigma.hat.inv %*%
            Sigma.hat.inv.X.T.demean.hat.crossprod
        ) - .5 * (
          Sigma.hat.inv.X.T.demean.hat.crossprod %*%
            Sigma.hat.inv.X.dSigma.dW.d3q1.X.Sigma.hat.inv
        )
      ## inner loop over W, using index variables q0, d2
      out2 <- sapply(1:Q, function(q0){
        ## message('        q0 = ', q0, sep = '')
        out3 <- sapply(1:D, function(d2){
          ## fast deriv of Sigma w.r.t d2q0-th element of W, equiv to
          ##   J.d2q0 <- matrix(0, D, Q)
          ##   J.d2q0[d2, q0] <- 1
          ##   dSigma.dW.d2q0 <- J.d2q0 %*% t(W.hat) + W.hat %*% t(J.d2q0)
          dSigma.dW.d2q0 <- matrix(0, D, D)
          dSigma.dW.d2q0[d2,] <- dSigma.dW.d2q0[,d2] <- W.hat[,q0]
          dSigma.dW.d2q0[d2, d2] <- 2 * W.hat[d2, q0]
          out4 <- sum(
            why.was.this.clusterfuck.in.inner.loop * dSigma.dW.d2q0
          )
          ## fast version of adding the following:
          ##   sum((
          ##     -.5 * N * Sigma.hat.inv +
          ##       .5 * (
          ##         Sigma.hat.inv %*% T.demean.hat.crossprod %*% Sigma.hat.inv
          ##       )
          ##   ) * (J.d2q0 %*% t(J.d3q1) + J.d3q1 %*% t(J.d2q0))
          ##   )
          if (q0 == q1){
            out4 <- out4 - N * Sigma.hat.inv[d2, d3] + (
              Sigma.hat.inv.X.T.demean.hat.crossprod %*% Sigma.hat.inv
            )[d2, d3]
          }
          return(out4)
        })
        return(out3)
      })
      dimnames(out2) <- list(W.d2 = 'W.d2.' %.% 1:D,
                             W.q0 = 'W.q0.' %.% 1:Q
                             )
      return(out2)
    }, simplify = 'array')
  }, simplify = 'array')
  dimnames(out1)[3:4] <- list('W.d3.' %.% 1:D,
                              'W.q1.' %.% 1:Q)
  names(dimnames(out1))[3:4] <- c('W.d3', 'W.q1')
  return(
    matrix(
      out1,
      nrow = D * Q,
      ncol = D * Q,
      dimnames = list(
        as.character(
          outer('W.d2.' %.% 1:D,
                'W.q0.' %.% 1:Q,
                paste,
                sep = ', '
                )
        ),
        as.character(
          outer('W.d3.' %.% 1:D,
                'W.q1.' %.% 1:Q,
                paste,
                sep = ', '
                )
        )
      )
    )
  )
}

d2L.dsigmasq.dmuT.fn <- function (D, Q, T.demean.hat, Sigma.hat.inv){
  message('  calculating d^2 L / dsigmasq dmu^T')
  out <- -t(rowSums(pow.mat(Sigma.hat.inv, 2) %*% t(T.demean.hat)))
  dimnames(out) <- list(sigmasq = 'sigmasq',
                        mu = 'mu.' %.% 1:D)
  return(out)
}

d2L.dsigmasq.dWT.fn <- function (D, Q, N, T.demean.hat, W.hat, Sigma.hat.inv){
  message('  calculating d^2 L / dsigmasq dWT')
  out <- t(
    sapply(1:Q, function(q0){
      sapply(1:D, function(d2){
        J <- matrix(0, D, Q)
        J[d2,q0] <- 1
        dSigma.dW.element <- J %*% t(W.hat) + W.hat %*% t(J)
        neg.dSigmainv.dW.element <-
          Sigma.hat.inv %*% dSigma.dW.element %*% Sigma.hat.inv
        ## description of optimizations:
        ## - single-obs llh uses A_i %*% B_i * t(A_i);
        ##   multiple-obs version is A %*% B * t(A)
        ##   but only the diagonal is needed
        ## - note that diag(A %*% B * t(A)) == rowSums(A %*% B * A)
        ##   and the latter is much faster
        ## - then sum individual llhs over all obs: sum(A %*% B * A)
        ## - scalar quantities multiplied by N instead of summing N copies
        (
          .5 * N * tr.mat(neg.dSigmainv.dW.element) -
            .5 * sum(T.demean.hat %*% (
              neg.dSigmainv.dW.element %*% Sigma.hat.inv +
                Sigma.hat.inv %*% neg.dSigmainv.dW.element
            ) * T.demean.hat)
        )
      })
    })
  )
  dimnames(out) <- list(W.q = 'W.q0.' %.% 1:Q,
                        W.d2 = 'W.d2.' %.% 1:D)
  return(
    matrix(
      t(out),
      nrow = 1,
      ncol = D * Q,
      dimnames = list('sigmasq',
                      as.character(
                        outer('W.d2.' %.% 1:D,
                              'W.q0.' %.% 1:Q,
                              paste,
                              sep = ', '
                              )
                      )
                      )
    )
  )
}

d2L.dmu.dWT.fn <- function (D, Q, T.demean.hat, W.hat, Sigma.hat.inv){
  message('  calculating d^2 L / dmu dW^T')
  out1 <- sapply(1:Q, function(q0){
    out2 <- sapply(1:D, function(d2){
      J <- matrix(0, D, Q)
      J[d2,q0] <- 1
      dSigma.dW.element <- J %*% t(W.hat) + W.hat %*% t(J)
      as.matrix(rowSums(
        -Sigma.hat.inv %*% dSigma.dW.element %*%
          Sigma.hat.inv %*% t(T.demean.hat)
      ))
    })
    dimnames(out2) <- list(mu = 'mu.' %.% 1:D,
                           W.d2 = 'W.d2.' %.% 1:D
                           )
    return(out2)
  }, simplify = 'array')
  dimnames(out1)[3] <- list('W.q0.' %.% 1:Q)
  names(dimnames(out1))[3] <- 'W.q0'
  return(
    matrix(
      out1,
      nrow = D,
      ncol = D * Q,
      dimnames = list('mu.' %.% 1:D,
                      as.character(
                        outer('W.d2.' %.% 1:D,
                              'W.q0.' %.% 1:Q,
                              paste,
                              sep = ', '
                              )
                      )
                      )
    )
  )
}



#############################
## main functions for mcmc ##
#############################

param.posterior.given.full.data <- function(data, Q, prior, mle = FALSE){

  message('  recalculating parameter posterior given missing data')

  if (Q == 1){
    stop('at least two principal components must be extracted')
  }

  T <- data
  N <- nrow(T)
  D <- ncol(T)
  n.missing <- sum(is.na(T))
  if (n.missing > 0){
    stop(sprintf('found %s missing elements', n.missing))
  }
  if (Q > D){
    stop('dimensionality of latent space must be ',
         '<= dimensionality of observed space')
  }

  ## calculate maximum likelihood estimates
  mu.hat <- t(T) %*% rep(1, N) / N
  vcov.hat.eigen <- eigen(cov(data), symmetric = TRUE)
  if (Q == D){
    sigmasq.hat <- 0
  } else {
    sigmasq.hat <- sum(vcov.hat.eigen$values[(Q + 1):D]) / (D - Q)
  }
  W.hat <- vcov.hat.eigen$vectors[, 1:Q, drop = FALSE] %*%
    diag(sqrt(vcov.hat.eigen$values[1:Q] - sigmasq.hat))
  Sigma.hat <- tcrossprod(W.hat) + sigmasq.hat * diag(D)
  Sigma.hat.inv <- solve(Sigma.hat)

  lik <- list(
    mode = c(as.numeric(mu.hat),
             as.numeric(W.hat),
             sigmasq.hat
             )
  )

  names(lik$mode) <- c('mu.' %.% 1:D,
                       as.character(
                         outer('W.d.' %.% 1:D,
                               'W.q.' %.% 1:Q,
                               paste,
                               sep = ', '
                               )
                       ),
                       'sigmasq'
                       )

  if (mle){
    return(lik)
  }

  ## precompute useful quantities
  Sigma.hat <- tcrossprod(W.hat) + sigmasq.hat * diag(D)
  Sigma.hat.inv <- solve(Sigma.hat)
  T.demean.hat <- sweep(T, 2, mu.hat, '-')
  T.demean.hat.crossprod <- crossprod(T.demean.hat)

  ## calculate hessian components
  d2L.dmu.dmuT <- d2L.dmu.dmuT.fn(D, Q, N, Sigma.hat.inv)
  d2L.dsigmasq2 <- d2L.dsigmasq2.fn(D, Q, N, T.demean.hat, Sigma.hat.inv)
  d2L.dW.dWT <- d2L.dW.dWT.fn(D, Q, N, T.demean.hat.crossprod, W.hat, Sigma.hat.inv)
  d2L.dsigmasq.dmuT <- d2L.dsigmasq.dmuT.fn(D, Q, T.demean.hat, Sigma.hat.inv)
  d2L.dsigmasq.dWT <- d2L.dsigmasq.dWT.fn(D, Q, N, T.demean.hat, W.hat, Sigma.hat.inv)
  d2L.dmu.dWT <- d2L.dmu.dWT.fn(D, Q, T.demean.hat, W.hat, Sigma.hat.inv)

  ## distinction between indices in flattened rows and cols (e.g. d vs d')
  ##   kept until here for clarity but not needed in hessian
  rownames(d2L.dmu.dmuT) <- gsub('\\.d[0-1]', '', rownames(d2L.dmu.dmuT))
  colnames(d2L.dmu.dmuT) <- gsub('\\.d[0-1]', '', colnames(d2L.dmu.dmuT))
  names(dimnames(d2L.dmu.dmuT)) <- NULL
  names(dimnames(d2L.dsigmasq2)) <- NULL
  rownames(d2L.dW.dWT) <- gsub('(d|q)\\d', '\\1', rownames(d2L.dW.dWT))
  colnames(d2L.dW.dWT) <- gsub('(d|q)\\d', '\\1', colnames(d2L.dW.dWT))
  names(dimnames(d2L.dsigmasq.dmuT)) <- NULL
  colnames(d2L.dsigmasq.dWT) <- gsub('(d|q)\\d', '\\1', colnames(d2L.dsigmasq.dWT))
  colnames(d2L.dmu.dWT) <- gsub('(d|q)\\d', '\\1', colnames(d2L.dmu.dWT))

  ## combine various second derivatives into hessian of llh
  ##  _                                                   _
  ## |                                                     |
  ## | d^2/dmu^2         d^2/dmu.dW       d^2/dmu.dsigmasq |
  ## | d^2/dW.dmu        d^2/dW^2         d^2/dW.dsigmasq  |
  ## | d^2/dsigmasq.dmu  d^2/dsigmasq.dW  d^2/dsigmasq^2   |
  ## |_                                                   _|
  lik$vcov.inv <- -1 * rbind(
    cbind(d2L.dmu.dmuT,      d2L.dmu.dWT,      t(d2L.dsigmasq.dmuT)),
    cbind(t(d2L.dmu.dWT),    d2L.dW.dWT,       t(d2L.dsigmasq.dWT)),
    cbind(d2L.dsigmasq.dmuT, d2L.dsigmasq.dWT, t(d2L.dsigmasq2))
  )
  ## enforce symmetry of hessian; in tests with D=125 and Q=5,
  ##   max(abs(hessian.mat[i,j] - hessian.mat[j,i])) is ~ 1e-9
  ##   but this can may cause problems with 751x751 matrix especially
  ##   when differences are large relative to hessian.mat[i,j]
  lik$vcov.inv <- (lik$vcov.inv + t(lik$vcov.inv)) / 2

  if (!is.null(prior)){

    post <- list()
    post$vcov.inv <- prior$vcov.inv + lik$vcov.inv
    post$vcov <- tryCatch(
      expr = solve(post$vcov.inv),
      error = function(e){
        warning('caught error while inverting posterior Hessian:\n  ',
                gsub('Error in ', '', e),
                'reattempting with Moore-Penrose pseudoinverse')
        ginv(post$vcov.inv)
      })
    post$mode <-
      post$vcov %*% (prior$vcov.inv %*% prior$mode + lik$vcov.inv %*% lik$mode)
    return(post)

  } else {

    lik$vcov <- tryCatch(
      expr = solve(lik$vcov.inv),
      error = function(e){
        warning('caught error while inverting likelihood Hessian:\n  ',
                gsub('Error in ', '', e),
                'reattempting with Moore-Penrose pseudoinverse')
        ginv(lik$vcov.inv)
      })
    return(lik)

  }

}

sample.missing.data.given.params <- function(data, mu, Sigma, mle = FALSE){

  message('  resampling missing data given parameters')
  missing <- is.na(data)

  ## generate binary sequences with 1 in position of the missing covariates
  missingness.patterns <- apply(missing, 1, function(x){
    paste(as.integer(x), collapse = '')
  })

  ## loop over different kinds of missingness,
  ##   e.g. T[,j] missing only, T[,c(j,j')] both missing, ...
  for (missingness.pattern in sort(unique(missingness.patterns))){

    ## for this pattern of missingness, which columns are (un)observed?
    missing.cols <- strsplit(missingness.pattern, '')[[1]] == '1'
    ## if nothing is missing
    if (sum(missing.cols) == 0){
      next
    }
    if (all(missing.cols)){
      stop('some observations are missing all values; ',
           'remove these and try again')
    }
    ind <- which(missingness.patterns == missingness.pattern)

    ## partition mu, Sigma into (non)missing X (non)missing blocks
    mu.miss <- mu[missing.cols]
    mu.obs <- mu[!missing.cols]
    Sigma.miss.miss <- Sigma[missing.cols, missing.cols, drop = FALSE]
    Sigma.miss.obs <- Sigma[missing.cols, !missing.cols, drop = FALSE]
    Sigma.obs.miss <- Sigma[!missing.cols, missing.cols, drop = FALSE]
    Sigma.obs.obs <- Sigma[!missing.cols, !missing.cols, drop = FALSE]

    ## precompute various quantities
    Sigma.obs.obs.inv <- tryCatch(
      expr = solve(Sigma.obs.obs),
      error = function(e){
        warning('caught error while inverting covariance matrix:\n  ',
                gsub('Error in ', '', e),
                'reattempting with Moore-Penrose pseudoinverse')
        ginv(Sigma.obs.obs)
      })
    ## multiply this by (obs.vals - obs.mus) to get (missing.vals.hat - missing.mus)
    Sigma.miss.post <- Sigma.miss.miss -
      Sigma.miss.obs %*% Sigma.obs.obs.inv %*% Sigma.obs.miss
    mu.shift.premultiply <- Sigma.miss.obs %*% Sigma.obs.obs.inv

    ## sample from posterior on missing data, given parameters and observed data
    for (i in ind){
      mu.miss.post <-
        mu.miss + mu.shift.premultiply %*% (data[i, !missing.cols] - mu.obs)
      if (mle){
        data[i, missing.cols] <- mu.miss.post
      } else {
        data[i, missing.cols] <- tryCatch(
          expr = mvrnorm(1, mu.miss.post, Sigma.miss.post),
          error = function(e){
            warning(
              'caught error while sampling missing data:\n  ',
              gsub('Error in ', '', e),
              'reattempting with truncated-eigenvalue PD reconstruction'
            )
            Sigma.miss.post.eigen <- eigen(Sigma.miss.post,
                                           symmetric = TRUE
                                           )
            Sigma.miss.post.closestpd <-
              Sigma.miss.post.eigen$vectors %*%
              diag(pmax(Sigma.miss.post.eigen$values, 0),
                   nrow = length(Sigma.miss.post.eigen$values)
                   ) %*%
              solve(Sigma.miss.post.eigen$vectors)
            mvrnorm(1, mu.miss.post, Sigma.miss.post.closestpd)
          })
      }
    }

  }

  return(data)

}

bpca <- function(data,
                 Q = NULL,
                 niter.mcmc = 1000,
                 maxiter.em = 100,
                 prior = 'default',
                 tol.em = 1e-6,
                 scaling = rep('pca', ncol(data))
                 ){

  T <- data
  D <- ncol(T)

  ## drop obs that are missing on all dimensions
  all.missing <- apply(is.na(T), 1, all)
  if (any(all.missing)){
    warning('dropping ', sum(all.missing), ' obs that are missing all values')
    missing.ind <- which(all.missing)
    T <- T[!all.missing,]
  }

  ## ensure that variables are binary before performing mca
  if (any(scaling == 'mca')){
    if (!all(T[,scaling == 'mca'] %in% c(NA, 0, 1))){
      stop('multiple correspondence analysis can only be performed on ',
           'binary variables; please check column(s) ',
           paste(which(scaling == 'mca'), collapse = ', ')
           )
    }
  }

  ## initialize data center
  center.star <- colMeans(T, na.rm = TRUE)
  ## initialize  data scaling: standardize for pca, scale by sqrt(p) for mca
  scale.star <- rep(NA, ncol(T))
  scale.star[scaling == 'pca'] <-
    apply(T[,scaling == 'pca', drop = FALSE], 2, sd, na.rm = TRUE)
  scale.star[scaling == 'mca'] <-
    sqrt(colMeans(T[,scaling == 'mca', drop = FALSE], na.rm = TRUE))
  scale.star[scaling == 'mca' & !is.finite(scale.star)] <- sqrt(
    colMeans(T[,scaling == 'mca' & !is.finite(scale.star), drop = FALSE],
             na.rm = TRUE
             )
  )
  ## initialize covariance matrix
  ##   use all (T[i,j], T[i,j']) pairs that are non-NA,
  ##   i.e. don't drop obs with missing values entirely
  Sigma.star <- tryCatch({
    ## this breaks when missingness is high, wrapping in tryCatch
    Sigma.star.attempt <- cov(
      scale(T,
            center = center.star,
            scale = scale.star
            ),
      use = 'pairwise.complete.obs'
    )
    ## ensure Sigma.star is invertible: if no throw error, if yes return
    solve(Sigma.star.attempt)
    Sigma.star.attempt
  },
  error = function(e){
    ## alternate smart approach:
    ##   Sigma.star <- CVcov(T, maxlam = 1, minlam = .01, steps = 10)
    ## ah screw it that takes too long, alternative dumb approach
    diag(apply(T, 2, var, na.rm = TRUE))
  })

  ## determine number of principal components
  if (is.null(Q)){
    Sigma.star.eigen <- eigen(Sigma.star, symmetric = TRUE)
    sv.thresh <- 2.858 * median(Sigma.star.eigen$values)
    Q <- sum(Sigma.star.eigen$values > sv.thresh)
    message(sprintf('number of dimensions set to Q=%s ', Q),
            'by optimal hard thresholding (Gavish & Donoho, 2014)'
            )
    if (Q == D){
      stop('thresholding failed, please specify Q (most likely issue: ',
           'covariance matrix was poorly initialized due to high missingness)'
           )
    }
  }

  ## initialize missing data after converting back to observed scale
  data.star <-
    sample.missing.data.given.params(
      T,
      center.star,
      Sigma.star * outer(scale.star, scale.star, '*'),
      mle = TRUE
    )

  ## revise data center
  center.star <- colMeans(data.star)
  ## revise data scaling: standardize for pca, scale by sqrt(p) for mca
  ##   when considering sampled missing values, p = mean(value)
  ##   in cases where a bad imputation draw takes mean below zero,
  ##   use observed-data p for that iteration-feature to avoid crashing
  scale.star <- rep(NA, ncol(T))
  scale.star[scaling == 'pca'] <-
    apply(data.star[,scaling == 'pca', drop = FALSE], 2, sd)
  scale.star[scaling == 'mca'] <-
    sqrt(colMeans(data.star[,scaling == 'mca', drop = FALSE]))
  scale.star[scaling == 'mca' & !is.finite(scale.star)] <- sqrt(
    colMeans(T[,scaling == 'mca' & !is.finite(scale.star), drop = FALSE],
             na.rm = TRUE
             )
  )
  ## rescale
  data.star <- scale(data.star,
                     center = center.star,
                     scale = scale.star
                     )

  ## initialize parameters
  params.star.update <-
    param.posterior.given.full.data(data.star, Q, prior = NULL, mle = TRUE)$mode

  ## go go gadget
  message('initializing by EM:')
  L1.update <- Inf
  for (iter in 1:maxiter.em){

    ## save last param draw
    params.star <- params.star.update

    ## redraw params
    mu.star <-
      scale.star * params.star[grep('^mu', names(params.star))] + center.star
    W.star <- matrix(params.star[grep('^W', names(params.star))], D, Q)
    sigmasq.star <- params.star[grep('^sigmasq', names(params.star))]
    Sigma.unscaled.star <- (tcrossprod(W.star) + sigmasq.star * diag(D))
    Sigma.star <- diag(scale.star) %*% Sigma.unscaled.star %*% diag(scale.star)

    ## redraw missing data after converting back to observed scale
    data.star <-
      sample.missing.data.given.params(
        T,
        mu.star,
        Sigma.star,
        mle = TRUE
      )

    ## revise data center
    center.star <- colMeans(data.star)
    ## revise data scaling: standardize for pca, scale by sqrt(p) for mca
    ##   when considering sampled missing values, p = mean(value)
    ##   in cases where a bad imputation draw takes mean below zero,
    ##   use observed-data p for that iteration-feature to avoid crashing
    scale.star <- rep(NA, ncol(T))
    scale.star[scaling == 'pca'] <-
      apply(data.star[,scaling == 'pca', drop = FALSE], 2, sd, na.rm = TRUE)
    scale.star[scaling == 'mca'] <-
      sqrt(colMeans(data.star[,scaling == 'mca', drop = FALSE], na.rm = TRUE))
    scale.star[scaling == 'mca' & !is.finite(scale.star)] <- sqrt(
      colMeans(T[,scaling == 'mca' & !is.finite(scale.star), drop = FALSE],
               na.rm = TRUE
               )
    )
    ## rescale
    data.star <- scale(data.star,
                       center = center.star,
                       scale = scale.star
                       )

    ## calculate param posterior given full data
    params.star.update <-
      param.posterior.given.full.data(data.star, Q, prior = NULL, mle = TRUE)$mode
    L1.update <- sum(abs(params.star.update - params.star))
    message(sprintf('EM iter %s: L1 update distance of %s', iter, L1.update))
    if (L1.update <= tol.em){
      message('EM converged within tolerance, stopping')
      break
    }
  }
  if (iter == maxiter.em && L1.update > tol.em){
    warning('reached maximum EM iterations before convergence')
  }
  mu.mle <- mu.star
  W.mle <- W.star
  sigmasq.mle <- sigmasq.star
  names(mu.mle) <- colnames(T)
  rownames(W.mle) <- colnames(T)
  data.mle <- data.star
  ## scores.mle <- data.star %*% W.mle  # fixed 2018/11/08
  scores.mle <- data.star %*% solve(Sigma.unscaled.star) %*% W.mle

  rm(iter, params.star.update, L1.update)

  ## start mcmc
  mu.mcmc <- matrix(NA_real_, nrow = length(mu.mle), ncol= niter.mcmc)
  W.mcmc <- W.rotate.mcmc <- array(NA_real_,
                                   dim = c(dim(W.mle), niter.mcmc),
                                   dimnames = c(dimnames(W.mle), list())
                                   )
  sigmasq.mcmc <- rep(NA_real_, niter.mcmc)
  scores.mcmc <- array(NA_real_, dim = c(nrow(data.mle), Q, niter.mcmc), list())
  scores.rotate.mcmc <-
    array(NA_real_, dim = c(nrow(data.mle), Q, niter.mcmc), list())

  ## prior
  if (prior == 'default'){
    prior <- list(
      mode = c(rep(0, D * (1 + Q)), # everything but sigmasq
               .5 # sigmasq, should be in [0,1] w/ standardized vars
               ),
      vcov = diag(
        c(rep(1, D * (1 + Q)),
          .5
          )
      )
    )
    prior$vcov.inv <- solve(prior$vcov)
  } else if (prior == 'none'){
    prior <- NULL
  } else if (is.list(prior) && all(c('mode', 'vcov') %in% names(prior))){
    prior$vcov.inv <- solve(prior$vcov)
  } else {
    message('setting prior to NULL')
  }

  digits <- ceiling(log(niter.mcmc + 1, 10))
  message('MCMC draw ', rep(' ', digits), sep = '')
  for (iter in 1:niter.mcmc){
    message(rep('\b', digits), sprintf('%' %.% digits %.% 'd', iter), sep = '')

    ## redraw missing data after converting back to observed scale
    data.star <-
      sample.missing.data.given.params(
        T,
        mu.star,
        Sigma.star,
        mle = FALSE
      )

    ## revise data center
    center.star <- colMeans(data.star)
    ## revise data scaling: standardize for pca, scale by sqrt(p) for mca
    ##   when considering sampled missing values, p = mean(value)
    ##   in cases where a bad imputation draw takes mean below zero,
    ##   use observed-data p for that iteration-feature to avoid crashing
    scale.star <- rep(NA, ncol(T))
    scale.star[scaling == 'pca'] <-
      apply(data.star[,scaling == 'pca', drop = FALSE], 2, sd, na.rm = TRUE)
    scale.star[scaling == 'mca'] <-
      sqrt(colMeans(data.star[,scaling == 'mca', drop = FALSE], na.rm = TRUE))
    scale.star[scaling == 'mca' & !is.finite(scale.star)] <- sqrt(
      colMeans(T[,scaling == 'mca' & !is.finite(scale.star), drop = FALSE],
               na.rm = TRUE
               )
    )
    ## rescale
    data.star <- scale(data.star,
                       center = center.star,
                       scale = scale.star
                       )

    ## sample from param posterior conditional on full data
    param.posterior <- param.posterior.given.full.data(data.star, Q, prior)
    params.star <- tryCatch(
      expr = mvrnorm(1, param.posterior$mode, param.posterior$vcov),
      error = function(e){
        warning('caught error while sampling parameters:\n  ',
                gsub('Error in ', '', e),
                'reattempting with truncated-eigenvalue PD reconstruction ',
                'of covariance matrix'
                )
        Sigma.eigen <- eigen(param.posterior$vcov,
                             symmetric = TRUE
                             )
        Sigma.closestpd <-
          Sigma.eigen$vectors %*%
          diag(pmax(Sigma.eigen$values, 0),
               nrow = length(Sigma.eigen$values)
               ) %*%
          solve(Sigma.eigen$vectors)
        mvrnorm(1, param.posterior$mode, Sigma.closestpd)
      })
    mu.star <-
      scale.star * params.star[grep('^mu', names(params.star))] + center.star
    W.star <- matrix(params.star[grep('^W', names(params.star))], D, Q)

    ## W is only identified up to an arbitrary rotation matrix;
    ##   rotate W.star the direction that most closely aligns with mle
    W.rotate.star <- procrustes(W.star, W.mle)$X.new
    W.rotate.mcmc[,,iter] <- W.rotate.star
    ## residual variance
    sigmasq.star <- params.star[grep('^sigmasq', names(params.star))]
    Sigma.unscaled.star <- tcrossprod(W.star) + sigmasq.star * diag(D)
    Sigma.star <- diag(scale.star) %*% Sigma.unscaled.star %*% diag(scale.star)

    ## store results from this iter
    mu.mcmc[,iter] <- mu.star
    W.mcmc[,,iter] <- W.star

    scores.mle <- data.star %*% solve(Sigma.unscaled.star) %*% W.star
    scores.vcov <- diag(Q) - t(W.star) %*% solve(Sigma.unscaled.star) %*% W.star
    scores.rotate.mle <-
      data.star %*% solve(Sigma.unscaled.star) %*% W.rotate.star
    scores.rotate.vcov <-
      diag(Q) - t(W.rotate.star) %*% solve(Sigma.unscaled.star) %*% W.rotate.star

    ## scores.mcmc[,,iter] <- data.star %*% W.star  # fixed 2018/11/08
    ## scores.rotate.mcmc[,,iter] <- data.star %*% W.rotate.star
    scores.mcmc[,,iter] <-
      scores.mle + mvrnorm(nrow(scores.mle), rep(0, Q), scores.vcov)
    scores.rotate.mcmc[,,iter] <-
      scores.rotate.mle + mvrnorm(nrow(scores.rotate.mle), rep(0, Q), scores.vcov)

    ## residual variance unexplained by first Q dimensions
    sigmasq.mcmc[iter] <- sigmasq.star

  }
  message('\n')

  out <- list(
    mle = list(mu = mu.mle,
               W = W.mle,
               sigmasq = sigmasq.mle,
               scores = scores.mle,
               data = data.mle
               ),
    mcmc = list(mu = mu.mcmc,
                W = W.mcmc,
                W.rotate = W.rotate.mcmc,
                sigmasq = sigmasq.mcmc,
                scores = scores.mcmc,
                scores.rotate = scores.rotate.mcmc
                )
  )

  if (any(all.missing)){
    attr(out, 'na.action') <- missing.ind
    attr(out, 'class') <- 'omit'
  }

  return(out)

}


## ## don't rescale sd based on missing values
## bpca.tmp <- function(data,
##                  Q = NULL,
##                  niter.mcmc = 1000,
##                  maxiter.em = 100,
##                  prior = 'default',
##                  tol.em = 1e-6,
##                  scaling = rep('pca', ncol(data))
##                  ){

##   T <- data
##   D <- ncol(T)

##   ## drop obs that are missing on all dimensions
##   all.missing <- apply(is.na(T), 1, all)
##   if (any(all.missing)){
##     warning('dropping ', sum(all.missing), ' obs that are missing all values')
##     missing.ind <- which(all.missing)
##     T <- T[!all.missing,]
##   }

##   ## ensure that variables are binary before performing mca
##   if (any(scaling == 'mca')){
##     if (!all(T[,scaling == 'mca'] %in% c(NA, 0, 1))){
##       stop('multiple correspondence analysis can only be performed on ',
##            'binary variables; please check column(s) ',
##            paste(which(scaling == 'mca'), collapse = ', ')
##            )
##     }
##   }

##   ## initialize data center
##   center.star <- colMeans(T, na.rm = TRUE)
##   ## initialize  data scaling: standardize for pca, scale by sqrt(p) for mca
##   scale.star <- rep(NA, ncol(T))
##   scale.star[scaling == 'pca'] <-
##     apply(T[,scaling == 'pca', drop = FALSE], 2, sd, na.rm = TRUE)
##   scale.star[scaling == 'mca'] <-
##     sqrt(colMeans(T[,scaling == 'mca', drop = FALSE], na.rm = TRUE))
##   scale.star[scaling == 'mca' & !is.finite(scale.star)] <- sqrt(
##     colMeans(T[,scaling == 'mca' & !is.finite(scale.star), drop = FALSE],
##              na.rm = TRUE
##              )
##   )
##   ## initialize covariance matrix
##   ##   use all (T[i,j], T[i,j']) pairs that are non-NA,
##   ##   i.e. don't drop obs with missing values entirely
##   Sigma.star <- tryCatch({
##     ## this breaks when missingness is high, wrapping in tryCatch
##     Sigma.star.attempt <- cov(
##       scale(T,
##             center = center.star,
##             scale = scale.star
##             ),
##       use = 'pairwise.complete.obs'
##     )
##     ## ensure Sigma.star is invertible: if no throw error, if yes return
##     solve(Sigma.star.attempt)
##     Sigma.star.attempt
##   },
##   error = function(e){
##     ## alternate smart approach:
##     ##   Sigma.star <- CVcov(T, maxlam = 1, minlam = .01, steps = 10)
##     ## ah screw it that takes too long, alternative dumb approach
##     diag(apply(T, 2, var, na.rm = TRUE))
##   })

##   ## determine number of principal components
##   if (is.null(Q)){
##     Sigma.star.eigen <- eigen(Sigma.star, symmetric = TRUE)
##     sv.thresh <- 2.858 * median(Sigma.star.eigen$values)
##     Q <- sum(Sigma.star.eigen$values > sv.thresh)
##     message(sprintf('number of dimensions set to Q=%s ', Q),
##             'by optimal hard thresholding (Gavish & Donoho, 2014)'
##             )
##     if (Q == D){
##       stop('thresholding failed, please specify Q (most likely issue: ',
##            'covariance matrix was poorly initialized due to high missingness)'
##            )
##     }
##   }

##   ## initialize missing data after converting back to observed scale
##   data.star <-
##     sample.missing.data.given.params(
##       T,
##       center.star,
##       Sigma.star * outer(scale.star, scale.star, '*'),
##       mle = TRUE
##     )

##   ## ## revise data center
##   ## center.star <- colMeans(data.star)
##   ## ## revise data scaling: standardize for pca, scale by sqrt(p) for mca
##   ## ##   when considering sampled missing values, p = mean(value)
##   ## ##   in cases where a bad imputation draw takes mean below zero,
##   ## ##   use observed-data p for that iteration-feature to avoid crashing
##   ## scale.star <- rep(NA, ncol(T))
##   ## scale.star[scaling == 'pca'] <-
##   ##   apply(data.star[,scaling == 'pca', drop = FALSE], 2, sd)
##   ## scale.star[scaling == 'mca'] <-
##   ##   sqrt(colMeans(data.star[,scaling == 'mca', drop = FALSE]))
##   ## scale.star[scaling == 'mca' & !is.finite(scale.star)] <- sqrt(
##   ##   colMeans(T[,scaling == 'mca' & !is.finite(scale.star), drop = FALSE],
##   ##            na.rm = TRUE
##   ##            )
##   ## )
##   ## ## rescale
##   ## data.star <- scale(data.star,
##   ##                    center = center.star,
##   ##                    scale = scale.star
##   ##                    )

##   ## initialize parameters
##   params.star.update <-
##     param.posterior.given.full.data(data.star, Q, prior = NULL, mle = TRUE)$mode

##   ## go go gadget
##   message('initializing by EM:')
##   L1.update <- Inf
##   for (iter in 1:maxiter.em){

##     ## save last param draw
##     params.star <- params.star.update

##     ## redraw params
##     mu.star <-
##       scale.star * params.star[grep('^mu', names(params.star))] + center.star
##     W.star <- matrix(params.star[grep('^W', names(params.star))], D, Q)
##     sigmasq.star <- params.star[grep('^sigmasq', names(params.star))]
##     Sigma.unscaled.star <- (tcrossprod(W.star) + sigmasq.star * diag(D))
##     Sigma.star <- diag(scale.star) %*% Sigma.unscaled.star %*% diag(scale.star)

##     ## redraw missing data after converting back to observed scale
##     data.star <-
##       sample.missing.data.given.params(
##         T,
##         mu.star,
##         Sigma.star,
##         mle = TRUE
##       )

##     ## ## revise data center
##     ## center.star <- colMeans(data.star)
##     ## ## revise data scaling: standardize for pca, scale by sqrt(p) for mca
##     ## ##   when considering sampled missing values, p = mean(value)
##     ## ##   in cases where a bad imputation draw takes mean below zero,
##     ## ##   use observed-data p for that iteration-feature to avoid crashing
##     ## scale.star <- rep(NA, ncol(T))
##     ## scale.star[scaling == 'pca'] <-
##     ##   apply(data.star[,scaling == 'pca', drop = FALSE], 2, sd, na.rm = TRUE)
##     ## scale.star[scaling == 'mca'] <-
##     ##   sqrt(colMeans(data.star[,scaling == 'mca', drop = FALSE], na.rm = TRUE))
##     ## scale.star[scaling == 'mca' & !is.finite(scale.star)] <- sqrt(
##     ##   colMeans(T[,scaling == 'mca' & !is.finite(scale.star), drop = FALSE],
##     ##            na.rm = TRUE
##     ##            )
##     ## )
##     ## ## rescale
##     ## data.star <- scale(data.star,
##     ##                    center = center.star,
##     ##                    scale = scale.star
##     ##                    )

##     ## calculate param posterior given full data
##     params.star.update <-
##       param.posterior.given.full.data(data.star, Q, prior = NULL, mle = TRUE)$mode
##     L1.update <- sum(abs(params.star.update - params.star))
##     message(sprintf('EM iter %s: L1 update distance of %s', iter, L1.update))
##     if (L1.update <= tol.em){
##       message('EM converged within tolerance, stopping')
##       break
##     }
##   }
##   if (iter == maxiter.em && L1.update > tol.em){
##     warning('reached maximum EM iterations before convergence')
##   }
##   mu.mle <- mu.star
##   W.mle <- W.star
##   sigmasq.mle <- sigmasq.star
##   names(mu.mle) <- colnames(T)
##   rownames(W.mle) <- colnames(T)
##   data.mle <- data.star
##   ## scores.mle <- data.star %*% W.mle  # fixed 2018/11/08
##   scores.mle <- data.star %*% solve(Sigma.unscaled.star) %*% W.mle

##   rm(iter, params.star.update, L1.update)

##   ## start mcmc
##   mu.mcmc <- matrix(NA_real_, nrow = length(mu.mle), ncol= niter.mcmc)
##   W.mcmc <- W.rotate.mcmc <- array(NA_real_,
##                                    dim = c(dim(W.mle), niter.mcmc),
##                                    dimnames = c(dimnames(W.mle), list())
##                                    )
##   sigmasq.mcmc <- rep(NA_real_, niter.mcmc)
##   scores.mcmc <- array(NA_real_, dim = c(nrow(data.mle), Q, niter.mcmc), list())
##   scores.rotate.mcmc <-
##     array(NA_real_, dim = c(nrow(data.mle), Q, niter.mcmc), list())

##   ## prior
##   if (prior == 'default'){
##     prior <- list(
##       mode = c(rep(0, D * (1 + Q)), # everything but sigmasq
##                .5 # sigmasq, should be in [0,1] w/ standardized vars
##                ),
##       vcov = diag(
##         c(rep(1, D * (1 + Q)),
##           .5
##           )
##       )
##     )
##     prior$vcov.inv <- solve(prior$vcov)
##   } else if (prior == 'none'){
##     prior <- NULL
##   } else if (is.list(prior) && all(c('mode', 'vcov') %in% names(prior))){
##     prior$vcov.inv <- solve(prior$vcov)
##   } else {
##     message('setting prior to NULL')
##   }

##   digits <- ceiling(log(niter.mcmc + 1, 10))
##   message('MCMC draw ', rep(' ', digits), sep = '')
##   for (iter in 1:niter.mcmc){
##     message(rep('\b', digits), sprintf('%' %.% digits %.% 'd', iter), sep = '')

##     ## redraw missing data after converting back to observed scale
##     data.star <-
##       sample.missing.data.given.params(
##         T,
##         mu.star,
##         Sigma.star,
##         mle = TRUE
##       )

##     ## revise data center
##     center.star <- colMeans(data.star)
##     ## revise data scaling: standardize for pca, scale by sqrt(p) for mca
##     ##   when considering sampled missing values, p = mean(value)
##     ##   in cases where a bad imputation draw takes mean below zero,
##     ##   use observed-data p for that iteration-feature to avoid crashing
##     scale.star <- rep(NA, ncol(T))
##     scale.star[scaling == 'pca'] <-
##       apply(data.star[,scaling == 'pca', drop = FALSE], 2, sd, na.rm = TRUE)
##     scale.star[scaling == 'mca'] <-
##       sqrt(colMeans(data.star[,scaling == 'mca', drop = FALSE], na.rm = TRUE))
##     scale.star[scaling == 'mca' & !is.finite(scale.star)] <- sqrt(
##       colMeans(T[,scaling == 'mca' & !is.finite(scale.star), drop = FALSE],
##                na.rm = TRUE
##                )
##     )
##     ## rescale
##     data.star <- scale(data.star,
##                        center = center.star,
##                        scale = scale.star
##                        )

##     ## sample from param posterior conditional on full data
##     param.posterior <- param.posterior.given.full.data(data.star, Q, prior)
##     params.star <- tryCatch(
##       expr = mvrnorm(1, param.posterior$mode, param.posterior$vcov),
##       error = function(e){
##         warning('caught error while sampling parameters:\n  ',
##                 gsub('Error in ', '', e),
##                 'reattempting with truncated-eigenvalue PD reconstruction ',
##                 'of covariance matrix'
##                 )
##         Sigma.eigen <- eigen(param.posterior$vcov,
##                              symmetric = TRUE
##                              )
##         Sigma.closestpd <-
##           Sigma.eigen$vectors %*%
##           diag(pmax(Sigma.eigen$values, 0),
##                nrow = length(Sigma.eigen$values)
##                ) %*%
##           solve(Sigma.eigen$vectors)
##         mvrnorm(1, param.posterior$mode, Sigma.closestpd)
##       })
##     mu.star <-
##       scale.star * params.star[grep('^mu', names(params.star))] + center.star
##     W.star <- matrix(params.star[grep('^W', names(params.star))], D, Q)

##     ## W is only identified up to an arbitrary rotation matrix;
##     ##   rotate W.star the direction that most closely aligns with mle
##     W.rotate.star <- procrustes(W.star, W.mle)$X.new
##     W.rotate.mcmc[,,iter] <- W.rotate.star
##     ## residual variance
##     sigmasq.star <- params.star[grep('^sigmasq', names(params.star))]
##     Sigma.unscaled.star <- tcrossprod(W.star) + sigmasq.star * diag(D)
##     Sigma.star <- diag(scale.star) %*% Sigma.unscaled.star %*% diag(scale.star)

##     ## store results from this iter
##     mu.mcmc[,iter] <- mu.star
##     W.mcmc[,,iter] <- W.star

##     scores.mle <- data.star %*% solve(Sigma.unscaled.star) %*% W.star
##     scores.vcov <- diag(Q) - t(W.star) %*% solve(Sigma.unscaled.star) %*% W.star
##     scores.rotate.mle <-
##       data.star %*% solve(Sigma.unscaled.star) %*% W.rotate.star
##     scores.rotate.vcov <-
##       diag(Q) - t(W.rotate.star) %*% solve(Sigma.unscaled.star) %*% W.rotate.star

##     ## scores.mcmc[,,iter] <- data.star %*% W.star  # fixed 2018/11/08
##     ## scores.rotate.mcmc[,,iter] <- data.star %*% W.rotate.star
##     scores.mcmc[,,iter] <-
##       scores.mle + mvrnorm(nrow(scores.mle), rep(0, Q), scores.vcov)
##     scores.rotate.mcmc[,,iter] <-
##       scores.rotate.mle + mvrnorm(nrow(scores.rotate.mle), rep(0, Q), scores.vcov)

##     ## residual variance unexplained by first Q dimensions
##     sigmasq.mcmc[iter] <- sigmasq.star

##   }
##   message('\n')

##   out <- list(
##     mle = list(mu = mu.mle,
##                W = W.mle,
##                sigmasq = sigmasq.mle,
##                scores = scores.mle,
##                data = data.mle
##                ),
##     mcmc = list(mu = mu.mcmc,
##                 W = W.mcmc,
##                 W.rotate = W.rotate.mcmc,
##                 sigmasq = sigmasq.mcmc,
##                 scores = scores.mcmc,
##                 scores.rotate = scores.rotate.mcmc
##                 )
##   )

##   if (any(all.missing)){
##     attr(out, 'na.action') <- missing.ind
##     attr(out, 'class') <- 'omit'
##   }

##   return(out)

## }



plot.bpca <- function(bpca.out,
                      dimensions = c(1, 2),
                      legend = FALSE,
                      rotate = TRUE,
                      thresh = .1,
                      xlim = NULL,
                      ylim = NULL,
                      cols = NULL,
                      est.pch = 4,
                      sample.pch = '.',
                      main = NULL
                      ){

  D <- length(bpca.out$mle$mu)
  dimnames <- names(bpca.out$mle$mu)
  if (is.null(xlim)){
    xlim <- c(-1.5, 1.5)
  }
  if (is.null(ylim)){
    ylim <- c(-1.5, 1.5)
  }
  if (is.null(cols)){
    cols <- brewer.pal(D, 'Set1')
  }

  plot(NA,
       xlim = xlim,
       ylim = ylim,
       asp = 1,
       xlab = paste("PC", dimensions[1]),
       ylab = paste("PC", dimensions[2]),
       bty = "n",
       main = main
       )
  abline(v = 0, lty = 2)
  abline(h = 0, lty = 2)

  dist.from.origin <- sqrt(rowSums(bpca.out$mle$W[,dimensions]^2))
  dimensions.to.plot <- which(dist.from.origin > thresh)
  dimensions.to.plot <-
    dimensions.to.plot[order(dist.from.origin[dimensions.to.plot])]

  for (d in dimensions.to.plot){

    ## posterior samples
    points(if (rotate){
      t(bpca.out$mcmc$W.rotate[d, dimensions,])
    } else {
      t(bpca.out$mcmc$W[d, dimensions,])
    },
    col = cols[d] %.% '40',
    pch = sample.pch,
    cex = 2
    )

    ## posterior means / vcov
    if (rotate){
      mu <- rowMeans(bpca.out$mcmc$W.rotate[d, dimensions,])
      Sigma <- cov(t(bpca.out$mcmc$W.rotate[d, dimensions,]))
    } else {
      mu <- rowMeans(bpca.out$mcmc$W[d, dimensions,])
      Sigma <- cov(t(bpca.out$mcmc$W[d, dimensions,]))
    }
    points(t(mu),
           col = cols[d] %.% '80',
           lwd = 5,
           pch = est.pch,
           cex = 1
           )
    lines(ellipse(mu, Sigma, radius = qnorm(.975)), col = cols[d], lty = 2)
  }

  for (d in dimensions.to.plot){
    if (rotate){
      mu <- rowMeans(bpca.out$mcmc$W.rotate[d, dimensions,])
    } else {
      mu <- rowMeans(bpca.out$mcmc$W[d, dimensions,])
    }
    thigmophobe.labels(
      mu[1],
      mu[2],
      labels = dimnames[d],
      col = cols[d]
    )
  }

  if (legend){
    legend('topright',
           pch = 15,
           pt.cex = 5,
           cex = 2,
           col = cols,
           legend = dimnames
           )
  }

}

