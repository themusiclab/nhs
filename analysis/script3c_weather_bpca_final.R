options(stringsAsFactors = FALSE)

## parallel
library(foreach)
library(doMC)
registerDoMC(cores = 4)

## data manipulation
library(abind)
library(reshape2)
library(xtable)

## math
library(MASS)
library(mvtnorm)   # for drawing from mvrnorm()
library(MCMCpack)  # for procrustes() rotation

## graphics
library(car)       # for scatterplotMatrix() visualization
library(ggplot2)
## library(plotrix)

## dirs
data.dir <- file.path('..', 'data', 'gsoy')
results.dir <- file.path('..', 'results')

## functions
source(file.path('..', 'functions', 'bpca_functions.R'))
`%.%` <- paste0
pval.1side.to.2side <- function(p){
    2 * pmin(p, 1 - p)
}



####################
## read gsoy data ##
####################

weather <- read.csv(file.path(data.dir, 'gsoy-trim.csv'))

id.vars <- c('country_name', 'country_code', 'station', 'date', 'name',
             'latitude', 'longitude', 'elevation'
             )

features.integer <- c(
  'dp01', 'dp10', 'dt00', 'dt32', 'dx32', 'dx70', 'dx90'## ,
  ## 'dsnd', 'dsnw', 'emsd', 'emsn', 'snow', 'tsun', 'wdmv',
)

features.integer.raw <- c('dp01', 'dp10')
features.integer.log <- c('dsnd', 'dsnw', 'dt00', 'dt32', 'dx32', 'dx70',
                          'dx90')
features.numeric.raw <- c('emnt', 'emxt', 'htdd', 'tavg', 'tmax', 'tmin')
features.numeric.log <- c('emxp', 'prcp', 'cldd')
features.numeric.nlog <- c('fzf0', 'fzf1', 'fzf5', 'fzf6', 'fzf2', 'fzf7',
                           'fzf8', 'fzf9', 'fzf3', 'fzf4')

for (feature in features.integer.log){
  weather[,feature] <- log(weather[,feature] + 1)
}
for (feature in features.numeric.log){
  weather[,feature] <- log(weather[,feature] + 1)
}
for (feature in features.numeric.nlog){
  weather[,feature] <- log(-weather[,feature] + 1)
}

feature.names <- c(features.integer.raw,
                   features.integer.log,
                   features.numeric.raw,
                   features.numeric.log,
                   features.numeric.nlog
                   )

par(mfrow = c(4, 7))
for (feature in feature.names){
  hist(weather[,feature],
       main = sprintf('%s: %0.2f NA',
                      feature,
                      mean(is.na(weather[,feature]))
                      )
       )
}



## raw data (handle rescaling within new bpca function)
X <- as.matrix(weather[,feature.names])
X.scale <- scale(X)


## select number of dimensions by optimal hard thresholding using the
##   full data to keep ndim constant across main/robust analyses
Sigma.naive <- cov(X.scale, use = 'pairwise.complete.obs')
Sigma.naive.eigen <- eigen(Sigma.naive)
sv.thresh <- 2.858 * median(Sigma.naive.eigen$values)
Q <- sum(Sigma.naive.eigen$values > sv.thresh)



#################
## do the mcmc ##
#################

ndraws <- 1000

options(error = recover)

fname <- file.path(
  results.dir,
  sprintf('weather_bpca_mcmc_Q%s_draw%s_famd_final.rds',
          Q,
          ndraws
          )
)

if (!file.exists(fname)){
  set.seed(02139)
  mod.bpca <- bpca(X,
                   Q = Q,
                   maxiter.em = 100,
                   niter.mcmc = ndraws,
                   scaling = rep('pca', length(feature.names))
                   )
  saveRDS(mod.bpca, fname)
} else {
  mod.bpca <- readRDS(fname)
}

scores.postmean <- apply(mod.bpca$mcmc$scores.rotate, 1:2, mean)
colnames(scores.postmean) <- 'score.' %.% 1:Q
write.csv(
  data.frame(country_code = weather$country_code,
             station = weather$station,
             date = weather$date,
             scores.postmean
             ),
  file.path(results.dir, 'weather_bpca_scores_final.csv'),
  row.names = FALSE
)



#############################
## mcmc mixing diagnostics ##
#############################

D <- ncol(X)
var.expl <- colSums(mod.bpca$mle$W^2)
var.total <- sum(var.expl) + D * mod.bpca$mle$sigmasq
sum(var.expl) / var.total

var.expl.mle <- colSums(mod.bpca$mle$W^2)
var.noise.mle <- D * mod.bpca$mle$sigmasq
var.noise.mle / sum(var.expl.mle)

## examine all chains in entirety
examine.ind <- 1:ndraws

## examine mixing of residual variance
plot(mod.bpca$mcmc$sigmasq[examine.ind],
     type = 'l'
     )

## examine mixing of means
par(mfrow = c(4, 7), oma = rep(0, 4), mar = rep(0, 4))
for (j in 1:length(feature.names)){
  plot(mod.bpca$mcmc$mu[j,examine.ind],
       type = 'l',
       ylim = range(c(0, mod.bpca$mcmc$mu[j,examine.ind]))
       )
  abline(h = 0, col = 'red')
}

## examine mixing of factor loadings
for (q in 1:Q){
  par(mfrow = c(4, 7), oma = rep(0, 4), mar = rep(0, 4))
  for (j in 1:length(feature.names)){
    plot(mod.bpca$mcmc$W.rotate[j, q, examine.ind],
         type = 'l',
         ylim = range(c(0, mod.bpca$mcmc$W[j, q, examine.ind]))
         )
    abline(h = 0, col = 'red')
  }
  readline(paste(
    'currently plotting dimension ',
    q,
    ', press <return> for next dimension\n',
    sep = ''
  ))
}



##########################################
## within- vs between- country variance ##
##########################################

## number of citations and passages for each culture
country.by.station <- table(weather$country_code,
                         weather$station
                         )
stations.per.country <- rowSums(country.by.station > 0)  # number of unique books
stationyears.per.country <- rowSums(country.by.station)   # number of passages

country.codes <- sort(unique(weather$country_code))
nthin <- 10
nnorm.per.pca.draw <- 100

## latent dimension x posterior draw x culture
mean.stars <- laply(
  1:Q,  # latent dimensions to analyze
  function(q){
    ## for each latent dimension, get culture mean posterior
    mean.stars.q <- llply(
      seq(nthin, ncol(mod.bpca$mcmc$mu), nthin),  # draws to analyze
      function(draw){
        ## for each posterior draw, get (culture mean posterior | scores)
        mod.star <- lm(
          score.q ~ 0 + country_code,
          data = data.frame(
            score.q =
              mod.bpca$mcmc$scores.rotate[, q, draw] -
              mean(mod.bpca$mcmc$scores.rotate[, q, draw]),
            country_code = weather$country_code
          )
        )
        mean.star <- mvrnorm(n = nnorm.per.pca.draw,
                           mu = coef(mod.star),
                           Sigma = vcov(mod.star)
                           )
        return(mean.star)
      })
    mean.stars.q <- do.call(rbind, mean.stars.q)
    colnames(mean.stars.q) <- gsub('country_code', '', colnames(mean.stars.q))
    return(mean.stars.q)
  }
)
dimnames(mean.stars)[1] <- list('q.' %.% 1:Q)
dimnames(mean.stars)[2] <- list(NULL)
names(dimnames(mean.stars)) <- c('dim', 'post.draw', 'country')
mean.summary <- adply(mean.stars,
                      c(1, 3),
                      function(x){
                        c(mean.est = mean(x),
                          mean.cilo = unname(quantile(x, .025)),
                          mean.cihi = unname(quantile(x, .975)),
                          mean.pval = pval.1side.to.2side(mean(x > 0))
                          )
                      })


## latent dimension x posterior draw x culture
sd.stars <- laply(
  1:Q,  # latent dimensions to analyze
  function(q){
    ## for each latent dimension, get within-culture sd posterior
    sd.stars.q <- llply(
      seq(nthin, ncol(mod.bpca$mcmc$mu), nthin),  # draws to analyze
      function(draw){
        ## for each posterior draw, get (within-culture sd | scores)
        sd.star <- sapply(
          country.codes,
          function(id){
            sd(mod.bpca$mcmc$scores.rotate[weather$country_code == id, q, draw])
          })
        return(sd.star)
      })
    sd.stars.q <- do.call(rbind, sd.stars.q)
    colnames(sd.stars.q) <- gsub('country_code', '', colnames(sd.stars.q))
    return(sd.stars.q)
  }
)
dimnames(sd.stars)[1] <- list('q.' %.% 1:Q)
dimnames(sd.stars)[2] <- list(NULL)
  names(dimnames(sd.stars)) <- c('dim', 'post.draw', 'country')
sd.summary <- adply(sd.stars,
                    c(1, 3),
                    function(x){
                      c(sd.est = mean(x),
                        sd.cilo = unname(quantile(x, .025, na.rm = TRUE)),
                        sd.cihi = unname(quantile(x, .975, na.rm = TRUE))
                        )
                    })

## loop over cultures and analyze distribution overlap with other group means
overlap.summary <- ldply(
  sort(unique(weather$country_code)),
  function(id){
    ## loop over dimensions
    ldply(
      1:Q,
      function(q){
        ## distribution of scores for this country and dimension
        country.distr <- mod.bpca$mcmc$scores.rotate[weather$country_code == id, q,]
        country.distr.lo <- unname(quantile(country.distr, .025))
        country.distr.hi <- unname(quantile(country.distr, .975))
        ## average value of various other groups,
        ##   excluding this country and those like it (per various groupings)
        other.country.mean <- mean(
          mod.bpca$mcmc$scores.rotate[weather$country_code != id, q,],
          na.rm = TRUE
        )
        ## return
        return(data.frame(
          dim = 'q.' %.% q,
          country = id,
          distr.lo = country.distr.lo,
          distr.hi = country.distr.hi,
          other.country.mean
        ))
      })
  })

ggplot(overlap.summary,
       aes(x = country,
           ymin = distr.lo,
           ymax = distr.hi
           )
       ) +
  geom_errorbar(width = 0) +
  geom_point(aes(y = other.country.mean, shape = 'mean excluding this country')) +
  coord_flip() +
  facet_wrap('dim', ncol = 3) +
  xlab(NULL) +
  ylab('position on latent dimension') +
  theme_light(20) +
  theme(legend.position = 'bottom')

country.summary <- merge(mean.summary, sd.summary, by = c('dim', 'country'))
country.summary <- merge(country.summary, overlap.summary, by = c('dim', 'country'))
country.summary$nstations <- stations.per.country[country.summary$country]
country.summary$nstationyears <- stationyears.per.country[country.summary$country]

write.csv(
  country.summary,
  file.path(results.dir, 'weather_bpca_country_summary_final.csv'),
  row.names = FALSE
)

## repeat culture-mean analysis, but on document level
##   to show appearance of uniqueness in low-doc cultures is
##   simply due to doc-level variation in focus, not something
##   peculiar about the types of cultures that have little ethnography
stationmean.stars <- laply(
  1:Q,  # latent dimensions to analyze
  function(q){
    ## for each latent dimension, get culture mean posterior
    mean.stars.q <- llply(
      seq(nthin, ncol(mod.bpca$mcmc$mu), nthin),  # draws to analyze
      function(draw){
        print(draw)
        ## for each posterior draw, get (culture mean posterior | scores)
        mod.star <- lm(
          score.q ~ 0 + station,
          data = data.frame(
            score.q =
              mod.bpca$mcmc$scores.rotate[, q, draw] -
              mean(mod.bpca$mcmc$scores.rotate[, q, draw]),
            station = weather$station
          )
        )
        mus = coef(mod.star)
        sigmas = diag(vcov(mod.star))
        mean.star <- t(replicate(
          nnorm.per.pca.draw,
          rnorm(length(mus),
                mus,
                sigmas
                )
        ))
        colnames(mean.star) <- gsub('station', '', names(coef(mod.star)))
        return(mean.star)
      })
    mean.stars.q <- do.call(rbind, mean.stars.q)
    return(mean.stars.q)
  }
)

dimnames(stationmean.stars)[1] <- list('q.' %.% 1:Q)
dimnames(stationmean.stars)[2] <- list(NULL)
names(dimnames(stationmean.stars)) <- c('dim', 'post.draw', 'station')
stationmean.summary <- adply(stationmean.stars,
                         c(1, 3),
                         function(x){
                           c(stationmean.est = mean(x),
                             stationmean.cilo = unname(quantile(x, .025)),
                             stationmean.cihi = unname(quantile(x, .975)),
                             stationmean.pval = pval.1side.to.2side(mean(x > 0))
                             )
                         })
stationmean.summary$country_code <- weather$country_code[match(stationmean.summary$station, weather$station)]
stationmean.summary$ncites.for.culture <- cites.per.culture[stationmean.summary$id_nhs]
write.csv(
  stationmean.summary,
  file.path(results.dir, 'weather_bpca_station_summary_final.csv'),
  row.names = FALSE
)

## latent dimension x posterior draw x culture
ratio.stars <- laply(
  1:Q,  # latent dimensions to analyze
  function(q){
    ## for each latent dimension, get within-culture sd posterior
    ratio.stars.q <- llply(
      seq(nthin, ncol(mod.bpca$mcmc$mu), nthin),  # draws to analyze
      function(draw){
        var.between.star <- var(sapply(
          country.codes,
          function(id){
            mean(mod.bpca$mcmc$scores.rotate[weather$country_code == id, q, draw])
          }))
        ## for each posterior draw, get (within-culture sd | scores)
        var.within.star <- sapply(
          country.codes,
          function(id){
            var(mod.bpca$mcmc$scores.rotate[weather$country_code == id, q, draw])
          })
        return(
            mean(var.within.star, na.rm = TRUE) / var.between.star
        )
      })
    ratio.stars.q <- do.call(c, ratio.stars.q)
    ## colnames(ratio.stars.q) <- gsub('id_nhs', '', colnames(ratio.stars.q))
    return(ratio.stars.q)
  }
)
## posterior summaries for within-to-between ratio (and vice versa)
within.to.between.ratio.est <- rowMeans(ratio.stars)
within.to.between.ratio.ci <- t(apply(ratio.stars, 1, quantile, c(.025, .975)))
colnames(within.to.between.ratio.ci) <- c('cilo', 'cihi')
between.to.within.ratio.est <- rowMeans(1 / ratio.stars)
between.to.within.ratio.ci <- t(apply(1 / ratio.stars, 1, quantile, c(.025, .975)))
colnames(between.to.within.ratio.ci) <- c('cilo', 'cihi')
## export
write.csv(
  data.frame(within.to.between.est = within.to.between.ratio.est,
             within.to.between = within.to.between.ratio.ci,
             between.to.within.est = between.to.within.ratio.est,
             between.to.within = between.to.within.ratio.ci
             ),
  file.path(results.dir, 'weather_bpca_within_between_ratios_final.csv')
  )
