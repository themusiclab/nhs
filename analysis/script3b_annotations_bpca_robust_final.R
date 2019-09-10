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
library(raster)

## dirs
data.dir <- file.path('..', 'data', 'nhs')
results.dir <- file.path('..', 'results')

## functions
source(file.path('..', 'functions', 'bpca_functions.R'))
`%.%` <- paste0
pval.1side.to.2side <- function(p){
    2 * pmin(p, 1 - p)
}

chain <- as.numeric(commandArgs(trailingOnly = TRUE))



###################
## read nhs data ##
###################

eth.prim <- read.csv(file.path(data.dir, 'NHSEthnography_AnnotatePrim.csv'))
eth.sec <- read.csv(file.path(data.dir, 'NHSEthnography_AnnotateSec.csv'))
eth.text <- read.csv(file.path(data.dir, 'NHSEthnography_FreeText.csv'))
eth.scrape <- read.csv(file.path(data.dir, 'NHSEthnography_Scraping.csv'))
eth.author <- read.csv(file.path(data.dir, 'NHSEthnography_Authors.csv'))

## merge primary, secondary, text
eth <- cbind(eth.prim,
             eth.sec[-match('indx', colnames(eth.sec))],
             eth.text[-match('indx', colnames(eth.text))]
             )

eth$text <- stringi::stri_trans_general(eth$text, 'latin-ascii')

## ## verify that only duplicate column is 'indx'
## colnames(eth.prim)[colnames(eth.prim) %in% c(colnames(eth.sec), colnames(eth.text))]
## colnames(eth.sec)[colnames(eth.sec) %in% c(colnames(eth.prim), colnames(eth.text))]
## colnames(eth.text)[colnames(eth.text) %in% c(colnames(eth.prim), colnames(eth.sec))]



###################
## summary stats ##
###################

culture.by.cite <- table(eth$id_nhs,
                         eth$cite_text_manual
                         )

## number of docs
length(unique(eth$cite_text_manual))

## words per description
words <- strsplit(unique(eth$text), ' +')
nwords <- sapply(words, length)
sum(nwords)
summary(nwords)

## number of docs for each culture
cites.per.culture <- rowSums(culture.by.cite > 0)
summary(cites.per.culture)

## number of passages for each culture
passages.per.culture <- rowSums(culture.by.cite)
summary(passages.per.culture)



###############################################################
## for coauthored docs, compute average of coauthor metadata ##
###############################################################

eth.author$author1_sex[eth.author$author1_sex %in% c('Unknown', '')] <- NA
eth.author$author2_sex[eth.author$author2_sex %in% c('Unknown', '')] <- NA
eth.author$author3_sex[eth.author$author3_sex %in% c('Unknown', '')] <- NA
eth.author$author4_sex[eth.author$author4_sex %in% c('Unknown', '')] <- NA

eth.author$author1_female <- eth.author$author1_sex == 'Female'
eth.author$author2_female <- eth.author$author2_sex == 'Female'
eth.author$author3_female <- eth.author$author3_sex == 'Female'
eth.author$author4_female <- eth.author$author4_sex == 'Female'

eth.author$author1_birthyear <- as.numeric(eth.author$author1_birthyear)
eth.author$author2_birthyear <- as.numeric(eth.author$author2_birthyear)
eth.author$author3_birthyear <- as.numeric(eth.author$author3_birthyear)
eth.author$author4_birthyear <- as.numeric(eth.author$author4_birthyear)

eth.author$author_female <- rowMeans(
  eth.author[,grep('author\\d_female', colnames(eth.author))],
  na.rm = TRUE
)
eth.author$author_birthyear <- rowMeans(
  eth.author[,grep('author\\d_birthyear', colnames(eth.author))],
  na.rm = TRUE
)
eth.author$author_age <-
  as.numeric(eth.author$pubyear) - eth.author$author_birthyear
eth.author$author_age[which(eth.author$author_age < 20)] <- NA
eth.author$author_age[which(eth.author$author_age > 80)] <- NA
## link using clean cite_text from scraped dataset
eth.scrape$author_female <- eth.author$author_female[
  match(eth.scrape$cite_text, eth.author$cite_text)
]
eth.scrape$author_age <- eth.author$author_age[
  match(eth.scrape$cite_text, eth.author$cite_text)
]
eth.scrape$pubyear <- eth.author$pubyear[
  match(eth.scrape$cite_text, eth.author$cite_text)
]
## then link to main dataset with consistent indx
eth$author_female <- eth.scrape$author_female[match(eth$indx, eth.scrape$indx)]
eth$author_age <- eth.scrape$author_age[match(eth$indx, eth.scrape$indx)]
eth$pubyear <- eth.scrape$pubyear[match(eth$indx, eth.scrape$indx)]



########################
## binary annotations ##
########################

## all binary annotations
annotations.binary <- c('singers_leader',
                        'singers_dance',
                        'audience_dance',
                        'religious',
                        'trance',
                        'ceremony',
                        'informal',
                        'appear',
                        'restrict',
                        'mimic',
                        'child_by',
                        'child_for',
                        'compose',
                        'improv',
                        'clap',
                        'stomp',
                        'instrument'
                        )

## for some binary annotations, false is never recorded
##   i.e. if missingness is excluded for these, there is no variation
## solution: reinterpret variable to mean 'annotated feature was visibly present'
##   and recode missingness indicator '.' to 0
annotations.binary.nozero <- c(
  'religious',
  'trance',
  'appear',
  'restrict',
  'mimic',
  'child_by',
  'child_for',
  'improv',
  'instrument'
)
for (j in annotations.binary.nozero){
  eth[eth[,j] == '.', j] <- '0'
  eth[,j] <- as.numeric(eth[,j])
}

## for remaining binary annotations (with both true and false values recorded)
##   code '.' as na
annotations.binary.yeszero <-
  annotations.binary[!annotations.binary %in% annotations.binary.nozero]
for (j in annotations.binary.yeszero){
  eth[eth[,j] == '.', j] <- NA
  eth[,j] <- as.numeric(eth[,j])
}



##############################################
## categorical annotations of low dimension ##
##############################################

annotations.categorical <- c('singers_sex',
                             'nowords',
                             'time_start',
                             'time_end',
                             'duration',
                             'recur',
                             'singers_n',
                             'singers_age1',
                             'singers_age2',
                             'shape_type',
                             'appear_paint',
                             'appear_adorn',
                             'appear_cloth',
                             'appear_mask',
                             'appear_obj',
                             'restrict_sex',
                             'restrict_marry',
                             'restrict_grp1',
                             'restrict_grp2',
                             'audience_n',
                             'audience_age1',
                             'audience_age2',
                             'audience_sex',
                             'audience_marry',
                             'audience_grp1',
                             'audience_grp2',
                             'instrument_type1',
                             'instrument_type2',
                             'instrument_type3'
                             )

## ignore categorical annotations with missingness >90%,
##   (nowords, duration, recur, shape_type, appear_*, restrict_marry)
##   (restrict_grp* dropped b/c 85% missingness and large dimension)
##   (audience_grp* dropped b/c 78% missingness and large ambiguous dimension)
round(
    sort(sapply(annotations.categorical, function(x) mean(eth[,x] == '.'))),
    2
)

## create male/female singer dummies
eth$dummy_singer_sex_male <- eth$singers_sex %in% c('Both sexes', 'Male')
eth$dummy_singer_sex_female <- eth$singers_sex %in% c('Both sexes', 'Female')
eth$dummy_singer_sex_male[eth$singers_sex == '.'] <-
  eth$dummy_singer_sex_female[eth$singers_sex == '.'] <- NA

## create time dummies
times <- unique(c(eth$time_start, eth$time_end))
times <- times[times != '.']
times.clean <- gsub('.*(\\d{4}) to (\\d{4}).*', '\\1.to.\\2', times)
times <- times[order(times.clean)]
time.start.matrix <- sapply(times, function(time){
  eth$time_start == time
})
time.end.matrix <- sapply(times, function(time){
  eth$time_end == time
})
## if both start & end times are given, set dummy for intervening periods to 1
time.matrix <- time.start.matrix | time.end.matrix
for (i in 1:nrow(eth)){
  if (sum(time.start.matrix[i,] | time.end.matrix[i,]) == 2){
    start.ind <- which(time.start.matrix[i,])
    end.ind <- which(time.end.matrix[i,])
    if (start.ind < end.ind){
      ## fill in start time to end time
      time.matrix[i, start.ind:end.ind] <- TRUE
    } else {
      ## fill in start time, wrapped to (earlier) end time
      time.matrix[i, c(1:end.ind, start.ind:length(times))] <- TRUE
    }
  }
}
colnames(time.matrix) <- 'dummy_time_' %.% times.clean
time.matrix[rowSums(time.matrix) == 0,] <- NA
eth <- cbind(eth, time.matrix)

## create solo/multiple singer dummy
eth$dummy_singers_multiple <- TRUE
eth$dummy_singers_multiple[eth$singers_n == 'Solo singer'] <- FALSE
eth$dummy_singers_multiple[eth$singers_n == '.'] <- NA

## create singer age-group dummy
ages <- sort(unique(c(eth$singers_age1, eth$singers_age2)))
ages <- ages[ages != '.']
age.matrix <- sapply(ages, function(age){
    eth$singers_age1 == age | eth$singers_age2 == age
})
colnames(age.matrix) <- 'dummy_singer_age_' %.% make.names(tolower(ages))
age.matrix[rowSums(age.matrix) == 0,] <- NA
eth <- cbind(eth, age.matrix)

## create numeric log-age of primary singer
eth$numeric_singer_logage <- log(c(
    "Child" = 7.5,
    "Adolescent/young adult" = 15,
    "Adult" = 30,
    "Elder" = 60
))[eth$singers_age1]

## create audience log-size variable based on approx midpoint of size range
audience.lo <- c(
    '.' = NA,
    'Solo listener' = 1,
    '2-5 listeners' = 2,
    'Multiple singers (number unknown)' = 2,  # set at most common nonsingle
    '6-10 listeners' = 6,
    '11-20 listeners' = 11,
    '21-30 listeners' = 21,
    '76-100 listeners' = 76,
    '>100 listeners' = 150                    # set at ~1.5-2x previous
)
audience.hi <- c(
    '.' = NA,
    'Solo listener' = 1,
    '2-5 listeners' = 5,
    'Multiple singers (number unknown)' = 5,  # set at most common nonsingle
    '6-10 listeners' = 10,
    '11-20 listeners' = 20,
    '21-30 listeners' = 30,
    '76-100 listeners' = 100,
    '>100 listeners' = 150                    # set at ~1.5-2x previous
    )
eth$numeric_audience_logsize <-
    log((audience.lo[eth$audience_n] + audience.hi[eth$audience_n]) / 2)

## create numeric log-age of primary audience
eth$numeric_audience_logage <- log(c(
    'Infant or toddler' = 3.75,
    "Child" = 7.5,
    "Adolescent/young adult" = 15,
    'All ages' = 22.5,              # arbitrarily set between adolescent & adult
    "Adult" = 30,
    "Elder" = 60
))[eth$audience_age1]

## create dummy for child-directed song
eth$dummy_audience_child <-
    eth$audience_age1 %in% c('Infant or toddler', 'Child') |
    eth$audience_age2 %in% c('Infant or toddler', 'Child')
eth$dummy_audience_child[eth$audience_age1 == '.'] <- NA

## create male/female audience dummies
eth$dummy_audience_sex_male <- eth$audience_sex %in% c('Both sexes', 'Male')
eth$dummy_audience_sex_female <- eth$audience_sex %in% c('Both sexes', 'Female')
eth$dummy_audience_sex_male[eth$audience_sex == '.'] <-
  eth$dummy_audience_sex_female[eth$audience_sex == '.'] <- NA

## create instrument dummies, merge idio/membrano into percussion
instruments <- sort(unique(c(
    eth$instrument_type1, eth$instrument_type2, eth$instrument_type3
)))
instruments <- instruments[instruments != '.']
instrument.matrix <- sapply(instruments, function(instrument){
    eth$instrument_type1 == instrument |
        eth$instrument_type2 == instrument |
        eth$instrument_type3 == instrument
})
colnames(instrument.matrix) <-
    'dummy_instrument_' %.% make.names(tolower(instruments))
instrument.matrix <- cbind(
    instrument.matrix,
    dummy_instrument_percussion =
        instrument.matrix[,'dummy_instrument_idiophone'] |
        instrument.matrix[,'dummy_instrument_membranophone']
)
instrument.matrix <- instrument.matrix[
   ,-grep('idiophone|membranophone', colnames(instrument.matrix))
]
instrument.matrix[rowSums(instrument.matrix) == 0,] <- NA
eth <- cbind(eth, instrument.matrix)



####################################
## annotations based on ocm codes ##
####################################

## find all non-null triggers
ocm.codes <- sort(unique(tolower(c(
  eth$trigger1, eth$trigger2, eth$trigger3,
  eth$context1, eth$context2, eth$context3,
  eth$function1, eth$function2, eth$function3,
  eth$content1, eth$content2, eth$content3, eth$content4
))))
ocm.codes <- ocm.codes[ocm.codes != '.']

## create trigger dummy matrix
trigger.matrix <- sapply(ocm.codes, function(ocm.code){
  tolower(eth$trigger1) == ocm.code |
    tolower(eth$trigger2) == ocm.code |
    tolower(eth$trigger3) == ocm.code
})
colnames(trigger.matrix) <-
  'dummy_trigger_' %.% tolower(gsub('\\.+', '.', make.names(ocm.codes)))
trigger.matrix[rowSums(trigger.matrix) == 0,] <- NA
eth <- cbind(eth, trigger.matrix)

## create context dummy matrix
context.matrix <- sapply(ocm.codes, function(ocm.code){
  tolower(eth$context1) == ocm.code |
    tolower(eth$context2) == ocm.code |
    tolower(eth$context3) == ocm.code
})
colnames(context.matrix) <-
  'dummy_context_' %.% tolower(gsub('\\.+', '.', make.names(ocm.codes)))
context.matrix[rowSums(context.matrix) == 0,] <- NA
eth <- cbind(eth, context.matrix)

## create function dummy matrix
function.matrix <- sapply(ocm.codes, function(ocm.code){
  tolower(eth$function1) == ocm.code |
    tolower(eth$function2) == ocm.code |
    tolower(eth$function3) == ocm.code
})
colnames(function.matrix) <-
  'dummy_function_' %.% tolower(gsub('\\.+', '.', make.names(ocm.codes)))
function.matrix[rowSums(function.matrix) == 0,] <- NA
eth <- cbind(eth, function.matrix)

## create content dummy matrix
content.matrix <- sapply(ocm.codes, function(ocm.code){
  eth$content1 == ocm.code |
    eth$content2 == ocm.code |
    eth$content3 == ocm.code |
    eth$content4 == ocm.code
})
colnames(content.matrix) <-
  'dummy_content_' %.% tolower(gsub('\\.+', '.', make.names(ocm.codes)))
content.matrix[rowSums(content.matrix) == 0,] <- NA
eth <- cbind(eth, content.matrix)

## generate pooled ocm matrix
trigger.matrix.noNA <- trigger.matrix
trigger.matrix.noNA[is.na(trigger.matrix.noNA)] <- FALSE
context.matrix.noNA <- context.matrix
context.matrix.noNA[is.na(context.matrix.noNA)] <- FALSE
function.matrix.noNA <- function.matrix
function.matrix.noNA[is.na(function.matrix.noNA)] <- FALSE
content.matrix.noNA <- content.matrix
content.matrix.noNA[is.na(content.matrix.noNA)] <- FALSE
ocm.matrix <- trigger.matrix.noNA | context.matrix.noNA | function.matrix.noNA
ocm.matrix[is.na(trigger.matrix) &
           is.na(context.matrix) &
           is.na(function.matrix)
           ] <- NA
colnames(ocm.matrix) <-
    'dummy_ocm_' %.% tolower(gsub('\\.+', '.', make.names(ocm.codes)))
eth <- cbind(eth, ocm.matrix)



## observe that trigger, context, function, and content are generally correlated
##   (except function <-> content)
cor(as.numeric(trigger.matrix),
    as.numeric(context.matrix),
    use = 'complete'
    )
cor(as.numeric(trigger.matrix),
    as.numeric(function.matrix),
    use = 'complete'
    )
cor(as.numeric(trigger.matrix),
    as.numeric(content.matrix),
    use = 'complete'
    )
cor(as.numeric(context.matrix),
    as.numeric(function.matrix),
    use = 'complete'
    )
cor(as.numeric(context.matrix),
    as.numeric(content.matrix),
    use = 'complete'
    )
cor(as.numeric(function.matrix),
    as.numeric(content.matrix),
    use = 'complete'
    )



############################
## identifying song types ##
############################

hypotheses <- read.csv(file.path(data.dir, 'universality_hypotheses.csv'))

dance.regex <- gsub(', ',
                    '|',
                    hypotheses$Target.word.list[
                      hypotheses$Hypothesis.short.name == 'dance'
                    ]
                    )
healing.regex <- gsub(', ',
                      '|',
                      hypotheses$Target.word.list[
                        hypotheses$Hypothesis.short.name == 'healing'
                      ]
                      )
lullaby.regex <- gsub(', ',
                      '|',
                      hypotheses$Target.word.list[
                        hypotheses$Hypothesis.short.name == 'lullabies'
                      ]
                      )
love.regex <- gsub(', ',
                   '|',
                   hypotheses$Target.word.list[
                     hypotheses$Hypothesis.short.name == 'love'
                   ]
                   )

eth$function.dance <-
  grepl('\\b(' %.% dance.regex %.% ')\\b', eth$text, 'latin-ascii') |
  eth$singers_dance | eth$audience_dance | eth$dummy_ocm_dance

eth$function.healing <-
  grepl('\\b(' %.% healing.regex %.% ')\\b', eth$text, 'latin-ascii') |
  eth$dummy_ocm_sickness.medical.care.and.shamans |
  eth$dummy_ocm_sorcery.creating.sickness.or.bad.luck.

eth$function.lullaby <-
  grepl('\\b(' %.% lullaby.regex %.% ')\\b', eth$text, 'latin-ascii') |
  eth$dummy_ocm_infancy.and.childhood |
  eth$audience_age1 == 'Infant or toddler' |
  eth$child_for

eth$function.love <-
  grepl('\\b(' %.% love.regex %.% ')\\b', eth$text, 'latin-ascii') |
  eth$dummy_ocm_marriage

## eliminate songs that fall into multiple categories
song.functions <- c('function.dance',
                    'function.healing',
                    'function.lullaby',
                    'function.love'
                    )
multifunction.ind <- which(rowSums(eth[,song.functions], na.rm = TRUE) > 1)
eth[multifunction.ind, song.functions] <- NA
eth$song.function <- apply(
  eth[,song.functions],
  1,
  function(x){
    out <- gsub('function.', '', names(which(x)))
    if (length(out) != 1){
      out <- 'other'
    }
    return(out)
  })

table(eth$song.function)



#########################
## final preprocessing ##
#########################

feature.names <- c(sort(annotations.binary),
                   colnames(eth)[grep('dummy', colnames(eth))],
                   colnames(eth)[grep('numeric', colnames(eth))]
                   )

## drop individual trigger/context/function ocm dummies b/c dimension too high
feature.names <- feature.names[
    -grep('dummy_(trigger|function|context|content)', feature.names)
]

## for main analysis,
##   drop features with almost no variation in nonmissing obs
##   (this only affects ocm code 'heat' which has no realizations)
feature.names <-
  feature.names[apply(eth[,feature.names], 2, var, na.rm = TRUE) > 0]

## for robustness, use all remaining features
feature.names.robustness <- feature.names

## for main analysis, drop features with extreme rarity or missingness
feature.is.nonbinary <- grepl('numeric', feature.names)
feature.names <-  # rare binary annotations
  feature.names[feature.is.nonbinary |
                  colMeans(eth[, feature.names], na.rm = TRUE) > .05
                ]
feature.names <-  # highly missing annotations
  feature.names[colMeans(is.na(eth[, feature.names])) < .8]

## keep track of non-binary annotations
feature.is.nonbinary.robustness <- grepl('numeric', feature.names.robustness)
feature.is.nonbinary.main <- grepl('numeric', feature.names)

## raw data (handle rescaling within new bpca function)
X.main <- as.matrix(eth[,feature.names.robustness])
colnames(X.main) <- gsub('numeric_|dummy_', '', colnames(X.main))
feature.names.robustness <- gsub('numeric_|dummy_', '', feature.names.robustness)
feature.names <- gsub('numeric_|dummy_', '', feature.names)

## for "factor analysis for mixed data":
##   standardize continuous variables as usual
##   rescale dummies with multiple correspondence analysis transformation
##   cbind and pca
X.num <- X.main[,feature.names.robustness[feature.is.nonbinary.robustness]]
X.num <- scale(X.num)
X.cat <- X.main[,feature.names.robustness[!feature.is.nonbinary.robustness]]
X.cat <- scale(X.cat,
               center = TRUE,
               scale = sqrt(colMeans(X.cat, na.rm = TRUE))
               )
X.famd <- cbind(X.num, X.cat)[,feature.names.robustness]

## select number of dimensions by optimal hard thresholding using the
##   full data to keep ndim constant across main/robust analyses
Sigma.naive <- cov(X.famd, use = 'pairwise.complete.obs')
Sigma.naive.eigen <- eigen(Sigma.naive)
sv.thresh <- 2.858 * median(Sigma.naive.eigen$values)
Q <- sum(Sigma.naive.eigen$values > sv.thresh)



#################
## do the mcmc ##
#################

ndraws <- 1000

## cambridge zip codes
seeds <- 02138

robustness <- TRUE

if (robustness){
  feature.is.nonbinary <- feature.is.nonbinary.robustness
  X <- X.main
} else {
  feature.is.nonbinary <- feature.is.nonbinary.main
  X <- X.main[,feature.names]
}

famd.fnames <- file.path(
  results.dir,
  sprintf('ethno_bpca_mcmc_Q%s_draw%s_seed%05d_famd_final_%s.rds',
          Q,
          ndraws,
          seeds,
          ifelse(robustness, 'robust', 'main')
          )
)

if (!file.exists(famd.fnames[chain])){
  set.seed(seeds[chain])
  mod.bpca <- bpca(X,
                   Q = Q,
                   maxiter.em = 100,
                   niter.mcmc = ndraws,
                   scaling = ifelse(feature.is.nonbinary,
                                    'pca',
                                    'mca'
                                    )
                   )
  saveRDS(mod.bpca, famd.fnames[chain])
}



#########################################
## merge chains after dropping burn-in ##
#########################################

mod.bpca <- readRDS(famd.fnames)

burn <- 1:200
mod.bpca$mcmc <- list(
    mu = mod.bpca$mcmc$mu[, -burn],
    W.rotate = mod.bpca$mcmc$W.rotate[,, -burn],
    scores.rotate = mod.bpca$mcmc$scores.rotate[,, -burn],
    sigmasq = mod.bpca$mcmc$sigmasq[-burn]
)

D <- ncol(X)
var.expl <- colSums(mod.bpca$mle$W^2)
var.total <- sum(var.expl) + D * mod.bpca$mle$sigmasq
round(var.expl / sum(var.expl), 3)
round(var.expl / var.total, 3)
sum(var.expl / var.total)

dimnames(mod.bpca$mcmc$scores.rotate) <- list(
  indx = eth$indx,
  dim = 'Q.' %.% 1:Q,
  sample = NULL
)

scores.postmean <- apply(mod.bpca$mcmc$scores.rotate, 1:2, mean)
colnames(scores.postmean) <- 'score.' %.% 1:Q

write.csv(data.frame(id_nhs = eth$id_nhs,
                     indx = eth$indx,
                     song.function = eth$song.function,
                     scores.postmean
                     ),
          file.path(results.dir, 'ethno_bpca_scores_final_robust.csv'),
          row.names = FALSE
          )



#######################
## interpret results ##
#######################

## interpret dimensions by most significantly contributing variables
interpretation <- ldply(
    1:ncol(mod.bpca$mle$W),
    function(q) {
    est <- apply(mod.bpca$mcmc$W.rotate[,q,], 1, mean)
    se <- apply(mod.bpca$mcmc$W.rotate[,q,], 1, sd)
    z <- est / se
    ind <- order(z)
    varnames <- rownames(mod.bpca$mle$W)[ind]
    out <- data.frame(variable = varnames,
                      miss = colMeans(is.na(X[,varnames])),
                      freq = ifelse(
                        feature.is.nonbinary.robustness[ind],
                        NA,
                        colMeans(X[,varnames] == 1, na.rm = TRUE)
                      ),
                      dim = q,
                      est = round(est[ind], 2),
                      se = round(se[ind], 2),
                      z = round(z[ind], 2)
                      )
    rownames(out) <- NULL
    out
    })

## ## print to console: top/bottom 10 contributing annotations to each dimension
## for (q in 1:Q){
##     cat('\n', rep('=', 80), '\n\nDIMENSION ', q, '\n\n', sep = '')
##     interpretation.q <- interpretation[interpretation$dim == q,]
##     interpretation.q$var <- substr(interpretation.q$variable, 1, 47)
##     interpretation.q$var <- interpretation.q$var %.%
##         ifelse(nchar(interpretation.q$variable) > 47, '...', '')
##     rownames(interpretation.q) <-
##       sapply(interpretation.q$var, function(x){
##         x %.% paste(rep(' ', 50 - nchar(x)), collapse = '')
##       })
##     cat('most negatively contributing:\n')
##     print(head(interpretation.q[,c('est', 'se', 'z')], 10),
##           digits = 3,
##           width = 3
##           )
##     cat('\n---\n\n')
##     cat('most positively contributing:\n')
##     print(tail(interpretation.q[,c('est', 'se', 'z')], 10),
##           digits = 3,
##           width = 3
##           )
## }

## write to file: sorted contributions from each annotation to each dimension
options(width = 100)
sink(file.path(results.dir, 'ethno_bpca_annotation_to_dimension_final_robust.txt'))
for (q in 1:Q){#dkhere
    cat('\n', rep('=', 80), '\n\nDIMENSION ', q, '\n\n', sep = '')
    interpretation.q <- interpretation[interpretation$dim == q,]
    interpretation.q$var <- substr(interpretation.q$variable, 1, 47)
    interpretation.q$var <- interpretation.q$var %.%
        ifelse(nchar(interpretation.q$variable) > 47, '...', '')
    rownames(interpretation.q) <-
      sapply(interpretation.q$var, function(x){
        x %.% paste(rep(' ', 50 - nchar(x)), collapse = '')
      })
    print(interpretation.q[,c('miss', 'freq', 'est', 'se', 'z')])
}
sink()
options(width=80)



########################
## culture mean vs sd ##
########################

ids.nhs <- sort(unique(eth$id_nhs))
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
          score.q ~ 0 + id_nhs,
          data = data.frame(
            score.q =
              mod.bpca$mcmc$scores.rotate[, q, draw] -
              mean(mod.bpca$mcmc$scores.rotate[, q, draw]),
            id_nhs = eth$id_nhs
          )
        )
        mean.star <- mvrnorm(n = nnorm.per.pca.draw,
                             mu = coef(mod.star),
                             Sigma = vcov(mod.star)
                             )
        return(mean.star)
      })
    mean.stars.q <- do.call(rbind, mean.stars.q)
    colnames(mean.stars.q) <- gsub('id_nhs', '', colnames(mean.stars.q))
    return(mean.stars.q)
  }
)
dimnames(mean.stars)[1] <- list('q.' %.% 1:Q)
dimnames(mean.stars)[2] <- list(NULL)
names(dimnames(mean.stars)) <- c('dim', 'post.draw', 'culture')
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
          ids.nhs,
          function(id){
            sd(mod.bpca$mcmc$scores.rotate[eth$id_nhs == id, q, draw])
          })
        return(sd.star)
      })
    sd.stars.q <- do.call(rbind, sd.stars.q)
    colnames(sd.stars.q) <- gsub('id_nhs', '', colnames(sd.stars.q))
    return(sd.stars.q)
  }
)
dimnames(sd.stars)[1] <- list('q.' %.% 1:Q)
dimnames(sd.stars)[2] <- list(NULL)
  names(dimnames(sd.stars)) <- c('dim', 'post.draw', 'culture')
sd.summary <- adply(sd.stars,
                    c(1, 3),
                    function(x){
                      c(sd.est = mean(x),
                        sd.cilo = unname(quantile(x, .025, na.rm = TRUE)),
                        sd.cihi = unname(quantile(x, .975, na.rm = TRUE))
                        )
                    })

## show that cultures with more cites are less extreme
culture.summary <- merge(mean.summary, sd.summary, by = c('dim', 'culture'))
culture.summary$ncites <- cites.per.culture[culture.summary$culture]
culture.summary$npassages <- passages.per.culture[culture.summary$culture]
write.csv(
  culture.summary,
  file.path(results.dir, 'ethno_bpca_culture_summary_final_robust.csv'),
  row.names = FALSE
)

sum(abs(culture.summary$mean.est / culture.summary$sd.est) > 1.96, na.rm = TRUE)



## repeat culture-mean analysis, but on document level
##   to show appearance of uniqueness in low-doc cultures is
##   simply due to doc-level variation in focus, not something
##   peculiar about the types of cultures that have little ethnography
docmean.stars <- laply(
  1:Q,  # latent dimensions to analyze
  function(q){
    ## for each latent dimension, get culture mean posterior
    mean.stars.q <- llply(
      seq(nthin, ncol(mod.bpca$mcmc$mu), nthin),  # draws to analyze
      function(draw){
        print(draw)
        ## for each posterior draw, get (culture mean posterior | scores)
        mod.star <- lm(
          score.q ~ 0 + cite_text_manual,
          data = data.frame(
            score.q =
              mod.bpca$mcmc$scores.rotate[, q, draw] -
              mean(mod.bpca$mcmc$scores.rotate[, q, draw]),
            cite_text_manual = eth$cite_text_manual
          )
        )
        mus = coef(mod.star)
        sigmas = diag(vcov(mod.star))
        ## mean.star <- mvrnorm(n = nnorm.per.pca.draw,
        ##                      mu = coef(mod.star),
        ##                      Sigma = vcov(mod.star)
        ##                      )
        mean.star <- t(replicate(
          nnorm.per.pca.draw,
          rnorm(length(mus),
                mus,
                sigmas
                )
        ))
        return(mean.star)
      })
    mean.stars.q <- do.call(rbind, mean.stars.q)
    colnames(mean.stars.q) <- gsub('cite_text_manual', '', colnames(mean.stars.q))
    return(mean.stars.q)
  }
)
dimnames(docmean.stars)[1] <- list('q.' %.% 1:Q)
dimnames(docmean.stars)[2] <- list(NULL)
names(dimnames(docmean.stars)) <- c('dim', 'post.draw', 'cite')
docmean.summary <- adply(docmean.stars,
                         c(1, 3),
                         function(x){
                           c(docmean.est = mean(x),
                             docmean.cilo = unname(quantile(x, .025)),
                             docmean.cihi = unname(quantile(x, .975)),
                             docmean.pval = pval.1side.to.2side(mean(x > 0))
                             )
                         })
docmean.summary$cite <- sort(unique(eth$cite_text_manual))[docmean.summary$cite]
docmean.summary$id_nhs <- eth$id_nhs[match(docmean.summary$cite, eth$cite_text_manual)]
docmean.summary$ncites.for.culture <- cites.per.culture[docmean.summary$id_nhs]
write.csv(
  docmean.summary,
  file.path(results.dir, 'ethno_bpca_doc_summary_final_robust.csv'),
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
          ids.nhs,
          function(id){
            mean(mod.bpca$mcmc$scores.rotate[eth$id_nhs == id, q, draw])
          }))
        ## for each posterior draw, get (within-culture sd | scores)
        var.within.star <- sapply(
          ids.nhs,
          function(id){
            var(mod.bpca$mcmc$scores.rotate[eth$id_nhs == id, q, draw])
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
  file.path(results.dir, 'ethno_bpca_within_between_ratios_robust_final.csv')
  )
