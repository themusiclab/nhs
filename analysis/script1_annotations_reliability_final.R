library(psych)

options(stringsAsFactors = FALSE)

## dirs
data.dir <- file.path('..', 'data', 'nhs')
results.dir <- file.path('..', 'results')

## functions
`%.%` <- paste0



###################
## read nhs data ##
###################

## original data
eth.prim <- read.csv(file.path(data.dir, 'NHSEthnography_AnnotatePrim.csv'))
eth.sec <- read.csv(file.path(data.dir, 'NHSEthnography_AnnotateSec.csv'))
eth.text <- read.csv(file.path(data.dir, 'NHSEthnography_FreeText.csv'))
eth <- cbind(eth.prim,
             eth.sec[-match('indx', colnames(eth.sec))],
             eth.text[-match('indx', colnames(eth.text))]
             )

## reliability
ocm.codes.reliability <-
  read.csv(file.path(data.dir, 'NHSEthnography_Reliability_OCM.csv'))
eth2 <- read.csv(file.path(data.dir, 'NHSEthnography_Reliability.csv'))
colnames(eth2) <- gsub('_rc', '', colnames(eth2))
eth2 <- eth2[order(eth2$indx),]

## subset to doubly coded data
eth <- eth[eth$indx %in% eth2$indx,]



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
  ## original
  eth[eth[,j] == '.', j] <- '0'
  eth[,j] <- as.numeric(eth[,j])
  ## reliability
  eth2[eth2[,j] == '.', j] <- '0'
  eth2[,j] <- as.numeric(eth2[,j])
}

## for remaining binary annotations (with both true and false values recorded)
##   code '.' as na
annotations.binary.yeszero <-
  annotations.binary[!annotations.binary %in% annotations.binary.nozero]
for (j in annotations.binary.yeszero){
  ## original
  eth[eth[,j] == '.', j] <- NA
  eth[,j] <- as.numeric(eth[,j])
  ## reliability
  eth2[eth2[,j] == '.', j] <- NA
  eth2[,j] <- as.numeric(eth2[,j])
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

## create male/female singer dummies (original)
eth$dummy_singer_sex_male <- eth$singers_sex %in% c('Both sexes', 'Male')
eth$dummy_singer_sex_female <- eth$singers_sex %in% c('Both sexes', 'Female')
eth$dummy_singer_sex_male[eth$singers_sex == '.'] <-
  eth$dummy_singer_sex_female[eth$singers_sex == '.'] <- NA
## create male/female singer dummies (reliability)
eth2$dummy_singer_sex_male <- eth2$singers_sex %in% c('Both sexes', 'Male')
eth2$dummy_singer_sex_female <- eth2$singers_sex %in% c('Both sexes', 'Female')
eth2$dummy_singer_sex_male[eth2$singers_sex == '.'] <-
  eth2$dummy_singer_sex_female[eth2$singers_sex == '.'] <- NA

## possible time values
times <- unique(c(eth$time_start, eth$time_end))
times <- times[times != '.']
times.clean <- gsub('.*(\\d{4}) to (\\d{4}).*', '\\1.to.\\2', times)
times <- times[order(times.clean)]

## create time dummies (original)
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

## create time dummies (reliability)
time.start.matrix <- sapply(times, function(time){
  eth2$time_start == time
})
time.end.matrix <- sapply(times, function(time){
  eth2$time_end == time
})
## if both start & end times are given, set dummy for intervening periods to 1
time.matrix <- time.start.matrix | time.end.matrix
for (i in 1:nrow(eth2)){
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
eth2 <- cbind(eth2, time.matrix)

## create solo/multiple singer dummy (original)
eth$dummy_singers_multiple <- TRUE
eth$dummy_singers_multiple[eth$singers_n == 'Solo singer'] <- FALSE
eth$dummy_singers_multiple[eth$singers_n == '.'] <- NA

## create solo/multiple singer dummy (reliability)
eth2$dummy_singers_multiple <- TRUE
eth2$dummy_singers_multiple[eth2$singers_n == 'Solo singer'] <- FALSE
eth2$dummy_singers_multiple[eth2$singers_n == '.'] <- NA

## create singer age-group dummy (original)
ages <- sort(unique(c(eth$singers_age1, eth$singers_age2)))
ages <- ages[ages != '.']
age.matrix <- sapply(ages, function(age){
    eth$singers_age1 == age | eth$singers_age2 == age
})
colnames(age.matrix) <- 'dummy_singer_age_' %.% make.names(tolower(ages))
age.matrix[rowSums(age.matrix) == 0,] <- NA
eth <- cbind(eth, age.matrix)

## create singer age-group dummy (reliability)
age.matrix <- sapply(ages, function(age){
    eth2$singers_age1 == age | eth2$singers_age2 == age
})
colnames(age.matrix) <- 'dummy_singer_age_' %.% make.names(tolower(ages))
age.matrix[rowSums(age.matrix) == 0,] <- NA
eth2 <- cbind(eth2, age.matrix)

## create numeric log-age of primary singer (original)
eth$numeric_singer_logage <- log(c(
    "Child" = 7.5,
    "Adolescent/young adult" = 15,
    "Adult" = 30,
    "Elder" = 60
))[eth$singers_age1]

## create numeric log-age of primary singer (reliability)
eth2$numeric_singer_logage <- log(c(
    "Child" = 7.5,
    "Adolescent/young adult" = 15,
    "Adult" = 30,
    "Elder" = 60
))[eth2$singers_age1]

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
## original
eth$numeric_audience_logsize <-
    log((audience.lo[eth$audience_n] + audience.hi[eth$audience_n]) / 2)
## reliability
eth2$numeric_audience_logsize <-
    log((audience.lo[eth2$audience_n] + audience.hi[eth2$audience_n]) / 2)

## create numeric log-age of primary audience (original)
eth$numeric_audience_logage <- log(c(
    'Infant or toddler' = 3.75,
    "Child" = 7.5,
    "Adolescent/young adult" = 15,
    'All ages' = 22.5,              # arbitrarily set between adolescent & adult
    "Adult" = 30,
    "Elder" = 60
))[eth$audience_age1]

## create numeric log-age of primary audience (reliability)
eth2$numeric_audience_logage <- log(c(
    'Infant or toddler' = 3.75,
    "Child" = 7.5,
    "Adolescent/young adult" = 15,
    'All ages' = 22.5,              # arbitrarily set between adolescent & adult
    "Adult" = 30,
    "Elder" = 60
))[eth2$audience_age1]

## create dummy for child-directed song (original)
eth$dummy_audience_child <-
    eth$audience_age1 %in% c('Infant or toddler', 'Child') |
    eth$audience_age2 %in% c('Infant or toddler', 'Child')
eth$dummy_audience_child[eth$audience_age1 == '.'] <- NA

## create dummy for child-directed song (reliability)
eth2$dummy_audience_child <-
    eth2$audience_age1 %in% c('Infant or toddler', 'Child') |
    eth2$audience_age2 %in% c('Infant or toddler', 'Child')
eth2$dummy_audience_child[eth2$audience_age1 == '.'] <- NA

## create male/female audience dummies (original)
eth$dummy_audience_sex_male <- eth$audience_sex %in% c('Both sexes', 'Male')
eth$dummy_audience_sex_female <- eth$audience_sex %in% c('Both sexes', 'Female')
eth$dummy_audience_sex_male[eth$audience_sex == '.'] <-
  eth$dummy_audience_sex_female[eth$audience_sex == '.'] <- NA

## create male/female audience dummies (reliability)
eth2$dummy_audience_sex_male <- eth2$audience_sex %in% c('Both sexes', 'Male')
eth2$dummy_audience_sex_female <- eth2$audience_sex %in% c('Both sexes', 'Female')
eth2$dummy_audience_sex_male[eth2$audience_sex == '.'] <-
  eth2$dummy_audience_sex_female[eth2$audience_sex == '.'] <- NA

## create instrument dummies, merge idio/membrano into percussion (original)
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

## create instrument dummies, merge idio/membrano into percussion (reliability)
instrument.matrix <- sapply(instruments, function(instrument){
    eth2$instrument_type1 == instrument |
        eth2$instrument_type2 == instrument |
        eth2$instrument_type3 == instrument
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
eth2 <- cbind(eth2, instrument.matrix)



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
ocm.matrix <- trigger.matrix.noNA | context.matrix.noNA | function.matrix.noNA | content.matrix.noNA
ocm.matrix[is.na(trigger.matrix) &
             is.na(context.matrix) &
             is.na(function.matrix) &
             is.na(content.matrix)
           ] <- NA
colnames(ocm.matrix) <-
  'dummy_ocm_' %.% tolower(gsub('\\.+', '.', make.names(ocm.codes)))
eth <- cbind(eth, ocm.matrix)

## reliability
ocm.matrix <- eth2[,'ocm' %.% ocm.codes.reliability$code]
colnames(ocm.matrix) <- tolower(ocm.codes.reliability$label)
ocm.matrix <- ocm.matrix[,ocm.codes]
colnames(ocm.matrix) <-
  'dummy_ocm_' %.% tolower(gsub('\\.+', '.', make.names(ocm.codes)))
ocm.matrix[rowSums(ocm.matrix) == 0,] <- NA
eth2 <- cbind(eth2, ocm.matrix)



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


reliability <- sapply(
  feature.names[-grep('ocm', feature.names)],
  function(x){
    print(x)
    y <- na.omit(cbind(eth[,x], eth2[,x]))
    if (any(colSums(y) == 0)){
      return(c(NA, NA, 0))
    }
    if (nrow(y) == 0){
      return(c(NA, NA, 0))
    } else {
      return(c(alpha(na.omit(y))$total$raw_alpha,
               cor(y[,1], y[,2]),
               nrow(y))
             )
    }
  })
reliability <- t(reliability)
colnames(reliability) <- c('alpha', 'corr', 'n')
write.csv(reliability,
          file.path(results.dir, 'ethno_reliability.csv'),
          row.names = FALSE
          )


round(median(reliability[,'alpha']), 3)
round(range(reliability[,'alpha']), 3)
round(median(reliability[,'corr']), 3)
round(range(reliability[,'corr']), 3)
