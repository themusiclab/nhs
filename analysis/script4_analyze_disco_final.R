# This script addresses concerns about region dependencies and
# addresses them with alternative groupings that vary in coarseness.

options(stringsAsFactors = FALSE)

## math
library(psych)    # for cronbach's alpha
library(binom)
library(glmnet)

## parallel
library(foreach)
library(doMC)
registerDoMC(cores = 7)

## data manipulation
library(reshape2)
library(xtable)
library(plyr)

## graphics
library(ggplot2)
library(car)

## dirs
data.dir <- file.path('..', 'data', 'nhs')
results.dir <- file.path('..', 'results')

## functions
`%.%` <- paste0

source(file.path('..', 'functions', 'bpca_functions.R'))

pval.1side.to.2side <- function(p){
    2 * pmin(p, 1 - p)
}

options(stringsAsFactors = FALSE)




####################################################
## read discography data for song-type prediction ##
####################################################

disco.meta <- read.csv(file.path(data.dir, 'NHSDiscography_Metadata.csv'))
song.type.lookup <- disco.meta$type
names(song.type.lookup) <- disco.meta$song

## four categories of song features for classification:
##   - average ratings by naive listeners
##   - raw ratings by experts, which will be averaged
##   - automatically extracted features from music21/jsymbolic transcriptions
##   - automatically extracted features from raw audio
disco.naive <-
  read.csv(file.path(data.dir, 'NHSDiscography_NaiveListeners.csv'))
disco.expert.raw <-
  read.csv(file.path(data.dir, 'NHSDiscography_Annotate.csv'))
disco.transcription <-
  read.csv(file.path(data.dir, 'NHSDiscography_TranscriptionFeatures.csv'))
disco.mir <-
  read.csv(file.path(data.dir, 'NHSDiscography_MIRFeatures.csv'))
disco.mir.panteli <- # separate df for the analysis with the panteli features
  read.csv(file.path(data.dir, 'NHSDiscography_MIRFeatures.csv'))
disco.culture.meta <-
  read.csv(file.path(data.dir, 'NHSCultures_Metadata.csv'))

###################################
## process expert-based features ##
###################################

disco.expert.raw$micrometer_duple <-
  disco.expert.raw$micrometer %in% c('Duple', 'Both duple and triple')
disco.expert.raw$micrometer_triple <-
  disco.expert.raw$micrometer %in% c('Triple', 'Both duple and triple')

## pool macrometer into groups (values other than 2, 3, or 4 are rare)
disco.expert.raw$macrometer_duple <-
  disco.expert.raw$macrometer_2 |
  disco.expert.raw$macrometer_4 |
  disco.expert.raw$macrometer_6 |
  disco.expert.raw$macrometer_8 |
  disco.expert.raw$macrometer_10 |
  disco.expert.raw$macrometer_12 |
  disco.expert.raw$macrometer_14 |
  ifelse(disco.expert.raw$macrometer_other == '.',
         FALSE,
         as.numeric(disco.expert.raw$macrometer_other) %% 2 == 0
         )
disco.expert.raw$macrometer_triple <-
  disco.expert.raw$macrometer_3 |
  disco.expert.raw$macrometer_6 |
  disco.expert.raw$macrometer_9 |
  disco.expert.raw$macrometer_12 |
  disco.expert.raw$macrometer_15 |
  ifelse(disco.expert.raw$macrometer_other == '.',
         FALSE,
         as.numeric(disco.expert.raw$macrometer_other) %% 3 == 0
         )

## break out variation by type
disco.expert.raw$variation_melodic <-
  disco.expert.raw$repeat_vary %in% c('Melodic variation',
                                      'Rhythmic and melodic variation'
                                      )
disco.expert.raw$variation_rhythmic <-
  disco.expert.raw$repeat_vary %in% c('Rhythmic variation',
                                      'Rhythmic and melodic variation'
                                      )

disco.expert.raw$tension <-
    disco.expert.raw$tension_melody +
    disco.expert.raw$tension_harmony +
    disco.expert.raw$tension_rhythm +
    disco.expert.raw$tension_motif +
    disco.expert.raw$tension_accent +
    disco.expert.raw$tension_dynamic

disco.expert.raw$dynamics <- c(
    'Gets louder' = 1,
    'Multiple dynamics' = 1,
    'No dynamics' = 0,
    'Quiets down' = 1
)[disco.expert.raw$dynamics]

disco.expert.raw$ritard_accel <- c(
    'No ritard or accel' = 0,
    'Slows down' = 1,
    'Speeds up' = 1,
    'Speeds up and slows down' = 1
)[disco.expert.raw$ritard]

## major/minor scale, single scale
disco.expert.raw$scale_quality_minor <- c(
  'Unknown' = NA,
  'Major' = 0,
  'Minor' = 1
)[disco.expert.raw$scale_quality]

features.expert <- c(
  'tempo_adj',
  'macrometer_ord',
  'syncopate',
  'accent',
  'dynamics',
  'ritard_accel',
  'micrometer_duple',
  'micrometer_triple',
  'macrometer_duple',
  'macrometer_triple',
  'variation_rhythmic',
  'variation_melodic',
  'ornament',
  'vibrato',
  'tension',
  'scale_quality_minor'
)

## aggregate annotators, append song metadata
disco.expert.raw.melt <- melt(
    disco.expert.raw[c('song', 'annotator', features.expert)],
    id.vars = c('song', 'annotator'),
    variable.name = 'feature'
)
disco.expert <- dcast(disco.expert.raw.melt,
                          song ~ feature,
                          fun.aggregate = function(x){
                            mean(x, na.rm = TRUE)
                          })

## inter-coder reliability on each feature
annotations.alpha <- sapply(
  features.expert,
  function(feature){
    cat(feature, '\n')
    annotation.by.coder <-
      acast(disco.expert.raw.melt[disco.expert.raw.melt$feature == feature,],
            song ~ annotator
            )
    psych::alpha(annotation.by.coder)$total$raw_alpha
})
as.matrix(sort(annotations.alpha))
mean(annotations.alpha)

## append song region
disco.expert$nhs_region <- disco.meta$nhs_region[
  match(sprintf('NHSDiscography-%03d', disco.expert$song),
        disco.meta$song
        )
]

## append song function
disco.expert$song_function <- disco.meta$type[
  match(sprintf('NHSDiscography-%03d', disco.expert$song),
        disco.meta$song
        )
]
disco.expert$song_function <- factor(disco.expert$song_function)

## create ow/nw var
disco.expert$culture <- disco.meta$culture[
  match(sprintf('NHSDiscography-%03d', disco.expert$song),
        disco.meta$song
        )
]
disco.expert$ow_nw <- disco.culture.meta$ow_nw[match(disco.expert$culture,
                                                     disco.culture.meta$culture)]
## some string match problems bc accents, hardcoding because R&R deadline is looming
disco.expert$ow_nw[disco.expert$culture == "Emberá"] <-
    disco.culture.meta$ow_nw[disco.culture.meta$culture == "EmberÃ¡"]
disco.expert$ow_nw[disco.expert$culture == "Javaé"] <-
    disco.culture.meta$ow_nw[disco.culture.meta$culture == "JavaÃ©"]

## append hraf song region
disco.expert$hraf_region <- disco.meta$hraf_region[
  match(sprintf('NHSDiscography-%03d', disco.expert$song),
        disco.meta$song
        )
]

## append hraf song subregion
disco.expert$hraf_subregion <- disco.meta$hraf_subregion[
  match(sprintf('NHSDiscography-%03d', disco.expert$song),
        disco.meta$song
        )
]

## append nhs subsistence 
disco.expert$nhs_subsistence <- disco.meta$nhs_subsistence[
  match(sprintf('NHSDiscography-%03d', disco.expert$song),
        disco.meta$song
        )
]


########################################
## parse transcription-based features ##
########################################

features.transcription <- c(
  'mean_interval',
  'distance_btwn_modal_intervals',
  'common_intervals_count',
  'stepwise_motion',
  'melodic_thirds',
  'duration_of_melodic_arcs',
  'size_of_melodic_arcs',
  'rel_strength_top_pitchcls',
  'interval_btwn_strongest_pitchcls',
  'pitch_class_variety',
  'range',
  'note_density',
  'average_note_duration',
  'modal_interval_prevalence',
  'rel_strength_modal_intervals',
  'amount_of_arpeggiation',
  'direction_of_motion',
  'modal_pitchcls_prev',
  'initial_tempo',
  'quality'
)

## subset columns to song id and features of interest
disco.transcription <- disco.transcription[, c('song', features.transcription)]

## append song region
disco.transcription$nhs_region <- disco.meta$nhs_region[
  match(sprintf('NHSDiscography-%03d', disco.transcription$song),
        disco.meta$song
        )
]

## append song function
disco.transcription$song_function <- disco.meta$type[
  match(sprintf('NHSDiscography-%03d', disco.transcription$song),
        disco.meta$song
        )
]
disco.transcription$song_function <- factor(disco.transcription$song_function)

## create ow/nw var
disco.transcription$culture <- disco.meta$culture[
  match(sprintf('NHSDiscography-%03d', disco.transcription$song),
        disco.meta$song
        )
]
disco.transcription$ow_nw <- disco.culture.meta$ow_nw[match(disco.transcription$culture,
                                                     disco.culture.meta$culture)]
## some string match problems bc accents, hardcoding because R&R deadline is looming
disco.transcription$ow_nw[disco.transcription$culture == "Emberá"] <-
    disco.culture.meta$ow_nw[disco.culture.meta$culture == "EmberÃ¡"]
disco.transcription$ow_nw[disco.transcription$culture == "Javaé"] <-
    disco.culture.meta$ow_nw[disco.culture.meta$culture == "JavaÃ©"]

## append hraf song region
disco.transcription$hraf_region <- disco.meta$hraf_region[
  match(sprintf('NHSDiscography-%03d', disco.transcription$song),
        disco.meta$song
        )
]

## append hraf song subregion
disco.transcription$hraf_subregion <- disco.meta$hraf_subregion[
  match(sprintf('NHSDiscography-%03d', disco.transcription$song),
        disco.meta$song
        )
]

## append nhs subsistence 
disco.transcription$nhs_subsistence <- disco.meta$nhs_subsistence[
  match(sprintf('NHSDiscography-%03d', disco.transcription$song),
        disco.meta$song
        )
]


###########################################
## process music info retrieval features ##
###########################################

features.mir <- colnames(disco.mir)[-match('song', colnames(disco.mir))]
## subset to 14s extracts
features.mir <- features.mir[grep('^ex_', features.mir)]
## drop sampling rate
features.mir <- features.mir[-grep('sampling_rate', features.mir)]
## drop simple centroid mean due to NAs
features.mir <- features.mir[-grep('simple_centroid_mean', features.mir)]
## drop mfcc derivatives
features.mir <- features.mir[-grep('d+mfcc', features.mir)]
## drop mfcc subbands
features.mir <- features.mir[-grep('subband', features.mir)]

## subset columns to song id and features of interest
disco.mir <- disco.mir[, c('song', features.mir)]

## append song region
disco.mir$nhs_region <- disco.meta$nhs_region[
  match(sprintf('NHSDiscography-%03d', disco.mir$song),
        disco.meta$song
        )
]

## append song function
disco.mir$song_function <- disco.meta$type[
  match(sprintf('NHSDiscography-%03d', disco.mir$song),
        disco.meta$song
        )
]
disco.mir$song_function <- factor(disco.mir$song_function)


## create ow/nw var
disco.mir$culture <- disco.meta$culture[
  match(sprintf('NHSDiscography-%03d', disco.mir$song),
        disco.meta$song
        )
]
disco.mir$ow_nw <- disco.culture.meta$ow_nw[match(disco.mir$culture,
                                                     disco.culture.meta$culture)]
## some string match problems bc accents, hardcoding because R&R deadline is looming
disco.mir$ow_nw[disco.mir$culture == "Emberá"] <-
    disco.culture.meta$ow_nw[disco.culture.meta$culture == "EmberÃ¡"]
disco.mir$ow_nw[disco.mir$culture == "Javaé"] <-
    disco.culture.meta$ow_nw[disco.culture.meta$culture == "JavaÃ©"]

## append hraf song region
disco.mir$hraf_region <- disco.meta$hraf_region[
  match(sprintf('NHSDiscography-%03d', disco.mir$song),
        disco.meta$song
        )
]

## append hraf song subregion
disco.mir$hraf_subregion <- disco.meta$hraf_subregion[
  match(sprintf('NHSDiscography-%03d', disco.mir$song),
        disco.meta$song
        )
]

## append nhs subsistence 
disco.mir$nhs_subsistence <- disco.meta$nhs_subsistence[
  match(sprintf('NHSDiscography-%03d', disco.mir$song),
        disco.meta$song
        )
]


#################################################
## create separate MIR features using pantelli ##
#################################################

features.mir.panteli <- colnames(disco.mir.panteli)[-match('song', colnames(disco.mir.panteli))]
## subset to 14s extracts
features.mir.panteli <- features.mir.panteli[grepl('^ex_', features.mir.panteli) | grepl('panteli', features.mir.panteli)]
## drop sampling rate
features.mir.panteli <- features.mir.panteli[-grep('sampling_rate', features.mir.panteli)]
## drop simple centroid mean due to NAs
features.mir.panteli <- features.mir.panteli[-grep('simple_centroid_mean', features.mir.panteli)]
## drop mfcc derivatives
features.mir.panteli <- features.mir.panteli[-grep('d+mfcc', features.mir.panteli)]
## drop mfcc subbands
features.mir.panteli <- features.mir.panteli[-grep('subband', features.mir.panteli)]

# features.mir.panteli adds the 840 audio features defined in Panteli et al. (2017) to the MIR dataset
print(length(features.mir.panteli) - length(features.mir))

## subset columns to song id and features of interest
disco.mir.panteli <- disco.mir.panteli[, c('song', features.mir.panteli)]

## append song region
disco.mir.panteli$nhs_region <- disco.meta$nhs_region[
  match(sprintf('NHSDiscography-%03d', disco.mir.panteli$song),
        disco.meta$song
        )
]

## append song function
disco.mir.panteli$song_function <- disco.meta$type[
  match(sprintf('NHSDiscography-%03d', disco.mir.panteli$song),
        disco.meta$song
        )
]
disco.mir.panteli$song_function <- factor(disco.mir.panteli$song_function)


## create ow/nw var
disco.mir.panteli$culture <- disco.meta$culture[
  match(sprintf('NHSDiscography-%03d', disco.mir.panteli$song),
        disco.meta$song
        )
]
disco.mir.panteli$ow_nw <- disco.culture.meta$ow_nw[match(disco.mir.panteli$culture,
                                                     disco.culture.meta$culture)]
## some string match problems bc accents, hardcoding because R&R deadline is looming
disco.mir.panteli$ow_nw[disco.mir.panteli$culture == "Emberá"] <-
    disco.culture.meta$ow_nw[disco.culture.meta$culture == "EmberÃ¡"]
disco.mir.panteli$ow_nw[disco.mir.panteli$culture == "Javaé"] <-
    disco.culture.meta$ow_nw[disco.culture.meta$culture == "JavaÃ©"]

## append hraf song region
disco.mir.panteli$hraf_region <- disco.meta$hraf_region[
  match(sprintf('NHSDiscography-%03d', disco.mir.panteli$song),
        disco.meta$song
        )
]

## append hraf song subregion
disco.mir.panteli$hraf_subregion <- disco.meta$hraf_subregion[
  match(sprintf('NHSDiscography-%03d', disco.mir.panteli$song),
        disco.meta$song
        )
]

## append nhs subsistence 
disco.mir.panteli$nhs_subsistence <- disco.meta$nhs_subsistence[
  match(sprintf('NHSDiscography-%03d', disco.mir.panteli$song),
        disco.meta$song
        )
]


###########################################
## process naive-listener-based features ##
###########################################

## drop 'func_' prefixed variables (listener predictions about song's function)
features.naive <- c(
  ## 'form_sing',
  ## 'form_inst',
  'form_melo',
  'form_rhyt',
  'form_fast',
  'form_beat',
  'form_exci',
  'form_happ',
  'form_plea'
)
functions.naive <- c(
  func_danc = 'Dance',
  func_heal = 'Healing',
  func_love = 'Love',
  func_baby = 'Lullaby'
)

## naive predictions
disco.naive.pred <- functions.naive[
  apply(
    disco.naive[, names(functions.naive)],
    1,
    which.max
  )
]

## subset columns to song id and features of interest
disco.naive <- disco.naive[, c('song', features.naive)]

## append song region
disco.naive$nhs_region <- disco.meta$nhs_region[
  match(sprintf('NHSDiscography-%03d', disco.naive$song),
        disco.meta$song
        )
]

## append song function
disco.naive$song_function <- disco.meta$type[
  match(sprintf('NHSDiscography-%03d', disco.naive$song),
        disco.meta$song
        )
]
disco.naive$song_function <- factor(disco.naive$song_function)


## create ow/nw var
disco.naive$culture <- disco.meta$culture[
  match(sprintf('NHSDiscography-%03d', disco.naive$song),
        disco.meta$song
        )
]
disco.naive$ow_nw <- disco.culture.meta$ow_nw[match(disco.naive$culture,
                                                     disco.culture.meta$culture)]
## some string match problems bc accents, hardcoding because R&R deadline is looming
disco.naive$ow_nw[disco.naive$culture == "Emberá"] <-
    disco.culture.meta$ow_nw[disco.culture.meta$culture == "EmberÃ¡"]
disco.naive$ow_nw[disco.naive$culture == "Javaé"] <-
    disco.culture.meta$ow_nw[disco.culture.meta$culture == "JavaÃ©"]

## append hraf song region
disco.naive$hraf_region <- disco.meta$hraf_region[
  match(sprintf('NHSDiscography-%03d', disco.naive$song),
        disco.meta$song
        )
]

## append hraf song subregion
disco.naive$hraf_subregion <- disco.meta$hraf_subregion[
  match(sprintf('NHSDiscography-%03d', disco.naive$song),
        disco.meta$song
        )
]

## append nhs subsistence 
disco.naive$nhs_subsistence <- disco.meta$nhs_subsistence[
  match(sprintf('NHSDiscography-%03d', disco.naive$song),
        disco.meta$song
        )
]




##############################
## song type classification ##
##############################

## confirm song ordering is identical in all datasets
identical(disco.naive$song, disco.expert$song)
identical(disco.expert$song, disco.transcription$song)
identical(disco.transcription$song, disco.mir$song)
identical(disco.transcription$song, disco.mir.panteli$song)

## id variables are same across all datasets (song.function is in the other dir)
song.function <- disco.naive$song_function
song.region <- disco.naive$nhs_region
ow.nw <- disco.naive$ow_nw
hraf.region <- disco.naive$hraf_region
hraf.subregion <- disco.naive$hraf_subregion
nhs.subsistence <- disco.naive$nhs_subsistence

fold.vars <- data.frame("old_world_new_world" = ow.nw,
                        "hraf_region" = hraf.region,
                        "hraf_subregion" = hraf.subregion,
                        "nhs_subsistence" = nhs.subsistence)

## label each feature with origin
disco.naive.mat <- scale(
  as.matrix(disco.naive[,features.naive])
)
colnames(disco.naive.mat) <- 'NAIVE.' %.% colnames(disco.naive.mat)
disco.expert.mat <- scale(
  as.matrix(disco.expert[,features.expert])
)
colnames(disco.expert.mat) <- 'EXPERT.' %.% colnames(disco.expert.mat)
disco.transcription.mat <- scale(
  as.matrix(disco.transcription[,features.transcription])
)
colnames(disco.transcription.mat) <-
    'TRANSCRIPTION.' %.% colnames(disco.transcription.mat)
disco.mir.mat <- scale(
  as.matrix(disco.mir[,features.mir])
)
colnames(disco.mir.mat) <- 'MIR.' %.% colnames(disco.mir.mat)
disco.mir.panteli.mat <- scale(
  as.matrix(disco.mir.panteli[,features.mir.panteli])
)
colnames(disco.mir.panteli.mat) <- 'MIR.' %.% colnames(disco.mir.panteli.mat)

## eliminate contextual features
disco.nocontext.mat <- cbind(
  disco.expert.mat[,'EXPERT.' %.% features.expert],
  disco.transcription.mat[,'TRANSCRIPTION.' %.% features.transcription]
)

##################################################
## categorical classification, split by dataset ##
##################################################

fold.vars.g1 <- c("hraf_region",
                  "hraf_subregion",
                  "nhs_subsistence")

for(fold.var in fold.vars.g1){

    ## ## naive accuracy based on song function ratings
    acc.human <- .427

    ## classify using naive ratings of song form (omitting predictions of function)
    set.seed(02139)
    mod.naive <- cv.glmnet(
        x = disco.naive.mat,
        y = factor(song.function),
        alpha = 1,
        family = 'multinomial',
        foldid = as.integer(factor(fold.vars[[fold.var]])),
        grouped = FALSE,
        standardize = TRUE,
        keep = TRUE
    )
    lambda.ind.naive <- match(mod.naive$lambda.min, mod.naive$glmnet.fit$lambda)
    predict.naive <- levels(factor(song.function))[
        apply(
            mod.naive$fit.preval[,,lambda.ind.naive],
            1,
            which.max
        )
    ]
    correct.naive <- predict.naive == song.function
    confusion.naive <- table(actual = song.function,
                             predicted = predict.naive
                             )
    acc.naive <- sum(diag(confusion.naive)) / sum(confusion.naive)

    ## classify using mir features
    set.seed(02139)
    mod.mir <- cv.glmnet(
        x = disco.mir.mat,
        y = factor(song.function),
        alpha = 1,
        family = 'multinomial',
        foldid = as.integer(factor(fold.vars[[fold.var]])),
        grouped = FALSE,
        standardize = TRUE,
        keep = TRUE
    )
    lambda.ind.mir <- match(mod.mir$lambda.min, mod.mir$glmnet.fit$lambda)
    predict.mir <- levels(factor(song.function))[
        apply(
            mod.mir$fit.preval[,,lambda.ind.mir],
            1,
            which.max
        )
    ]
    correct.mir <- predict.mir == song.function
    confusion.mir <- table(actual = song.function,
                           predicted = predict.mir
                           )
    acc.mir <- sum(diag(confusion.mir)) / sum(confusion.mir)

    ## classify using mir AND panteli features
    set.seed(02139)
    mod.mir.panteli <- cv.glmnet(
        x = disco.mir.panteli.mat,
        y = factor(song.function),
        alpha = 1,
        family = 'multinomial',
        foldid = as.integer(factor(fold.vars[[fold.var]])),
        grouped = FALSE,
        standardize = TRUE,
        keep = TRUE
    )
    lambda.ind.mir.panteli <- match(mod.mir.panteli$lambda.min,
                                    mod.mir.panteli$glmnet.fit$lambda)
    predict.mir.panteli <- levels(factor(song.function))[
        apply(
            mod.mir.panteli$fit.preval[,,lambda.ind.mir.panteli],
            1,
            which.max
        )
    ]
    correct.mir.panteli <- predict.mir.panteli == song.function
    confusion.mir.panteli <- table(actual = song.function,
                                   predicted = predict.mir.panteli
                                   )
    acc.mir.panteli <- sum(diag(confusion.mir.panteli)) / sum(confusion.mir.panteli)

    ## classify using transcriptions
    set.seed(02139)
    mod.transcription <- cv.glmnet(
        x = disco.transcription.mat,
        y = factor(song.function),
        alpha = 1,
        family = 'multinomial',
        foldid = as.integer(factor(fold.vars[[fold.var]])),
        grouped = FALSE,
        standardize = TRUE,
        keep = TRUE
    )
    lambda.ind.transcription <- match(mod.transcription$lambda.min, mod.transcription$glmnet.fit$lambda)
    predict.transcription <- levels(factor(song.function))[
        apply(
            mod.transcription$fit.preval[,,lambda.ind.transcription],
            1,
            which.max
        )
    ]
    correct.transcription <- predict.transcription == song.function
    confusion.transcription <- table(actual = song.function,
                                     predicted = predict.transcription
                                     )
    acc.transcription <- sum(diag(confusion.transcription)) / sum(confusion.transcription)

    ## classify using experts
    set.seed(02139)
    mod.expert <- cv.glmnet(
        x = disco.expert.mat,
        y = factor(song.function),
        alpha = 1,
        family = 'multinomial',
        foldid = as.integer(factor(fold.vars[[fold.var]])),
        grouped = FALSE,
        standardize = TRUE,
        keep = TRUE
    )
    lambda.ind.expert <- match(mod.expert$lambda.min, mod.expert$glmnet.fit$lambda)
    predict.expert <- levels(factor(song.function))[
        apply(
            mod.expert$fit.preval[,,lambda.ind.expert],
            1,
            which.max
        )
    ]
    correct.expert <- predict.expert == song.function
    confusion.expert <- table(actual = song.function,
                              predicted = predict.expert
                              )
    acc.expert <- sum(diag(confusion.expert)) / sum(confusion.expert)

    ## all context-free variables, which are just concatenated
    ##   (1) transcription variables and (2) expert variables
    set.seed(02139)
    mod.nocontext <- cv.glmnet(
        x = disco.nocontext.mat,
        y = factor(song.function),
        alpha = 1,
        family = 'multinomial',
        foldid = as.integer(factor(fold.vars[[fold.var]])),
        grouped = FALSE,
        standardize = TRUE,
        keep = TRUE
    )
    lambda.ind.nocontext <- match(mod.nocontext$lambda.min, mod.nocontext$glmnet.fit$lambda)
    predict.nocontext <- levels(factor(song.function))[
        apply(
            mod.nocontext$fit.preval[,,lambda.ind.nocontext],
            1,
            which.max
        )
    ]
    correct.nocontext <- predict.nocontext == song.function
    confusion.nocontext <- table(actual = song.function,
                                 predicted = predict.nocontext
                                 )
    acc.nocontext <- sum(diag(confusion.nocontext)) / sum(confusion.nocontext)
    diag(confusion.nocontext) / rowSums(confusion.nocontext)



    ## nadeau & bengio method 1 (corrected resampled t-test):
    ##   corrective factor for estimated generalization error sd
    nb1.factor <- sqrt(
        1 / length(unique(fold.vars[[fold.var]])) +
        length(unique(song.function)) / (length(song.function) - length(unique(song.function)))
    )

    ## first calculate average accuracy for each fold
    correct.fold.naive <-
        tapply(correct.naive, fold.vars[[fold.var]], mean, simplify = TRUE)
    correct.fold.mir <-
        tapply(correct.mir, fold.vars[[fold.var]], mean, simplify = TRUE)
    correct.fold.mir.panteli <-
        tapply(correct.mir.panteli, fold.vars[[fold.var]], mean, simplify = TRUE)
    correct.fold.expert <-
        tapply(correct.expert, fold.vars[[fold.var]], mean, simplify = TRUE)
    correct.fold.transcription <-
        tapply(correct.transcription, fold.vars[[fold.var]], mean, simplify = TRUE)
    correct.fold.nocontext <-
        tapply(correct.nocontext, fold.vars[[fold.var]], mean, simplify = TRUE)

    nb1.n1 <- (length(unique(fold.vars[[fold.var]])) - 1) * length(unique(song.function))
    nb1.n2 <- length(unique(song.function))
    ## approximation for correlation of fold accuracies
    nb1.rho0 <- nb1.n2 / (nb1.n1 + nb1.n2)
    nb1.factor <- sqrt(1 / length(unique(fold.vars[[fold.var]])) + nb1.rho0 / (1 - nb1.rho0))

    ## loop over each and
    feature.sets <- c('naive', 'mir', 'mir.panteli', 'expert', 'transcription', 'nocontext')
    acc.results <- data.frame(
        feature.set = feature.sets,
        accuracy = sapply(feature.sets, function(x) get('acc.' %.% x)),
        se = sapply(feature.sets, function(x) sd(get('correct.fold.' %.% x) ) * nb1.factor)
    )
    acc.results$cilo <- qnorm(.025) * acc.results$se + acc.results$accuracy
    acc.results$cihi <- qnorm(.975) * acc.results$se + acc.results$accuracy

    write.csv(do.call(rbind,
                      lapply(feature.sets,
                             function(x){
                                 out <- as.data.frame(unclass(get('confusion.' %.% x)))
                                 colnames(out) <- 'predict.' %.% colnames(out)
                                 out <- cbind(feature.set = x, actual = rownames(out), out)
                                 out
                             })
                      ),
              file.path(results.dir, paste0(fold.var, "_", 'disco_overall_confusion.csv')),
              row.names = FALSE
              )

    write.csv(acc.results,
              file.path(results.dir, paste0(fold.var, "_", 'disco_overall_acc.csv')),
              row.names = FALSE
              )
}

# Do old_world_new_world separately because you can't fit with 2 folds
fold.var <- "old_world_new_world"
fold.id <- as.factor(ow.nw)

## classify using naive ratings of song form (omitting predictions of function)
set.seed(02139)
mod.naive <- cv.glmnet(
    x = disco.naive.mat,
    y = factor(song.function),
    alpha = 1,
    family = 'multinomial',
    grouped = FALSE,
    standardize = TRUE,
    keep = TRUE
)
lambda.seq <- mod.naive$lambda

best.error <- Inf
for(lambda in lambda.seq){
    predictions.naive <- rep(NA, nrow(disco.naive.mat))
    lambda.mod.ow <- glmnet(
        x = disco.naive.mat[fold.id == "OW",],
        y = factor(song.function)[fold.id == "OW"],
        lambda = lambda,
        alpha = 1,
        family = 'multinomial',
        standardize = TRUE)
    nw.preds <- predict(lambda.mod.ow,
                        disco.naive.mat[fold.id == "NW",],
                        type = "response")
    
    lambda.mod.nw <- glmnet(
        x = disco.naive.mat[fold.id == "NW",],
        y = factor(song.function)[fold.id == "NW"],
        lambda = lambda,
        alpha = 1,
        family = 'multinomial',
        standardize = TRUE)
    ow.preds <- predict(lambda.mod.nw,
                        disco.naive.mat[fold.id == "OW",],
                        type = "response")

    total.error <- 0    
    for(n in 1:nrow(nw.preds)){
        for(k in 1:ncol(nw.preds)){
            total.error <- total.error + log(nw.preds[n,k,1]) *
                as.integer(colnames(nw.preds[,,1])[k] == factor(song.function)[fold.id == "NW"][n])
        }
    }    
    for(n in 1:nrow(ow.preds)){
        for(k in 1:ncol(ow.preds)){
            total.error <- total.error + log(ow.preds[n,k,1]) *
                as.integer(colnames(ow.preds[,,1])[k] == factor(song.function)[fold.id == "OW"][n])
        }
    }
    llk <- -total.error
    if(total.error < best.error){
        best.error <- total.error

        predictions.naive[fold.id == "OW"] <- levels(factor(song.function))[apply(ow.preds,
                                                                                  1,
                                                                                  which.max)]
        predictions.naive[fold.id == "NW"] <- levels(factor(song.function))[apply(nw.preds,
                                                                                  1,
                                                                                  which.max)]
        correct.naive <- predictions.naive == song.function
        confusion.naive <- table(actual = song.function,
                                 predicted = predictions.naive
                                 )
        acc.naive <- sum(diag(confusion.naive)) / sum(confusion.naive)
        
    }    
}

## classify using mir features
set.seed(02139)
mod.mir <- cv.glmnet(
    x = disco.mir.mat,
    y = factor(song.function),
    alpha = 1,
    family = 'multinomial',
    grouped = FALSE,
    standardize = TRUE,
    keep = TRUE
)
lambda.seq <- mod.mir$lambda

best.error <- Inf
for(lambda in lambda.seq){
    predictions.mir <- rep(NA, nrow(disco.mir.mat))
    lambda.mod.ow <- glmnet(
        x = disco.mir.mat[fold.id == "OW",],
        y = factor(song.function)[fold.id == "OW"],
        lambda = lambda,
        alpha = 1,
        family = 'multinomial',
        standardize = TRUE)
    nw.preds <- predict(lambda.mod.ow,
                        disco.mir.mat[fold.id == "NW",],
                        type = "response")
    
    lambda.mod.nw <- glmnet(
        x = disco.mir.mat[fold.id == "NW",],
        y = factor(song.function)[fold.id == "NW"],
        lambda = lambda,
        alpha = 1,
        family = 'multinomial',
        standardize = TRUE)
    ow.preds <- predict(lambda.mod.nw,
                        disco.mir.mat[fold.id == "OW",],
                        type = "response")

    total.error <- 0    
    for(n in 1:nrow(nw.preds)){
        for(k in 1:ncol(nw.preds)){
            total.error <- total.error + log(nw.preds[n,k,1]) *
                as.integer(colnames(nw.preds[,,1])[k] == factor(song.function)[fold.id == "NW"][n])
        }
    }    
    for(n in 1:nrow(ow.preds)){
        for(k in 1:ncol(ow.preds)){
            total.error <- total.error + log(ow.preds[n,k,1]) *
                as.integer(colnames(ow.preds[,,1])[k] == factor(song.function)[fold.id == "OW"][n])
        }
    }
    llk <- -total.error
    cat("\nllk\n")
    if(total.error < best.error){
        best.error <- total.error

        predictions.mir[fold.id == "OW"] <- levels(factor(song.function))[apply(ow.preds,
                                                                                  1,
                                                                                  which.max)]
        predictions.mir[fold.id == "NW"] <- levels(factor(song.function))[apply(nw.preds,
                                                                                  1,
                                                                                  which.max)]
        correct.mir <- predictions.mir == song.function
        confusion.mir <- table(actual = song.function,
                                 predicted = predictions.mir
                                 )
        acc.mir <- sum(diag(confusion.mir)) / sum(confusion.mir)
        
    }    
}

## classify using mir AND panteli features
set.seed(02139)
mod.mir.panteli <- cv.glmnet(
    x = disco.mir.panteli.mat,
    y = factor(song.function),
    alpha = 1,
    family = 'multinomial',
    grouped = FALSE,
    standardize = TRUE,
    keep = TRUE
)
lambda.seq <- mod.mir.panteli$lambda

best.error <- Inf
for(lambda in lambda.seq){
    predictions.mir.panteli <- rep(NA, nrow(disco.mir.panteli.mat))
    lambda.mod.ow <- glmnet(
        x = disco.mir.panteli.mat[fold.id == "OW",],
        y = factor(song.function)[fold.id == "OW"],
        lambda = lambda,
        alpha = 1,
        family = 'multinomial',
        standardize = TRUE)
    nw.preds <- predict(lambda.mod.ow,
                        disco.mir.panteli.mat[fold.id == "NW",],
                        type = "response")
    
    lambda.mod.nw <- glmnet(
        x = disco.mir.panteli.mat[fold.id == "NW",],
        y = factor(song.function)[fold.id == "NW"],
        lambda = lambda,
        alpha = 1,
        family = 'multinomial',
        standardize = TRUE)
    ow.preds <- predict(lambda.mod.nw,
                        disco.mir.panteli.mat[fold.id == "OW",],
                        type = "response")

    total.error <- 0    
    for(n in 1:nrow(nw.preds)){
        for(k in 1:ncol(nw.preds)){
            total.error <- total.error + log(nw.preds[n,k,1]) *
                as.integer(colnames(nw.preds[,,1])[k] == factor(song.function)[fold.id == "NW"][n])
        }
    }    
    for(n in 1:nrow(ow.preds)){
        for(k in 1:ncol(ow.preds)){
            total.error <- total.error + log(ow.preds[n,k,1]) *
                as.integer(colnames(ow.preds[,,1])[k] == factor(song.function)[fold.id == "OW"][n])
        }
    }
    llk <- -total.error
    if(total.error < best.error){
        best.error <- total.error

        predictions.mir.panteli[fold.id == "OW"] <- levels(factor(song.function))[apply(ow.preds,
                                                                                  1,
                                                                                  which.max)]
        predictions.mir.panteli[fold.id == "NW"] <- levels(factor(song.function))[apply(nw.preds,
                                                                                  1,
                                                                                  which.max)]
        correct.mir.panteli <- predictions.mir.panteli == song.function
        confusion.mir.panteli <- table(actual = song.function,
                                 predicted = predictions.mir.panteli
                                 )
        acc.mir.panteli <- sum(diag(confusion.mir.panteli)) / sum(confusion.mir.panteli)
        
    }    
}

## classify using transcriptions
set.seed(02139)
mod.transcription <- cv.glmnet(
    x = disco.transcription.mat,
    y = factor(song.function),
    alpha = 1,
    family = 'multinomial',
    grouped = FALSE,
    standardize = TRUE,
    keep = TRUE
)
lambda.seq <- mod.transcription$lambda

best.error <- Inf
for(lambda in lambda.seq){
    predictions.transcription <- rep(NA, nrow(disco.transcription.mat))
    lambda.mod.ow <- glmnet(
        x = disco.transcription.mat[fold.id == "OW",],
        y = factor(song.function)[fold.id == "OW"],
        lambda = lambda,
        alpha = 1,
        family = 'multinomial',
        standardize = TRUE)
    nw.preds <- predict(lambda.mod.ow,
                        disco.transcription.mat[fold.id == "NW",],
                        type = "response")
    
    lambda.mod.nw <- glmnet(
        x = disco.transcription.mat[fold.id == "NW",],
        y = factor(song.function)[fold.id == "NW"],
        lambda = lambda,
        alpha = 1,
        family = 'multinomial',
        standardize = TRUE)
    ow.preds <- predict(lambda.mod.nw,
                        disco.transcription.mat[fold.id == "OW",],
                        type = "response")

    total.error <- 0    
    for(n in 1:nrow(nw.preds)){
        for(k in 1:ncol(nw.preds)){
            total.error <- total.error + log(nw.preds[n,k,1]) *
                as.integer(colnames(nw.preds[,,1])[k] == factor(song.function)[fold.id == "NW"][n])
        }
    }    
    for(n in 1:nrow(ow.preds)){
        for(k in 1:ncol(ow.preds)){
            total.error <- total.error + log(ow.preds[n,k,1]) *
                as.integer(colnames(ow.preds[,,1])[k] == factor(song.function)[fold.id == "OW"][n])
        }
    }
    llk <- -total.error
    if(total.error < best.error){
        best.error <- total.error

        predictions.transcription[fold.id == "OW"] <- levels(factor(song.function))[apply(ow.preds,
                                                                                  1,
                                                                                  which.max)]
        predictions.transcription[fold.id == "NW"] <- levels(factor(song.function))[apply(nw.preds,
                                                                                  1,
                                                                                  which.max)]
        correct.transcription <- predictions.transcription == song.function
        confusion.transcription <- table(actual = song.function,
                                 predicted = predictions.transcription
                                 )
        acc.transcription <- sum(diag(confusion.transcription)) / sum(confusion.transcription)
        
    }    
}

## classify using experts
set.seed(02139)
mod.expert <- cv.glmnet(
    x = disco.expert.mat,
    y = factor(song.function),
    alpha = 1,
    family = 'multinomial',
    grouped = FALSE,
    standardize = TRUE,
    keep = TRUE
)
lambda.seq <- mod.expert$lambda

best.error <- Inf
for(lambda in lambda.seq){
    predictions.expert <- rep(NA, nrow(disco.expert.mat))
    lambda.mod.ow <- glmnet(
        x = disco.expert.mat[fold.id == "OW",],
        y = factor(song.function)[fold.id == "OW"],
        lambda = lambda,
        alpha = 1,
        family = 'multinomial',
        standardize = TRUE)
    nw.preds <- predict(lambda.mod.ow,
                        disco.expert.mat[fold.id == "NW",],
                        type = "response")
    
    lambda.mod.nw <- glmnet(
        x = disco.expert.mat[fold.id == "NW",],
        y = factor(song.function)[fold.id == "NW"],
        lambda = lambda,
        alpha = 1,
        family = 'multinomial',
        standardize = TRUE)
    ow.preds <- predict(lambda.mod.nw,
                        disco.expert.mat[fold.id == "OW",],
                        type = "response")

    total.error <- 0    
    for(n in 1:nrow(nw.preds)){
        for(k in 1:ncol(nw.preds)){
            total.error <- total.error + log(nw.preds[n,k,1]) *
                as.integer(colnames(nw.preds[,,1])[k] == factor(song.function)[fold.id == "NW"][n])
        }
    }    
    for(n in 1:nrow(ow.preds)){
        for(k in 1:ncol(ow.preds)){
            total.error <- total.error + log(ow.preds[n,k,1]) *
                as.integer(colnames(ow.preds[,,1])[k] == factor(song.function)[fold.id == "OW"][n])
        }
    }
    llk <- -total.error
    if(total.error < best.error){
        best.error <- total.error

        predictions.expert[fold.id == "OW"] <- levels(factor(song.function))[apply(ow.preds,
                                                                                  1,
                                                                                  which.max)]
        predictions.expert[fold.id == "NW"] <- levels(factor(song.function))[apply(nw.preds,
                                                                                  1,
                                                                                  which.max)]
        correct.expert <- predictions.expert == song.function
        confusion.expert <- table(actual = song.function,
                                 predicted = predictions.expert
                                 )
        acc.expert <- sum(diag(confusion.expert)) / sum(confusion.expert)
        
    }    
}

## all context-free variables, which are just concatenated
##   (1) transcription variables and (2) expert variables
set.seed(02139)
mod.nocontext <- cv.glmnet(
    x = disco.nocontext.mat,
    y = factor(song.function),
    alpha = 1,
    family = 'multinomial',
    grouped = FALSE,
    standardize = TRUE,
    keep = TRUE
)
lambda.seq <- mod.nocontext$lambda

best.error <- Inf
for(lambda in lambda.seq){
    predictions.nocontext <- rep(NA, nrow(disco.nocontext.mat))
    lambda.mod.ow <- glmnet(
        x = disco.nocontext.mat[fold.id == "OW",],
        y = factor(song.function)[fold.id == "OW"],
        lambda = lambda,
        alpha = 1,
        family = 'multinomial',
        standardize = TRUE)
    nw.preds <- predict(lambda.mod.ow,
                        disco.nocontext.mat[fold.id == "NW",],
                        type = "response")
    
    lambda.mod.nw <- glmnet(
        x = disco.nocontext.mat[fold.id == "NW",],
        y = factor(song.function)[fold.id == "NW"],
        lambda = lambda,
        alpha = 1,
        family = 'multinomial',
        standardize = TRUE)
    ow.preds <- predict(lambda.mod.nw,
                        disco.nocontext.mat[fold.id == "OW",],
                        type = "response")

    total.error <- 0    
    for(n in 1:nrow(nw.preds)){
        for(k in 1:ncol(nw.preds)){
            total.error <- total.error + log(nw.preds[n,k,1]) *
                as.integer(colnames(nw.preds[,,1])[k] == factor(song.function)[fold.id == "NW"][n])
        }
    }    
    for(n in 1:nrow(ow.preds)){
        for(k in 1:ncol(ow.preds)){
            total.error <- total.error + log(ow.preds[n,k,1]) *
                as.integer(colnames(ow.preds[,,1])[k] == factor(song.function)[fold.id == "OW"][n])
        }
    }
    llk <- -total.error
    if(total.error < best.error){
        best.error <- total.error

        predictions.nocontext[fold.id == "OW"] <- levels(factor(song.function))[apply(ow.preds,
                                                                                  1,
                                                                                  which.max)]
        predictions.nocontext[fold.id == "NW"] <- levels(factor(song.function))[apply(nw.preds,
                                                                                  1,
                                                                                  which.max)]
        correct.nocontext <- predictions.nocontext == song.function
        confusion.nocontext <- table(actual = song.function,
                                 predicted = predictions.nocontext
                                 )
        acc.nocontext <- sum(diag(confusion.nocontext)) / sum(confusion.nocontext)
        
    }    
}

## nadeau & bengio method 1 (corrected resampled t-test):
##   corrective factor for estimated generalization error sd
nb1.factor <- sqrt(
    1 / length(unique(fold.vars[[fold.var]])) +
    length(unique(song.function)) / (length(song.function) - length(unique(song.function)))
)

## first calculate average accuracy for each fold
correct.fold.naive <-
    tapply(correct.naive, fold.vars[[fold.var]], mean, simplify = TRUE)
correct.fold.mir <-
    tapply(correct.mir, fold.vars[[fold.var]], mean, simplify = TRUE)
correct.fold.mir.panteli <-
    tapply(correct.mir.panteli, fold.vars[[fold.var]], mean, simplify = TRUE)
correct.fold.expert <-
    tapply(correct.expert, fold.vars[[fold.var]], mean, simplify = TRUE)
correct.fold.transcription <-
    tapply(correct.transcription, fold.vars[[fold.var]], mean, simplify = TRUE)
correct.fold.nocontext <-
    tapply(correct.nocontext, fold.vars[[fold.var]], mean, simplify = TRUE)

nb1.n1 <- (length(unique(fold.vars[[fold.var]])) - 1) * length(unique(song.function))
nb1.n2 <- length(unique(song.function))
## approximation for correlation of fold accuracies
nb1.rho0 <- nb1.n2 / (nb1.n1 + nb1.n2)
nb1.factor <- sqrt(1 / length(unique(fold.vars[[fold.var]])) + nb1.rho0 / (1 - nb1.rho0))

## loop over each and
feature.sets <- c('naive', 'mir', 'mir.panteli', 'expert', 'transcription', 'nocontext')
acc.results <- data.frame(
    feature.set = feature.sets,
    accuracy = sapply(feature.sets, function(x) get('acc.' %.% x)),
    se = sapply(feature.sets, function(x) sd(get('correct.fold.' %.% x) ) * nb1.factor)
)
acc.results$cilo <- qnorm(.025) * acc.results$se + acc.results$accuracy
acc.results$cihi <- qnorm(.975) * acc.results$se + acc.results$accuracy

write.csv(do.call(rbind,
                  lapply(feature.sets,
                         function(x){
                             out <- as.data.frame(unclass(get('confusion.' %.% x)))
                             colnames(out) <- 'predict.' %.% colnames(out)
                             out <- cbind(feature.set = x, actual = rownames(out), out)
                             out
                         })
                  ),
          file.path(results.dir, paste0(fold.var, "_", 'disco_overall_confusion.csv')),
          row.names = FALSE
          )

write.csv(acc.results,
          file.path(results.dir, paste0(fold.var, "_", 'disco_overall_acc.csv')),
          row.names = FALSE
          )




##############################################################
## binary classification after subsetting to two song types ##
##############################################################

functions <- sort(levels(song.function))
functionpairs <- rbind(c('Dance', 'Healing'),
                        c('Dance', 'Love'),
                        c('Dance', 'Lullaby'),
                        c('Healing', 'Love'),
                        c('Healing', 'Lullaby'),
                        c('Love', 'Lullaby')
                        )

functionpairs.results <- list()

for (i in 1:nrow(functionpairs)){

  ## glmnet: "the last level in alphabetical order is the target class"
  functionpair <- sort(functionpairs[i,])
  functionpair.ind <- which(song.function %in% functionpair)

  mod.nocontext <- cv.glmnet(
    x = disco.nocontext.mat[functionpair.ind,],
    y = factor(song.function[functionpair.ind]),
    alpha = 1,
    family = 'binomial',
    foldid = as.integer(factor(song.region[functionpair.ind])),
    grouped = FALSE,
    standardize = TRUE,
    keep = TRUE
  )
  lambda.ind.nocontext <- match(mod.nocontext$lambda.min, mod.nocontext$glmnet.fit$lambda)
  coef.nocontext <- mod.nocontext$glmnet.fit$beta[,lambda.ind.nocontext]
  predict.nocontext <- functionpair[round(mod.nocontext$fit.preval[,lambda.ind.nocontext]) + 1]

  ## nadeau & bengio method 1 (corrected resampled t-test):
  ##   corrective factor for estimated generalization error sd
  nb1.factor <- sqrt(
    1 / length(unique(song.region)) +
      2 / (length(functionpair.ind) - 2)
  )

  correct <- song.function[functionpair.ind] == predict.nocontext
  correct.region <- tapply(correct, song.region[functionpair.ind], mean, simplify = TRUE)

  ## expected proportion correct from random guesses according to known props
  n <- length(functionpair.ind)
  p <-
    (sum(song.function == functionpair[1]) / n)^2 +
    (sum(song.function == functionpair[2]) / n)^2

  functionpairs.results[[
    sprintf('%s (-) vs %s (+)', functionpair[1], functionpair[2])
    ]] <- list(
      function1 = functionpair[1],
      function2 = functionpair[2],
      accuracy = mean(correct),
      se = sd(correct.region) * nb1.factor,
      baseline.correct.random = p,
      n = length(functionpair.ind),
      confusion = table(
        actual = as.character(song.function[functionpair.ind]),
        predict = predict.nocontext
      ),
      coef = sort(coef.nocontext[coef.nocontext != 0])
    )

}

## selected lasso coefficients
sink(file.path(
  results.dir,
  'disco_pairs_lassocoefs.txt'
))
for (functionpair in names(functionpairs.results)){
  cat('\n', rep('=', 40), '\n\n', sep = '')
  cat('COMPARISON:', functionpair, '\n')
  functionpairs.result <- functionpairs.results[[functionpair]]
  cat('\n', rep('-', 20), '\n\n', sep = '')
  cat('ACCURACY:', round(functionpairs.result$accuracy, 2), '\n\n')
  out <- as.matrix(round(functionpairs.result$coef, 2))
  colnames(out) <- c('SELECTED COEF')
  print(out)
}
sink()

acc <- ldply(functionpairs.results,
             function(x){
               data.frame(n = x$n,
                          accuracy = x$accuracy,
                          se = x$se,
                          baseline = x$baseline.correct.random,
                          function1 = x$function1,
                          function2 = x$function2
                          )
             },
             .id = 'comparison'
             )
acc$cilo <- qnorm(.025) * acc$se + acc$acc
acc$cihi <- qnorm(.975) * acc$se + acc$acc
acc$p <- pval.1side.to.2side(
  pnorm((acc$acc - acc$baseline) / acc$se)
)
acc$p.adj <- p.adjust(acc$p, 'BY')
write.csv(acc,
          file.path(results.dir, 'disco_pairs_acc.csv'),
          row.names = FALSE
          )

## confusion matrix for pairwise comparison
acc.mat <- matrix(0,
                  nrow = 4,
                  ncol = 4,
                  dimnames = list(levels(song.function),
                                  levels(song.function)
                                  )
                  )
acc.mat[as.matrix(acc[,c('function1', 'function2')])] <- acc$acc
acc.mat <- acc.mat + t(acc.mat)
acc.mat[acc.mat == 0] <- NA
write.csv(acc.mat)




################
## disco bpca ##
################

ndraws <- 1000
## cambridge zip codes
seeds <- 02139
## experts found top 2 dimensions to be interpretable
Q <- 2
## run chain
pca.fname <- file.path(
  results.dir,
  sprintf('disco_bpca_mcmc_Q%s_draw%s_seed%05d.rds',
          Q,
          ndraws,
          seeds
          )
)

if (!file.exists(pca.fname)){
  ## single bpca run
  ##   we're just drawing samples from the ppca posterior, no missingness
  mod.bpca <- bpca(disco.nocontext.mat,
                   Q = Q,
                   maxiter.em = 100,
                   niter.mcmc = ndraws
                   )
  ## flip dimensions so higher complexity is +
  mod.bpca$mle$W <- -1 * mod.bpca$mle$W
  mod.bpca$mle$scores <- -1 * mod.bpca$mle$scores
  mod.bpca$mcmc$W <- -1 * mod.bpca$mcmc$W
  mod.bpca$mcmc$W.rotate <- -1 * mod.bpca$mcmc$W.rotate
  mod.bpca$mcmc$scores <- -1 * mod.bpca$mcmc$scores
  mod.bpca$mcmc$scores.rotate <- -1 * mod.bpca$mcmc$scores.rotate
  saveRDS(mod.bpca, pca.fname)
} else {
  mod.bpca <- readRDS(pca.fname)
}

## variance explained
D <- ncol(disco.nocontext.mat)
var.expl <- colSums(mod.bpca$mle$W^2)
var.total <- sum(var.expl) + D * mod.bpca$mle$sigmasq
round(var.expl / sum(var.expl), 3)
round(var.expl / var.total, 3)
sum(var.expl / var.total)

## label flipped dimensions
dim.labels <- c(q.1 = 'melodic complexity',
                q.2 = 'rhythmic complexity'
                )

## interpret dimensions by most significantly contributing variables
interpretation <- ldply(
    1:ncol(mod.bpca$mle$W),
    function(q) {
    est <- apply(mod.bpca$mcmc$W.rotate[,q,], 1, mean)
    se <- apply(mod.bpca$mcmc$W.rotate[,q,], 1, sd)
    z <- est / se
    ind <- order(z)
    data.frame(variable = rownames(mod.bpca$mle$W)[ind],
               dim = q,
               est = est[ind],
               se = se[ind],
               z = z[ind]
               )
    })

## print to console: all contributing annotations to each dimension
sink(file.path(
  results.dir,
  'disco_bpca_annotation_to_dimension.txt'
))
for (q in 1:Q){
  cat('\n', rep('=', 80),
      '\n\nDIMENSION ', q, ': ', dim.labels[q], '\n\n', sep = '')
  interpretation.q <- interpretation[interpretation$dim == q,]
  interpretation.q$var <- substr(interpretation.q$variable, 1, 47)
  interpretation.q$var <- interpretation.q$var %.%
    ifelse(nchar(interpretation.q$variable) > 47, '...', '')
  rownames(interpretation.q) <-
    sapply(interpretation.q$var, function(x){
      x %.% paste(rep(' ', 50 - nchar(x)), collapse = '')
    })
  print(interpretation.q[,c('est', 'se', 'z')],
        digits = 3,
        width = 3
        )
}
sink()

write.csv(
  data.frame(song = disco.meta$song,
             type = song.function,
             region = song.region,
             bpca = apply(mod.bpca$mcmc$scores, c(1, 2), mean)
             ),
  file.path(results.dir,
            'disco_bpca_scores.csv'
            )
)



#########################################################
## analyze song function using within-region variation ##
#########################################################

nthin <- 10
nnorm.per.pca.draw <- 100

## regress discography scores on region and song function
noregionfe.stars <- laply(
  1:Q,  # latent dimensions to analyze
  function(q){
    ## for each latent dimension, get culture fe posterior
    noregionfe.stars.q <- llply(
      seq(nthin, ncol(mod.bpca$mcmc$mu), nthin),  # draws to analyze
      function(draw){
        ## for each posterior draw, get (culture fe posterior | scores)
        mod.star <- lm(
          score.q ~
            0 +
            type
         ,
          data = na.omit(data.frame(
            score.q =
              mod.bpca$mcmc$scores.rotate[, q, draw] -
              mean(mod.bpca$mcmc$scores.rotate[, q, draw]),
            nhs_region = disco.meta$nhs_region,
            type = disco.meta$type
          ))
        )
        coef.star <- mvrnorm(n = nnorm.per.pca.draw,
                             mu = coef(mod.star),
                             Sigma = vcov(mod.star)
                             )
        return(coef.star)
      })
    noregionfe.stars.q <- do.call(rbind, noregionfe.stars.q)
    return(noregionfe.stars.q)
  }
)

noregionfe.summary <- ddply(
  expand.grid(type1 = song.types,
              type2 = song.types,
              dim = 1:Q
              ),
  c('type1', 'type2', 'dim'),
  function(d){
    i <- d$type1
    j <- d$type2
    q <- d$dim
    diff <- noregionfe.stars[q, , 'type' %.% i] - noregionfe.stars[q, , 'type' %.% j]
    data.frame(
      type1 = i,
      type1.est = mean(noregionfe.stars[q, , 'type' %.% i]),
      type1.cilo = unname(quantile(noregionfe.stars[q, , 'type' %.% i], .025)),
      type1.cihi = unname(quantile(noregionfe.stars[q, , 'type' %.% i], .975)),
      type2 = j,
      type2.est = mean(noregionfe.stars[q, , 'type' %.% j]),
      type2.cilo = unname(quantile(noregionfe.stars[q, , 'type' %.% j], .025)),
      type2.cihi = unname(quantile(noregionfe.stars[q, , 'type' %.% j], .975)),
      diff.est = ifelse(i == j, NA, mean(diff)),
      diff.cilo = ifelse(i == j, NA, unname(quantile(diff, .025))),
      diff.cihi = ifelse(i == j, NA, unname(quantile(diff, .975))),
      diff.p = ifelse(i == j, NA, pval.1side.to.2side(mean(diff < 0)))
    )
  })

noregionfe.summary.uniquepairs <-
  noregionfe.summary[
    match(noregionfe.summary$type1, song.types) <
      match(noregionfe.summary$type2, song.types),
    ]
noregionfe.summary.uniquepairs$diff.p.adj <-
  p.adjust(noregionfe.summary.uniquepairs$diff.p, 'BY')

write.csv(noregionfe.summary.uniquepairs,
          file.path(results.dir, 'disco_songtype_comparison_noregionfe.csv'),
          row.names = FALSE
          )



## regress discography scores on region and song function
regionfe.stars <- laply(
  1:Q,  # latent dimensions to analyze
  function(q){
    ## for each latent dimension, get culture fe posterior
    regionfe.stars.q <- llply(
      seq(nthin, ncol(mod.bpca$mcmc$mu), nthin),  # draws to analyze
      function(draw){
        ## for each posterior draw, get (culture fe posterior | scores)
        mod.star <- lm(
          score.q ~
            0 +
            type +
            nhs_region
         ,
          data = na.omit(data.frame(
            score.q =
              mod.bpca$mcmc$scores.rotate[, q, draw] -
              mean(mod.bpca$mcmc$scores.rotate[, q, draw]),
            nhs_region = disco.meta$nhs_region,
            type = disco.meta$type
          ))
        )
        coef.star <- mvrnorm(n = nnorm.per.pca.draw,
                             mu = coef(mod.star),
                             Sigma = vcov(mod.star)
                             )
        return(coef.star)
      })
    regionfe.stars.q <- do.call(rbind, regionfe.stars.q)
    return(regionfe.stars.q)
  }
)

regionfe.summary <- ddply(
  expand.grid(type1 = song.types,
              type2 = song.types,
              dim = 1:Q
              ),
  c('type1', 'type2', 'dim'),
  function(d){
    i <- d$type1
    j <- d$type2
    q <- d$dim
    diff <- regionfe.stars[q, , 'type' %.% i] - regionfe.stars[q, , 'type' %.% j]
    data.frame(
      type1 = i,
      type1.est = mean(regionfe.stars[q, , 'type' %.% i]),
      type1.cilo = unname(quantile(regionfe.stars[q, , 'type' %.% i], .025)),
      type1.cihi = unname(quantile(regionfe.stars[q, , 'type' %.% i], .975)),
      type2 = j,
      type2.est = mean(regionfe.stars[q, , 'type' %.% j]),
      type2.cilo = unname(quantile(regionfe.stars[q, , 'type' %.% j], .025)),
      type2.cihi = unname(quantile(regionfe.stars[q, , 'type' %.% j], .975)),
      diff.est = ifelse(i == j, NA, mean(diff)),
      diff.cilo = ifelse(i == j, NA, unname(quantile(diff, .025))),
      diff.cihi = ifelse(i == j, NA, unname(quantile(diff, .975))),
      diff.p = ifelse(i == j, NA, pval.1side.to.2side(mean(diff < 0)))
    )
  })

regionfe.summary.uniquepairs <-
  regionfe.summary[
    match(regionfe.summary$type1, song.types) <
      match(regionfe.summary$type2, song.types),
    ]
regionfe.summary.uniquepairs$diff.p.adj <-
  p.adjust(regionfe.summary.uniquepairs$diff.p, 'BY')

write.csv(regionfe.summary.uniquepairs,
          file.path(results.dir, 'disco_songtype_comparison_regionfe.csv'),
          row.names = FALSE
          )
