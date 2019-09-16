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


## ###############################################
## ## read transcription ngrams for description ##
## ###############################################

## ## read in pitch-grams
## disco.pitch.paths <- list.files(
##   file.path(data.dir, 'Discography Transcriptions', 'ngrams'),
##   pattern = 'pitch',
##   full.names = TRUE
## )
## disco.pitch <- sapply(disco.pitch.paths, function(x){
##   ## each line is a separate voice, insert extra separator so ngrams don't
##   ## span multiple voices
##   paste(readLines(x), collapse = ' . ')
## })
## names(disco.pitch) <- basename(names(disco.pitch))
## names(disco.pitch) <- gsub('-pitches\\.txt', '', names(disco.pitch))

## ## read in rhythm-grams
## disco.rhythm.paths <- list.files(
##   file.path(data.dir, 'Discography Transcriptions', 'ngrams'),
##   pattern = 'rhythm',
##   full.names = TRUE
## )
## disco.rhythm <- sapply(disco.rhythm.paths, function(x){
##   ## each line is a separate voice, insert extra separator so ngrams don't
##   ## span multiple voices
##   paste(readLines(x), collapse = ' . ')
## })
## names(disco.rhythm) <- basename(names(disco.rhythm))
## names(disco.rhythm) <- gsub('-rhythms\\.txt', '', names(disco.rhythm))



## ## construct dtm with raw pitch-grams
## disco.pitch.dtm <- dfm(disco.pitch,
##                        what = 'word',
##                        ngrams = 2:3
##                        )
## ## drop pitch-grams that span a rest
## disco.pitch.dtm <- disco.pitch.dtm[,!grepl('\\.', colnames(disco.pitch.dtm))]
## ## identify 2/3-grams by counting number of token separators
## pitch.ngrams.length <- nchar(gsub('[0-9]', '', colnames(disco.pitch.dtm))) + 1
## ## reorder columns
## disco.pitch.dtm <- disco.pitch.dtm[,order(pitch.ngrams.length,
##                                           colnames(disco.pitch.dtm))
##                                    ]
## pitch.ngrams.length <- nchar(gsub('[0-9]', '', colnames(disco.pitch.dtm))) + 1



## ## construct dtm with raw rhythm-grams
## disco.rhythm.dtm <- dfm(disco.rhythm,
##                        what = 'fastestword',
##                        ngrams = 2:3
##                        )
## ## drop rhythm-grams that span a rest
## disco.rhythm.dtm <- disco.rhythm.dtm[,!grepl('\\.', colnames(disco.rhythm.dtm))]
## ## identify 2/3-grams by counting number of token separators
## rhythm.ngrams.length <-
##   nchar(gsub('[0-9/]', '', colnames(disco.rhythm.dtm))) + 1
## ## reorder columns
## disco.rhythm.dtm <- disco.rhythm.dtm[,order(rhythm.ngrams.length,
##                                           colnames(disco.rhythm.dtm))
##                                    ]
## rhythm.ngrams.length <-
##   nchar(gsub('[0-9/]', '', colnames(disco.rhythm.dtm))) + 1



## ## identify pitchgrams that are additive shifts of each other
## pitch.to.pitchrel <- sapply(
##   colnames(disco.pitch.dtm, '_'),
##   function(token){
##     pitch.difference <- diff(as.numeric(strsplit(token, '_')[[1]]))
##     paste(
##       sprintf('%+d', pitch.difference),
##       collapse = '_'
##     )
##   })
## ## construct a thesaurus for quanteda
## pitchrel.dict <- dictionary(
##   sapply(
##     sort(unique(pitch.to.pitchrel)),
##     function(pitchrel){
##       names(pitch.to.pitchrel[pitch.to.pitchrel == pitchrel])
##     },
##     simplify = FALSE
##   )
## )
## ## rebuild dtm with collapsed pitch-grams
## disco.pitchrel.dtm <- dfm(disco.pitch,
##                            what = 'word',
##                            ngrams = 2:3,
##                            dictionary = pitchrel.dict
##                            )
## rownames(disco.pitchrel.dtm) <-
##   gsub('-pitches\\.txt', '', rownames(disco.pitchrel.dtm))
## pitchrel.ngrams.length <-
##   nchar(gsub('[0-9+\\-]', '', colnames(disco.pitchrel.dtm))) + 2



## ## identify rhythmgrams where intervals are multiplicative shifts of each other
## rhythm.to.rhythmrel <- sapply(
##   colnames(disco.rhythm.dtm, '_'),
##   function(token){
##     rhythm.intervals <- strsplit(token, '_')[[1]]
##     rhythm.scaled <- sapply(rhythm.intervals,
##                            function(x){eval(parse(text = x))
##                            })
##     rhythm.scaled <- rhythm.scaled[-1] / rhythm.scaled[1]
##     paste(
##       sprintf('x%0.2f', rhythm.scaled),
##       collapse = '_'
##     )
##   })
## ## construct a thesaurus for quanteda
## rhythmrel.dict <- dictionary(
##   sapply(
##     sort(unique(rhythm.to.rhythmrel)),
##     function(rhythmrel){
##       names(rhythm.to.rhythmrel[rhythm.to.rhythmrel == rhythmrel])
##     },
##     simplify = FALSE
##   )
## )
## ## rebuild dtm with collapse rhythm-grams
## disco.rhythmrel.dtm <- dfm(disco.rhythm,
##                            what = 'fastestword',
##                            ngrams = 2:3,
##                            dictionary = rhythmrel.dict
##                            )
## rownames(disco.rhythmrel.dtm) <-
##   gsub('-rhythmes\\.txt', '', rownames(disco.rhythmrel.dtm))
## rhythmrel.ngrams.length <-
##   nchar(gsub('[0-9x.]', '', colnames(disco.rhythmrel.dtm))) + 2


## ## normalize datasets by calculating ngram proportion
## ##   (normalize separately for 2-grams and 3-grams)

## disco.pitch.prop.dtm <- disco.pitch.dtm
## for (i in 1:nrow(disco.pitch.prop.dtm)){
##   disco.pitch.prop.dtm[i, pitch.ngrams.length == 2] <-
##     disco.pitch.prop.dtm[i, pitch.ngrams.length == 2] /
##     sum(disco.pitch.prop.dtm[i, pitch.ngrams.length == 2])
##   disco.pitch.prop.dtm[i, pitch.ngrams.length == 3] <-
##     disco.pitch.prop.dtm[i, pitch.ngrams.length == 3] /
##     sum(disco.pitch.prop.dtm[i, pitch.ngrams.length == 3])
## }

## disco.pitchrel.prop.dtm <- disco.pitchrel.dtm
## for (i in 1:nrow(disco.pitchrel.prop.dtm)){
##   disco.pitchrel.prop.dtm[i, pitchrel.ngrams.length == 2] <-
##     disco.pitchrel.prop.dtm[i, pitchrel.ngrams.length == 2] /
##     sum(disco.pitchrel.prop.dtm[i, pitchrel.ngrams.length == 2])
##   disco.pitchrel.prop.dtm[i, pitchrel.ngrams.length == 3] <-
##     disco.pitchrel.prop.dtm[i, pitchrel.ngrams.length == 3] /
##     sum(disco.pitchrel.prop.dtm[i, pitchrel.ngrams.length == 3])
## }

## disco.rhythm.prop.dtm <- disco.rhythm.dtm
## for (i in 1:nrow(disco.rhythm.prop.dtm)){
##   disco.rhythm.prop.dtm[i, rhythm.ngrams.length == 2] <-
##     disco.rhythm.prop.dtm[i, rhythm.ngrams.length == 2] /
##     sum(disco.rhythm.prop.dtm[i, rhythm.ngrams.length == 2])
##   disco.rhythm.prop.dtm[i, rhythm.ngrams.length == 3] <-
##     disco.rhythm.prop.dtm[i, rhythm.ngrams.length == 3] /
##     sum(disco.rhythm.prop.dtm[i, rhythm.ngrams.length == 3])
## }

## disco.rhythmrel.prop.dtm <- disco.rhythmrel.dtm
## for (i in 1:nrow(disco.rhythmrel.prop.dtm)){
##   disco.rhythmrel.prop.dtm[i, rhythmrel.ngrams.length == 2] <-
##     disco.rhythmrel.prop.dtm[i, rhythmrel.ngrams.length == 2] /
##     sum(disco.rhythmrel.prop.dtm[i, rhythmrel.ngrams.length == 2])
##   disco.rhythmrel.prop.dtm[i, rhythmrel.ngrams.length == 3] <-
##     disco.rhythmrel.prop.dtm[i, rhythmrel.ngrams.length == 3] /
##     sum(disco.rhythmrel.prop.dtm[i, rhythmrel.ngrams.length == 3])
## }



## ## null model is random draw from corresponding pitch collection
## scales <- list(
##   'Aeolian' = c(0, 2, 3, 5, 7, 8, 10),
##   'Dorian' = c(0, 2, 3, 5, 7, 9, 10),
##   'Generic major' = c(0, 2, 4, 5, 7),
##   'Generic minor' = c(0, 2, 3, 5, 7),
##   'Ionian' = c(0, 2, 4, 5, 7, 9, 11),
##   'Locrian' = c(0, 1, 3, 5, 6, 8, 10),
##   'Lydian' = c(0, 2, 4, 6, 7, 9, 11),
##   'Major pentatonic' = c(0, 2, 4, 7, 9),
##   'Minor pentatonic' = c(0, 3, 5, 7, 10),
##   'Mixolydian' = c(0, 2, 4, 5, 7, 9, 10),
##   'Phrygian' = c(0, 1, 3, 5, 7, 8, 10)
## )

## ddply(disco.expert.raw, 'song', function(x){
##   scale <- x$scale_type1
##   scale <- scale[!scale %in% c('.', 'Undefined')]
##   names(sort(table(scale), decreasing = TRUE))[1]
##   })

## disco.ngram.summary <- rbind(
##   data.frame(type = 'pitch',
##              length = pitch.ngrams.length,
##              name = colnames(disco.pitch.dtm),
##              instances = colSums(disco.pitch.dtm),
##              proportion = colSums(disco.pitch.prop.dtm) / nrow(disco.meta),
##              songs = colSums(disco.pitch.dtm > 0)
##              ),
##   data.frame(type = 'relative pitch',
##              length = pitchrel.ngrams.length,
##              name = colnames(disco.pitchrel.dtm),
##              instances = colSums(disco.pitchrel.dtm),
##              proportion = colSums(disco.pitchrel.prop.dtm) / nrow(disco.meta),
##              songs = colSums(disco.pitchrel.dtm > 0)
##              ),
##   data.frame(type = 'rhythm',
##              length = rhythm.ngrams.length,
##              name = colnames(disco.rhythm.dtm),
##              instances = colSums(disco.rhythm.dtm),
##              proportion = colSums(disco.rhythm.prop.dtm) / nrow(disco.meta),
##              songs = colSums(disco.rhythm.dtm > 0)
##              ),
##   data.frame(type = 'relative rhythm',
##              length = rhythmrel.ngrams.length,
##              name = colnames(disco.rhythmrel.dtm),
##              instances = colSums(disco.rhythmrel.dtm),
##              proportion = colSums(disco.rhythmrel.prop.dtm) / nrow(disco.meta),
##              songs = colSums(disco.rhythmrel.dtm > 0)
##              )
## )

## disco.ngram.summary <-
##   ddply(disco.ngram.summary, c('type', 'length'), function(x){
##     ## if (x$type == 'rhythm' & x$length == 2){
##     ##   browser()
##     ## }
##     x <- x[order(x$proportion, decreasing = TRUE),]
##     x$proportion.cumulative <- cumsum(x$proportion)
##     return(x)
##   })
## ## disco.ngram.summary$name <- factor(disco.ngram.summary$name,
## ##                                    levels = )

## disco.ngram.summary.top20 <-
##   ddply(disco.ngram.summary, c('type', 'length'), head, 20)

## type <- 'rhythm'
## length <- 2

## disco.ngram.summary$plot.order <- 1:nrow(disco.ngram.summary)

## disco.ngram.quantile <- ddply(
##   disco.ngram.summary,
##   c('type', 'length'),
##   function(d){
##     c(q_0.75 = min(which(d$proportion.cumulative >= .75)),
##       n.observed = nrow(d)
##       )
## })


## for (type in unique(disco.ngram.summary$type)){
##   pdf(file.path(results.dir, sprintf('cdf_%s.pdf', gsub(' ', '_', type))),
##       12, 8
##       )
##   print(
##   ggplot(
##     disco.ngram.summary[
##       disco.ngram.summary$type == type,
##       ],
##     aes(## x = name,
##       x = plot.order,
##         y = proportion.cumulative
##         )
##   ) +
##     geom_col(width = 1) +
##     ylim(0, 1) +
##     facet_grid(. ~ length, labeller = label_both, scales = 'free', drop = TRUE) +
##     theme(axis.text.x = element_blank()
##           ## axis.text.x = element_text(angle = 90, hjust = 1)
##           ) +
##     ggtitle(type) +
##     xlab(type %.% 'gram, ordered by frequency')
##   )
##   dev.off()
## }

## ggplot(
##   disco.ngram.summary.top20[
##     disco.ngram.summary.top20$type == type,
##     ],
##   aes(x = name,
##       y = proportion.cumulative
##       )
## ) +
##   geom_col(width = 1) +
##   ylim(0, 1) +
##   facet_grid(. ~ length, labeller = label_both, scales = 'free', drop = TRUE) +
##   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
##   ggtitle(type)



###################################
## process expert-based features ##
###################################

## tone.to.numeric <- c(
##   'C' = 0,
##   'C#' = 1,
##   'D' = 2,
##   'D#' = 3,
##   'E' = 4,
##   'F' = 5,
##   'F#' = 6,
##   'G' = 7,
##   'G#' = 8,
##   'A' = 9,
##   'A#' = 10,
##   'B' = 11
## )

## ## tonal center doesn't make sense for comparison b/c keys can be transposed
## ## reduce dimension of tonal center (was 12 categories)
## ##   while respecting the fact that B is close to C
## disco.expert.raw$tonal_pitch_cos <- cos(
##   2 * pi / 12 * tone.to.numeric[disco.expert.raw$tonal_pitch1]
## )
## disco.expert.raw$tonal_pitch_sin <- sin(
##   2 * pi / 12 * tone.to.numeric[disco.expert.raw$tonal_pitch1]
## )

## disco.expert.raw$tonal_multiple <-
##   !disco.expert.raw$tonal_pitch2 %in% c('Single point of stability', '.')

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
## disco.expert.raw$macrometer_odd <-
##   disco.expert.raw$macrometer_5 |
##   disco.expert.raw$macrometer_7 |
##   disco.expert.raw$macrometer_11 |
##   disco.expert.raw$macrometer_13 |
##   ifelse(disco.expert.raw$macrometer_other == '.',
##          FALSE,
##          as.numeric(disco.expert.raw$macrometer_other) %% 2 != 0 &
##          as.numeric(disco.expert.raw$macrometer_other) %% 3 != 0
##          )

## break out variation by type
disco.expert.raw$variation_melodic <-
  disco.expert.raw$repeat_vary %in% c('Melodic variation',
                                      'Rhythmic and melodic variation'
                                      )
disco.expert.raw$variation_rhythmic <-
  disco.expert.raw$repeat_vary %in% c('Rhythmic variation',
                                      'Rhythmic and melodic variation'
                                      )

## ## singer count and gender
## disco.expert.raw$singers_n <- ifelse(
##   disco.expert.raw$singers_n != '7 or more',
##   as.numeric(disco.expert.raw$singers_n),
##   7
## )
## disco.expert.raw$singers_female <-
##   disco.expert.raw$singers_sex %in% c('Female', 'Both sexes')
## disco.expert.raw$singers_male <-
##   disco.expert.raw$singers_sex %in% c('Male', 'Both sexes')

## ## unison, polyphony, and call_response are '.' for solo singers;
## ##   we pool solo singers with group singers for which the feature is absent
## disco.expert.raw$group <- disco.expert.raw$leader != '.'
## disco.expert.raw$unison <- as.numeric(disco.expert.raw$unison == 1)
## disco.expert.raw$polyphony <- as.numeric(disco.expert.raw$polyphony == 1)
## disco.expert.raw$call_response <- as.numeric(disco.expert.raw$call_response == 1)

## ## pool humming and pitched syllables
## disco.expert.raw$words <- as.numeric(disco.expert.raw$words == 'Words present')

## ending dynamics/speed are unobserved for songs that e.g. end abruptly;
##   drop these features
## disco.expert.raw$ending_speed <-
##   disco.expert.raw$ending_accel - disco.expert.raw$ending_ritard
## disco.expert.raw$ending_volume <-
##   disco.expert.raw$ending_loud - disco.expert.raw$ending_quiet
## disco.expert.raw$ending_abrupt <-
##   disco.expert.raw$ending_finalnote - disco.expert.raw$ending_long
## disco.expert.raw$ending_cut <-
##     disco.expert.raw$ending_stop | disco.expert.raw$ending_unknown

disco.expert.raw$tension <-
    disco.expert.raw$tension_melody +
    disco.expert.raw$tension_harmony +
    disco.expert.raw$tension_rhythm +
    disco.expert.raw$tension_motif +
    disco.expert.raw$tension_accent +
    disco.expert.raw$tension_dynamic

## disco.expert.raw$clapstomp <-
##     disco.expert.raw$clap | disco.expert.raw$stomp
## disco.expert.raw$instru_any <- disco.expert.raw$instru != 'No instruments'
## disco.expert.raw$instru_count <- c(
##   'No instruments' = 0,
##   '1' = 1,
##   '2' = 2,
##   '3' = 3,
##   '4' = 4,
##   '5 or more' = 5
## )[disco.expert.raw$instru]

disco.expert.raw$dynamics <- c(
    'Gets louder' = 1,
    'Multiple dynamics' = 1,
    'No dynamics' = 0,
    'Quiets down' = 1
)[disco.expert.raw$dynamics]
## disco.expert.raw$dynamics_louder <- c(
##     'Gets louder' = 1,
##     'Multiple dynamics' = 0,
##     'No dynamics' = 0,
##     'Quiets down' = -1
## )[disco.expert.raw$dynamics]
## disco.expert.raw$dynamics_count <- c(
##     'No dynamics' = 0,
##     'Gets louder' = 1,
##     'Quiets down' = 1,
##     'Multiple dynamics' = 2
## )[disco.expert.raw$dynamics]

disco.expert.raw$ritard_accel <- c(
    'No ritard or accel' = 0,
    'Slows down' = 1,
    'Speeds up' = 1,
    'Speeds up and slows down' = 1
)[disco.expert.raw$ritard]
## disco.expert.raw$ritard_accel <- c(
##     'No ritard or accel' = 0,
##     'Slows down' = -1,
##     'Speeds up' = 1,
##     'Speeds up and slows down' = 0
## )[disco.expert.raw$ritard]
## disco.expert.raw$ritard_count <- c(
##     'No ritard or accel' = 0,
##     'Slows down' = 1,
##     'Speeds up' = 1,
##     'Speeds up and slows down' = 2
## )[disco.expert.raw$ritard]

## major/minor scale, single scale
disco.expert.raw$scale_quality_minor <- c(
  'Unknown' = NA,
  'Major' = 0,
  'Minor' = 1
)[disco.expert.raw$scale_quality]
## disco.expert.raw$scale_multiple <-
##   !disco.expert.raw$scale_type2 %in% c('Single pitch collection', '.')

## ## numeric
## features.expert.numeric <- c(
##   'tempo_adj',
##   'macrometer_ord',
##   'singers_n',
##   'syncopate',
##   'accent',
##   ## 'ending_speed',
##   ## 'ending_volume'
##   ## 'ending_abrupt',
##   ## 'ending_follow',
##   ## 'instru_count',
##   'dynamics_louder',
##   'dynamics_count',
##   'ritard_accel',
##   'ritard_count'
## )

## ## dummies
## features.expert.binaryorprop <- c(
##   ## 'tonal',
##   ## 'tonal_multiple',
##   ## 'scale',
##   'micrometer_duple',
##   'micrometer_triple',
##   'macrometer_duple',
##   'macrometer_triple',
##   'macrometer_odd',
##   ## 'repeat_small',
##   ## 'repeat_large',
##   'variation_rhythmic',
##   'variation_melodic',
##   'singers_female',
##   'singers_male',
##   ## 'group',
##   ## 'leader_female',
##   ## 'leader_male',
##   ## 'unison',
##   ## 'polyphony',
##   ## 'call_response',
##   'ornament',
##   'vibrato',
##   'words',
##   'tension_melody',
##   'tension_harmony',
##   'tension_rhythm',
##   'tension_motif',
##   'tension_accent',
##   'tension_dynamic',
##   'tension_voices',
##   'tension_inst',
##   'clapstomp',
##   'instru_any',
##   ## DK: SM says instrument variables are cheating
##   ## 'instru_idio',
##   ## 'instru_membrano',
##   ## 'instru_aero',
##   ## 'instru_chordo',
##   ## 'instru_rhythm1',
##   ## 'instru_rhythm2',
##   ## 'instru_pitched',
##   ## 'instru_drone',
##   ## 'instru_harmony',
##   ## 'instru_bassline',
##   ## 'instru_cpt',
##   ## 'instru_melody',
##   'scale_quality_minor',
##   'scale_multiple'
## )
## features.expert <- c(features.expert.numeric, features.expert.binaryorprop)

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

## ## rescale features based on non-binary/proportion annotations
## ##   by two s.d. to be on roughly the same scale
## for (feature in features.expert.numeric){
##   disco.expert[,feature] <-
##     disco.expert[,feature] / 2 / sd(disco.expert[,feature])
## }

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

## features.transcription <- c(
##   'mean_interval',
##   'distance_btwn_modal_intervals',
##   'number_of_common_pitches',
##   'common_intervals_count',
##   'stepwise_motion',
##   'melodic_thirds',
##   'duration_of_melodic_arcs',
##   'size_of_melodic_arcs',
##   'rel_strength_top_pitchcls',
##   'interval_btwn_strongest_pitchcls',
##   'pitch_class_variety',
##   'range',
##   'note_density',
##   'average_note_duration',
##   'variability_of_note_duration',
##   'modal_interval_prevalence',
##   'rel_strength_modal_intervals',
##   'amount_of_arpeggiation',
##   'direction_of_motion',
##   'modal_pitchcls_prev',
##   'initial_tempo',
##   'quality'
## )

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

## ## rescale features based on non-binary/proportion annotations
## ##   by two s.d. to be on roughly the same scale
## for (feature in features.transcription.numeric){
##   disco.transcription[,feature] <-
##     disco.transcription[,feature] / 2 / sd(disco.transcription[,feature])
## }

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

## ## rescale features based on non-binary/proportion annotations
## ##   by two s.d. to be on roughly the same scale
## for (feature in features.naive.numeric){
##   disco.naive[,feature] <-
##     disco.naive[,feature] / 2 * sd(disco.naive[,feature])
## }

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

## ## label each feature with origin
## disco.naive.mat <- as.matrix(disco.naive[,features.naive])
## colnames(disco.naive.mat) <- 'NAIVE.' %.% colnames(disco.naive.mat)
## disco.expert.mat <- as.matrix(disco.expert[,features.expert])
## colnames(disco.expert.mat) <- 'EXPERT.' %.% colnames(disco.expert.mat)
## disco.transcription.mat <- as.matrix(disco.transcription[,features.transcription])
## colnames(disco.transcription.mat) <- 'TRANSCRIPTION.' %.% colnames(disco.transcription.mat)
## disco.all.mat <- cbind(
##   disco.naive.mat[,'NAIVE.' %.% features.naive],
##   disco.expert.mat[,'EXPERT.' %.% features.expert],
##   disco.transcription.mat[,'TRANSCRIPTION.' %.% features.transcription]
## )
## features.nocontext <-
##   readLines(file.path(data.dir, 'discography_features_nocontextual.txt'))
## disco.all.mat <-
##   disco.all.mat[, colnames(disco.all.mat)[
##     gsub('^[A-Z]+\\.', '', colnames(disco.all.mat)) %in% features.nocontext
##   ]
##   ]

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

## disco.all.mat <- cbind(
##   disco.naive.mat[,'NAIVE.' %.% features.naive],
##   disco.expert.mat[,'EXPERT.' %.% features.expert],
##   disco.transcription.mat[,'TRANSCRIPTION.' %.% features.transcription],
##   disco.mir.mat[,'MIR.' %.% features.mir]
## )

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
    set.seed(02138)
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
    set.seed(02138)
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
    set.seed(02138)
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
    set.seed(02138)
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
    set.seed(02138)
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
    set.seed(02138)
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
set.seed(02138)
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
        
        print("Updating error")
        print(acc.naive)
    }    
}

## classify using mir features
set.seed(02138)
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
    print(llk)
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
        
        print("Updating error")
        print(acc.mir)
    }    
}

## classify using mir AND panteli features
set.seed(02138)
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
        
        print("Updating error")
        print(acc.mir.panteli)
    }    
}

## classify using transcriptions
set.seed(02138)
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
        
        print("Updating error")
        print(acc.transcription)
    }    
}

## classify using experts
set.seed(02138)
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
        
        print("Updating error")
        print(acc.expert)
    }    
}

## all context-free variables, which are just concatenated
##   (1) transcription variables and (2) expert variables
set.seed(02138)
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
        
        print("Updating error")
        print(acc.nocontext)
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
