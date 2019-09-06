options(stringsAsFactors = FALSE)

## zipf
library(tolerance)

## data manipulation
library(reshape2)
library(xtable)
library(plyr)
library(quanteda)

## graphics
library(ggplot2)
library(gridExtra)

## dirs
data.dir <- file.path('..', 'data', 'nhs')
results.dir <- file.path('..', 'results')

## functions
`%.%` <- paste0

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



###############################################
## read transcription ngrams for description ##
###############################################

## read in pitch-grams
disco.pitch.paths <- list.files(
  file.path(data.dir, 'NHSDiscography_Ngrams'),
  pattern = 'pitch',
  full.names = TRUE
)
disco.pitch <- sapply(disco.pitch.paths, function(x){
  ## each line is a separate voice, insert extra separator so ngrams don't
  ## span multiple voices
  paste(readLines(x), collapse = ' . ')
})
names(disco.pitch) <- basename(names(disco.pitch))
names(disco.pitch) <- gsub('-pitches\\.txt', '', names(disco.pitch))

## read in rhythm-grams
disco.rhythm.paths <- list.files(
  file.path(data.dir, 'NHSDiscography_Ngrams'),
  pattern = 'rhythm',
  full.names = TRUE
)
disco.rhythm <- sapply(disco.rhythm.paths, function(x){
  ## each line is a separate voice, insert extra separator so ngrams don't
  ## span multiple voices
  paste(readLines(x), collapse = ' . ')
})
names(disco.rhythm) <- basename(names(disco.rhythm))
names(disco.rhythm) <- gsub('-rhythms\\.txt', '', names(disco.rhythm))



## construct dtm with raw pitch-grams
disco.pitch.dtm <- dfm(disco.pitch,
                       what = 'word',
                       ngrams = 2:3
                       )
## drop pitch-grams that span a rest
disco.pitch.dtm <- disco.pitch.dtm[,!grepl('\\.', colnames(disco.pitch.dtm))]
## identify 2/3-grams by counting number of token separators
pitch.ngrams.length <- nchar(gsub('[0-9]', '', colnames(disco.pitch.dtm))) + 1
## reorder columns
disco.pitch.dtm <- disco.pitch.dtm[,order(pitch.ngrams.length,
                                          colnames(disco.pitch.dtm))
                                   ]
pitch.ngrams.length <- nchar(gsub('[0-9]', '', colnames(disco.pitch.dtm))) + 1



## construct dtm with raw rhythm-grams
disco.rhythm.dtm <- dfm(disco.rhythm,
                       what = 'fastestword',
                       ngrams = 2:3
                       )
## drop rhythm-grams that span a rest
disco.rhythm.dtm <- disco.rhythm.dtm[,!grepl('\\.', colnames(disco.rhythm.dtm))]
## identify 2/3-grams by counting number of token separators
rhythm.ngrams.length <-
  nchar(gsub('[0-9/]', '', colnames(disco.rhythm.dtm))) + 1
## reorder columns
disco.rhythm.dtm <- disco.rhythm.dtm[,order(rhythm.ngrams.length,
                                          colnames(disco.rhythm.dtm))
                                   ]
rhythm.ngrams.length <-
  nchar(gsub('[0-9/]', '', colnames(disco.rhythm.dtm))) + 1



## identify pitchgrams that are additive shifts of each other
pitch.to.pitchrel <- sapply(
  colnames(disco.pitch.dtm, '_'),
  function(token){
    pitch.difference <- diff(as.numeric(strsplit(token, '_')[[1]]))
    paste(
      sprintf('%+d', pitch.difference),
      collapse = '_'
    )
  })
## construct a thesaurus for quanteda
pitchrel.dict <- dictionary(
  sapply(
    sort(unique(pitch.to.pitchrel)),
    function(pitchrel){
      names(pitch.to.pitchrel[pitch.to.pitchrel == pitchrel])
    },
    simplify = FALSE
  )
)
## rebuild dtm with collapsed pitch-grams
disco.pitchrel.dtm <- dfm(disco.pitch,
                           what = 'word',
                           ngrams = 2:3,
                           dictionary = pitchrel.dict
                           )
rownames(disco.pitchrel.dtm) <-
  gsub('-pitches\\.txt', '', rownames(disco.pitchrel.dtm))
pitchrel.ngrams.length <-
  nchar(gsub('[0-9+\\-]', '', colnames(disco.pitchrel.dtm))) + 2



## identify rhythmgrams where intervals are multiplicative shifts of each other
rhythm.to.rhythmrel <- sapply(
  colnames(disco.rhythm.dtm, '_'),
  function(token){
    rhythm.intervals <- strsplit(token, '_')[[1]]
    rhythm.scaled <- sapply(rhythm.intervals,
                           function(x){eval(parse(text = x))
                           })
    rhythm.scaled <- rhythm.scaled[-1] / rhythm.scaled[1]
    paste(
      sprintf('x%0.2f', rhythm.scaled),
      collapse = '_'
    )
  })
## construct a thesaurus for quanteda
rhythmrel.dict <- dictionary(
  sapply(
    sort(unique(rhythm.to.rhythmrel)),
    function(rhythmrel){
      names(rhythm.to.rhythmrel[rhythm.to.rhythmrel == rhythmrel])
    },
    simplify = FALSE
  )
)
## rebuild dtm with collapse rhythm-grams
disco.rhythmrel.dtm <- dfm(disco.rhythm,
                           what = 'fastestword',
                           ngrams = 2:3,
                           dictionary = rhythmrel.dict
                           )
rownames(disco.rhythmrel.dtm) <-
  gsub('-rhythmes\\.txt', '', rownames(disco.rhythmrel.dtm))
rhythmrel.ngrams.length <-
  nchar(gsub('[0-9x.]', '', colnames(disco.rhythmrel.dtm))) + 2


## normalize datasets by calculating ngram proportion
##   (normalize separately for 2-grams and 3-grams)

disco.pitch.prop.dtm <- disco.pitch.dtm
for (i in 1:nrow(disco.pitch.prop.dtm)){
  disco.pitch.prop.dtm[i, pitch.ngrams.length == 2] <-
    disco.pitch.prop.dtm[i, pitch.ngrams.length == 2] /
    sum(disco.pitch.prop.dtm[i, pitch.ngrams.length == 2])
  disco.pitch.prop.dtm[i, pitch.ngrams.length == 3] <-
    disco.pitch.prop.dtm[i, pitch.ngrams.length == 3] /
    sum(disco.pitch.prop.dtm[i, pitch.ngrams.length == 3])
}

disco.pitchrel.prop.dtm <- disco.pitchrel.dtm
for (i in 1:nrow(disco.pitchrel.prop.dtm)){
  disco.pitchrel.prop.dtm[i, pitchrel.ngrams.length == 2] <-
    disco.pitchrel.prop.dtm[i, pitchrel.ngrams.length == 2] /
    sum(disco.pitchrel.prop.dtm[i, pitchrel.ngrams.length == 2])
  disco.pitchrel.prop.dtm[i, pitchrel.ngrams.length == 3] <-
    disco.pitchrel.prop.dtm[i, pitchrel.ngrams.length == 3] /
    sum(disco.pitchrel.prop.dtm[i, pitchrel.ngrams.length == 3])
}

disco.rhythm.prop.dtm <- disco.rhythm.dtm
for (i in 1:nrow(disco.rhythm.prop.dtm)){
  disco.rhythm.prop.dtm[i, rhythm.ngrams.length == 2] <-
    disco.rhythm.prop.dtm[i, rhythm.ngrams.length == 2] /
    sum(disco.rhythm.prop.dtm[i, rhythm.ngrams.length == 2])
  disco.rhythm.prop.dtm[i, rhythm.ngrams.length == 3] <-
    disco.rhythm.prop.dtm[i, rhythm.ngrams.length == 3] /
    sum(disco.rhythm.prop.dtm[i, rhythm.ngrams.length == 3])
}

disco.rhythmrel.prop.dtm <- disco.rhythmrel.dtm
for (i in 1:nrow(disco.rhythmrel.prop.dtm)){
  disco.rhythmrel.prop.dtm[i, rhythmrel.ngrams.length == 2] <-
    disco.rhythmrel.prop.dtm[i, rhythmrel.ngrams.length == 2] /
    sum(disco.rhythmrel.prop.dtm[i, rhythmrel.ngrams.length == 2])
  disco.rhythmrel.prop.dtm[i, rhythmrel.ngrams.length == 3] <-
    disco.rhythmrel.prop.dtm[i, rhythmrel.ngrams.length == 3] /
    sum(disco.rhythmrel.prop.dtm[i, rhythmrel.ngrams.length == 3])
}



disco.ngram.summary <- rbind(
  data.frame(type = 'pitch',
             length = pitch.ngrams.length,
             name = colnames(disco.pitch.dtm),
             instances = colSums(disco.pitch.dtm),
             proportion = colSums(disco.pitch.prop.dtm) / nrow(disco.meta),
             songs = colSums(disco.pitch.dtm > 0)
             ),
  data.frame(type = 'relative pitch',
             length = pitchrel.ngrams.length,
             name = colnames(disco.pitchrel.dtm),
             instances = colSums(disco.pitchrel.dtm),
             proportion = colSums(disco.pitchrel.prop.dtm) / nrow(disco.meta),
             songs = colSums(disco.pitchrel.dtm > 0)
             ),
  data.frame(type = 'rhythm',
             length = rhythm.ngrams.length,
             name = colnames(disco.rhythm.dtm),
             instances = colSums(disco.rhythm.dtm),
             proportion = colSums(disco.rhythm.prop.dtm) / nrow(disco.meta),
             songs = colSums(disco.rhythm.dtm > 0)
             ),
  data.frame(type = 'relative rhythm',
             length = rhythmrel.ngrams.length,
             name = colnames(disco.rhythmrel.dtm),
             instances = colSums(disco.rhythmrel.dtm),
             proportion = colSums(disco.rhythmrel.prop.dtm) / nrow(disco.meta),
             songs = colSums(disco.rhythmrel.dtm > 0)
             )
)
## summarize frequencies of absolute/relative pitch/rhythm 2/3-grams
disco.ngram.summary <-
  ddply(disco.ngram.summary, c('type', 'length'), function(x){
    x <- x[order(x$proportion, decreasing = TRUE),]
    x$proportion.cumulative <- cumsum(x$proportion)
    x$rank <- 1:nrow(x)
    return(x)
  })

## repeat by region
disco.ngram.region <- ldply(
  sort(unique(disco.meta$hraf_region)),
  function(region){
    ## subset to this region
    ind <- which(disco.meta$hraf_region == region)
    out <- rbind(
      data.frame(type = 'pitch',
                 length = pitch.ngrams.length,
                 name = colnames(disco.pitch.dtm),
                 instances = colSums(disco.pitch.dtm[ind,]),
                 proportion = colSums(disco.pitch.prop.dtm[ind,]) / length(ind),
                 songs = colSums(disco.pitch.dtm[ind,] > 0)
                 ),
      data.frame(type = 'relative pitch',
                 length = pitchrel.ngrams.length,
                 name = colnames(disco.pitchrel.dtm),
                 instances = colSums(disco.pitchrel.dtm[ind,]),
                 proportion = colSums(disco.pitchrel.prop.dtm[ind,]) / length(ind),
                 songs = colSums(disco.pitchrel.dtm[ind,] > 0)
                 ),
      data.frame(type = 'rhythm',
                 length = rhythm.ngrams.length,
                 name = colnames(disco.rhythm.dtm),
                 instances = colSums(disco.rhythm.dtm[ind,]),
                 proportion = colSums(disco.rhythm.prop.dtm[ind,]) / length(ind),
                 songs = colSums(disco.rhythm.dtm[ind,] > 0)
                 ),
      data.frame(type = 'relative rhythm',
                 length = rhythmrel.ngrams.length,
                 name = colnames(disco.rhythmrel.dtm),
                 instances = colSums(disco.rhythmrel.dtm[ind,]),
                 proportion = colSums(disco.rhythmrel.prop.dtm[ind,]) / length(ind),
                 songs = colSums(disco.rhythmrel.dtm[ind,] > 0)
                 )
    )
    ## summarize frequencies of absolute/relative pitch/rhythm 2/3-grams
    out <-
      ddply(out, c('type', 'length'), function(x){
        x <- x[order(x$proportion, decreasing = TRUE),]
        x$proportion.cumulative <- cumsum(x$proportion)
        x$region.rank <- 1:nrow(x)
        return(x)
      })
    out$region <- region
    return(out)

  })




## extract and clean up absolute pitch 2-grams for output
pitch.names <-
  c('C', 'C#', 'D', 'Eb', 'E', 'F', 'F#', 'G', 'Ab', 'A', 'Bb', 'B')
disco.pitch.transitions.summary <- disco.ngram.summary[
  disco.ngram.summary$length == 2 & disco.ngram.summary$type == 'pitch',
  ]
disco.pitch.transitions.summary$from <- as.integer(
  gsub('(\\d+)_(\\d+)', '\\1', disco.pitch.transitions.summary$name)
  )
disco.pitch.transitions.summary$to <- as.integer(
  gsub('(\\d+)_(\\d+)', '\\2', disco.pitch.transitions.summary$name)
)
disco.pitch.transitions.summary$from.pitch <- factor(
  disco.pitch.transitions.summary$from,
  levels = 0:11,
  labels = pitch.names
)
disco.pitch.transitions.summary$to.pitch <- factor(
  disco.pitch.transitions.summary$to,
  levels = 0:11,
  labels = pitch.names
)
disco.pitch.transitions.summary$proportion.exit <- NA
for (i in 0:11){
  ind <- which(disco.pitch.transitions.summary$from == i)
  disco.pitch.transitions.summary$proportion.exit[ind] <-
    disco.pitch.transitions.summary$proportion[ind] /
    sum(disco.pitch.transitions.summary$proportion[ind])
}
## output
write.csv(disco.pitch.transitions.summary,
          file.path(results.dir, 'disco_ngram_pitchtransitions_final.csv')
          )
## plot transition rarity
ggplot(disco.pitch.transitions.summary,
       aes(x = to.pitch,
           y = from.pitch,
           fill = proportion,
           label = round(proportion, 3)
           )
       ) +
  geom_tile() +
  geom_text() +
  scale_fill_gradient(trans = 'sqrt') +
  ggtitle('transition rarity')
## plot transition matrix
ggplot(disco.pitch.transitions.summary,
       aes(x = to.pitch,
           y = from.pitch,
           fill = proportion.exit,
           label = round(proportion.exit, 3)
           )
       ) +
  geom_tile() +
  geom_text() +
  scale_fill_gradient(trans = 'sqrt') +
  ggtitle('transition probability conditional on first pitch')



##############################################################################
## examine relative pitch/rhythm 2/3-grams, estimate zipf-mandelbrot params ##
##############################################################################

zm.coefs <- list()
zm.plots <- list()
zm.plots.loglog <- list()
zm.rsq <- list()
zm.coefs.region <- list()
zm.rsq.region <- list()

## number of possible categories (not well-defined for rhythm-grams)
Ns <- c(relpitch_2gram = length(-11:11),
        relpitch_3gram = length(-11:11)^2,
        relrhythm_2gram = NULL,
        relrhythm_3gram = NULL
        )

## look up global prevalence for each regional ngram,
disco.ngram.region$global.rank <- NA
for (type in c('relative pitch', 'relative rhythm')){
  for (n in 2:3){
    d <- disco.ngram.summary[
      disco.ngram.summary$type == type &
        disco.ngram.summary$length == n,
      ]
    ind <- which(disco.ngram.region$type == type &
                   disco.ngram.region$length == n
                 )
    disco.ngram.region$global.rank[ind] <-
      d$rank[match(disco.ngram.region$name[ind], d$name)]
  }
}



## define subset of interest
for (type in c('relative pitch', 'relative rhythm')){
  for (n in 2:3){

    name <- sprintf('%s_%sgram', gsub('relative ', 'rel', type), n)
    ## subset to this class of ngrams
    d <- disco.ngram.summary[
      disco.ngram.summary$type == type &
        disco.ngram.summary$length == n,
      ]
    d.region <- disco.ngram.region[
      disco.ngram.region$type == type &
        disco.ngram.region$length == n,
      ]

    ## extract various statistics
    freqs <- d$instances     # counts of unique events
    class(freqs) <- 'table'  #   (let zm.ll recognize as counts)
    N <- Ns[name]            # number of possible events
    if (is.na(N)){           #   (if unknown, estimate following zm.ll)
      N <- nrow(d)
    }

    ## fitted proportion predicted by zipf-mandelbrot
    fit <- zm.ll(freqs, N, dist = 'Zipf-Man')
    zm.coefs[[name]] <- fit@coef
    normalization <- sum(1 / (1:N + fit@coef['b'])^fit@coef['s'])
    d$proportion.zm <-
      1 / (d$rank + fit@coef['b'])^fit@coef['s'] / normalization
    zm.rsq[[name]] <-
      1 - var(d$proportion - d$proportion.zm) / var(d$proportion)
    write.csv(d, file.path(results.dir, sprintf('disco_ngram_%s_final.csv', name)))

    ## fitted proportion predicted by zipf-mandelbrot, by region
    zm.coefs.region[[name]] <- list()
    zm.rsq.region[[name]] <- list()
    d.region$proportion.zm <- NA
    for (region in unique(d.region$region)){
      ind <- which(d.region$region == region)
      freqs <- d.region$instances[ind]     # counts of unique events
      class(freqs) <- 'table'  #   (let zm.ll recognize as counts)
      fit <- zm.ll(freqs, N, dist = 'Zipf-Man')
      normalization <- sum(1 / (1:N + fit@coef['b'])^fit@coef['s'])
      d.region$proportion.zm[ind] <-
        1 / (d.region$region.rank[ind] + fit@coef['b'])^fit@coef['s'] / normalization
      zm.coefs.region[[name]][[region]] <- fit@coef
      zm.rsq.region[[name]][[region]] <-
        1 -
        var(d.region$proportion[ind] - d.region$proportion.zm[ind]) /
        var(d.region$proportion[ind])
    }
    write.csv(d.region,
              file.path(results.dir, sprintf('disco_ngram_%s_region_final.csv', name))
              )

    ## make inspection plots
    zm.plots[[name]] <- ggplot(d) +
      geom_line(aes(x = global.rank,
                    y = proportion,
                    color = region
                    ),
                data = d.region
                ) +
      geom_point(aes(x = rank,
                     y = proportion
                     ),
                 size = 2
                 ) +
      geom_line(aes(x = rank,
                    y = proportion.zm
                    ),
                linetype = 'dashed',
                color = 'gray80'
                ) +
      ggtitle(sprintf('%s %s-grams (R^2 = %s)',
                      type,
                      n,
                      round(zm.rsq[[name]], 3)
                      )
              ) +
      theme(legend.position = 'bottom')

    zm.plots.loglog[[name]] <- ggplot(d, aes(x = rank)) +
      geom_line(aes(x = global.rank,
                    y = proportion,
                    color = region
                    ),
                data = d.region
                ) +
      geom_point(aes(x = rank,
                     y = proportion
                     ),
                 size = 2
                 ) +
      geom_line(aes(x = rank,
                    y = proportion.zm
                    ),
                linetype = 'dashed',
                color = 'gray80'
                ) +
      scale_x_log10() +
      scale_y_log10() +
      ggtitle(sprintf('%s %s-grams, log-log scale (R^2 = %s)',
                      type,
                      n,
                      round(zm.rsq[[name]], 3)
                      )
              ) +
    theme(legend.position = 'bottom')

  }
}

do.call(grid.arrange,
        zm.plots
        )

do.call(grid.arrange,
        zm.plots.loglog
        )

zm.coefs
zm.rsq

lapply(zm.coefs.region, unlist)
lapply(zm.rsq.region, unlist)
