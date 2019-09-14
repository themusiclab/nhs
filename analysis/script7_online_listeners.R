library(diptest)
library(testit)

## Function to calculate the i_th mode ('rank') for character vec
Mode <- function(x, rank) {
    names(sort(table(x), decreasing = TRUE))[rank]
}

## Return 1 if multimodal, else 0
multimodalSong <- function(ratings) {
    ratings <- ratings[ratings != "."]
    return(
        ifelse(dip.test(ratings)[["p.value"]] < .05, 1, 0)
    )
}

data.dir <- file.path('..', 'data', 'nhs')

## Load expert annotations data
annotate.dat <-
  read.csv(file.path(data.dir, 'NHSDiscography_Annotate.csv'))

## NOTE: Set to true to get results for PhDs
subset.to.phds <- FALSE
if(subset.to.phds){
    annotate.dat <- annotate.dat[annotate.dat$annotator_degree == "PhD",]
}

## Load features and predicted pitch from the algorithm
transcription.dat <-
  read.csv(file.path(data.dir, 'NHSDiscography_TranscriptionFeatures.csv'))

## What proportion of song annotations were "tonal"
mean(annotate.dat$tonal)

## More than two-thirds of songs were rated as "tonal" by all thirty expert listeners
avg.tonal.rtg <- tapply(annotate.dat$tonal, annotate.dat$song, mean)
mean(avg.tonal.rtg)
mean(avg.tonal.rtg == 1)
sum(avg.tonal.rtg >= .9)
min(avg.tonal.rtg)

## Key mapping from algorithm prediction to tonal center (we ignore Maj/min)
krumhansl.mapping <- data.frame(key = c("C",
                                        "C#",
                                        "D",
                                        "D#",
                                        "E",
                                        "F",
                                        "F#",
                                        "G",
                                        "G#",
                                        "A",
                                        "A#",
                                        "B",
                                        "C",
                                        "C#",
                                        "D",
                                        "D#",
                                        "E",
                                        "F",
                                        "F#",
                                        "G",
                                        "G#",
                                        "A",
                                        "A#",
                                        "B"
                                        ),
                                number = seq(0,23)
                                )

## Create new column for character tonal center predicted by algo
transcription.dat$key1.char <- krumhansl.mapping$key[transcription.dat$key1 + 1]
transcription.dat$key2.char <- krumhansl.mapping$key[transcription.dat$key2 + 1]
transcription.dat$key3.char <- krumhansl.mapping$key[transcription.dat$key3 + 1]
transcription.dat$key4.char <- krumhansl.mapping$key[transcription.dat$key4 + 1]
transcription.dat$key5.char <- krumhansl.mapping$key[transcription.dat$key5 + 1]

## Visually inspect to make sure everything looks right
transcription.dat[,grepl("key", colnames(transcription.dat))]

## Add krumhansl scores to the annotations data
annotate.dat$key1.char <- transcription.dat$key1.char[annotate.dat$song]
annotate.dat$key2.char <- transcription.dat$key2.char[annotate.dat$song]
annotate.dat$key3.char <- transcription.dat$key3.char[annotate.dat$song]
annotate.dat$key4.char <- transcription.dat$key4.char[annotate.dat$song]
annotate.dat$key5.char <- transcription.dat$key5.char[annotate.dat$song]

## Create a numeric coding of human prediction for the dip test
annotate.dat$tonal_pitch1.numeric <- match(annotate.dat$tonal_pitch1,
                                               levels(annotate.dat$tonal_pitch1))
## Recode the 'no tonal center' codings as NA
annotate.dat$tonal_pitch1.numeric[annotate.dat$tonal_pitch1 == "."] <- NA

## The numeric score makes "A" equal to 1, recode so 0 is "C", 1 is "C#", etc
annotate.dat$tonal_pitch1.numeric <- ifelse(annotate.dat$tonal_pitch1.numeric - 5 < 0,
                                            annotate.dat$tonal_pitch1.numeric + 7,
                                            annotate.dat$tonal_pitch1.numeric - 5)

annotate.dat$first.mode <-
    tapply(annotate.dat$tonal_pitch1, annotate.dat$song, Mode, rank = 1)[annotate.dat$song]
annotate.dat$second.mode <-
    tapply(annotate.dat$tonal_pitch1, annotate.dat$song, Mode, rank = 2)[annotate.dat$song]
annotate.dat$third.mode <-
    tapply(annotate.dat$tonal_pitch1, annotate.dat$song, Mode, rank = 3)[annotate.dat$song]
annotate.dat$multimodal.song <-
    tapply(annotate.dat$tonal_pitch1.numeric, annotate.dat$song, multimodalSong)[annotate.dat$song]

unimodal.tonal.codes <- subset(annotate.dat,
                               annotate.dat$multimodal.song == 0)

multimodal.tonal.codes <- subset(annotate.dat,
                                 annotate.dat$multimodal.song == 1)

mean(annotate.dat$tonal_pitch1 == annotate.dat$first.mode)
mean((annotate.dat$tonal_pitch1 == annotate.dat$first.mode) |
     (annotate.dat$tonal_pitch1 == annotate.dat$second.mode))
mean((annotate.dat$tonal_pitch1 == annotate.dat$first.mode) |
     (annotate.dat$tonal_pitch1 == annotate.dat$second.mode) |
     (annotate.dat$tonal_pitch1 == annotate.dat$third.mode))

mean(unimodal.tonal.codes$tonal_pitch1 == unimodal.tonal.codes$first.mode)
mean((unimodal.tonal.codes$tonal_pitch1 == unimodal.tonal.codes$first.mode) |
     (unimodal.tonal.codes$tonal_pitch1 == unimodal.tonal.codes$second.mode))
mean((unimodal.tonal.codes$tonal_pitch1 == unimodal.tonal.codes$first.mode) |
     (unimodal.tonal.codes$tonal_pitch1 == unimodal.tonal.codes$second.mode) |
     (unimodal.tonal.codes$tonal_pitch1 == unimodal.tonal.codes$third.mode))

mean(multimodal.tonal.codes$tonal_pitch1 == multimodal.tonal.codes$first.mode)
mean((multimodal.tonal.codes$tonal_pitch1 == multimodal.tonal.codes$first.mode) |
     (multimodal.tonal.codes$tonal_pitch1 == multimodal.tonal.codes$second.mode))
mean((multimodal.tonal.codes$tonal_pitch1 == multimodal.tonal.codes$first.mode) |
     (multimodal.tonal.codes$tonal_pitch1 == multimodal.tonal.codes$second.mode) |
     (multimodal.tonal.codes$tonal_pitch1 == multimodal.tonal.codes$third.mode))

# Collapse so a row is a song instead of an annotator
cols.to.collapse.on <- c("song",
                         "first.mode",
                         "key1.char",
                         "key2.char",
                         "key3.char",
                         "key4.char",
                         "key5.char")
unimodal.tonal.songs <- subset(annotate.dat,
                               annotate.dat$multimodal.song == 0,
                               select = cols.to.collapse.on)

cols.to.collapse.on <- c(cols.to.collapse.on, "second.mode")
multimodal.tonal.songs <- subset(annotate.dat,
                                 annotate.dat$multimodal.song == 1,
                                 select = cols.to.collapse.on)

unimodal.tonal.songs <- unique(unimodal.tonal.songs)
multimodal.tonal.songs <- unique(multimodal.tonal.songs)

## The algorithm's first-rank estimate for the tonal center matched the expert listeners' modal rating
# Unimodal
one.key.unimodal.accuracy <- mean(unimodal.tonal.songs$first.mode == unimodal.tonal.songs$key1.char)

two.key.unimodal.accuracy <- mean(unimodal.tonal.songs$first.mode == unimodal.tonal.songs$key1.char |
                                  unimodal.tonal.songs$first.mode == unimodal.tonal.songs$key2.char)

three.key.unimodal.accuracy <- mean(unimodal.tonal.songs$first.mode == unimodal.tonal.songs$key1.char |
                                    unimodal.tonal.songs$first.mode == unimodal.tonal.songs$key2.char |
                                    unimodal.tonal.songs$first.mode == unimodal.tonal.songs$key3.char)
        
four.key.unimodal.accuracy <- mean(unimodal.tonal.songs$first.mode == unimodal.tonal.songs$key1.char |
                                   unimodal.tonal.songs$first.mode == unimodal.tonal.songs$key2.char |
                                   unimodal.tonal.songs$first.mode == unimodal.tonal.songs$key3.char |
                                   unimodal.tonal.songs$first.mode == unimodal.tonal.songs$key4.char)

five.key.unimodal.accuracy <- mean(unimodal.tonal.songs$first.mode == unimodal.tonal.songs$key1.char |
                                   unimodal.tonal.songs$first.mode == unimodal.tonal.songs$key2.char |
                                   unimodal.tonal.songs$first.mode == unimodal.tonal.songs$key3.char |
                                   unimodal.tonal.songs$first.mode == unimodal.tonal.songs$key4.char |
                                   unimodal.tonal.songs$first.mode == unimodal.tonal.songs$key5.char)

## Bimodal
one.key.multimodal.accuracy <- mean(multimodal.tonal.songs$first.mode == multimodal.tonal.songs$key1.char |
                                    multimodal.tonal.songs$second.mode == multimodal.tonal.songs$key1.char)

two.key.multimodal.accuracy <- mean(multimodal.tonal.songs$first.mode == multimodal.tonal.songs$key1.char |
                                    multimodal.tonal.songs$second.mode == multimodal.tonal.songs$key1.char |
                                    multimodal.tonal.songs$first.mode == multimodal.tonal.songs$key2.char |
                                    multimodal.tonal.songs$second.mode == multimodal.tonal.songs$key2.char)

three.key.multimodal.accuracy <- mean(multimodal.tonal.songs$first.mode == multimodal.tonal.songs$key1.char |
                                   multimodal.tonal.songs$second.mode == multimodal.tonal.songs$key1.char |
                                   multimodal.tonal.songs$first.mode == multimodal.tonal.songs$key2.char |
                                   multimodal.tonal.songs$second.mode == multimodal.tonal.songs$key2.char |
                                   multimodal.tonal.songs$first.mode == multimodal.tonal.songs$key3.char |
                                   multimodal.tonal.songs$second.mode == multimodal.tonal.songs$key3.char)

four.key.multimodal.accuracy <- mean(multimodal.tonal.songs$first.mode == multimodal.tonal.songs$key1.char |
                                  multimodal.tonal.songs$second.mode == multimodal.tonal.songs$key1.char |
                                  multimodal.tonal.songs$first.mode == multimodal.tonal.songs$key2.char |
                                  multimodal.tonal.songs$second.mode == multimodal.tonal.songs$key2.char |
                                  multimodal.tonal.songs$first.mode == multimodal.tonal.songs$key3.char |
                                  multimodal.tonal.songs$second.mode == multimodal.tonal.songs$key3.char |
                                  multimodal.tonal.songs$first.mode == multimodal.tonal.songs$key4.char |
                                  multimodal.tonal.songs$second.mode == multimodal.tonal.songs$key4.char)

five.key.multimodal.accuracy <- mean(multimodal.tonal.songs$first.mode == multimodal.tonal.songs$key1.char |
                                  multimodal.tonal.songs$second.mode == multimodal.tonal.songs$key1.char |
                                  multimodal.tonal.songs$first.mode == multimodal.tonal.songs$key2.char |
                                  multimodal.tonal.songs$second.mode == multimodal.tonal.songs$key2.char |
                                  multimodal.tonal.songs$first.mode == multimodal.tonal.songs$key3.char |
                                  multimodal.tonal.songs$second.mode == multimodal.tonal.songs$key3.char |
                                  multimodal.tonal.songs$first.mode == multimodal.tonal.songs$key4.char |
                                  multimodal.tonal.songs$second.mode == multimodal.tonal.songs$key4.char |
                                  multimodal.tonal.songs$first.mode == multimodal.tonal.songs$key5.char |
                                  multimodal.tonal.songs$second.mode == multimodal.tonal.songs$key5.char)

prop.unimodal <- nrow(unimodal.tonal.songs) / length(unique(annotate.dat$song))
prop.multimodal <- nrow(multimodal.tonal.songs) / length(unique(annotate.dat$song))
assert((prop.multimodal + prop.unimodal) == 1)

# Combine for overall accuracy
overall.one.key.accuracy <- one.key.unimodal.accuracy * prop.unimodal + one.key.multimodal.accuracy * prop.multimodal
overall.two.key.accuracy <- two.key.unimodal.accuracy * prop.unimodal +  two.key.multimodal.accuracy * prop.multimodal
overall.three.key.accuracy <- three.key.unimodal.accuracy * prop.unimodal + three.key.multimodal.accuracy * prop.multimodal
overall.four.key.accuracy <- four.key.unimodal.accuracy * prop.unimodal + four.key.multimodal.accuracy * prop.multimodal
overall.five.key.accuracy <- five.key.unimodal.accuracy * prop.unimodal + five.key.multimodal.accuracy * prop.multimodal

## Pitch difference
multimodal.annotate.collapsed <- unique(multimodal.tonal.songs[,c("song", "first.mode", "second.mode")])

multimodal.annotate.collapsed$first.mode.int <- krumhansl.mapping$number[match(multimodal.annotate.collapsed$first.mode,
                                                                                  krumhansl.mapping$key)]

multimodal.annotate.collapsed$second.mode.int <- krumhansl.mapping$number[match(multimodal.annotate.collapsed$second.mode,
                                                                                   krumhansl.mapping$key)]

sort(table(abs(multimodal.annotate.collapsed$first.mode.int -
               multimodal.annotate.collapsed$second.mode.int)), decreasing = TRUE)

## Conduct a permutation test to see if this could happen by chance
perm.test.iters <- 1000

one.key.sampling.dist <- c()
two.key.sampling.dist <- c()
three.key.sampling.dist <- c()
four.key.sampling.dist <- c()
five.key.sampling.dist <- c()

for(i in 1:perm.test.iters){
    print(i)
    iter.dat <- data.frame(annotate.dat)
    for(coder in iter.dat$annotator){
        iter.dat[iter.dat$annotator == coder,"tonal_pitch1"] <-
            iter.dat[iter.dat$annotator == coder,"tonal_pitch1"][sample(sum(iter.dat$annotator == coder))]
        iter.dat[iter.dat$annotator == coder,"tonal_pitch1.numeric"] <-
            iter.dat[iter.dat$annotator == coder,"tonal_pitch1.numeric"][sample(sum(iter.dat$annotator == coder))]
    }
    
    iter.dat$first.mode <-
        tapply(iter.dat$tonal_pitch1, iter.dat$song, Mode, rank = 1)[iter.dat$song]
    iter.dat$second.mode <-
        tapply(iter.dat$tonal_pitch1, iter.dat$song, Mode, rank = 2)[iter.dat$song]
    iter.dat$third.mode <-
        tapply(iter.dat$tonal_pitch1, iter.dat$song, Mode, rank = 3)[iter.dat$song]
    iter.dat$multimodal.song <-
        tapply(iter.dat$tonal_pitch1.numeric, iter.dat$song, multimodalSong)[iter.dat$song]

    cols.to.collapse.on <- c("song", "first.mode", "key1.char", "key2.char", "key3.char", "key4.char", "key5.char")
    iter.unimodal.tonal.songs <- subset(iter.dat,
                                   iter.dat$multimodal.song == 0,
                                   select = cols.to.collapse.on)
    
    cols.to.collapse.on <- c(cols.to.collapse.on, "second.mode")
    iter.multimodal.tonal.songs <- subset(iter.dat,
                                     iter.dat$multimodal.song == 1,
                                     select = cols.to.collapse.on)
    
    iter.unimodal.tonal.songs <- unique(iter.unimodal.tonal.songs)
    iter.multimodal.tonal.songs <- unique(iter.multimodal.tonal.songs)

    ## The algorithm's first-rank estimate for the tonal center matched the expert listeners' modal rating
    ## Unimodal
    iter.one.key.unimodal.accuracy <- mean(iter.unimodal.tonal.songs$first.mode == iter.unimodal.tonal.songs$key1.char)
    
    iter.two.key.unimodal.accuracy <- mean(iter.unimodal.tonal.songs$first.mode == iter.unimodal.tonal.songs$key1.char |
                                           iter.unimodal.tonal.songs$first.mode == iter.unimodal.tonal.songs$key2.char)
    
    iter.three.key.unimodal.accuracy <- mean(iter.unimodal.tonal.songs$first.mode == iter.unimodal.tonal.songs$key1.char |
                                             iter.unimodal.tonal.songs$first.mode == iter.unimodal.tonal.songs$key2.char |
                                             iter.unimodal.tonal.songs$first.mode == iter.unimodal.tonal.songs$key3.char)
    
    iter.four.key.unimodal.accuracy <- mean(iter.unimodal.tonal.songs$first.mode == iter.unimodal.tonal.songs$key1.char |
                                            iter.unimodal.tonal.songs$first.mode == iter.unimodal.tonal.songs$key2.char |
                                            iter.unimodal.tonal.songs$first.mode == iter.unimodal.tonal.songs$key3.char |
                                            iter.unimodal.tonal.songs$first.mode == iter.unimodal.tonal.songs$key4.char)
    
    iter.five.key.unimodal.accuracy <- mean(iter.unimodal.tonal.songs$first.mode == iter.unimodal.tonal.songs$key1.char |
                                            iter.unimodal.tonal.songs$first.mode == iter.unimodal.tonal.songs$key2.char |
                                            iter.unimodal.tonal.songs$first.mode == iter.unimodal.tonal.songs$key3.char |
                                            iter.unimodal.tonal.songs$first.mode == iter.unimodal.tonal.songs$key4.char |
                                            iter.unimodal.tonal.songs$first.mode == iter.unimodal.tonal.songs$key5.char)
    
    ## Bimodal
    iter.one.key.multimodal.accuracy <- mean(iter.multimodal.tonal.songs$first.mode == iter.multimodal.tonal.songs$key1.char |
                                             iter.multimodal.tonal.songs$second.mode == iter.multimodal.tonal.songs$key1.char)
    
    iter.two.key.multimodal.accuracy <- mean(iter.multimodal.tonal.songs$first.mode == iter.multimodal.tonal.songs$key1.char |
                                             iter.multimodal.tonal.songs$second.mode == iter.multimodal.tonal.songs$key1.char |
                                             iter.multimodal.tonal.songs$first.mode == iter.multimodal.tonal.songs$key2.char |
                                             iter.multimodal.tonal.songs$second.mode == iter.multimodal.tonal.songs$key2.char)
    
    iter.three.key.multimodal.accuracy <- mean(iter.multimodal.tonal.songs$first.mode == iter.multimodal.tonal.songs$key1.char |
                                               iter.multimodal.tonal.songs$second.mode == iter.multimodal.tonal.songs$key1.char |
                                               iter.multimodal.tonal.songs$first.mode == iter.multimodal.tonal.songs$key2.char |
                                               iter.multimodal.tonal.songs$second.mode == iter.multimodal.tonal.songs$key2.char |
                                               iter.multimodal.tonal.songs$first.mode == iter.multimodal.tonal.songs$key3.char |
                                               iter.multimodal.tonal.songs$second.mode == iter.multimodal.tonal.songs$key3.char)
    
    iter.four.key.multimodal.accuracy <- mean(iter.multimodal.tonal.songs$first.mode == iter.multimodal.tonal.songs$key1.char |
                                              iter.multimodal.tonal.songs$second.mode == iter.multimodal.tonal.songs$key1.char |
                                              iter.multimodal.tonal.songs$first.mode == iter.multimodal.tonal.songs$key2.char |
                                              iter.multimodal.tonal.songs$second.mode == iter.multimodal.tonal.songs$key2.char |
                                              iter.multimodal.tonal.songs$first.mode == iter.multimodal.tonal.songs$key3.char |
                                              iter.multimodal.tonal.songs$second.mode == iter.multimodal.tonal.songs$key3.char |
                                              iter.multimodal.tonal.songs$first.mode == iter.multimodal.tonal.songs$key4.char |
                                              iter.multimodal.tonal.songs$second.mode == iter.multimodal.tonal.songs$key4.char)
    
    iter.five.key.multimodal.accuracy <- mean(iter.multimodal.tonal.songs$first.mode == iter.multimodal.tonal.songs$key1.char |
                                              iter.multimodal.tonal.songs$second.mode == iter.multimodal.tonal.songs$key1.char |
                                              iter.multimodal.tonal.songs$first.mode == iter.multimodal.tonal.songs$key2.char |
                                              iter.multimodal.tonal.songs$second.mode == iter.multimodal.tonal.songs$key2.char |
                                              iter.multimodal.tonal.songs$first.mode == iter.multimodal.tonal.songs$key3.char |
                                              iter.multimodal.tonal.songs$second.mode == iter.multimodal.tonal.songs$key3.char |
                                              iter.multimodal.tonal.songs$first.mode == iter.multimodal.tonal.songs$key4.char |
                                              iter.multimodal.tonal.songs$second.mode == iter.multimodal.tonal.songs$key4.char |
                                              iter.multimodal.tonal.songs$first.mode == iter.multimodal.tonal.songs$key5.char |
                                              iter.multimodal.tonal.songs$second.mode == iter.multimodal.tonal.songs$key5.char)
    
    iter.prop.unimodal <- nrow(iter.unimodal.tonal.songs) / length(unique(iter.dat$song))
    iter.prop.multimodal <- nrow(iter.multimodal.tonal.songs) / length(unique(iter.dat$song))
    assert((iter.prop.multimodal + iter.prop.unimodal) == 1)
    
    one.key.sampling.dist <- c(one.key.sampling.dist,
                               iter.one.key.unimodal.accuracy * prop.unimodal +
                               iter.one.key.multimodal.accuracy * prop.multimodal)
    two.key.sampling.dist <- c(two.key.sampling.dist,
                               iter.two.key.unimodal.accuracy * prop.unimodal +
                               iter.two.key.multimodal.accuracy * prop.multimodal)
    three.key.sampling.dist <-c(three.key.sampling.dist,
                                iter.three.key.unimodal.accuracy * prop.unimodal +
                                iter.three.key.multimodal.accuracy * prop.multimodal)
    four.key.sampling.dist <- c(four.key.sampling.dist,
                                iter.four.key.unimodal.accuracy * prop.unimodal +
                                iter.four.key.multimodal.accuracy * prop.multimodal)
    five.key.sampling.dist <- c(five.key.sampling.dist,
                                iter.five.key.unimodal.accuracy * prop.unimodal +
                                iter.five.key.multimodal.accuracy * prop.multimodal)       
}

# How often were the observed accuracies lower than those from random shuffling?
mean(overall.one.key.accuracy < sort(one.key.sampling.dist))
mean(overall.two.key.accuracy < sort(two.key.sampling.dist))
mean(overall.three.key.accuracy < sort(three.key.sampling.dist))
mean(overall.four.key.accuracy < sort(four.key.sampling.dist))
mean(overall.five.key.accuracy < sort(five.key.sampling.dist))
