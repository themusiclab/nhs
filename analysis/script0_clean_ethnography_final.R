options(stringsAsFactors = FALSE)
`%.%` <- paste0

data.dir <- '../data/nhs'

## make folders
results.dir <- '../results'
dir.create(results.dir)



###################
## read nhs data ##
###################

## invalid multibyte characters were stripped from free text as follows:
##   iconv NHSEthnography_FreeText.csv -f UTF-8 -t UTF-8 -c -o NHSEthnography_FreeText_clean.csv

eth.prim <- read.csv(file.path(data.dir, 'NHSEthnography_AnnotatePrim.csv'))
eth.sec <- read.csv(file.path(data.dir, 'NHSEthnography_AnnotateSec.csv'))
eth.text <- read.csv(file.path(data.dir, 'NHSEthnography_FreeText_clean.csv'))

eth <- cbind(eth.prim,
             eth.sec[-match('indx', colnames(eth.sec))],
             eth.text[-match('indx', colnames(eth.text))]
             )
check.for.empty.lyrics <- gsub('[[:punct:]]', ' ', eth$lyric)
check.for.empty.lyrics <- gsub('^\\s+|\\s+$', '', check.for.empty.lyrics)

## for unique ethnography snippets or first occurrence, assign own index
##   for 2nd+ occurrence, assign index of first instance
duplicate.eth.lookup <- 1:nrow(eth)
duplicate.eth.lookup[eth$text_duplicate == 1] <-
  match(eth$text[eth$text_duplicate == 1], eth$text)

## clone ethnography and mark lyrics by uppercasing
##   (initialize at unmarked values and substitute marked version later)
eth$text_upcase_lyrics <- tolower(eth$text)

## windowing params for pass 1:
eth$changed.pass1 <- FALSE
window.shift.pass1 <- 1
window.size.pass1 <- 10
eth$changed.pass2 <- FALSE
window.shift.pass2 <- 1
window.size.pass2 <- 10

sink(file.path(results.dir, 'eth_nuke_lyrics.txt'))
for (i in which(check.for.empty.lyrics != '')){

  ind <- duplicate.eth.lookup[i]

  cat(rep('-', 10), '\ni = ', i, '\n', sep = '')
  if (ind != i){
    cat('  (duplicate of i = ', ind, ')\n', sep = '')
  }
  cat('\n')

  ethnography <- gsub("'", 'APOSTROPHE', eth$text_upcase_lyrics[ind])
  ethnography <- gsub('[[:punct:]]', ' ', ethnography)
  ethnography <- gsub('APOSTROPHE', "'", ethnography)
  ethnography <- gsub('\\s{2,}', ' ', ethnography)
  ethnography <- gsub('^\\s+|\\s+$', '', ethnography)
  ethnography.upper <- ethnography

  lyric <- gsub("'", 'APOSTROPHE', tolower(eth$lyric[i]))
  lyric <- gsub('[[:punct:]]', ' ', lyric)
  lyric <- gsub('APOSTROPHE', "'", lyric)
  lyric <- gsub('\\s{2,}', ' ', lyric)
  lyric <- gsub('^\\s+|\\s+$', '', lyric)
  lyric.words <- strsplit(lyric, ' ')[[1]]

  ## word-wise batching
  batch.n <- max(
    1,
    ceiling((length(lyric.words) - window.size.pass1) / window.shift.pass1) + 1
  )

  ## first pass: edit distance costs insert=1, delete=1, substitute=1
  for (batch in 1:batch.n){

    lyric.batch.words <- lyric.words[
        seq((batch-1) * window.shift.pass1 + 1,
            min((batch-1) * window.shift.pass1 + 1 + window.size.pass1,
                length(lyric.words)
                ),
            1
            )
      ]

    ## if lyric batch is short but full lyric is longer than window.size
    ##   then take last window.size words and convert prev matches to lowercase
    ##   so overlapping section can be matched
    if (length(lyric.batch.words) < window.size.pass1 && batch > 1){
      lyric.batch.words <- lyric.words[
        seq(length(lyric.words) - window.size.pass1 + 1, length(lyric.words))
      ]
    }

    lyric.batch <- paste(lyric.batch.words, collapse = ' ')
    if (batch == 1){
      ethnography.downcase.prev <- ethnography.upper
    } else {
      ethnography.downcase.prev <-
        paste(align.pre, tolower(align.batch), align.post, sep = '')
    }

    alignment <- adist(lyric.batch,
                       ethnography.downcase.prev,
                       counts = TRUE,
                       partial = TRUE
                       )
    align.cost <- alignment[1, 1] / min(nchar(lyric.batch), nchar(ethnography))

    align.start <- attr(alignment, 'offsets')[1, 1, 'first']
    align.end <-  attr(alignment, 'offsets')[1, 1, 'last']

    ## pre-match
    align.pre <- ifelse(
      align.start > 1,
      yes = substr(ethnography.upper, 1, align.start - 1),
      no = ''
    )

    ## match
    align.batch <- substr(ethnography, align.start, align.end)

    ## post-match
    align.post <- ifelse(
      align.end < nchar(ethnography),
      yes = substr(ethnography.upper, align.end + 1, nchar(ethnography.upper)),
      no = ''
    )

    ## if no match found for this window, skip
    if (align.cost > .05){
      next
    }

    ethnography.upper <- paste(align.pre,
                               toupper(align.batch),
                               align.post,
                               sep = ''
                               )

  }
  eth$changed.pass1[i] <- ethnography.upper != ethnography

  ## second pass with more aggressive insertion settings
  ##   (should only activate if first stage missed lyric b/c otherwise
  ##    first stage would upcase its matches & substitutions are costly)

  batch.n <- max(
    1,
    ceiling((length(lyric.words) - window.size.pass2) / window.shift.pass2) + 1
  )

  ## second pass: edit distance costs insert=1, delete=5, substitute=5
  ##   with higher cost threshold to address situations like
  ##   "NATIVE LYRICS [ENGLISH TRANS] NATIVE LYRICS [ENGLISH TRANS]"
  ##   representing 5-10% of lyrics
  for (batch in 1:batch.n){

    lyric.batch.words <- lyric.words[
      seq((batch-1) * window.shift.pass2 + 1,
          min((batch-1) * window.shift.pass2 + 1 + window.size.pass2,
              length(lyric.words)
              ),
          1
          )
    ]

    ## if lyric batch is short but full lyric is longer than window.size
    ##   then take last window.size words and convert prev matches to lowercase
    ##   so overlapping section can be matched
    if (length(lyric.batch.words) < window.size.pass2 && batch > 1){
      lyric.batch.words <- lyric.words[
        seq(length(lyric.words) - window.size.pass2 + 1, length(lyric.words))
      ]
    }

    lyric.batch <- paste(lyric.batch.words, collapse = ' ')
    if (batch == 1){
      ethnography.downcase.prev <- ethnography.upper
    } else {
      ethnography.downcase.prev <-
        paste(align.pre, align.batch, align.post, sep = '')
    }

    alignment <- adist(lyric.batch,
                       ethnography.downcase.prev,
                       costs = c(insertions = 1,
                                 deletions = 20,
                                 substitutions = 20
                                 ),
                       counts = TRUE,
                       partial = TRUE
                       )
    align.cost <- alignment[1, 1] / min(nchar(lyric.batch), nchar(ethnography))

    align.start <- attr(alignment, 'offsets')[1, 1, 'first']
    align.end <-  attr(alignment, 'offsets')[1, 1, 'last']

    ## pre-match
    align.pre <- ifelse(
      align.start > 1,
      yes = substr(ethnography.upper, 1, align.start - 1),
      no = ''
    )

    ## match
    align.batch <- substr(ethnography, align.start, align.end)

    ## post-match
    align.post <- ifelse(
      align.end < nchar(ethnography),
      yes = substr(ethnography.upper, align.end + 1, nchar(ethnography.upper)),
      no = ''
    )

    ## if still no match found for this window, skip
    if (align.cost > 1){
      next
    }

    ethnography.upper <- paste(align.pre,
                               toupper(align.batch),
                               align.post,
                               sep = ''
                               )

  }

  ## only mark words that are fully matched
  ##   (partial matches are typically low-quality matches on window boundary)
  ethnography.upper <- gsub('\\b([a-z]+[A-Z]+|[A-Z]+[a-z]+)\\b',
                            '\\L\\1',
                            ethnography.upper,
                            perl = TRUE
                            )


  ## update marked ethnography for subsequent lyrics from same snippet
  eth$text_upcase_lyrics[ind] <- ethnography.upper

  cat('LYRICS:\n', lyric, '\n\n')
  cat('ETHNOGRAPHY:\n',
      ethnography.upper,
      '\n\n')

  eth$changed.pass2[i] <- ethnography.upper != ethnography

}
sink()

## process lyric-free ethnography in the same way
##   as ethnography that contained (now purged) lyrics
emptylyrics.ind <- which(check.for.empty.lyrics == '')
eth$text_upcase_lyrics[emptylyrics.ind] <-
  gsub("'", 'APOSTROPHE', eth$text_upcase_lyrics[emptylyrics.ind])
eth$text_upcase_lyrics[emptylyrics.ind] <-
  gsub('[[:punct:]]', ' ', eth$text_upcase_lyrics[emptylyrics.ind])
eth$text_upcase_lyrics[emptylyrics.ind] <-
  gsub('APOSTROPHE', "'", eth$text_upcase_lyrics[emptylyrics.ind])
eth$text_upcase_lyrics[emptylyrics.ind] <-
  gsub('\\s{2,}', ' ', eth$text_upcase_lyrics[emptylyrics.ind])
eth$text_upcase_lyrics[emptylyrics.ind] <-
  gsub('^\\s+|\\s+$', '', eth$text_upcase_lyrics[emptylyrics.ind])

## copy cleaned version of ethnographic text over all duplicates of original
eth$text_purge_lyrics <- eth$text_upcase_lyrics[duplicate.eth.lookup]
eth$text_purge_lyrics <- gsub('[A-Z]', '', eth$text_purge_lyrics)
eth$text_purge_lyrics <- gsub(' {2,}', ' ', eth$text_purge_lyrics)
eth$text_purge_lyrics <- gsub('^\\s+|\\s+$', '', eth$text_purge_lyrics)

eth.text$text_nolyrics <- ''
eth.text$text_nolyrics[match(eth.nolyrics$indx, eth.text$indx)] <-
  eth.nolyrics$text.nolyrics

write.csv(eth.text,
          file.path(data.dir, 'NHSEthnography_FreeText_nolyrics.csv'),
          row.names = FALSE
          )

