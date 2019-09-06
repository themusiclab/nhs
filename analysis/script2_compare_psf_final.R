options(stringsAsFactors = FALSE)
## options(java.parameters = "-Xmx4096m")

## parallel
library(foreach)
library(doMC)
registerDoMC(cores = 20)

## data manipulation
library(XML)
library(jsonlite)
library(plyr)
library(reshape2)

## text
library(quanteda)

## math
library(Matrix)

## visualization
library(ggplot2)

## functions
`%.%` <- paste0
pval.1side.to.2side <- function(p){
    2 * pmin(p, 1 - p)
}

source('~/scripts/alert.R')

## dirs
nhs.dir <- file.path('..', 'data', 'nhs')
psf.dir <- file.path('..', 'data', 'psf', 'psfbag')
results.dir <- file.path('..', 'results')



###############
## ocm codes ##
###############

## ocm codes and major groupings
ocm.codes <- read.csv(file.path(nhs.dir, 'ocm_codes_all.csv'))
ocm.codes.major <- ocm.codes$code[nchar(ocm.codes$code) == 3 &
                                    substr(ocm.codes$code, 3, 3) == 0
                                  ]
ocm.codes.major <- substr(ocm.codes.major, 1, 2)
names(ocm.codes.major) <- make.names(
  gsub(
    ',',
    '',
    ocm.codes$subject[nchar(ocm.codes$code) == 3 &
                        substr(ocm.codes$code, 3, 3) == 0
                      ]
  )
)
ocm.codes.music <- c('533',  # music
                     '534',  # musical instruments
                     '545'   # musical & theatrical productions
                     )
names(ocm.codes.music) <- make.names(
  ocm.codes$subject[
    match(ocm.codes.music, ocm.codes$code)
  ]
)



#########
## psf ##
#########

## locate psf xml files for all cultures / docs
psf.fnames <- list.files(file.path(psf.dir, 'ethnography'),
                         pattern = '^[a-z]+\\d+-\\d+.xml$',
                         recursive = TRUE,
                         full.names = TRUE
                         )
## ## subset to docs that appear in nhs
names(psf.fnames) <- gsub('\\.xml', '', basename(psf.fnames))

## all origin documents successfully located
all(!is.na(psf.fnames))

## process and save psf data if it doesn't already exist, otherwise load it
if (!file.exists(file.path(results.dir, 'psf_ocms_by_para.rds')) |
      !file.exists(file.path(results.dir, 'psf_words_by_para.rds'))
    ){

  psf.ocms <- list()
  psf.words <- list()
  ## read all psf raw files
  for (i in seq_along(psf.fnames)){

    cat(i, '/', length(psf.fnames), '\n')

    doc <- names(psf.fnames)[i]

    ## read xml file
    doc.xml <- xmlParse(psf.fnames[doc])

    ## make list of paragraphs, each elem is vector of ocm codes
    para.ocms <- getNodeSet(doc.xml,
                           '//vref/vhead/ocms',
                           fun = function(x){
                             unlist(xmlToList(x))
                           })

    ## make list of paragraphs, each elem is named vector of words/counts
    para.words <- getNodeSet(doc.xml,
                            '//vref/vpara/vtags',
                            fun = function(x){
                              words <- unlist(
                                getNodeSet(x,
                                           'w',
                                           fun = function(y){
                                             xmlGetAttr(y, 't')
                                           })
                              )
                              counts <- unlist(
                                getNodeSet(x,
                                           'w',
                                           fun = function(y){
                                             xmlGetAttr(y, 'n')
                                           })
                              )
                              names(counts) <- words
                              return(counts)
                            })

    ## extract and append paragraph ids
    para.sid <- unlist(
      getNodeSet(doc.xml,
                 '//vref',
                 fun = function(x){
                   xmlGetAttr(x, 'sid')
                 })
    )
    names(para.ocms) <- para.sid
    names(para.words) <- para.sid

    ## store parsed results
    psf.ocms[[doc]] <- para.ocms
    psf.words[[doc]] <- para.words

    ## clean up
    free(doc.xml)
    rm(doc.xml)

  }

  ## psf.word is a list of docs -> list of paras
  ##   each para entry is named vector (words as names, word counts as entries)
  ## flatten into list of paras
  psf.words <- unlist(psf.words, recursive = FALSE)
  ## find all unique words in psf sample
  vocab <- sort(unique(unlist(
    lapply(psf.words, names)  # extract unique words for para
  )))
  ## convert to row/col indices in sparse dtm
  psf.ind <- ldply(psf.words,
                   function(x){
                     data.frame(term = match(names(x), vocab),
                                count = x
                                )
                   },
                   .id = 'para',
                   .progress = 'text'
                   )
  ## drop duplicated doc name from flattening list
  ##   (convert from culture-doc.culture-doc-para to culture-doc-para)
  psf.ind$para <- substr(psf.ind$para, 10, 25)
  para.ids <- sort(unique(psf.ind$para))
  psf.ind$doc <- match(psf.ind$para, para.ids)
  psf.ind$count <- as.numeric(psf.ind$count)
  ## convert to sparse dtm
  psf.dtm <- sparseMatrix(i = psf.ind$doc,
                          j = psf.ind$term,
                          x = psf.ind$count,
                          dimnames = list(para = para.ids,
                                          term = vocab
                                          )
                          )

  saveRDS(psf.ocms, file.path(results.dir, 'psf_ocms_by_para.rds'))
  saveRDS(psf.dtm, file.path(results.dir, 'psf_words_by_para.rds'))

} else {

  psf.ocms <- readRDS(file.path(results.dir, 'psf_ocms_by_para.rds'))
  psf.dtm <- readRDS(file.path(results.dir, 'psf_words_by_para.rds'))

}

psf.para.per.culture <- sapply(
  sort(unique(substr(names(psf.ocms), 1, 4))),
  function(culture.id){
    ind <- which(substr(names(psf.ocms), 1, 4) == culture.id)
    sum(sapply(psf.ocms[ind], length))
  })

## clean up ocms: discard 703 invalid codes out of 1.3m total (0.05%)
psf.ocms.broken <- unlist(psf.ocms)
psf.ocms.broken <- psf.ocms.broken[!psf.ocms.broken %in% ocm.codes$code]
length(psf.ocms.broken) / length(unlist(psf.ocms))
psf.ocms <- rapply(psf.ocms,
              function(x){
                x[x %in% ocm.codes$code]
              },
              how = 'replace'
              )
rm(psf.ocms.broken)

## nhs is based on jointly meeting keyword criteria and ocm criteria
##   because there are too many keyword passages for human review
##   we don't have the same limits here, so we'll use all the data
nhs.para.ids <- rownames(psf.dtm)[
  rowSums(
    psf.dtm[,c('song',
               'songs',
               'sing',
               'sings',
               'singer',
               'singers',
               'sang',
               'sung',
               'singing',
               'lullaby'
               )
            ]
  ) > 0
]

## link each para to its origin doc
nhs.para.per.doc <- table(substr(nhs.para.ids, 1, 8))
nhs.para.per.culture <- table(substr(nhs.para.ids, 1, 4))
nhs.doc.per.culture <- table(substr(unique(substr(nhs.para.ids, 1, 8)), 1, 4))

## final processing of psf dtm
psf.dtm <- new('dfmSparse', psf.dtm)
psf.dtm <- dfm_wordstem(psf.dtm)

## create nhs dtm
nhs.dtm <- psf.dtm[nhs.para.ids,]

## create nhs ocms
nhs.ocms <- unlist(psf.ocms, recursive = FALSE)
names(nhs.ocms) <- gsub('.*\\.', '', names(nhs.ocms))  # strip redundant doc id
nhs.ocms <- nhs.ocms[nhs.para.ids]
nhs.ocms.by.doc <- lapply(
  names(nhs.para.per.doc),
  function(doc){
    unlist(nhs.ocms[substr(nhs.para.ids, 1, 8) == doc])
  })

## overall ocm frequency in nhs source documents
ocm.freq <- table(unlist(psf.ocms))



#################################
## documents to culture groups ##
#################################

## culture metadata, including region, glotto family, etc
cultures <- read.csv(file.path(nhs.dir, 'NHSCultures_Metadata.csv'))
cultures$glotto_family[cultures$glotto_family == ''] <- NA

eth.meta <- read.csv(file.path(nhs.dir, 'NHSEthnography_Metadata.csv'))
cultures$hraf_region <- eth.meta$hraf_region[
  match(cultures$id_hraf, eth.meta$id_hraf)
]

## look up hraf culture id in metadata
nhs.doc.to.culture <- match(
  substr(names(nhs.para.per.doc), 1, 4),
  tolower(cultures$id_hraf)
)
names(nhs.doc.to.culture) <- names(nhs.para.per.doc)



#######################################################################
## compare nsf ocm codes and words to null distribution, full sample ##
#######################################################################

## fn to draw ocm codes under null of music being independent of
##   loop over docs
##     for doc i, look up number of paras in nhs from i
##     draw that number of paras from doc i
##     look up ocm codes for those paras and combine
##  concatenate over docs
sample.ocms <- function(){
    lapply(
      seq_along(nhs.para.per.doc),
      function(i){
        ## for doc i, draw same number of paras as there are in nhs
        doc.name <- names(nhs.para.per.doc)[i]
        if (length(psf.ocms[[doc.name]]) > 0){
            doc.ocm <- sample(psf.ocms[[doc.name]],
                              size = nhs.para.per.doc[i]
                              )
            ## concatenate this doc's ocm codes
            return(unlist(doc.ocm))
        } else {
            ## this doc has no ocm codes
            return(character(0))
        }
      })
}

doc.id.to.para.ids <-
  sapply(names(nhs.para.per.doc),
         function(doc.id){
           which(substr(rownames(psf.dtm), 1, 8) == doc.id)
         },
         simplify = 'list'
         )

sample.words <- function(dtm = psf.dtm, transpose = FALSE){
  ## if only a subset of words are needed,
  ##   drop unnecessary columns before selecting rows
  ind <- unlist(sapply(
    seq_along(nhs.para.per.doc),
    function(i){
      ## for doc i, draw same number of paras as there are in nhs
      doc.name <- names(nhs.para.per.doc)[i]
      doc.ind <- sample(doc.id.to.para.ids[[i]],
                        size = nhs.para.per.doc[i]
                        )
      return(doc.ind)
    }))
  if (!transpose){
    dtm[ind,]
  } else {
    dtm[,ind]
  }
}

## ## fn to build wordlists from seed words
## ##   by crawling outward in WordNet lexical database
## append.related.words <- function(rootwords, depth = 1){
##   word.relations <- c(
##     ## "Antonym" = "!",
##     ## "Domain of synset - Topic" = ";c",
##     ## "Member of this domain - Topic" = "-c",
##     ## "Domain of synset - Region" = ";r",
##     ## "Member of this domain - Region" = "-r",
##     ## "Domain of synset - Usage" = ";u",
##     ## "Member of this domain - Usage" = "-u",
##     ## "Hypernym" = "@",
##     ## "Instance Hypernym" = "@i",
##     ## "Hyponym" = "~",
##     ## "Instance Hyponym" = "~i",
##     ## "Member holonym" = "#m",
##     ## "Substance holonym" = "#s",
##     ## "Part holonym" = "#p",
##     ## "Member meronym" = "%m",
##     ## "Substance meronym" = "%s",
##     ## "Part meronym" = "%p",
##     "Also see" = "^",
##     "Attribute" = "=",
##     "Entailment" = "*",
##     "Cause" = ">",
##     "Verb Group" = "$",
##     "Derivationally related form" = "+",
##     "Participle of verb" = "<",
##     "Partainym (pertains to noun) / Derived from adjective" = "\\",
##     "Similar to" = "&"
##   )
##   for (i in 1:depth){
##     ## get words in wordnet
##     filters <- sapply(
##       rootwords,
##       function(x){
##         getTermFilter('ExactMatchFilter', x, ignoreCase = TRUE)
##       })
##     terms <- unlist(lapply(
##       filters,
##       function(filter){
##         lapply(c('NOUN', 'VERB', 'ADJECTIVE', 'ADVERB'),
##                function(pos){
##                  getIndexTerms(pos, maxLimit = 1000, filter)
##                })
##       }
##     ))
##     ## identify related words by part-of-speech and relations
##     relatedwords <- unlist(
##       lapply(terms,
##              function(term){
##                lapply(getSynsets(term),
##                       function(synset){
##                         lapply(word.relations,
##                                function(relation){
##                                  lapply(getRelatedSynsets(synset, relation),
##                                         getWord
##                                         )
##                                })
##                       })
##              })
##     )
##     ## drop multi-word synonyms
##     relatedwords <- relatedwords[!grepl(' ', relatedwords)]
##     rootwords <- unique(c(rootwords, relatedwords))
##   }
##   ## stem and drop words that do not appear in corpus
##   rootwords <- char_wordstem(rootwords)
##   relatedwords <- char_wordstem(relatedwords)
##   words <- unique(c(rootwords, relatedwords))
##   words <- words[words %in% colnames(psf.dtm) &
##                    words %in% colnames(nhs.dtm)
##                  ]
##   return(words)
## }

## read in word/ocm lists for each hypothesis; to reduce memory use,
##   we'll only keep word null distribution for hypothesis-related words
hypotheses <- read.csv(file.path(nhs.dir, 'universality_hypotheses.csv'))
hypotheses$Target.OCM.codes <- gsub('[A-Z,]', '', hypotheses$Target.OCM.codes)
hypotheses$Target.OCM.codes <- gsub(' +', ' ', hypotheses$Target.OCM.codes)

## overall frequency of hypothesis ocms in psf
matched.codes <- data.frame(
  hypothesis.ocm = sort(unique(unlist(strsplit(hypotheses$Target.OCM.codes, ' '))))
)
matched.codes$hypothesis.subject <- ocm.codes$subject[
  match(matched.codes$hypothesis.ocm, ocm.codes$code)
]
matched.codes$hypothesis.freq <- unclass(ocm.freq[matched.codes$hypothesis.ocm])

## find closest control ocms in terms of overall frequency that
##   (1) are not in the same major group as any music ocm
##   (2) do not start with 1, e.g. sources/geography/research methods
##   (3) not previously matched
control.freq <- ocm.freq[
  substr(names(ocm.freq), 1, 1) != '1' &
  !substr(names(ocm.freq), 1, 2) %in% substr(matched.codes$hypothesis.ocm, 1, 2)
]
## list major ocm groups that are eligible for matching
names(ocm.codes.major[
  substr(ocm.codes.major, 1, 1) != '1' &
  !substr(ocm.codes.major, 1, 2) %in% substr(matched.codes$hypothesis.ocm, 1, 2)
])
## match hypothesis ocms to control ocms
for (i in 1:nrow(matched.codes)){
  matched.codes$control.ocm[i] <-
    names(which.min(abs(control.freq - matched.codes$hypothesis.freq[i])))
  control.freq <- control.freq[-match(matched.codes$control.ocm[i],
                                      names(control.freq)
                                      )
                               ]
}
matched.codes$control.subject <- ocm.codes$subject[
  match(matched.codes$control.ocm, ocm.codes$code)
]
matched.codes$control.freq <- unclass(ocm.freq[matched.codes$control.ocm])
## examine matches
write.csv(matched.codes[, c('hypothesis.subject',
                            'hypothesis.freq',
                            'control.subject',
                            'control.freq'
                            )
                        ],
          file.path(results.dir,
                    'psf_nhs_compare_minorocm_matches_final.csv'
                    )
          )

## sample from null distribution of ocm codes
ndraws <- 5000
set.seed(02139)
if (!file.exists(file.path(results.dir, 'nulldistr_ocms.rds'))){
  nulldistr.ocms.by.doc <- replicate(ndraws, sample.ocms(), simplify = FALSE)
  saveRDS(nulldistr.ocms.by.doc,
          file.path(results.dir, 'nulldistr_ocms.rds')
          )
} else {
  nulldistr.ocms.by.doc <- readRDS(file.path(results.dir, 'nulldistr_ocms.rds'))
}
nulldistr.ocms <- lapply(nulldistr.ocms.by.doc, unlist)

## sample from null distribution of words
hypothesis.words <- unlist(strsplit(hypotheses$Target.word.list, split = ', ?'))
hypothesis.words <- unique(char_wordstem(hypothesis.words))
set.seed(02139)
if (!file.exists(
  file.path(results.dir, 'nulldistr_words_poolcultures.rds')
  )){
  psf.hypothesis.dtm <-
    psf.dtm[, hypothesis.words[hypothesis.words %in% colnames(psf.dtm)]]
  nulldistr.words <- t(
    replicate(ndraws,
              colSums(sample.words(dtm = psf.hypothesis.dtm))
              )
  )
  saveRDS(nulldistr.words,
          file.path(results.dir, 'nulldistr_words_poolcultures.rds')
          )
} else {
  nulldistr.words <- readRDS(
    file.path(results.dir, 'nulldistr_words_poolcultures.rds')
  )
}



## nhs is an outlier in the number of labeled ocm codes, partly because we're
##   selecting based on a paragraph having the music code 533, which means every
##   paragraph automatically has at least one code. after removing this code,
##   song-related paragraphs still have slightly more than a similar sample from
##   psf (because songs have social content), but counts are very comparable
nhs.ocm.count <- length(unlist(nhs.ocms))
nhs.ocm.count
nulldistr.ocm.count <- sapply(nulldistr.ocms, length)
mean(nulldistr.ocm.count)
mean(nhs.ocm.count >= nulldistr.ocm.count)
nhs.ocm.count.nomusic <- sum(unlist(nhs.ocms) != '533')
nhs.ocm.count.nomusic
mean(nhs.ocm.count.nomusic >= nulldistr.ocm.count)



## summarize differences between nhs and psf in specified ocm codes
ocm.category.summary <- function(category, codes){

  ocm.codes.category <- ocm.codes[ocm.codes$code %in% codes,]

  nhs.ocms.category <- table(unlist(nhs.ocms))
  nhs.ocms.category <- nhs.ocms.category[
    as.character(ocm.codes.category$code)
  ]
  names(nhs.ocms.category) <- ocm.codes.category$subject
  nhs.ocms.category[is.na(nhs.ocms.category)] <- 0

  nulldistr.ocm.category <- sapply(
    nulldistr.ocms,
    function(draw.ocm){
      out <- table(draw.ocm)
      out <- out[as.character(ocm.codes.category$code)]
      names(out) <- ocm.codes.category$subject
      out[is.na(out)] <- 0
      return(out)
    })
  if (!is.matrix(nulldistr.ocm.category)){
    ## if only one code is supplied, sapply outputs vector
    nulldistr.ocm.category <- t(nulldistr.ocm.category)
  }

  out <- ldply(
    1:nrow(ocm.codes.category),
    function(i){
      out <- data.frame(
        ocm = ocm.codes.category$subject[i],
        type = 'null',
        est = mean(nulldistr.ocm.category[i,]),
        cilo = quantile(nulldistr.ocm.category[i,], .025),
        cihi = quantile(nulldistr.ocm.category[i,], .975),
        p = pval.1side.to.2side(
          mean(nulldistr.ocm.category[i,] >= nhs.ocms.category[i])
        )
      )
      out <- rbind.fill(
        out,
        data.frame(
          ocm = ocm.codes.category$subject[i],
          type = 'nhs',
          est = nhs.ocms.category[i]
        )
      )
    })

  nulldistr.ocm.any <- colSums(nulldistr.ocm.category)
  nhs.ocm.any <- sum(nhs.ocms.category)
  out <- rbind.fill(
    out,
    data.frame(
      ocm = 'ANY',
      type = 'null',
      est = mean(nulldistr.ocm.any),
      cilo = quantile(nulldistr.ocm.any, .025),
      cihi = quantile(nulldistr.ocm.any, .975),
      p = pval.1side.to.2side(
        mean(nulldistr.ocm.any >= nhs.ocm.any)
      )
    ),
    data.frame(
      ocm = 'ANY',
      type = 'nhs',
      est = nhs.ocm.any
    )
  )

  ## within a category, adjust for testing of multiple words
  ## don't adjust category totals yet
  ##   (this will be done after all categories are computed)
  out$p.adj <- p.adjust(ifelse(out$ocm == 'ANY', NA, out$p), 'BY')
  out$ocm.short <- substr(out$ocm, 1, 20)

  ocm.drop <- out$ocm[which(out$cihi == 0)]
  out <- out[
    !out$ocm %in% ocm.drop,
    ]
  out$category <- toupper(category)

  return(out)

}

## summarize differences between nhs and psf in specified groups and ocm codes
ocm.category.group.summary <- function(category, group, group.ind, codes){

  ocm.codes.category <- ocm.codes[ocm.codes$code %in% codes,]

  nhs.ocms.category <- table(unlist(nhs.ocms.by.doc[group.ind]))
  nhs.ocms.category <- nhs.ocms.category[
    as.character(ocm.codes.category$code)
  ]
  names(nhs.ocms.category) <- ocm.codes.category$subject
  nhs.ocms.category[is.na(nhs.ocms.category)] <- 0

  nulldistr.ocm.category <- sapply(
    nulldistr.ocms.by.doc,
    function(draw.ocm){
      out <- table(unlist(draw.ocm[group.ind]))
      out <- out[as.character(ocm.codes.category$code)]
      names(out) <- ocm.codes.category$subject
      out[is.na(out)] <- 0
      return(out)
    })
  if (!is.matrix(nulldistr.ocm.category)){
    ## if only one code is supplied, sapply outputs vector
    nulldistr.ocm.category <- t(nulldistr.ocm.category)
  }

  nulldistr.ocm.any <- colSums(nulldistr.ocm.category)
  nhs.ocm.any <- sum(nhs.ocms.category)
  out <- rbind.fill(
    data.frame(
      ocm = 'ANY',
      type = 'null',
      est = mean(nulldistr.ocm.any),
      cilo = quantile(nulldistr.ocm.any, .025),
      cihi = quantile(nulldistr.ocm.any, .975),
      p = pval.1side.to.2side(
        mean(nulldistr.ocm.any >= nhs.ocm.any)
      )
    ),
    data.frame(
      ocm = 'ANY',
      type = 'nhs',
      est = nhs.ocm.any
    )
  )
  out$category <- toupper(category)
  out$group <- group
  return(out)

}



## summarize differences between nhs and psf in specified word counts
words.category.summary <- function(category, words){

  out <- ldply(
    words,
    function(word){
      out <- data.frame(
        word = word,
        type = 'null',
        est = mean(nulldistr.words[,word]),
        cilo = quantile(nulldistr.words[,word], .025),
        cihi = quantile(nulldistr.words[,word], .975),
        p = pval.1side.to.2side(
          mean(nulldistr.words[,word] >= sum(nhs.dtm[,word]))
        )
      )
      out <- rbind.fill(
        out,
        data.frame(
          word = word,
          type = 'nhs',
          est = sum(nhs.dtm[,word])
        )
      )
    })
  out <- rbind.fill(
    out,
    data.frame(
      word = 'ANY',
      type = 'null',
      est = mean(rowSums(nulldistr.words[,words])),
      cilo = quantile(rowSums(nulldistr.words[,words]), .025),
      cihi = quantile(rowSums(nulldistr.words[,words]), .975),
      p = pval.1side.to.2side(
        mean(rowSums(nulldistr.words[,words]) >= sum(nhs.dtm[,words]))
      )
    ),
    data.frame(
      word = 'ANY',
      type = 'nhs',
      est = sum(nhs.dtm[,words])
    )
  )

}



## compare music paragraphs to null distr on ocms, pooling all cultures
ocm.summary <- ldply(
  1:nrow(hypotheses),
  function(i){
    codes <- strsplit(hypotheses$Target.OCM.codes[i], ' ')[[1]]
    codes.matched <-
      matched.codes$control.ocm[match(codes, matched.codes$hypothesis.ocm)]
    if (length(codes) > 0){
      return(
        rbind(
          cbind(
            ocm.category.summary(toupper(hypotheses$Hypothesis.short.name[i]),
                                 codes
                                 ),
            analysis = 'actual'
          ),
          cbind(
            ocm.category.summary(toupper(hypotheses$Hypothesis.short.name[i]),
                                 codes.matched
                                 ),
            analysis = 'control'
          )
        )
      )
    } else {
      return(NULL)
    }
  })

ocm.summary$label <- sprintf('%s:\n%s',
                             ocm.summary$category,
                             tolower(ocm.summary$ocm.short)
                             )
ocm.summary$label <- factor(
  ocm.summary$label,
  levels = unique(ocm.summary$label[
    order(ocm.summary$category,
          ocm.summary$p
          )
  ])
)

## report results pooling all minor ocms related to a hypothesis
ocm.summary.pooled <- ocm.summary[ocm.summary$ocm == 'ANY',]
ocm.summary.pooled$p.adj <- p.adjust(ocm.summary.pooled$p, 'BY')
ocm.summary.pooled <- ocm.summary[ocm.summary$ocm == 'ANY',]
ocm.summary.pooled$p.adj[ocm.summary.pooled$analysis == 'actual'] <-
  p.adjust(ocm.summary.pooled$p[ocm.summary.pooled$analysis == 'actual'], 'BY')
ocm.summary.pooled$p.adj[ocm.summary.pooled$analysis == 'control'] <-
  p.adjust(ocm.summary.pooled$p[ocm.summary.pooled$analysis == 'control'], 'BY')
null.ind <- which(ocm.summary.pooled$type == 'null')
ocm.summary.pooled <- data.frame(
  category = ocm.summary.pooled$category[null.ind],
  analysis = ocm.summary.pooled$analysis[null.ind],
  null.count = ocm.summary.pooled$est[null.ind],
  null.cilo = ocm.summary.pooled$cilo[null.ind],
  null.cihi = ocm.summary.pooled$cihi[null.ind],
  nhs.count = ocm.summary.pooled$est[-null.ind],
  p = ocm.summary.pooled$p[null.ind],
  p.adj = ocm.summary.pooled$p.adj[null.ind]
  )

## clean up pooled results for output
ocm.summary.pooled$ocm.codes[ocm.summary.pooled$analysis == 'actual'] <-
  hypotheses$Target.OCM.codes[
    match(tolower(ocm.summary.pooled$category[ocm.summary.pooled$analysis == 'actual']),
          hypotheses$Hypothesis.short.name
          )
  ]
ocm.summary.pooled$ocm.codes[ocm.summary.pooled$analysis == 'control'] <-
  sapply(ocm.summary.pooled$ocm.codes[ocm.summary.pooled$analysis == 'actual'],
         function(codes){
           codes <- strsplit(codes, ' ')[[1]]
           paste(
             matched.codes$control.ocm[match(codes, matched.codes$hypothesis.ocm)],
             collapse = ' '
             )
         })
ocm.summary.pooled$ocm.subjects <- sapply(
  ocm.summary.pooled$ocm.codes,
  function(x){
    paste(
      ocm.codes$subject[match(strsplit(x, ' ')[[1]], ocm.codes$code)],
      collapse = ';'
      )
  })
write.csv(ocm.summary.pooled,
          file.path(results.dir, 'psf_nhs_compare_minorocm_pooled_final.csv'),
          row.names = FALSE
          )



## compare music paras to null distr on ocms, examining each region separately
ocm.summary.regions <- ldply(
  1:nrow(hypotheses),
  function(i){
    codes <- strsplit(hypotheses$Target.OCM.codes[i], ' ')[[1]]
    if (length(codes) > 0){
      return(
        ldply(
          sort(unique(cultures$nhs_region[nhs.doc.to.culture])),
          function(region){
            cat(toupper(hypotheses$Hypothesis.short.name[i]), region, '\n')
            ocm.category.group.summary(
              toupper(hypotheses$Hypothesis.short.name[i]),
              region,
              which(cultures$nhs_region[nhs.doc.to.culture] == region),
              codes
            )
        })
      )
    } else {
      return(NULL)
    }
  })
nhs.doc.per.region <- table(cultures$nhs_region[nhs.doc.to.culture])
ocm.summary.regions$ndocs <- as.integer(nhs.doc.per.region[ocm.summary.regions$group])
write.csv(ocm.summary.regions,
          file.path(results.dir, 'psf_nhs_compare_minorocm_region_final.csv'),
          row.names = FALSE
          )
ocm.summary.regions$p[ocm.summary.regions$type == 'nhs'] <-
  ocm.summary.regions$p[ocm.summary.regions$type == 'null']
ggplot(ocm.summary.regions,
       aes(x = ndocs,
           y = est,
           ymin = cilo,
           ymax = cihi,
           color = p < .05
           )) +
  geom_errorbar() +
  geom_point(data = ocm.summary.regions[ocm.summary.regions$type == 'nhs',]) +
  facet_wrap('category', scales = 'free') +
  xlab('number of docs about a region') +
  ylab('ocm code count within docs about a region') +
  theme_light(base_size = 20) +
  ggtitle('nhs vs psf on total ocm codes, within hypothesis and hraf region\n(each bar is null distribution for a region, each point is actual nhs realization)')


#dkhere
## compare music paras to null distr on ocms, examining each region separately
ocm.summary.hraf.regions <- ldply(
  1:nrow(hypotheses),
  function(i){
    codes <- strsplit(hypotheses$Target.OCM.codes[i], ' ')[[1]]
    if (length(codes) > 0){
      return(
        ldply(
          sort(unique(cultures$hraf_region[nhs.doc.to.culture])),#dkhere
          function(region){
            cat(toupper(hypotheses$Hypothesis.short.name[i]), region, '\n')
            ocm.category.group.summary(
              toupper(hypotheses$Hypothesis.short.name[i]),
              region,
              which(cultures$hraf_region[nhs.doc.to.culture] == region),
              codes
            )
        })
      )
    } else {
      return(NULL)
    }
  })
nhs.doc.per.hraf.region <- table(cultures$hraf_region[nhs.doc.to.culture])
ocm.summary.hraf.regions$ndocs <- as.integer(nhs.doc.per.hraf.region[ocm.summary.hraf.regions$group])
write.csv(ocm.summary.hraf.regions,
          file.path(results.dir, 'psf_nhs_compare_minorocm_hrafregion_final.csv'),
          row.names = FALSE
          )
ocm.summary.hraf.regions$p[ocm.summary.hraf.regions$type == 'nhs'] <-
  ocm.summary.hraf.regions$p[ocm.summary.hraf.regions$type == 'null']
ggplot(ocm.summary.hraf.regions,
       aes(x = ndocs,
           y = est,
           ymin = cilo,
           ymax = cihi,
           color = p < .05
           )) +
  geom_errorbar() +
  geom_point(data = ocm.summary.hraf.regions[ocm.summary.hraf.regions$type == 'nhs',]) +
  facet_wrap('category', scales = 'free') +
  xlab('number of docs about a region') +
  ylab('ocm code count within docs about a region') +
  theme_light(base_size = 20) +
  ggtitle('nhs vs psf on total ocm codes, within hypothesis and hraf region\n(each bar is null distribution for a region, each point is actual nhs realization)')


## compare music paras to null distr on ocms, examining each glotto family separately
ocm.summary.glottos <- ldply(
  1:nrow(hypotheses),
  function(i){
    codes <- strsplit(hypotheses$Target.OCM.codes[i], ' ')[[1]]
    if (length(codes) > 0){
      return(
        ldply(
          sort(unique(cultures$glotto_family[nhs.doc.to.culture])),
          function(glotto){
            cat(toupper(hypotheses$Hypothesis.short.name[i]), glotto, '\n')
            ocm.category.group.summary(
              toupper(hypotheses$Hypothesis.short.name[i]),
              glotto,
              which(cultures$glotto_family[nhs.doc.to.culture] == glotto),
              codes
            )
        })
      )
    } else {
      return(NULL)
    }
  })
nhs.doc.per.glotto <- table(cultures$glotto_family[nhs.doc.to.culture])
ocm.summary.glottos$ndocs <- as.integer(nhs.doc.per.glotto[ocm.summary.glottos$group])
write.csv(ocm.summary.glottos,
          file.path(results.dir, 'psf_nhs_compare_minorocm_glotto_final.csv'),
          row.names = FALSE
          )
ocm.summary.glottos$p[ocm.summary.glottos$type == 'nhs'] <-
  ocm.summary.glottos$p[ocm.summary.glottos$type == 'null']
ggplot(ocm.summary.glottos,
       aes(x = ndocs,
           y = est,
           ymin = cilo,
           ymax = cihi,
           color = p < .05
           )) +
  geom_errorbar() +
  geom_point(data = ocm.summary.glottos[ocm.summary.glottos$type == 'nhs',]) +
  facet_wrap('category', scales = 'free') +
  xlab('number of docs about a glotto family') +
  ylab('ocm code count within docs about a glotto family') +
  theme_light(base_size = 20) +
  ggtitle('nhs vs psf on total ocm codes, within hypothesis and glotto family\n(each bar is null distribution for a glotto family, each point is actual nhs realization)')

## compare music paragraphs to null distr on words
words.summary <- ldply(
  1:nrow(hypotheses),
  function(i){
    words <- strsplit(hypotheses$Target.word.list[[i]], split = ', ')[[1]]
    words <- unique(char_wordstem(words))
    words <- words[words %in% colnames(psf.dtm)]
    if (length(words) > 0){
      return(data.frame(
          category = toupper(hypotheses$Hypothesis.short.name[i]),
          words.category.summary(toupper(hypotheses$Hypothesis.short.name[i]),
                               words
                               )
      ))
      } else {
        return(data.frame(
          category = toupper(hypotheses$Hypothesis.short.name[i])
        ))
    }
  })

## report results pooling all words related to a hypothesis
words.summary.pooled <- words.summary[words.summary$word == 'ANY',]
words.summary.pooled$p.adj <- p.adjust(words.summary.pooled$p, 'BY')
null.ind <- which(words.summary.pooled$type == 'null')
words.summary.pooled <- data.frame(
  category = words.summary.pooled$category[null.ind],
  null.count = words.summary.pooled$est[null.ind],
  null.cilo = words.summary.pooled$cilo[null.ind],
  null.cihi = words.summary.pooled$cihi[null.ind],
  nhs.count = words.summary.pooled$est[-null.ind],
  p = words.summary.pooled$p[null.ind],
  p.adj = words.summary.pooled$p.adj[null.ind]
  )
write.csv(words.summary.pooled,
          file.path(results.dir, 'psf_nhs_compare_words_pooled_final.csv'),
          row.names = FALSE
          )
