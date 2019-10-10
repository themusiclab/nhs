library("caret")

get.function.cluster.matches <- function(dat,
                                         dim.cols = c("score.1", "score.2", "score.3"),
                                         label.col = "song.function",
                                         return.dat = FALSE){

    ## Get centroids
    centroids <- aggregate(dat[,dim.cols],
                           by = list(dat[[label.col]]),
                           mean)
   
    centroids.mat <- as.matrix(centroids[,dim.cols])
    centroids.labels <- centroids[,"Group.1"]
    
    ## Assign all songs to nearest group centroids
    scores.mat <- as.matrix(dat[,dim.cols])

    dat$pred.function <- unlist(lapply(seq_len(nrow(scores.mat)),
                                       function(i) centroids.labels[which.min(sqrt(colSums((scores.mat[i, ] -
                                                                                            t(centroids.mat))^2)))]))

    dat$accurate.pred <- as.integer(dat[[label.col]] == dat$pred.function)
    accuracy.by.group <- tapply(dat$accurate.pred, dat[[label.col]], mean)
    overall.accuracy <- mean(dat$accurate.pred)
    names(overall.accuracy) <- "overall"
    if(return.dat){
        return(dat)
    }
    else{
        return(c(accuracy.by.group, overall.accuracy))
    }
}

dat.overall <- read.csv("../results/ethno_bpca_scores_final.csv", stringsAsFactors = FALSE)

dat.overall <- dat.overall[dat.overall$song.function != "other",]

dat.overall$score.1 <- scale(dat.overall$score.1)
dat.overall$score.2 <- scale(dat.overall$score.2)
dat.overall$score.3 <- scale(dat.overall$score.3)

overall.results.ethno <- data.frame(accuracy = get.function.cluster.matches(dat.overall))

ethno.preds.dat <- get.function.cluster.matches(dat.overall, return.dat = TRUE)
    
n.perms <- 10000

overall.null.results <- list()

set.seed(63130)
for(i in 1:n.perms){
    if(i %% 1000 == 0){
        print(i)
    }
    perm.dist <- data.frame(dat.overall)

    # resample functions
    perm.dist$song.function <- sample(perm.dist$song.function, nrow(perm.dist), replace = FALSE)
    overall.null.results[[length(overall.null.results) + 1]] <- get.function.cluster.matches(perm.dist)
}
overall.null.results <- as.data.frame(do.call("rbind", overall.null.results))

overall.results.ethno$pval <- rep(NA, nrow(overall.results.ethno))

overall.results.ethno$pval[rownames(overall.results.ethno) == "dance"] <-
    sum(overall.null.results$dance > overall.results.ethno["dance", "accuracy"]) / nrow(overall.null.results)
overall.results.ethno$pval[rownames(overall.results.ethno) == "healing"] <-
    sum(overall.null.results$healing > overall.results.ethno["healing", "accuracy"]) / nrow(overall.null.results)
overall.results.ethno$pval[rownames(overall.results.ethno) == "lullaby"] <-
    sum(overall.null.results$lullaby > overall.results.ethno["lullaby", "accuracy"]) / nrow(overall.null.results)
overall.results.ethno$pval[rownames(overall.results.ethno) == "love"] <-
    sum(overall.null.results$love > overall.results.ethno["love", "accuracy"]) / nrow(overall.null.results)
overall.results.ethno$pval[rownames(overall.results.ethno) == "overall"] <-
    sum(overall.null.results$overall > overall.results.ethno["overall", "accuracy"]) / nrow(overall.null.results)

overall.results.ethno$pval <- p.adjust(overall.results.ethno$pval, method = "BY")
overall.conf.mat.ethno <- confusionMatrix(as.factor(ethno.preds.dat$song.function), as.factor(ethno.preds.dat$pred.function))$table

write.csv(overall.results.ethno, "../results/ethno_centroid_acc.csv")
write.csv(overall.conf.mat.ethno, "../results/ethno_centroid_confusion.csv")

#############
# untrimmed #
#############
rm(list = ls()[!ls() %in% "get.function.cluster.matches"])

dat.robust <- read.csv("../results/ethno_bpca_scores_robust_final.csv", stringsAsFactors = FALSE)

dat.robust <- dat.robust[dat.robust$song.function != "other",]

dat.robust$score.1 <- scale(dat.robust$score.1)
dat.robust$score.2 <- scale(dat.robust$score.2)
dat.robust$score.3 <- scale(dat.robust$score.3)

robust.results.ethno <- data.frame(accuracy = get.function.cluster.matches(dat.robust))

ethno.preds.dat.robust <- get.function.cluster.matches(dat.robust, return.dat = TRUE)
    
n.perms <- 10000

robust.null.results <- list()

set.seed(63130)
for(i in 1:n.perms){
    if(i %% 1000 == 0){
        print(i)
    }
    perm.dist <- data.frame(dat.robust)

    # resample functions
    perm.dist$song.function <- sample(perm.dist$song.function, nrow(perm.dist), replace = FALSE)
    robust.null.results[[length(robust.null.results) + 1]] <- get.function.cluster.matches(perm.dist)
}
robust.null.results <- as.data.frame(do.call("rbind", robust.null.results))

robust.results.ethno$pval <- rep(NA, nrow(robust.results.ethno))

robust.results.ethno$pval[rownames(robust.results.ethno) == "dance"] <-
    sum(robust.null.results$dance > robust.results.ethno["dance", "accuracy"]) / nrow(robust.null.results)
robust.results.ethno$pval[rownames(robust.results.ethno) == "healing"] <-
    sum(robust.null.results$healing > robust.results.ethno["healing", "accuracy"]) / nrow(robust.null.results)
robust.results.ethno$pval[rownames(robust.results.ethno) == "lullaby"] <-
    sum(robust.null.results$lullaby > robust.results.ethno["lullaby", "accuracy"]) / nrow(robust.null.results)
robust.results.ethno$pval[rownames(robust.results.ethno) == "love"] <-
    sum(robust.null.results$love > robust.results.ethno["love", "accuracy"]) / nrow(robust.null.results)
robust.results.ethno$pval[rownames(robust.results.ethno) == "overall"] <-
    sum(robust.null.results$overall > robust.results.ethno["overall", "accuracy"]) / nrow(robust.null.results)

robust.results.ethno$pval <- p.adjust(robust.results.ethno$pval, method = "BY")
robust.conf.mat.ethno <- confusionMatrix(as.factor(ethno.preds.dat.robust$song.function), as.factor(ethno.preds.dat.robust$pred.function))$table

write.csv(robust.results.ethno, "../results/robust_ethno_centroid_acc.csv")
write.csv(robust.conf.mat.ethno, "../results/robust_ethno_centroid_confusion.csv")

#########
# disco #
#########
rm(list = ls()[!ls() %in% "get.function.cluster.matches"])

disco.dat <- read.csv("../results/disco_bpca_scores.csv")

disco.dat$bpca.1 <- scale(disco.dat$bpca.1)
disco.dat$bpca.2 <- scale(disco.dat$bpca.2)

overall.results.disco <- data.frame(accuracy = get.function.cluster.matches(disco.dat,
                                                                            dim.cols = c("bpca.1", "bpca.2"),
                                                                            label.col = "type"))

disco.preds.dat <- get.function.cluster.matches(disco.dat,
                                                dim.cols = c("bpca.1", "bpca.2"),
                                                label.col = "type",
                                                return.dat = TRUE)

n.perms <- 10000

disco.null.results <- list()

set.seed(63130)

for(i in 1:n.perms){
    if(i %% 1000 == 0){
        print(i)
    }
    disco.perm.dist <- data.frame(disco.dat)

    # resample functions
    disco.perm.dist$type <- sample(disco.perm.dist$type, nrow(disco.perm.dist), replace = FALSE)
    disco.null.results[[length(disco.null.results) + 1]] <- get.function.cluster.matches(disco.perm.dist,
                             dim.cols = c("bpca.1", "bpca.2"),
                             label.col = "type")
}

disco.null.results <- as.data.frame(do.call("rbind", disco.null.results))

overall.results.disco$pval <- rep(NA, nrow(overall.results.disco))

overall.results.disco$pval[rownames(overall.results.disco) == "Dance"] <-
    sum(disco.null.results$Dance > overall.results.disco["Dance", "accuracy"]) / nrow(disco.null.results)
overall.results.disco$pval[rownames(overall.results.disco) == "Healing"] <-
    sum(disco.null.results$Healing > overall.results.disco["Healing", "accuracy"]) / nrow(disco.null.results)
overall.results.disco$pval[rownames(overall.results.disco) == "Lullaby"] <-
    sum(disco.null.results$Lullaby > overall.results.disco["Lullaby", "accuracy"]) / nrow(disco.null.results)
overall.results.disco$pval[rownames(overall.results.disco) == "Love"] <-
    sum(disco.null.results$Love > overall.results.disco["Love", "accuracy"]) / nrow(disco.null.results)
overall.results.disco$pval[rownames(overall.results.disco) == "overall"] <-
    sum(disco.null.results$overall > overall.results.disco["overall", "accuracy"]) / nrow(disco.null.results)

overall.results.disco$pval <- p.adjust(overall.results.disco$pval, method = "BY")
overall.conf.mat.disco <- confusionMatrix(as.factor(disco.preds.dat$type), as.factor(disco.preds.dat$pred.function))$table

write.csv(overall.results.disco, "../results/disco_centroid_acc.csv")
write.csv(overall.conf.mat.disco, "../results/disco_centroid_confusion.csv")
