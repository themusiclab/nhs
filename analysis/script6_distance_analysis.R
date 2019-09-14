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

dat <- read.csv("ethno_bpca_scores_20181228_csv.csv", stringsAsFactors = FALSE)

dat <- dat[dat$song.function != "other",]

dat$score.1 <- scale(dat$score.1)
dat$score.2 <- scale(dat$score.2)
dat$score.3 <- scale(dat$score.3)

# for base dat
get.function.cluster.matches(dat)

ethno.preds.dat <- get.function.cluster.matches(dat, return.dat = TRUE)
confusionMatrix(as.factor(ethno.preds.dat$song.function), as.factor(ethno.preds.dat$pred.function))
    
n.perms <- 10000

null.results <- list()

set.seed(63130)
for(i in 1:n.perms){
    if(i %% 1000 == 0){
        print(i)
    }
    perm.dist <- data.frame(dat)

    # resample functions
    perm.dist$song.function <- sample(perm.dist$song.function, nrow(perm.dist), replace = FALSE)
    null.results[[length(null.results) + 1]] <- get.function.cluster.matches(perm.dist)
}
null.results <- as.data.frame(do.call("rbind", null.results))

alt.results <- get.function.cluster.matches(dat)

png("dance.png")
plot(density(null.results$dance), xlim = c(0,1))
abline(v = alt.results[names(alt.results) == "dance"], col = "red")
dev.off()
sum(null.results$dance > alt.results[names(alt.results) == "dance"]) / nrow(null.results)

png("healing.png")
plot(density(null.results$healing), xlim = c(0,1))
abline(v = alt.results[names(alt.results) == "healing"], col = "red")
dev.off()
sum(null.results$healing > alt.results[names(alt.results) == "healing"]) / nrow(null.results)

png("love.png")
plot(density(null.results$love), xlim = c(0,1))
abline(v = alt.results[names(alt.results) == "love"], col = "red")
dev.off()
sum(null.results$love > alt.results[names(alt.results) == "love"]) / nrow(null.results)

png("lullaby.png")
plot(density(null.results$lullaby), xlim = c(0,1))
abline(v = alt.results[names(alt.results) == "lullaby"], col = "red")
dev.off()
sum(null.results$lullaby > alt.results[names(alt.results) == "lullaby"]) / nrow(null.results)

print(sum(null.results$overall > alt.results[names(alt.results) == "overall"]) / nrow(null.results))

## Unsupervised clustering
scores.mat <- as.matrix(dat[,c("score.1", "score.2", "score.3")])

set.seed(63130)
kmeans.out <- kmeans(scores.mat, 4)

table(kmeans.out$cluster, dat$song.function)

#############
# untrimmed #
#############

dat.robust <- read.csv("ethno_bpca_scores_20181228_robust.csv", stringsAsFactors = FALSE)

dat.robust <- dat.robust[dat.robust$song.function != "other",]

dat.robust$score.1 <- scale(dat.robust$score.1)
dat.robust$score.2 <- scale(dat.robust$score.2)
dat.robust$score.3 <- scale(dat.robust$score.3)

# for base dat
get.function.cluster.matches(dat.robust)

ethno.preds.dat.robust <- get.function.cluster.matches(dat.robust, return.dat = TRUE)
confusionMatrix(as.factor(ethno.preds.dat.robust$song.function), as.factor(ethno.preds.dat.robust$pred.function))
    
n.perms <- 10000

null.results <- list()

set.seed(63130)
for(i in 1:n.perms){
    if(i %% 1000 == 0){
        print(i)
    }
    perm.dist <- data.frame(dat.robust)

    # resample functions
    perm.dist$song.function <- sample(perm.dist$song.function, nrow(perm.dist), replace = FALSE)
    null.results[[length(null.results) + 1]] <- get.function.cluster.matches(perm.dist)
}
null.results <- as.data.frame(do.call("rbind", null.results))

alt.results <- get.function.cluster.matches(dat.robust)

png("dance.png")
plot(density(null.results$dance), xlim = c(0,1))
abline(v = alt.results[names(alt.results) == "dance"], col = "red")
dev.off()
sum(null.results$dance > alt.results[names(alt.results) == "dance"]) / nrow(null.results)

png("healing.png")
plot(density(null.results$healing), xlim = c(0,1))
abline(v = alt.results[names(alt.results) == "healing"], col = "red")
dev.off()
sum(null.results$healing > alt.results[names(alt.results) == "healing"]) / nrow(null.results)

png("love.png")
plot(density(null.results$love), xlim = c(0,1))
abline(v = alt.results[names(alt.results) == "love"], col = "red")
dev.off()
sum(null.results$love > alt.results[names(alt.results) == "love"]) / nrow(null.results)

png("lullaby.png")
plot(density(null.results$lullaby), xlim = c(0,1))
abline(v = alt.results[names(alt.results) == "lullaby"], col = "red")
dev.off()
sum(null.results$lullaby > alt.results[names(alt.results) == "lullaby"]) / nrow(null.results)

print(sum(null.results$overall > alt.results[names(alt.results) == "overall"]) / nrow(null.results))

## Unsupervised clustering
scores.mat <- as.matrix(dat.robust[,c("score.1", "score.2", "score.3")])

set.seed(63130)
kmeans.out <- kmeans(scores.mat, 4)

table(kmeans.out$cluster, dat.robust$song.function)

#########
# disco #
#########

disco.dat <- read.csv("disco_bpca_scores_20181229.csv")
disco.dat$bpca.1 <- scale(disco.dat$bpca.1)
disco.dat$bpca.2 <- scale(disco.dat$bpca.2)

disco.mat <- disco.dat[,c("bpca.1", "bpca.2")]

get.function.cluster.matches(disco.dat,
                             dim.cols = c("bpca.1", "bpca.2"),
                             label.col = "type")

color = rep(NA, length=length(nrow(disco.dat)))
color[which(disco.dat$type=="Lullaby")] = "red"
color[which(disco.dat$type=="Healing")] = "green"
color[which(disco.dat$type=="Love")] = "blue"
color[which(disco.dat$type=="Dance")] = "yellow"

png("centroid_plot.png")
plot(disco.dat$bpca.1, disco.dat$bpca.2, col = color, pch = 16)
points(c(-0.1789042, 0.2079665, -0.3362085, 0.3210106),
       c(0.57773462, -0.07716147, -0.06866793, -0.43704931),
       col = c("yellow", "green", "blue", "red"), pch = 19, cex = 2)
dev.off()

disco.alt.dat.w.pred <- get.function.cluster.matches(disco.dat,
                                                     dim.cols = c("bpca.1", "bpca.2"),
                                                     label.col = "type",
                                                     return.dat = TRUE)
confusionMatrix(disco.alt.dat.w.pred$type, disco.alt.dat.w.pred$pred.function)

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

disco.alt.results <- get.function.cluster.matches(disco.dat,
                             dim.cols = c("bpca.1", "bpca.2"),
                             label.col = "type")

print(sum(disco.null.results$Dance > disco.alt.results[names(disco.alt.results) == "Dance"]) / nrow(disco.null.results))

print(sum(disco.null.results$Healing > disco.alt.results[names(disco.alt.results) == "Healing"]) / nrow(disco.null.results))

print(sum(disco.null.results$Love > disco.alt.results[names(disco.alt.results) == "Love"]) / nrow(disco.null.results))

print(sum(disco.null.results$Lullaby > disco.alt.results[names(disco.alt.results) == "Lullaby"]) / nrow(disco.null.results))

print(sum(disco.null.results$overall > disco.alt.results[names(disco.alt.results) == "overall"]) / nrow(disco.null.results))

p.adjust(c(sum(disco.null.results$Dance > disco.alt.results[names(disco.alt.results) == "Dance"]) / nrow(disco.null.results),
         sum(disco.null.results$Healing > disco.alt.results[names(disco.alt.results) == "Healing"]) / nrow(disco.null.results),
         sum(disco.null.results$Love > disco.alt.results[names(disco.alt.results) == "Love"]) / nrow(disco.null.results),
         sum(disco.null.results$Lullaby > disco.alt.results[names(disco.alt.results) == "Lullaby"]) / nrow(disco.null.results)),
         method = "BY")

#################
# Disco k means #
#################

disco.kmeans.out <- kmeans(disco.mat, 4)

table(disco.kmeans.out$cluster, disco.dat$type)

pvals <- list()
for(song.type in unique(disco.dat$type)){
    for(dim in c("bpca.1", "bpca.2")){
        print(paste0(song.type, ": ", dim))
        ttest.out <- t.test(disco.dat[[dim]][disco.dat$type == song.type],
                            disco.dat[[dim]][disco.dat$type != song.type])
        pvals[[length(pvals) + 1]] <- ttest.out$p.value
        names(pvals)[[length(pvals)]] <- paste0(song.type, ": ", dim)
    }
}

adjusted.pvals <- p.adjust(pvals, method = "BY")

for(v in 1:length(adjusted.pvals)){
    print(adjusted.pvals[v])
    cat("\n")
}

