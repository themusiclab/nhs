# load necessary packages for importing the function
library(RCurl)
options(scipen=999)
 
# cluster SEs
url.robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url.robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)

dat <- read.csv("../data/nhs/TML-FC_NHS_sci.csv")
dat$correct <- as.integer(dat$type == dat$response)
dat$id <- as.factor(dat$id)
dat$musictrad <- as.factor(dat$musictrad)
dat$musicskill <- as.factor(dat$musicskill)

# Overall
overall.confusion <- table(Actual = dat$type,
                           Predicted = dat$response)
write.csv(overall.confusion, "../results/online_listeners_overall_confusion_final.csv")

# musictrad
dat.musictrad1 <- subset(dat, dat$musictrad == 1)
musictrad1.confusion <- table(Actual = dat.musictrad1$type,
                              Predicted = dat.musictrad1$response)
write.csv(musictrad1.confusion, "../results/online_listeners_musictrad1_confusion_final.csv")

dat.musictrad2 <- subset(dat, dat$musictrad == 2)
musictrad2.confusion <- table(Actual = dat.musictrad2$type,
                              Predicted = dat.musictrad2$response)
write.csv(musictrad2.confusion, "../results/online_listeners_musictrad2_confusion_final.csv")

dat.musictrad3 <- subset(dat, dat$musictrad == 3)
musictrad3.confusion <- table(Actual = dat.musictrad3$type,
                              Predicted = dat.musictrad3$response)
write.csv(musictrad3.confusion, "../results/online_listeners_musictrad3_confusion_final.csv")

dat.musictrad4 <- subset(dat, dat$musictrad == 4)
musictrad4.confusion <- table(Actual = dat.musictrad4$type,
                              Predicted = dat.musictrad4$response)
write.csv(musictrad4.confusion, "../results/online_listeners_musictrad4_confusion_final.csv")

# musicskill
dat.musicskill1 <- subset(dat, dat$musicskill == 1)
musicskill1.confusion <- table(Actual = dat.musicskill1$type,
                              Predicted = dat.musicskill1$response)
write.csv(musicskill1.confusion, "../results/online_listeners_musicskill1_confusion_final.csv")

dat.musicskill2 <- subset(dat, dat$musicskill == 2)
musicskill2.confusion <- table(Actual = dat.musicskill2$type,
                              Predicted = dat.musicskill2$response)
write.csv(musicskill2.confusion, "../results/online_listeners_musicskill2_confusion_final.csv")

dat.musicskill3 <- subset(dat, dat$musicskill == 3)
musicskill3.confusion <- table(Actual = dat.musicskill3$type,
                              Predicted = dat.musicskill3$response)
write.csv(musicskill3.confusion, "../results/online_listeners_musicskill3_confusion_final.csv")

dat.musicskill4 <- subset(dat, dat$musicskill == 4)
musicskill4.confusion <- table(Actual = dat.musicskill4$type,
                              Predicted = dat.musicskill4$response)
write.csv(musicskill4.confusion, "../results/online_listeners_musicskill4_confusion_final.csv")

dat.musicskill5 <- subset(dat, dat$musicskill == 5)
musicskill5.confusion <- table(Actual = dat.musicskill5$type,
                              Predicted = dat.musicskill5$response)
write.csv(musicskill5.confusion, "../results/online_listeners_musicskill5_confusion_final.csv")

# write out accuracy for each matrix
overall.acc <- sum(diag(overall.confusion)) / sum(overall.confusion)

musictrad1.acc <- sum(diag(musictrad1.confusion)) / sum(musictrad1.confusion)
musictrad2.acc <- sum(diag(musictrad2.confusion)) / sum(musictrad2.confusion)
musictrad3.acc <- sum(diag(musictrad3.confusion)) / sum(musictrad3.confusion)
musictrad4.acc <- sum(diag(musictrad4.confusion)) / sum(musictrad4.confusion)

musicskill1.acc <- sum(diag(musicskill1.confusion)) / sum(musicskill1.confusion)
musicskill2.acc <- sum(diag(musicskill2.confusion)) / sum(musicskill2.confusion)
musicskill3.acc <- sum(diag(musicskill3.confusion)) / sum(musicskill3.confusion)
musicskill4.acc <- sum(diag(musicskill4.confusion)) / sum(musicskill4.confusion)
musicskill5.acc <- sum(diag(musicskill5.confusion)) / sum(musicskill5.confusion)

acc.table <- data.frame(overall.acc = overall.acc,
                        musictrad1.acc = musictrad1.acc,
                        musictrad2.acc = musictrad2.acc,
                        musictrad3.acc = musictrad3.acc,
                        musictrad4.acc = musictrad4.acc,
                        musicskill1.acc = musicskill1.acc,
                        musicskill2.acc = musicskill2.acc,
                        musicskill3.acc = musicskill3.acc,
                        musicskill4.acc = musicskill4.acc,
                        musicskill5.acc = musicskill5.acc)

write.csv(acc.table, "../results/online_listeners_accuracy_final.csv")



mod.trad <- lm(correct ~ musictrad, data = dat)
mod.trad.glm <- glm(correct ~ musictrad, family = "binomial", data = dat)
sink("../results/musictrad_accuracy_mod_results.txt")
print(summary(mod.trad, cluster=c("id")))
sink()

sink("../results/musicskill_accuracy_mod_results.txt")
mod.skill <- lm(correct ~ musicskill, data = dat)
summary(mod.skill, cluster=c("id"))
sink()
