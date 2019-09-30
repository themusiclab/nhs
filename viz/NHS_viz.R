###########################
## NHS viz  ###############
## Samuel Mehr 04sep2019 ##
###########################

# if working in cloned repo, set your working directory to /nhs/viz/vizData

## config
library(ggplot2)
library(plotly)
library(tidyr)
library(maps)
library(ggridges)
library(forcats)
library(yarrr)

## read in all datasets
ethnogMap <- read.csv("ethnogMap.csv")
ethnogBPCA <- read.csv("ethnogBPCA.csv")
disco_mlasso <- read.csv("catLASSO.csv")
disco_llasso <- read.csv("../../results/disco_pairs_acc.csv")
discogBPCA <- read.csv("discogBPCA.csv")
tonalityHistogram <- read.csv("tonalityHistogram.csv")
tonalityScatter <- read.csv("tonalityScatter.csv")
discogMap <- read.csv("discogMap.csv")
zipfMelo <- read.csv("../../results/disco_ngram_relpitch_2gram_final.csv")
zipfRhyt <- read.csv("../../results/disco_ngram_relrhythm_2gram_final.csv")
figS1data <- read.csv("figS1.csv")
figS2data <- read.csv("figS2.csv")
figS3data <- read.csv("figS3.csv")
figS4data <- read.csv("figS4.csv")
figS5data <- read.csv("figS5.csv")
figS6data <- read.csv("figS6.csv")
figS7data <- read.csv("figS7.csv")
figS8data <- read.csv("figS8.csv")
figS9data <- read.csv("figS9.csv")

## Figs 1 & 4 ########################################################################################################## 

# ethnography map
cult.x <- ethnogMap$longitud
cult.y <- ethnogMap$latitude
cult.mp <- NULL
mapWorld <- borders("world", colour="gray80", fill="gray80") # create a layer of borders
mp <- ggplot() + mapWorld
(
cult.mp <- mp +
  geom_point(aes(x = cult.x, y = cult.y),
             pch = 21,
             stroke = .5,
             colour = "black",
             fill = "#5f577d",
             alpha = 0.9,
             size = 2) +
  theme(legend.position = 0,
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
)
ggsave("../pdf/NHS_fig1B.pdf", width = 7, height = 4)

# discography map
track.x <- discogMap$longitude
track.y <- discogMap$latitude
track.mp <- NULL
library(yarrr)
(
track.mp <- mp +
  geom_point(data = discogMap, aes(x = track.x, y = track.y, fill = factor(discogMap$type)),
             shape = 21,
             alpha = .9,
             stroke = .5,
             color = "black",
             size = 2) +
  scale_fill_manual(values = palette(piratepal(palette = "google"))) +
  theme(legend.position = 'n',
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=5)))
  )
ggsave("../pdf/NHS_fig4B.pdf", width = 7, height = 4)

## Fig 2 ##########################################################################################################
# Dance RGB ... 109 152 243 or #6d98f3
# Healing RGB ... 233	100	101	or #e96465
# Love RGB ... 249 198 89	or #f9c659 
# Lullaby RGB ... 106	184	120	#6ab878
titlefont <- list(family = "Arial, sans-serif",
                  size = 16,
                  color = "black")
tickfont <- list(family = "Arial, sans-serif",
                 size = 12,
                 color = "black")
formal <- ethnogBPCA$score1
excite <- ethnogBPCA$score2
narrat <- ethnogBPCA$score3

## centroids & faded 3D plot
(
fig2_centroids <- plot_ly() %>%
    add_trace(x = formal[which(ethnogBPCA$songfunction == "other")],
              y = excite[which(ethnogBPCA$songfunction == "other")],
              z = narrat[which(ethnogBPCA$songfunction == "other")],
              type = "scatter3d",
              mode = "markers",
              hoverinfo = 'none',
              marker = list(symbol = "circle",
                            color = "#7f7f7f",
                            opacity = .05,
                            size = 4)) %>%
  add_trace(x = formal[which(ethnogBPCA$songfunction == "dance")],
            y = excite[which(ethnogBPCA$songfunction == "dance")],
            z = narrat[which(ethnogBPCA$songfunction == "dance")], 
            type = "scatter3d", 
            mode = "markers",
            hoverinfo = 'none',
            marker = list(symbol = "circle", 
                          color = "#6d98f3",
                          opacity = .1,
                          size = 4)) %>%
  add_trace(x = formal[which(ethnogBPCA$songfunction == "healing")],
            y = excite[which(ethnogBPCA$songfunction == "healing")],
            z = narrat[which(ethnogBPCA$songfunction == "healing")], 
            type = "scatter3d", 
            mode = "markers",
            hoverinfo = 'none',
            marker = list(symbol = "circle", 
                          color = "#e96465",
                          opacity = .1,
                          size = 4)) %>%
  add_trace(x = formal[which(ethnogBPCA$songfunction == "love")],
            y = excite[which(ethnogBPCA$songfunction == "love")],
            z = narrat[which(ethnogBPCA$songfunction == "love")], 
            type = "scatter3d", 
            mode = "markers",
            hoverinfo = 'none',
            marker = list(symbol = "circle", 
                          color = "#f9c659",
                          opacity = .1,
                          size = 4)) %>%
  add_trace(x = formal[which(ethnogBPCA$songfunction == "lullaby")],
            y = excite[which(ethnogBPCA$songfunction == "lullaby")],
            z = narrat[which(ethnogBPCA$songfunction == "lullaby")], 
            type = "scatter3d", 
            mode = "markers",
            hoverinfo = 'none',
            marker = list(symbol = "circle", 
                          color = "#6ab878",
                          opacity = .1,
                          size = 4)) %>%
  add_trace(x = .276, ## dance centroid
            y = .513,
            z = -.213, 
            type = "scatter3d", 
            mode = "markers",
            hoverinfo = 'none',
            marker = list(symbol = "diamond", 
                          color = "#6d98f3",
                          line = list(color = "#000000", width = 2.5),
                          opacity = 1,
                          size = 6)) %>%
  add_trace(x = .122, ## healing centroid
            y = -.320,
            z = .662, 
            type = "scatter3d", 
            mode = "markers",
            hoverinfo = 'none',
            marker = list(symbol = "diamond", 
                          color = "#e96465",
                          line = list(color = "#000000", width = 2.5),
                          opacity = 1,
                          size = 6)) %>%
  add_trace(x = -.534, ## love centroid
            y = -.283,
            z = -.319, 
            type = "scatter3d", 
            mode = "markers",
            hoverinfo = 'none',
            marker = list(symbol = "diamond", 
                          color = "#f9c659",
                          line = list(color = "#000000", width = 2.5),
                          opacity = 1,
                          size = 6)) %>%
  add_trace(x = -.695, ## lullaby centroid
            y = -.132,
            z = .128, 
            type = "scatter3d", 
            mode = "markers",
            hoverinfo = 'none',
            marker = list(symbol = "diamond", 
                          color = "#6ab878",
                          line = list(color = "#000000", width = 2.5),
                          opacity = 1,
                          size = 6)) %>%
  add_trace(x = .276, ## dance highlight
                        y = -.513,
                        z = .213,
                        type = "scatter3d",
                        mode = "markers",
                        hoverinfo = 'none',
                        marker = list(#symbol = "diamond",
                                      color = "#6d98f3",
                                      line = list(color = "#000000", width = 1),
                                      opacity = 1,
                                      size = 6)) %>%
              add_trace(x = .122, ## healing highlight
                        y = .320,
                        z = -.662,
                        type = "scatter3d",
                        mode = "markers",
                        hoverinfo = 'none',
                        marker = list(#symbol = "diamond",
                                      color = "#e96465",
                                      line = list(color = "#000000", width = 1),
                                      opacity = 1,
                                      size = 6)) %>%
              add_trace(x = -.534, ## love highlight
                        y = .283,
                        z = .319,
                        type = "scatter3d",
                        mode = "markers",
                        hoverinfo = 'none',
                        marker = list(#symbol = "diamond",
                                      color = "#f9c659",
                                      line = list(color = "#000000", width = 1),
                                      opacity = 1,
                                      size = 6)) %>%
              add_trace(x = -.695, ## lullaby highlight
                        y = .132,
                        z = -.128,
                        type = "scatter3d",
                        mode = "markers",
                        hoverinfo = 'none',
                        marker = list(#symbol = "diamond",
                                      color = "#6ab878",
                                      line = list(color = "#000000", width = 1),
                                      opacity = 1,
                                      size = 6)) %>%
  layout(showlegend = FALSE,
         scene = list(xaxis = list(showspikes = FALSE, titlefont = titlefont, tickfont = tickfont, title = '', gridcolor = "black"),
                      yaxis = list(showspikes = FALSE, titlefont = titlefont, tickfont = tickfont, title = '', gridcolor = "black"),
                      zaxis = list(showspikes = FALSE, titlefont = titlefont, tickfont = tickfont, title = '', gridcolor = "black"),
                      camera = list(eye = list(x = 1.2, y = -2, z = .5)),
                      aspectratio = list(x = 1, y = 1, z = 1))) %>%
  config(displayModeBar = FALSE)
)

# histograms
(
fig2b <- ggplot(subset(ethnogBPCA, songfunction %in% c("lullaby","dance","healing","love"))) +
  geom_density(aes(x = score1, 
                   fill = factor(songfunction)),
               alpha = 0.8) +
  scale_fill_manual(values = c("#6d98f3","#e96465","#f9c659","#6ab878")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 10,
                                 color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
)
ggsave("../pdf/NHS_fig2B.pdf", width = 4, height = 4)

(
fig2c <- ggplot(subset(ethnogBPCA, songfunction %in% c("lullaby","dance","healing","love"))) +
  geom_density(aes(x = score2, 
                   fill = factor(songfunction)),
               alpha = 0.8) +
  scale_fill_manual(values = c("#6d98f3","#e96465","#f9c659","#6ab878")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 10,
                                 color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
)
ggsave("../pdf/NHS_fig2C.pdf", width = 4, height = 4)

(
fig2d <- ggplot(subset(ethnogBPCA, songfunction %in% c("lullaby","dance","healing","love"))) +
  geom_density(aes(x = score3, 
                   fill = factor(songfunction)),
               alpha = 0.8) +
  scale_fill_manual(values = c("#6d98f3","#e96465","#f9c659","#6ab878")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 10,
                                 color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
)
ggsave("../pdf/NHS_fig2D.pdf", width = 4, height = 4)

## Fig 3 ##########################################################################################################
## 3 ridgeline plots

cult_pcasort <- fct_reorder(ethnogBPCA$culture, ethnogBPCA$ncites, .desc = FALSE)
(
fig3a <- ggplot(ethnogBPCA, aes(x = score1, y = cult_pcasort, height = ..density.., fill = meanest1)) +
    geom_density_ridges(stat = "density", scale = 5.5, bw = .15, alpha = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = .7) +
    scale_fill_gradient2(low = "red", 
                         mid = "#aec5eb",
                         high = "red",
                         limits = c(-2,2)) +
    theme_ridges(grid = FALSE) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_discrete(expand = c(0.04, 0))
)
ggsave("../pdf/NHS_fig3A.pdf", width = 5, height = 10)

(
fig3b <- ggplot(ethnogBPCA, aes(x = score2, y = cult_pcasort, height = ..density.., fill = meanest2)) +
    geom_density_ridges(stat = "density", scale = 5.5, bw = .15, alpha = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 1) +
    scale_fill_gradient2(low = "red", 
                         mid = "#aec5eb",
                         high = "red",
                         limits = c(-2,2)) +
    theme_ridges(grid = FALSE) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_discrete(expand = c(0.04, 0))
)
ggsave("../pdf/NHS_fig3B.pdf", width = 5, height = 10)

(
fig3c <- ggplot(ethnogBPCA, aes(x = score3, y = cult_pcasort, height = ..density.., fill = meanest3)) +
    geom_density_ridges(stat = "density", scale = 5.5, bw = .15, alpha = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 1) +
    scale_fill_gradient2(low = "red", 
                         mid = "#aec5eb",
                         high = "red",
                         limits = c(-2,2)) +
    theme_ridges(grid = FALSE) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_discrete(expand = c(0.04, 0))
  ) 
ggsave("../pdf/NHS_fig3C.pdf", width = 5, height = 10)

## Fig 5 ###########################################################################################################

## categorical lasso
fig5b <- disco_mlasso
fig5b %>%
  mutate(feature.set = fct_relevel(featureset, "mir.panteli", "naive", "expert", "transcription", "nocontext")) %>%
  ggplot(aes(x = feature.set, y = accuracy)) +
    geom_hline(yintercept = .25, color = "red", size = 1.5, linetype = "dashed") +
    geom_hline(yintercept = .424, color = "blue", size = 1.5, linetype = "dashed") +
    geom_bar(stat = "identity", alpha = .8, colour = "black", fill = "#44A08D") +
    geom_errorbar(aes(ymin = cilo, ymax = cihi), width = .1) +
    theme_bw() +
    theme(text = element_text(size = 14,
                              color = "black"),
          axis.text = element_text(size = 10,
                                   color = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank()) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,1.05),
                       breaks = c(.10,.20,.3,.4,.5,.6,.7,.8,.9,1),
                       labels = c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%"))
ggsave("../pdf/NHS_fig5B.pdf", width = 4, height = 4)

## logistic lasso
(
fig5c <- ggplot(data = disco_llasso, aes(x = reorder(comparison, accuracy), y = accuracy)) +
          geom_hline(yintercept = .50, color = "red", size = 1.5, linetype = "dashed") +
          geom_bar(stat = "identity", alpha = .8, colour = "black", fill = "#4568DC") +
          geom_errorbar(aes(ymin = cilo, ymax = cihi), width = .1) +
          theme_bw() +
          theme(text = element_text(size = 14,
                                    color = "black"),
                axis.text = element_text(size = 10,
                                         color = "black"),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                axis.text.x = element_blank()) +
          scale_y_continuous(expand = c(0,0),
                             limits = c(0,1.05),
                             breaks = c(.10,.20,.3,.4,.5,.6,.7,.8,.9,1),
                             labels = c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%"))
)
ggsave("../pdf/NHS_fig5C.pdf", width = 4, height = 4)

## Fig 6 ###########################################################################################################

# big histogram
histcolors <- c("#ff9075","#0040da","#5400b8","#00d252","#831bd1","#4ab200","#b618d7","#5de059",
                "#cb5aff","#b0ce00","#6369ff","#00d333","#3e9a00","#ff40de","#00e08b","#f665ff",
                "#009f3a","#f900a3","#008a2c","#c000a6","#a8d54a","#012ea9","#cfb400","#410075",
                "#acac00","#f086ff","#1e7c00","#ff72df","#698d00","#9d8bff","#f78f00","#005fc2",
                "#ff0021","#00dbbd","#f8007c","#006215","#b70082","#79da9c","#550064","#dac744",
                "#1a1968","#c39d00","#002a7c","#d19300","#4d9eff","#e13400","#01a8f5","#ff3437",
                "#01b7d8","#ff5e35","#01acdc","#c50023","#64d8d2","#bc003d","#018d5f","#b30058",
                "#019f8f","#ad2c00","#6eccff","#ff832e","#8494ff","#7e8800","#d598ff","#153e00",
                "#ff9af6","#586200","#bca8ff","#9c6a00","#0163ae","#ffb159","#00488f","#b2d17f",
                "#7c0065","#bacd93","#2c1a55","#ffaa78","#003e73","#943a00","#8aaeff","#766100",
                "#e9b2ff","#1a2b08","#ff8bd5","#005e3e","#ff6196","#00b2bd","#950026","#8ed0e7",
                "#600000","#a1c2ff","#7f4600","#008bbb","#a00049","#017167","#8b0058","#d6c68f",
                "#12244c","#f4bb86","#321c43","#adb28d","#68003a","#003820","#ff7f96","#004a66",
                "#ffaea9","#43123f","#704b00","#f4b2e2","#3a2001","#ffa2c1","#411a17","#c9a7c5",
                "#5d2500","#77003a","#9c6d6c","#56001b","#664053","#421726")

(
  ggplot(tonalityHistogram,
         aes(x = newpc)
  ) +
    geom_histogram(aes(fill = factor(song)
    ),
    position = position_dodge(width = .9),
    alpha = 1
    ) +
    expand_limits(y = 35) +
    scale_fill_manual(values = histcolors) +
    theme_bw() +
    ylab('Number of ratings') +
    xlab('Absolute distance from modal rating (in semitones)') +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6)) +
    scale_y_continuous(breaks = c(0,5,10,15,20,25,30)) +
    theme(axis.text = element_text(size = 10,
                                   color = "black"),
          legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()
    )        
)
ggsave("../pdf/NHS_fig6A.pdf", width = 6, height = 3)

# krumhansl vs modal key
(
  fig6b <- ggplot(tonalityScatter) +
    geom_jitter(aes(y = key1,
                    x = modalpc),
                alpha = .5) +
    theme_bw() +
    ylab('First-rank prediction from \n Krumhansl-Schmukler algorithm') +
    xlab('Modal rating from expert listeners') +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11),
                       labels = c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")
    ) +
    scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11),
                       labels = c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")
                           ) +
    theme(axis.text = element_text(size = 10,
                                   color = "black"),
          legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()
    ) 
)
ggsave("../pdf/NHS_fig6B.pdf", width = 4, height = 4)

## Fig 7 ###########################################################################################################

# scatter
(
fig7a <- ggplot(data = discogBPCA, 
       aes(x = bpca1, y = bpca2, fill = type)) +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             color = "black") +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "black") +
  geom_point(shape = 21, ## main plot
             size = 2,
             alpha = .8) +
  geom_point(data = subset(discogBPCA, song %in% c(79, 91), fill = type),
             shape = 23,
             fill = "#6d98f3",
             size = 3) +
  geom_point(data = subset(discogBPCA, song %in% c(37, 71), fill = type),
             shape = 23,
             fill = "#6ab878",
             size = 3) +
  scale_fill_manual(values = c("#6d98f3","#e96465","#f9c659","#6ab878")) +
  theme_bw() +
  xlab("PC1: Melodic Complexity") +
  ylab("PC2: Rhythmic Complexity") +
  theme(axis.text = element_text(size = 10,
                                 color = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
)
ggsave("../pdf/NHS_fig7A.pdf", width = 5, height = 5)

# pca1 histogram
(
fig7b <- ggplot(data = discogBPCA) +
  geom_density(aes(x = bpca1, 
                   fill = type),
               alpha = 0.7) +
  scale_fill_manual(values = c("#6d98f3","#e96465","#f9c659","#6ab878")) +
  theme_bw() +
  theme(axis.text = element_text(size = 10,
                                 color = "black"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
)
ggsave("../pdf/NHS_fig7B.pdf", width = 4, height = 4)

# pca2 histogram
(
fig6c <- ggplot(data = discogBPCA) +
  geom_density(aes(x = bpca2, 
                   fill = type),
               alpha = 0.7) +
  scale_fill_manual(values = c("#6d98f3","#e96465","#f9c659","#6ab878")) +
  theme_bw() +
  theme(axis.text = element_text(size = 10,
                                 color = "black"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
)
ggsave("../pdf/NHS_fig7C.pdf", width = 4, height = 4)

## Fig 8 ##########################################################################################################
(
fig8a = ggplot(zipfMelo, aes(x = rank)) +
  geom_point(aes(y = proportion),
             alpha = .5,
             pch = 16,
             color = "blue",
             size = 2) +
  geom_line(aes(y = proportion.zm),
            size = .8) +
  scale_x_log10(breaks = c(1:24),
                labels = c("1","2","3","4","5","6","7","8","9","10","","","","","15","","","","","20","","","","24")) +
  scale_y_log10(breaks = c(.001,.01,.1), 
                labels = c(".001",".01",".1")) +
  theme_bw() +
  xlab("Rank") +
  ylab("Proportion") +
  theme(axis.text = element_text(size = 10,
                            color = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
)
ggsave("../pdf/NHS_fig8A.pdf", width = 4, height = 4)

(
  fig8b = ggplot(zipfRhyt, aes(x = rank)) +
    geom_point(aes(y = proportion),
               alpha = .5,
               pch = 16,
               color = "blue",
               size = 2) +
    geom_line(aes(y = proportion.zm),
              size = .8) +
    scale_x_log10(breaks = c(1:10,20,30,40,50,60,70,80,90,100),
                  labels = c("1","2","3","4","5","6","","8","","10","20","30","40","","","70","","","100")) +
    scale_y_log10(breaks = c(.00001,.0001,.001,.01,.1), 
                  labels = c(".00001",".0001",".001",".01",".1")) +
    theme_bw() +
    xlab("Rank") +
    ylab("Proportion") +
    theme(axis.text = element_text(size = 10,
                                   color = "black"),
          legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
)
ggsave("../pdf/NHS_fig8B.pdf", width = 4.17, height = 4)

## Fig S1 #########################################################################################################
## 3 ridgeline plots

cult_pcasort <- fct_reorder(figS1data$culture, figS1data$ncites, .desc = FALSE)
(
  figS1a <- ggplot(figS1data, aes(x = score1, y = cult_pcasort, height = ..density.., fill = meanest1)) +
    geom_density_ridges(stat = "density", scale = 5.5, bw = .15, alpha = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray", alpha = 1) +
    scale_fill_gradient2(low = "red", 
                         mid = "#03396c",
                         high = "red",
                         limits = c(-4,4)) +
    theme_ridges(grid = FALSE) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_discrete(expand = c(0.04, 0))
)
ggsave("../pdf/NHS_figS1A.pdf", width = 5, height = 10)

(
  figS1b <- ggplot(figS1data, aes(x = score2, y = cult_pcasort, height = ..density.., fill = meanest2)) +
    geom_density_ridges(stat = "density", scale = 5.5, bw = .15, alpha = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray", alpha = 1) +
    scale_fill_gradient2(low = "red", 
                         mid = "#03396c",
                         high = "red",
                         limits = c(-4,4)) +
    theme_ridges(grid = FALSE) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_discrete(expand = c(0.04, 0))
)
ggsave("../pdf/NHS_figS1B.pdf", width = 5, height = 10)

(
  figS1c <- ggplot(figS1data, aes(x = score3, y = cult_pcasort, height = ..density.., fill = meanest3)) +
    geom_density_ridges(stat = "density", scale = 5.5, bw = .15, alpha = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray", alpha = 1) +
    scale_fill_gradient2(low = "red", 
                         mid = "#03396c",
                         high = "red",
                         limits = c(-4,4)) +
    theme_ridges(grid = FALSE) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_discrete(expand = c(0.04, 0))
) 
ggsave("../pdf/NHS_figS1C.pdf", width = 5, height = 10)

## Fig S2 #########################################################################################################

line1 <- function(x) {1.96*x}
line2 <- function(x) {-1.96*x}

# PC1
(
  figs2a <- ggplot(figS2data, aes(x = sdest1, y = meanest1, color = ncites)) +
    geom_point(alpha = .8) +
    geom_errorbar(aes(ymin = meancilo1, ymax = meancihi1), alpha = .5) +
    geom_errorbarh(aes(xmin = sdcilo1, xmax = sdcihi1), alpha = .5) +
    geom_abline(intercept = 0, slope = 1.96, linetype = "dotdash") +
    geom_abline(intercept = 0, slope = -1.96, linetype = "dotdash") +
    stat_function(fun = line1, geom = "area", alpha = .2) +
    stat_function(fun = line2, geom = "area", alpha = .2) +
    scale_color_gradientn(colors = c("red", "#56B1F7","#132B43"), values = c(0, .05, .1, 1)) +
    theme_bw() +
    labs(color = "# of docs") +
    theme(axis.text = element_text(colour = "black", size = 10),
          axis.title.x = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("within-society standard deviation") +
    ylab("society deviation from global mean") +
    scale_x_continuous(expand = c(0,0), limits = c(0,1.5)) +
    scale_y_continuous(expand = c(0,0))
)
ggsave("../pdf/NHS_figS2A.pdf", width = 10, height = 5)

# PC2
(
  figs2b <- ggplot(figS2data, aes(x = sdest2, y = meanest2, color = ncites)) +
    geom_point(alpha = .8) +
    geom_errorbar(aes(ymin = meancilo2, ymax = meancihi2), alpha = .5) +
    geom_errorbarh(aes(xmin = sdcilo2, xmax = sdcihi2), alpha = .5) +
    geom_abline(intercept = 0, slope = 1.96, linetype = "dotdash") +
    geom_abline(intercept = 0, slope = -1.96, linetype = "dotdash") +
    stat_function(fun = line1, geom = "area", alpha = .2) +
    stat_function(fun = line2, geom = "area", alpha = .2) +
    scale_color_gradientn(colors = c("red", "#56B1F7","#132B43"), values = c(0, .05, .1, 1)) +
    theme_bw() +
    labs(color = "# of docs") +
    theme(axis.text = element_text(colour = "black", size = 10),
          axis.title.x = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("within-society standard deviation") +
    ylab("society deviation from global mean") +
    scale_x_continuous(expand = c(0,0), limits = c(0,1.5)) +
    scale_y_continuous(expand = c(0,0))
)
ggsave("../pdf/NHS_figS2B.pdf", width = 10, height = 5)

# PC3
(
  figs2c <- ggplot(figS2data, aes(x = sdest3, y = meanest3, color = ncites)) +
    geom_point(alpha = .8) +
    geom_errorbar(aes(ymin = meancilo3, ymax = meancihi3), alpha = .5) +
    geom_errorbarh(aes(xmin = sdcilo3, xmax = sdcihi3), alpha = .5) +
    geom_abline(intercept = 0, slope = 1.96, linetype = "dotdash") +
    geom_abline(intercept = 0, slope = -1.96, linetype = "dotdash") +
    stat_function(fun = line1, geom = "area", alpha = .2) +
    stat_function(fun = line2, geom = "area", alpha = .2) +
    scale_color_gradientn(colors = c("red", "#56B1F7","#132B43"), values = c(0, .05, .1, 1)) +
    theme_bw() +
    labs(color = "# of docs") +
    theme(axis.text = element_text(colour = "black", size = 10),
          axis.title.x = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("within-society standard deviation") +
    ylab("society deviation from global mean") +
    scale_x_continuous(expand = c(0,0), limits = c(0,1.5)) +
    scale_y_continuous(expand = c(0,0))
)
ggsave("../pdf/NHS_figS2C.pdf", width = 10, height = 5)

## Fig S3 ##########################################################################################################

line1 <- function(x) {1.96*x}
line2 <- function(x) {-1.96*x}

# PC1
(
figs3a <- ggplot(figS3data, aes(x = sdest1, y = meanest1, color = ncites)) +
    geom_point(alpha = .8) +
    geom_errorbar(aes(ymin = meancilo1, ymax = meancihi1), alpha = .5) +
    geom_errorbarh(aes(xmin = sdcilo1, xmax = sdcihi1), alpha = .5) +
    geom_abline(intercept = 0, slope = 1.96, linetype = "dotdash") +
    geom_abline(intercept = 0, slope = -1.96, linetype = "dotdash") +
    stat_function(fun = line1, geom = "area", alpha = .2) +
    stat_function(fun = line2, geom = "area", alpha = .2) +
    scale_color_gradientn(colors = c("red", "#56B1F7","#132B43"), values = c(0, .05, .1, 1)) +
    theme_bw() +
    labs(color = "# of docs") +
    theme(axis.text = element_text(colour = "black", size = 10),
          axis.title.x = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("within-society standard deviation") +
    ylab("society deviation from global mean") +
    scale_x_continuous(expand = c(0,0), limits = c(0,1.5)) +
    scale_y_continuous(expand = c(0,0))
)
ggsave("../pdf/NHS_figS3A.pdf", width = 10, height = 5)

# PC2
(
  figs3b <- ggplot(figS3data, aes(x = sdest2, y = meanest2, color = ncites)) +
    geom_point(alpha = .8) +
    geom_errorbar(aes(ymin = meancilo2, ymax = meancihi2), alpha = .5) +
    geom_errorbarh(aes(xmin = sdcilo2, xmax = sdcihi2), alpha = .5) +
    geom_abline(intercept = 0, slope = 1.96, linetype = "dotdash") +
    geom_abline(intercept = 0, slope = -1.96, linetype = "dotdash") +
    stat_function(fun = line1, geom = "area", alpha = .2) +
    stat_function(fun = line2, geom = "area", alpha = .2) +
    scale_color_gradientn(colors = c("red", "#56B1F7","#132B43"), values = c(0, .05, .1, 1)) +
    theme_bw() +
    labs(color = "# of docs") +
    theme(axis.text = element_text(colour = "black", size = 10),
          axis.title.x = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("within-society standard deviation") +
    ylab("society deviation from global mean") +
    scale_x_continuous(expand = c(0,0), limits = c(0,1.5)) +
    scale_y_continuous(expand = c(0,0))
)
ggsave("../pdf/NHS_figS3B.pdf", width = 10, height = 5)

# PC3
(
  figs3c <- ggplot(figS3data, aes(x = sdest3, y = meanest3, color = ncites)) +
    geom_point(alpha = .8) +
    geom_errorbar(aes(ymin = meancilo3, ymax = meancihi3), alpha = .5) +
    geom_errorbarh(aes(xmin = sdcilo3, xmax = sdcihi3), alpha = .5) +
    geom_abline(intercept = 0, slope = 1.96, linetype = "dotdash") +
    geom_abline(intercept = 0, slope = -1.96, linetype = "dotdash") +
    stat_function(fun = line1, geom = "area", alpha = .2) +
    stat_function(fun = line2, geom = "area", alpha = .2) +
    scale_color_gradientn(colors = c("red", "#56B1F7","#132B43"), values = c(0, .05, .1, 1)) +
    theme_bw() +
    labs(color = "# of docs") +
    theme(axis.text = element_text(colour = "black", size = 10),
          axis.title.x = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("within-society standard deviation") +
    ylab("society deviation from global mean") +
    scale_x_continuous(expand = c(0,0), limits = c(0,1.5)) +
    scale_y_continuous(expand = c(0,0))
)
ggsave("../pdf/NHS_figS3C.pdf", width = 10, height = 5)

## Fig S4 ########################################################################################

figS4data$newdim = factor(figS4data$dim, levels = c("Formality","Arousal","Religiosity"))
(
  figs4 <- ggplot(figS4data,
                  aes(x = coef,
                      y = est,
                      ymin = cilo,
                      ymax = cihi)) +
    geom_hline(yintercept = 0, color = 'gray', linetype = 'dashed') +
    geom_point() +
    geom_errorbar(width = .5) +
    facet_wrap('newdim') + 
    ylab('expected change in Bayesian principal component') +
    theme_bw() +
    xlab("") +
    ylab("Expected change in principal component") +
    theme(legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text = element_text(angle = 45, hjust = 1))
)
ggsave("../pdf/NHS_figS4.pdf", width = 10, height = 7)

## Fig S5 ########################################################################################

# reorder dims & cultures
figS5data$newdim = factor(figS5data$dim, 
                          levels = c("Formality","Arousal","Religiosity"))
#figS2data$culture = with(figS2data, reorder(culture))

(
figs5 <- ggplot(figS5data,
                aes(x = culture,
                    ymin = distrlo,
                    ymax = distrhi)) +
                geom_errorbar(width = 0,
                              alpha = .8) +
                geom_point(aes(y = otherculturemean),
                           pch = 0,
                           alpha = .6,
                           size = 1) +
                geom_point(aes(y = otherhrafregionmean),
                           pch = 1,
                           alpha = .6, 
                           size = 1) +
                geom_point(aes(y = otherhrafsubregionmean),
                           pch = 2,
                           alpha = .6, 
                           size = 1) +
                geom_point(aes(y = otherglottomean),
                           pch = 3,
                           alpha = .6, 
                           size = 1) +
                geom_point(aes(y = otherworldmean),
                           pch = 4,
                           alpha = .6, 
                           size = 1) +
                geom_point(aes(y = otherhrafsubsistencemean),
                           pch = 5,
                           alpha = .6, 
                           size = 1) +
    coord_flip() +
    facet_wrap('newdim', ncol = 3) +
    scale_x_discrete(limits = rev(levels(figS5data$culture))) +
    theme_bw() +
    xlab("") +
    ylab("") +
    theme(legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  )
ggsave("../pdf/NHS_figS5.pdf", width = 8, height = 8)

## Fig S6 #########################################################################################################
(
  figS6a <- ggplot(figS6data) +
    geom_point(aes(y = docmeanest1, x = docnum), alpha = .6, size = 0.75) + ## doc means
    geom_errorbar(aes(ymin = docmeancilo1, ymax = docmeancihi1, x = docnum), width = 0, alpha = .25) + ## doc CIs
    geom_point(aes(y = cultmeanest1, x = docnum), pch = 23, size = 3, fill = "red", alpha = .9) + ## culture means
    geom_errorbar(aes(ymin = cultmeancilo1, ymax = cultmeancihi1, x = docnum), color = "red", width = 0, alpha = .75) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw() +
    theme(axis.text = element_text(colour = "black", size = 14),
          axis.title.x = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("number of documents per society") +
    ylab("deviation from global mean") +
    scale_x_continuous(breaks = c(1,4,8,20,44,88,106,141,157,193,243,265,301,353,367,382,414,431,449,468),
                       labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","26")) +
    scale_y_continuous(limits = c(-4,4))
)
ggsave("../pdf/NHS_figS6A.pdf", width = 15, height = 5)

(
  figs6b <- ggplot(figS6data) +
    geom_point(aes(y = docmeanest2, x = docnum), alpha = .6, size = 0.75) + ## doc means
    geom_errorbar(aes(ymin = docmeancilo2, ymax = docmeancihi2, x = docnum), width = 0, alpha = .25) + ## doc CIs
    geom_point(aes(y = cultmeanest2, x = docnum), pch = 23, size = 3, fill = "red", alpha = .9) + ## culture means
    geom_errorbar(aes(ymin = cultmeancilo2, ymax = cultmeancihi2, x = docnum), color = "red", width = 0, alpha = .75) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw() +
    theme(axis.text = element_text(colour = "black", size = 14),
          axis.title.x = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("number of documents per society") +
    ylab("deviation from global mean") +
    scale_x_continuous(breaks = c(1,4,8,20,44,88,106,141,157,193,243,265,301,353,367,382,414,431,449,468),
                       labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","26")) +
    scale_y_continuous(limits = c(-4,4))
)
ggsave("../pdf/NHS_figS6B.pdf", width = 15, height = 5)

(
  figs6c <- ggplot(figS6data) +
    geom_point(aes(y = docmeanest3, x = docnum), alpha = .6, size = 0.75) + ## doc means
    geom_errorbar(aes(ymin = docmeancilo3, ymax = docmeancihi3, x = docnum), width = 0, alpha = .25) + ## doc CIs
    geom_point(aes(y = cultmeanest3, x = docnum), pch = 23, size = 3, fill = "red", alpha = .9) + ## culture means
    geom_errorbar(aes(ymin = cultmeancilo3, ymax = cultmeancihi3, x = docnum), color = "red", width = 0, alpha = .75) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw() +
    theme(axis.text = element_text(colour = "black", size = 14),
          axis.title.x = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("number of documents per society") +
    ylab("deviation from global mean") +
    scale_x_continuous(breaks = c(1,4,8,20,44,88,106,141,157,193,243,265,301,353,367,382,414,431,449,468),
                       labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","26")) +
    scale_y_continuous(limits = c(-4,4))
)
ggsave("../pdf/NHS_figS6C.pdf", width = 15, height = 5)

## Fig S7 #########################################################################################################
## 3 ridgeline plots

wet_pcasort <- fct_reorder(figS7data$ctryname, figS7data$nstations, .desc = FALSE)
(
  ridges1 <- ggplot(figS7data, aes(x = score1, y = wet_pcasort, height = ..density.., fill = meanest1)) +
    geom_density_ridges(stat = "density", scale = 5.5, bw = .15, alpha = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray", alpha = 1) +
    scale_fill_gradient2(low = "red", 
                         mid = "#03396c",
                         high = "red",
                         limits = c(-4,4)) +
    theme_ridges(grid = FALSE) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_discrete(expand = c(0.04, 0))
)
ggsave("../pdf/NHS_figS7A.pdf", width = 6.8, height = 10)

(
  ridges2 <- ggplot(figS7data, aes(x = score2, y = wet_pcasort, height = ..density.., fill = meanest2)) +
    geom_density_ridges(stat = "density", scale = 5.5, bw = .15, alpha = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray", alpha = 1) +
    scale_fill_gradient2(low = "red", 
                         mid = "#03396c",
                         high = "red",
                         limits = c(-4,4)) +
    theme_ridges(grid = FALSE) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_discrete(expand = c(0.04, 0))
)
ggsave("../pdf/NHS_figS7B.pdf", width = 6.8, height = 10)

(
  ridges3 <- ggplot(figS7data, aes(x = score3, y = wet_pcasort, height = ..density.., fill = meanest3)) +
    geom_density_ridges(stat = "density", scale = 5.5, bw = .15, alpha = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray", alpha = 1) +
    scale_fill_gradient2(low = "red", 
                         mid = "#03396c",
                         high = "red",
                         limits = c(-4,4)) +
    theme_ridges(grid = FALSE) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_discrete(expand = c(0.04, 0))
) 
ggsave("../pdf/NHS_figS7C.pdf", width = 6.8, height = 10)

## Fig S8 ########################################################################################################

line1 <- function(x) {1.96*x}
line2 <- function(x) {-1.96*x}

# PC1
(
  figs8a <- ggplot(figS8data, aes(x = sdest1, y = meanest1, color = nstations)) +
    geom_point(alpha = .8) +
    geom_errorbar(aes(ymin = meancilo1, ymax = meancihi1), alpha = .5) +
    geom_errorbarh(aes(xmin = sdcilo1, xmax = sdcihi1), alpha = .5) +
    geom_abline(intercept = 0, slope = 1.96, linetype = "dotdash") +
    geom_abline(intercept = 0, slope = -1.96, linetype = "dotdash") +
    stat_function(fun = line1, geom = "area", alpha = .2) +
    stat_function(fun = line2, geom = "area", alpha = .2) +
    scale_color_gradientn(colors = c("red", "#56B1F7","#132B43"), values = c(0, .05, .1, 1)) +
    theme_bw() +
    labs(color = "# of docs") +
    theme(axis.text = element_text(colour = "black", size = 10),
          axis.title.x = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("within-country standard deviation") +
    ylab("country deviation from global mean") +
    scale_x_continuous(expand = c(0,0), limits = c(0,1.5)) +
    scale_y_continuous(expand = c(0,0))
)
ggsave("../pdf/NHS_figS8A.pdf", width = 10, height = 5)

# PC2
(
  figs8b <- ggplot(figS8data, aes(x = sdest2, y = meanest2, color = nstations)) +
    geom_point(alpha = .8) +
    geom_errorbar(aes(ymin = meancilo2, ymax = meancihi2), alpha = .5) +
    geom_errorbarh(aes(xmin = sdcilo2, xmax = sdcihi2), alpha = .5) +
    geom_abline(intercept = 0, slope = 1.96, linetype = "dotdash") +
    geom_abline(intercept = 0, slope = -1.96, linetype = "dotdash") +
    stat_function(fun = line1, geom = "area", alpha = .2) +
    stat_function(fun = line2, geom = "area", alpha = .2) +
    scale_color_gradientn(colors = c("red", "#56B1F7","#132B43"), values = c(0, .05, .1, 1)) +
    theme_bw() +
    labs(color = "# of docs") +
    theme(axis.text = element_text(colour = "black", size = 10),
          axis.title.x = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("within-country standard deviation") +
    ylab("country deviation from global mean") +
    scale_x_continuous(expand = c(0,0), limits = c(0,1.5)) +
    scale_y_continuous(expand = c(0,0))
)
ggsave("../pdf/NHS_figS8B.pdf", width = 10, height = 5)

# PC3
(
  figs8c <- ggplot(figS8data, aes(x = sdest3, y = meanest3, color = nstations)) +
    geom_point(alpha = .8) +
    geom_errorbar(aes(ymin = meancilo3, ymax = meancihi3), alpha = .5) +
    geom_errorbarh(aes(xmin = sdcilo3, xmax = sdcihi3), alpha = .5) +
    geom_abline(intercept = 0, slope = 1.96, linetype = "dotdash") +
    geom_abline(intercept = 0, slope = -1.96, linetype = "dotdash") +
    stat_function(fun = line1, geom = "area", alpha = .2) +
    stat_function(fun = line2, geom = "area", alpha = .2) +
    scale_color_gradientn(colors = c("red", "#56B1F7","#132B43"), values = c(0, .05, .1, 1)) +
    theme_bw() +
    labs(color = "# of docs") +
    theme(axis.text = element_text(colour = "black", size = 10),
          axis.title.x = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("within-country standard deviation") +
    ylab("country deviation from global mean") +
    scale_x_continuous(expand = c(0,0), limits = c(0,1.5)) +
    scale_y_continuous(expand = c(0,0))
)
ggsave("../pdf/NHS_figS8C.pdf", width = 10, height = 5)

## Fig S9 ########################################################################################

(
  figS9 <- ggplot(figS9data,
                   aes(x = ndocs,
                       y = est,
                       ymin = cilo,
                       ymax = cihi)) +
    geom_errorbar(alpha = .5) +
    geom_point(data = figS9data[figS9data$type == 'nhs', ],
               aes(color = p < .05),
               alpha = .8) +
    facet_wrap('category', scales = 'free') +
    xlab('Number of documents from an HRAF region') +
    ylab('Number of instances of the OCM identifier(s) from an HRAF region') +
    scale_color_manual(values = c("red","blue"),
                       labels = c("not significant","significant"),
                       name = "") +
    theme(axis.text = element_text(colour = "black", size = 6),
          axis.title.x = element_text(size = 12, color = "black"),
          panel.background = element_blank(),
          panel.border = element_rect(color = "black",
                                      fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")
)
ggsave("../pdf/NHS_figS9.pdf", width = 7, height = 6)
