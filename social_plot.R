library(ggplot2)
library(ggrepel)
library(dplyr)

#input data
social_scores <- read.csv("C:/Users/Hoyt/Desktop/RaceWritingComputation/CodeData/SOCIAL_SCORES.csv")

#extra only Bible alignments
social_scores <- social_scores %>% filter(sample_group == "KJV_align")

#set index column
social_scores$idu <- as.numeric(row.names(social_scores))

windowsFonts(Times=windowsFont("TT Times New Roman"))

#plot the results
setwd("C:/Users/Hoyt/Desktop/RaceWritingComputation/Figures")
png(filename="Fig3.png", 
    type="cairo",
    units="in", 
    width=12, 
    height=8, 
    pointsize=10, 
    res=96)

p <- ggplot(data = social_scores, mapping = aes(x = idu , y = score, label = labeled_points))
p + geom_hline(yintercept = 0, size = 1.2, color = "gray80") +
  geom_point(mapping = aes(color = highlight), size = 1.2) + 
  geom_text_repel(size = 5.5, family="Times", nudge_y = .2, nudge_x = -.3) +
  scale_y_continuous(limits = c(-1.2, 2.7)) +
  scale_x_continuous(limits = c(1, 175)) +
  labs(x="", y="Social Score", title="") +
  scale_color_manual(labels = c("yes", "no"), values = c("black", "red")) +
  theme(legend.position = "", axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
        axis.text=element_text(size=14), title=element_text(size=14)) #,face="bold"))

dev.off()