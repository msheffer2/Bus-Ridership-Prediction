# Set Working Directory
################################################################################
setwd("C:\\repos\\Bus-Ridership-Prediction")

# Load libraries
################################################################################
library(ggplot2)
library(Cairo)
library(tidyr)
library(dplyr)

# Load Data & Plot Precision Results
################################################################################
load("./output/summary.Rdata")

colors <- c(
  "#f1eef6", "#f1eef6", "#f1eef6", "#f1eef6", "#f1eef6", "#f1eef6", 
  "#d0d1e6", "#d0d1e6", "#d0d1e6", "#d0d1e6", "#d0d1e6", "#d0d1e6", 
  "#a6bddb", "#a6bddb", "#a6bddb", "#a6bddb", "#a6bddb", "#a6bddb", 
  "#74a9cf", "#74a9cf", "#74a9cf", "#74a9cf", "#74a9cf", "#74a9cf", 
  "#FF0000", "#FF0000", "#FF0000", "#FF0000", "#FF0000", "#FF0000", 
  "#3690c0", "#3690c0", "#3690c0", "#3690c0", "#3690c0", "#3690c0", 
  "#0570b0", "#0570b0", "#0570b0", "#0570b0", "#0570b0", "#0570b0", 
  "#034e7b", "#034e7b", "#034e7b", "#034e7b", "#034e7b", "#034e7b"
)

# Plot
tplot <- ggplot(summary, aes(x=reorder(Name, Rrun), y=Projected)) + 
  geom_bar(position=position_dodge(), stat="identity", colour=colors, fill=colors) + 
  labs(x="Model", title="", y="Precision (RMSE) - Lower is Better") + 
  scale_y_continuous(limits=c(0,3200), breaks=seq(0,3000,500)) + 
  geom_text(aes(label = lab_pro, y= Projected), angle=90, nudge_y=150) + 
  geom_hline(aes(yintercept=818.1), size=2, colour="#FFC125") +
  annotate("text", x=29, y=2000, label="Test Cases RMSE: 818.1", color="#FFC125", hjust=-.1) +
  annotate("segment", x=29, xend=27, y=2000, yend=1000, color="#FFC125", size=1, arrow=arrow()) +
  annotate("text", x=1, y=0, label="Lasso Models", hjust=.05, vjust=1) +
  annotate("text", x=7, y=0, label="CART Models", hjust=.05, vjust=2.5) +
  annotate("text", x=13, y=0, label="Cubist Models", hjust=.05, vjust=1) +
  annotate("text", x=19, y=0, label="Random Forest", hjust=.05, vjust=2.5) +
  annotate("text", x=25, y=0, label="Bagged Trees", hjust=.05, vjust=1) +
  annotate("text", x=31, y=0, label="GBM Models", hjust=.05, vjust=2.5) +
  annotate("text", x=37, y=0, label="SVM Models", hjust=.05, vjust=1) +
  annotate("text", x=43, y=0, label="Neural Network", hjust=.05, vjust=2.5) +
  theme(text = element_text(size=15), 
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle=90, vjust=.3),
        panel.background = element_blank(),
        legend.position="none")
tplot

Cairo(file="./output/precision.png", 
      type="png", units="in", width=10, height=10, pointsize=15, dpi=300)
print(tplot)
dev.off()

rm(summary, colors, tplot)


