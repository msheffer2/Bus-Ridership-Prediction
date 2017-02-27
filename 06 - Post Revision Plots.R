# Set Working Directory
################################################################################
setwd("C:\\repos\\Bus-Ridership-Prediction")

# Load libraries
################################################################################
library(dplyr)
library(ggplot2)
library(Cairo)
#Explicitly called = caret

# Plotting Test Case RMSE
###############################################################################

#Data
dat <- data.frame(c(195, 818.1, 810.6))
dat <- cbind(dat, c("Model 25","Current Model", "Model 25 Revised"))
names(dat) <- c("Train", "Model")
dat$lab <- as.character(dat$Train)

#Plot
colors <- c(
  "#FFC125", "#0570b0", "#FF0000"
)

tplot <- ggplot(dat, aes(x=Model, y=Train, fill=Model)) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_manual(values=colors) +
  labs(x="", title="", y="Precision (RMSE) - Lower is Better") + 
  scale_y_continuous(limits=c(0,1000), breaks=seq(0,1000,500)) + 
  geom_text(aes(label = lab, y= Train), nudge_y=50, hjust=0) + 
  #geom_text(aes(label = lab1, y= Train), nudge_y=50, hjust=1.5) + 
  theme(text = element_text(size=15), 
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none")
tplot

Cairo(file="./output/mod25_compare.png", 
      type="png", units="in", width=10, height=10, pointsize=15, dpi=300)
print(tplot)
dev.off()
rm(dat, colors, tplot)

# Plotting Importance
###############################################################################

#Data
load("./output/mod25_rev.Rdata")

imp <- caret::varImp(mod25_rev$fit) %>%
  tibble::rownames_to_column() %>%
  rename(Variable=rowname) %>%
  arrange(desc(Overall))

imp$Variable <- c("Day of Service", "High Frequency Line", "Personal Crime Index (Sum)",
                  "Weighted Crime Index", "Line of Service", "Number of Retail Jobs",
                  "Average Walk Score", "Population Size along Line", "Number of Non-retail Jobs",
                  "Number of Vacant Dwelling Unites (Sum)", "Minimum Walk Score", "Season")

imp$Importance <- round(imp$Overall, digits=1)
imp <- select(imp, Variable, Importance) %>%
  mutate(order=nrow(imp):1,
         lab=as.character(Importance))

#Plot

iplot <- ggplot(imp, aes(x=reorder(Variable, order), y=Importance)) + 
  geom_bar(stat="identity", fill="darkmagenta") + coord_flip() + 
  geom_text(aes(label = lab, y= Importance), hjust=-.25, vjust=.3) + 
  labs(x="", title="", y="Variable Importance (OOB Error)") +
  theme(text = element_text(size=15), 
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none")
iplot

Cairo(file="./output/importance.png", 
      type="png", units="in", width=10, height=10, pointsize=15, dpi=300)
print(iplot)
dev.off()
rm(imp, iplot, mod25_rev)










