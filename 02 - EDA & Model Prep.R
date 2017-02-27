# Set Working Directory
################################################################################
setwd("C:\\repos\\Bus-Ridership-Prediction")

# Load libraries
################################################################################
library(dplyr)
library(ggplot2)
library(Cairo)
library(caret)

# Read Data Files
################################################################################
load("./data/dat_v0.Rdata")

################################################################################
# EDA - Plotting ridership by each possible predictor and looking at transforms
################################################################################

#Data Prep
rides <- dat_v0$rides
#lines <- as.factor(dat_v0$line)
hold <- select(dat_v0, line, year, season, day, rides, line, contains("line_"), 
               contains("flag_"))
pdat1 <- select(dat_v0, -rides, -line, -contains("line_"), -contains("flag_"),
               -year, -season, -day)
dichotomies <- select(dat_v0, parknride, transitcenter, college, hospital, library, 
                      frequent, contains("emp_"))
dhold <- dichotomies

dichotomies <- data.frame(apply(dichotomies, 2, as.factor))
pdat1 <- select(pdat1, -parknride, -transitcenter, -college, -hospital, -library, 
       -frequent, -contains("emp_"))

# Plot dichotomies
################################################################################
plot_d <- function(DATA, FOLDER){
  names <- names(DATA)
  for (i in (1:length(names))){
    which <- names[i]
    print(which)
    
    plot <- ggplot(DATA, aes_string(x=which, y=rides)) + 
      geom_boxplot() +
      labs(title=which, x="", y="rides") +
      theme(text = element_text(size=15),
            panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.ticks=element_blank(),
            legend.position="none") 
    
    filename <- paste0("./output/eda/", FOLDER, "/", i, " - ", which, ".png")
    Cairo(file=filename, type="png", units="in", width=10, height=10, 
          pointsize=15, dpi=100)
    print(plot)
    dev.off()
  }
  rm(filename, i, names, plot, which)
}
plot_d(DATA=dichotomies, FOLDER="dichotomies")
rm(plot_d, dichotomies)

# Plot raw interval data
################################################################################
plot_i <- function(DATA, FOLDER){
  names <- names(DATA)
  for (i in (1:length(names))){
    which <- names[i]
    print(which)
    
    plot <- ggplot(DATA, aes_string(x=which, y=rides)) +
      geom_point(size=3) +
      labs(title=which, x="", y="rides") +
      stat_smooth(method=loess) + 
      theme(text = element_text(size=15),
            panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            axis.ticks=element_blank(),
            legend.position="none")
    
    filename <- paste0("./output/eda/", FOLDER, "/", i, " - ", which, ".png")
    Cairo(file=filename, type="png", units="in", width=10, height=10, 
          pointsize=15, dpi=100)
    print(plot)
    dev.off()
  }
  rm(filename, i, names, plot, which)
}
plot_i(DATA=pdat1, FOLDER="raw")

#NOTE: Overall, pretty decent distributions

# Plot log of interval data
################################################################################
pdat2 <- pdat1
colnames(pdat2) <- paste("l", colnames(pdat1), sep = "")
pdat2 <- pdat2 + .0001
pdat2 <- data.frame(apply(pdat2, 2, log))
plot_i(DATA=pdat2, FOLDER="logs")

#NOTE: Logs do not improve the distributions

# Plot square of interval data
################################################################################
pdat3 <- pdat1
colnames(pdat3) <- paste("s", colnames(pdat1), sep = "")
pdat3 <- data.frame(apply(pdat3, 2, function(x) x*x))
plot_i(DATA=pdat3, FOLDER="squares")

#NOTE: Squares look better but not sure they are really necessary

# Try Boxcox transformation
################################################################################
trans <- preProcess(pdat1, method = c("BoxCox"))
trans
pdat4 <- predict(trans, pdat1)
colnames(pdat4) <- paste("b", colnames(pdat4), sep = "")
plot_i(DATA=pdat4, FOLDER="boxcox")

#NOTE: Boxcox does not improve the distributions
rm(trans, plot_i, rides)

# Identify Zero Variance Variables
################################################################################
zv <- nearZeroVar(pdat1, saveMetrics = TRUE)
zv <- zv[zv$zeroVar==TRUE,]
zv <- rownames(zv)
zv

#NOTE:  none in data
rm(zv)

# Any Highly Correlated?
################################################################################
corr <- cor(as.matrix(pdat1))
highcorr <- findCorrelation(corr, cutoff=.5)
length(highcorr)
names(pdat1[highcorr])
pdat1nc <- select(pdat1, -(highcorr))
rm(highcorr, corr)

corr <- cor(as.matrix(pdat2))
highcorr <- findCorrelation(corr, cutoff=.5)
length(highcorr)
names(pdat2[highcorr])
pdat2nc <- select(pdat2, -(highcorr))
rm(highcorr, corr)

corr <- cor(as.matrix(pdat3))
highcorr <- findCorrelation(corr, cutoff=.5)
length(highcorr)
names(pdat3[highcorr])
pdat3nc <- select(pdat3, -(highcorr))
rm(highcorr, corr)

corr <- cor(as.matrix(pdat4))
highcorr <- findCorrelation(corr, cutoff=.5)
length(highcorr)
names(pdat4[highcorr])
pdat4nc <- select(pdat4, -(highcorr))
rm(highcorr, corr)

# Create Train and Test Partitions
################################################################################
set.seed(2345)
flag <- createDataPartition(hold$line, p=.7, list=FALSE)
hold$tflag <- 0
hold[flag,]$tflag <- 1; #Training Data
hold[-flag,]$tflag <- 2; #Test Data
table(hold$tflag)
rm(flag)

dat_v1 <- bind_cols(hold, pdat1, dhold)
save(dat_v1, file="./data/dat_v1.Rdata")

dat_v2 <- bind_cols(hold, pdat2, dhold)
save(dat_v2, file="./data/dat_v2.Rdata")

#NOTE: At the time of the analysis, I decided to exlude the log-transformed
#      data since they did not appear to improve on the distributions over the
#      raw data, so all models were run on v1, v3, and v4 data only.

dat_v3 <- bind_cols(hold, pdat3, dhold)
save(dat_v3, file="./data/dat_v3.Rdata")

dat_v4 <- bind_cols(hold, pdat4, dhold)
save(dat_v4, file="./data/dat_v4.Rdata")

dat_v1nc <- bind_cols(hold, pdat1nc, dhold)
save(dat_v1nc, file="./data/dat_v1nc.Rdata")

dat_v2nc <- bind_cols(hold, pdat2nc, dhold)
save(dat_v2nc, file="./data/dat_v2nc.Rdata")

#NOTE: At the time of the analysis, I decided to exlude the log-transformed
#      data since they did not appear to improve on the distributions over the
#      raw data, so all models were run on v1, v3, and v4 data only.

dat_v3nc <- bind_cols(hold, pdat3nc, dhold)
save(dat_v3nc, file="./data/dat_v3nc.Rdata")

dat_v4nc <- bind_cols(hold, pdat4nc, dhold)
save(dat_v4nc, file="./data/dat_v4nc.Rdata")

rm(hold, dhold, list=ls(pattern="dat"))




    
    

