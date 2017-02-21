# Set Working Directory
################################################################################
setwd("C:\\repos\\Bus-Ridership-Prediction")

# Load libraries
################################################################################
library(dplyr)
#Explicitly called:  leaps

# Load Data Files & Prep for Merge
################################################################################
load("./data/iv1.Rdata"); #Crime and Demographic data
load("./data/iv2.Rdata"); #Population and Jobs data
load("./data/iv3.Rdata"); #Frequency and Number of Stops data
load("./data/iv4.Rdata"); #Ontime Performance data
load("./data/iv5.Rdata"); #Race, Education, and Additional Employment Data
load("./data/rides.Rdata"); #Ridership data

lines <- select(rides, line) %>%
    group_by(line) %>%
    summarize(count=n()) %>%
    as.data.frame() %>%
    select(line)

rides <-select(rides, line, year, season,day)

iv1 <- left_join(lines, iv1, by="line")
iv2 <- left_join(lines, iv2, by="line")
iv3 <- left_join(lines, iv3, by="line")
iv4 <- left_join(rides, iv4, by=c("line", "year", "season", "day"))
iv5 <- left_join(lines, iv5, by="line")
rm(lines, rides)

load("./data/rides.Rdata")

# Merge Data Files
dat <- left_join(rides, iv1, by="line") %>%
  left_join(iv2, by="line") %>%
  left_join(iv3, by="line") %>%
  left_join(iv4, by=c("line", "year", "season", "day")) %>%
  left_join(iv5, by="line")

rm(iv1, iv2, iv3, iv4, iv5, rides)

# Identify Missing Data
################################################################################
misslist <- function(x) {
    missing <- data.frame(colSums(is.na(x)))
    missing$var <- rownames(missing)
    names(missing) <- c("nmiss", "var")
    missing <- select(missing, var, nmiss) %>%
        filter(nmiss > 0)
    misslist <- missing$var
    return(misslist)
}
misslist(dat)

# Non Imputation Missing Replacement, i.e. assuming missing = not present/frequent
fixlist <- c("parknride", "transitcenter", "college", "hospital", "library", "frequent",
             "emp_big10", "emp_adidas", "emp_axiom", "emp_columbia", "emp_easy", 
             "emp_greenbrier", "emp_ibm", "emp_nike", "emp_pinpoint", "emp_tektronix",
             "emp_waggener")

dat[fixlist] <- apply(dat[fixlist], 2, function(x) ifelse(is.na(x), 0, x))
rm(fixlist)

#NOTE:  All other missing data will be imputed
missing <- misslist(dat)

# Imputation Model
################################################################################
fillit <- function(x){
  hold <- dat
  dat2 <- select(dat, -rides)
  dat2 <- data.frame(apply(hold, 2, 
                           function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
  for (i in missing){
    use <- dat2[names(dat2) != i]
    use <- cbind(use, dat[names(dat)==i])
    form1 <- paste(i," ~.")
    base <- leaps::regsubsets(as.formula(form1), data=use, method="forward", nvmax = 10)
    sout <- summary(base)
    best<- sout$which[seq(along = sout$cp)[sout$cp == min(sout$cp)], ]
    form2 <- names(best)[best][-1]
    form2 <- paste(form2, collapse = " + ")
    form2 <- paste(i," ~", form2)
    fit <- lm(as.formula(form2), data=dat2) 
    pred <- predict(fit, dat2)
    pred <- ifelse(pred < 0, 0, pred)
    hold[i] <- pred
  }
  return(hold)
}
predicted <- fillit()
rm(fillit, missing, misslist, dat)

# Creating some flags for modelling
################################################################################
dat_v0 <- predicted

#Create Season Flags
dat_v0$flag_win <- ifelse(dat_v0$season==1, 1, 0)
dat_v0$flag_spr <- ifelse(dat_v0$season==2, 1, 0)
dat_v0$flag_sum <- ifelse(dat_v0$season==3, 1, 0)
dat_v0$flag_fal <- ifelse(dat_v0$season==4, 1, 0)

#Create Line Flags
dat_v0$line_1 <- ifelse(dat_v0$line==1, 1, 0)
dat_v0$line_4 <- ifelse(dat_v0$line==4, 1, 0)
dat_v0$line_6 <- ifelse(dat_v0$line==6, 1, 0)
dat_v0$line_8 <- ifelse(dat_v0$line==8, 1, 0)
dat_v0$line_9 <- ifelse(dat_v0$line==9, 1, 0)
dat_v0$line_10 <- ifelse(dat_v0$line==10, 1, 0)
dat_v0$line_11 <- ifelse(dat_v0$line==11, 1, 0)
dat_v0$line_12 <- ifelse(dat_v0$line==12, 1, 0)
dat_v0$line_14 <- ifelse(dat_v0$line==14, 1, 0)
dat_v0$line_15 <- ifelse(dat_v0$line==15, 1, 0)
dat_v0$line_16 <- ifelse(dat_v0$line==16, 1, 0)
dat_v0$line_17 <- ifelse(dat_v0$line==17, 1, 0)
dat_v0$line_18 <- ifelse(dat_v0$line==18, 1, 0)
dat_v0$line_19 <- ifelse(dat_v0$line==19, 1, 0)
dat_v0$line_20 <- ifelse(dat_v0$line==20, 1, 0)
dat_v0$line_21 <- ifelse(dat_v0$line==21, 1, 0)
dat_v0$line_22 <- ifelse(dat_v0$line==22, 1, 0)
dat_v0$line_23 <- ifelse(dat_v0$line==23, 1, 0)
dat_v0$line_24 <- ifelse(dat_v0$line==24, 1, 0)
dat_v0$line_25 <- ifelse(dat_v0$line==25, 1, 0)
dat_v0$line_28 <- ifelse(dat_v0$line==28, 1, 0)
dat_v0$line_29 <- ifelse(dat_v0$line==29, 1, 0)
dat_v0$line_30 <- ifelse(dat_v0$line==30, 1, 0)
dat_v0$line_31 <- ifelse(dat_v0$line==31, 1, 0)
dat_v0$line_32 <- ifelse(dat_v0$line==32, 1, 0)
dat_v0$line_33 <- ifelse(dat_v0$line==33, 1, 0)
dat_v0$line_34 <- ifelse(dat_v0$line==34, 1, 0)
dat_v0$line_35 <- ifelse(dat_v0$line==35, 1, 0)
dat_v0$line_36 <- ifelse(dat_v0$line==36, 1, 0)
dat_v0$line_37 <- ifelse(dat_v0$line==37, 1, 0)
dat_v0$line_38 <- ifelse(dat_v0$line==38, 1, 0)
dat_v0$line_39 <- ifelse(dat_v0$line==39, 1, 0)
dat_v0$line_43 <- ifelse(dat_v0$line==43, 1, 0)
dat_v0$line_44 <- ifelse(dat_v0$line==44, 1, 0)
dat_v0$line_45 <- ifelse(dat_v0$line==45, 1, 0)
dat_v0$line_46 <- ifelse(dat_v0$line==46, 1, 0)
dat_v0$line_47 <- ifelse(dat_v0$line==47, 1, 0)
dat_v0$line_48 <- ifelse(dat_v0$line==48, 1, 0)
dat_v0$line_50 <- ifelse(dat_v0$line==50, 1, 0)
dat_v0$line_51 <- ifelse(dat_v0$line==51, 1, 0)
dat_v0$line_52 <- ifelse(dat_v0$line==52, 1, 0)
dat_v0$line_53 <- ifelse(dat_v0$line==53, 1, 0)
dat_v0$line_54 <- ifelse(dat_v0$line==54, 1, 0)
dat_v0$line_55 <- ifelse(dat_v0$line==55, 1, 0)
dat_v0$line_56 <- ifelse(dat_v0$line==56, 1, 0)
dat_v0$line_57 <- ifelse(dat_v0$line==57, 1, 0)
dat_v0$line_58 <- ifelse(dat_v0$line==58, 1, 0)
dat_v0$line_59 <- ifelse(dat_v0$line==59, 1, 0)
dat_v0$line_61 <- ifelse(dat_v0$line==61, 1, 0)
dat_v0$line_62 <- ifelse(dat_v0$line==62, 1, 0)
dat_v0$line_63 <- ifelse(dat_v0$line==63, 1, 0)
dat_v0$line_64 <- ifelse(dat_v0$line==64, 1, 0)
dat_v0$line_65 <- ifelse(dat_v0$line==65, 1, 0)
dat_v0$line_66 <- ifelse(dat_v0$line==66, 1, 0)
dat_v0$line_67 <- ifelse(dat_v0$line==67, 1, 0)
dat_v0$line_68 <- ifelse(dat_v0$line==68, 1, 0)
dat_v0$line_70 <- ifelse(dat_v0$line==70, 1, 0)
dat_v0$line_71 <- ifelse(dat_v0$line==71, 1, 0)
dat_v0$line_72 <- ifelse(dat_v0$line==72, 1, 0)
dat_v0$line_75 <- ifelse(dat_v0$line==75, 1, 0)
dat_v0$line_76 <- ifelse(dat_v0$line==76, 1, 0)
dat_v0$line_77 <- ifelse(dat_v0$line==77, 1, 0)
dat_v0$line_78 <- ifelse(dat_v0$line==78, 1, 0)
dat_v0$line_79 <- ifelse(dat_v0$line==79, 1, 0)
dat_v0$line_80 <- ifelse(dat_v0$line==80, 1, 0)
dat_v0$line_81 <- ifelse(dat_v0$line==81, 1, 0)
dat_v0$line_83 <- ifelse(dat_v0$line==83, 1, 0)
dat_v0$line_84 <- ifelse(dat_v0$line==84, 1, 0)
dat_v0$line_85 <- ifelse(dat_v0$line==85, 1, 0)
dat_v0$line_87 <- ifelse(dat_v0$line==87, 1, 0)
dat_v0$line_88 <- ifelse(dat_v0$line==88, 1, 0)
dat_v0$line_92 <- ifelse(dat_v0$line==92, 1, 0)
dat_v0$line_93 <- ifelse(dat_v0$line==93, 1, 0)
dat_v0$line_94 <- ifelse(dat_v0$line==94, 1, 0)
dat_v0$line_96 <- ifelse(dat_v0$line==96, 1, 0)
dat_v0$line_99 <- ifelse(dat_v0$line==99, 1, 0)
dat_v0$line_115 <- ifelse(dat_v0$line==115, 1, 0)
dat_v0$line_152 <- ifelse(dat_v0$line==152, 1, 0)
dat_v0$line_154 <- ifelse(dat_v0$line==154, 1, 0)
dat_v0$line_155 <- ifelse(dat_v0$line==155, 1, 0)
dat_v0$line_156 <- ifelse(dat_v0$line==156, 1, 0)

#Convert Season Variable into Factor
dat_v0$season <- factor(dat_v0$season, 
                       levels=c(1,2,3,4), 
                       labels=c("Winter", "Spring", "Summer", "Fall"))

#Convert Day Variable into Factor
dat_v0$day <- factor(dat_v0$day, 
                    levels=c(1,2,3), 
                    labels=c("Weekday", "Saturday", "Sunday"))

# Save Data Files
################################################################################
save(dat_v0, file="./data/dat_v0.Rdata")

rm(predicted, dat_v0)

 
    
    
    
    

