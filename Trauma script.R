# Import dataset + attach packages

library(readxl)
Data_sheet_2018_2019_29_09_20_1_ <- read_excel("~/Documents/Job/Research /ICU /Trauma study/Data/Data sheet 2018-2019 29.09.20 (1).xlsx", 
                                               sheet = "20192018")
View(Data_sheet_2018_2019_29_09_20_1_)

Trauma_data <- Data_sheet_2018_2019_29_09_20_1_

library(tidyverse)

#Assess missing data in variables 

library(Amelia)

colnames(Trauma_data)

Trauma_data <- Trauma_data[, c("Age", "Sex...4", "CCI", "Mechanism", "GCS (Ps calculation)",
                               "ISS", "Ps", "APACHE", "Number of Operations", "Most severely injured body region",
                               "Dead", "LOS")]

missmap(Trauma_data, rank.order = FALSE, main = "Missing values vs observed")

#Label variables 
Trauma_data <- NIV_data[complete.cases(NIV_data$Troponin),] #omit cases without trop

age <- Trauma_data$Age
age <- na.omit(age)
sex <- Trauma_data$Sex...4
sex <- na.omit(sex)
cci <- Trauma_data$CCI
cci <- na.omit(cci)
mech <- as.factor(Trauma_data$Mechanism)
mech <- na.omit(mech)
GCS <- Trauma_data$`GCS (Ps calculation)`
GCS <- na.omit(GCS)
iss <- Trauma_data$ISS
iss <- na.omit(iss)
ps <- Trauma_data$Ps
ps <- na.omit(ps)
ps <- as.integer(ps)
apache <- Trauma_data$APACHE
apache <- na.omit(apache)
num_op <- Trauma_data$`Number of Operations`
num_op <- na.omit(num_op)
most_severe_injury_loc <- Trauma_data$`Most severely injured body region`
most_severe_injury_loc <- as.factor(most_severe_injury_loc)
most_severe_injury_loc <- na.omit(most_severe_injury_loc)
dead <- Trauma_data$Dead
dead <- na.omit(dead)
los <- Trauma_data$LOS
los <- na.omit(los)

Trauma_data <- Trauma_data[complete.cases(Trauma_data$CCI),] #remove cases without CCI data

#Modify mechanism of injury to different categories 

mech <- as.factor(recode(mech, "Blow(s)"="Other", "Burn" = "Other", "Crush"="Other",
                         "Stabbing"="Other", "Vehicle incident/collision"="RTC",
                         "Fall less than 2m"= "Fall<2m", "Fall more than 2m"="Fall>2m"))
table(mech)
levels(mech)
mech <- relevel(mech, ref="RTC")

table(most_severe_injury_loc)
levels(most_severe_injury_loc)
most_severe_injury_loc <- relevel(most_severe_injury_loc, ref="Chest")

#Categorise the scoring variabels 

library(dplyr)
GCS <- case_when(GCS %in% 15 ~ 0,  #GCS 
          GCS %in% 3:14 ~ 1)

GCS <- as.factor(GCS)
table(GCS)

apache <- case_when(apache %in% 0:10 ~ 0,  #apache 
                    apache %in% 11:20 ~ 1,
                    apache %in% 21:30 ~ 2)
apache <- as.factor(apache)
table(apache)

ps <- case_when(ps %in% 81:100 ~ 0,  #PS19 
                    ps %in% 61:80 ~ 1,
                    ps %in% 41:60 ~ 2,
                ps %in% 21:40 ~ 3,
                ps %in% 0:20 ~ 4)
ps <- as.factor(ps)
table(ps)

iss <- case_when(iss %in% 1:20 ~ 0,  #PS19 
                iss %in% 21:40 ~ 1,
                iss %in% 41:60 ~ 2,
                iss %in% 61:80 ~ 3)
iss <- as.factor(iss)
table(iss)

# Assess linearity of variables 

# Density plot to look for normal distribution 
library(ggpubr)
library(moments)
par(mar=c(5,5,5,5))

d <- density(num_op) # returns the density data 
skewness(num_op, na.rm = TRUE) #Skewness
plot(d, main = "Density plot No. of Surgeries", sub = "Skewness = 2.74") # plots the results

# Bar plot
?barplot
mech_counts <- table(mech)
barplot(mech_counts, main="Mechanism of injury", horiz = FALSE,
        xlab="Mechanism of injury", ylim )

most_severe_injury_loc_counts <- table(most_severe_injury_loc)
par(mar=c(6,6,6,6))
barplot(most_severe_injury_loc_counts, main="Most severely injured location", horiz = FALSE)

par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.

mech_counts <- table(mech)
barplot(mech_counts, main="Mechanism of Injury", horiz=TRUE, xlim = c(0, 200), cex.names=0.8)

#Dichotomise variables that are non-linear - not done but could do...

#Categorise PF 
p_f <- ifelse(p_f < 13.3, "severe",
              ifelse(p_f>=13.3 & p_f<26.7, "Moderate", NA))
table(p_f)

#Create bar plot PS19 vs age category 

age <- Trauma_data$Age
age <- as.integer(age)
age <- na.omit(age)
age_cat <- case_when(age %in% 10:20 ~ 1,  #age categories 
                 age %in% 21:30 ~ 2,
                 age %in% 31:40 ~ 3,
                 age %in% 41:50 ~ 4,
                 age %in% 51:60 ~ 5,
                 age %in% 61:70 ~ 6,
                 age %in% 71:80 ~ 7,
                 age %in% 81:90 ~ 8,
                 age %in% 91:100 ~ 9)
table(age_cat)
Trauma_data$age_cat <- age_cat
table(age_cat, ps)

age_cat1 <- Trauma_data[Trauma_data$age_cat == '1',] #Gather mean and SD for PS 
age_cat2 <- Trauma_data[Trauma_data$age_cat == '2',]
age_cat3 <- Trauma_data[Trauma_data$age_cat == '3',]
age_cat4 <- Trauma_data[Trauma_data$age_cat == '4',]
age_cat5 <- Trauma_data[Trauma_data$age_cat == '5',]
age_cat6 <- Trauma_data[Trauma_data$age_cat == '6',]
age_cat7 <- Trauma_data[Trauma_data$age_cat == '7',]
age_cat8 <- Trauma_data[Trauma_data$age_cat == '8',]
age_cat9 <- Trauma_data[Trauma_data$age_cat == '9',]

mean(age_cat9$Ps, na.rm = TRUE)
sd(age_cat9$Ps, na.rm = TRUE)

library(readxl)
Data_sheet_2018_2019_29_09_20_1_ <- read_excel("~/Documents/Job/Research /ICU /Trauma study/Data/Data sheet 2018-2019 29.09.20 (1).xlsx", 
                                               sheet = "Survival vs PS graph")
View(Data_sheet_2018_2019_29_09_20_1_)

PSGraph <- Data_sheet_2018_2019_29_09_20_1_
age_gp <- PSGraph$Age
age_gp <- as.factor(age_gp)
survival <- PSGraph$Survival
ps_y_n <- PSGraph$PS19_y_n
ps_y_n <- as.factor(ps_y_n)
ps_sd <- PSGraph$sd

ggplot(data=PSGraph, aes(x=age_gp, y=survival, fill=ps_y_n)) +
  geom_bar(stat="identity", position=position_dodge())
  

p <- ggplot(data=PSGraph, aes(x=age_gp, y=survival, fill=ps_y_n)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=survival), vjust=1.6, color="black",
            position = position_dodge(0.9), size=2.8)+
scale_fill_brewer(palette="Set1")+
  ylim(0,100)

p + scale_x_discrete("Age (years)",labels = c("1" = "10-20","2" = "21-30", "3" = "31-40","4" = "41-50","5" = "51-60","6" = "61-70","7" = "71-80","8" = "81-90","9" = "91-100"))+
  ylab("Survival (%)")+
  labs(fill = "", title = "Observed vs Ps19 predicted survival")+
  theme(plot.title = element_text(hjust = 0.5))


# Univariate models

univariate <- glm(dead ~ apache, family = binomial (link=logit) )
summary(univariate)
exp(univariate$coefficients)
exp(confint(univariate))

# Correlation matrix 


Trauma_data_correlation <- Trauma_data[, c("Age", "GCS (Ps calculation)",
                               "ISS", "Ps", "APACHE", "Number of Operations")]

library("Hmisc")
res2 <- rcorr(as.matrix(Trauma_data_correlation), type = "spearman")
res2

res2$r
res2$P

flattenCorrMatrix <- function(cormat, pmat) { #format correlation matrix 
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

correlation_matrix <- flattenCorrMatrix(res2$r, res2$P)
write.csv(correlation_matrix, "Correlation_matrix.csv", row.names = F) #export table 


library(corrplot) #visualise the correlation matrix
res <- cor(Trauma_data_correlation)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


corrplot(res2$r, type="upper", order="hclust", #insignificant correlations removed
         p.mat = res2$P, sig.level = 0.01, insig = "blank")

#Multivariate 

Trauma_data_multivariate <- Trauma_data[, c("Age", "GCS (Ps calculation)",
                                            "ISS", "Ps", "APACHE", "Number of Operations",
                                            "Dead", "LOS", "Mechanism", "Most severely injured body region")]

Trauma_data_multivariate <- Trauma_data_multivariate[complete.cases(Trauma_data_multivariate$Ps),]
Trauma_data_multivariate <- Trauma_data_multivariate[complete.cases(Trauma_data_multivariate$`GCS (Ps calculation)`),]
age <- Trauma_data_multivariate$Age
iss <- Trauma_data_multivariate$ISS
GCS <- Trauma_data_multivariate$`GCS (Ps calculation)`
GCS <- case_when(GCS %in% 15 ~ 0,  #GCS recode
                 GCS %in% 3:14 ~ 1)

GCS <- as.factor(GCS)
table(GCS)
ps <- Trauma_data_multivariate$Ps
num_op <- Trauma_data_multivariate$`Number of Operations`
mech <- Trauma_data_multivariate$Mechanism
mech <- as.factor(recode(mech, "Blow(s)"="Other", "Burn" = "Other", "Crush"="Other", #recode mech 
                         "Stabbing"="Other", "Vehicle incident/collision"="RTC",
                         "Fall less than 2m"= "Fall<2m", "Fall more than 2m"="Fall>2m"))
table(mech)
levels(mech)
mech <- relevel(mech, ref="RTC") #Relevel mech 
most_severe_injury_loc <- Trauma_data_multivariate$`Most severely injured body region`
most_severe_injury_loc <- as.factor(most_severe_injury_loc)
most_severe_injury_loc <- relevel(most_severe_injury_loc, ref = "Chest") #relevel injury site 
dead <- Trauma_data_multivariate$Dead



multivariate <- glm(dead ~ most_severe_injury_loc+age+GCS+num_op, family = binomial (link=logit) )
summary(multivariate)
exp(multivariate$coefficients)
exp(confint(multivariate))


#Dominance analysis 

anova(multivariate, test = "Chisq")

library(pscl)

pR2(multivariate)

library(dominanceanalysis)

da.glm.fit()("names")

dapres<-dominanceAnalysis(multivariate)

getFits(dapres,"r2.m")

dominanceMatrix(dapres, type="complete",fit.functions = "r2.m", ordered=TRUE) #complete dominance = 1, complete dominance not established = 0.5

contributionByLevel(dapres,fit.functions="r2.m")

plot(dapres, which.graph ="conditional",fit.function = "r2.m") #Conditional dominance plot 

dominanceMatrix(dapres, type="conditional",fit.functions = "r2.m", ordered=TRUE)

averageContribution(dapres,fit.functions = "r2.m")

plot(dapres, which.graph ="general",fit.function = "r2.m") #General dominance plot

bootmodpres100 <- bootDominanceAnalysis(Multivariate, R=100) # Bootstrap analysis for dominance analysis 
summary(bootmodpres100,fit.functions="r2.m")

bootavemodpres100<-bootAverageDominanceAnalysis(Multivariate,R=100) # Bootstrap analysis for general dominance analysis 
summary(bootavemodpres100,fit.functions=c("r2.m"))


# ROC graphs 

Trauma_data <- Data_sheet_2018_2019_29_09_20_1_
Trauma_data <- Trauma_data[, c("Age", "Sex...4", "CCI", "Mechanism", "GCS (Ps calculation)",
                               "ISS", "Ps", "APACHE", "Number of Operations", "Most severely injured body region",
                               "Dead", "LOS")]

# Include Age, APACHE, ISS, number of surgeries, GCS, 

Trauma_data <- Trauma_data[complete.cases(Trauma_data$Age),] #omit cases without age
Trauma_data <- Trauma_data[complete.cases(Trauma_data$APACHE),] #omit cases without APACHE
Trauma_data <- Trauma_data[complete.cases(Trauma_data$ISS),] #omit cases without ISS
Trauma_data <- Trauma_data[complete.cases(Trauma_data$`Number of Operations`),] #omit cases without no. operations
Trauma_data <- Trauma_data[complete.cases(Trauma_data$`GCS (Ps calculation)`),] #omit cases without GCS 

age <- Trauma_data$Age
apache <- Trauma_data$APACHE
iss <- Trauma_data$ISS
surgeries <- Trauma_data$`Number of Operations`
GCS <- Trauma_data$`GCS (Ps calculation)`
dead <- Trauma_data$Dead

library(pROC)
library(randomForest)

par(mar=c(1,1,1,1))

#Overlap AUCs

univariate <- glm(dead ~ age, family = binomial, data = Trauma_data)
summary(univariate)
par(pty = "s")

roc(dead, univariate$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage")
univariate2 <- glm(dead ~ apache, family = binomial, data = Trauma_data)

univariate3 <- glm(dead ~ iss, family = binomial, data = Trauma_data)
univariate4 <- glm(dead ~ GCS, family = binomial, data = Trauma_data)
univariate5 <- glm(dead ~ surgeries, family = binomial, data = Trauma_data)


roc(dead, univariate$fitted.values, plot = TRUE, legacy.axes = TRUE, percent = TRUE,
    xlab= "False Positive Percentage", ylab = "True Positive Percentage", col = "black", lwd = 1, print.auc = FALSE, print.auc.y = 70, print.auc.x = 40)
plot.roc(dead, univariate2$fitted.values, percent = TRUE, col = "blue", lwd = 1, add = TRUE, print.auc = FALSE, print.auc.y = 80, print.auc.x = 40)

plot.roc(dead, univariate3$fitted.values, percent = TRUE, col = "red", lwd = 1, add = TRUE, print.auc = FALSE, print.auc.y = 60, print.auc.x = 40)
plot.roc(dead, univariate4$fitted.values, percent = TRUE, col = "orange", lwd = 1, add = TRUE, print.auc = FALSE, print.auc.y = 50, print.auc.x = 40)
plot.roc(dead, univariate5$fitted.values, percent = TRUE, col = "forest green", lwd = 1, add = TRUE, print.auc = FALSE, print.auc.y = 40, print.auc.x = 40)


legend("bottomright", title = "Predictors of mortality", 
       legend = c("Age", "APACHE II score", "ISS", "GCS", "Number of surgeries"), col = c("black", "blue", "red", "orange", "forest green"), 
       lwd = 2, "Legend", cex=0.8)

