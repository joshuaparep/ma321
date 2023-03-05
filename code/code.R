#install.packages("tidyverse")
#install.packages("glmnet")
#install.packages("moments")
#install.packages("ggthemes")
#install.packages("ggstatsplot")
#install.packages("flextable") for beautify tables
#install.packages("dlookr")
library(dlookr)
library(flextable)
library(ggstatsplot)
library(knitr)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(moments)
library(caret)
library(glmnet)
library(mice)
library(tidyverse)

#Task (1.A) - Load Data
df <- read.csv("./raw_data/house_data.csv")
attach(df)

#Task 1(B) - Statistical Analysis Description
head(df)
names(df)
dim(data)
summary(df)
View(df)
str(df)

##############################################
### REMOVING COLUMN WITH TOTAL No. of NA > 80 %
##############################################
# define a function to check NAs as we will be using it frequenty to check
# identify columns (by column number) with missing values
naCols <- which(colSums(is.na(df))>0)
# get columns with missing values sorted by number of missing values
sort(colSums(sapply(df[naCols],is.na)), decreasing = TRUE) #10 clumns with NA


#Visualize missing data 
library(VIM)
aggr_plot <- aggr(df, col=c('navyblue','red'),
                  numbers=TRUE,
                  sortVars=TRUE,
                  labels=names(df),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("Histogram of Missing data","Pattern"))

#set NA threshold to 80% and remove the column with total no. NA above 80%
#new dataframe with total no. NA < 80 (4 columns with NA > 80% deselected)
#threshold <- 0.8 
#df <- data %>%
#  select(where(~mean(is.na(.)) < threshold))
 
#############################################################################
### HANDLING REMAINING NAs AFTER REMOVING COLUMN WITH TOTAL NO. of NA > 80%
#############################################################################
df_naCols <- which(colSums(is.na(df))>0) #get column with any NAs
sort(colSums(sapply(df[df_naCols],is.na)), decreasing = TRUE) # 6 columns with NAs

str(df$LotFrontage)     # 259 NAs -> int
str(df$MasVnrArea)      # 8  NAs  -> int
unique(df$GarageType)   # 81 NAs  -> char
unique(df$GarageCond)   # 81 NAs -> char
unique(df$BsmtQual)     # 37 NAs -> char
unique(df$BsmtCond)     # 37 NAs -> char

boxplot(df$MasVnrArea)
Q <- quantile(df$MasVnrArea, probs=c(.25, .75), na.rm = TRUE)

eliminated<- subset(df, df$MasVnrArea > (Q[1] - 1.5*iqr) & df$MasVnrArea < (Q[2]+1.5*iqr))  #Removing outliers


# Drop variables with 80% missing data
df1 <- subset(df, select = -c(PoolQC, MiscFeature, Alley, Fence))

# Before Imputing NAs, factor char variables first
# Factor all categorical variables
df1[sapply(df1, is.character)] <- lapply(df1[sapply(df1, is.character)], as.factor)

str(df1) # Show char variables converted to factor

# Subset numerical variables that require factoring
num_var_factor <-  c("OverallQual", "OverallCond", "FullBath", "BedroomAbvGr",
                                 "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", "MoSold", "YrSold")

# Factor numerical variables
df1[num_var_factor] <- lapply(df1[num_var_factor], factor)

str(df1) # Show numerical variables converted to factor


# Merge imputed categorical variable to dataframe
#df_merge <- cbind(df, df_sub)

#Convert categorical variables to factor
md.pattern(df1)

#impute missing data - In this case, the random forest mice function is used. Random seed set to 5
imp_df <- mice(df1, seed = 2023, m=5, method = "rf")
comppleted_imp_df <- complete(imp_df,3) # use 3rd cycle complete imputed dataset
summary(complete(comppleted_imp_df)) ##Summary for Descriptive Statistical Analysis
stripplot(imp_df, pch = 20, cex = 1.2) #illustrate imputed data

# Check for any NA after imputation?
sapply(comppleted_imp_df, function(x) sum(is.na(x))) # good to go!
View(comppleted_imp_df)


#############################################################################
############################################
##### CHECKING AND HANDLING OUTLIERS #######
############################################
length(select_if(df1, is.numeric)) # 14 numberical variables
names(select_if(df1, is.numeric))

diagnose_numeric(comppleted_imp_df) %>% 
  filter(minus > 0 | zero > 0) %>% 
  select(variables, median, zero:outlier) %>% 
  flextable()

# Take average for the outliers to check the influence on variables
diagnose_outlier(comppleted_imp_df) %>% flextable()
# Get descriptive statistics after imputed
describe(comppleted_imp_df) %>% flextable()

###Normality Test
normality(comppleted_imp_df) %>% flextable()

#check outliers graphically for all the numberical variables 
comppleted_imp_df %>% select(SalePrice) %>%  plot_outlier()   
comppleted_imp_df %>% select(LotFrontage) %>%  plot_outlier() 
comppleted_imp_df %>% select(LotArea) %>%  plot_outlier()     
comppleted_imp_df %>% select(YearBuilt) %>%  plot_outlier()   
comppleted_imp_df %>% select(MasVnrArea) %>%  plot_outlier()
comppleted_imp_df %>% select(TotalBsmtSF) %>%  plot_outlier() 
comppleted_imp_df %>% select(X2ndFlrSF) %>%  plot_outlier()   
comppleted_imp_df %>% select(LowQualFinSF) %>%  plot_outlier()
comppleted_imp_df %>% select(GrLivArea) %>%  plot_outlier()   
comppleted_imp_df %>% select(GarageArea) %>%  plot_outlier()
comppleted_imp_df %>% select(PoolArea) %>%  plot_outlier()    
comppleted_imp_df %>% select(MiscVal) %>%  plot_outlier()     

# Replace the outliers of the variable with capping.
outliers_imputed_data <- imputate_outlier(comppleted_imp_df,SalePrice, method = "capping")
plot(outliers_imputed_data)
boxplot(outliers_imputed_data)#2 outliers

#check imputed outliers - checking any learn relationship
plot(SalePrice ~ log(LotArea), data=comppleted_imp_df)     # Take log
plot(SalePrice ~ log(LotArea), data=comppleted_imp_df)     # Take log
plot(SalePrice ~ YearBuilt, data=comppleted_imp_df)        #
plot(SalePrice ~ log(MasVnrArea + 1), data=comppleted_imp_df)# Take care of 0
plot(SalePrice ~ log(TotalBsmtSF), data=comppleted_imp_df) # Take log
plot(SalePrice ~ log(X2ndFlrSF + 1), data=comppleted_imp_df)   # take care of 0
plot(SalePrice ~ log(LowQualFinSF + 1), data=comppleted_imp_df)# Take log
plot(SalePrice ~ GrLivArea, data=comppleted_imp_df)        #
plot(log(SalePrice) ~ log(GarageArea), data=comppleted_imp_df)# Take log
plot(SalePrice ~ log(PoolArea) , data=comppleted_imp_df)    #0
plot(SalePrice ~ MiscVal, data=comppleted_imp_df)     #0



############################################
##### DESCRIPTIVE STATISTICS ANALYSIS ######
############################################
sum_dat<-dat %>%
  group_by(Continent) %>%
  summarise(mean=mean(SP.DYN.LE00.IN),
            std_dev=sd(SP.DYN.LE00.IN))
Summary(sum_dat)


# get descriptive statistics BEFORE imputed
describe(dat) %>% flextable()
# get descriptive statistics AFTER imputed
describe(dat_c) %>% flextable()










