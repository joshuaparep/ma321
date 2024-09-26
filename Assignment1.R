#Assignment Mell Pull ok changes made
library("VIM")
library("dplyr")
library("tidyverse")
library("corrplot")
library("mice")
library("ggplot2")
library("VIM")


################################################################################
####  Q1: STATISTICAL Descriptive ANALYSIS
################################################################################
# Load the dataset
df <- read.csv("house_data.csv")

attach(df)
head(df)  # Viewing the first few rows of the dataset
names(df) # Viewing the names of the variables
dim(df)   #An overview of the dataset's structure
view(df)
glimpse(df)
str(df)
summary(df) #Numerical summaries of the variables in the dataset
introduce(df) #Get more detail about row, columns and NAs
##check duplicate
duplicated(df)
df[duplicated(ddf),]
# Check unique values in var
sapply(df, function(x) length(unique(x)))

### Missing Value Analysis - check NAs
sum(is.na(df)) # 5,910 total NAs
df_naCols <- which(colSums(is.na(df))>0) # Identify variables with NAs
sort(colSums(sapply(df[df_naCols],is.na)), decreasing = TRUE) # 10 variables with NAs (Total NAs ordered in Dec)

################ GRAPHICAL DESCRIPTIVE ANALYSIS ###############################
# V1 - NA graphical description
plot_histogram(df) #Histogram for all the numberical variables

md.pattern(df) #

# V1 - NA graphical description
aggr(df[-1], prop = T, numbers = T, cex.axis=.5, cex.numbers = 0.1,
     ylab=c("Proportion of missingness","Missingness Pattern"),
     labels=names(df[-1]))

# V3 - NA graphical description 
aggr_plot <- aggr(df, col=c('navyblue','red'),
                  numbers=TRUE,
                  sortVars=TRUE,
                  labels=names(df),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("Histogram of Missing data","Pattern"))

#Replace the missing categorical variables with n/a as they imply that a house/propert has a missing trait.       
df[, !(names(df) %in% c("LotFrontage", "MasVnrArea"))][is.na(df[, !(names(df) %in% c("LotFrontage", "MasVnrArea"))])] <- "no"

############## FACTOR VARIABLEs - BOTHE CAT & NUM ##########################################
#comppleted_imp_df[] <- lapply(comppleted_imp_df, factor)
#factor_df <- comppleted_imp_df

#############################################################
#Group houses based on Overall condition
# Drop variables with 80% missing data (4 variables here have NA > 80%)
df1 <- subset(df, select = -c(PoolQC, MiscFeature, Alley, Fence))
# Also, deselecting irrelevant variables
df1 <- subset(df1, select = -c(Id, LowQualFinSF, PoolArea, MiscVal, MoSold))

unique(df$OverallCond)
#OverallCon btween 7-10 is classified as 1 (Good), btwn 4-6 is classifed as 2 (Average),
# between 1-3 is classified as 3 (Poor) condition
df1$OverallCond <- with(df1, ifelse(OverallCond <=3, "Poor", ifelse(OverallCond <=6, "Average", "Good")))

# Factor all categorical Variables variables
df1[sapply(df1, is.character)] <- lapply(df1[sapply(df1, is.character)], as.factor)

# Subset numerical variables that require factoring (9 num variabels require factoring here)
#num_fac <-  c("OverallQual", "FullBath", "BedroomAbvGr",
#                     "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces")
#
# Factor numerical variables
#sub_df[num_fac] <- lapply(sub_df[num_fac], factor)

# Select all the categorical variables
#cat_df <- unlist(lapply(sub_df, is.factor))
#cat_df <- sub_df[cat_df]

num_df <- unlist(lapply(sub_df, is.numeric))
num_df <- sub_df[num_df]
##############################################################################
################ HANDLING NAs - IMPUTAE REMAINING MISSING VALUES ##############
#impute NAs - In this case, the random forest mice function is used. Random m set to 5
imp_df <- mice(df1, seed = 123, m=5, method = "rf")
comppleted_imp_df <- complete(imp_df,3) # use 3rd cycle complete imputed dataset

summary(complete(comppleted_imp_df)) ##Summary for Descriptive Statistical Analysis

class(comppleted_imp_df)
unique(comppleted_imp_df$OverallCond)

# Check for any NA after imputation?
sapply(comppleted_imp_df, function(x) sum(is.na(x))) # good to go!
sum(is.na(comppleted_imp_df))
df0 <- comppleted_imp_df

########## CORRELATION ########################################################
#Correlation with 
numericVars <- which(sapply(df, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
all_numVar <- df[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")


# calulate the correlations in numerical variables
r <- cor(num_df, use="complete.obs")
round(r,2)
library(ggplot2)
install.packages("ggcorrplot")
library("ggcorrplot")
ggcorrplot(r)

ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)

#############################################################################
############################################
##### CHECKING AND HANDLING OUTLIERS #######
############################################
length(select_if(df1, is.numeric)) # 14 numberical variables
names(select_if(df1, is.numeric))

#install.packages("flextable")
library(flextable) # for beautifying tables
library(dlookr)    # for the main event of the evening
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

# Check outliers graphically for all the numerical variables 
# Since outliers in these numerical var contains important info, they are retained
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
comppleted_imp_df %>% select(MiscVal) %>%  plot_outlier()     t

###############################################################################
######  COLLINEARITy #
###############################################################################
#install.packages("DataExplorer")
library(DataExplorer)
plot_correlation(na.omit(comppleted_imp_df), maxcat = 5L)

df_scaled <- comppleted_imp_df %>% mutate_if(is.numeric, scale)

summary(df_scaled)



#############################################################################
###  QUESTION 2: Logistic Regression to Classifiy Overall House Condition
#############################################################################

###### SELECT NUMERICAL VARIABLES ########
num_df <- unlist(lapply(comppleted_imp_df, is.numeric))
num_df <- comppleted_imp_df[, num_df]

# Find 0's in num var
colSums(num_df == 0)

######### SUBSET CATEGORICAL VARIABLEA ########
cat_df <- unlist(lapply(comppleted_imp_df, is.factor))
cat_df <- comppleted_imp_df[, cat_df]
str(cat_df)



# TO DO - NEED TO SELECT FEATURES FIRST
count(comppleted_imp_df, OverallCond) #Check the classification distribution

############## Training Data ###############################
#create a list of random number ranging from 1 to number of rows from actual data 
#and 80% of the data into training data 

#Multinomial Logistic regression
library(nnet)
comppleted_imp_df$OverallConClass<-relevel(comppleted_imp_df$OverallConClass, ref="1")
mymodel<-multinom(OverallConClass~., data=comppleted_imp_df)
summary(mymodel)

#2-tailed z test
z <- summary(mymodel)$coefficients/summary(mymodel)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#d <- comppleted_imp_df
#mylogit <- glm(d$OverallConClass ~., data = d, family = "binomial")

summary(mylogit)
# Multinomial Logistic Regression - using mglogit
#install.packages("mlogit")
library("mlogit")
df3 <- mlogit.data(comppleted_imp_df, varying = NULL, choice = "OverallConClass", shape="wide")
head(df3)

model <- mlogit(df3$OverallConClass ~1, data = df3, reflevel = "1")
summary(model)


#############
xtabs(OverallCond ~., data=df0, sparse=TRUE)

xtabs(~ df0$OverallCond + df0$OverallQual, data=df0)

logistic <- glm(OverallCond ~., data=df0, family="binomial")
summary(logistic)


####STEPWISE###
base.mod <- glm(OverallCond ~ 1 , data= df0, family="binomial")  # base intercept only model
all.mod <- glm(OverallCond ~ . , data= df0, family="binomial") # full model with all predictors


############## Log Reg ##
index <- sample(nrow(df0),nrow(df0)*0.80)
credit_train = df0[index,]
credit_test = df0[-index,]


fullMod3 <- glm(OverallCond~., family="binomial", data=credit_train)

mod3 <- glm(OverallCond ~ Condition1 + YearBuilt + Exterior1st + BsmtQual + GrLivArea +
              Functional + GarageArea + SalePrice + SaleCondition + SaleType + PavedDrive +
              Fireplaces + GrLivArea + TotalBsmtSF + BldgType, family="binomial", data=credit_train)


summary(fullMod3) # AIC = 952.37.5
summary(mod3)     # AIC = 983.53



# Select all the categorical variables
cat <- unlist(lapply(df0, is.factor))
cat <- df0[cat]

xtabs(formula = ~., data = cat, subset, sparse = FALSE,
      na.action, addNA = FALSE, exclude = if(!addNA) c(NA, NaN),
      drop.unused.levels = FALSE)



#############################################################################
###  QUESTION 3: Predicting House Prices
#############################################################################
# FEATURE SELECTION - for Overall Condition rating classification
fullMod1 <- glm(OverallCond~., family="binomial", data=df0)

mod1 <- glm(OverallCond ~ Condition1 + HouseStyle + YearBuilt + Exterior1st + Exterior1st +
                  MasVnrArea + Foundation + TotalBsmtSF + GrLivArea + Functional +GarageArea  +
                  YrSold + SaleType + SalePrice, family="binomial", data=df0)

# Backwards selection is the default
step1 = step(fullMod) 
### Fianl MOdel from step - AIC  1083.33
stepMod1 <- glm(OverallCond ~ Street + YearBuilt + MasVnrArea + ExterCond + Foundation + 
                             BsmtQual + BsmtCond + TotalBsmtSF + X1stFlrSF + X2ndFlrSF + 
                             FullBath + BedroomAbvGr + KitchenQual + Fireplaces + GarageArea + 
                             GarageCond + YrSold + SalePrice,  family="binomial", data=df0)

summary(fullMod1) # AIC 1, 165.9
summary(mod1)    #AIC 1, 203.9
summary(step1)   #AIC  1, 083.33
summary(stepMod1)#AIC  1, 083


#############################################################################
###  QUESTION 4: Research Question in Relation to House Data
#############################################################################
###
#FEATURE SELECTION 2 - for SalePrice prediction
##

fullMod2 <- lm(df0$SalePrice~., family="binomial", data=df0)
summary(fullMod2)
mod2 <- lm(SalePrice ~ LotArea + Street + LotConfig + Neighborhood + Condition1 + Condition2 +
             BldgType + HouseStyle + OverallCond + RoofMatl + MasVnrArea + ExterQual + BsmtQual +
             TotalBsmtSF +GrLivArea + BedroomAbvGr + KitchenAbvGr + KitchenQual + Functional + Fireplaces +
             GarageArea + SaleType + SaleCondition, family="binomial", data=df0)

# Backwards selection is the default
step2 = step(fullMod2) 
### Fianl MOdel from step - AIC  1083.33
stepMod2 <- lm(df0$SalePrice ~ LotArea + Street + LotConfig + Neighborhood + 
                  Condition1 + Condition2 + BldgType + HouseStyle + OverallQual + 
                  OverallCond + YearBuilt + RoofMatl + MasVnrArea + ExterQual + 
                  BsmtQual + BsmtCond + TotalBsmtSF + X2ndFlrSF + GrLivArea + 
                  FullBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + Functional + 
                  Fireplaces + GarageArea + SaleType + SaleCondition,family="binomial", data=df0 )

summary(fullMod2) # A-R^2 = 0.8982 
summary(mod2)    # A-R^2 = 0.8902 
summary(step2)   # A-R^2 = 0.8985   AIC = 29710.05
summary(stepMod2)# A-R^2 = 0.8985 


summary(df$SalePrice)
ggplot(df,aes(x = SalePrice) )+ geom_histogram(bins = 30, fill= 'blue')

#install.packages("gridExtra")
library("gridExtra")

head(df)



#########################################################
library(corrplot)
data.corr <- as.data.frame(sapply(df, as.numeric))

correlations = cor(data.corr, method = "s")
# Show variables that have strong correlations with price, focus on coefficient > 0.5 or < -0.5
corr.price = as.matrix(sort(correlations[,'SalePrice'], decreasing = TRUE))
corr.id = names(which(apply(corr.price, 1, function(x) (x > 0.05 | x < -0.50))))
corrplot(as.matrix(correlations[corr.id,corr.id]), type = 'upper', method='color', addCoef.col = 'black', tl.cex = 1,cl.cex = 1, number.cex=1)


sapply(df, function(x) length(unique(x)))


