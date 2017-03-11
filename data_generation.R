setwd("/Users/desert/hs616")

#install.packages("corrgram")
library(corrgram)

#1. At the top of your file set the random seed to 101 by pasting with this statement: 
set.seed(101)

#2. Create a function header with parameter N for a function named generateDataset.
# The purpose of this function is to simulate 8 predictor variables (described below) 
# and their relationship to baby IQ. You will incorporate the predictor variables
# into a data frame along with a ninth column for a composite variable (DBF) and 
# a tenth column for response variable IQ. All variables are numeric. You will develop 
# the function incrementally as you code the numbered problems below. 
# Start by initializing each variable to zero within the function body. 
### Parent Characteristics ###
#MA # Maternal Age at time of pregnancy (yrs) 
#PSS# Social_Status of parents
#BE # Breadwinners_Education
#CC # Smoking status
#NP # Mother's No. of pregnancies

### Infant Characteristics ###
#GA # Estimated gestational age(GA) (wk)
# BW # Birth weight(BW) (g)
# BL # # Birth height(BL) (cm)
# DBF # Composite variable: data on baby and family characteristics
# IQ # BabyIQ
generateDataset <- function(N) {
  MA <- 0
  PSS <- 0
  BE <- 0
  CC <- 0
  NP <- 0
  #infant
  GA <- 0
  BW <- 0
  BL <- 0
  DBF <- 0
  IQ <- 0
  data.frame(MA,PSS,BE,CC,NP,GA,BW,BL,DBF,IQ)
}
generateDataset(10) # Testing output

#3 through 13
generateDataset <- function(N) {
  MA <-rnorm(n=N,mean = 29.3,sd = 1.9)
  PSS <- rnorm(n=N,mean = 4.9,sd = 1.9)
  BE <- runif(n = N,min = (2.6-0.8), max = (2.6+0.8))
  is_smoker <- rbern(n = N,p = 0.4)
  smoke <- runif(n = N,min = (3.7-1.2),max = (3.7+1.2))
  CC <- ifelse(is_smoker > 0, smoke[is_smoker], 0)
  NP<- rpois(n = N, lambda = 2)
  alt_NP <- rnorm(n = N,mean = 2,sd = sqrt(2))
  #infant
  GA <- runif(n = N,min = (39.2-2.0),max = (39.2+2.0) )
  BW <- runif(n = N, min = (3251-562), max = (3251+562))
  BL <- runif(n = N, min = (51.1-2.6), max = (51.1+2.6))
  MA[1] <- max(MA) + 6
  PSS[1] <- max(PSS) + 2 
  BE[1] <- max(BE) + 2 
  CC[1] <- 0
  DBF <- 10^(-0.3) * (MA) + 10^(-1) * (PSS) +
    10^(-1.2) * (BE) +
    -10^(-0.4) * (CC) - 7
  IQ <- 20*DBF - DBF^2 + rnorm(N, sd=2)
  data.frame(MA,PSS,BE,CC,NP,GA,BW,BL,DBF,IQ)
}

#14
df <- generateDataset(20)
summary(df)

#15.Produce pairwise plots of all the variables and also pairwise correlations. 
# List one pair that is positively correlated (how strongly?), 
# one that is negatively correlated (how strongly?), 
# and one pair that is basically not correlated.
corrgram(df, lower.panel=panel.conf, upper.panel=panel.pts, diag.panel=panel.density)
# MA and DBF - positively correlated (0.85)
# BE and IQ - negatively correlated (-0.30)
# GA and BW - basically not correlated. (0.00)

#16. Fit a linear model with IQ ~ DBF. 
# Display summary and the Residual vs Fitted plot of the linear model.
model <- lm(IQ ~ DBF, data=df)
summary(model)
plot(model, 1)

# 17. Fit a linear model with IQ ~ DBF to the dataset with the outlier 
# (first row) removed. Do not mutate the original data.
# Display summary and the Residual vs Fitted plot of the linear model.
# Write a couple of sentences comparing the Adjusted R-squared and the 
# plots of the two fitted models. Are the residuals normally distributed?
model2 <-  lm(IQ ~ DBF, data=tail(x = df,n = -1))
summary(model2)
plot(model2, 1)
# In the orignal model, which contained outliers, we recieved an 
# Adjusted R-squared of 0.1899. In the second model, which removed outliers,
# we recieved an  Adjusted R-squared of 0.823. This suggests our second model
# has less residual error than the original. This is reiterated by the 
# residual plots. In the orignal model we have a median residual of 1.430, and they
# a range of -13.643 to 6.088.  In the second model we have a median residual of 0.3903,
# and they a range of -5.4654 to 3.6582.  Ultimately, this means we are better able to
# predict IQ using DBF after excluding outliers.Residuals are not normally distibuted in the orignal model.
# Residuals are more or less normally distributed in model2.

# Below validates our previous statements re: the normality of residuals
shapiro.test(df$DBF) # Deviations from normality are statistically significant
shapiro.test(tail(df$DBF, -1)) # Cannot reject data comes from a normal distribution

# Plots also suppor our first statements
plot_compare <- function(dat1, dat2){
  par(mfrow=c(1, 2))
  d1 <- density(dat1)
  d2 <- density(dat2)
  plot(d1, type="n")
  polygon(d1, col="red", border="gray")
  plot(d2, type="n")
  polygon(d2, col="blue", border="gray")
  
}
plot_compare(residuals(model),residuals(model2)) # reiterates our first statements
plot(model,2) # reiterates our first statements
plot(model2,2)
plot(model,1) # reiterates our first statements
plot(model2,1)
par(mfrow=c(1, 1))


# 18. Fit a linear model with IQ as a function of DBF plus one more predictor
# variable of your choosing. Use all rows of the dataset, including the outlier.
# Display summary and the Residual vs Fitted plot of the linear model. 
# Write a sentence or two on your findings.
model3 <- lm(IQ ~ DBF + BE, data=df)
summary(model3)
plot(model3,1)
# In the this model, which contained outliers, we recieved an 
# Adjusted R-squared:  0.5108 which suggests it has less residual error than the original.
# There is also no patterns present in our Residual vs Fitted plot which suggests
#  residuals are normally distributed, and our assumptions are met.

