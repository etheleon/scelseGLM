## Linear Model

#set the working directory & read the data
setwd("/Users/ziggy/Documents/R/SCELSE")  #enter your path to the data file

#Import the data from a tab delimited ascii file
Birds <- read.table(file = "loyn.txt",
                    header = TRUE,
                    dec = ".")
#dec = '.'   means that the point is used for decimals.

########################################################################
# Underlying question and task
# The variable ABUND is the density of birds in 56 forest patches. 
# The explanatory variables are size of the forest patches (AREA), 
# distance to the nearest forest patch (DIST), distance to the 
# nearest larger forest patch (LDIST), year of isolation of the 
# patch (YR.ISOL), agricultural grazing intensity at each patch (GRAZE) 
# and altitude (ALT). The underlying aim of the research is to find 
# a relationship between bird densities and the explanatory variables. 
 
########################################################################


#First - Data Exploration (look at the data)

#Install / Load packages from R 

#install.packages("lattice") #OR in R Studio: Packages -> Install

library(lattice)  #For fancy multipanel graphs

########################################################################
#Inspect the file
#What do we have?
names(Birds)

#[1] "Site"    "ABUND"   "AREA"    "DIST"
#    "LDIST"   "YR.ISOL" "GRAZE"
#[8] "ALT"

str(Birds)  #Make sure that ABUND and AREA are num  and not factors!!!!!!
#If they are factors then change the dec argument!
#'data.frame':   56 obs. of  8 variables:
# $ Site   : int  1 2 3 4 5 6 7 8 9 10 ...
# $ ABUND  : num  5.3 2 1.5 17.1 13.8 14.1 3.8 2.2 3.3 3 ...
# $ AREA   : num  0.1 0.5 0.5 1 1 1 1 1 1 1 ...
# $ DIST   : int  39 234 104 66 246 234 467 284 156 311 ...
# $ LDIST  : int  39 234 311 66 246 285 467 1829 156 571 ...
# $ YR.ISOL: int  1968 1920 1900 1966 1918 1965 1955 1920 1965 1900 ...
# $ GRAZE  : int  2 5 5 3 5 3 5 5 4 5 ...
# $ ALT    : int  160 60 140 160 140 130 90 60 130 130 ...
########################################################################



########################################################################
#Data exploration
#A Outliers in Y / Outliers in X
#B Collinearity X
#C Relationships Y vs X
#D Spatial/temporal aspects of sampling design (not relevant here)
#E Interactions (is the quality of the data good enough to include them?)
#F Zero inflation Y
#G Are categorical covariates balanced?



###################################################
#First some elementary R commands.
#1 How do you acces variables in an object like Birds?
Birds            #All data
head(Birds)      #First 6 rows
head(Birds, 10)  #First 10 rows
Birds$ABUND      #ABUND variable
Birds[,1]        #First column
Birds[,2]        #Second coloumn
Birds[,"ABUND"]  #ABUND variable 
Birds[1,"ABUND"] #First row of ABUND variable
Birds[1:10,"ABUND"]  #First 10 rows of ABUND
c("ABUND", "AREA")   #Two characters concatenated
Birds[, c("ABUND", "AREA")] #ABUND and AREA variables

MyVar <- c("ABUND", "AREA")  #Same as last two steps
Birds[, MyVar]
##################################################

#A Outliers in Y

par(mfrow = c(1, 2))
boxplot(Birds$ABUND, 
        main = "Abundance")
dotchart(Birds$ABUND, 
         xlab = "Range of data", 
         ylab = "Values")

#A Outliers in X
par(mfrow = c(2, 3), mar = c(4, 3, 3, 2))
dotchart(Birds$AREA, main = "Area")
dotchart(Birds$DIST, main = "Distance")
dotchart(Birds$LDIST, main = "Distance to larger patch")
dotchart(Birds$YR.ISOL, main = "Year of isloation")
dotchart(Birds$ALT, main = "Altitude")
dotchart(Birds$GRAZE, main = "Grazing levels")

#Apply transformations
Birds$LOGAREA  <- log10(Birds$AREA)
Birds$LOGDIST  <- log10(Birds$DIST)
Birds$LOGLDIST <- log10(Birds$LDIST)


#B Collinearity X
pairs(Birds[,c("LOGAREA","LOGDIST","LOGLDIST",
               "YR.ISOL","ALT","GRAZE")])

## Or
MyVar <- c("LOGAREA","LOGDIST","LOGLDIST",
           "YR.ISOL","ALT","GRAZE")
pairs(Birds[, MyVar])


#How do you detect collinearity between a continuous covariate
#and a categorical? Make a conditional boxplot
boxplot(YR.ISOL ~ factor(GRAZE), 
        data = Birds,
        ylab = "Year of isolation",
        xlab = "Grazing intensity")

boxplot(LOGAREA ~ factor(GRAZE), 
        data = Birds)


#C. Relationships Y vs X
MyVar <- c("ABUND","LOGAREA","LOGDIST","LOGLDIST",
           "YR.ISOL","ALT","GRAZE")
pairs(Birds[, MyVar])

boxplot(ABUND ~ factor(GRAZE), 
        data = Birds,
        varwidth = TRUE,
        ylab = "Bird abundance",
        xlab = "Grazing levels",
        main = "Grazing")


#E Interactions
coplot(ABUND ~ LOGAREA | factor(GRAZE),
       data = Birds)

coplot(ABUND ~ LOGAREA | factor(GRAZE),
       data = Birds,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })


#F. Zero inflation
sum(Birds$ABUND == 0)
100 * sum(Birds$ABUND == 0) / nrow(Birds)


#G. Are categorical covariates balanced? Works on Factors only (categorical)
table(Birds$GRAZE)


###############################################################################

###############################################################################
#Analysis

M1 <- lm(ABUND ~ LOGAREA, data = Birds)

#What is the model that we are fitting?
# ABUND_i = alpha + beta * LOGAREA_i + eps_i
# eps_i ~ N(0, sigma^2)

# where i = 1,...,56
# ABUND_i is the abundance at site i
summary(M1) 


# What is the fitted model
# E(ABUND_i) = mu_i = 10.40 + 9.77 * LOGAREA_i 
# eps ~ N(0,  7.28^2)



#Is everything significant?
summary(M1)

#ABUND = alpha + beta * LOGAREA + eps

#Interpretation of F and t values 
#H0: beta = 0
#H1: beta <> 0

#F_1,54 = 65.38 (p<0.001)
#Or:
#t_n-1    t_55 = 8.08 (p < 0.001)
#Text in paper: A t-value indicated a significant effect (t = 8.08; df = 55, p < 0.001)


################################################
#Model validation (independence is most important)
#1. Homogeneity
#2. Independence

#Homogeneity
#E1 <- resid(M1)   #or better: 
E1 <- rstandard(M1) #which are corrected by leverage
F1 <- fitted(M1)

plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Residuals", 
     main = "Homogeneity?")
abline(h = 0, v = 0, lty = 2)


#Independence: Dependence due to model misfit
#Plot residuals versus covariates
plot(x = Birds$LOGAREA, 
     y = E1)
abline(0,0,lty=2)
#if you dont see a linear pattern, cannot do linear


#But plot residuals also versus covariates NOT in the model!!!!!!!
boxplot(E1 ~ factor(Birds$GRAZE)) #here there is a grazing effect on residuals - means grazing should have been fitted in model
abline(h = 0, lty = 2)

plot(y = E1, 
     x = Birds$YR.ISOL, 
     col = Birds$GRAZE, 
     pch =16)
abline(h = 0, lty = 2)
# most are OK except YR.ISOL - there is a pattern there, so should be included in model
# in fact, grazing and years since isolation were colinear (see Ex1)

#and also for all the other covariates!!!
#...
#...



#Normality
hist(E1, main = "Normality", breaks=10)
#Or qq-plot
qqnorm(E1)
#qqline(E1)


###############################################################################
#Plot

range(Birds$LOGAREA)
#-1.000000  3.248219
#Create a data frame that contains x numbers of LOGAREA values
MyData <- data.frame(LOGAREA = seq(from= -1,
                                   to = 3.25,
                                   length = 25))

P1 <- predict(M1, newdata = MyData)
plot(x = Birds$LOGAREA, 
     y = Birds$ABUND,
     xlab = "Log transformed area",
     ylab = "Bird abundance")

lines(x = MyData$LOGAREA, 
      y = P1, 
      lwd = 3, 
      lty = 1,
      col = 1)


###################################################################
#Below is some advanced stuff to add confidence bands.

PP1 <- predict(M1, newdata = MyData, se = TRUE, interval = "confidence")

SeConf.Up  <- PP1$fit[,3]
SeConf.Low <- PP1$fit[,2]

plot(x = Birds$LOGAREA, 
     y = Birds$ABUND,
     xlab = "Log transformed area",
     ylab = "Bird abundance")

lines(x = MyData$LOGAREA, 
      y = PP1$fit[,1], 
      lwd = 3, 
      lty = 1,
      col = 1)


polygon(c(MyData$LOGAREA, rev(MyData$LOGAREA)),
        c(SeConf.Low, rev(SeConf.Up)),
        col = 2, border = NULL,
        density = 50)


#Useful for later...when we have multiple lines
legend("topleft",
       legend=c("Line 1"),
       col = c(1),
       lty = c(1),
       lwd = c(1))


########################################################################
#Now, model ABUND as a function of a categorical covariate (factor Graze)

M2 <- lm(ABUND ~ factor(GRAZE), data=Birds)
summary(M2)
anova(M2)




#Apply model as a function of all covariates
#But no interactions

M3 <- lm(ABUND ~ LOGAREA + LOGDIST + LOGLDIST +
           YR.ISOL + ALT + factor(GRAZE),
         data = Birds)


#
#Is everything significant?
summary(M3)
drop1(M3, test = "F")



#Now, as a function of the interaction between a continuous and a categorical covariate
M4 <- lm(ABUND ~ LOGAREA * factor(GRAZE), #this is = to LOGAREA + factor(GRAZE) + LOGAREA : factor(GRAZE)
         data = Birds)

summary(M4)
drop1(M4, test = "F") #test for the interaction - if no-significant, remove and test again

M5 <- lm(ABUND ~ LOGAREA + factor(GRAZE), data = Birds)
summary(M5)
drop1(M5, test = "F")


