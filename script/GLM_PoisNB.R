# GLM Poisson and Negative Binomial (overdispersed Poisson)

#######################################################################
#set the working directory & read the data
#On a Windows computer: adjust blah blah blah
#setwd("c:/blah blah blah")

#For a Mac:
setwd("/Users/ziggy/Documents/R/SCELSE")


#Import the data from a tab delimited ascii file
Benthos <- read.table(file = "infauna.txt", 
                      header = TRUE,
                      dec = ".")
#dec = '.'   means that the point is used for decimals. 
#Change to   dec = ","   if required.
########################################################################




########################################################################
#To see what is in the object Benthos, type:
names(Benthos)
str(Benthos)

#[1] "Period"       "Fishing"      "OrganicM"     "Mud"         
#[5] "Silt"         "Clay"         "Ampeliscidae" "Cirratulidae"

# 'data.frame':	80 obs. of  8 variables:
 # $ Period      : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ Fishing     : Factor w/ 2 levels "no","yes": 1 1 1 1 1 2 2 2 2 2 ...
 # $ OrganicM    : num  1.16 1.38 1.84 1.62 1.62 2.13 1.78 1.87 1.56 1.15 ...
 # $ Mud         : num  35.3 34.5 38.4 30.7 32.6 ...
 # $ Silt        : num  25.1 21.9 23.5 17.7 23.3 ...
 # $ Clay        : num  10.16 12.52 14.94 12.99 9.27 ...
 # $ Ampeliscidae: int  7 4 7 3 4 2 2 0 2 3 ...
 # $ Cirratulidae: int  29 52 68 43 41 22 11 7 5 4 ...
##########################################################################





########################################################################
#Aim of the analysis:
#Model Ampeliscidae as a function of the covariates:  
#      Period, Fishing, OrganicM, Mud, Silt, Clay,
#      with interaction between Fishing and OrganicM

#where
#Fishing:     Fishing vs no fishing
#Period:      Three time periods
########################################################################




########################################################################
#Housekeeping
#Converting Period and Fishing into factors
Benthos$fPeriod <- factor(Benthos$Period)
 
Benthos$fFishing <- factor(Benthos$Fishing, 
                           levels = c("no", "yes"),
                           labels = c("No Fishing", "Fishing"))
########################################################################





########################################################################
#Housekeeping
#Load packages from R 
library(lattice)  

########################################################################






########################################################################
#DATA EXPLORATION: 
#Outliers: Y and X
#Collinearity X
#Relationships  Y vs X    but also interactions
#Zero inflation: Make a frequency plot


#Are there any outliers?
  #Outliers in the response variable?
  #Outliers in the explanatory variables?
   

MyVar <- c("Ampeliscidae", "OrganicM", "Silt", "Clay", "Mud")

dotplot(as.matrix(Benthos[,MyVar]), groups=FALSE,
        strip = strip.custom(bg = 'white',
        par.strip.text = list(cex = 1.5)),
        scales = list(x = list(relation = "free", draw = TRUE),
                      y = list(relation = "free", draw = FALSE)),
        col=1, cex  =0.5, pch = 16,
        xlab = list(label = "Value of the variable", cex = 1.5),
        ylab = list(label = "Order of the data from text file", cex = 1.5))


#great because response var ranges till 15 (<20-25), Poisson may be good

#Numer of zeros in the response variable
table(Benthos$Ampeliscidae)
plot(table(Benthos$Ampeliscidae), type = "h")  
100 * sum(Benthos$Ampeliscidae == 0) / nrow(Benthos) #calculate % of zeros in data
#Not a problem


 
#What about categorical covariates?
#Do we have a reasonable number of observations per level of a categorical covariate?
with(Benthos, table(fFishing)) #with the object "Benthos", give me a table
with(Benthos, table(fPeriod))

with(Benthos, table(fPeriod,fFishing)) #if I was interested in their interaction
#Nice, more or less balanced design
 
 
#Collinearity
pairs(Benthos[, c("OrganicM", "Mud", "Silt", "Clay")])
cor(Benthos[, c("OrganicM", "Mud", "Silt", "Clay")]) #gives you a correlation matrix
#Too much collinearity!

par(mfrow = c(1, 2))
boxplot(OrganicM ~ fPeriod, 
        xlab = "Period",
        data = Benthos, 
        main = "OrganicM")
boxplot(OrganicM ~ fFishing, 
        xlab = "Fishing", 
        data = Benthos, 
        main = "OrganicM")

par(mfrow = c(1, 2))
boxplot(Mud ~ fPeriod, 
        xlab = "Period", 
        data = Benthos, 
        main = "Mud")
boxplot(Mud ~ fFishing, 
        xlab = "Fishing",
        data = Benthos, 
        main = "Mud")    #collinearity between Mud and Fishing - wont be able to say if its because of 1 or the other

par(mfrow = c(1, 2))
boxplot(Silt ~ fPeriod, data = Benthos, main = "Silt")
boxplot(Silt ~ fFishing, data = Benthos, main = "Silt")

par(mfrow = c(1, 2))
boxplot(Clay ~ fPeriod, data = Benthos, main = "Clay")
boxplot(Clay ~ fFishing, data = Benthos, main = "Clay")


#Either use mud  or   silt and clay.
#Fishing seems to be collinear with some of these.
#If you use  mud or clay you cannot use fishing as it is collinear!



#Interactions?
#Based on biology we would expect that Ampeliscidae vs O_Material
#changes depending on the dredging effect (CT)!!!

coplot(Ampeliscidae ~ OrganicM | fFishing,
       data = Benthos,
        panel = function(x, y, ...) {
          tmp <- lm(y ~ x, na.action = na.omit)
          abline(tmp)
          points(x, y) })
             
#There is indication for interaction!!
################################################### 






####################################################
#Start analysis

#Fit the model E(Ampeliscidae ) = mu = exp(alpha + A + B + A:B + C )
M1 <- glm(Ampeliscidae ~ OrganicM * fFishing + fPeriod, 
          data = Benthos,
          family = poisson)
         
#Exactly the same model
M1 <- glm(Ampeliscidae ~ OrganicM + fFishing + 
                         OrganicM : fFishing + 
                         fPeriod, 
          data = Benthos,
          family = poisson)


#What is the model that we are fitting?
#Ampeliscidae_i ~ P(mu_i)
#E(Ampeliscidae_i) = mu_i   
#and   var(Ampeliscidae_i) = mu_i
#
#log(mu_i) = alpha + OrganicM_i + fFishing_i + 
#            Period_i + OrganicM_i x fFishing_i
           

# summary(M1)
# drop1(M1, test = "Chi")
# but before this check for overdispersion

#Pearson residuals:
#  (Y - E(y))          (Y - mu)
# --------------  = -----------------
#  sqrt(var(Y))         sqrt(mu)

#mu = exp(blah blah blah)

#Is the model overdispersed?        
E1 <- resid(M1, type = "pearson")
N  <- nrow(Benthos)
p  <- length(coef(M1))
sum(E1^2) / (N - p)
#Just ok!


summary(M1)
drop1(M1, test="Chi")
#Everything significant. Nothing can be droped


#Suppose that the interaction is not significant:
#Mtest <- glm(Ampeliscidae ~ OrganicM + fFishing  + fPeriod, 
#          data = Benthos,
#          family = poisson)
#summary(Mtest)
#drop1(Mtest, test =  "Chi")
############################################




############################################
#Model validation
#Plot residuals vs fitted values
#Influential observations
#Plot residuals vs each covariate (in the model, 
#and not in the model)


F1 <- fitted(M1) #command fitted gives already e^model
E1 <- resid(M1, type = "pearson")
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

#Cook's distance: influential obs
par(mfrow = c(1, 1))
plot(M1, which = 4)


#Plot Pearson residuals versus each covariate
plot(x = Benthos$OrganicM, 
     y = E1)
abline(h = 0, lty = 2)

#do the same for other covariates not in model 
plot(x = Benthos$Clay, 
     y = E1)
abline(h = 0, lty = 2)

plot(x = Benthos$Mud, 
     y = E1)
abline(h = 0, lty = 2)

plot(x = Benthos$Silt, 
     y = E1)
abline(h = 0, lty = 2)

boxplot(E1 ~ fPeriod, data = Benthos) 
boxplot(E1 ~ fFishing, data = Benthos) 
#Looks all ok





###################################################
#Model interpretation
#Sketch fitted values  for the GLM Poisson model
M1 <- glm(Ampeliscidae ~ OrganicM * fFishing + fPeriod, 
          data = Benthos,
          family = poisson)

summary(M1)

#                         Estimate Std. Error z value Pr(>|z|)    
#(Intercept)               -0.1766     0.5064  -0.349   0.7272    
#OrganicM                   0.6578     0.3227   2.039   0.0415 *  
#fFishingFishing            1.3694     0.6557   2.088   0.0368 *  
#fPeriod2                   1.1919     0.1792   6.652 2.89e-11 ***
#fPeriod3                   1.4371     0.1758   8.177 2.92e-16 ***
#OrganicM:fFishingFishing  -1.1314     0.4485  -2.523   0.0116 *  

#A. What is the model that we are fitting?

#Ampeliscidae_i ~ Poisson(mu_i)
#E(Ampeliscidae_i)   = mu_i
#var(Ampeliscidae_i) = mu_i

#         alpha + OrganicM + Fishing + Period + OrganicM:Fishing  
#mu_i = e

#B. What are the equations for each period/Fishing combination?

#What is the equation for Period = 1 & Fishing = Non Fishing
#         -0.1766 + 0.657 * OrganicM_i  
#mu_i = e

#What is the equation for Period = 2 & CT = Non-Fishing
#         -0.1766 +  1.1919 + 0.657 * OrganicM_i  
#mu_i = e


#What is the equation for Period = 3 & CT = Non-Fishing
#         -0.1766 +  1.4371 + 0.657 * OrganicM_i  
#mu_i = e


#What is the equation for Period = 1 & CT = Fishing
#         -0.1766 + 1.3694 + (0.657 -1.1314) * OrganicM_i  
#mu_i = e


#What is the equation for Period = 2 & CT = Fishing
#         -0.1766 + 1.3694 + 1.19 + (0.657 -1.1314) * OrganicM_i  
#mu_i = e


#Sketch them in a graph
# range(Benthos$OrganicM)

MyData1 <- data.frame(OrganicM =
                       seq(from = min(Benthos$OrganicM),
                           to = max(Benthos$OrganicM),
                           length=10),
                      fPeriod = "1",
                      fFishing = "No Fishing")
                      
 MyData2 <- data.frame(OrganicM =
                       seq(from = min(Benthos$OrganicM),
                           to = max(Benthos$OrganicM),
                           length=10),
                      fPeriod = "2",
                      fFishing = "No Fishing")

MyData3 <- data.frame(OrganicM =
                       seq(from = min(Benthos$OrganicM),
                           to = max(Benthos$OrganicM),
                           length=10),
                      fPeriod = "3",
                      fFishing = "No Fishing")
    

 MyData4 <- data.frame(OrganicM =
                       seq(from = min(Benthos$OrganicM),
                           to = max(Benthos$OrganicM),
                           length=10),
                      fPeriod = "1",
                      fFishing = "Fishing")
                      
 MyData5 <- data.frame(OrganicM =
                       seq(from = min(Benthos$OrganicM),
                           to = max(Benthos$OrganicM),
                           length=10),
                      fPeriod = "2",
                      fFishing = "Fishing")

MyData6 <- data.frame(OrganicM =
                       seq(from = min(Benthos$OrganicM),
                           to = max(Benthos$OrganicM),
                           length=10),
                      fPeriod = "3",
                      fFishing = "Fishing")
    
P1 <- predict(M1, newdata = MyData1, type = "response")
P2 <- predict(M1, newdata = MyData2, type = "response")
P3 <- predict(M1, newdata = MyData3, type = "response")
P4 <- predict(M1, newdata = MyData4, type = "response")
P5 <- predict(M1, newdata = MyData5, type = "response")
P6 <- predict(M1, newdata = MyData6, type = "response")


plot(x = Benthos$OrganicM , 
     y = Benthos$Ampeliscidae,
     ylab = "Ampeliscidae",
     xlab = "Organic Matter",
     pch = 16,
     col = as.numeric(Benthos$fFishing))

lines(MyData1$OrganicM, P1, lty=1,col=1,lwd=1)
lines(MyData2$OrganicM, P2, lty=1,col=1,lwd=1)
lines(MyData3$OrganicM, P3, lty=1,col=1,lwd=1)
lines(MyData4$OrganicM, P4, lty=3,col=2,lwd=1)
lines(MyData5$OrganicM, P5, lty=3,col=2,lwd=1)
lines(MyData6$OrganicM, P6, lty=3,col=2,lwd=1)

legend("topleft",cex = 0.5,
        legend=c("NF1","NF2","NF3","F1","F2","F3"),
        col = c(1,1,1,2,2,2),
        lty = c(1,1,1,3,3,3),
        lwd = c(1,1,1,1,1,1))
        
#Want to have CIs?
P1 <- predict(M1, newdata = MyData1, type = "link", se = TRUE)
P2 <- predict(M1, newdata = MyData2, type = "link", se = TRUE)
P3 <- predict(M1, newdata = MyData3, type = "link", se = TRUE)
P4 <- predict(M1, newdata = MyData4, type = "link", se = TRUE)
P5 <- predict(M1, newdata = MyData5, type = "link", se = TRUE)
P6 <- predict(M1, newdata = MyData6, type = "link", se = TRUE)

plot(x = Benthos$OrganicM , y = Benthos$Ampeliscidae,
     ylab = "Ampeliscidae",
     xlab = "Organic Matter")

lines(MyData1$OrganicM, exp(P1$fit), lty=1,col=1,lwd=1)
lines(MyData1$OrganicM, exp(P1$fit + 2*P1$se), lty=2,col=1,lwd=1)
lines(MyData1$OrganicM, exp(P1$fit - 2*P1$se), lty=2,col=1,lwd=1)


lines(MyData2$OrganicM, exp(P2$fit), lty=1,col=1,lwd=1)
lines(MyData1$OrganicM, exp(P2$fit + 2*P2$se), lty=2,col=1,lwd=1)
lines(MyData1$OrganicM, exp(P2$fit - 2*P2$se), lty=2,col=1,lwd=1)

#etc  etc etc


##############################################################
#Second species: Cirratulidae
#Similar data exploration steps
#Home work: do the data exploration


dotchart(Benthos$Cirratulidae)

#Start analysis
#Fit the model E[Y] = mu = exp(alpha + A + B + A:B + C )
M1 <- glm(Cirratulidae ~ OrganicM * fFishing + fPeriod,
          data = Benthos,
          family = poisson)

#Exactly the same model
M1 <- glm(Cirratulidae ~ OrganicM + fFishing +
                         OrganicM : fFishing +
                         fPeriod,
          data = Benthos,
          family = poisson)

#Fit the model
#What is the model that we are fitting?
# Cirratulidae_i ~ P(mu_i)
# E(Cirratulidae_i) = mu_i   and   var(Cirratulidae_i) = mu_i
#
#log(mu_i) = alpha + OrganicM_i + fFishing_i +
#            Period_i + OrganicM_i x fFishing_i


summary(M1)



#Is the model overdispersed?
E1 <- resid(M1, type = "pearson")
N  <- nrow(Benthos)
p  <- length(coef(M1)) 
sum(E1^2) / (N - p)
#[1] 17.12971
#overdispersed

#or
Overdispersion <- sum(E1^2) / M1$df.res
Overdispersion

#Why do you have overdispersion
#A. Outliers                  => Remove them..but subjective
#B. Missing covariates        => Add them
#C. Missing interactions      => Add them (coplot)
#D. Zero inflation            => ZIP/ZINB
#E. Dependency                => GLMM
#F. Non-linear relationships  => GAM
#G. Wrong link function       => Change it
#H. Variance is bigger than mean => NB



plot(y = E1, x = Benthos$OrganicM)
100 * sum(Benthos$Cirratulidae == 0)/ nrow(Benthos)



#Apply NB GLM
library(MASS)
M2 <- glm.nb(Cirratulidae ~ OrganicM + fFishing +
                         OrganicM : fFishing +
                         fPeriod,
             data = Benthos)

#by default it is the log link

E2 <- resid(M2, type = "pearson")
Overdispersion <- sum(E2^2) / M2$df.res
Overdispersion

#If not lucky....overdispersion > 1.5....
#then:
#Why do you have overdispersion
 #A. Outliers                  => Remove them..but subjective
 #B. Missing covariates        => Add them
 #C. Missing interactions      => Add them (coplot)
 #D. Zero inflation            => ZIP/ZINB
 #E. Dependency                => GLMM
 #F. Non-linear relationships  => GAM
 #G. Wrong link function       => Change it





#What is the model that we are fitting?

# Cirratulidae_i ~ NB(mu_i, k)
# E(Cirratulidae_i) = mu_i
# var(Cirratulidae_i) = mu_i + mu_i^2 / k

#log(mu_i) = alpha + OrganicM_i + fFishing_i +
#            Period_i + OrganicM_i x fFishing_i


print(summary(M2), digits = 2, signif.stars=FALSE)
drop1(M2, test = "Chi")

#the interaction is no longer significant

M3 <- glm.nb(Cirratulidae ~ OrganicM + fFishing+
                         fPeriod, link = "log",
             data =Benthos)

print(summary(M3), digits = 2, signif.stars=FALSE)
drop1(M3, test = "Chi")


#organic material is not significant
M4 <- glm.nb(Cirratulidae ~ fFishing +
                         fPeriod, link = "log",
             data =Benthos)

print(summary(M4), digits = 2, signif.stars=FALSE)
drop1(M4, test = "Chi")

#both fFishing and fPeriod are significant

#Coefficients:
#                Estimate Std. Error z value Pr(>|z|)
#(Intercept)         3.34       0.17    20.1   <2e-16
#fFishingFishing    -0.56       0.15    -3.8    1e-04
#fPeriod2            0.81       0.19     4.2    3e-05
#fPeriod3            0.82       0.19     4.3    2e-05

#NEXT:
#    Model validation
#    Model interpretation
###############################################


#Model validation
par(mfrow = c(2,2))
plot(M4)

#Or:
E4 <- resid(M4, type = "pearson")
F4 <- fitted(M4)
plot(x = F4, 
     y = E4,
     xlab = "Fitted values",
     ylab = "Residuals")

#Influential observations
plot(M4, which = c(4))

#or
par(mfrow=c(1,1))
plot(cooks.distance(M4),
     type = "h",
     ylim=c(0,1))
abline(h=1)



#residuals vs covariates
par(mfrow=c(1,2))
boxplot(E4 ~ fPeriod, data = Benthos)
boxplot(E4 ~ fFishing, data = Benthos)


#Model interpretation
summary(M4)
#Coefficients:
#                Estimate Std. Error z value Pr(>|z|)
#(Intercept)       3.3419     0.1666  20.061  < 2e-16 ***
#fFishingFishing  -0.5609     0.1471  -3.813 0.000137 ***
#fPeriod2          0.8083     0.1920   4.210 2.56e-05 ***
#fPeriod3          0.8250     0.1920   4.297 1.73e-05 ***

#Theta:  2.471

#Model interpretation:
# Model:
# Cirratulidae_i ~ NB(mu_i, 2.471)
# E(Cirratulidae_i) = mu_i
# var(Cirratulidae_i) = mu_i + mu_i^2 / 2.471
#
# Period 1 & NO_Fishing
# log(mu_i) = 3.34

#Period 2& NO_Fishing
# log(mu_i) = 3.34 +0.81

#Period 3& NO_Fishing
# log(mu_i) = 3.34 +0.83

# Period 1& Fishing
# log(mu_i) = 3.34 - 0.56

#Period 2& Fishing
# log(mu_i) = 3.34 - 0.56 + 0.81

#Period 3& Fishing
# log(mu_i) = 3.34 - 0.56 + 0.83



#Sketch model fit
MyData <- expand.grid(fPeriod = levels(Benthos$fPeriod),
                      fFishing = levels(Benthos$fFishing))
MyData

P <- predict(M4, newdata = MyData,
                    type = "link", se = TRUE)

MyData$Fit   <- exp(P$fit) #need to do exponential
MyData$SeUp  <- exp(P$fit + 2 * P$se.fit)
MyData$SeLow <- exp(P$fit - 2 * P$se.fit)








