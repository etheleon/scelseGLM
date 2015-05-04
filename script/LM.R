----
title: Linear Model
author: Wesley (Adapted from ziggy)
---

```{r echo=FALSE}
#set the working directory & read the data
#setwd("/Users/ziggy/Documents/R/SCELSE")  #enter your path to the data file
```

```{r dataframe}
#Import the data from a tab delimited ascii file
Birds <- read.table(file = "data/loyn.txt",
                    header = TRUE,
                    dec = ".")
#dec = '.'   means that the point is used for decimals.
```
## Changes
1. Switched to using ggplot2
2. formatted this into a Rmarkdown file.

## Underlying question and task

The variable **ABUND** is the density of birds in 56 forest patches.
The explanatory variables are size of the forest patches (AREA),
distance to the nearest forest patch (DIST), distance to the
nearest larger forest patch (LDIST), year of isolation of the
patch (YR.ISOL), agricultural grazing intensity at each patch (GRAZE)
and altitude (ALT). The underlying aim of the research is to find
a relationship between bird densities and the explanatory variables.


## Data Exploration

```{r echo=FALSE, message=FALSE, warning=FALSE}
#install.packages("lattice") #OR in R Studio: Packages -> Install
library(ggplot2)
library(gridExtra)
library(magrittr)
library(dplyr)
library(stargazer)
theme_set(theme_bw())
#library(lattice)  #For fancy multipanel graphs
```

Inspect the file. What do we have?
```{r}
names(Birds)
str(Birds)
```

### Questions to answer in the following

1. Outliers in Y / Outliers in X
2. Collinearity X
3. Relationships Y vs X
4. Spatial/temporal aspects of sampling design (not relevant here)
5. Interactions (is the quality of the data good enough to include them?)
6. Zero inflation Y
7. Are categorical covariates balanced?

```{r echo=TRUE, eval=FALSE}
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
```


#### 1 Outliers in Y
```{r}
qplot(x=as.factor(1), y=ABUND, data=Birds, geom=c("boxplot", "jitter"))
#A Outliers in X
c("AREA", "DIST", "LDIST", "YR.ISOL","ALT", "GRAZE" ) %>%
lapply(function(colName){
qplot(factor(1), Birds[,colName], geom="jitter")+xlab(colName)
                    }) %>%
do.call(grid.arrange, .)


```{r}
#Apply transformations
Birds %<>% mutate(LOGAREA  = log10(AREA))
Birds %<>% mutate(LOGDIST  = log10(DIST))
Birds %<>% mutate(LOGLDIST = log10(LDIST))
```

#### 2 Collinearity X
```{r collinear}
pairs(Birds[,c("LOGAREA","LOGDIST","LOGLDIST",
               "YR.ISOL","ALT","GRAZE")])

## Or
MyVar <- c("LOGAREA","LOGDIST","LOGLDIST",
           "YR.ISOL","ALT","GRAZE")
pairs(Birds[, MyVar])
```

```{r}
#How do you detect collinearity between a continuous covariate
#and a categorical? Make a conditional boxplot
p1 = qplot(factor(GRAZE), YR.ISOL, data=Birds, geom=c("boxplot", "jitter"))+
    labs(   x= "Year of isolation",
            y= "Grazing intensity")
p2 = qplot(factor(GRAZE), LOGAREA, data=Birds, geom=c("boxplot", "jitter"))+
    labs(   x= "Year of isolation",
            y= "Grazing intensity")
grid.arrange(p1, p2)
```

### 3 Relationships Y vs X
```{r}
MyVar = c("ABUND","LOGAREA","LOGDIST","LOGLDIST",
           "YR.ISOL","ALT","GRAZE")
pairs(Birds[, MyVar])

qplot(factor(GRAZE), ABUND, data=Birds, geom=c("boxplot", "jitter"))+labs(
        y = "Bird abundance",
        x = "Grazing levels",
        title = "Grazing")
```

### 5. Interactions
```{r}
p1 = qplot(ABUND, LOGAREA, data=Birds) + facet_wrap(~GRAZE)
p1
p1 + stat_smooth(method = "lm", formula = y ~ x, na.rm=T)
```

### 6. Zero inflation
```{r}
sum(Birds$ABUND == 0)
100 * sum(Birds$ABUND == 0) / nrow(Birds)
```

#### 7. Are categorical covariates balanced? Works on Factors only (categorical)
```{r}
```{r results='asis', echo=FALSE}
library(xtable)
xtable(
       table(Birds$GRAZE)
       )
```

```{r echo=TRUE}
M1 = lm(ABUND ~ LOGAREA, data = Birds)
```

    >??? Shouldnt look at the p-value in this case
    >Forgot why
    >when you have 2 covariates (continuous & binomial)

##### What is the model that we are fitting?

 ABUND_i = alpha + beta * LOGAREA_i + eps_i
 eps_i ~ N(0, sigma^2)
 where i = 1..`r nrow(Birds)`
 ABUND_i is the abundance at site i

##### What is the fitted model

 E(ABUND_i) = mu_i = 10.40 + 9.77 * LOGAREA_i 
 eps ~ N(0,  7.28^2)

##### Is everything significant?

```{r echo=FALSE, results='asis'}
stargazer(M1)
```

ABUND = alpha + beta * LOGAREA + eps

#### Interpretation of F and t values 

H0: beta = 0
H1: beta <> 0

F_1,54 = 65.38 (p<0.001)
Or:
t_n-1    t_55 = 8.08 (p < 0.001)
Text in paper: A t-value indicated a significant effect (t = 8.08; df = 55, p < 0.001)

### Model validation (independence is most important)

1. Homogeneity
2. Independence

#### A Homogeneity

```{r echo=TRUE}
#ziggy: best to calculate by hand (lme or lmer sometimes include or doesnt include)

E1 <- resid(M1)
#Better: corrected by leverage
E1 <- rstandard(M1) 
F1 <- fitted(M1)

plot(x = F1,
     y = E1,
     xlab = "Fitted values",
     ylab = "Residuals", 
     main = "Homogeneity?")
abline(h = 0, v = 0, lty = 2)
```

#### B Independence

Dependence due to model misfit.
Check by plotting residuals versus covariates

```{r}
par(mfrow=c(2,2))
plot(M1)
```

But plot residuals also versus covariates NOT in the model!!!!!!!

```{r}
plot(x = Birds$LOGAREA,
     y = E1)
abline(0,0,lty=2)
#if you dont see a linear pattern, cannot do linear
```

```{r}
boxplot(E1 ~ factor(Birds$GRAZE)) #here there is a grazing effect on residuals - means grazing should have been fitted in model
abline(h = 0, lty = 2)

qplot(
     x = YR.ISOL,
     y = E1, 
     color = factor(GRAZE),
     data=Birds
     )+scale_color_brewer("Graze", type="qual")+xlab("Year")

#abline(h = 0, lty = 2)

# most are OK except YR.ISOL - there is a pattern there, so should be included in model
# in fact, grazing and years since isolation were colinear (see Ex1)
```

and also for all the other covariates!!!

### Normality

```{r}
hist(E1, main = "Normality", breaks=10)
#Or qq-plot aka normality check
qqnorm(E1)
#qqline(E1)
```

#Plot

```{r predict}
range(Birds$LOGAREA)
```

```{r}
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
plotBare= ggplot(Birds, aes(x=ABUND, y=LOGAREA))                       +
            geom_point()                                               +
            geom_smooth(method="lm", formula = y ~ I(log(x)), na.rm=T) +
            labs(
     x = "Log transformed area",
     y = "Bird abundance"
     )
plotBare + coord_trans(x = "log10")

#Base plot Version
PP1 <- predict(M1, newdata = MyData, se = TRUE, interval = "confidence")

SeConf.Up  <- PP1$fit[,3]
SeConf.Low <- PP1$fit[,2]

plot(x = Birds$LOGAREA,
     y = Birds$ABUND)

lines(x = MyData$LOGAREA,
      y = PP1$fit[,1],
      lwd = 3,
      lty = 1,
      col = 1)


#Useful for later...when we have multiple lines
legend("topleft",
       legend=c("Line 1"),
       col = c(1),
       lty = c(1),
       lwd = c(1))
```

Now, model ABUND as a function of a categorical covariate (factor Graze)

```{r}
M2 <- lm(ABUND ~ factor(GRAZE), data=Birds)
```

```{r results='asis', echo=FALSE}
summary(M2) %>% stargazer
```


```{r}
#do.call(txtboxplot, sapply(unique(Birds$GRAZE), function(grazeID) filter(Birds, GRAZE ==grazeID)$ABUND,simplify=F))
anova(M2)
```

Apply model as a function of all covariates
But no interactions

```{r}
M3 <- lm(ABUND ~ LOGAREA + LOGDIST + LOGLDIST +
           YR.ISOL + ALT + factor(GRAZE),
         data = Birds)
```

### Is everything significant?

```{r results='asis', echo=FALSE}
summary(M3) %>% stargazer
```

```{r results='asis', echo=FALSE}
drop1(M3, test = "F")
```

Now, as a function of the interaction between a continuous and a categorical covariate
```{r}
         #this is = to LOGAREA + factor(GRAZE) + LOGAREA : factor(GRAZE)
M4 <- lm(ABUND ~ LOGAREA * factor(GRAZE), 
         data = Birds)
```

```{r results='asis', echo=FALSE}
summary(M4)
#test for the interaction - if no-significant, remove and test again
drop1(M4, test = "F") 
```

```{r results='asis', echo=FALSE}
M5 <- lm(ABUND ~ LOGAREA + factor(GRAZE), data = Birds)
```

```{r results='asis', echo=FALSE}
summary(M5)
```

```{r results='asis', echo=FALSE}
drop1(M5, test = "F")
```
