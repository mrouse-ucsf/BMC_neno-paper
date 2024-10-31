##Data cleaning and analysis for Neno data##

##import dataset
library(readxl)
dataset <- read_excel("~/Desktop/R for Neno Review/Neno dataset_10.25.23 clean for R.xlsx")

##set working directory
setwd("/Users/Desktop/R for Neno Review")

##load packages for data cleaning 
library(dplyr)
library(tidyverse)
library(tidyr)
library(magrittr)
library(table1)
library(readxl)
library(flextable)
library(janitor)
library(zoo)

## factor is for grouping, character is for open variables ##
summary(dataset)
table(dataset$facility)

## converted character variables to numeric ##
dataset$'neo comp none' <- as.numeric(dataset$'neo comp none')
summary(dataset$`neo comp none`)
dataset$referred <- as.numeric(dataset$referred)
summary(dataset$referred)

## converted year variable to categorical ##
dataset$'year' <- as.factor(dataset$'year')
summary(dataset$year)

## renamed variables ##
names(dataset)
names(dataset)[names(dataset) == "sepsis...10"] <- "matsepsis"
names(dataset)[names(dataset) == "sepsis...37"] <- "neosepsis"
names(dataset)[names(dataset) == "home tba"] <- "hometba"
names(dataset)[names(dataset) == "in transit"] <- "transit"
names(dataset)[names(dataset) == "other facility"] <- "otherfac"
names(dataset)[names(dataset) == "this facility"] <- "thisfac"

names(dataset)[names(dataset) == "ob antibiotics"] <- "obantibio"
names(dataset)[names(dataset) == "blood transfusion"] <- "bloodtrans"
names(dataset)[names(dataset) == "newborn antibiotics"] <- "neoantibio"
names(dataset)[names(dataset) == "newborn management other"] <- "neomanageother"
names(dataset)[names(dataset) == "neo comp other"] <- "neocompother"
names(dataset)[names(dataset) == "neo comp none"] <- "neocompnone"
names(dataset)[names(dataset) == "weight 2500"] <- "wt2500"
names(dataset)[names(dataset) == "alive exp nvp"] <- "liveexpnvp"
names(dataset)[names(dataset) == "alive exp no nvp"] <- "expnonvp"
names(dataset)[names(dataset) == "liveexpnvp"] <- "expnvp"
names(dataset)[names(dataset) == "neonatal death"] <- "neodeath"
names(dataset)[names(dataset) == "alive no exp"] <- "no exp"
names(dataset)[names(dataset) == "alive unknown exp"] <- "unknownexp"
names(dataset)[names(dataset) == "total neonates"] <- "totalneo"
names(dataset)[names(dataset) == "no exp"] <- "noexp"

names(dataset)

## created composite variable of total births ##
dataset$totalbirths <- dataset$hometba+dataset$transit+dataset$otherfac+dataset$thisfac
summary(dataset$totalbirths)

## created matsepsis to percent by dividing by total births ##
dataset$matsepsispercent <- dataset$matsepsis/dataset$totalbirths
summary(dataset$matsepsispercent)
#### 20% is a high sepsis rate, so we go look at the facility and look at total (double check) ##

## create all % variables by dividing by total births 
dataset$rupercent <- dataset$ru/dataset$totalbirths
dataset$oplpercent <- dataset$opl/dataset$totalbirths
dataset$preeclpercent <- dataset$preecl/dataset$totalbirths
dataset$aphpercent <- dataset$aph/dataset$totalbirths
dataset$fdpercent <- dataset$fd/dataset$totalbirths
dataset$nonepercent <- dataset$none/dataset$totalbirths
dataset$rppercent <- dataset$rp/dataset$totalbirths
dataset$otherpercent <- dataset$other/dataset$totalbirths
dataset$pphpercent <- dataset$pph/dataset$totalbirths
dataset$plpercent <- dataset$pl/dataset$totalbirths
dataset$breechpercent <- dataset$breech/dataset$totalbirths
dataset$cspercent <- dataset$cs/dataset$totalbirths
dataset$spvpercent <- dataset$spv/dataset$totalbirths
dataset$vacuumpercent <- dataset$vacuum/dataset$totalbirths
dataset$transitpercent <- dataset$transit/dataset$totalbirths
dataset$hometbapercent <- dataset$hometba/dataset$totalbirths
dataset$otherfacpercent <- dataset$otherfac/dataset$totalbirths
dataset$thisfacpercent <- dataset$thisfac/dataset$totalbirths
dataset$anticorticopercent <- dataset$anticortico/dataset$totalbirths
dataset$obantibiopercent <- dataset$obantibio/dataset$totalbirths
dataset$erppercent <- dataset$erp/dataset$totalbirths
dataset$anticonvulsivepercent <- dataset$anticonvulsive/dataset$totalbirths
dataset$bloodtranspercent <- dataset$bloodtrans/dataset$totalbirths
dataset$mrppercent <- dataset$mrp/dataset$totalbirths
dataset$nasgpercent <- dataset$nasg/dataset$totalbirths
dataset$neoantibiopercent <- dataset$neoantibio/dataset$totalbirths
dataset$neomanageotherpercent <- dataset$neomanageother/dataset$totalbirths
dataset$resuscitationpercent <- dataset$resuscitation/dataset$totalbirths
dataset$asphyxiapercent <- dataset$asphyxia/dataset$totalbirths
dataset$neocompnonepercent <- dataset$neocompnone/dataset$totalbirths
dataset$neocompotherpercent <- dataset$neocompother/dataset$totalbirths
dataset$prematpercent <- dataset$premat/dataset$totalbirths
dataset$neosepsispercent <- dataset$neosepsis/dataset$totalbirths
dataset$wt2500percent <- dataset$wt2500/dataset$totalbirths
dataset$expnvppercent <- dataset$expnvp/dataset$totalbirths
dataset$expnonvppercent <- dataset$expnonvp/dataset$totalbirths
dataset$neodeathpercent <- dataset$neodeath/dataset$totalbirths
dataset$fsbpercent <- dataset$fsb/dataset$totalbirths
dataset$noexppercent <- dataset$noexp/dataset$totalbirths
dataset$unknownexppercent <- dataset$unknownexp/dataset$totalbirths
dataset$msbpercent <- dataset$msb/dataset$totalbirths
dataset$totalneopercent <- dataset$totalneo/dataset$totalbirths
dataset$referredpercent <- dataset$referred/dataset$totalbirths

## CHECK DATA SET
summary(dataset)

summary(dataset$neoantibiopercent)
summary(dataset$neocompnonepercent)
summary(dataset$referredpercent)

## because of missing data, I removed rows with many na's — original row#s: 8, 190, 202, 476, 477 ##
## team was not able to verify these rows, so many missing which led to elimination from dataset ##
dataset <- dataset[-8,]
dataset <- dataset[-189,]
dataset <- dataset[-200,]
dataset <- dataset[-473,]
dataset <- dataset[-473,]

summary(dataset)
names(dataset)

## table 1 by year 
library(table1)
table1(~ru + opl + preecl +aph + fd + none + matsepsis + rp + other + pph + pl | year, data=dataset)

## change month variable to numeric 
dataset$month[dataset$month=="January"] <- 1
summary(dataset$month)
dataset$month[dataset$month=="February"] <- 2
dataset$month[dataset$month=="March"] <- 3
dataset$month[dataset$month=="April"] <- 4
dataset$month[dataset$month=="May"] <- 5
dataset$month[dataset$month=="June"] <- 6
dataset$month[dataset$month=="July"] <- 7
dataset$month[dataset$month=="August"] <- 8
dataset$month[dataset$month=="September"] <- 9
dataset$month[dataset$month=="October"] <- 10
dataset$month[dataset$month=="November"] <- 11
dataset$month[dataset$month=="December"] <- 12

## ensure month variable is numeric 
dataset$month<-as.numeric(dataset$month)
summary(dataset$month)

## make code for GAIN and PIH periods using if else meaning if 2019 and 2020, then assign GAIN or else assign PIH ##
dataset$GAIN <- as.factor(ifelse(dataset$year =="2019"|dataset$year == "2020", "GAIN", "PIH"))
summary(dataset$GAIN)

## to account for GAIN transition in October 2020, we assigned months greater than 9 aka September to PIH, moving Oct, Nov, Dec of 2020 to PIH time period ##
dataset$GAIN [dataset$year == "2020" & dataset$month >9] <- "PIH" 
summary(dataset$GAIN)

## table 1 by GAIN versus PIH 
table1(~ru + opl + preecl +aph + fd + none + matsepsis + rp + other + pph + pl | GAIN, data=dataset)

# make p-value function
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- wilcox.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- fisher.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}

## table 1 with total for maternal complications
table1(~ru + opl + preecl +aph + fd + none + matsepsis + rp + other + pph + pl | GAIN, data=dataset, overall=T)

## table 1 with p-value for maternal complications
table1(~ru + opl + preecl +aph + fd + none + matsepsis + rp + other + pph + pl | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))
## ping Isaac and Kapira to ask what they were doing related to the complications that were significantly lower
## break into six month intervals to detect changes ##

## table 1 with total for birth type 
table1(~breech + cs + spv + vacuum | GAIN, data=dataset, overall=T)

## table 1 with total for birth place
table1(~transit + hometba + thisfac + otherfac | GAIN, data=dataset, overall=T)

## table 1 with total for maternal complication management
table1(~anticortico + obantibio + erp + anticonvulsive + bloodtrans + mrp + nasg | GAIN, data=dataset, overall=T)

## table 1 with total for newborn complication management
table1(~neoantibio + neomanageother + resuscitation | GAIN, data=dataset, overall=T)

## table 1 with total for newborn complications
table1(~asphyxia + neocompnone + neocompother + premat + neosepsis + wt2500 | GAIN, data=dataset, overall=T)

## table 1 with total for newborn survival
table1(~expnvp + expnonvp + neodeath + fsb + noexp + unknownexp + msb | GAIN, data=dataset, overall=T)

## table 1 with total for referred 
table1(~totalneo + referred + totalbirths | GAIN, data=dataset, overall=T)

## table 1 with p-value for maternal complications %
table1(~rupercent + oplpercent + preeclpercent +aphpercent + fdpercent + nonepercent + matsepsispercent + rppercent + otherpercent + pphpercent + plpercent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))

## table 1 with p-value for birth type % 
table1(~breechpercent + cspercent + spvpercent + vacuumpercent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))

## table 1 with p-value for birth place %
table1(~transitpercent + hometbapercent + thisfacpercent + otherfacpercent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))

## table 1 with p-value for maternal complication management %
table1(~anticorticopercent + obantibiopercent + erppercent + anticonvulsivepercent + bloodtranspercent + mrppercent + nasgpercent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))

## table 1 with p-value for newborn complication management %
table1(~neoantibiopercent + neomanageotherpercent + resuscitationpercent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))

## table 1 with p-value for newborn complications %
table1(~asphyxiapercent + neocompnonepercent + neocompotherpercent + prematpercent + neosepsispercent + wt2500percent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))

## table 1 with p-value for newborn survival %
table1(~expnvppercent + expnonvppercent + neodeathpercent + fsbpercent + noexppercent + unknownexppercent + msbpercent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))

## table 1 with p-value for referred %
table1(~totalneopercent + referredpercent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))

## create a subset of my data by centers and hospitals where NDH and LCH are a hospital category ##
centers<- dataset[ which(dataset$facility=="Chifunga Health Centre" |
                           dataset$facility=="Dambe Health Centre" |
                           dataset$facility=="Luwani Health Centre" |
                           dataset$facility=="Magareta Health Centre" |
                           dataset$facility=="Matandani Health Centre" |
                           dataset$facility=="Matope Health Centre" |
                           dataset$facility=="Neno Parish Health Centre" |
                           dataset$facility=="Nsambe Sda Health Centre"),]

table(centers$facility)

hospitals<- dataset[ which(dataset$facility=="Lisungwi Health Centre" |
                           dataset$facility=="Neno District Hospital"),]

table(hospitals$facility)

## table 1 by subset of hospitals and centers with p-value ##
table1(~rupercent + oplpercent + preeclpercent +aphpercent + fdpercent + nonepercent + matsepsispercent + rppercent + otherpercent + pphpercent + plpercent | GAIN, data=centers, overall=F, extra.col=list(`P-value`=pvalue))

table1(~rupercent + oplpercent + preeclpercent +aphpercent + fdpercent + nonepercent + matsepsispercent + rppercent + otherpercent + pphpercent + plpercent | GAIN, data=hospitals, overall=F, extra.col=list(`P-value`=pvalue))


## create a binary variable for hospitals and centers ##
dataset$HospCent[dataset$facility== "Chifunga Health Centre"] <- "center"
dataset$HospCent[dataset$facility== "Dambe Health Centre"] <- "center"
dataset$HospCent[dataset$facility== "Luwani Health Centre"] <- "center"
dataset$HospCent[dataset$facility== "Magareta Health Centre"] <- "center"
dataset$HospCent[dataset$facility== "Matandani Health Centre"] <- "center"
dataset$HospCent[dataset$facility== "Matope Health Centre"] <- "center"
dataset$HospCent[dataset$facility== "Neno Parish Health Centre"] <- "center"
dataset$HospCent[dataset$facility== "Nsambe Sda Health Centre"] <- "center"
dataset$HospCent[dataset$facility== "Lisungwi Health Centre"] <- "hospital"
dataset$HospCent[dataset$facility== "Neno District Hospital"] <- "hospital"

summary(dataset$HospCent)
table(dataset$HospCent)

## table 1 by hospital and center with p-value ##
table1(~rupercent + oplpercent + preeclpercent +aphpercent + fdpercent + nonepercent + matsepsispercent + rppercent + otherpercent + pphpercent + plpercent | HospCent, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))

##create a subset of my data by GAIN and PIH for handoff times with a pre and post variable ##
prehandoff<- dataset[ which(dataset$GAIN=="GAIN"),]
posthandoff<- dataset[ which(dataset$GAIN=="PIH"),]

table(prehandoff$facility)
table(posthandoff$facility)

## table 1 by handoff and center versus hospital 
table1(~rupercent + oplpercent + preeclpercent +aphpercent + fdpercent + nonepercent + matsepsispercent + rppercent + otherpercent + pphpercent + plpercent | HospCent, data=prehandoff, overall=F, extra.col=list(`P-value`=pvalue))
table1(~rupercent + oplpercent + preeclpercent +aphpercent + fdpercent + nonepercent + matsepsispercent + rppercent + otherpercent + pphpercent + plpercent | HospCent, data=posthandoff, overall=F, extra.col=list(`P-value`=pvalue))

## make a dummy variable for group subsets by six month time period ##
dataset$timegroupvar<-"error"
summary(dataset$timegroupvar)
summary(which(dataset$year=="2019" & dataset$month<=6))

dataset$timegroupvar [c(dataset$year=="2019" & dataset$month <=6)] <- "group1"
table(dataset$timegroupvar)
dataset$timegroupvar [which(dataset$year=="2019" & dataset$month >6)] <- "group2"
dataset$timegroupvar [which(dataset$year== "2020" & dataset$month <=6)] <- "group3"
dataset$timegroupvar [which(dataset$year== "2020" & dataset$month >6)] <- "group4"
dataset$timegroupvar [which(dataset$year== "2021" & dataset$month <=6)] <- "group5"
dataset$timegroupvar [which(dataset$year== "2021" & dataset$month >6)] <- "group6"
dataset$timegroupvar [which(dataset$year== "2022" & dataset$month <=6)] <- "group7"
dataset$timegroupvar [which(dataset$year== "2022" & dataset$month >6)] <- "group8"
dataset$timegroupvar [which(dataset$year== "2023")] <- "group9"
table(dataset$timegroupvar)

## p-value for table 1 by hospital or health center
table1(~rupercent + oplpercent + preeclpercent +aphpercent + fdpercent + nonepercent + matsepsispercent + rppercent + otherpercent + pphpercent + plpercent | HospCent, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))

table1(~rupercent + oplpercent + preeclpercent +aphpercent + fdpercent + nonepercent + matsepsispercent + rppercent + otherpercent + pphpercent + plpercent | HospCent, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))

## if you have issues with syntax, google and check the library, because you will need to re-install if it is not the most recent used ##

##tabyl function to get descriptive percentages [you can choose % by col or row wise]
## library(janitor) again here if pipe isn't working ##
dataset %>%
  tabyl(HospCent, GAIN) %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col")  %>%
  adorn_pct_formatting()  %>%
  adorn_ns(position = "front")  %>%
  adorn_title (
    row_name = "time",
    col_name = "facility",
    placement = "combined") %>%
  flextable::flextable() %>%
  flextable::autofit()

##easy way to get total percentages 
table(dataset$timegroupvar)
prop.table(table(dataset$timegroupvar))


##build a multivariate linear regression model to explore 
###when you build a multivariate model in r, you have to name your model yourself
###lm means linear module in r language 
modelhandofffacility <- lm (GAIN ~ HospCent, data=dataset)
modelruhandoff <- lm (rupercent ~ GAIN + HospCent, data=dataset)

##get coefficients 
summary(modelruhandoff)

##get confidence intervals
confint(modelruhandoff)


##transform my variables into readable percents by moving decimal over two to the right
dataset[ , 49:92] <- dataset[ , 49:92]*100

## table 1 with p-values for all newly calculated percent variables 
table1(~rupercent + oplpercent + preeclpercent +aphpercent + fdpercent + nonepercent + matsepsispercent + rppercent + otherpercent + pphpercent + plpercent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))
table1(~breechpercent + cspercent + spvpercent + vacuumpercent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))
table1(~transitpercent + hometbapercent + thisfacpercent + otherfacpercent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))
table1(~anticorticopercent + obantibiopercent + erppercent + anticonvulsivepercent + bloodtranspercent + mrppercent + nasgpercent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))
table1(~neoantibiopercent + neomanageotherpercent + resuscitationpercent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))
table1(~asphyxiapercent + neocompnonepercent + neocompotherpercent + prematpercent + neosepsispercent + wt2500percent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))
table1(~neodeathpercent + fsbpercent + msbpercent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))
table1(~totalneopercent + referredpercent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))

##build a multivariate logistic regression model if independent variable was noncontinuous 

##start with pphpercent and build a model with facility variable as a confounder 
view(dataset)
mod1<-lm(dataset$pphpercent~dataset$GAIN)
summary(mod1)

wilcox.test(dataset$pphpercent~dataset$GAIN)

##try doing linear model with mrppercent since it was significant in bivariate and add facility to determine if is is a confounder
mod1<-lm(dataset$mrppercent~dataset$GAIN)
summary(mod1)

mod2<-lm(dataset$mrppercent~dataset$GAIN+dataset$facility)
summary(mod2)

mod3<-lm(dataset$mrppercent~dataset$GAIN+dataset$HospCent)
summary(mod3)

mod4<-lm(dataset$mrppercent~dataset$GAIN+dataset$facility+dataset$timegroupvar)
summary(mod4)

mod5<-lm(dataset$mrppercent~dataset$GAIN+dataset$timegroupvar)
summary(mod5)

BIC(mod1)
BIC(mod2)
BIC(mod3)
BIC(mod4)
BIC(mod5)

summary(mod1)
confint(mod1)

summary(mod2)
confint(mod2)

summary(mod3)
confint(mod3)

summary(mod4)
confint(mod4)

##
anova(mod1, mod2, test='F')
anova(mod2, mod3, test='F')
summary(mod1)
summary(mod2)
AIC(mod1)
AIC(mod2)
BIC(mod1)
BIC(mod2)
AIC(mod4)
anova(mod2)
anova(mod2, mod4, test="F")
##cannot use anova goodness of fit just between two variables, it has to be a nested model
anova(mod2, mod5, test="F")
AIC(mod2)
AIC(mod5)


##relevel variables to make NDH the reference group across multivariate models 
dataset$facility <- as.factor(dataset$facility)
dataset$facility<-relevel(dataset$facility,ref="Neno District Hospital")


##compared nested models with goodness of fit test, and non-nested models with Akaike information criterion and found the best model was adjusting only for facility
##run multivariate model with each variable, including only facility variable for adjusted p-value
mod4<-lm(dataset$mrppercent~dataset$GAIN+dataset$facility)
summary(mod4)
confint(mod4)
###bloodtranspercent
mod4<-lm(dataset$bloodtranspercent~dataset$GAIN+dataset$facility)
summary(mod4)
###anticonvulsivepercent
mod4<-lm(dataset$anticonvulsivepercent~dataset$GAIN+dataset$facility)
summary(mod4)
###erppercent
mod4<-lm(dataset$erppercent~dataset$GAIN+dataset$facility)
summary(mod4)
###obantibiopercent
mod4<-lm(dataset$obantibiopercent~dataset$GAIN+dataset$facility)
summary(mod4)
##anticorticopercent
mod4<-lm(dataset$anticorticopercent~dataset$GAIN+dataset$facility)
summary(mod4)
##nasgpercent
mod4<-lm(dataset$nasgpercent~dataset$GAIN+dataset$facility)
summary(mod4)

######OB COMPLICATION ADJUSTED
###rupercent
mod4<-lm(dataset$rupercent~dataset$GAIN+dataset$facility)
summary(mod4)
###oplpercent
mod4<-lm(dataset$oplpercent~dataset$GAIN+dataset$facility)
summary(mod4)
###preeclpercent
mod4<-lm(dataset$preeclpercent~dataset$GAIN+dataset$facility)
summary(mod4)
###aphpercent
mod4<-lm(dataset$aphpercent~dataset$GAIN+dataset$facility)
summary(mod4)
confint(mod4)
###fdpercent
mod4<-lm(dataset$fdpercent~dataset$GAIN+dataset$facility)
summary(mod4)
###nonepercent
mod4<-lm(dataset$nonepercent~dataset$GAIN+dataset$facility)
summary(mod4)
###matsepsispercent
mod4<-lm(dataset$matsepsispercent~dataset$GAIN+dataset$facility)
summary(mod4)
confint(mod4)
###rppercent
mod4<-lm(dataset$rppercent~dataset$GAIN+dataset$facility)
summary(mod4)
###otherpercent
mod4<-lm(dataset$otherpercent~dataset$GAIN+dataset$facility)
summary(mod4)
###pphpercent
mod4<-lm(dataset$pphpercent~dataset$GAIN+dataset$facility)
summary(mod4)
###plpercent
mod4<-lm(dataset$plpercent~dataset$GAIN+dataset$facility)
summary(mod4)

######BIRTH TYPE ADJUSTED
###breechpercent
mod4<-lm(dataset$breechpercent~dataset$GAIN+dataset$facility)
summary(mod4)
confint(mod4)
###cspercent
mod4<-lm(dataset$cspercent~dataset$GAIN+dataset$facility)
summary(mod4)
###spvpercent
mod4<-lm(dataset$spvpercent~dataset$GAIN+dataset$facility)
summary(mod4)
###vacuumpercent
mod4<-lm(dataset$vacuumpercent~dataset$GAIN+dataset$facility)
summary(mod4)

######BIRTH PLACE ADJUSTED
###transitpercent
mod4<-lm(dataset$transitpercent~dataset$GAIN+dataset$facility)
summary(mod4)
###hometbapercent
mod4<-lm(dataset$hometbapercent~dataset$GAIN+dataset$facility)
summary(mod4)
###thisfacpercent
mod4<-lm(dataset$thisfacpercent~dataset$GAIN+dataset$facility)
summary(mod4)
###otherfacpercent
mod4<-lm(dataset$otherfacpercent~dataset$GAIN+dataset$facility)
summary(mod4)

######NEWBORN MANAGEMENT ADJUSTED
###neoantibiopercent
mod4<-lm(dataset$neoantibiopercent~dataset$GAIN+dataset$facility)
summary(mod4)
confint(mod4)
###neomanageotherpercent
mod4<-lm(dataset$neomanageotherpercent~dataset$GAIN+dataset$facility)
summary(mod4)
###resuscitationpercent
mod4<-lm(dataset$resuscitationpercent~dataset$GAIN+dataset$facility)
summary(mod4)

######NEWBORN COMPLICATION ADJUSTED
###asphyxiapercent
mod4<-lm(dataset$asphyxiapercent~dataset$GAIN+dataset$facility)
summary(mod4)
###neocompnonepercent
mod4<-lm(dataset$neocompnonepercent~dataset$GAIN+dataset$facility)
summary(mod4)
###neocompotherpercent
mod4<-lm(dataset$neocompotherpercent~dataset$GAIN+dataset$facility)
summary(mod4)
###prematpercent
mod4<-lm(dataset$prematpercent~dataset$GAIN+dataset$facility)
summary(mod4)
confint(mod4)
###neosepsispercent
mod4<-lm(dataset$neosepsispercent~dataset$GAIN+dataset$facility)
summary(mod4)
confint(mod4)

mod7<-lm(dataset$neosepsispercent~dataset$GAIN)
summary(mod7)
confint(mod7)

###wt2500percent
mod4<-lm(dataset$wt2500percent~dataset$GAIN+dataset$facility)
summary(mod4)

######NEWBORN SURVIVAL ADJUSTED
###neodeathpercent
mod4<-lm(dataset$neodeathpercent~dataset$GAIN+dataset$facility)
summary(mod4)
###fsbpercent
mod4<-lm(dataset$fsbpercent~dataset$GAIN+dataset$facility)
summary(mod4)
###msbpercent
mod4<-lm(dataset$msbpercent~dataset$GAIN+dataset$facility)
summary(mod4)

######REFERRED ADJUSTED
###totalneopercent
mod4<-lm(dataset$totalneopercent~dataset$GAIN+dataset$facility)
summary(mod4)
###referredpercent
mod4<-lm(dataset$referredpercent~dataset$GAIN+dataset$facility)
summary(mod4)


##sub-analysis of birth place to remove all facilities where vacuum and CS do not occur, only keep NDH and LCH
###create a subset of my data with only NDH and LCH for facility called dataset2
table(dataset$facility)
class(dataset$facility)
dataset2<-dataset[c(dataset$facility == "Neno District Hospital" | dataset$facility == "Lisungwi Health Centre"),]
#####change c to ! makes it 'not' 
summary(dataset2)
table(dataset2$facility)

summary(dataset$GAIN)

table(dataset$facility, dataset$vacuumpercent) 
table(dataset$facility, dataset$cspercent) 

###cspercent
mod6<-lm(dataset2$cspercent~dataset2$GAIN+dataset2$facility)
summary(mod6)
confint(mod6)

summary(dataset2$cspercent)
summary(dataset2$cspercent[dataset2$GAIN=="PIH"])
summary(dataset2$cspercent[dataset2$GAIN=="GAIN"])
which(dataset2$cspercent==0)
#####ping Kapira for June and July of 2019, no CS at Lisungwi reported, what was up?

###vacuumpercent
mod6<-lm(dataset2$vacuumpercent~dataset2$GAIN+dataset2$facility)
summary(mod6)

summary(dataset2$vacuumpercent)
summary(dataset2$vacuumpercent[dataset2$GAIN=="PIH"])
summary(dataset2$vacuumpercent[dataset2$GAIN=="GAIN"])

table1(~spvpercent + breechpercent + vacuumpercent + cspercent | GAIN, data=dataset, overall=F, extra.col=list(`P-value`=pvalue))

sum(dataset$totalbirths)

aggregate(dataset$totalbirths ~ dataset$GAIN, data=dataset, sum)
mean(dataset$totalbirths)
min(dataset$totalbirths)
max(dataset$totalbirths)

max(dataset$oplpercent)

aggregate(dataset$totalbirths ~ dataset$facility, data=dataset, mean)

R.version

##sub-analysis of referrals to remove hospitals for which health center refer, only keep health centers
###create a subset of my data to remove NDH and LCH called dataset3
table(dataset$facility)
class(dataset$facility)
dataset3<-dataset[!(dataset$facility == "Neno District Hospital" | dataset$facility == "Lisungwi Health Centre"),]
#####change c to ! makes it 'not' 
summary(dataset3)
table(dataset3$facility)

mod7<-lm(dataset3$referredpercent~dataset3$GAIN+dataset3$facility)
summary(mod7)
confint(mod7)
table1(~referredpercent | GAIN, data=dataset3, overall=F, extra.col=list(`P-value`=pvalue))

##sub-analysis of referrals to remove NDH since it is the referral hospital, only keep health centers and LCH
###create a subset of my data to remove NDH called dataset4
table(dataset$facility)
class(dataset$facility)
dataset4<-dataset[!(dataset$facility == "Neno District Hospital"),]
#####change c to ! makes it 'not' 
summary(dataset4)
table(dataset4$facility)

mod8<-lm(dataset4$referredpercent~dataset4$GAIN+dataset4$facility)
summary(mod8)
confint(mod8)

##summary statistics
aggregate(dataset$totalbirths ~ dataset$HospCent, data=dataset, sum)
aggregate(dataset$totalbirths ~ dataset$HospCent, data=dataset, mean)
mean(dataset$totalbirths)
min(dataset$totalbirths)
aggregate(dataset$totalbirths ~ dataset$facility, data=dataset, mean)
aggregate(dataset$referredpercent ~ dataset$facility, data=dataset, mean)
aggregate(dataset$referredpercent ~ dataset$facility, data=dataset, median)