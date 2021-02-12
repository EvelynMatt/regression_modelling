head(lse)

require(tidyverse)
lse %>% select(BA:ANTO) ->y
cor(y)-> results
cor(lse[,-c(1:4)])
results

install.packages("corrplot")
library(corrplot)
install.packages("devtools")
devtools::install_github("taiyun/corrplot")
corrplot.mixed(results, lower.col = "black", number.cex=0.5, tl.cex=0.5)

library(lares)
y %>% corr_var(BA)
corr_var(y, BA, top = 5) 
corr_cross(y, top = 30) ##### checking for multicollinearity


lm1 <- lm(BA~STJ+VOD+EZJ+RMV+CNA, data=lse) 
plot(lm1)
summary(lm1)
shapiro.test(residuals(lm1))
dfR <- nrow(y) - length(coef(lm1)) 
dfM <- length(coef(lm1))-1
dfT <- nrow(y) - 1
RSS <- sum(resid(lm1)^2)
TSS <- sum((y$BA - mean(y$BA))^2)
MSS <- TSS - RSS
MSR <- RSS/dfR 
MSM <- MSS/dfM
F <- MSM/MSR
qf(p = 0.95, df1 = 5, df2 = 1001)
confint.lm(lm1,level=0.95)

predict(lm1, interval = "confidence", level = 0.95)

coef(lm1)
plot(lm1$residuals, ylab = 'Residuals')  ### pattern here
plot(lm1$residuals~ lse$STJ+lse$VOD+lse$EZJ+lse$RMV+lse$CNA)
plot(lm1$residuals~Year, data = lse)
car::vif(lm1)
AIC(lm1)
hist(lm1$residuals, xlab = 'Residuals', main = 'Histogram Of Residuals')
require('forecast')
checkresiduals(lm1)


RMSE <- function(error) { sqrt(mean(error^2)) }
RMSE(lm1$residuals)

######################BUILDING A NEW MODEL########################################
#######USING VARIABLES WITHOUT HIGH MULTICOLLINEARITY####

lm_01<- lm(BA~ STJ+RB+PRU+EXPN+CPG+CCL+
             RR+DLG+TUI+LLOY+RMV+SDR+EZJ+BATS+
             SPX+TSCO+RTO+ANTO, data=lse)
summary(lm_01)

full_lm <- lm(BA ~ STJ+RB+MIN+PRU+CCH+EXPN+VOD+GVC+AHT+CPG+CCL+RR+DLG+
                   TUI+LLOY+RMV+SSE+SDR+SMT+EZJ+NMC+BATS+SPX+TSCO+CNA+
                   RTO+ANTO, data=lse)
summary(full_lm)

step(lm_01, scope=~ STJ+RB+PRU+EXPN+CPG+CCL+
       RR+DLG+TUI+LLOY+RMV+SDR+EZJ+BATS+
       SPX+TSCO+RTO+ANTO, direction = 'both')

lm(formula = BA ~ STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
     LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, data = lse)

step(lm_01, scope=~ STJ+RB+PRU+EXPN+CPG+CCL+
             RR+DLG+TUI+LLOY+RMV+SDR+EZJ+BATS+
             SPX+TSCO+RTO+ANTO, direction = 'backward' ) 

lm(formula = BA ~ STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
     LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, data = lse)


step(full_lm, scope= ~STJ+RB+MIN+PRU+CCH+EXPN+VOD+GVC+AHT+CPG+CCL+RR+DLG+
             TUI+LLOY+RMV+SDR+EZJ+NMC+BATS+SPX+TSCO+
             RTO+ANTO, direction = 'both')
#RR, TUI, ANTO

step(full_lm, scope= ~STJ+RB+MIN+PRU+CCH+EXPN+VOD+GVC+AHT+CPG+CCL+RR+DLG+
             TUI+LLOY+RMV+SDR+EZJ+NMC+BATS+SPX+TSCO+
             RTO+ANTO, direction = 'backward')
# RR, TUI, ANTO

lm_02<-lm (BA~STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
             LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, data=lse)
summary(lm_02)
plot(lm_02)

full2_lm<-lm (BA~STJ+RB+MIN+PRU+CCH+EXPN+VOD+GVC+AHT+CPG+CCL+DLG+
                      LLOY+RMV+SDR+EZJ+NMC+BATS+SPX+TSCO+
                      RTO, data=lse)

summary(full2_lm)
plot(full2_lm)

step(lm_02, scope=~STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
       LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, direction = 'both' ) 
lm(formula = BA ~ STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
     LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, data = lse)

step(lm_02, scope=~STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
       LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, direction = 'backward' ) 
lm(formula = BA ~ STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
     LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, data = lse)



step(full2_lm, scope = ~STJ+RB+MIN+PRU+CCH+EXPN+VOD+GVC+AHT+CPG+CCL+DLG+
             LLOY+RMV+SDR+EZJ+NMC+BATS+SPX+TSCO+
             RTO, direction = 'both')
step(full2_lm, scope = ~STJ+RB+MIN+PRU+CCH+EXPN+VOD+GVC+AHT+CPG+CCL+DLG+
             LLOY+RMV+SDR+EZJ+NMC+BATS+SPX+TSCO+
             RTO, direction = 'backward')


###################CHECKING DEPENDENT AGAINST RESPONSE FOR TRANSFORMATION######

plot(lm_02$residuals~STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
       LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, data = lse)

plot(sqrt(lse$BA)~lse$STJ+lse$RB+lse$MIN+lse$PRU+lse$CCH+lse$EXPN+
             lse$VOD+lse$GVC+lse$AHT+lse$CPG+lse$CCL+lse$DLG+
             lse$LLOY+lse$RMV+lse$SDR+lse$EZJ+lse$NMC+lse$BATS+
             lse$SPX+lse$TSCO+
             lse$RTO)

############ POTENTIAL TRANFORMATIONS #################

plot(BA~STJ+RB+MIN+PRU+CCH+EXPN+VOD+GVC+AHT+CPG+CCL+DLG+
   LLOY+RMV+SDR+EZJ+NMC+BATS+SPX+TSCO+
   RTO, data=lse)

lse$CCL1 <-lse$CCL^2 ###
lse$CCH1 <-lse$CCH^2
lse$PRU1 <-lse$PRU^2
lse$DLG1 <-lse$DLG^1/2 
lse$EXPN1 <-lse$EXPN^2
lse$VOD1 <-lse$VOD^2
lse$GVC1 <-lse$GVC^1/2
lse$AHT1 <-(-1/4^(lse$AHT))
lse$CPG1 <-lse$CPG^2
lse$RMV1 <-(-1/2^(lse$RMV))
lse$EZJ1 <-lse$EZJ^2
lse$TSCO1 <-lse$TSCO^2
lse$SPX1 <-lse$SPX^2
lse$NMC1 <-lse$NMC^2
lse$SDR1 <-lse$SDR^2
lse$RTO1 <-lse$RTO^1/2
lse$TSCO1 <-lse$TSCO^2
lse$MIN1 <-(-1/2^lse$MIN) 




lm_04<-lm (BA~STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
             LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, data=lse)
summary(lm_04)
plot(lm_04)
shapiro.test(residuals(lm_04))
anova(lm_04)
AIC(lm_04)  


################## REFIT MODEL#################
step(lm_04, scope = ~STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
       LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, direction = 'both')

lm(formula = BA ~ STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
     LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, data = lse)

step(lm_04, scope = ~STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
       LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, direction = 'backward')

lm(formula = BA ~ STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
     LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, data = lse)

full2_lm04 <-lm(BA ~ STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
                  LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, data = lse)
summary(full2_lm04)
plot(full2_lm04)
shapiro.test(residuals(full2_lm04))
anova(full2_lm04)

plot(BA~lse$STJ+lse$MIN+lse$PRU+lse$CCH+
             lse$VOD+lse$GVC+lse$AHT+lse$CPG+lse$CCL+lse$DLG+
             lse$LLOY+lse$RMV+lse$SDR+lse$EZJ+lse$NMC+
             lse$SPX+lse$TSCO+
             lse$RTO)




summary(finalmodel)
plot(finalmodel, which = 2)
shapiro.test(residuals(finalmodel))
anova(full2_lm04)
AIC(finalmodel)

################## REFIT MODEL#################

step(full2_lm04, scope = ~ STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
       LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, direction = 'backward')

lm(formula = BA ~ STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
     LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, data = lse)

step(full2_lm04, scope = ~ STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
       LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, direction = 'both')

lm(formula = BA ~ STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
     LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, data = lse)

final<- lm (BA ~ STJ + PRU + EXPN + CPG + CCL + DLG + TUI + 
              LLOY + RMV + SDR + EZJ + BATS + TSCO + RTO + ANTO, data=lse)

################## THIS IS THE FINAL MODEL FROM SELECTION#################
Final<- lm (BA ~ STJ + PRU1 + EXPN + CPG + CCL + DLG + TUI + 
              LLOY + RMV + SDR1 + EZJ + BATS + TSCO + RTO + ANTO, data=lse)
summary(Final)
shapiro.test(residuals(Final))
plot(Final)
car::vif(Final)
AIC(Final)
require(forecast)
checkresiduals(Final)

##############################################################################
###### Model selection ######

####variables with low multicollinerity <80 #####

modelfit1<- lm(BA~ STJ+RB+MIN+PRU+CCH+EXPN+VOD+
                 GVC+AHT+CPG+CCL+RR+DLG+TUI+LLOY+
                 RMV+SDR+EZJ+BATS+SPX+TSCO+RTO+ANTO, data = lse)

####variables with low multicollinerity  #####

modelfit2<- lm(BA~ STJ+RB+VOD+CPG+CCL+RR+DLG+TUI+
                 LLOY+SDR+EZJ+BATS+TSCO+CNA+RTO+ANTO, data = lse)

plot(BA~STJ+RB+MIN+PRU+CCH+EXPN+VOD+
       GVC+AHT+CPG+CCL+RR+DLG+TUI+LLOY+
       RMV+SDR+EZJ+BATS+SPX+TSCO+RTO+ANTO, data = lse)

plot(BA~STJ+RB+VOD+CPG+CCL+RR+DLG+TUI+
       LLOY+SDR+EZJ+BATS+TSCO+CNA+RTO+ANTO, data = lse)


step(modelfit1, scope = ~STJ+RB+MIN+PRU+CCH+EXPN+VOD+
       GVC+AHT+CPG+CCL+RR+DLG+TUI+LLOY+
       RMV+SDR+EZJ+BATS+SPX+TSCO+RTO+ANTO, direction = 'both')

lm(formula = BA ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + 
     AHT + CPG + RR + DLG + LLOY + RMV + SDR + EZJ + BATS + SPX + 
     TSCO + RTO, data = lse)

step(modelfit1, scope = ~STJ+RB+MIN+PRU+CCH+EXPN+VOD+
       GVC+AHT+CPG+CCL+RR+DLG+TUI+LLOY+
       RMV+SDR+EZJ+BATS+SPX+TSCO+RTO+ANTO, direction = 'backward')

lm(formula = BA ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + 
     AHT + CPG + RR + DLG + LLOY + RMV + SDR + EZJ + BATS + SPX + 
     TSCO + RTO, data = lse)

step(modelfit2, scope = ~STJ+RB+VOD+CPG+CCL+RR+DLG+TUI+
       LLOY+SDR+EZJ+BATS+TSCO+CNA+RTO+ANTO, direction = 'both')

lm(formula = BA ~ STJ + VOD + CPG + DLG + TUI + LLOY + SDR + 
     EZJ + BATS + TSCO + CNA + RTO + ANTO, data = lse)

step(modelfit2, scope = ~STJ+RB+VOD+CPG+CCL+RR+DLG+TUI+
       LLOY+SDR+EZJ+BATS+TSCO+CNA+RTO+ANTO, direction = 'backward')

lm(formula = BA ~ STJ + VOD + CPG + DLG + TUI + LLOY + SDR + 
     EZJ + BATS + TSCO + CNA + RTO + ANTO, data = lse)





modelfit1.1<- lm(BA~STJ + RB + MIN + PRU1 + CCH + EXPN + VOD1 + GVC + 
                   AHT + CPG + RR + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
                   TSCO + RTO, data = lse)

modelfit2.2<- lm(BA ~ STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
                   EZJ + BATS + TSCO + CNA + RTO + ANTO, data = lse)

plot(modelfit1.1$residuals~STJ + RB + MIN + PRU1 + CCH + EXPN + VOD1 + GVC + 
       AHT + CPG + RR + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
       TSCO + RTO, data = lse)

plot(modelfit2.2$residuals~ STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
       EZJ + BATS + TSCO + CNA + RTO + ANTO, data = lse)


step(modelfit1.1, scope=~STJ + RB + MIN + PRU1 + CCH + EXPN + VOD1 + GVC + 
       AHT + CPG + RR + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
       TSCO + RTO, direction = 'backward')
lm(formula = BA ~ STJ + RB + MIN + PRU1 + CCH + EXPN + GVC + 
     AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
     TSCO + RTO, data = lse)

step(modelfit1.1, scope=~STJ + RB + MIN + PRU1 + CCH + EXPN + VOD1 + GVC + 
       AHT + CPG + RR + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
       TSCO + RTO, direction = 'both')
lm(formula = BA ~ STJ + RB + MIN + PRU1 + CCH + EXPN + GVC + 
     AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
     TSCO + RTO, data = lse)


step(modelfit2.2, scope=~STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
       EZJ + BATS + TSCO + CNA + RTO + ANTO, direction = 'backward')
lm(formula = BA ~ STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
     EZJ + BATS + TSCO + CNA + RTO + ANTO, data = lse)

step(modelfit2.2, scope=~STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
       EZJ + BATS + TSCO + CNA + RTO + ANTO, direction = 'both')
lm(formula = BA ~ STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
     EZJ + BATS + TSCO + CNA + RTO + ANTO, data = lse)

plot(modelfit1.1)
summary(modelfit1.1)
summary(modelfit2.2)
plot(modelfit2.2)

################## BOXCOX
boxcox(BA ~ STJ + STJ + RB + MIN + PRU1 + CCH + EXPN + GVC + 
         AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
         TSCO + RTO, data=lse)

##### boxcox makes Cp negative so not suitable yet?

modelfit1.1<- lm(BA ~ STJ + RB + MIN + PRU1 + CCH + EXPN + GVC + 
                   AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
                   TSCO + RTO, data=lse)


modelfit2.2<- lm(BA ~ STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
                   EZJ + BATS + TSCO + CNA + RTO + ANTO, data=lse)


summary(modelfit1.1)
summary(modelfit2.2)


step(modelfit1.1, scope=~STJ + RB + MIN + PRU1 + CCH + EXPN + VOD + GVC + 
       AHT + CPG + RR + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
       TSCO + RTO, direction = 'both')
lm(formula = BA ~ STJ + RB + MIN + PRU1 + CCH + EXPN + GVC + 
     AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
     TSCO + RTO + VOD, data = lse)

step(modelfit1.1, scope=~STJ + RB + MIN + PRU1 + CCH + EXPN + VOD + GVC + 
       AHT + CPG + RR + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
       TSCO + RTO, direction = 'backward')
lm(formula = BA ~ STJ + RB + MIN + PRU1 + CCH + EXPN + GVC + 
     AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
     TSCO + RTO, data = lse)


step(modelfit2.2, scope=~STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
       EZJ + BATS + TSCO + CNA + RTO + ANTO, direction = 'both')
lm(formula = BA ~ STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
     EZJ + BATS + TSCO + CNA + RTO + ANTO, data = lse)


step(modelfit2.2, scope=~STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
       EZJ + BATS + TSCO + CNA + RTO + ANTO, direction = 'backward')
lm(formula = BA ~ STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
     EZJ + BATS + TSCO + CNA + RTO + ANTO, data = lse)

################
modelfit3<- lm (BA ~ STJ + RB + MIN + PRU1 + CCH + EXPN + GVC + 
                  AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
                  TSCO + RTO, data = lse)

modelfit4<- lm(BA ~ STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
                 EZJ + BATS + TSCO + CNA + RTO + ANTO, data = lse)




plot(modelfit3$residuals~STJ + RB + MIN + PRU1 + CCH + EXPN + GVC + 
       AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
       TSCO + RTO, data = lse)

plot(modelfit4$residuals~STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
       EZJ + BATS + TSCO + CNA + RTO + ANTO, data = lse)

plot(modelfit3)
plot(modelfit4)

summary(modelfit3)
summary(modelfit4)


modelfit4.1<- lm(BA ~ STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
                   EZJ + BATS + TSCO + CNA + RTO + ANTO, data = lse)

summary(modelfit4.1)
plot(modelfit4.1)

modelfit3.1<- lm (BA ~ STJ + RB + MIN + PRU1 + CCH + EXPN + GVC + 
                    AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
                    TSCO + RTO, data = lse)

summary(modelfit3.1)
plot(modelfit3.1, which = 2)

plot(modelfit4.1$residuals)
plot(modelfit3.1$residuals)

###### selection

step(modelfit3.1, scope = ~STJ + RB + MIN + PRU1 + CCH + EXPN + GVC + 
       AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
       TSCO + RTO, direction = 'both')
lm(formula = BA ~ STJ + RB + MIN + PRU1 + CCH + EXPN + GVC + 
     AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
     TSCO + RTO, data = lse)

step(modelfit3.1, scope = ~STJ + RB + MIN + PRU1 + CCH + EXPN + GVC + 
       AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
       TSCO + RTO, direction = 'backward')
lm(formula = BA ~ STJ + RB + MIN + PRU1 + CCH + EXPN + GVC + 
     AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
     TSCO + RTO, data = lse)



step(modelfit4.1, scope = ~STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
       EZJ + BATS + TSCO + CNA + RTO + ANTO, direction = 'both')
lm(formula = BA ~ STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
     EZJ + BATS + TSCO + CNA + RTO + ANTO, data = lse)

step(modelfit4.1, scope = ~STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
       EZJ + BATS + TSCO + CNA + RTO + ANTO, direction = 'backward')
lm(formula = BA ~ STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
     EZJ + BATS + TSCO + CNA + RTO + ANTO, data = lse)



modelfit6<- lm(BA ~ STJ + RB + MIN + PRU1 + CCH + EXPN + GVC + 
                 AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
                 TSCO + RTO, data = lse)


modelfit7<- lm(BA ~ STJ + VOD1 + CPG + DLG + TUI + LLOY + SDR1 + 
                 EZJ + BATS + TSCO + CNA + RTO + ANTO, data = lse)
AIC(modelfit6)

summary(modelfit6)
summary(modelfit7)
plot(modelfit6, which=2)
plot(modelfit7, which=2)
car::vif(modelfit7)

plot(modelfit6$residuals~STJ + RB + MIN + PRU + CCH + EXPN + VOD1 + 
       GVC + AHT + CPG + DLG + LLOY + RMV + SDR + EZJ + BATS + SPX + 
       TSCO + RTO, data = lse)


plot(modelfit7$residuals~STJ + CPG + DLG + TUI + LLOY + EZJ + BATS + 
       TSCO + CNA + RTO + ANTO, data = lse)

confint.lm(modelfit6,level=0.95)
confint.lm(modelfit7,level=0.95)

############################ USING LEAPS #################################
require(leaps)
leap.mod <- leaps(x=y[,2:28], y=y[,1], nbest=5, method='adjr2',
                  names=colnames(y[,-1]))

result.tab <- data.frame(adjr2=leap.mod$adjr2, size=leap.mod$size,
                         leap.mod$which, row.names=NULL)

head(result.tab)
plot(adjr2~size, data=result.tab)


result.tab %>% filter(size==9)
##best model is: STJ, PRU, VOD, GVC, CPG, RMV, SSE, RTO

result.tab %>% filter(size==10)
##best model is: STJ, MIN, VOD, GVC, AHT, CPG, DLG,SPX, RTO

result.tab %>% filter(size==11)
##best model is:  STJ, RB, MIN, VOD, GVC, AHT, CPG, DLG, SPX, RTO

size9<- lm(BA~ STJ+PRU+VOD+GVC+CPG+RMV+SSE+RTO, data = lse)
size10<- lm(BA~STJ+MIN+VOD+GVC+ AHT+ CPG+ DLG+ SPX+ RTO, data=lse)
size11<- lm(BA~STJ+ RB +MIN+ VOD+ GVC+ AHT+ CPG+ DLG+ SPX+ RTO, data=lse)

plot(size9$residuals~STJ+PRU+VOD+GVC+CPG+RMV+SSE+RTO, data = lse)
plot(size10$residuals~STJ+MIN+VOD+GVC+ AHT+ CPG+ DLG+ SPX+ RTO, data = lse)
plot(size11$residuals~STJ+ RB +MIN+ VOD+ GVC+ AHT+ CPG+ DLG+ SPX+ RTO, data = lse)

############

require(olsrr)
ols_mallows_cp(size9, full_lm)
ols_mallows_cp(size10, full_lm)
ols_mallows_cp(size11, full_lm)
ols_mallows_cp(modelfit6, full_lm)
ols_mallows_cp(modelfit7, full_lm)
ols_mallows_cp(Final, full_lm)

sum((resid(size9)/ (1 - hatvalues(size9)))^2)
sum((resid(size10)/ (1 - hatvalues(size10)))^2)
sum((resid(size11)/ (1 - hatvalues(size11)))^2)
sum((resid(modelfit6)/ (1 - hatvalues(modelfit6)))^2)
sum((resid(modelfit7)/ (1 - hatvalues(modelfit7)))^2)
sum((resid(Final)/ (1 - hatvalues(Final)))^2)

summary(size9)
summary(size10)
summary(size11)
summary(modelfit6)
summary(modelfit7)
summary(Final)

AIC(size9)
AIC(size10)
AIC(size11)
AIC(modelfit6)
AIC(modelfit7)
AIC(Final)





#########################PREDICTION USING THE MODEL ######################
predict_BAE <- function(lse, newdata){
  # Carry out any transformations prior to fitting you model # Add transformed variables to both lse and newdata. E.g.:
  
  lse$PRU1 <-lse$PRU^2
  newdata$PRU1 <-newdata$PRU^2
  lse$SDR1 <-lse$SDR^2
  newdata$SDR1 <-newdata$SDR^2
  # this is the part that fits your linear model
  BAE.lm <- lm(BA ~ STJ + RB + MIN + PRU1 + CCH + EXPN + GVC + 
                 AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
                 TSCO + RTO + Year, data=lse)
  # this is the part that produces predictions using your linear model 
  predictions <- predict(BAE.lm, newdata = newdata) 
  return(predictions)
}

###################### IMPROVING ON THE MODEL #######################################
modelfit6<- lm(BA ~ STJ + RB + MIN + PRU1 + CCH + EXPN + GVC + 
                 AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
                 TSCO + RTO, data = lse)
summary(modelfit6)

restricted.lm<-lm(BA ~ STJ + RB + MIN + CCH + EXPN + GVC + 
                    AHT + CPG + DLG + LLOY + RMV + SDR1 + EZJ + BATS + SPX + 
                    TSCO + RTO, data = lse)

car::vif(restricted.lm)
restricted.lm<-lm(BA ~ STJ + RMV+ MIN+
                    AHT + DLG  + SDR1 + EZJ + BATS + SPX + 
                    TSCO , data = lse)


car::vif(restricted.lm)
shapiro.test(residuals(restricted.lm))
checkresiduals(restricted.lm)
summary(restricted.lm)
plot(restricted.lm, which = 2)

dfR <- nrow(y) - length(coef(modelfit6)) 
dfM <- length(coef(modelfit6))-1
full.rss<- sum(residuals(modelfit6)^2)
restricted.rss <- sum(residuals(restricted.lm)^2)
fstat<-((restricted.rss - full.rss) / 8) / (full.rss / 988)


qf(p = 0.95, df1 = 18, df2 = 988)

################################### NEW MODEL PREDICTION

predict_BAE <- function(lse, newdata){
  # Carry out any transformations prior to fitting you model # Add transformed variables to both lse and newdata. E.g.:
  
  lse$SDR1 <-lse$SDR^2
  newdata$SDR1 <-newdata$SDR^2
  # this is the part that fits your linear model
  BAE.lm <- lm(BA ~ STJ + RMV + MIN +
                 AHT + DLG  + SDR1 + EZJ + BATS + SPX + 
                 TSCO  + Year, data=lse)
  # this is the part that produces predictions using your linear model 
  predictions <- predict(BAE.lm, newdata = newdata) 
  return(predictions)
}



