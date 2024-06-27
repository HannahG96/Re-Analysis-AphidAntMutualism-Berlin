### MODELS ###

##########################################################################################
### 1-A. APHID DENSITY ####
#=nb of aphids/length of focal zone (mm)

hist(Aphid_density$N_aphid.mm) 
hist(sqrt(Aphid_density$N_aphid.mm)) #square root transformation looks better

# Fit the model with square root transformation:
Aph<-lmer(sqrt(N_aphid.mm) ~ date_s + meanAnt.mean_s + Seal_500_s +
            date_s:meanAnt.mean_s +
            date_s:Seal_500_s +
            meanAnt.mean_s:Seal_500_s + 
            (1|plantPop),
          data=Aphid_density)#N=53

#Look at diagnostics:
plot(Aph)
qqnorm(resid(Aph))
res <- DHARMa::simulateResiduals(Aph) # using package DHARMa for testing residuals
plot(res) # Looks good

# model summary:
#-->t-tests use Satterthwaite's method
#=Satterthwaite's approximmation to degrees of freedom
summary(Aph) # P values are consistent with the partial R2 confidence intervals below

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
r.squaredGLMM(Aph)
#########marginal:0.467 
#########conditional:0.691 
#Calculate partial R2 for each predictor (only fixed effects):
r2beta(Aph, method="nsj")

#######################################################
### 1-B. APHID DENSITY EXCLUDING OUTLIER PLOT: Nl-200 
# Test effect of outlier plot with max sealing:
sub_df <- Aphid_density[-which(Aphid_density$plot.simple == "Nl-200"),]
sub_Aph<-lmer(sqrt(N_aphid.mm) ~ date_s + meanAnt.mean_s + Seal_500_s +
                date_s:meanAnt.mean_s +
                date_s:Seal_500_s +
                meanAnt.mean_s:Seal_500_s + 
                (1|plantPop), #+(1|plot.simple),
              data=sub_df)
summary(sub_Aph)  # No more interaction effect
#Look at diagnostics:
res <- DHARMa::simulateResiduals(sub_Aph) # using package DHARMa for testing residuals
plot(res) # Looks all good, no significant deviations anywhere
#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
r.squaredGLMM(sub_Aph)
#Calculate partial R2 for each predictor (only fixed effects):
r2beta(sub_Aph, method="nsj")

#############################################################################################
###2. Ant NUMBER: RESPONSE AS DECIMAL VARIABLE #####
hist(Ant_attendance$meanAnt.mean)

#Fit LMER with Gaussian distribution:
#-->log-transformed response to normalize data
hist(log(Ant_attendance$meanAnt.mean))

Ant.nb<-lmer(log(meanAnt.mean)~date_s + N_aphid_s + Seal_500_s +  
               date_s:N_aphid_s + date_s:Seal_500_s +
               N_aphid_s:Seal_500_s +
               (1|plantPop),
               data=Ant_attendance)#N=53
#Look at diagnostics:
plot(Ant.nb)
qqnorm(resid(Ant.nb))
res <- simulateResiduals(Ant.nb)
plot(res)

#Look at the model summary:
#-->t-tests use Satterthwaite's method
summary(Ant.nb) # P values are consistent with the partial R2 confidence intervals below
#-->Sealing is only marginally significant

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
r.squaredGLMM(Ant.nb)
#########marginal:0.3961412
#########conditional:0.7521842

#Calculate partial R2 for each predictor (only fixed effects):
r2beta(Ant.nb, method="nsj")
#-->partial Rsquared of sealing remains relatively high


###############################################################################################
### 3-A. Ant ATTENDANCE: ANT-PER-APHID RATIO ####
hist(Ant_attendance$AntperAphid.mean) # Not looking normal

# Fit LMER of log-transformed response:
#-->log-transformed response to normalize data
hist(log(Ant_attendance$AntperAphid.mean))

AntAtt<-lmer(log(AntperAphid.mean) ~ date_s + N_aphid_s + Seal_500_s +  
               date_s:N_aphid_s + date_s:Seal_500_s +
               N_aphid_s:Seal_500_s +
               (1|plantPop),
             data=Ant_attendance,
             control=lmerControl(optimizer="bobyqa",
                                 optCtrl=list(maxfun=2e5)))

#Look at diagnostics:
plot(AntAtt)
qqnorm(resid(AntAtt))
res <- simulateResiduals(AntAtt)
plot(res) # ok for normality, but still a slight curved trend in residuals... 

#Look at the model summary:
#-->t-tests use Satterthwaite's method
summary(AntAtt) # not the same results as the partial R2 here...

#Calculate the marginal (= fixed effect) and conditional (= fixed + random effects) r2 values:
r.squaredGLMM(AntAtt)
######### marginal:0.54
######### conditional:0.83

#Calculate partial R2 for each predictor (only fixed effects):
r2beta(AntAtt, method="nsj") 

###############################################################################################
### 3-B. ANT ATTENDANCE: TENDING TIME #### 

hist(Tending_Time$aphid_IA.sum)
# beta regression for this kind of proportions:
# https://rcompanion.org/handbook/J_02.html

#### BETA REGRESSIONS using the glmmTMB package:
tmp <-  na.omit(Tending_Time) #remove one NA value
Tend.betareg <- glmmTMB(aphid_IA.sum ~ date_s + N_aphid_s + Seal_500_s + 
                          date_s:N_aphid_s + date_s:Seal_500_s +
                          N_aphid_s:Seal_500_s +
                          (1|plantPop),
                        data= tmp,
                        family=beta_family)

# Test residuals:
hist(residuals(Tend.betareg)) # looks normal
res <- DHARMa::simulateResiduals(Tend.betareg)
plot(res) # looks good

# Test fixed effects:
#-->Wald test (z-test)
summary(Tend.betareg) # another way which gives similar p-values


# another way to look at fixed effects is calculating confidence intervals for the predictor coefficients:
coefs <- broom.mixed::tidy(Tend.betareg, conf.int = TRUE)
# same thing presented in a graph:
dw <- dotwhisker::dwplot(Tend.betareg,by_2sd=FALSE) 
print(dw+geom_vline(xintercept=0,lty=2))

# R squared: DOES NOT WORK with betaregression


#############################################################################################
# 4. Ant AGGRESSIVITY #####
#=probability of an aggressive reaction (reaction score >0): aggressive (1) vs. avoidance (0)
Ant_aggressivity$reaction<-as.factor(Ant_aggressivity$reaction)

# Fit GLMER:
#Model1: aggressive (1) vs. avoidance (0)
Ant_aggressivity$plot.simple<-as.factor(Ant_aggressivity$plot.simple)

reaction.binom<-glmer(reaction ~ context + date_s + N_aphid_s + Seal_500_s + 
                        context:date_s + context:N_aphid_s + context:Seal_500_s +
                        date_s:N_aphid_s + date_s:Seal_500_s +
                        N_aphid_s:Seal_500_s + 
                        (1|plantPop/date),
                      family = binomial,
                      data=Ant_aggressivity,
                      glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

# Test residuals:
plot(res <- simulateResiduals(reaction.binom)) # Some trend but not signif

# Test fixed effects:
#-->Wald Test
#=coefficient estimates are expected to be normally distributed (z-test)
summary(reaction.binom) # sealing, context and date are still signif

# r2 values:
r.squaredGLMM(reaction.binom)
#########marginal:0.212
#########conditional:0.347
# warning message

# partial r2
r2beta(reaction.binom, method="sgv")
#-->gives a different marginal r2=0.154

############################################################################################
###5. ANT AGGRESSIVENESS: EXCLUDED DATA 
#--> ants reacting with tolerance/curiosity and exploration to simulated attack
#-->These "no reactions" were classified as failed experiments and excluded from analysis to
#test ant aggressiveness along urbanisation gradient (N=47)

#Test probability of aggressive/avoidance reaction vs. no reaction (curiousity/tolerance)
#-->To justify this exclusion: test whether the likelihood of successful (aggressive/avoidance reaction)
#vs. failed (curiosity/tolerance) experiment of simulated attack is evenly distributed 
#along urbanisation gradient:
NOreaction.binom<-glmmTMB(noReact ~ context + date_s + N_aphid_s + Seal_500_s + 
                            context:date_s + context:N_aphid_s + context:Seal_500_s +
                            date_s:N_aphid_s + date_s:Seal_500_s +
                            N_aphid_s:Seal_500_s + 
                            (1|plantPop/date),
                          control=glmmTMBControl(optimizer=optim,
                                                 optArgs=list(method="BFGS")),
                          family = binomial(),
                          data= Exp3b_noReact)

# Test residuals:
plot(res <- simulateResiduals(NOreaction.binom)) #OK

# Test fixed effects:
#-->Wald Test
#=coefficient estimates are expected to be normally distributed (z-test)
summary(NOreaction.binom) # date is signif

# r2 values (using theoretical value):
r.squaredGLMM(NOreaction.binom)
#########marginal:0.336
#########conditional:0.443
# warning message

# partial r2
#r2beta(NOreaction.binom, method="sgv")-->not working for glmmTMB models
#partR2(NOreaction.binom)alternative package 'partR2'-->not working for glmmTMB models


############## REVISIONS: ADDITIONAL ANALYSES TO TEST ROBUSTNESS OF RESULTS ############
#-->we preferred analysing trends in our response variables using a continuous urban variable (%sealing)
#-->reviewers suggested categorization of our urban variable, however this is not recommendable as our data
#is not evenly distributed along %sealing and ecologically significant categories can not be made
#### AIM: #### 
#check robustness of some of our analyses by removing the most urban plot

#ANT AGGRESSIVENESS
new.dat<-Ant_aggressivity[-which(Ant_aggressivity$plot.simple=="Nl-200"),]#23 obs removed
# Fit GLMER:
#Model1: aggressive (1) vs. avoidance (0)
new.dat$plot.simple<-as.factor(new.dat$plot.simple)

reaction.binom_new<-glmer(reaction ~ context + date_s + N_aphid_s + Seal_500_s + 
                        context:date_s + context:N_aphid_s + context:Seal_500_s +
                        date_s:N_aphid_s + date_s:Seal_500_s +
                        N_aphid_s:Seal_500_s + 
                        (1|plantPop/date),
                        family = binomial,
                      data=new.dat,
                      glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

# Test residuals:
plot(res <- simulateResiduals(reaction.binom_new)) # good

# Test fixed effects:
#-->Wald Test
#=coefficient estimates are expected to be normally distributed (z-test)
summary(reaction.binom_new) # sealing and context are still signif, date is marginally signif

# r2 values:
r.squaredGLMM(reaction.binom_new)
#########marginal:0.212
#########conditional:0.347
# warning message

# partial r2
r2beta(reaction.binom_new, method="sgv")
#-->gives a different marginal r2=0.154