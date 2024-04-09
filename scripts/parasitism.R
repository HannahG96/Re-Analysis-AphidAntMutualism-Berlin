#EXPLORE PARASITISM DATA

# Test whether there are trends in the proportion of parasitized aphids along the urbanisation gradient

plot(prop_paras ~ Seal_500, Ant_attendance, col = date) #=> a simple corr suggests a slight positive association
cor.test( ~ prop_paras + Seal_500_s, Ant_attendance, method = "pearson") 

## try to fit a model: percentages with lots of zeros...


# PArasitism vs. sealing

plot(prop_paras ~ Seal_500, Ant_attendance, col = date) #=> a simple corr suggests a slight positive association
cor.test( ~ prop_paras + Seal_500_s, Ant_attendance, method = "pearson") 

## try to fit a model: percentages with lots of zeros...
Ant_attendance$N_paras = ceiling( Ant_attendance$N_aphid * Ant_attendance$prop_paras)


prop_paras <- glmmTMB(prop_paras ~ date_s + Seal_500_s
                           + date_s:Seal_500_s + N_aphid_s +
                             (1|plantPop+plot.simple),
                           weights = N_aphid,
                        data= Ant_attendance,
                        family= gaussian)

# Test residuals:
hist(residuals(prop_paras))
res <- DHARMa::simulateResiduals(prop_paras)
plot(res) # VERY BAD => Need other model


#########PROBABILITY OF PARASITISM PRESENT (1/0) IN APHID COLONY~SEALING
# Hurdle model part 1 - Probability of presence of parasitism
tmp = mutate(Ant_attendance, pres_paras= as.numeric(prop_paras>0))
pres_paras <- glmmTMB(pres_paras ~ date_s + Seal_500_s
                      + date_s:Seal_500_s +  N_aphid_s 
                      + date_s:N_aphid_s + date_s:Seal_500_s + N_aphid_s:Seal_500_s
                        +(1|plantPop))# + plot.simple),


# Hurdle model part 1
tmp = mutate(Ant_attendance, pres_paras= as.numeric(prop_paras>0))
pres_paras <- glmmTMB(pres_paras ~ date_s + Seal_500_s
                      + date_s:Seal_500_s +  
                        (1|plantPop + plot.simple),
                      data= tmp,
                      family= binomial())
plot(pres_paras ~ Seal_500, tmp, col = date)

##Test residuals:
hist(residuals(pres_paras)) # bof
res <- DHARMa::simulateResiduals(pres_paras)
plot(res) # some trend but not significant

plot(res) # looks not too horrible


## Test fixed effects:
car::Anova(pres_paras, type="III") # one way to test for significance (probably the best)
##-->Wald test (z-test)
summary(pres_paras)
#=> no effect on presence of parasitism


# r2 values:
r.squaredGLMM(pres_paras)
#########marginal:
#########conditional:

##################TRENDS IN PROPORTION OF PARASITISM WITHIN PARASITIZED COLONIES~SEALING
# Hurdle model part 2 - proportion of parasitism (only paras. host plants)
#-->PROBLEMS WITH THIS MODEL
tmp = filter(Ant_attendance, prop_paras != 0)
#tmp<-tmp[-which(tmp$plot.simple=="Nl-200"),]-->removing the most urban plot: Sealing becomes insignificant
prop_paras <- glmmTMB(prop_paras ~ date_s + Seal_500_s
                      + date_s:Seal_500_s +  N_aphid_s 
                      + date_s:N_aphid_s + date_s:Seal_500_s + N_aphid_s:Seal_500_s
                        +(1|plantPop), #+ plot.simple),
                      data= tmp,
                      family= gaussian)

# Hurdle model part 2
tmp = filter(Ant_attendance, prop_paras != 0)
prop_paras <- glmmTMB(prop_paras ~ date_s + Seal_500_s
                      + date_s:Seal_500_s +  N_aphid_s +
                        (1|plantPop + plot.simple),

                      data= tmp,
                      family= gaussian())


# Test residuals:
hist(residuals(prop_paras)) # bof
res <- DHARMa::simulateResiduals(prop_paras)
plot(res) # -->PROBLEM WITH THIS MODEL


#################TRENDS IN THE NUMBER OF MUMMIES WITHIN PARASITIZED COLONIES~SEALING
## ALTERNATIVE for model 2: model parasitized aphids counts using a poisson distribution
#Transform proportion of parasitism into aphid mummy counts:
tmp$N_paras = ceiling( tmp$N_aphid * (tmp$prop_paras/100))

counts_paras <- glmmTMB(N_paras ~ date_s + Seal_500_s
                        + date_s:Seal_500_s +  N_aphid_s 
                        + date_s:N_aphid_s + date_s:Seal_500_s + N_aphid_s:Seal_500_s
                        +(1|plantPop), #+ plot.simple),
                        data= tmp,
                        family= poisson)

# Test residuals:
hist(residuals(counts_paras)) # OK
res <- DHARMa::simulateResiduals(counts_paras)
plot(res) # -->OK

# Test fixed effects:
car::Anova(counts_paras, type="III") # one way to test for significance (probably the best)
#-->Wald test (z-test)
summary(counts_paras) 

# r2 values:
r.squaredGLMM(counts_paras)
#########marginal:0.4070
#########conditional:0.9598---->THIS IS VERY HIGH....

###########################################################################################################
##############################EXCLUDED FROM ANALYSIS###############################################

plot(res) # looks not too bad

# Test fixed effects:
car::Anova(prop_paras, type="III") # one way to test for significance (probably the best)
#-->Wald test (z-test)
summary(prop_paras) 

plot(prop_paras ~ Seal_500, tmp, col = date)
#=> Sealing has positive effect, but mostly due to high values in the highly urban colony (only one).

## Hurdle model part 2 - Maximum parasitism - no dates => NS
tmp = filter(Ant_attendance, prop_paras != 0) %>%
  group_by(plantPop) %>%
  reframe(max_para = max(prop_paras, na.rm = TRUE),
          Seal_500 = mean(Seal_500),
          plot.simple = unique(plot.simple))

max_para <- lm(max_para ~  Seal_500 ,
                      data= tmp)

# Test residuals:
hist(residuals(max_para)) # bof
res <- DHARMa::simulateResiduals(max_para)
plot(res) # horrible heteroscedasticity!

# Test fixed effects:
car::Anova(max_para, type="III") # one way to test for significance (probably the best)
#-->Wald test (z-test)
summary(max_para) 

plot(max_para ~ jitter(Seal_500), tmp)
#=> Sealing has no visible effect
