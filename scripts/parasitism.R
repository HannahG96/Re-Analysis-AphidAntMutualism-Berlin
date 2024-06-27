#EXPLORE PARASITISM DATA

# Test whether there are trends in the proportion of parasitized aphids along the urbanisation gradient

plot(prop_paras ~ Seal_500, Ant_attendance, col = date) #=> a simple corr suggests a slight positive association
cor.test( ~ prop_paras + Seal_500_s, Ant_attendance, method = "pearson") 

## try to fit a model: percentages with lots of zeros...
# PArasitism vs. sealing

## try to fit a model: percentages with lots of zeros...
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


#########PROBABILITY OF PARASITISM PRESENT (1/0) IN APHID COLONY ~ SEALING

# Hurdle model part 1 - Probability of presence of parasitism
tmp = mutate(Ant_attendance, pres_paras= as.numeric(prop_paras>0))
pres_paras <- glmmTMB(pres_paras ~ date_s + Seal_500_s
                      + date_s:Seal_500_s +  N_aphid_s 
                      + date_s:N_aphid_s + date_s:Seal_500_s + N_aphid_s:Seal_500_s
                      +(1|plantPop), # + plot.simple),
                      data=tmp, family=binomial())

#########TRENDS IN PROPORTION OF PARASITISM WITHIN PARASITIZED COLONIES ~ SEALING
# Hurdle model part 2 - proportion of parasitism (only paras. host plants)
#-->PROBLEMS WITH THIS MODEL
tmp = filter(Ant_attendance, prop_paras != 0)
prop_paras <- glmmTMB(prop_paras ~ date_s + Seal_500_s
                      + date_s:Seal_500_s +  N_aphid_s 
                      + date_s:N_aphid_s + date_s:Seal_500_s + N_aphid_s:Seal_500_s
                        +(1|plantPop),
                      data= tmp,
                      family= gaussian)


# Test residuals:
hist(residuals(prop_paras)) # bof
res <- DHARMa::simulateResiduals(prop_paras)
plot(res) # -->PROBLEM WITH THIS MODEL


#################TRENDS IN THE NUMBER OF MUMMIES WITHIN PARASITIZED COLONIES~SEALING
## ALTERNATIVE for model 2: model parasitized aphids counts using a poisson distribution

#Back-transform proportion of parasitism into aphid mummy counts:
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
#-->Wald test (z-test)
summary(counts_paras) # no significant variables/interactions 

# r2 values:
r.squaredGLMM(counts_paras)
#########marginal:0.4070
#########conditional:0.9598---->THIS IS VERY HIGH....