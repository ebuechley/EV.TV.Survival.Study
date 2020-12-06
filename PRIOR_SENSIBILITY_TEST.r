##########################################################################
#
# EGYPTIAN VULTURE MONTHLY SURVIVAL ANALYSIS FROM TELEMETRY - PRIOR SENSIBILITY TEST
#
##########################################################################
# written by Steffen Oppel, December 2020
# motivated by paper Bannan et al 2020 about appropriate use of priors in ecology
# simulate survival probabilities from specified priors to ensure they cover plausible survival probabilities

## NEED TO CHANGE PRECISION FOR PARAMETERS FROM 0.01 to 0.5!

### CONVERSION OF PRECISION SPECIFICATION
# in JAGS, precision in the normal distribution is specified by 1/variance
# in R, precision in normal distribution is specified by sqrt(variance)

precconv<-function(x){sqrt(1/x)}



### CREATE DATA FRAME WITH 10000 RANDOM VALUES DRAWN FROM PRIORS

mean.phi <- runif(10000,0.9, 1)   # uninformative prior for all MONTHLY survival probabilities
lp.mean <- log(mean.phi/(1 - mean.phi))    # logit transformed survival intercept
b.phi.capt <- rnorm(10000,0, precconv(0.5))         # Prior for captive effect on survival probability on logit scale
b.phi.mig <- rnorm(10000,0, precconv(0.5))          # Prior for migration effect on survival probability on logit scale
b.phi.lat1 <- rnorm(10000,0, precconv(0.5))          # Prior for AFRICA effect on survival probability on logit scale
b.phi.age <- rnorm(10000,0, precconv(0.5))            # Prior for age effect on survival probability on logit scale
b.phi.pop <- runif(10000,0, 4)         # Prior for population effect on survival probability on logit scale
sigma.surv <- runif(10000,0, 2)                     # Prior for standard deviation of survival
tau.surv <- sigma.surv^-2
surv.raneff <- rnorm(10000,0, precconv(tau.surv))

INPUT<-data.frame(lp.mean,b.phi.capt,b.phi.mig,b.phi.lat1,b.phi.age,b.phi.pop,surv.raneff, simul=1)
FAKEDATA<-expand.grid(mig=c(0,1),lat1=c(0,1),capt=c(0,1),age=c(0,1),pop=c(0,1)) %>% mutate(simul=1)


#### SPECIFY EQUATION TO CALCULATE SURVIVAL PROBABILITY FROM PRIORS
FAKEDATA %>% full_join(INPUT, by='simul') %>%
  mutate(logit_phi=
           lp.mean +      ### intercept for mean survival 
            b.phi.mig*(mig) +       ### survival dependent on migratory stage of the month (stationary or migratory)
            b.phi.lat1*(lat1) +       ### survival dependent on migratory stage of the month (stationary or migratory)
            b.phi.capt*(capt) +     ### survival dependent on captive-release (captive-raised or other)
            b.phi.age*(age) +     ### survival dependent on age (juvenile or other)
           b.phi.pop*(pop)  +    ### survival dependent on population (western Europe or other)
            surv.raneff) %>%
  mutate(phi=plogis(logit_phi)) %>%
  
  ggplot() + geom_histogram(aes(x=phi))


