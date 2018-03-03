library(lme4)
library(MCMCglmm)
library(tidyverse)
library(broom)
library(nadiv)

df_syndrome <- read_csv("syndrome.csv")

# Univariate
lmer_b <- lmer(boldness ~ scale(assay_rep, scale=FALSE) + scale(body_size) +
                   (1|ID),
               data = df_syndrome)
plot(lmer_b)
qqnorm(residuals(lmer_b))
hist(residuals(lmer_b))
summary(lmer_b)

rep_bold <- tidy(lmer_b, effects = "ran_pars", scales = "vcov") %>% select(group, estimate) %>%
    spread(group, estimate) %>%
    mutate(repeatability = ID/(ID + Residual))
rep_bold

# Bivariate
prior_E_B_1px = list(R = list(V = diag(2), nu = 0.002),
                     G = list(G1 = list(V = diag(2), nu = 2, alpha.mu = rep(0,2),
                                        alpha.V = diag(25^2,2,2))))

mcmc_E_B_us <- MCMCglmm(cbind(scale(exploration), scale(boldness)) ~ 
                            trait-1 + 
                            trait:scale(assay_rep, scale = FALSE) +
                            trait:scale(body_size),
                        random =~ us(trait):ID,
                        rcov =~ us(trait):units,
                        family = c("gaussian","gaussian"), 
                        prior = prior_E_B_1px, 
                        nitt=420000,
                        burnin=20000,
                        thin=100,
                        verbose = TRUE,
                        data = as.data.frame(df_syndrome))
plot(mcmc_E_B_us$VCV)
summary(mcmc_E_B_us)

mcmc_prop_E <- mcmc_E_B_us$VCV[,"traitexploration:traitexploration.ID"]/(
    mcmc_E_B_us$VCV[,"traitexploration:traitexploration.ID"] +
        mcmc_E_B_us$VCV[,"traitexploration:traitexploration.units"]
)

plot(mcmc_prop_E)

HPDinterval(mcmc_prop_E)

# From Jarred

library(MCMCglmm)

N<-500
# 500 individuals

V<-matrix(c(1,0.5, 0.5, 1),2,2)
# 2x2 random/residual-effect covariance matrix

Vr<-matrix(1,1,1)
# residual variance for repeat measure trait

u<-MASS::mvrnorm(N, rep(0, 2), V)
# random effect for repeat measure trait followed by
# residuals for the single measured trait.

e<-rnorm(2*N,0,sqrt(Vr))
# residuals for the repeat trait

ysingle<-1+u[,2]
# single measure traits has intercept of 1

individual<-as.factor(rep(1:N, 3))
# individuals are ordered within each trait/time combination

type<-as.factor(c(rep("s", N), rep("r",2*N)))
# designate which observations are single measures (s)
# or repeat measures (r)

yrep<--1+u[rep(1:N, 2),1]+e
# the repeat measure trait has an intercept of -1

# use flat priors, but use covu=TRUE to mdoel covariances
# between R1 effects and the random effects
# (G is not needed because random effect prior is
#  specified in R1)

dat1<-data.frame(y=c(ysingle,yrep), type=type,
                 individual=individual)


# It sounds like one of your responses is repeat measure and the other
# not. With a bivariate model this means you want to model the covariance
# between the ID random effect for the repeat-measure trait and the
# residual for the single-measure trait (an ID term and  a residual would
# be non-identifiable for the latter). In MCMCglmm you can do this using
# the covu argument in the prior. An example is below.

prior1<-list(R=list(R1=list(V=V, nu=0, covu=TRUE),
                    R2=list(V=Vr, nu=0)))

str(dat1)
str(prior1)

m.test1<-MCMCglmm(y~type-1,
                  random=~us(at.level(type,"r")):individual,
                  rcov=~us(at.level(type, "s")):individual + us(at.level(type, "r")):units,
                  data=dat1,
                  prior=prior1,
                  verbose=FALSE)

str(dat1)

summary(m.test1)

# Have to fit repeat measure residuals in the
# second residual term.












# Other example

mcmc_biv_1 <- MCMCglmm(cbind(scale(ESM), scale(Outcome)) ~ trait-1,
                       random =~ us(trait):ID,
                       rcov =~ idh(trait):units)