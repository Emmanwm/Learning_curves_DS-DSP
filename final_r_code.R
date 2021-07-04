
### FINAL CODE ###

library(tidyverse)
library(mascutils)
library(bayr)
library(brms)
library(ggthemes)

df <- readxl::read_excel("C:/Users/Emma/Documents/Uni/Thesis/Code and df dance/df_triallevel_23678with_h_subject.xlsx")

df$subject<- as.factor(df$subject)
mycolors=c("#6b5f3c","#ccc627","#54ab8e","#587ed1","#b04366")


## FREE LEARNING CURVES ##


# Free learning curve 1
# per subject per sequence
df %>% 
  ggplot(aes(x = repetition,
             y = RT,
             color=subject)) +
  geom_point() +
  geom_smooth(se = F) +
  facet_wrap(~subject_h, scales = "free_y")+
  ylab("RT (s)")+
  xlab("Repetition")+
  ylim(0,1.288)+
  theme_minimal()+
  scale_color_manual(values=mycolors)

# Free learning curve 2
# Individual learning curves over blocks/reps

df %>% 
  ggplot(aes(x = repetition,
             y = RT,
             group = subject,
             color=subject)) +
  geom_smooth(se = F)+
  scale_x_continuous(limits = c(0,192), expand = c(0, 0))+
  theme_classic()+
  ylab("RT (s)")+
  xlab("Repetition")+
  scale_color_manual(values=mycolors)+
  geom_vline(xintercept = c(24,48,72,96,120,144,168),colour="grey", show.legend=TRUE)+
  geom_text(aes(x=12, label="Block 1", y=0.1), colour="grey") +
  geom_text(aes(x=36, label="Block 2", y=0.1), colour="grey") +
  geom_text(aes(x=60, label="Block 3", y=0.1), colour="grey") +
  geom_text(aes(x=84, label="Block 4", y=0.1), colour="grey") +
  geom_text(aes(x=108, label="Block 5", y=0.1), colour="grey") +
  geom_text(aes(x=132, label="Block 6", y=0.1), colour="grey") +
  geom_text(aes(x=156, label="Block 7", y=0.1), colour="grey") +
  geom_text(aes(x=180, label="Block 8", y=0.1), colour="grey") 

## NON-LINEAR MULTILEVEL REGRESSION ##

# specify formula, variables and weakly informative priors
F_ary <- formula(RT ~ asym + ampl * exp(-rate * repetition))

F_ary_ef_1 <- list(formula(ampl ~ 1|subject),
                   formula(rate ~ 1|subject),
                   formula(asym ~ 1|subject))

F_ary_prior <- c(set_prior("normal(5, 100)", nlpar = "ampl", lb = 0),
                 set_prior("normal(.5, 3)", nlpar = "rate", lb = 0),
                 set_prior("normal(3, 20)", nlpar = "asym", lb = 0))

# create model including MCMC sampling
M_1 <- 
  df %>% 
  brm(bf(F_ary,
         flist = F_ary_ef_1,
         nl = T), 
      prior = F_ary_prior,
      family = "exgaussian",
      data = .,
      iter = 10, 
      warmup = 8,
      save_pars=save_pars("subject"))

P_1 <- posterior(M_1) 
PP_1 <- post_pred(M_1, thin = 10)

# save model estimates
save(M_1, P_1, PP_1, df, file = "model_estimates.Rda")

# get parameters for fixed effects, random factor variation and random effects
bayr::fixef(P_1)
bayr::grpef(P_1)
P_1 %>% re_scores() %>% bayr::ranef() 

## LEARNING CURVES BASED ON MODEL ESTIMATES ##

df$Subject<-df$subject
# learning curves per participant
df %>% 
  mutate(M_1 = predict(PP_1)$center) %>% 
  ggplot(aes(x = repetition,
             y = M_1,
             color=Subject)) +
  facet_wrap(~subject, scales = "free_y") +
  geom_smooth(se = F) +
  geom_point(alpha=0.2, size=1)+
  ylim(0,1.5)+
  labs(x="Trial",y="Model estimates")+
  theme_hc()+
  scale_color_manual(values=mycolors)

# crossbar plots for each parameter and participant
P_1 %>% 
  re_scores() %>% 
  bayr::ranef() %>% 
  ggplot(aes(x = re_entity, 
             y = center, 
             ymin = lower, 
             ymax = upper)) +
  facet_grid(nonlin~1, scales = "free_y") +
  geom_crossbar(width = .2) +
  labs(x = "Subject", y = "Model estimates") +
  theme_hc()

