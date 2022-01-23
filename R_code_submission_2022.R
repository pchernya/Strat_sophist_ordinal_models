setwd("C:\\Users\\...")

################################################################################
#* Data read in and prep *#
library(magrittr)
D_fin <- read.csv("data_final.csv") %>% na.omit()
D_fin$post_strat <- as.ordered(
                    as.factor(D_fin$post_strat + 1))
#^^ make 1,2,3,4 as strategy sophist
D_fin$pre_strat <- as.factor(D_fin$pre_strat + 1) 
#^^ make categorical pre-strategy
D_fin$Instructors<-as.factor(D_fin$Instructors)
D_fin$SID<-as.factor(D_fin$SID)
D_fin$Class<-as.factor(D_fin$Class)
D_fin$School<-as.factor(D_fin$School)
D_fin$Item<-as.factor(D_fin$Item)
D_fin$Condition <- relevel(as.factor(D_fin$Condition),ref="SKIP")
summary(D_fin)

### set ORDER of items to align with developmental progression ###
D_fin$Item<-ordered(D_fin$Item, 
                    levels=c("i1B","i2B","i3B","i4B","i5B",
                             "i6B","i7B","i53B","i531B","i532B","i533B",
                             "i58B","i582B","i61B","i63B","i64B",
                             "iA17aB","iA17bB","i69B","i72B"))
### simplify item names
levels(D_fin$Item)<-c('1','2','3','4','5','6','7','53','53-1','53-2',
                      '53-3','58','58-2','61','63','64','A17a','A17b','69','72')
### consistently name pre-sophistication
levels(D_fin$pre_strat)<-c("Pre-soph = 1","Pre-soph = 2","Pre-soph = 3","Pre-soph = 4")

################################################################################
#* Exploratory plots *#
library(ggplot2)
library(forcats)
library(viridis)

pA<-ggplot(D_fin,aes(x=Condition,fill=fct_rev(post_strat))) + 
  geom_bar(position="fill") + facet_wrap(~pre_strat,nrow=1) +
  scale_fill_viridis(direction = -1,discrete=TRUE) +
  xlab("Intervention condition by Pre-sophistication") +
  ylab("Relative frequency") + labs(fill='Post-assessment sophistication') +
  theme_bw(base_size = 15) + theme(legend.position="top")
pA #Pre-sophistication x Intervention (alternate)

pB<-ggplot(D_fin,aes(x=Condition,fill=fct_rev(post_strat))) + 
  geom_bar(position="fill") + 
  scale_fill_viridis(direction = -1,discrete=TRUE) +
  facet_wrap(vars(Item),as.table=TRUE,nrow=2) +
  xlab("Intervention condition by Item") +
  ylab("Relative frequency") + labs(fill='Post-assessment sophistication') +
  theme_bw(base_size = 14) + theme(legend.position="top")
pB 

pC<-ggplot(D_fin,aes(x=Condition,fill=fct_rev(post_strat))) + 
  geom_bar(position="fill") +
  scale_fill_viridis(direction = -1,discrete=TRUE) +
  facet_wrap(vars(Class),nrow=2,as.table=TRUE) +
  xlab("Intervention condition by Classroom") +
  ylab("Relative frequency") + labs(fill='Post-assessment sophistication') +
  theme_bw(base_size = 14) + theme(legend.position="top")
pC #Intervention x classroom

library(ggpubr)
FIG_0<-ggarrange(pA, pB, pC,nrow=3,common.legend = TRUE)
FIG_0

################################################################################
#* Ordinal Models *#
library(brms)

# Cumulative logit with NO strat-specific effects || All ran effs avgd across strategies #
# random effect structure: Item x SID within Class
f_0<-formula(post_strat ~ pre_strat + Condition + 
               (1|Item) + (1|SID) + (1|Class),data=D_fin) #reported in paper
my_priors<-c(set_prior("normal(0, 1.5)", class = "b"),
             set_prior("normal(0, 2.0)", class = "Intercept"),
             set_prior("normal(0, 2.0)", class = "sd"))
# Runtime: approx. 35 mins
m_1<-brm(f_0, data=D_fin, prior=my_priors, family=cumulative(),
         control = list(adapt_delta=0.85),
         cores=3,iter=4000,warmup=1000,chains=3)
m_1
gc()
m_1<-add_criterion(m_1,c("loo")) 
m_1$criteria$loo  

# Runtime: approx. 10 mins
# Stopping ratio (i.e. Sequential logit)
m_1_sr<-brm(f_1, data=D_fin, prior=my_priors, family=sratio(),
            control = list(adapt_delta=0.85,max_treedepth=12),
            cores=3,iter=4000,warmup=1000,chains=3)
gc()
m_1_sr<-add_criterion(m_1_sr,c("loo"))
m_1_sr$criteria$loo  
gc()

# Runtime: approx. 10 mins
# Adj category
m_1_ac<-brm(f_1, data=D_fin, prior=my_priors, family=acat(),
            control = list(adapt_delta=0.85,max_treedepth=12),
            cores=3,iter=4000,warmup=1000,chains=3)
gc()
m_1_ac<-add_criterion(m_1_ac,c("loo"))
m_1_ac$criteria$loo  
gc()


##############################################################################
# Strategy-specific Intervention effects ONLY || All random effects averaged #
# random effect structure: Item x SID within Class
f_2<-formula(post_strat ~ pre_strat + cs(Condition) + 
               (1|Item) + (1|SID) + (1|Class),data=D_fin)
my_priors<-c(set_prior("normal(0, 1.5)", class = "b"),
             set_prior("normal(0, 2.0)", class = "Intercept"),
             set_prior("normal(0, 2.0)", class = "sd"))
# Runtime: approx. 15 mins
m_2_sr<-brm(f_2, data=D_fin, prior=my_priors, family=sratio(),
            control = list(adapt_delta=0.85),
            cores=3,iter=4000,warmup=1000,chains=3)
m_2_sr
gc()
m_2_sr<-add_criterion(m_2_sr,c("loo")) 
m_2_sr$criteria$loo  
gc()

# Runtime: approx. 15 mins
# Adj category
m_2_ac<-brm(f_2, data=D_fin, prior=my_priors, family=acat(),
            control = list(adapt_delta=0.85,max_treedepth=12),
            cores=3,iter=4000,warmup=1000,chains=3)
gc()
m_2_ac<-add_criterion(m_2_ac,c("loo"))
m_2_ac$criteria$loo  
gc()


####################################################################################
# Strategy-specific Pre-sophistication effects ONLY || All random effects averaged #
# random effect structure: Item x SID within Class
f_3<-formula(post_strat ~ cs(pre_strat) + Condition + 
               (1|Item) + (1|SID) + (1|Class),data=D_fin)
my_priors<-c(set_prior("normal(0, 1.5)", class = "b"),
             set_prior("normal(0, 2.0)", class = "Intercept"),
             set_prior("normal(0, 2.0)", class = "sd"))
###
m_3_sr<-brm(f_3, data=D_fin, prior=my_priors, family=sratio(),
            control = list(adapt_delta=0.85),
            cores=3,iter=4000,warmup=1000,chains=3)
m_3_sr
gc()
m_3_sr<-add_criterion(m_3_sr,c("loo")) 
m_3_sr$criteria$loo  
gc()

# Adj category
m_3_ac<-brm(f_3, data=D_fin, prior=my_priors, family=acat(),
            control = list(adapt_delta=0.85,max_treedepth=12),
            cores=3,iter=4000,warmup=1000,chains=3)
gc()
m_3_ac<-add_criterion(m_3_ac,c("loo"))
m_3_ac$criteria$loo  
gc()

#####################################################################################
# Strat-spec (Intervention AND Pre-soph) NO interaction || Strat-avg random effects #
library(brms)
f_4<-formula(post_strat ~ cs(pre_strat) + cs(Condition) +
               (1|Item) + (1|SID) + (1|Class),data=D_fin)
my_priors<-c(set_prior("normal(0, 1.5)", class = "b"),
             set_prior("normal(0, 2.0)", class = "Intercept"),
             set_prior("normal(0, 2.0)", class = "sd"))
m_4_sr<-brm(f_4, data=D_fin, prior=my_priors, family=sratio(),
            control = list(adapt_delta=0.85,max_treedepth=12),
            cores=3,iter=4000,warmup=1000,chains=3)
gc()
m_4_sr<-add_criterion(m_4_sr,c("loo"))
m_4_sr$criteria$loo  
gc()

###
m_4_ac<-brm(f_4, data=D_fin, prior=my_priors, family=acat(),
            control = list(adapt_delta=0.85,max_treedepth=12),
            cores=3,iter=4000,warmup=1000,chains=3)
gc()
m_4_ac<-add_criterion(m_4_ac,c("loo"))
m_4_ac$criteria$loo  
gc()


#####################################################################################
# Strat-spec (Intervention X Pre-soph) WITH interaction || Strat-avg random effects #
library(brms)
f_5<-formula(post_strat ~ cs(pre_strat*Condition) +
               (1|Item) + (1|SID) + (1|Class),data=D_fin)
my_priors<-c(set_prior("normal(0, 1.5)", class = "b"),
             set_prior("normal(0, 2.0)", class = "Intercept"),
             set_prior("normal(0, 2.0)", class = "sd"))
m_5_sr<-brm(f_5, data=D_fin, prior=my_priors, family=sratio(),
            control = list(adapt_delta=0.85,max_treedepth=12),
            cores=3,iter=4000,warmup=1000,chains=3)
gc()
m_5_sr<-add_criterion(m_5_sr,c("loo"))
m_5_sr$criteria$loo  
gc()

###
m_5_ac<-brm(f_5, data=D_fin, prior=my_priors, family=acat(),
            control = list(adapt_delta=0.85,max_treedepth=12),
            cores=3,iter=4000,warmup=1000,chains=3)
gc()
m_5_ac<-add_criterion(m_5_ac,c("loo"))
m_5_ac$criteria$loo  
gc()

### formally compare model 4 and 5 to assess interaction ###
compare_ic(m_4_sr$criteria$loo,m_5_sr$criteria$loo)
compare_ic(m_4_ac$criteria$loo,m_5_ac$criteria$loo)
compare_ic(m_7_sr$criteria$loo,m_8_sr$criteria$loo)
loo_compare(m_4_sr,m_5_sr)
loo_compare(m_7_sr,m_8_sr)


#####################################################################################
# Strat-spec (Intervention AND pre-soph) NO interaction || Strat-spec class effects #
library(brms)
f_6<-formula(post_strat ~ cs(pre_strat) + cs(Condition) +
               (1|Item) + (1|SID) + (cs(1)|Class),data=D_fin)
my_priors<-c(set_prior("normal(0, 1.5)", class = "b"),
             set_prior("normal(0, 2.0)", class = "Intercept"),
             set_prior("normal(0, 2.0)", class = "sd"))
m_6_sr<-brm(f_6, data=D_fin, prior=my_priors, family=sratio(),
            control = list(adapt_delta=0.85,max_treedepth=12),
            save_all_pars = TRUE,file="m_6_sr",seed=10111985,
            cores=3,iter=4000,warmup=1000,chains=3)
gc()
m_6_sr<-add_criterion(m_6_sr,c("loo"))
m_6_sr$criteria$loo  
gc()

###
m_6_ac<-brm(f_6, data=D_fin, prior=my_priors, family=acat(),
            control = list(adapt_delta=0.85,max_treedepth=12),
            save_all_pars = TRUE,file="m_6_ac",seed=10111985,
            cores=3,iter=4000,warmup=1000,chains=3)
gc()
m_6_ac<-add_criterion(m_6_ac,c("loo"))
m_6_ac$criteria$loo  
gc()


###########################################################################################
# Strat-spec (Intervention & Ppre-soph) NO interaction || Strat-spec Class + Item effects #
library(brms)
f_7<-formula(post_strat ~ cs(pre_strat) + cs(Condition) +
               (cs(1)|Item) + (1|SID) + (cs(1)|Class),data=D_fin)
my_priors<-c(set_prior("normal(0, 1.5)", class = "b"),
             set_prior("normal(0, 2.0)", class = "Intercept"),
             set_prior("normal(0, 2.0)", class = "sd"))
m_7_sr<-brm(f_7, data=D_fin, prior=my_priors, family=sratio(),
            control = list(adapt_delta=0.85,max_treedepth=12),
            save_all_pars = TRUE,file="m_7_sr",seed=10111985,
            cores=3,iter=4000,warmup=1000,chains=3)
gc()
m_7_sr<-add_criterion(m_7_sr,c("loo"))
m_7_sr$criteria$loo  
gc()

###
m_7_ac<-brm(f_7, data=D_fin, prior=my_priors, family=acat(),
            control = list(adapt_delta=0.85,max_treedepth=12),
            save_all_pars = TRUE,file="m_7_ac",seed=10111985,
            cores=3,iter=4000,warmup=1000,chains=3)
gc()
m_7_ac<-add_criterion(m_7_ac,c("loo"))
m_7_ac$criteria$loo  
gc()

####################################################################################################
# Strat-spec (Intervention & Pre-soph) NO interaction || Strat-spec Class + Item + Student effects #
library(brms)
f_8<-formula(post_strat ~ cs(pre_strat) + cs(Condition) +
               (cs(1)|Item) + (cs(1)|SID) + (cs(1)|Class),data=D_fin)
my_priors<-c(set_prior("normal(0, 1.5)", class = "b"),
             set_prior("normal(0, 2.0)", class = "Intercept"),
             set_prior("normal(0, 2.0)", class = "sd"))
m_8_sr<-brm(f_8, data=D_fin, prior=my_priors, family=sratio(),
            control = list(adapt_delta=0.85,max_treedepth=12),
            save_all_pars = TRUE,file="m_8_sr",seed=10111985,
            cores=3,iter=4000,warmup=1000,chains=3)
gc()
m_8_sr<-add_criterion(m_8_sr,c("loo"))
m_8_sr$criteria$loo  
gc()

###
m_8_ac<-brm(f_8, data=D_fin, prior=my_priors, family=acat(),
            control = list(adapt_delta=0.85,max_treedepth=12),
            save_all_pars = TRUE,file="m_8_ac",seed=10111985,
            cores=3,iter=4000,warmup=1000,chains=3)
gc()
m_8_ac<-add_criterion(m_8_ac,c("loo"))
m_8_ac$criteria$loo  
gc()

###
compare_ic(m_7_sr$criteria$loo,m_8_sr$criteria$loo)
compare_ic(m_8_sr$criteria$loo,m_8_ac$criteria$loo)
