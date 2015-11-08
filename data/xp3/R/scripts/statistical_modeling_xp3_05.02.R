################### Statistical Modeling - ALL ################################
########################################################################
library(languageR)
library(lme4)
library(Hmisc)


# Vocabulary test

### decorrelation

d.sub.05.num = d.sub.05[,c("jaar","lextale.scores", "av_prof", "trial")]
cor(d.sub.05.num)
d.sub.05$resav_prof = resid(lm(av_prof ~ jaar, data = d.sub.05))

### Props

d.sub.04$group = factor(d.sub.04$group)

lmer.select.fixed.effects01 = lmer(correctness ~ (group*red*p_dutch + group*red*voctest_correctness + trial)  + (1|set) + (1|subject), data = d.sub.04, family = binomial(logit))
summary(lmer.select.fixed.effects01) # 811.8
drop1(lmer.select.fixed.effects01)

lmer.select.fixed.effects02 = update(lmer.select.fixed.effects01 , ~. -group:red:p_dutch);
summary(lmer.select.fixed.effects02) # 810
anova(lmer.select.fixed.effects01, lmer.select.fixed.effects02)
drop1(lmer.select.fixed.effects02)

lmer.select.fixed.effects03= update(lmer.select.fixed.effects02 , ~. -trial);
summary(lmer.select.fixed.effects03) # 809.2
anova(lmer.select.fixed.effects02, lmer.select.fixed.effects03)
drop1(lmer.select.fixed.effects03)

lmer.select.fixed.effects04= update(lmer.select.fixed.effects03 , ~. -group:red:voctest_correctness);
summary(lmer.select.fixed.effects04) # 809
anova(lmer.select.fixed.effects03, lmer.select.fixed.effects04)
drop1(lmer.select.fixed.effects04)

lmer.select.fixed.effects05= update(lmer.select.fixed.effects04 , ~. -red:voctest_correctness);
summary(lmer.select.fixed.effects05) # 807.1
anova(lmer.select.fixed.effects04, lmer.select.fixed.effects05)
drop1(lmer.select.fixed.effects05)

lmer.select.fixed.effects06= update(lmer.select.fixed.effects05 , ~. -group:red);
summary(lmer.select.fixed.effects06) # 805.3
anova(lmer.select.fixed.effects05, lmer.select.fixed.effects06)
drop1(lmer.select.fixed.effects06)

lmer.select.fixed.effects07= update(lmer.select.fixed.effects06 , ~. -group:voctest_correctness);
summary(lmer.select.fixed.effects07) # 804.3
anova(lmer.select.fixed.effects06, lmer.select.fixed.effects07)
drop1(lmer.select.fixed.effects07)

# Random |set

lmer.select.rand.effects.final.01 = lmer(correctness ~ group + red + p_dutch + voctest_correctness + (1|set) + (1|subject) + group:p_dutch + red:p_dutch, data = d.sub.04, family = binomial(logit));
summary(lmer.select.rand.effects.final.01) # 804.3

lmer.select.rand.effects.a = lmer(correctness ~ group + red + p_dutch + voctest_correctness + (1+red|set) + (1|subject) + group:p_dutch + red:p_dutch, data = d.sub.04, family = binomial(logit));
summary(lmer.select.rand.effects.a) # 804.6

lmer.select.rand.effects.a = lmer(correctness ~ group + red + p_dutch + voctest_correctness + (1+group|set) + (1|subject) + group:p_dutch + red:p_dutch, data = d.sub.04, family = binomial(logit));
summary(lmer.select.rand.effects.a) # 808.3

# Subject

lmer.select.rand.effects.a = lmer(correctness ~ group + red + p_dutch + voctest_correctness + (1|set) + (1+red|subject) + group:p_dutch + red:p_dutch, data = d.sub.04, family = binomial(logit));
summary(lmer.select.rand.effects.a) # 807.7

lmer.select.rand.effects.a = lmer(correctness ~ group + red + p_dutch + voctest_correctness + (1|set) + (1+p_dutch|subject) + group:p_dutch + red:p_dutch, data = d.sub.04, family = binomial(logit));
summary(lmer.select.rand.effects.a) # 808.3

lmer.select.rand.effects.a = lmer(correctness ~ group + red + p_dutch + voctest_correctness + (1|set) + (1+voctest_correctness|subject) + group:p_dutch + red:p_dutch, data = d.sub.04, family = binomial(logit));
summary(lmer.select.rand.effects.a) # 808.1

# final model Props

final.model = lmer(correctness ~ group + red*p_dutch + group*p_dutch + voctest_correctness + (1|set) + (1|subject), data = d.sub.04, family = binomial(logit));
summary(final.model) # 804.3
options(contrasts=c("contr.treatment", "contr.poly"))

#### split by red.type and p_dutch

sub.red = d.sub.04[d.sub.04$red =="r",]

sub.u = d.sub.04[d.sub.04$red =="u",]

sub.i = d.sub.04[d.sub.04$p_dutch =="I",]

sub.l = d.sub.04[d.sub.04$p_dutch =="L",]


final.model.r = lmer(correctness ~ group*p_dutch + voctest_correctness + (1|set) + (1|subject), data = sub.red, family = binomial(logit));
summary(final.model.r)

final.model.u = lmer(correctness ~ group*p_dutch + voctest_correctness + (1|set) + (1|subject), data = sub.u, family = binomial(logit));
summary(final.model.u)

final.model.i = lmer(correctness ~ group + red + voctest_correctness + (1|set) + (1|subject), data = sub.i, family = binomial(logit));
summary(final.model.i)

final.model.l = lmer(correctness ~ group +red + voctest_correctness + (1|set) + (1|subject), data = sub.l, family = binomial(logit));
summary(final.model.l)

  # # # # # # # # # # # #
### RTs ######################################################
##############################################################
##############################################################

d.sub.06.num = d.sub.05[,c("trial", "jaar", "av_prof", "lextale.scores")]
cor(d.sub.06.num)
d.sub.06$resav_prof = resid(lm(av_prof ~ jaar, data = d.sub.06))

d.sub.06$experiment = factor(d.sub.06$group)

lmer.select.fixed.effects01a = lmer(log_rt ~ (group*red*p_dutch + group*red*voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.fixed.effects01a) # -7.806
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.fixed.effects01a) < mean(residuals(lmer.select.fixed.effects01a)) + 2.5 * sd(residuals(lmer.select.fixed.effects01a)) & residuals(lmer.select.fixed.effects01a) > mean(residuals(lmer.select.fixed.effects01a)) - 2.5 * sd(residuals(lmer.select.fixed.effects01a))),]
lmer.select.fixed.effects01b = lmer(log_rt ~ (group*red*p_dutch + group*red*voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.fixed.effects01b) # -223.5
drop1(lmer.select.fixed.effects01b)
lmer.select.fixed.effects01c = update(lmer.select.fixed.effects01b, ~. -group:red:voctest_correctness);
anova(lmer.select.fixed.effects01b, lmer.select.fixed.effects01c)

lmer.select.fixed.effects01a = lmer(log_rt ~ (group*red*p_dutch + red*voctest_correctness +group*voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.fixed.effects01a) # -9.704
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.fixed.effects01a) < mean(residuals(lmer.select.fixed.effects01a)) + 2.5 * sd(residuals(lmer.select.fixed.effects01a)) & residuals(lmer.select.fixed.effects01a) > mean(residuals(lmer.select.fixed.effects01a)) - 2.5 * sd(residuals(lmer.select.fixed.effects01a))),]
lmer.select.fixed.effects01b = lmer(log_rt ~ (group*red*p_dutch + red*voctest_correctness +group*voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.fixed.effects01b) # -225.5
drop1(lmer.select.fixed.effects01b)
lmer.select.fixed.effects01c = update(lmer.select.fixed.effects01b, ~. -red:voctest_correctness);
anova(lmer.select.fixed.effects01b, lmer.select.fixed.effects01c)

lmer.select.fixed.effects01a = lmer(log_rt ~ (group*red*p_dutch + group*voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.fixed.effects01a) # -9.929
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.fixed.effects01a) < mean(residuals(lmer.select.fixed.effects01a)) + 2.5 * sd(residuals(lmer.select.fixed.effects01a)) & residuals(lmer.select.fixed.effects01a) > mean(residuals(lmer.select.fixed.effects01a)) - 2.5 * sd(residuals(lmer.select.fixed.effects01a))),]
lmer.select.fixed.effects01b = lmer(log_rt ~ (group*red*p_dutch + group*voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.fixed.effects01b) # -227.1
drop1(lmer.select.fixed.effects01b)
lmer.select.fixed.effects01c = update(lmer.select.fixed.effects01b, ~. -group:voctest_correctness);
anova(lmer.select.fixed.effects01b, lmer.select.fixed.effects01c)

lmer.select.fixed.effects01a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.fixed.effects01a) # -10.89
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.fixed.effects01a) < mean(residuals(lmer.select.fixed.effects01a)) + 2.5 * sd(residuals(lmer.select.fixed.effects01a)) & residuals(lmer.select.fixed.effects01a) > mean(residuals(lmer.select.fixed.effects01a)) - 2.5 * sd(residuals(lmer.select.fixed.effects01a))),]
lmer.select.fixed.effects01b = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.fixed.effects01b) # -228.4
drop1(lmer.select.fixed.effects01b)


############### Random effects #####

#|set

lmer.select.random.effects.final.01 = lmer(log_rt ~ (group*red*p_dutch + trial + log(duration) + log_preceding_rt)  + (1|set) + (1|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.final.01) # 57.4

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1+group|set) + (1|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 60.1

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1+red|set) + (1|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 60.17

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1+voctest_correctness|set) + (1|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 62.36

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1+log_preceding_rt|set) + (1|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 61.6

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1+trial|set) + (1|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 60.13

# |subject

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+red|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 59.33

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+p_dutch|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 61.22

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+voctest_correctness|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 60.3

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 48.84

lmer.select.random.effects.final.02 = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.final.02) # 48.84

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial+log(duration)|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 51.67

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial+log_preceding_rt|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 49.45

### lose fixed effect 01

lmer.select.random.effects.final.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.random.effects.final.a) # -20.56
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.random.effects.final.a) < mean(residuals(lmer.select.random.effects.final.a)) + 2.5 * sd(residuals(lmer.select.random.effects.final.a)) & residuals(lmer.select.random.effects.final.a) > mean(residuals(lmer.select.random.effects.final.a)) - 2.5 * sd(residuals(lmer.select.random.effects.final.a))),]
lmer.select.random.effects.final.b = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.random.effects.final.b) #-243.5
drop1(lmer.select.random.effects.final.b)
lmer.select.random.effects.final.c = update(lmer.select.random.effects.final.b, ~. -group:red:p_dutch);
anova(lmer.select.random.effects.final.b, lmer.select.random.effects.final.c)

lmer.select.random.effects.final.a = lmer(log_rt ~ (group*red + group*p_dutch + red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.random.effects.final.a) # -18.25
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.random.effects.final.a) < mean(residuals(lmer.select.random.effects.final.a)) + 2.5 * sd(residuals(lmer.select.random.effects.final.a)) & residuals(lmer.select.random.effects.final.a) > mean(residuals(lmer.select.random.effects.final.a)) - 2.5 * sd(residuals(lmer.select.random.effects.final.a))),]
lmer.select.random.effects.final.b = lmer(log_rt ~ (group*red + group*p_dutch + red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.random.effects.final.b) #-245.1
drop1(lmer.select.random.effects.final.b)
lmer.select.random.effects.final.c = update(lmer.select.random.effects.final.b, ~. -group:p_dutch);
anova(lmer.select.random.effects.final.b, lmer.select.random.effects.final.c)

lmer.select.random.effects.final.a = lmer(log_rt ~ (group*red + red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.random.effects.final.a) # -18.98
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.random.effects.final.a) < mean(residuals(lmer.select.random.effects.final.a)) + 2.5 * sd(residuals(lmer.select.random.effects.final.a)) & residuals(lmer.select.random.effects.final.a) > mean(residuals(lmer.select.random.effects.final.a)) - 2.5 * sd(residuals(lmer.select.random.effects.final.a))),]
lmer.select.random.effects.final.b = lmer(log_rt ~ (group*red + red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.random.effects.final.b) #-247.1
drop1(lmer.select.random.effects.final.b)
lmer.select.random.effects.final.c = update(lmer.select.random.effects.final.b, ~. -red:p_dutch);
anova(lmer.select.random.effects.final.b, lmer.select.random.effects.final.c)

lmer.select.random.effects.final.a = lmer(log_rt ~ (group*red + p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.random.effects.final.a) # -16.95
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.random.effects.final.a) < mean(residuals(lmer.select.random.effects.final.a)) + 2.5 * sd(residuals(lmer.select.random.effects.final.a)) & residuals(lmer.select.random.effects.final.a) > mean(residuals(lmer.select.random.effects.final.a)) - 2.5 * sd(residuals(lmer.select.random.effects.final.a))),]
lmer.select.random.effects.final.b = lmer(log_rt ~ (group*red + p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.random.effects.final.b) #-238.2
drop1(lmer.select.random.effects.final.b)
lmer.select.random.effects.final.c = update(lmer.select.random.effects.final.b, ~. -group:red);
anova(lmer.select.random.effects.final.b, lmer.select.random.effects.final.c)

lmer.select.random.effects.final.a = lmer(log_rt ~ (group + red + p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.random.effects.final.a) # -17.45
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.random.effects.final.a) < mean(residuals(lmer.select.random.effects.final.a)) + 2.5 * sd(residuals(lmer.select.random.effects.final.a)) & residuals(lmer.select.random.effects.final.a) > mean(residuals(lmer.select.random.effects.final.a)) - 2.5 * sd(residuals(lmer.select.random.effects.final.a))),]
lmer.select.random.effects.final.b = lmer(log_rt ~ (group + red + p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.random.effects.final.b) #-230.3
drop1(lmer.select.random.effects.final.b)
lmer.select.random.effects.final.c = update(lmer.select.random.effects.final.b, ~. -red);
anova(lmer.select.random.effects.final.b, lmer.select.random.effects.final.c)

lmer.select.random.effects.final.a = lmer(log_rt ~ (group + p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.random.effects.final.a) # -19.44
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.random.effects.final.a) < mean(residuals(lmer.select.random.effects.final.a)) + 2.5 * sd(residuals(lmer.select.random.effects.final.a)) & residuals(lmer.select.random.effects.final.a) > mean(residuals(lmer.select.random.effects.final.a)) - 2.5 * sd(residuals(lmer.select.random.effects.final.a))),]
lmer.select.random.effects.final.b = lmer(log_rt ~ (group + p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.random.effects.final.b) #-232.1
drop1(lmer.select.random.effects.final.b)
lmer.select.random.effects.final.c = update(lmer.select.random.effects.final.b, ~. -group);
anova(lmer.select.random.effects.final.b, lmer.select.random.effects.final.c)

lmer.select.random.effects.final.a = lmer(log_rt ~ (p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.random.effects.final.a) # -20.37
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.random.effects.final.a) < mean(residuals(lmer.select.random.effects.final.a)) + 2.5 * sd(residuals(lmer.select.random.effects.final.a)) & residuals(lmer.select.random.effects.final.a) > mean(residuals(lmer.select.random.effects.final.a)) - 2.5 * sd(residuals(lmer.select.random.effects.final.a))),]
lmer.select.random.effects.final.b = lmer(log_rt ~ (p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.random.effects.final.b) #-232.6
drop1(lmer.select.random.effects.final.b)
lmer.select.random.effects.final.c = update(lmer.select.random.effects.final.b, ~. -p_dutch);
anova(lmer.select.random.effects.final.b, lmer.select.random.effects.final.c)

lmer.select.random.effects.final.a = lmer(log_rt ~ (voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.random.effects.final.a) # -17.57
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.random.effects.final.a) < mean(residuals(lmer.select.random.effects.final.a)) + 2.5 * sd(residuals(lmer.select.random.effects.final.a)) & residuals(lmer.select.random.effects.final.a) > mean(residuals(lmer.select.random.effects.final.a)) - 2.5 * sd(residuals(lmer.select.random.effects.final.a))),]
lmer.select.random.effects.final.b = lmer(log_rt ~ (voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.random.effects.final.b) #-224.5
drop1(lmer.select.random.effects.final.b)
lmer.select.random.effects.final.c = update(lmer.select.random.effects.final.b, ~. -log(duration));
anova(lmer.select.random.effects.final.b, lmer.select.random.effects.final.c)

lmer.select.random.effects.final.a = lmer(log_rt ~ (voctest_correctness + trial + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.random.effects.final.a) # -16.04
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.random.effects.final.a) < mean(residuals(lmer.select.random.effects.final.a)) + 2.5 * sd(residuals(lmer.select.random.effects.final.a)) & residuals(lmer.select.random.effects.final.a) > mean(residuals(lmer.select.random.effects.final.a)) - 2.5 * sd(residuals(lmer.select.random.effects.final.a))),]
lmer.select.random.effects.final.b = lmer(log_rt ~ (voctest_correctness + trial + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.random.effects.final.b) #-222.9
drop1(lmer.select.random.effects.final.b)

final.model = lmer(log_rt ~ (voctest_correctness + log(duration) + trial + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = TRUE);
summary(final.model)
###########################################################
s = d.sub.06[d.sub.06$red == "r"]


## final model, spli, red == "u"

final.model = lmer(log_rt ~ (log_preceding_rt + trial + log(duration))  + (1|set) + (1+log_preceding_rt|subject), data = d.sub.06.01, REML = TRUE);
summary(final.model) # - 272.5

# Test for homogeneous Variances and normal distribution of residuals (?)

plot(residuals(final.model) ~ fitted(final.model)) # homogeneouys variance of residuals?
qqnorm(residuals(final.model)) # residuals normally distributed?

