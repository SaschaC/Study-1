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

d.sub.05$group = factor(d.sub.05$group)

lmer.select.fixed.effects01 = lmer(correctness ~ (group*red*p_dutch + av_prof + voctest_correctness + trial + jaar)  + (1|set) + (1|subject), data = d.sub.05, family = binomial(logit), REML = FALSE)
summary(lmer.select.fixed.effects01) # 780.4
drop1(lmer.select.fixed.effects01)

lmer.select.fixed.effects02 = update(lmer.select.fixed.effects01 , ~. -av_prof);
summary(lmer.select.fixed.effects02) # 778.4
anova(lmer.select.fixed.effects01, lmer.select.fixed.effects02)
drop1(lmer.select.fixed.effects02)

lmer.select.fixed.effects03= update(lmer.select.fixed.effects02 , ~. -jaar);
summary(lmer.select.fixed.effects03) # 777
anova(lmer.select.fixed.effects02, lmer.select.fixed.effects03)
drop1(lmer.select.fixed.effects03)

lmer.select.fixed.effects04= update(lmer.select.fixed.effects03 , ~. -group:red:p_dutch);
summary(lmer.select.fixed.effects04) # 776.6
anova(lmer.select.fixed.effects03, lmer.select.fixed.effects04)
drop1(lmer.select.fixed.effects04)

lmer.select.fixed.effects05= update(lmer.select.fixed.effects04 , ~. -group:red);
summary(lmer.select.fixed.effects05) # 774.8
anova(lmer.select.fixed.effects04, lmer.select.fixed.effects05)
drop1(lmer.select.fixed.effects05)

lmer.select.fixed.effects06= update(lmer.select.fixed.effects05 , ~. -trial);
summary(lmer.select.fixed.effects06) # 775.3
anova(lmer.select.fixed.effects05, lmer.select.fixed.effects06)
drop1(lmer.select.fixed.effects06)

# Random |set

lmer.select.rand.effects.final.01 = lmer(correctness ~ group + red + p_dutch + voctest_correctness + (1|set) + (1|subject) + group:p_dutch + red:p_dutch, data = d.sub.05, family = binomial(logit));
summary(lmer.select.rand.effects.final.01) # 775.3

lmer.select.rand.effects.a = lmer(correctness ~ group + red + p_dutch + voctest_correctness + (1+red|set) + (1|subject) + group:p_dutch + red:p_dutch, data = d.sub.05, family = binomial(logit));
summary(lmer.select.rand.effects.a) # 776.3

lmer.select.rand.effects.a = lmer(correctness ~ group + red + p_dutch + voctest_correctness + (1+group|set) + (1|subject) + group:p_dutch + red:p_dutch, data = d.sub.05, family = binomial(logit));
summary(lmer.select.rand.effects.a) # 779.2

# Subject

lmer.select.rand.effects.a = lmer(correctness ~ group + red + p_dutch + voctest_correctness + (1|set) + (1+red|subject) + group:p_dutch + red:p_dutch, data = d.sub.05, family = binomial(logit));
summary(lmer.select.rand.effects.a) # 779.2

lmer.select.rand.effects.a = lmer(correctness ~ group + red + p_dutch + voctest_correctness + (1|set) + (1+p_dutch|subject) + group:p_dutch + red:p_dutch, data = d.sub.05, family = binomial(logit));
summary(lmer.select.rand.effects.a) # 779.2

lmer.select.rand.effects.a = lmer(correctness ~ group + red + p_dutch + voctest_correctness + (1|set) + (1+voctest_correctness|subject) + group:p_dutch + red:p_dutch, data = d.sub.05, family = binomial(logit));
summary(lmer.select.rand.effects.a) # 779.2

# final model Props

final.model = lmer(correctness ~ group + red*group + red*p_dutch + group*p_dutch + voctest_correctness + (1|set) + (1|subject), data = d.sub.05, family = binomial(logit));
summary(final.model) # 777.1
options(contrasts=c("contr.treatment", "contr.poly"))

#### split by red.type and p_dutch

sub.red = d.sub.05[d.sub.05$red =="r",]

sub.u = d.sub.05[d.sub.05$red =="u",]

sub.i = d.sub.05[d.sub.05$p_dutch =="I",]

sub.l = d.sub.05[d.sub.05$p_dutch =="L",]


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

d.sub.06$experiment = factor(d.sub.06$experiment)

lmer.select.fixed.effects01a = lmer(log_rt ~ (group*red*p_dutch + av_prof + voctest_correctness + trial + jaar + log(duration) + log_preceding_rt)  + (1|set) + (1|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.fixed.effects01a) # -34.46
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.fixed.effects01a) < mean(residuals(lmer.select.fixed.effects01a)) + 2.5 * sd(residuals(lmer.select.fixed.effects01a)) & residuals(lmer.select.fixed.effects01a) > mean(residuals(lmer.select.fixed.effects01a)) - 2.5 * sd(residuals(lmer.select.fixed.effects01a))),]
lmer.select.fixed.effects01b = lmer(log_rt ~ (group*red*p_dutch + av_prof + voctest_correctness + trial + jaar + log(duration) + log_preceding_rt)  + (1|set) + (1|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.fixed.effects01b) # -199.9
drop1(lmer.select.fixed.effects01b)
lmer.select.fixed.effects01c = update(lmer.select.fixed.effects01b, ~. -av_prof);
anova(lmer.select.fixed.effects01b, lmer.select.fixed.effects01c)

lmer.select.fixed.effects01a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + jaar + log(duration) + log_preceding_rt)  + (1|set) + (1|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.fixed.effects01a) # -32.46
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.fixed.effects01a) < mean(residuals(lmer.select.fixed.effects01a)) + 2.5 * sd(residuals(lmer.select.fixed.effects01a)) & residuals(lmer.select.fixed.effects01a) > mean(residuals(lmer.select.fixed.effects01a)) - 2.5 * sd(residuals(lmer.select.fixed.effects01a))),]
lmer.select.fixed.effects01b = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + jaar + log(duration) + log_preceding_rt)  + (1|set) + (1|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.fixed.effects01b) # -201.9
drop1(lmer.select.fixed.effects01b)
lmer.select.fixed.effects01c = update(lmer.select.fixed.effects01b, ~. -jaar);
anova(lmer.select.fixed.effects01b, lmer.select.fixed.effects01c)


############### Random effects #####

#|set

lmer.select.random.effects.final.01 = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.final.01) # 100.6

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1+group|set) + (1|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 100.9

lmer.select.random.effects.final.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1+red|set) + (1|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 101.9

lmer.select.random.effects.final.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1+voctest_correctness|set) + (1|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 103.8

lmer.select.random.effects.final.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1+log_preceding_rt|set) + (1|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 103.5

lmer.select.random.effects.final.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1+trial|set) + (1|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 103.4

# |subject

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+red|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 101.9

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+p_dutch|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 102.9

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+voctest_correctness|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 103.8

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+voctest_correctness|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 103.8

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 88.2

lmer.select.random.effects.final.02 = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.final.02) # 100.6

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial+log(duration)|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 92.14

lmer.select.random.effects.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial+log_preceding_rt|subject), data = d.sub.06, REML = TRUE);
summary(lmer.select.random.effects.a) # 88.83

### lose fixed effect 01

lmer.select.random.effects.final.a = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.random.effects.final.a) # -132.4
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.random.effects.final.a) < mean(residuals(lmer.select.random.effects.final.a)) + 2.5 * sd(residuals(lmer.select.random.effects.final.a)) & residuals(lmer.select.random.effects.final.a) > mean(residuals(lmer.select.random.effects.final.a)) - 2.5 * sd(residuals(lmer.select.random.effects.final.a))),]
lmer.select.random.effects.final.b = lmer(log_rt ~ (group*red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.random.effects.final.b) #-338.5
drop1(lmer.select.random.effects.final.b)
lmer.select.random.effects.final.c = update(lmer.select.random.effects.final.b, ~. -group:red:p_dutch);
anova(lmer.select.random.effects.final.b, lmer.select.random.effects.final.c)

lmer.select.random.effects.final.a = lmer(log_rt ~ (group*red + group*p_dutch + red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.random.effects.final.a) # -132.4
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.random.effects.final.a) < mean(residuals(lmer.select.random.effects.final.a)) + 2.5 * sd(residuals(lmer.select.random.effects.final.a)) & residuals(lmer.select.random.effects.final.a) > mean(residuals(lmer.select.random.effects.final.a)) - 2.5 * sd(residuals(lmer.select.random.effects.final.a))),]
lmer.select.random.effects.final.b = lmer(log_rt ~ (group*red + group*p_dutch + red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.random.effects.final.b) #-338.5
drop1(lmer.select.random.effects.final.b)
lmer.select.random.effects.final.c = update(lmer.select.random.effects.final.b, ~. -group:p_dutch);
anova(lmer.select.random.effects.final.b, lmer.select.random.effects.final.c)

lmer.select.random.effects.final.a = lmer(log_rt ~ (group*red + red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.random.effects.final.a) # -132.4
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.random.effects.final.a) < mean(residuals(lmer.select.random.effects.final.a)) + 2.5 * sd(residuals(lmer.select.random.effects.final.a)) & residuals(lmer.select.random.effects.final.a) > mean(residuals(lmer.select.random.effects.final.a)) - 2.5 * sd(residuals(lmer.select.random.effects.final.a))),]
lmer.select.random.effects.final.b = lmer(log_rt ~ (group*red + red*p_dutch + voctest_correctness + trial + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.random.effects.final.b) #-338.5
drop1(lmer.select.random.effects.final.b)
lmer.select.random.effects.final.c = update(lmer.select.random.effects.final.b, ~. -trial);
anova(lmer.select.random.effects.final.b, lmer.select.random.effects.final.c)

lmer.select.random.effects.final.a = lmer(log_rt ~ (group*red + red*p_dutch + voctest_correctness + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.random.effects.final.a) # -132.4
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.random.effects.final.a) < mean(residuals(lmer.select.random.effects.final.a)) + 2.5 * sd(residuals(lmer.select.random.effects.final.a)) & residuals(lmer.select.random.effects.final.a) > mean(residuals(lmer.select.random.effects.final.a)) - 2.5 * sd(residuals(lmer.select.random.effects.final.a))),]
lmer.select.random.effects.final.b = lmer(log_rt ~ (group*red + red*p_dutch + voctest_correctness + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.random.effects.final.b) #-338.5
drop1(lmer.select.random.effects.final.b)
lmer.select.random.effects.final.c = update(lmer.select.random.effects.final.b, ~. -group:red);
anova(lmer.select.random.effects.final.b, lmer.select.random.effects.final.c)

lmer.select.random.effects.final.a = lmer(log_rt ~ (red*p_dutch + voctest_correctness + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.random.effects.final.a) # -132.4
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.random.effects.final.a) < mean(residuals(lmer.select.random.effects.final.a)) + 2.5 * sd(residuals(lmer.select.random.effects.final.a)) & residuals(lmer.select.random.effects.final.a) > mean(residuals(lmer.select.random.effects.final.a)) - 2.5 * sd(residuals(lmer.select.random.effects.final.a))),]
lmer.select.random.effects.final.b = lmer(log_rt ~ (red*p_dutch + voctest_correctness + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.random.effects.final.b) #-338.5
drop1(lmer.select.random.effects.final.b)
lmer.select.random.effects.final.c = update(lmer.select.random.effects.final.b, ~. -red:p_dutch);
anova(lmer.select.random.effects.final.b, lmer.select.random.effects.final.c)

lmer.select.random.effects.final.a = lmer(log_rt ~ (voctest_correctness + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06, REML = FALSE);
summary(lmer.select.random.effects.final.a) # -132.4
d.sub.06.01 = d.sub.06[which(residuals(lmer.select.random.effects.final.a) < mean(residuals(lmer.select.random.effects.final.a)) + 2.5 * sd(residuals(lmer.select.random.effects.final.a)) & residuals(lmer.select.random.effects.final.a) > mean(residuals(lmer.select.random.effects.final.a)) - 2.5 * sd(residuals(lmer.select.random.effects.final.a))),]
lmer.select.random.effects.final.b = lmer(log_rt ~ (voctest_correctness + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = FALSE);
summary(lmer.select.random.effects.final.b) #-338.5
drop1(lmer.select.random.effects.final.b)

# final model rts

final.model = lmer(log_rt ~ (voctest_correctness + log(duration) + log_preceding_rt)  + (1|set) + (1+trial|subject), data = d.sub.06.01, REML = FALSE);
summary(final.model)
###########################################################
s = d.sub.06[d.sub.06$red == "r"]


## final model, spli, red == "u"

final.model = lmer(log_rt ~ (log_preceding_rt + trial + log(duration))  + (1|set) + (1+log_preceding_rt|subject), data = d.sub.06.01, REML = TRUE);
summary(final.model) # - 272.5

# Test for homogeneous Variances and normal distribution of residuals (?)

plot(residuals(final.model) ~ fitted(final.model)) # homogeneouys variance of residuals?
qqnorm(residuals(final.model)) # residuals normally distributed?

