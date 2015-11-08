################### Statistical Modeling - ALL ################################
########################################################################
library(languageR)
library(lme4)
library(Hmisc)
# 1. Comparison between +spelling and -spelling group:
# vocabulary test, average proficiency from questionnaires,
# years of French in school, lextale
## vocabulary test
table(xp2_voctest$voctest_correctness,xp2_voctest$spelling)
prop.table(table(xp2_voctest$voctest_correctness,xp2_voctest$spelling),2)
prop.table(table(xp2_voctest$voctest_correctness))
chisq.test(table(xp2_voctest$voctest_correctness,xp2_voctest$spelling))
## average proficiency from questionnaires
mean(xp2_questinf$av_prof[xp2_questinf$spelling=="-spelling"])
mean(xp2_questinf$av_prof[xp2_questinf$spelling=="+spelling"])
t.test(xp2_questinf$av_prof[xp2_questinf$spelling=="-spelling"], 
       xp2_questinf$av_prof[xp2_questinf$spelling=="+spelling"])
## Years of French
mean(xp2_questinf$jaar[xp2_questinf$spelling=="-spelling"])
mean(xp2_questinf$jaar[xp2_questinf$spelling=="+spelling"])
t.test(xp2_questinf$jaar[xp2_questinf$spelling=="-spelling"], 
       xp2_questinf$jaar[xp2_questinf$spelling=="+spelling"])
## Lextale
get_score<-function(d) {
  correct.words<-length(which(d$Stimulus.ACC == 1 & d$CorrectRight == 5))
  incorrect.pseudo<-length(which(d$Stimulus.ACC == 0 & d$CorrectRight == 1))
  score<-correct.words - (2*incorrect.pseudo)
  score
}
spelling<-xp2_lextale[xp2_lextale$spelling=="+spelling",]
nospelling<-xp2_lextale[xp2_lextale$spelling=="-spelling",]
spelling$subject_oexp<-factor(spelling$subject_oexp)
nospelling$subject_oexp<-factor(spelling$subject_oexp)
score_all<-sapply(split(xp2_lextale,
                        xp2_lextale$subject_oexp), get_score)
mean(score_all)
range(score_all)
score1<-sapply(split(nospelling,
                     nospelling$subject_oexp), get_score)
mean(score1)
score2<-sapply(split(spelling,
                     spelling$subject_oexp), get_score)
mean(score2)
t.test(score1, score2)
# 2. Linear regressions: Accuracy
## Backfit 1
m1 = glmer(correctness ~ (red*spelling)  + (1|set) 
          + (1|subject_oexp), data = xp2.sub.03, family = binomial(logit))
summary(m1)
drop1(m1)
m2 = update(m1, ~. -red:spelling)
summary(m2)
anova(m1, m2)
drop1(m2)
m3 = update(m2, ~. -spelling)
summary(m3)
anova(m2, m3)
drop1(m3)
## Forward fit 1
m3a = glmer(correctness ~ (red)
           + (1+red|set) + (1|subject_oexp),family=binomial(logit),
           data = xp2.sub.03);
AIC(m3a)
m3a = glmer(correctness ~ (red)
            + (1|set) + (1+red|subject_oexp),family=binomial(logit),
            data = xp2.sub.03);
AIC(m3a)

final_model<-glmer(correctness ~ (red)
                   + (1|set) + (1+red|subject_oexp),family=binomial(logit),
                   data = xp2.sub.03);
summary(final_model)
# 3. Linear regression: RTs
## Backfit 1
m1<-lmer(log_rt ~ (red*spelling+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject_oexp), data = xp2.sub.04)
d<-xp2.sub.04[which(residuals(m1) < mean(residuals(m1))
                    +2.5 * sd(residuals(m1)) & residuals(m1)
                    > mean(residuals(m1)) - 2.5 * 
                      sd(residuals(m1))),]
m1<-lmer(log_rt ~ (red*spelling+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject_oexp), data = d)
summary(m1)
drop1(m1)
m2<-lmer(log_rt ~ (red*spelling+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject_oexp), data = xp2.sub.04)
d<-xp2.sub.04[which(residuals(m2) < mean(residuals(m2))
                    +2.5 * sd(residuals(m2)) & residuals(m2)
                    > mean(residuals(m2)) - 2.5 * 
                      sd(residuals(m2))),]
m2<-lmer(log_rt ~ (red*spelling+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject_oexp), data = d)
summary(m2)
drop1(m2)
## Forward fit 1
m1a<-lmer(log_rt ~ (red*spelling+
                         log(duration)+previous_log_rt)+(1|set)
          + (1|subject_oexp), data = xp2.sub.04)
AIC(m1a)
m2a<-lmer(log_rt ~ (red*spelling+
                      log(duration)+previous_log_rt)+(1+red|set)
          + (1|subject_oexp), data = xp2.sub.04)
AIC(m2a)
m3a<-lmer(log_rt ~ (red*spelling+
                      log(duration)+previous_log_rt)+(1+red+spelling|set)
          + (1|subject_oexp), data = xp2.sub.04)
AIC(m3a)
m4a<-lmer(log_rt ~ (red*spelling+
                      log(duration)+previous_log_rt)+(1+red+previous_log_rt|set)
          + (1|subject_oexp), data = xp2.sub.04)
AIC(m4a)
m5a<-lmer(log_rt ~ (red*spelling+
                      log(duration)+previous_log_rt)+(1+red+log(duration)|set)
          + (1|subject_oexp), data = xp2.sub.04)
AIC(m5a)
m6a<-lmer(log_rt ~ (red*spelling+
                      log(duration)+previous_log_rt)+(1+red|set)
          + (1+red|subject_oexp), data = xp2.sub.04)
AIC(m6a)
m7a<-lmer(log_rt ~ (red*spelling+
                      log(duration)+previous_log_rt)+(1+red|set)
          + (1+red|subject_oexp), data = xp2.sub.04)
AIC(m7a)
m8a<-lmer(log_rt ~ (red*spelling+
                      log(duration)+previous_log_rt)+(1+red|set)
          + (1+red+log(duration)|subject_oexp), data = xp2.sub.04)
AIC(m8a)
m9a<-lmer(log_rt ~ (red*spelling+
                      log(duration)+previous_log_rt)+(1+red|set)
          + (1+red+previous_log_rt|subject_oexp), data = xp2.sub.04)
AIC(m9a)
## Final model
m3<-lmer(log_rt ~ (red*spelling+
                     log(duration)+previous_log_rt)+(1+red|set)
         + (1+red+previous_log_rt|subject_oexp), data = xp2.sub.04)
d<-xp2.sub.04[which(residuals(m3) < mean(residuals(m3))
                    +2.5 * sd(residuals(m3)) & residuals(m3)
                    > mean(residuals(m3)) - 2.5 * 
                        sd(residuals(m3))),]
m3<-lmer(log_rt ~ (red*spelling+
                     log(duration)+previous_log_rt)+(1+red|set)
         + (1+red+previous_log_rt|subject_oexp), data = d)
summary(m3)
final_model<-m3