################### Statistical Modeling - ALL ################################
########################################################################
library(languageR)
library(lme4)
library(Hmisc)

# 1. Comparison between +spelling and -spelling group:
# vocabulary test, average proficiency from questionnaires,
# years of French in school, lextale
## vocabulary test
table(xp3_voctest$voctest_correctness,xp3_voctest$spelling)
prop.table(table(xp3_voctest$voctest_correctness))
prop.table(table(xp3_voctest$voctest_correctness,xp3_voctest$spelling),2)
chisq.test(table(xp3_voctest$voctest_correctness,xp3_voctest$spelling))
## average proficiency from questionnaires
mean(xp3_questinf$av_prof)
range(xp3_questinf$av_prof)
mean(xp3_questinf$av_prof[xp3_questinf$spelling=="-spelling"])
mean(xp3_questinf$av_prof[xp3_questinf$spelling=="+spelling"])
t.test(xp3_questinf$av_prof[xp3_questinf$spelling=="-spelling"], 
       xp3_questinf$av_prof[xp3_questinf$spelling=="+spelling"])
## Years of French
mean(xp3_questinf$jaar)
range(xp3_questinf$jaar)
mean(xp3_questinf$jaar[xp3_questinf$spelling=="-spelling"])
mean(xp3_questinf$jaar[xp3_questinf$spelling=="+spelling"])
t.test(xp3_questinf$jaar[xp3_questinf$spelling=="-spelling"], 
## Lextale
get_score<-function(d) {
  correct.words<-length(which(d$Stimulus.ACC == 1 & d$CorrectRight == 5))
  incorrect.pseudo<-length(which(d$Stimulus.ACC == 0 & d$CorrectRight == 1))
  score<-correct.words - (2*incorrect.pseudo)
  score
}
spelling<-xp3_lextale[xp3_lextale$spelling=="+spelling",]
spelling$subject<-factor(spelling$subject)
nospelling<-xp3_lextale[xp3_lextale$spelling=="-spelling",]
nospelling$subject<-factor(nospelling$subject)

score_all<-sapply(split(xp3_lextale,
                        xp3_lextale$subject), get_score)
#Lextale files of following two PPs were corrupted, therefore
#use mean score of all PPs
score_all["23"]<--1.56
score_all["3007"]<--1.56
str(score_all)
mean(score_all)
range(score_all)
score1<-sapply(split(nospelling,
                     nospelling$subject), get_score)
mean(score1)
score2<-sapply(split(spelling,
                     spelling$subject), get_score)
mean(score2)
t.test(score1, score2)
# 2. Linear regressions: Accuracy
## Backfit 1
m1 = glmer(correctness ~ (red*spelling
                         )  + (1|set) 
          + (1|subject), data = xp3.sub.03, family = binomial(logit))
summary(m1)
drop1(m1)
m2 = glmer(correctness ~ (red+spelling
)  + (1|set) 
+ (1|subject), data = xp3.sub.03, family = binomial(logit))
summary(m2)
drop1(m2)
m3 = glmer(correctness ~ (red
)  + (1|set) 
+ (1|subject), data = xp3.sub.03, family = binomial(logit))
summary(m3)
drop1(m3)
## Forward fit 1
m3b = glmer(correctness ~ (red
)  + (1+red|set) 
+ (1|subject), data = xp3.sub.03, family = binomial(logit))
summary(m3) # convergence failure
m3b = glmer(correctness ~ (red
)  + (1|set) 
+ (1+red|subject), data = xp3.sub.03, family = binomial(logit))
summary(m3)

final_model<- glmer(correctness ~ red + (1|set) 
  + (1|subject), data = xp3.sub.03, family = binomial(logit))
summary(final_model)
# Linear regression: RTs
## Backfit 1
m1<-lmer(log_rt ~ (red*spelling+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = xp3.sub.04)
d<-xp3.sub.04[which(residuals(m1) < mean(residuals(m1))
                    +2.5 * sd(residuals(m1)) & residuals(m1)
                    > mean(residuals(m1)) - 2.5 * 
                      sd(residuals(m1))),]
m1<-lmer(log_rt ~ (red*spelling+trial+
                       log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = d)
summary(m1)
drop1(m1)
m2<-lmer(log_rt ~ (red+spelling+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = xp3.sub.04)
d<-xp3.sub.04[which(residuals(m2) < mean(residuals(m2))
                    +2.5 * sd(residuals(m2)) & residuals(m2)
                    > mean(residuals(m2)) - 2.5 * 
                      sd(residuals(m2))),]
m2<-lmer(log_rt ~ (red+spelling+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = d)
summary(m2)
drop1(m2)
m3<-lmer(log_rt ~ (spelling+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = xp3.sub.04)
d<-xp3.sub.04[which(residuals(m3) < mean(residuals(m3))
                    +2.5 * sd(residuals(m3)) & residuals(m3)
                    > mean(residuals(m3)) - 2.5 * 
                      sd(residuals(m3))),]
m3<-lmer(log_rt ~ (spelling+trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = d)
summary(m3)
drop1(m3)
m4<-lmer(log_rt ~ (trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = xp3.sub.04)
d<-xp3.sub.04[which(residuals(m4) < mean(residuals(m4))
                    +2.5 * sd(residuals(m4)) & residuals(m4)
                    > mean(residuals(m4)) - 2.5 * 
                      sd(residuals(m4))),]
m4<-lmer(log_rt ~ (trial+
                     log(duration)+previous_log_rt)+(1|set)
         + (1|subject), data = d)
summary(m4)
## Forward fit 1
m4a<-lmer(log_rt ~ (trial+
                      log(duration)+previous_log_rt)+(1|set)
          + (1|subject), data = xp3.sub.04)
AIC(m4a)
m4b<-lmer(log_rt ~ (trial+
                      log(duration)+previous_log_rt)+(1+trial|set)
          + (1|subject), data = xp3.sub.04)
AIC(m4b)
m4c<-lmer(log_rt ~ (trial+
                      log(duration)+previous_log_rt)+(1+previous_log_rt|set)
          + (1|subject), data = xp3.sub.04)
AIC(m4c)
m4d<-lmer(log_rt ~ (trial+
                      log(duration)+previous_log_rt)+(1|set)
          + (1+trial|subject), data = xp3.sub.04)
AIC(m4d) # better but convergence error
m4e<-lmer(log_rt ~ (trial+
                      log(duration)+previous_log_rt)+(1|set)
          + (1+trial+previous_log_rt|subject), data = xp3.sub.04)
AIC(m4e)
## Final model
final_model <- m4
summary(m4)
