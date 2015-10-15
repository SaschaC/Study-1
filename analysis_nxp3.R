library(lme4)
library(LMERConvenienceFunctions)
library(lmtest)
#### Read in data ############
setwd(paste("W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/",
      "main experiment/Results - Compiled/nexp",sep=""))
xp3_lexdec <- read.delim("../oexp/xp3/day2_part1(lex_dec)/compiled_4.txt")
xp3_voctest <- read.delim("../oexp/xp3/day2_part2(voctest)/results_voctest_xp3.txt",head=FALSE)
xp3_lextale <- read.delim("../oexp/xp3/day2_part4(lextale)/results_lextale_xp3_all.txt")
xp3_questinf <- read.delim("../oexp/xp3//pp_questinfo_xp3.txt", dec=",")
phonotactics<-read.delim("phonotactics.txt")
#############################################
########## Data Preparation ####################
#############################################
# colnames voctest, pp_info
xp3_voctest = xp3_voctest[,-1]
colnames(xp3_voctest) = c("file", "a1", "a2", "a3", "a4", "correct_answer", "assistant", "participant", "subject", "group", "response", "response_word", "voctest_correctness" )
colnames(xp3_questinf)[1]<-"subject"
colnames(xp3_lextale)[1]<-"subject"
#only leave those subjects in questinf and voctest, and lextale that are also in the lexdec
#and verify correct N (54)
xp3_questinf<-xp3_questinf[xp3_questinf$subject%in%xp3_lexdec$subject,]
xp3_questinf$subject<-factor(xp3_questinf$subject)
length(unique(xp3_questinf$subject))
xp3_voctest<-xp3_voctest[xp3_voctest$subject%in%xp3_lexdec$subject,]
xp3_voctest$subject<-factor(xp3_voctest$subject)
length(unique(xp3_voctest$subject))
xp3_lextale<-xp3_lextale[xp3_lextale$subject%in%xp3_lexdec$subject,]
xp3_lextale$subject<-factor(xp3_lextale$subject)
length(unique(xp3_lextale$subject))
unique(xp3_lexdec$subject)[which(!(unique(xp3_lexdec$subject)%in%xp3_lextale$subject))]
#add spelling columns
xp3_lexdec$spelling<-as.factor(ifelse(xp3_lexdec$group==1,"-spelling","+spelling"))
xp3_voctest$spelling<-ifelse(xp3_voctest$group==1,"-spelling","+spelling")
xp3_lextale<-merge(xp3_lextale,unique(xp3_voctest[,c("subject","group")]), by.x="subject",by.y="subject",all.x=TRUE)
xp3_lextale$spelling<-ifelse(xp3_lextale$group==1,"-spelling","+spelling")
xp3_questinf<-merge(xp3_questinf,unique(xp3_voctest[,c("subject","group")]), by.x="subject",by.y="subject",all.x=TRUE)
xp3_questinf$spelling<-ifelse(xp3_questinf$group==1,"-spelling","+spelling")
### rename levels of response correctness, and reduction for lexical decision task
xp3_lexdec$correctness<-as.factor(xp3_lexdec$correctness)
levels(xp3_lexdec$correctness)<-c("wrong","correct")
xp3_lexdec$red<-as.factor(xp3_lexdec$red)
levels(xp3_lexdec$red)<-c("reduced","full")
### add "set"-column
xp3_voctest$set <- sapply(as.character(xp3_voctest$file), function(x) 
    strsplit(x, split = ("_"))[[1]][1])
xp3_lexdec$set <- sapply(as.character(xp3_lexdec$file), function(x) 
    strsplit(x, split = ("_"))[[1]][1])
# merge lexical decision with vocabulary test, questionnare info, and phonotactics
xp3_lexdec$order <- seq(1:nrow(xp3_lexdec))
xp3_voctest$subject_set <- paste(xp3_voctest$subject, xp3_voctest$set, 
                                 sep = "_")
xp3_lexdec$subject_set <- paste(xp3_lexdec$subject, xp3_lexdec$set, 
                                sep = "_")
xp3_lexdec <- merge(xp3_lexdec, 
                    xp3_voctest[,c("subject_set","voctest_correctness")],
                    by.x = "subject_set", by.y = "subject_set", all.x = TRUE)
xp3_lexdec<- merge(xp3_lexdec, 
                   xp3_questinf[,c("subject","jaar","av_prof")], 
                   by.x = "subject", by.y = "subject", all.x = TRUE)
xp3_lexdec<-merge(xp3_lexdec, phonotactics[,c("set","dutch","p_universally")], 
                  by.x = "set", by.y = "set", all.x = TRUE)
xp3_lexdec <- xp3_lexdec[order(xp3_lexdec$order),]
#### add log_rt column #####################
xp3_lexdec$log_rt = log(xp3_lexdec$rt)
### create previous rt columns ####
previous_rt<-c()
for (i in 1:nrow(xp3_lexdec)) {
  if (xp3_lexdec$trial[i] == 1){
    previous_rt[i]<-round(mean(xp3_lexdec$rt[1:3]))
  }
  else{ 
    previous_rt[i]<-xp3_lexdec$rt[i-1]
  }
}
xp3_lexdec$previous_rt<-previous_rt
xp3_lexdec$previous_log_rt<-log(previous_rt)
######### sub.01: remove high error sets 
table.sets.correct<-prop.table(table(
    xp3_lexdec$correctness[xp3_lexdec$red=="reduced"&xp3_lexdec$is_target=="yes"], 
    xp3_lexdec$set[xp3_lexdec$red=="reduced"&xp3_lexdec$is_target=="yes"]),2)
table.sets.correct
table.sets.correct[,order(table.sets.correct[2,])]
xp3.sub.01<-subset(xp3_lexdec, !(set %in% c("t13", "t04", "t07","t11")))
xp3.sub.01$set<-factor(xp3.sub.01$set)
######### sub.02: remove high error participants
table.subj.correct<-prop.table(table(
    xp3.sub.01$correctness[!(xp3.sub.01$is_target=="yes"&xp3.sub.01$red=="full")], 
    xp3.sub.01$subject[!(xp3.sub.01$is_target=="yes"&xp3.sub.01$red=="full")]),2)
table.subj.correct[,order(table.subj.correct[2,])]
xp3.sub.02<-subset(xp3.sub.01, !(subject %in% c(
    "31","3006","38","4","40")))
xp3.sub.02$subject<-factor(xp3.sub.02$subject)
#########sub.03: only targets #########
xp3.sub.03 = subset(xp3.sub.02, is_target == "yes")
xp3.sub.03$set<-factor(xp3.sub.03$set)
######### sub.04: remove outliers and false responses #####
xp3.sub.04 = subset(xp3.sub.03, xp3.sub.03$log_rt < mean(xp3.sub.03$log_rt) + 
                    (2.5 * sd(xp3.sub.03$log_rt)) & xp3.sub.03$log_rt 
                > mean(xp3.sub.03$log_rt) - (2.5 * sd(xp3.sub.03$log_rt)))
xp3.sub.04 = subset(xp3.sub.04, correctness == "correct")
###################################################################
###################################################################
###################################################################
get_prop_confints<-function(f,v1,v2,flevel,v2level) {
  
  t_sum<-table(v1[f==flevel])
  t_contingency<-table(v1[f==flevel],v2[f==flevel])
  confints<-matrix(c(prop.test(t_contingency[1,v2level],t_sum[1])
                     $conf.int,prop.test(t_contingency[2,v2level],t_sum[2])
                     $conf.int),2,2)
  dimnames(confints)<-list(c("bottom","top"),dimnames(t_contingency)[[1]])
  confints
}

error.bar <- function (x.coordinate, middle, interval, ...) {
  if (is.vector(interval)) { 
    # if only one vector is provided (because the interval is symmetric)
    arrows(x.coordinate, middle -(middle - interval), x.coordinate, middle + 
             (middle - interval), angle=90, code=3, length = 0.1, lwd =2, ...)
  } else {                   
    # if a data frame or matrix is provided (because the interval is not symmetric)
    arrows(x.coordinate, interval[1,], x.coordinate, interval[2,], 
           angle=90, code=3, length = 0.1, lwd =2)
  }
}

get_error_proportions <- function(d, v1, v2){
  
  props_1<-prop.table(table(
    d$correctness,v1,v2)[,1,],2)
  props_1<-round(props_1,2)
  props_2<-prop.table(table(
    d$correctness,v1,v2)[,2,],2)
  props_2<-round(props_2,2)
  error_props<-matrix(c(props_1["wrong",],props_2["wrong",]),
                      nrow=2,ncol=2,dimnames=list(levels(v2),levels(v1)))
  error_props
}

get_rts <- function(d,v1,v2){
  
  rts <- round(tapply(d$rt, list(v2, v1), mean))
  ttests<-tapply(d$rt, list(v2,v1), t.test)
  confints_rts<-sapply(ttests, function(x){x$conf.int}) 
  list(rts,confints_rts)
  
}

bar.col=c("grey40", "grey20")

draw_plot<-function(d, confints, ylimits, label){
  mids<-barplot(d, col = bar.col, beside=T, ylab = label,
                cex.axis =  1.5, cex.main = 2, cex.lab = 2, 
                cex.names = 2, ylim = ylimits, lwd = 2,xpd=FALSE)
  text(mids, d-0.05, labels = d, pos=1, cex = 1)
  legend(2.75, 1.0, dimnames(d)[[1]],fill = bar.col)
  error.bar(mids,d,confints)
}

# Props

props_redSpelling<-get_error_proportions(xp3.sub.03,xp3.sub.03$red, xp3.sub.03$spelling)
confints1<-get_prop_confints(xp3.sub.03$red,xp3.sub.03$spelling,
                             xp3.sub.03$correctness,"reduced","wrong")
confints2<-get_prop_confints(xp3.sub.03$red,xp3.sub.03$spelling,
                             xp3.sub.03$correctness,"full","wrong")
confints_props<-cbind(confints1,confints2)    
props_redSpelling
draw_plot(props_redSpelling,confints_props,c(0,1),"Error Proportions")

props_redPhontac<-get_error_proportions(xp3.sub.03,xp3.sub.03$red, xp3.sub.03$p_universally)
confints1<-get_prop_confints(xp3.sub.03$red,xp3.sub.03$p_universally,
                             xp3.sub.03$correctness,"reduced","wrong")
confints2<-get_prop_confints(xp3.sub.03$red,xp3.sub.03$p_universally,
                             xp3.sub.03$correctness,"full","wrong")
confints_props<-cbind(confints1,confints2)    
props_redPhontac
draw_plot(props_redPhontac,confints_props,c(0,1),"Error Proportions")

props_phontacSpelling<-get_error_proportions(xp3.sub.03,xp3.sub.03$p_universally, xp3.sub.03$spelling)
confints1<-get_prop_confints(xp3.sub.03$p_universally,xp3.sub.03$spelling,
                             xp3.sub.03$correctness,"I","wrong")
confints2<-get_prop_confints(xp3.sub.03$p_universally,xp3.sub.03$spelling,
                             xp3.sub.03$correctness,"L","wrong")
confints_props<-cbind(confints1,confints2)    
props_phontacSpelling
draw_plot(props_phontacSpelling,confints_props,c(0,1),"Error Proportions")

# RTs
rts_redSpelling<-get_rts(xp3.sub.03,xp3.sub.03$red, xp3.sub.03$spelling)
draw_plot(rts_redSpelling[[1]], rts_redSpelling[[2]],c(800,2000),label=("RT (ms)"))

rts_redPhontac<-get_rts(xp3.sub.03,xp3.sub.03$red, xp3.sub.03$p_universally)
rts_redPhontac[[1]]

rts_phontacSpelling<-get_rts(xp3.sub.03,xp3.sub.03$p_universally, xp3.sub.03$spelling)
rts_phontacSpelling[[1]]
### stats, group comparison
#age
mean(sapply(split(xp3_lexdec$age,xp3_lexdec$subject),unique))
range(sapply(split(xp3_lexdec$age,xp3_lexdec$subject),unique))
# vocabulary test
table(xp3_voctest$voctest_correctness,xp3_voctest$spelling)
prop.table(table(xp3_voctest$voctest_correctness))
prop.table(table(xp3_voctest$voctest_correctness,xp3_voctest$spelling),2)
chisq.test(table(xp3_voctest$voctest_correctness,xp3_voctest$spelling))
# av_prof
mean(xp3_questinf$av_prof)
range(xp3_questinf$av_prof)
mean(xp3_questinf$av_prof[xp3_questinf$spelling=="-spelling"])
mean(xp3_questinf$av_prof[xp3_questinf$spelling=="+spelling"])
t.test(xp3_questinf$av_prof[xp3_questinf$spelling=="-spelling"], 
       xp3_questinf$av_prof[xp3_questinf$spelling=="+spelling"])
#years French
mean(xp3_questinf$jaar)
range(xp3_questinf$jaar)
mean(xp3_questinf$jaar[xp3_questinf$spelling=="-spelling"])
mean(xp3_questinf$jaar[xp3_questinf$spelling=="+spelling"])
t.test(xp3_questinf$jaar[xp3_questinf$spelling=="-spelling"], 
       xp3_questinf$jaar[xp3_questinf$spelling=="+spelling"])
#lextale
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

score_all["23"]<--1.56
score_all["3007"]<--1.56
str(score_all)
mean(score_all)
range(score_all)

score1<-sapply(split(nospelling,
                     nospelling$subject), get_score)
score1["23"]<--1.56
score1["3007"]<--1.56
mean(score1)
score2<-sapply(split(spelling,
                     spelling$subject), get_score)
mean(score2)
t.test(score1, score2)

### stats lexical decision
#props
m0 = glmer(correctness ~ (spelling*red*p_universally+
                            voctest_correctness + trial)  + (1|set) + 
             (1|subject), family = binomial(logit), data = xp3.sub.03)
summary(m0)
m1<- bfFixefLMER_t.fnc(m0, reset.REML.TRUE = FALSE,t.threshold=1.97)
summary(m1)
m2 <- ffRanefLMER.fnc(model = m1, ran.effects = 
                        list(ran.intercepts = c("set","subject"), slopes = c(
                          "red", "p_universally","spelling","trial"), 
                          corr = c(1, 1,1,1), by.vars = c("set","subject")
                        )) 
summary(m2)
test<-glmer(correctness ~ spelling + red + p_universally + trial + (1+red|set) + 
              (1 | subject) + spelling:red + spelling:p_universally + 
              red:p_universally, family = binomial(logit), data = xp3.sub.03)
#RTs
m0 = lmer(log_rt ~ (spelling*red*p_universally + 
                      voctest_correctness + trial+previous_log_rt)  + (1|set) +  
            (1|subject), data = xp3.sub.04)
summary(m0)

m1<- bfFixefLMER_F.fnc(m0, reset.REML.TRUE = FALSE)

summary(m1)
m2 <- ffRanefLMER.fnc(model = m1, ran.effects = 
                        list(ran.intercepts = c("set", "subject"), slopes = c(
                          "red", "p_universally","previous_log_rt","trial"), 
                          corr = c(1, 1,1,1,1), by.vars = c("set", "subject")
                        )) 
summary(m2)
m3<-bfFixefLMER_F.fnc(m2, reset.REML.TRUE = FALSE)
summary(m3)
