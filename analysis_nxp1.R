library(lme4)
library(LMERConvenienceFunctions)
library(lmtest)
# 1. Read in data
setwd(paste("W:/EXPERIMENTS/AUDITORY_RESTORED/REDUCTIES/SASCHA/Study_1/",
      "main experiment/Results - Compiled/nexp/data",sep=""))
## Read in lexicald decision(lexdec), vocabulary, (voctest),
## lextale, data from participant questionnaires (questinf),
## and phonotactic legality for each word (phonotactics)
xp1_lexdec <- read.delim("nxp1_lexdec.txt")
xp1_voctest <- read.delim("nxp1_voctest.txt")
xp1_lextale <- read.delim("nxp1_lextale.txt")
xp1_questinf <- read.delim("nxp1_questinf.txt")
phonotactics<-read.delim("phonotactics.txt")
# 2. Data preparation
## rename levels of Response Correctness, and Reduction for lexical decision task
xp1_lexdec$correctness<-as.factor(xp1_lexdec$correctness)
levels(xp1_lexdec$correctness)<-c("wrong","correct")
xp1_lexdec$red<-as.factor(xp1_lexdec$red)
levels(xp1_lexdec$red)<-c("reduced","full")
##only leave those subjects in questinf and voctest, and 
##lextale that are also in the lexdec and verify the correct 
##number of subjects (=54)
xp1_questinf<-xp1_questinf[xp1_questinf$subject_oexp%in%xp1_lexdec$subject_oexp,]
xp1_questinf$subject_oexp<-factor(xp1_questinf$subject_oexp)
length(unique(xp1_questinf$subject_oexp))
xp1_voctest<-xp1_voctest[xp1_voctest$subject_oexp%in%xp1_lexdec$subject_oexp,]
xp1_voctest$subject_oexp<-factor(xp1_voctest$subject_oexp)
length(unique(xp1_voctest$subject_oexp))
xp1_lextale<-xp1_lextale[xp1_lextale$subject_oexp%in%xp1_lexdec$subject_oexp,]
xp1_lextale$subject_oexp<-factor(xp1_lextale$subject_oexp)
length(unique(xp1_lextale$subject_oexp))
unique(xp1_lexdec$subject_oexp)[which(!(unique(xp1_lexdec$subject_oexp)%in%xp1_lextale$subject_oexp))]
## add "set"-column
xp1_voctest$set <- sapply(as.character(xp1_voctest$file), function(x) 
    strsplit(x, split = ("_"))[[1]][1])
xp1_lexdec$set <- sapply(as.character(xp1_lexdec$file), function(x) 
    strsplit(x, split = ("_"))[[1]][1])
## merge lexical decision data with vocabulary test, 
## questionnare info, and phonotactics
xp1_lexdec$order <- seq(1:nrow(xp1_lexdec))
xp1_voctest$subject_set <- paste(xp1_voctest$subject_oexp, xp1_voctest$set, 
                                 sep = "_")
xp1_lexdec$subject_set <- paste(xp1_lexdec$subject_oexp, xp1_lexdec$set, 
                                sep = "_")
xp1_lexdec <- merge(xp1_lexdec, 
                    xp1_voctest[,c("subject_set","voctest_correctness")],
                    by.x = "subject_set", by.y = "subject_set", all.x = TRUE)
xp1_lexdec<- merge(xp1_lexdec, 
                   xp1_questinf[,c("subject_oexp","jaar","av_prof")], 
                   by.x = "subject_oexp", by.y = "subject_oexp", all.x = TRUE)
xp1_lexdec<-merge(xp1_lexdec, phonotactics[,c("set","dutch","p_universally")], 
                  by.x = "set", by.y = "set", all.x = TRUE)
xp1_lexdec <- xp1_lexdec[order(xp1_lexdec$order),]
## add 'log_rt' column
xp1_lexdec$log_rt = log(xp1_lexdec$rt)
## add 'previous_rt' columns
previous_rt<-c()
for (i in 1:nrow(xp1_lexdec)) {
  if (xp1_lexdec$trial[i] == 1){
    previous_rt[i]<-round(mean(xp1_lexdec$rt[1:3]))
  }
  else{ 
    previous_rt[i]<-xp1_lexdec$rt[i-1]
  }
}
xp1_lexdec$previous_rt<-previous_rt
xp1_lexdec$previous_log_rt<-log(previous_rt)
## sub.01: remove high error target sets 
## (only consider reduced variants, because participants have 
##learnt the reduced variants))
lexdec.sets.correct<-prop.table(table(
    xp1_lexdec$correctness[xp1_lexdec$red=="reduced"&xp1_lexdec$is_target=="yes"], 
    xp1_lexdec$set[xp1_lexdec$red=="reduced"&xp1_lexdec$is_target=="yes"]),2)
lexdec.sets.correct
lexdec.sets.correct[,order(lexdec.sets.correct[2,])]
xp1.sub.01<-subset(xp1_lexdec, !(set %in% c("t13", "t04", "t12")))
xp1.sub.01$set<-factor(xp1.sub.01$set)
## sub.02: remove participants with an error rate higher 
## than 40 % (do not consider responses to full targets, 
## because participants have learnt the reduced variants)
lexdec.subj.correct<-prop.table(table(
    xp1.sub.01$correctness[!(xp1.sub.01$is_target=="yes"&xp1.sub.01$red=="full")], 
    xp1.sub.01$subject_oexp[!(xp1.sub.01$is_target=="yes"&xp1.sub.01$red=="full")]),2)
lexdec.subj.correct[,order(lexdec.subj.correct[2,])]
xp1.sub.02<-subset(xp1.sub.01, !(subject_oexp %in% c(
    "10_2")))
xp1.sub.02$subject_oexp<-factor(xp1.sub.02$subject_oexp)
# sub.03 (final data for accuracy analyses): only targets (no fillers)
xp1.sub.03 = subset(xp1.sub.02, is_target == "yes")
xp1.sub.03$set<-factor(xp1.sub.03$set)
## sub.04 ((final data for RT analyses): remove outliers and false responses
xp1.sub.04 = subset(xp1.sub.03, xp1.sub.03$log_rt < mean(xp1.sub.03$log_rt) + 
                    (2.5 * sd(xp1.sub.03$log_rt)) & xp1.sub.03$log_rt 
                > mean(xp1.sub.03$log_rt) - (2.5 * sd(xp1.sub.03$log_rt)))
xp1.sub.04 = subset(xp1.sub.04, correctness == "correct")
# 3. Descriptive Analyses of results
## 3.1 Define functions for plotting and tabulating results
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

get_acc_proportions <- function(d, v1, v2){
    
    props_1<-prop.table(table(
        d$correctness,v1,v2)[,1,],2)
    props_1<-round(props_1,2)
    props_2<-prop.table(table(
        d$correctness,v1,v2)[,2,],2)
    props_2<-round(props_2,2)
    error_props<-matrix(c(props_1["correct",],props_2["correct",]),
                        nrow=2,ncol=2,dimnames=list(levels(v2),levels(v1)))
    error_props
}

get_rts <- function(d,v1,v2){
   
rts <- round(tapply(d$rt, list(v2, v1), mean))
ttests<-tapply(d$rt, list(v2,v1), t.test)
confints_rts<-sapply(ttests, function(x){x$conf.int}) 
list(rts,confints_rts)
    
}

bar.col=c("red", "blue")

draw_plot<-function(d, confints, ylimits, label){
    mids<-barplot(d, col = bar.col, beside=T, ylab = label,
                           cex.axis =  1.5, cex.main = 2, cex.lab = 2, 
                           cex.names = 2, ylim = ylimits, lwd = 2,xpd=FALSE)
    text(mids, d-100, labels = d, pos=1, cex = 1)
    legend(2.75, 2000, dimnames(d)[[1]],fill = bar.col)
    error.bar(mids,d,confints)
}
## 3.2 Tabulate and plot Accuracy
### reduction vs. spelling
props_redSpelling<-get_acc_proportions(xp1.sub.03,xp1.sub.03$red, xp1.sub.03$spelling)
confints1<-get_prop_confints(xp1.sub.03$red,xp1.sub.03$spelling,
                                    xp1.sub.03$correctness,"reduced","correct")
confints2<-get_prop_confints(xp1.sub.03$red,xp1.sub.03$spelling,
                                      xp1.sub.03$correctness,"full","correct")
confints_props<-cbind(confints1,confints2)    
props_redSpelling
draw_plot(props_redSpelling,confints_props,c(0,1),"Accuracy")
### reduction vs. phonotactics
props_redPhontac<-get_acc_proportions(xp1.sub.03,xp1.sub.03$red, xp1.sub.03$p_universally)
confints1<-get_prop_confints(xp1.sub.03$red,xp1.sub.03$p_universally,
                                    xp1.sub.03$correctness,"reduced","correct")
confints2<-get_prop_confints(xp1.sub.03$red,xp1.sub.03$p_universally,
                                      xp1.sub.03$correctness,"full","correct")
confints_props<-cbind(confints1,confints2)    
props_redPhontac
draw_plot(props_redPhontac,confints_props,c(0,1),"Error Proportions")
### phonotactics vs. spelling
props_phontacSpelling<-get_acc_proportions(xp1.sub.03,xp1.sub.03$p_universally, xp1.sub.03$spelling)
confints1<-get_prop_confints(xp1.sub.03$p_universally,xp1.sub.03$spelling,
                             xp1.sub.03$correctness,"I","correct")
confints2<-get_prop_confints(xp1.sub.03$p_universally,xp1.sub.03$spelling,
                             xp1.sub.03$correctness,"L","correct")
confints_props<-cbind(confints1,confints2)    
props_phontacSpelling
draw_plot(props_phontacSpelling,confints_props,c(0,1),"Error Proportions")
## 3.3 Tabulate and Plot RTs
### reduction vs. spelling
rts_redSpelling<-get_rts(xp1.sub.04,xp1.sub.04$red, xp1.sub.04$spelling)
draw_plot(rts_redSpelling[[1]], rts_redSpelling[[2]],c(800,2000),label=("RT (ms)"))
### reduction vs. phonotactics
rts_redPhontac<-get_rts(xp1.sub.04,xp1.sub.04$red, xp1.sub.04$p_universally)
rts_redPhontac[[1]]
draw_plot(rts_redPhontac[[1]], rts_redPhontac[[2]],c(800,2000),label=("RT (ms)"))
### phonotactics vs. spelling
rts_phontacSpelling<-get_rts(xp1.sub.04,xp1.sub.04$p_universally, xp1.sub.04$spelling)
rts_phontacSpelling[[1]]
draw_plot(rts_phontacSpelling[[1]], rts_phontacSpelling[[2]],c(800,2000),label=("RT (ms)"))
