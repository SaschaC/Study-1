rm(list=ls(all=TRUE))
###################################

# function for error bar

error.bar <- function (x.coordinate, middle, interval, ...) {
  if (is.vector(interval)) { # if only one vector is provided (because the interval is symmetric)
    arrows(x.coordinate, middle -(middle - interval) , x.coordinate, middle + (middle - interval), angle=90, code=3, length = 0.05, lwd =2, ...)
  } else {                   # if a data frame or matrix is provided (because the interval is not symmetric)
    arrows(x.coordinate, interval[,1], x.coordinate, interval[,2], angle=90, code=3, length = 0.05, lwd =2)
  }
}

##### voctest #########

voctest.results.sub.props = prop.table(matrix(xtabs(~ voctest.results.sub$voctest_correctness),ncol=1),2); voctest.results.sub.props
voctest.results.sub.props = prop.table(matrix(xtabs(~ voctest.results.sub$voctest_correctness + voctest.results.sub$group),ncol=2),2); voctest.results.sub.props

chisq.test(xtabs(~voctest.results.sub$voctest_correctness + voctest.results.sub$group))

##### Data exploration #################
########################################

### Overall RT distribution (sub.01 - [high error subjects excluded]) ####

raw_RT_hist = hist(sub.01$rt, xlab = "RT from Stimulus End" ); 
raw_RT_box = boxplot(sub.01$rt, main = "Distribution of RTs", cex.main = 3.5, ylab = "RT from Stimulus Beginning (ms)", cex.lab = 1.5, cex.axis = 1.5)

### single stimuli correct - words ##########

words = subset(sub.01, is_word == "yes")
words$stimulus = factor(words$stimulus)

pseudos = subset(sub.01, is_word == "no")
pseudos$stimulus = factor(pseudos$stimulus)
stimuli = as.character(levels(words$stimulus))

freq = vector(length = length(stimuli))
correct = words[words$correctness == "yes",]

for (i in 1:length(stimuli)) {
  
 freq[i] = sum(correct$stimulus == stimuli[i])
}
freq;

stimuli.correct = data.frame(stimuli, freq)

order.index = order(stimuli.correct$freq)
ordered.stimuli.correct = stimuli.correct[order.index,]; ordered.stimuli.correct;

####################### Plotting ###############################
################################################################
################################################################

###### Response proportions ###################

bar.col = c("white", "grey")

par(mfrow = c(1,1))
par(mar=c(3, 4.5, 2, 1) + 0.2)

#layout(mat=matrix(c(1,2,3),1),nrow(mat) = 2, height = c(0.3,1), width = c(1,1))
#par(xpd=T, mar=par()$mar+c(0,0,0,6))

plotdata = d.sub.04

response.proportions = prop.table(matrix(xtabs(~plotdata$correctness + plotdata$red + plotdata$group)[1:8], ncol=4),2);response.proportions
correct.proportions = matrix(response.proportions[2,], ncol = 2); correct.proportions

temp.experiment = c("no spelling", "no spelling", "spelling", "spelling")
temp.red = c("unreduced", "reduced", "unreduced", "reduced")



temp.n.correct = c(nrow(subset(plotdata, group == "1" & red == "u" & correctness == "yes")),+ 
                     nrow(subset(plotdata, group == "1" & red == "r" & correctness == "yes")),+ 
                     nrow(subset(plotdata, group == "2" & red == "u" & correctness == "yes")),+ 
                     nrow(subset(plotdata, group == "2" & red == "r" & correctness == "yes")))
temp.n.incorrect = c(nrow(subset(plotdata, group == "1" & red == "u" & correctness == "no")),+ 
                       nrow(subset(plotdata, group == "1" & red == "r" & correctness == "no")),+ 
                       nrow(subset(plotdata, group == "2" & red == "u" & correctness == "no")),+ 
                       nrow(subset(plotdata, group == "2" & red == "r" & correctness == "no")))

dataframe.responses = data.frame(temp.experiment, temp.red, temp.n.correct,temp.n.incorrect); dataframe.responses

dataframe.responses$props.correct = prop.table(matrix(c(dataframe.responses$temp.n.correct, dataframe.responses$temp.n.incorrect), ncol =2),1)[,1]
dataframe.responses$props.incorrect = prop.table(matrix(c(dataframe.responses$temp.n.correct, dataframe.responses$temp.n.incorrect), ncol =2),1)[,2]
dataframe.responses

dataframe.responses$cofint.bottom = c(prop.test(nrow(subset(plotdata, group == "1" & red == "u" & correctness == "yes")), nrow(subset(plotdata, group == "1" & red == "u")), conf.level = 0.95)$conf.int[1],+
                                        prop.test(nrow(subset(plotdata, group == "1" & red == "r" & correctness == "yes")), nrow(subset(plotdata, group == "1" & red == "r")), conf.level = 0.95)$conf.int[1],+
                                        prop.test(nrow(subset(plotdata, group == "2" & red == "u" & correctness == "yes")), nrow(subset(plotdata, group == "2" & red == "u")), conf.level = 0.95)$conf.int[1],+
                                        prop.test(nrow(subset(plotdata, group == "2" & red == "r" & correctness == "yes")), nrow(subset(plotdata, group == "2" & red == "r")), conf.level = 0.95)$conf.int[1])
dataframe.responses$cofint.upper = c(prop.test(nrow(subset(plotdata, group == "1" & red == "u" & correctness == "yes")), nrow(subset(plotdata, group == "1" & red == "u")), conf.level = 0.95)$conf.int[2],+
                                         prop.test(nrow(subset(plotdata, group == "1" & red == "r" & correctness == "yes")), nrow(subset(plotdata, group == "1" & red == "r")), conf.level = 0.95)$conf.int[2],+
                                           prop.test(nrow(subset(plotdata, group == "2" & red == "u" & correctness == "yes")), nrow(subset(plotdata, group == "2" & red == "u")), conf.level = 0.95)$conf.int[2],+
                                           prop.test(nrow(subset(plotdata, group == "2" & red == "r" & correctness == "yes")), nrow(subset(plotdata, group == "2" & red == "r")), conf.level = 0.95)$conf.int[2])


dataframe.responses
correct.proportions = matrix (dataframe.responses$props.correct, ncol = 2)

labelsize = 1.9
err.prop.mids<-barplot(correct.proportions, col = bar.col, beside=T, names.arg = c("-SP", "+SP"), ylab = "Accuracy", cex.axis =  labelsize, cex.main = labelsize, cex.lab = labelsize, cex.names = labelsize, ylim = c(0.2,1), lwd = 2, xpd=F, space = c(0.1, 0, 0.5,0))
text(err.prop.mids, correct.proportions-0.1, labels = round(correct.proportions,2), pos=1, cex = labelsize-0.5)
text(err.prop.mids, correct.proportions-0.09, labels = c(".87", ".80", ".82", ".74"), pos=1, cex = labelsize)


error.bar(err.prop.mids,dataframe.responses$props.correct,dataframe.responses[,7:8])
  
legend(1.5, 1, c("full", "reduced"),fill = bar.col)

####################### RT #####################

#(example, predicitiona with conf.int. of predictions)
lm.targ.red = lm (rt ~ targettype * red, data = d.sub.05)
preds.targ.red = expand.grid(targettype = levels(d.sub.05$targettype), red = levels(d.sub.05$red))
preds.targ.red[c("predictions", "lower", "upper")] = predict(lm.targ.red, newdata = preds.targ.red, interval = "confidence");preds.targ.red

rts = tapply(d.sub.06.01$rt, list(d.sub.06.01$red, d.sub.06.01$group), mean)[2:1,]; rts
str(tapply(d.sub.06$rt, d.sub.06$red, t.test))

confints = c(tapply(d.sub.06.01$rt, list(d.sub.06.01$red, d.sub.06.01$group), t.test)[[2]][["conf.int"]][1], +
tapply(d.sub.06.01$rt, list(d.sub.06.01$red, d.sub.06.01$group), t.test)[[1]][["conf.int"]][1], +
tapply(d.sub.06.01$rt, list(d.sub.06.01$red, d.sub.06.01$group), t.test)[[4]][["conf.int"]][1], +
tapply(d.sub.06.01$rt, list(d.sub.06.01$red, d.sub.06.01$group), t.test)[[3]][["conf.int"]][1])
                   
rts.mids<-barplot(rts, col = bar.col, beside=T, names.arg = c("-SP", "+SP"), ylab = "Reaction Time (ms)", cex.axis = labelsize, cex.main = labelsize, cex.lab = labelsize, cex.names = labelsize, lwd = 2, ylim = c(1000,1800), xpd = FALSE, space = c(0.1,0,0.5,0))
text(rts.mids, rts-200, labels = round(rts,0), pos=1, cex = labelsize)
error.bar(rts.mids,rts,confints)

legend(1.75, 1800, c("full", "reduced"),fill = bar.col)

box()

############### Targets - RTs ~ Subjects / Items #############
test 
interactionplot_all_items = interaction.plot(x.factor = test$red, trace.factor = test$subject, response = test$log_rt, legend = F, main = "Item slopes", cex.main = 2, cex.lab = 1.5, xlab = "Target Type", ylab = "RT from stimulus end")
interactionplot_all_items = interaction.plot(x.factor = d.sub.06$red, trace.factor = d.sub.06$subject, response = d.sub.06$log_rt, legend = F, main = "Subject slopes", cex.main = 2, cex.lab = 1.5, xlab = "Target Type", ylab = "RT from stimulus end")

interactionplot_group1_items = interaction.plot(x.factor = d.sub.06$targettype, trace.factor = d.sub.06_group1$set, response = d.sub.061$log_rt, legend = F, main = "Item slopes:\nGroup 1 (e.g., la pelouse)", cex.main = 2, cex.lab = 1.5, xlab = "Target Type", ylab = "RT from stimulus end")
interactionplot_group2_items = interaction.plot(x.factor = d.sub.06$targettype, trace.factor = d.sub.06_group2$set, response = d.sub.06$log_rt, legend = F, main = "Item slopes:\nGroup 2 (e.g., la p'louse)",cex.main = 2, cex.lab = 1.5, xlab = "Target Type", ylab = "RT from stimulus end")

interactionplot_group1_subjects = interaction.plot(x.factor = d.sub.06$targettype, trace.factor = d.sub.06$subject, response = d.sub.06_group1$log_rt, legend = F, main = "Subject slopes:\nGroup 1 (e.g., la pelouse)",cex.main = 2, cex.lab = 1.5, xlab = "Target Type", ylab = "RT from stimulus end")
interactionplot_group2_subjects = interaction.plot(x.factor = d.sub.06$targettype, trace.factor = d.sub.06$subject, response = d.sub.06_group2$log_rt, legend = F, main = "Subject slopes:\nGroup 2 (e.g., la p'louse)",cex.main = 2, cex.lab = 1.5, xlab = "Target Type", ylab = "RT from stimulus end")

### subject / items aggregate ###################

items1 = aggregate(sub.05$log_rt, list(sub.05$set, sub.05$targettype), mean)[1:24,]
items2 = aggregate(sub.05$log_rt, list(sub.05$set, sub.05$targettype), mean)[25:48,]
items = merge(items1, items2, by.x = "Group.1", by.y = "Group.1"); items

subjects1 = aggregate(sub.05$log_rt, list(sub.05$subject, sub.05$targettype), mean)[1:28,]
subjects2 = aggregate(sub.05$log_rt, list(sub.05$subject, sub.05$targettype), mean)[29:56,]
subjects = merge(subjects1, subjects2, by.x = "Group.1", by.y = "Group.1"); subjects

length(levels(sub.05$subject))