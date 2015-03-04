setwd('first-test')
f <- read.csv('data/features.csv')
df <- data.frame(f)
df$X <- NULL
df$X.1 <- NULL
df$X.2 <- NULL
df$dataset <- NULL
df$isTree <- NULL
classifiers <- df$classifier
df$classifier <- NULL
c <- cor(df, use="na.or.complete")
c <- cor(df[,-c(16:18)], use="complete.obs")
c <- cor(df[4:nrow(df),], use="complete.obs")

# Correlations of f and roc
# ROC
c[,ncol(c)]
# f-measure
c[,ncol(c)-1]

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

# Predicting classifier (factorial) with the rest (continuous)
# Answer: Multinomial Logistic Regression
mlr <- multinom(classifiers ~ totalSize + nSnapshots + avgGap + avgSize + nInserts + nDeletes + nComm + maxTreeDepth + avgTreeDepth + totalInstances + ratioInstances + ratioInstancesVSIS + totalStructural + ratioStructural + ratioStructuralVSIS, data = df)
summary(mlr)
z <- summary(mlr)$coefficients/summary(mlr)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

# linear models
rf <- lm(f ~ totalSize + nSnapshots + avgGap + avgSize + nInserts + nDeletes + nComm + maxTreeDepth + avgTreeDepth + totalInstances + ratioInstances + ratioInstancesVSIS + totalStructural + ratioStructural + ratioStructuralVSIS, data = df)
rr <- lm(roc ~ totalSize + nSnapshots + avgGap + avgSize + nInserts + nDeletes + nComm + maxTreeDepth + avgTreeDepth + totalInstances + ratioInstances + ratioInstancesVSIS + totalStructural + ratioStructural + ratioStructuralVSIS, data = df[4:nrow(df),])

# Multicolinialitat per tot arreu! Treiem variables que depenen d'altres; construccio iterativa
rf.0 <- lm(f ~ log(nSnapshots) + log(nInserts) , data=df)

rr.0 <- lm(roc ~ log(nSnapshots), data=df)
rr.1 <- lm(roc ~ log(nSnapshots) + log(avgGap) + log(totalSize) + avgTreeDepth + ratioInstances + ratioStructural, data=df)
rr.1b <- lm(roc ~ log(nSnapshots) + log(avgGap) + log(totalSize) + avgTreeDepth + ratioInstancesVSIS, data=df)

# Comparar models
stargazer(rr.0, rr.1, rr.1b, type='text', no.space=TRUE)

# Multi-collinearity
vif(rr.1c)
sqrt(vif(rr.1)) > 2 # problem?

# Ara amb multinomial per variable dependent categorica
rm.0 <- multinom(classifier ~ log(nSnapshots), data=df)
rm.1 <- multinom(classifier ~ log(nSnapshots) + log(avgGap) + log(totalSize) + avgTreeDepth + ratioInstances + ratioStructural, data=df)
rm.1b <- multinom(classifier ~ log(nSnapshots) + log(avgGap) + log(totalSize) + avgTreeDepth + ratioInstancesVSIS, data=df)

# Masses categories a la categorica de resposta; rebaixar. La recodifiquem.
df$classifier2[df$classifier == "weka.classifiers.bayes.NaiveBayes" | 
                       df$classifier == "weka.classifiers.bayes.BayesNet"] <- "bayes"
df$classifier2[df$classifier == "weka.classifiers.functions.MultilayerPerceptron" | 
                 df$classifier == "weka.classifiers.functions.SimpleLogistic" |
                 df$classifier == "weka.classifiers.functions.Logistic" |
                 df$classifier == "weka.classifiers.functions.SGD" |
                 df$classifier == "weka.classifiers.functions.VotedPerceptron"] <- "functions"
df$classifier2[df$classifier == "weka.classifiers.rules.DecisionTable" | 
                 df$classifier == "weka.classifiers.rules.OneR" |
                 df$classifier == "weka.classifiers.rules.JRip"] <- "rules"
df$classifier2[df$classifier == "weka.classifiers.trees.HoeffdingTree" | 
                 df$classifier == " weka.classifiers.trees.RandomForest" |
                 df$classifier == "weka.classifiers.trees.DecisionStump" |
                 df$classifier == "weka.classifiers.trees.J48"] <- "trees"

# Apliquem la categorica de resposta recofificada
rm.0 <- multinom(classifier2 ~ log(nSnapshots), data=df)
rm.1 <- multinom(classifier2 ~ log(nSnapshots) + log(avgGap) + log(totalSize) + avgTreeDepth + ratioInstances + ratioStructural, data=df)
rm.1b <- multinom(classifier2 ~ log(nSnapshots) + log(avgGap) + log(totalSize) + avgTreeDepth + ratioInstancesVSIS, data=df)

# Com que tot es fals no hi ha problema; en cas contrari treure variables

stargazer(rf, rr, type="html", align=TRUE)

# Plot dels coeficients amb el 2*sd (per veure si trepitgen el 0)
library(arm)
pdf("graphs/coefs_linearmodel.pdf", width=7)
coefplot(rr.1, main="", xlim=c(-4,2), mar=c(1,6,5.1,2))
dev.off()

# El multinomial millor la taula

# Les funcions es poden aproximar tan com vulguis amb polinomis...

# Ara fem SIMULACIO (mostrar resultats dels models)
library(effects)

plot(effect('log(totalSize)', rm.1))
# Tot en un!!
plot(effect('log(avgGap)', rm.1), confint=FALSE, rug=FALSE)
plot(allEffects(rm.1), rug=FALSE)

# Ara les linials
pdf("graphs/treedepth_vs_roc.pdf")
plot(effect('avgTreeDepth', rr.1), main="", rug=FALSE, xlab="Input KOS chain average tree depth", ylab="Area under ROC")
dev.off()
plot(effect('ratioStructural', rr.1), main="", rug=FALSE, xlab="Input KOS structural property ratio", ylab="Area under ROC")

# Important: "keeping the other predictors constant, the average effect of ratioStructural on ROC is -1.56
# La resta de variables actuen com a variables de control, i.e. estan fixes en les seves mitjanes

e <- effect('ratioStructural', rr.1)

# Provem si ratioInserts millora els models
df$ratioInserts <- df$nInserts / (df$nInserts + df$nDeletes + df$nComm)
df$ratioDeletes <- df$nDeletes / (df$nInserts + df$nDeletes + df$nComm)
df$ratioComm <- df$nComm / (df$nInserts + df$nDeletes + df$nComm)

cor.test(df$ratioInserts, df$nDeletes)
cor.test(df$ratioDeletes, df$nDeletes)
cor.test(df$ratioComm, df$nDeletes)

rr.1 <- lm(roc ~ log(nSnapshots) + log(avgGap) + log(totalSize) + avgTreeDepth + ratioInstances + ratioStructural, data=df)
rr.2 <- lm(roc ~ log(nSnapshots) + log(avgGap) + log(totalSize) + avgTreeDepth + ratioInstances + ratioStructural + ratioInserts, data=df)
rr.3 <- lm(roc ~ log(nSnapshots) + log(avgGap) + log(totalSize) + avgTreeDepth + ratioInstances + ratioStructural + ratioDeletes, data=df)
rr.4 <- lm(roc ~ log(nSnapshots) + log(avgGap) + log(totalSize) + avgTreeDepth + ratioInstances + ratioStructural + ratioComm, data=df)
rr.5 <- lm(roc ~ log(nSnapshots) + log(avgGap) + log(totalSize) + avgTreeDepth + ratioInstances + ratioStructural + ratioInserts + ratioComm, data=df)
stargazer(rr.2, rr.3, rr.4, type='text', no.space=TRUE)
stargazer(rr.1, rr.2, rr.3, rr.4, rr.5, type='text', no.space=TRUE)

# Hem millorat amb inserts i comms!
png("graphs/coefs_linearmodel_ratioInserts.png", width=800)
coefplot(rr.2, main="", xlim=c(-4,2), mar=c(1,6,5.1,2))
dev.off()

rm.2 <- multinom(classifier2 ~ log(nSnapshots) + log(avgGap) + log(totalSize) + avgTreeDepth + ratioInstances + ratioStructural + ratioInserts, data=df)
rm.3 <- multinom(classifier2 ~ log(nSnapshots) + log(avgGap) + log(totalSize) + avgTreeDepth + ratioInstances + ratioStructural + ratioDeletes, data=df)
rm.4 <- multinom(classifier2 ~ log(nSnapshots) + log(avgGap) + log(totalSize) + avgTreeDepth + ratioInstances + ratioStructural + ratioComm, data=df)
stargazer(rm.2, rm.3, rm.4, type='text', no.space=TRUE)
