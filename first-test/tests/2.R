library(lattice)

# Load data
setwd('~/src/R-tests/first-test')
f <- read.csv('data/features.csv')
df <- data.frame(f)
df$X <- NULL
df$X.1 <- NULL
df$X.2 <- NULL
df$dataset <- NULL
df$isTree <- NULL
classifiers <- df$classifier

# ROC distribution
# Remove zeros and NA's
roc <- df[df$roc != 0, "roc"]
roc <- roc[!is.na(roc)]
histogram(roc)
