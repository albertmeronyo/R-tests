library(lattice)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(grid)

fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=12) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

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
roc <- df[df$roc >= 0.5, "roc"]
roc <- roc[!is.na(roc)]
histogram(roc)
plot(density(roc))
hist(roc, freq=FALSE, breaks=10, main="Density plot", xlab="QoE", col="lightgreen")
curve(dnorm(roc, mean=mean(roc), sd=sd(roc)), col="darkblue", lwd=2)
ggplot(df[df$roc >= 0.5,], aes(roc)) + geom_histogram(binwidth=0.05)
ggplot(df[df$roc >= 0.5,], aes(roc)) + geom_histogram(fill="#c0392b", alpha=0.75, binwidth=0.05) + fte_theme() + labs(title="Distribution of QoE in Web vocabularies", x="auc", y="freq") + scale_x_continuous(breaks=seq(0,50, by=5)) + scale_y_continuous(labels=comma) + geom_hline(yintercept=0, size=0.4, color="black")
foo <- df[df$roc >= 0.5,]
foo <- foo[!is.na(df$roc),]
foo <- foo[foo$roc < 1,]
ggplot(foo, aes(roc)) + geom_histogram(fill="#c0392b", alpha=0.75, binwidth=0.05) + fte_theme() + labs(title="", x="auc", y="freq") + scale_x_continuous(breaks=seq(0.5, 1.0, 0.05))
ggsave("tutorial_1.png", dpi=300, width=4, height=3)
# % of datasets in a certain quality range
length(roc[roc > 0.8]) / length(roc)

# Same, layering ROC and f-measure
df.roc <- df[df$roc >= 0.5, ]
df.roc <- df.roc[!is.na(df.roc$roc),]
df.roc <- df.roc[!is.na(df.roc$f),]


