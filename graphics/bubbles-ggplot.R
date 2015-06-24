# I got the script from
# http://www.r-bloggers.com/bubble-chart-by-using-ggplot2/
# But some functions deprecated so I make some fix for it

# Get the data
crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv", header=TRUE, sep="\t")

# Load the package, GGPlot
library(ggplot2)

# Build the plot
p <- ggplot(crime, aes(murder,burglary,size=population, label=state))\
 + geom_point(colour="red") +scale_size_continuous(range=c(1,20))+geom_text(size=3)
 + xlab("Murders per 1,000 population") + ylab("Burglaries per 1,000")

# That's it!
