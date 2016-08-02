# Load library
library(WDI)

# Build object
d = WDI(indicator='NY.GNP.PCAP.CD',country=c('ID'),start=1970,end=2010)

# Data Summary
summary(d)

# Get first 5 rows
head(d,5)

# Build plot
library(ggplot2)
ggplot(d,aes(year,NY.GNP.PCAP.CD,color=country)) + geom_line() + xlab('Year') +
 ylab('GDI per capita (Atlas Method USD)') + labs(title='GNI Per Capita (USD Atlas Method)')

# Search object
WDIsearch('gnp')

