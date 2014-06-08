library(WDI)
d = WDI(indicator='NY.GNP.PCAP.CD',country=c('ID'),start=1970,end=2010)
summary(d)
head(d,5)
library(ggplot2)
ggplot(d,aes(year,NY.GNP.PCAP.CD,color=country)) + geom_line() + xlab('Year') +
 ylab('GDI per capita (Atlas Method USD)') + labs(title='GNI Per Capita (USD Atlas Method)')
WDIsearch('gnp')

