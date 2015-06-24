# This script I get from
# http://flowingdata.com/2010/11/23/how-to-make-bubble-charts/

# Get the dataset
crime <- read.csv(
  "http://datasets.flowingdata.com/crimeRatesByState2005.tsv",
  header=TRUE,
  sep="\t")

# Set the bubble size
radius <- sqrt( crime$population/ pi )

# Build the chart
symbols(crime$murder, crime$burglary, circles=radius,
        inches=0.35, fg="white", bg="red",
        xlab="Murder Rate", ylab="Burglary Rate")

# Put in States to the chart
text(crime$murder, crime$burglary, crime$state,
     cex=0.5)

