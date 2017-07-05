# load library
library(ggplot2)
library(reshape2)

# build biorhythm function
# dob: date of birth
# target: today if not set
biorhythm <- function(dob, target = Sys.Date()) {
  
  # convert to date
  dob <- as.Date(dob)
  target <- as.Date(target)
  
  # calculate difference
  t <- round(as.numeric(difftime(target, dob)))
  
  # get 2 weeks before and after target
  days <- (t - 14) : (t + 14)
  
  # build dataframe based on theory.
  # cyclical wavelength:
  # - physical = 23 days, emotional = 28 days, intellectual = 33 days
  # this cycle is calculated with ```sin ( 2 * pi * t / w )```
  # - t = number of days since birth, w = wavelength
  period <- data.frame(Date = seq.Date(from = target - 15, by = 1, length.out = 29),
                       Physical = sin (2 * pi * days / 23) * 100, 
                       Emotional = sin (2 * pi * days / 28) * 100, 
                       Intellectual = sin (2 * pi * days / 33) * 100)
  
  # 'melt' the data
  period <- melt(period, id.vars = "Date", variable.name = "Biorhythm", value.name = "Percentage")
  
  # build plots from melted data
  ggplot(period, aes(x = Date, y = Percentage, col = Biorhythm)) + geom_line() +  
    ggtitle(paste("DoB:", format(dob, "%d %B %Y"))) + 
    geom_vline(xintercept = as.numeric(target))
}

# sample data
dob <- "1987-01-25"
target <- "2017-07-15"

# run
biorhythm(dob, target)
