#' 227.215 Biostats.

#' In this notebook we'll take another look at data on Moroccan donkeys that we looked at
#' in semester 1.
donkey <- read.csv("http://www.massey.ac.nz/~jcmarsha/227215/data/donkey.csv")

#' Take a look at it
head(donkey)

#' A pairs plot of all measurement variables (columns 3 through 7)
plot(donkey[,3:7])
