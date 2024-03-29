% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rprofet.R
\name{WOEProfet}
\alias{WOEProfet}
\title{WOE Transformation}
\usage{
WOEProfet(data, id, target, varcol)
}
\arguments{
\item{data}{Dataframe of binned variables.}

\item{id}{ID variable.}

\item{target}{A binary target variable.}

\item{varcol}{Vector of variables to have WOE transformation.}
}
\value{
A list with the following components.
\item{Bin}{Dataframe with the binned variables and their WOE values.}
\item{WOE}{Dataframe with the WOE values.}
\item{IV}{Each attribute and their associated information values.}
\item{vars}{A list containing the different WOE values for each attribute.}
}
\description{
Function that calculates the WOE for each bin and the information value for each variable.
}
\examples{
mydata <- ISLR::Default
mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
mydata$default<-ifelse(mydata$default=="Yes",1,0) ## Creating numeric binary target variable
binned <- BinProfet(mydata, id= "ID", target= "default", num.bins = 5) ## Binning variables
WOE_dat <- WOEProfet(binned, "ID", "default", 3:5)
head(WOE_dat$Bin)
head(WOE_dat$WOE)
WOE_dat$IV
head(WOE_dat$vars$income)
}
