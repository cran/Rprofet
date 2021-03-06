% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rprofet.R
\name{WOEClust_hclust}
\alias{WOEClust_hclust}
\title{Variable Clustering}
\usage{
WOEClust_hclust(object, id, target, num_clusts, method = "ward.D")
}
\arguments{
\item{object}{A WOEProfet object containing dataframes with binned and WOE values.}

\item{id}{ID variable.}

\item{target}{A binary target variable.}

\item{num_clusts}{Number of desired clusters.}

\item{method}{Clustering method to be used. This should be one of "ward.D", "ward.D2", "single", "average", "mcquitty", "median",or "centroid".}
}
\value{
A dataframe with the name of all the variables to be clustered,
the corresponding cluster and the information value for each variable.
}
\description{
Function that implements hierarchical clustering on the variables to be used as a form of variable selection.
}
\examples{
mydata <- ISLR::Default
mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
mydata$default<-ifelse(mydata$default=="Yes",1,0) ## Creating numeric binary target variable

## create two new variables from bivariate normal
sigma <- matrix(c(45000,-3000,-3000, 55000), nrow = 2)
set.seed(10)
newvars <- MASS::mvrnorm(nrow(mydata),
                         mu=c(1000,200), Sigma=sigma)

mydata$newvar1 <- newvars[,1]
mydata$newvar2 <- newvars[,2]

binned <- BinProfet(mydata, id= "ID", target= "default", num.bins = 5) ## Binning variables

WOE_dat <- WOEProfet(binned, "ID","default")

## Cluster variables by WOEClust_hclust
clusters <- WOEClust_hclust(WOE_dat, id="ID", target="default", num_clusts=3)
clusters

}
