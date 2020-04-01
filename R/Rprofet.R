#' Binning Variable(s)
#'
#' @description Function that bins selected variable(s) and returns a dataframe with binned values.
#' Uses greedy binning algorithm to perform coarse binning of selected variable(s).
#' @param dat Dataframe of that contains ID, binary target and variables to be binned.
#' @param id ID variable. See 'Details'.
#' @param target The binary taget/response variable for WOE. See 'Details'.
#' @param varcol Vector of variables to be binned.
#' @param minimum.cat Minimum number of bins.
#' @param num.bins Target number of bins.
#' Overridden by the number of levels if \code{varcol} is factor.
#' @param min.pts.bin Minimum number of observations in a bin.
#' @param bracket Indicating if the intervals should be closed on the right or
#' left. Options include \code{left} and \code{right}.
#' @param special.values A vector of values that should have their own bin.
#' See 'Details'.
#' @details The \code{id} and the \code{target} variables must be provided.
#' Works for numeric, factor and binary \code{target}. To build a scorecard,
#' a binary \code{target} is required.
#' @details Actual number of bins exceeds \code{num.bins} if
#' \code{special.values} specified.
#'
#'
#' @examples mydata <- ISLR::Default
#' @examples head(mydata)
#'
#' @examples mydata$ID <- seq(1:nrow(mydata)) ## make an ID variable
#' @examples mydata$default <- ifelse(mydata$default=="Yes", 1, 0) ## target coded with 1, 0
#'
#' @examples ## bin balance and income
#' @examples binned1 <- BinProfet(mydata, id="ID", target="default",
#' @examples                   varcol = c("balance",  "income"), num.bins = 5)
#' @examples head(binned1)
#'
#' @examples ## bin categorical variable-------------------
#' @examples binned2 <- BinProfet(mydata, id="ID", target="default",
#' @examples                    varcol = "student", num.bins = 5)
#' @examples head(binned2)
#' @examples summary(binned2$student_Bins) ## num.bins overriden
#'
#' @return  A dataframe containing the ID, target, and binned variable(s) with corresponding binned vlues.
#'
#' @export
BinProfet<-function(dat,id,target,varcol,minimum.cat = 4,num.bins = 10,min.pts.bin = 25,bracket = "left",special.values = NULL){

  ## Obtaining the input column names or column numbers depending which input
  if(is.character(target) == TRUE){ target <- match(target,colnames(dat)) }
  if(is.character(id) == TRUE){
    id_char <- id
    id <- match(id,colnames(dat))
  }
  else if(is.numeric(id) == TRUE){
    id_char <- colnames(dat)[id]
  }
  if(missing(varcol)){ varcol <- (1:ncol(dat))[c(-id,-target)]}
  if(is.character(varcol) == TRUE){ varcol <- match(varcol,colnames(dat)) }


  ## Function that bins the variables based on user inputs
  BinFun1<-function(dat,varcol,badcol,idcol,minimum.cat,num.bins,min.pts.bin,bracket,special.values){

    options(scipen=9999999)

    ## If the variable is not numeric then proceed
    if (is.numeric(dat[,varcol])==FALSE){
      ## Creating two dataframes, one containing the idcol and varcol for missing observations and others for nonmissing observations
      NAS<-dat[which(is.na(dat[,varcol])),c(idcol,varcol)]
      Norm<-dat[which(!is.na(dat[,varcol])),c(idcol,varcol)]

      ## If the missing data frame has rows, Assign the bins values of "Missing" attach to the regular values
      if (nrow(NAS)>0){
        NAS$Bins<-"Missing"
        Norm$Bins<-as.character(Norm[,2])
        final<-rbind(NAS,Norm)
      }
      ## If the missing data frame is empty, then just use the regular factor levels
      else if (nrow(NAS)==0){
        Norm$Bins<-as.character(Norm[,2])
        final<-Norm
      }
    }

    ## If the variable is numeric then proceed
    else if (is.numeric(dat[,varcol])==TRUE){
      ## Creating three data frames, One for Missing values, Special Values, and Normal values
      NAS<-dat[which(is.na(dat[,varcol])),c(idcol,varcol)]
      spec<-dat[which(dat[,varcol] %in% special.values),c(idcol,varcol)]
      Norm<-dat[which(!is.na(dat[,varcol]) & !(dat[,varcol] %in% special.values)),c(idcol,varcol)]

      ## If missing and special value data frames are longer than 0 then proceed
      if (nrow(NAS)>0 & nrow(spec)>0){

        ## Assigning Bins for missing and special values
        NAS$Bins<-"Missing"
        spec$Bins<-as.character(spec[,2])

        ## If normal unique values less than min category input then unique values are the bins
        if (length(table(Norm[,2]))<minimum.cat){
          Norm$Bins<-as.character(Norm[,2])
        }

        ## If there are more unique values than min categories input then proceed
        else if (length(table(Norm[,2]))>=minimum.cat){
          ## Binning the variable based on user inputs
          findbins<-binr::bins.greedy(Norm[,2],nbins = num.bins, minpts = min.pts.bin)

          ## Adjusting intervals based on bracket being specified right or left
          if (bracket=="right"){
            Norm$Bins=as.character(cut(Norm[,2]
                                       ,breaks=c(-Inf,findbins$binhi[1:length(unique(findbins$binhi))-1],Inf)))
          }
          else if (bracket=="left"){
            Norm$Bins=as.character(cut(Norm[,2]
                                       ,breaks=c(-Inf,findbins$binlo[1:length(unique(findbins$binlo))],Inf),
                                       right = FALSE))
          }}

        final<-rbind(NAS,spec,Norm)
      }

      ## If missing data frame is greater than 0 and special values is empty proceed
      else if (nrow(NAS)>0 & nrow(spec)==0){

        ## Assign missing to missing data frame bins
        NAS$Bins<-"Missing"

        ## If normal unique values less than min category input then unique values are the bins
        if (length(table(Norm[,2]))<minimum.cat){
          Norm$Bins<-as.character(Norm[,2])
        }

        ## If there are more unique values than min categories input then bin the variable and adjust intervals
        else if (length(table(Norm[,2]))>=minimum.cat){
          findbins<-binr::bins.greedy(Norm[,2],nbins = num.bins, minpts = min.pts.bin)
          if (bracket=="right"){
            Norm$Bins=as.character(cut(Norm[,2]
                                       ,breaks=c(-Inf,findbins$binhi[1:length(unique(findbins$binhi))-1],Inf)))
          }
          else if (bracket=="left"){
            Norm$Bins=as.character(cut(Norm[,2]
                                       ,breaks=c(-Inf,findbins$binlo[1:length(unique(findbins$binlo))],Inf),
                                       right = FALSE))
          }}

        final<-rbind(NAS,Norm)
      }

      ## If missing data frame is empty and special value data frame exists then proceed
      else if (nrow(NAS)==0 & nrow(spec)>0){

        ## Make special values there own bins
        spec$Bins<-as.character(spec[,2])

        ## If normal unique values less than min category input then unique values are the bins
        if (length(table(Norm[,2]))<minimum.cat){
          Norm$Bins<-as.character(Norm[,2])
        }

        ## If there are more unique values than min categories input then bin the variable and adjust intervals
        else if (length(table(Norm[,2]))>=minimum.cat){
          findbins<-binr::bins.greedy(Norm[,2],nbins = num.bins, minpts = min.pts.bin)
          if (bracket=="right"){
            Norm$Bins=as.character(cut(Norm[,2]
                                       ,breaks=c(-Inf,findbins$binhi[1:length(unique(findbins$binhi))-1],Inf)))
          }
          else if (bracket=="left"){
            Norm$Bins=as.character(cut(Norm[,2]
                                       ,breaks=c(-Inf,findbins$binlo[1:length(unique(findbins$binlo))],Inf),
                                       right = FALSE))
          }}

        final<-rbind(spec,Norm)
      }

      ## If missing data frame and special values data frame do not exist, proceed
      else if (nrow(NAS)==0 & nrow(spec)==0){

        if (nrow(spec)>0){
          spec$Bins<-as.character(spec[,2])
        }

        ## If normal unique values less than min category input then unique values are the bins
        if (length(table(Norm[,2]))<minimum.cat){
          Norm$Bins<-as.character(Norm[,2])
        }

        ## If there are more unique values than min categories input then bin the variable and adjust intervals
        else if (length(table(Norm[,2]))>=minimum.cat){
          findbins<-binr::bins.greedy(Norm[,2],nbins = num.bins, minpts = min.pts.bin)
          if (bracket=="right"){
            Norm$Bins=as.character(cut(Norm[,2]
                                       ,breaks=c(-Inf,findbins$binhi[1:length(unique(findbins$binhi))-1],Inf)))
          }
          else if (bracket=="left"){
            Norm$Bins=as.character(cut(Norm[,2]
                                       ,breaks=c(-Inf,findbins$binlo[1:length(unique(findbins$binlo))],Inf),
                                       right = FALSE))
          }}

        final<-Norm
      }
    }
    Bins<-data.frame(final, stringsAsFactors = TRUE)
    Bins[,3] <- as.factor(Bins[,3])
    colnames(Bins)[3] <- paste(colnames(Bins)[2],"Bins",sep="_")
    return(c(Bins))}


  ## Wrapper function that calls BinFun1 function based on user function inputs
  BinFun2<-function(varcol){
    BinFun1(dat,
            varcol,
            badcol=target,
            idcol=id,
            minimum.cat,
            num.bins,
            min.pts.bin,
            bracket,
            special.values)}

  ## Using apply function to call BinFun2 for each variable individually
  z<-lapply(varcol, BinFun2)

  ## Reduce the list into a single dataframe of the binned vlaues
  Merged=Reduce(function(x, y) merge(x, y,all.x=T,by=id_char),z)
  Merged2=merge(dat[,c(id,target)],Merged, by = id_char)

  Merged2=Merged2[,c(1,2,seq(4,ncol(Merged2), by = 2))]


  ##Functions to help reorder the factor levels increasing numeric values
  findLo = function(vec){
    if(sapply(strsplit(vec, ","), length)==1){
      return(NA)
    }
    tmp = strsplit(vec, ",")
    lo = as.numeric(tmp[[1]][1])
    return(lo)
  }

  reorder_levels = function(vars){
    x = gsub("\\[|\\]|\\(|\\)", "", levels(vars))
    vec = sapply(x, findLo)
    NewBins = factor(vars,levels(vars)[order(vec)])
    return(NewBins)
  }

  ##Reording factor levels based on increasing numeric value
  Merged2[,3:ncol(Merged2)] =  as.data.frame(lapply(Merged2[,3:ncol(Merged2)],reorder_levels))


  return(Merged2)
}


#' WOE Transformation
#'
#' Function that calculates the WOE for each bin and the information value for each variable.
#'
#' @param dat Dataframe of binned variables.
#' @param id ID variable.
#' @param target A binary target variable.
#' @param varcol Vector of variables to have WOE transformation.
#'
#' @details The \code{id} and the \code{target} variables must be provided.
#' The \code{target} variable must be a numeric binary variable.
#'
#'
#' @examples mydata <- ISLR::Default
#'
#' @examples mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
#' @examples mydata$default<-ifelse(mydata$default=="Yes",1,0) ## Creating numeric binary target variable
#'
#' @examples binned <- BinProfet(mydata, id= "ID", target= "default", num.bins = 5) ## Binning variables
#'
#' @examples WOE_dat <- WOEProfet(binned, "ID","default", 3:5)
#'
#' @examples head(WOE_dat$BinWOE)
#' @examples head(WOE_dat$WOE)
#' @examples WOE_dat$IV
#' @examples head(WOE_dat$vars$income)
#'
#'
#' @return A list with the following components.
#' \item{BinWOE}{Dataframe with the binned variables and their WOE values.}
#' \item{WOE}{Dataframe with the WOE values.}
#' \item{IV}{Each attribute and their associated information values.}
#' \item{vars}{A list containing the different WOE values for each attribute.}
#'
#'
#' @export
WOEProfet <- function(dat,id,target,varcol){

  ## Ifelse statement to identify if the target input is the column number or column name
  ##    If it is the column name, store the name and convert target input to column number
  if(is.character(target) == TRUE){
    tar_char <- target
    target <- match(target,colnames(dat))
  }
  else if(is.numeric(target) == TRUE){
    tar_char <- colnames(dat)[target]
  }

  ## Ifelse statement to identify if the ID input is the column number or column name
  ##    If it is the column name, store the name and convert ID input to column number
  if(is.character(id) == TRUE){
    id_char <- id
    id <- match(id,colnames(dat))
  }
  else if(is.numeric(id) == TRUE){
    id_char <- colnames(dat)[id]
  }

  ## If the varcol input is missing, use all columns except target and ID columns
  if(missing(varcol)){ varcol <- (1:ncol(dat))[c(-id,-target)]}

  ## If varcol input is characters, convert to column numbers
  if(is.character(varcol) == TRUE){ varcol <- match(varcol,colnames(dat)) }

  ## Function used to calculate WOE for each bin. Wrapper functions are used to access this function and will
  ##    pass in one binned variable at a time. This function will be called for every variable in varcol input
  WOEFun1<-function(dat,idcol,targetcol,varcol){
    dat2<-dat[,c(idcol,targetcol,varcol)]
    dat2$Bins<-as.character(dat2[,3])

    ## Aggregated the number of "Good" and "Bad" instances that occur for each bin
    NumBad<-aggregate(dat2[,2]~Bins,data=dat2,FUN=sum)
    NumGood<-aggregate((ifelse(dat2[,2]==1,0,1))~Bins,data=dat2,FUN=sum)

    ## Calculating the WOE for each bin
    IVF1_i<-(NumBad[,2]/sum(NumBad[,2]))-(NumGood[,2]/sum(NumGood[,2]))
    IVF2_i<-log((NumBad[,2]/sum(NumBad[,2]))/(NumGood[,2]/sum(NumGood[,2])))
    IVF3_i<-log(((NumBad[,2]+.5)/(sum(NumBad[,2])+.5))/((NumGood[,2]+.5)/(sum(NumGood[,2])+.5)))
    IVF23_i<-ifelse(IVF2_i==-Inf|IVF2_i==Inf,IVF3_i,IVF2_i)
    ## Calculating the IV for each bin
    IVF_i<-IVF1_i*IVF23_i

    ## Attaching WOE to ID, Target, and bin and renaming columns
    Temp<-data.frame(cbind(NumBad[,1],IVF23_i))
    Temp<-plyr::rename(Temp, c("V1"="Bins", "IVF23_i"="woe"))
    Temp2<-plyr::join(dat2,Temp,by='Bins',type="left",match="all")

    ##Further dataframe management to obtain right format, naming structure, and contain ID, bin, and WOE
    WOE<-data.frame(Temp2)
    colnames(WOE)[5] <- paste(colnames(WOE)[3],"WOE",sep="_")
    WOE[,5] <- as.numeric(as.character(WOE[,5]))
    WOE2<-WOE[,c(1,3,5)]

    return(c(WOE2))
  }

  ## Function used to calculate the Information Value for each variable. Wrapper functions are used to access this function and will
  ##    pass in one binned variable at a time. This function will be called for every variable in varcol input
  IVFun1<-function(dat,idcol,targetcol,varcol){
    dat2<-dat[,c(idcol,targetcol,varcol)]
    dat2$Bins<-as.character(dat2[,3])

    ## Aggregated the number of "Good" and "Bad" instances that occur for each bin
    NumBad<-aggregate(dat2[,2]~Bins,data=dat2,FUN=sum)
    NumGood<-aggregate((ifelse(dat2[,2]==1,0,1))~Bins,data=dat2,FUN=sum)

    ##Calculate WOE for each bin and subsequently IV for each bin
    IVF1_i<-(NumBad[,2]/sum(NumBad[,2]))-(NumGood[,2]/sum(NumGood[,2]))
    IVF2_i<-log((NumBad[,2]/sum(NumBad[,2]))/(NumGood[,2]/sum(NumGood[,2])))
    IVF3_i<-log(((NumBad[,2]+.5)/(sum(NumBad[,2])+.5))/((NumGood[,2]+.5)/(sum(NumGood[,2])+.5)))
    IVF23_i<-ifelse(IVF2_i==-Inf|IVF2_i==Inf,IVF3_i,IVF2_i)
    IVF_i<-IVF1_i*IVF23_i

    ## Sum up IV values to obtain IV for the variable
    IVF<-sum(IVF_i)
    IVF2<-c(colnames(dat2[3]),IVF)

    return(c(IVF2))
  }


  ## Wrapper Function for WOEFun1. Uses inputs obtained from WOEFun3 function call.
  WOEFun2<-function(varcol){
    WOEFun1(dat,
            idcol=id,
            targetcol=target,
            varcol)}

  ## Using lapply function to pass in each variable individually. Returns a list WOE2 data frame from WOEFun1
  z<-lapply(varcol, WOEFun2)

  ## Uses the Reduce function to convert the list into a dataframe containing the bins and WOE values
  Merged=Reduce(function(x, y) merge(x, y,all.x=T,by=id_char),z)

  ## Attaching the ID and Target variables
  Merged2=merge(dat[,c(id_char,tar_char)],Merged, by = id_char)

  ## Obtaining ID, Target, and WOE variables
  Merged3=Merged2[,c(1,2,seq(4,ncol(Merged2),by=2))]


  ## Wrapper Function for IVFun1. Uses inputs obtained from WOEFun3 function call.
  IVFun2<-function(varcol){
    IVFun1(dat=dat,
           idcol=id,
           targetcol=target,
           varcol)}

  ## Using lapply function to pass in each variable individually.
  IV_and_Var<-lapply(varcol, IVFun2)

  ## Creating a dataframe from the returned list that includes the variable and the corresponding IV.
  IV_and_Var2<-data.frame(Variable=sapply(IV_and_Var,"[",1),
                          IV=as.numeric(sapply(IV_and_Var,"[",2)))
  IV_and_Var2$Variable = as.character(IV_and_Var2$Variable)


  ## Function that generates a sql statement that displays the distinct bins and the corresponding WOE for each variable.
  VarWOE = function(var){
    sql_state = paste("Select",var,",",paste(var,"WOE",sep = "_"), "from", deparse(substitute(Merged2)),"group by",var,",",paste(var,"WOE",sep = "_"), sep = " ")
    chk = sqldf::sqldf(sql_state)
    return(chk)
  }

  ## Using Apply function to generate sql statements for each variable individually
  ##  returning a list that will show the distinct bins and corresponding WOE for each inputted variable.
  woeList = lapply(colnames(dat)[varcol],VarWOE)
  names(woeList) = gsub("_Bins","",colnames(dat)[varcol])

  ## Creating a list object to be returned.
  item <- list(BinWOE=Merged2,WOE=Merged3,IV=IV_and_Var2,vars=woeList)

  return(item)
}



#' Scorecard Builder
#'
#' Function that fits a logistic regression models and scores points for each bin and calculates observations' total score.
#'
#' @param object A WOEProfet object containing dataframes with binned and WOE values.
#' @param target A binary target variable.
#' @param id ID variable.
#' @param varcol Vector of WOE variables to be used in the logistic regression model.
#' @param PDO Points to Double Odds.
#' @param BaseOdds Base Odds.
#' @param BasePts Base Points.
#' @param reverse Logical. If true, higher points corresponds to a lower probability of being target.
#'
#'
#' @examples mydata <- ISLR::Default
#'
#' @examples mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
#' @examples mydata$default<-ifelse(mydata$default=="Yes",1,0) ## Creating numeric binary target variable
#'
#' @examples binned <- BinProfet(mydata, id= "ID", target= "default", num.bins = 5) ## Binning variables
#'
#' @examples WOE_dat <- WOEProfet(binned, "ID","default", 3:5) ## WOE transformation of bins
#'
#' @examples Score_dat <- ScorecardProfet(WOE_dat, target="default",
#' @examples                              id= "ID", PDO = 50, BaseOdds = 10, BasePts = 1000, reverse = TRUE)
#'
#' @examples Score_dat$GLMSummary
#' @examples head(Score_dat$Scorecard) ## Less points means more likely to default
#'
#'
#' @return A list with the following components.
#' \item{Scorecard}{The actual scorecard model. Table with the attribute bins and their corresponding WOE values and the points assigned to each bin.}
#' \item{Results}{Dataframe with the bin, WOE value, and points assigned to each attribute and the total score for each observation.}
#' \item{GLMSummary}{The summary of the logistic regression model fitted to build the scorecard.}
#'
#' @export
ScorecardProfet <- function(object, target, id, varcol, PDO = 100, BaseOdds = 10, BasePts = 1000, reverse = FALSE ){


  data = object[[2]]

  ## Ifelse statement to identify if the target input is column names
  ##    If it is obtain the column numbers
  if(is.character(target) == TRUE){
    tar_num <- match(target,colnames(data))
  }
  ##    Else store the target columns and convert target to column names
  else if(is.numeric(target) == TRUE){
    tar_num <- target
    target <- colnames(data)[target]
  }

  ## If ID input is column names, convert it to column numbers
  if(is.character(id) == TRUE){ id <- match(id, colnames(data))}

  ## If varcol input is missing, Use all columns except target and ID columns
  if(missing(varcol)){ varcol <- colnames(data)[c(-id,-tar_num)] }

  ## If varcol is column numbers, convert to column names
  if(is.numeric(varcol) == TRUE){ varcol <- colnames(data)[varcol] }

  if(reverse == TRUE){
    data[,varcol] = -data[,varcol]
  }


  ## Calculating the factor and offset based on user inputs
  factor = PDO/log(2)
  offset = BasePts-(factor*log(BaseOdds))

  ## Generating a character statement that is the formula to be used for the logistic regression
  form = as.formula(paste(target, paste(varcol, collapse=" + "), sep=" ~ "))

  ## Fitting a logistic regression model
  logmod <- glm(form,data=data,family = "binomial")

  ## Storing the logistic regression model summary
  logmod.summary = summary(logmod)

  ## Storing the model coefficients and the model terms
  modelcoef=as.numeric(logmod$coefficients)
  modelterms=as.factor(labels(logmod$coefficients))

  ## Storing model intercept and number of predictors
  a=modelcoef[1]
  n=length(modelcoef)-1

  datBinWOE = object[[1]]

  calc_points <- function(datBinWOE, varcol, modelcoef, modelterms, a, n, factor, offset){
    pat = gsub("_WOE", "",varcol[1])

    score_bins = data.frame(datBinWOE[,colnames(datBinWOE)[which(stringr::str_detect(colnames(datBinWOE), pat))]])
    score_bins$score = round(-(datBinWOE[,varcol]*modelcoef[which(modelterms==varcol)]+a/n)*factor+offset/n,0)
    colnames(score_bins)[3] = paste(varcol, "Points", sep = "_")

    return(score_bins)
  }


  card_wrapper <- function(varcol){
    calc_points(datBinWOE,
                varcol,
                modelcoef,
                modelterms,
                a,
                n,
                factor,
                offset)
  }


  z = lapply(varcol, card_wrapper)


  tmp = data.frame(ID = datBinWOE[,id], default = datBinWOE[,target], as.data.frame(z))
  colnames(tmp)[1:2] = colnames(datBinWOE)[1:2]


  tmp$Score = rowSums(tmp[,seq(5,ncol(tmp), by = 3)], na.rm = TRUE)



  lvl_order <- function(varcol){
    tmp_lvls = as.numeric(tmp[,gsub("_WOE","",varcol)])
    return(tmp_lvls)
  }


  lvls = lapply(varcol, lvl_order)

  lvls2 = as.data.frame(lvls)

  name_lvls <- function(varcol){
    cnames = paste(gsub("_WOE","",varcol), "Level", sep = "_")
    return(cnames)
  }

  colnames(lvls2) = sapply(varcol,name_lvls)


  tmp2 = data.frame(tmp, lvls2)


  scard <- function(varcol){
    attribute = gsub("_Bins_WOE","",varcol)

    sql_state = paste("Select '",attribute,"' as Attribute,", paste(attribute,"Bins",sep = "_"), "as Bins,", paste(attribute,"Bins_WOE",sep = "_"),"as WOE,",
                      paste(attribute,"Bins_WOE_Points",sep = "_"), "as Points from", deparse(substitute(tmp2)), "group by", paste(attribute,"Bins_Level",sep = "_"))

    return(sqldf::sqldf(sql_state))

  }


  chk = lapply(varcol, scard)

  chk2 = do.call("rbind",chk)


  ## generating a list to return mulitple items
  tmp3 = list(Scorecard = chk2, Results = tmp, GLMSummary = logmod.summary)

  return(tmp3)

}



#' Variable Clustering
#'
#' @description Function that implements hierarchical clustering on the variables to be used as a form of variable selection.
#'
#' @param object A WOEProfet object containing dataframes with binned and WOE values.
#' @param id ID variable.
#' @param target A binary target variable.
#' @param num_clusts Number of desired clusters.
#' @param method Clustering method to be used. This should be one of "ward.D", "ward.D2", "single", "average", "mcquitty", "median",or "centroid".
#'
#' @return A dataframe with the name of all the variables to be clustered,
#' the corresponding cluster and the information value for each variable.
#'
#' @examples mydata <- ISLR::Default
#' @examples mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
#' @examples mydata$default<-ifelse(mydata$default=="Yes",1,0) ## Creating numeric binary target variable
#'
#' @examples ## create two new variables from bivariate normal
#' @examples sigma <- matrix(c(45000,-3000,-3000, 55000), nrow = 2)
#' @examples set.seed(10)
#' @examples newvars <- MASS::mvrnorm(nrow(mydata),
#' @examples                          mu=c(1000,200), Sigma=sigma)
#'
#' @examples mydata$newvar1 <- newvars[,1]
#' @examples mydata$newvar2 <- newvars[,2]
#'
#' @examples binned <- BinProfet(mydata, id= "ID", target= "default", num.bins = 5) ## Binning variables
#'
#' @examples WOE_dat <- WOEProfet(binned, "ID","default")
#'
#' @examples ## Cluster variables by WOEClust_hclust
#' @examples clusters <- WOEClust_hclust(WOE_dat, id="ID", target="default", num_clusts=3)
#' @examples clusters
#'
#' @export
WOEClust_hclust<-function(object,id,target,num_clusts,method='ward.D'){

  dat = object[[2]]

  ## Adjusting ID, target inputs to be column numbers
  if(is.character(id) == TRUE){id = match(id,colnames(dat))}
  if(is.character(target) == TRUE){target = match(target,colnames(dat))}

  woe_trans = t(dat[,-c(id,target)])
  dist.probes = dist(woe_trans)

  probes.complete=hclust(dist.probes, method = method)

  groups = cutree(probes.complete, k=num_clusts)

  Memberships<-data.frame(groups)
  Memberships<-cbind(rownames(Memberships),Memberships)
  rownames(Memberships) <- NULL
  names(Memberships)<-c("Variable","Group")
  Memberships$Variable = as.character(Memberships$Variable)

  infoVal = object[[3]]

  Memberships$IV = round(infoVal[,2],4)

  Memberships = Memberships[order(Memberships$Group,-Memberships$IV),]

  Memberships$Variable = as.character(Memberships$Variable)


  return(Memberships)
}



#' Kmeans Variable Clustering
#'
#' @description Function that implements kmeans variable clusteting to be used as a form of variable selection.
#'
#' @param object A WOEProfet object containing dataframes with binned and WOE values.
#' @param id ID variable.
#' @param target A binary target variable.
#' @param num_clusts Number of desired clusters.
#'
#' @return A dataframe with the name of all the variables to be clustered,
#' the corresponding cluster and the information value for each variable.
#'
#' @examples mydata <- ISLR::Default
#' @examples mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
#' @examples mydata$default<-ifelse(mydata$default=="Yes",1,0) ## Creating numeric binary target variable
#'
#' @examples ## create two new variables from bivariate normal
#' @examples sigma <- matrix(c(45000,-3000,-3000, 55000), nrow = 2)
#' @examples set.seed(10)
#' @examples newvars <- MASS::mvrnorm(nrow(mydata),
#' @examples                          mu=c(1000,200), Sigma=sigma)
#'
#' @examples mydata$newvar1 <- newvars[,1]
#' @examples mydata$newvar2 <- newvars[,2]
#'
#' @examples binned <- BinProfet(mydata, id= "ID", target= "default", num.bins = 5) ## Binning variables
#'
#' @examples WOE_dat <- WOEProfet(binned, "ID","default")
#'
#' @examples ## Cluster variables by WOEClust_kmeans
#' @examples clusters <- WOEClust_kmeans(WOE_dat, id="ID", target="default", num_clusts=3)
#' @examples clusters
#'
#' @export
#'
#' @export
WOEClust_kmeans<-function(object,id,target,num_clusts){

  dat = object[[2]]

  ## Adjusting ID, target inputs to be column numbers
  if(is.character(id) == TRUE){id = match(id,colnames(dat))}
  if(is.character(target) == TRUE){target = match(target,colnames(dat))}

  ## Removes ID, target columns. Performs variable clustering and generates data frame containing variable and their cluster number.
  dat2<-dat[,-c(1,2)]
  Groups<-ClustOfVar::kmeansvar(X.quanti =dat2[,1:ncol(dat2)], X.quali = NULL, num_clusts,
                                iter.max = 150, nstart = 1, matsim = FALSE)
  Memberships<-data.frame(Groups$cluster)
  Memberships<-cbind(rownames(Memberships),Memberships)
  rownames(Memberships) <- NULL
  names(Memberships)<-c("Variable","Group")

  ## Storing the column numbers
  varcol = colnames(dat2)
  varcol = match(varcol,colnames(dat))

  ## Function from WOEFun3 that calculates IV for each variables
  IVFun1<-function(dat,idcol,targetcol,varcol){
    dat2<-dat[,c(idcol,targetcol,varcol)]
    dat2$Bins<-as.character(dat2[,3])

    NumBad<-aggregate(dat2[,2]~Bins,data=dat2,FUN=sum)
    NumGood<-aggregate((ifelse(dat2[,2]==1,0,1))~Bins,data=dat2,FUN=sum)


    IVF1_i<-(NumBad[,2]/sum(NumBad[,2]))-(NumGood[,2]/sum(NumGood[,2]))
    IVF2_i<-log((NumBad[,2]/sum(NumBad[,2]))/(NumGood[,2]/sum(NumGood[,2])))
    IVF3_i<-log(((NumBad[,2]+.5)/(sum(NumBad[,2])+.5))/((NumGood[,2]+.5)/(sum(NumGood[,2])+.5)))
    IVF23_i<-ifelse(IVF2_i==-Inf|IVF2_i==Inf,IVF3_i,IVF2_i)
    IVF_i<-IVF1_i*IVF23_i

    IVF<-sum(IVF_i)
    IVF2<-c(colnames(dat2[3]),IVF)

    return(c(IVF2))
  }

  ## Wrapper function for IVFun1
  IVFun2<-function(varcol){IVFun1(dat=dat,idcol=id,targetcol=target,varcol)}

  ## Calling the IVFun functions to calculate the IV for each variable
  IV_and_Var<-lapply(varcol, IVFun2)
  IV_and_Var2<-data.frame(Variable=sapply(IV_and_Var,"[",1),
                          IV=as.numeric(sapply(IV_and_Var,"[",2)))

  ## Managing and organizing the data frame to order it by cluster number and then IV
  Memberships2 = merge(Memberships,IV_and_Var2, By = "Variable")

  KMEANSGroups = Memberships2[order(Memberships2$Group,-Memberships2$IV),]

  KMEANSGroups$Variable = as.character(KMEANSGroups$Variable)

  return(KMEANSGroups)
}



#' Visualizing WOE and Target Rates
#'
#' Function generating three plots: WOE value for each bin, target rate for each bin, and the frequency for each bin
#'
#' @param dataset Dataframe containing binned values and a binary target variable.
#' @param target A numeric binary target variable.
#' @param var The desired WOE binned attribute to visualize.
#' @param color A hexadecimal value representing a specific color.
#'
#' @details A list of the hexadecimal colors can be found at this link http://www.sthda.com/sthda/RDoc/images/hextable.gif
#'
#'
#' @examples mydata <- ISLR::Default
#'
#' @examples mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
#' @examples mydata$default<-ifelse(mydata$default=="Yes",1,0) ## Creating numeric binary target variable
#'
#' @examples binned <- BinProfet(mydata, id= "ID", target= "default", num.bins = 5) ## Binning variables
#'
#' @examples WOEplotter(binned, target= "default", var= "income_Bins")
#'
#' @examples ##--Changing Colors------------------------------
#' @examples WOEplotter(binned, target= "default", var= "income_Bins", color = "#33FF33")
#'
#' @export
WOEplotter <- function(dataset,target,var, color = "#0066CC"){

  ## Converting var and target inputs to the column names
  if(is.character(var) == FALSE){var = colnames(dataset)[var]}
  if(is.character(target) == FALSE){target = colnames(dataset)[target]}


  dat = dataset[,c(target,var)]

  ## Calculating "good" and "bad" events for each bin
  NumBad<-aggregate(dat[,1]~dat[,2],data=dat,FUN=sum)
  NumGood<-aggregate((ifelse(dat[,1]==1,0,1))~dat[,2], data=dat,FUN=sum)

  ## Calculating the WOE for each bin and IV for the variable
  IVF1_i<-(NumBad[,2]/sum(NumBad[,2]))-(NumGood[,2]/sum(NumGood[,2]))
  IVF2_i<-log((NumBad[,2]/sum(NumBad[,2]))/(NumGood[,2]/sum(NumGood[,2])))
  IVF3_i<-log(((NumBad[,2]+.5)/(sum(NumBad[,2])+.5))/((NumGood[,2]+.5)/(sum(NumGood[,2])+.5)))
  IVF23_i<-ifelse(IVF2_i==-Inf|IVF2_i==Inf,IVF3_i,IVF2_i)
  IVF_i<-IVF1_i*IVF23_i
  IVF<-sum(IVF_i)

  ## Creating the dataframe to contain the bins, WOE, bad rate, and frequency
  Temp<-data.frame(NumBad[,1],IVF23_i)
  colnames(Temp) = c("Bins","woe")
  dat$Bins = dat[,2]

  Temp2<-merge(dat,Temp,by="Bins",all.x = TRUE)

  dat=Temp2[,c(1,2,4)]
  WeOfEv=dat[,3]
  FinBins=dat[,1]
  dat[,4]<-ifelse(dat[,2]==1,0,1)
  Check2<-data.frame(aggregate(dat[,2]~dat[,3]+dat[,1],data=dat,FUN=mean))
  names(Check2)<-c("WoE","Bins","TargetRate")
  Check3<-data.frame(table(dat[,1]))
  names(Check3)<-c("Bins","Freq")
  Check4<-merge(Check2,Check3,by="Bins")


  ### Visualization for the WOE bins

  ## Barplot for the WOE for each
  a<-ggplot2::ggplot() +
    ggplot2::geom_bar(data = Check4, ggplot2::aes(x=Check4$Bins, y=Check4$WoE) , fill=color, stat = "identity") +
    ggplot2::theme(legend.position="none", axis.text.x=ggplot2::element_text(angle=45, hjust=1))+
    ggplot2::labs(x = "",y="WoE")

  ## Barplot for the bin frequency
  b<-ggplot2::ggplot() +
    ggplot2::geom_bar(data = Check4, ggplot2::aes(x=Check4$Bins, y=Check4$Freq), fill=color, stat = "identity") +
    ggplot2::theme(legend.position="none", axis.text.x=ggplot2::element_text(angle=45, hjust=1))+
    ggplot2::labs(x = "",y="Bin Frequency")

  ## Barplot for the target rate
  c<-ggplot2::ggplot() +
    ggplot2::geom_bar(data = Check4, ggplot2::aes(x=Check4$Bins, y=Check4$TargetRate), fill=color, stat = "identity") +
    ggplot2::theme(legend.position="none", axis.text.x=ggplot2::element_text(angle=45, hjust=1))+
    ggplot2::labs(x = "",y="Target Rate")

  ## Command arranging the order of the three barplots and placing the variable IV at the top
  e<-gridExtra::grid.arrange(a,c,b, ncol=3, nrow=1,top=paste(var,"\n IV=",IVF))

}


#' Custom Binning Numeric Variables
#'
#' Function that bins a numeric variable based on user inputted breaks, plots the information on the new bins, and returns a vector of the newly binned values
#'
#' @param dataset Dataframe containing the target variable and desired numeric variables to be binned.
#' @param var A specific numeric attribute to be binned. Must be specified.
#' @param target A binary target variable. Must be specified.
#' @param breaks A vector of breakpoints for the desired bins. Must be specified.
#' @param right_bracket Logical. Specifying whether the intervals are closed on the right or the left.
#' @param color A hexadecimal value representing a specific color.
#'
#' @return A vector containing the newly binned values. Generates three barplots
#' displaying the worth of evidence (WoE), target rate and frequency across
#' all bins. The bars are arranged in an ascending order of WoE.
#'
#'
#' @examples mydata <- ISLR::Default
#' @examples mydata$default <- ifelse(mydata$default=="Yes", 1, 0) ## target coded with 1, 0
#' @examples
#' @examples WC_1 <- WOE_custom(dataset=mydata, var="balance", target = "default",
#' @examples            breaks=seq(0,3000,1000))
#' @examples levels(factor(WC_1))
#' @examples
#' @examples WC_2 <- WOE_custom(dataset=mydata, var="income", target = "default",
#' @examples              breaks=seq(0,75000, 15000))
#' @examples levels(factor(WC_2))
#'
#' @export

WOE_custom <- function(dataset, var, target, breaks, right_bracket = F, color = "#0066CC"){

  ## Obtaining var input column names
  if(is.numeric(var) == TRUE){var = colnames(dataset)[var]}
  if(missing(target)){
    print("ERROR: Target variable input is missing.")
    return()
  }

  ## Using the use inputted values to create new bins based on the desired cutpoints
  var_bins = cut(dataset[,var], breaks, right = right_bracket)
  var_bins = as.character(var_bins)
  var_bins = ifelse(is.na(var_bins), "Missing", var_bins)
  dataset$var_bins = var_bins
  colnames(dataset)[match(deparse(substitute(var_bins)), colnames(dataset))] = paste(var,"_Bins", sep = "")
  var_name = paste(var,"_Bins", sep = "")

  dat = data.frame(dataset[,target],var_bins)

  ## Calculating "good" and "bad" events for each bin
  NumBad<-aggregate(dat[,1]~dat[,2],data=dat,FUN=sum)
  NumGood<-aggregate((ifelse(dat[,1]==1,0,1))~dat[,2], data=dat,FUN=sum)

  ## Calculating the WOE for each bin and IV for the variable
  IVF1_i<-(NumBad[,2]/sum(NumBad[,2]))-(NumGood[,2]/sum(NumGood[,2]))
  IVF2_i<-log((NumBad[,2]/sum(NumBad[,2]))/(NumGood[,2]/sum(NumGood[,2])))
  IVF3_i<-log(((NumBad[,2]+.5)/(sum(NumBad[,2])+.5))/((NumGood[,2]+.5)/(sum(NumGood[,2])+.5)))
  IVF23_i<-ifelse(IVF2_i==-Inf|IVF2_i==Inf,IVF3_i,IVF2_i)
  IVF_i<-IVF1_i*IVF23_i
  IVF<-sum(IVF_i)

  ## Creating the dataframe to contain the bins, WOE, bad rate, and frequency
  Temp<-data.frame(NumBad[,1],IVF23_i)
  colnames(Temp) = c("Bins","woe")
  dat$Bins = dat[,2]

  Temp2<-merge(dat,Temp,by="Bins",all.x = TRUE)

  dat=Temp2[,c(1,2,4)]
  WeOfEv=dat[,3]
  FinBins=dat[,1]
  dat[,4]<-ifelse(dat[,2]==1,0,1)
  Check2<-data.frame(aggregate(dat[,2]~dat[,3]+dat[,1],data=dat,FUN=mean))
  names(Check2)<-c("WoE","Bins","TargetRate")
  Check3<-data.frame(table(dat[,1]))
  names(Check3)<-c("Bins","Freq")
  Check4<-merge(Check2,Check3,by="Bins")



  ### Visualization for the WOE bins

  ## Barplot for the WOE for each
  a<-ggplot2::ggplot() +
    ggplot2::geom_bar(data = Check4, ggplot2::aes(x=Check4$Bins, y=Check4$WoE) , fill=color, stat = "identity") +
    ggplot2::theme(legend.position="none", axis.text.x=ggplot2::element_text(angle=45, hjust=1))+
    ggplot2::labs(x = "",y="WoE")

  ## Barplot for the bin frequency
  b<-ggplot2::ggplot() +
    ggplot2::geom_bar(data = Check4, ggplot2::aes(x=Check4$Bins, y=Check4$Freq), fill=color, stat = "identity") +
    ggplot2::theme(legend.position="none", axis.text.x=ggplot2::element_text(angle=45, hjust=1))+
    ggplot2::labs(x = "",y="Bin Frequency")

  ## Barplot for the target rate
  c<-ggplot2::ggplot() +
    ggplot2::geom_bar(data = Check4, ggplot2::aes(x=Check4$Bins, y=Check4$TargetRate), fill=color, stat = "identity") +
    ggplot2::theme(legend.position="none", axis.text.x=ggplot2::element_text(angle=45, hjust=1))+
    ggplot2::labs(x = "",y="Target Rate")

  ## Command arranging the order of the three barplots and placing the variable IV at the top
  e<-gridExtra::grid.arrange(a,c,b, ncol=3, nrow=1,top=paste(var_name,"\n IV=",IVF))


  return(var_bins)
}


#' Custom Binning Factor Variables
#'
#' Function that bins a factor variable based on user inputted factor levels, plots the information on the new bins, and returns a vector of the newly binned values
#'
#' @param dataset Dataframe containing the target variable and desired factor variables to be binned.
#' @param var A specific factor attribute to be binned.
#' @param target A binary target variable. Must be specified.
#' @param new_levels A vector the same length as the number of levels
#' for the categorical variable containing the new factor levels.
#' Must be specified.
#' @param color A hexadecimal value representing a specific color.
#'
#' @return A vector containing the newly binned values.
#' Generates three barplots displaying the worth of evidence (WoE),
#' target rate and frequency across all bins.
#' The bars are arranged in an ascending order of WoE.
#'
#'
#' @examples mydata <- ISLR::Default
#' @examples mydata$default <- ifelse(mydata$default=="Yes", 1, 0) ## target coded with 1, 0
#' @examples ## WOE_customFactor
#' @examples custom1 <- WOE_customFactor(mydata, var="student", target="default",
#' @examples                  new_levels=c("Student : No","Student : Yes"))
#' @examples levels(custom1)
#' @examples ## --------------------------
#' @examples mydata$balance_cat <- cut(mydata$balance, breaks = c(-1,400,800,1200,1600,2000,2400,2800),
#' @examples                       labels = c("Very-Low","Low","Med-Low","Med","Med-High","High","Very-High"))
#' @examples custom2 <- WOE_customFactor(mydata, var="balance_cat", target="default",
#' @examples                    new_levels=c(1,1,2,2,2,3,3))
#' @examples levels(custom2)
#'
#' @export

WOE_customFactor <- function(dataset, var, target, new_levels, color = "#0066CC"){

  ## Creating default values for function inputs
  if(is.numeric(var) == TRUE){var = colnames(dataset)[var]}
  if(missing(color)){color = "#0066CC"}
  if(missing(target)){
    print("ERROR: Target variable input is missing.")
    return()
  }

  ## Storing the old factor variables
  var_bins = dataset[,var]

  old_levels = levels(var_bins)


  fact = data.frame(A = as.character(old_levels), B = as.character(new_levels))


  counter = unique(fact[,2])

  lookup = NULL
  for(i in counter){
    tmp = subset(fact, fact$B == i)
    lookup = rbind(lookup,c(i,paste(tmp$A, collapse = "/")))
  }
  lookup = data.frame(lookup)

  new_levels = as.character(lookup[match(new_levels,lookup[,1]),2])


  levels(var_bins) = new_levels


  var_name = paste(var,"_Bins", sep = "")

  dat = data.frame(dataset[,target],var_bins)

  ## Calculating "good" and "bad" events for each bin
  NumBad<-aggregate(dat[,1]~dat[,2],data=dat,FUN=sum)
  NumGood<-aggregate((ifelse(dat[,1]==1,0,1))~dat[,2], data=dat,FUN=sum)

  ## Calculating the WOE for each bin and IV for the variable
  IVF1_i<-(NumBad[,2]/sum(NumBad[,2]))-(NumGood[,2]/sum(NumGood[,2]))
  IVF2_i<-log((NumBad[,2]/sum(NumBad[,2]))/(NumGood[,2]/sum(NumGood[,2])))
  IVF3_i<-log(((NumBad[,2]+.5)/(sum(NumBad[,2])+.5))/((NumGood[,2]+.5)/(sum(NumGood[,2])+.5)))
  IVF23_i<-ifelse(IVF2_i==-Inf|IVF2_i==Inf,IVF3_i,IVF2_i)
  IVF_i<-IVF1_i*IVF23_i
  IVF<-sum(IVF_i)

  ## Creating the dataframe to contain the bins, WOE, bad rate, and frequency
  Temp<-data.frame(NumBad[,1],IVF23_i)
  colnames(Temp) = c("Bins","woe")
  dat$Bins = dat[,2]

  Temp2<-merge(dat,Temp,by="Bins",all.x = TRUE)

  dat=Temp2[,c(1,2,4)]
  WeOfEv=dat[,3]
  FinBins=dat[,1]
  dat[,4]<-ifelse(dat[,2]==1,0,1)
  Check2<-data.frame(aggregate(dat[,2]~dat[,3]+dat[,1],data=dat,FUN=mean))
  names(Check2)<-c("WoE","Bins","TargetRate")
  Check3<-data.frame(table(dat[,1]))
  names(Check3)<-c("Bins","Freq")
  Check4<-merge(Check2,Check3,by="Bins")

  ### Visualization for the WOE bins

  ## Barplot for the WOE for each
  a<-ggplot2::ggplot() +
    ggplot2::geom_bar(data = Check4, ggplot2::aes(x=Check4$Bins, y=Check4$WoE) , fill=color, stat = "identity") +
    ggplot2::theme(legend.position="none", axis.text.x=ggplot2::element_text(angle=45, hjust=1))+
    ggplot2::labs(x = "",y="WoE")

  ## Barplot for the bin frequency
  b<-ggplot2::ggplot() +
    ggplot2::geom_bar(data = Check4, ggplot2::aes(x=Check4$Bins, y=Check4$Freq), fill=color, stat = "identity") +
    ggplot2::theme(legend.position="none", axis.text.x=ggplot2::element_text(angle=45, hjust=1))+
    ggplot2::labs(x = "",y="Bin Frequency")

  ## Barplot for the target rate
  c<-ggplot2::ggplot() +
    ggplot2::geom_bar(data = Check4, ggplot2::aes(x=Check4$Bins, y=Check4$TargetRate), fill=color, stat = "identity") +
    ggplot2::theme(legend.position="none", axis.text.x=ggplot2::element_text(angle=45, hjust=1))+
    ggplot2::labs(x = "",y="Target Rate")

  ## Command arranging the order of the three barplots and placing the variable IV at the top
  e<-gridExtra::grid.arrange(a,c,b, ncol=3, nrow=1,top=paste(var_name,"\n IV=",IVF))

  return(var_bins)

}



