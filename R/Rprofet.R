#' @title Coarse Binning Variable(s)
#' @description Function that bins selected variable(s) and returns a dataframe with binned values. Uses greedy binning algorithm to perform coarse binning of selected variable(s).
#' @param data Dataframe of that contains ID, binary target and variables to be binned.
#' @param id ID variable. See 'Details'.
#' @param target The binary target/response variable for WOE. See 'Details'.
#' @param varcol Vector of variables to be binned.
#' @param min.cat Minimum number of bins.
#' @param num.bins Target number of bins. Overridden by the number of levels if varcol is factor.
#' @param min.pts.bin Minimum number of observations in a bin.
#' @param bracket Indicating if the intervals should be closed on the right or left. Options include left and right.
#' @param special.values A vector of values that should have their own bin. See 'Details'.
#' @param sort_id Logical. The default is FALSE which does not sort the data by ID column. If TRUE, then data is sorted increasingly by ID column.
#'
#' @return A dataframe containing the ID, target, and binned variable(s) with corresponding binned values.
#' @export
#'
#' @examples
#'mydata <- ISLR::Default
#'head(mydata)
#'mydata$ID <- seq(1:nrow(mydata)) ## make an ID variable
#'mydata$default <- ifelse(mydata$default=="Yes", 1, 0) ## target coded with 1, 0
#'
#'## bin balance and income
#'binned1 <- BinProfet(mydata, id="ID", target="default",
#'                         varcol = c("balance",  "income"), num.bins = 5)
#'head(binned1)
#'
#'## bin categorical variable-------------------
#'binned2 <- BinProfet(mydata, id="ID", target="default",
#'                         varcol = "student", num.bins = 5)
#'head(binned2)
#'summary(binned2$student_Bins) ## num.bins overriden

BinProfet <-  function (data, id, target, varcol, min.cat = 4, num.bins = 10,
                        min.pts.bin = 25, bracket = "left", special.values = NULL, sort_id = FALSE)
{
  ## Obtaining the input column names or column numbers depending which input
  if (is.character(target) == TRUE) {
    target <- match(target, colnames(data))
  }
  if (is.character(id) == TRUE) {
    id_char <- id
    id <- match(id, colnames(data))
  } else if (is.numeric(id) == TRUE) {
    id_char <- colnames(data)[id]
  }
  if (missing(varcol)) {
    varcol <- (1:ncol(data))[c(-id, -target)]
  }
  if (is.character(varcol) == TRUE) {
    varcol <- match(varcol, colnames(data))
  }

  ## Function that bins the variables based on user inputs
  BinFun1 <- function(data, varcol, badcol, idcol, min.cat,
                      num.bins, min.pts.bin, bracket, special.values) {
    options(scipen = 9999999)
    if (is.numeric(data[, varcol]) == FALSE) { #For Non Numeric variables
      NAS <- data[which(is.na(data[, varcol])), c(idcol,
                                                  varcol)] #get NA rows with id and var col
      Norm <- data[which(!is.na(data[, varcol])), c(idcol,
                                                    varcol)] #get not NA rows with id and var col
      if (nrow(NAS) > 0) {
        NAS$Bins <- "Missing" #assign to NA rows
        Norm$Bins <- as.character(Norm[, 2]) #make sure the else are character
        final <- rbind(NAS, Norm)
      }
      else if (nrow(NAS) == 0) {
        Norm$Bins <- as.character(Norm[, 2])
        final <- Norm
      }
    }
    else if (is.numeric(data[, varcol]) == TRUE) { #numerical variables
      NAS <- data[which(is.na(data[, varcol])), c(idcol,
                                                  varcol)]
      spec <- data[which(data[, varcol] %in% special.values), #Special values
                   c(idcol, varcol)]
      Norm <- data[which(!is.na(data[, varcol]) & !(data[,
                                                         varcol] %in% special.values)), c(idcol, varcol)]
      if (nrow(NAS) > 0 & nrow(spec) > 0) {#if both NA and Special values exist
        NAS$Bins <- "Missing"
        spec$Bins <- as.character(spec[, 2])
        if (length(table(Norm[, 2])) < min.cat) { #if the number of categories are smaller than mincat
          Norm$Bins <- as.character(Norm[, 2])        #then the var is just saved as character
        }
        else if (length(table(Norm[, 2])) >= min.cat) {
          findbins <- binr::bins.greedy(Norm[, 2], nbins = num.bins, #:: allows to specify the specific function when multiple packages have the same function name
                                        minpts = min.pts.bin)
          if (bracket == "right") {
            Norm$Bins = as.character(cut(Norm[, 2], breaks = c(-Inf,
                                                               findbins$binhi[1:length(unique(findbins$binhi)) -
                                                                                1], Inf), dig.lab=6))
          }
          else if (bracket == "left") {
            Norm$Bins = as.character(cut(Norm[, 2], breaks = c(-Inf,
                                                               findbins$binlo[1:length(unique(findbins$binlo))],
                                                               Inf), right = FALSE, dig.lab=6))
          }
        }
        final <- rbind(NAS, spec, Norm)
      }
      else if (nrow(NAS) > 0 & nrow(spec) == 0) {#when there are missing values but no special values
        NAS$Bins <- "Missing"
        if (length(table(Norm[, 2])) < min.cat) {
          Norm$Bins <- as.character(Norm[, 2])
        }
        else if (length(table(Norm[, 2])) >= min.cat) {
          findbins <- binr::bins.greedy(Norm[, 2], nbins = num.bins,
                                        minpts = min.pts.bin)
          if (bracket == "right") {
            Norm$Bins = as.character(cut(Norm[, 2], breaks = c(-Inf,
                                                               findbins$binhi[1:length(unique(findbins$binhi)) -
                                                                                1], Inf), dig.lab=6))
          }
          else if (bracket == "left") {
            Norm$Bins = as.character(cut(Norm[, 2], breaks = c(-Inf,
                                                               findbins$binlo[1:length(unique(findbins$binlo))],
                                                               Inf), right = FALSE, dig.lab=6))
          }
        }
        final <- rbind(NAS, Norm)
      }
      else if (nrow(NAS) == 0 & nrow(spec) > 0) {#when there are sepcial values but no NAs
        spec$Bins <- as.character(spec[, 2])
        if (length(table(Norm[, 2])) < min.cat) {
          Norm$Bins <- as.character(Norm[, 2])
        }
        else if (length(table(Norm[, 2])) >= min.cat) {
          findbins <- binr::bins.greedy(Norm[, 2], nbins = num.bins,
                                        minpts = min.pts.bin)
          if (bracket == "right") {
            Norm$Bins = as.character(cut(Norm[, 2], breaks = c(-Inf,
                                                               findbins$binhi[1:length(unique(findbins$binhi)) -
                                                                                1], Inf), dig.lab=6))
          }
          else if (bracket == "left") {
            Norm$Bins = as.character(cut(Norm[, 2], breaks = c(-Inf,
                                                               findbins$binlo[1:length(unique(findbins$binlo))],
                                                               Inf), right = FALSE, dig.lab=6))
          }
        }
        final <- rbind(spec, Norm)
      }
      else if (nrow(NAS) == 0 & nrow(spec) == 0) {#When no NAs and no special values
        if (nrow(spec) > 0) {
          spec$Bins <- as.character(spec[, 2])
        }
        if (length(table(Norm[, 2])) < min.cat) {
          Norm$Bins <- as.character(Norm[, 2])
        }
        else if (length(table(Norm[, 2])) >= min.cat) {
          findbins <- binr::bins.greedy(Norm[, 2], nbins = num.bins,
                                        minpts = min.pts.bin)
          if (bracket == "right") {
            Norm$Bins = as.character(cut(Norm[, 2], breaks = c(-Inf,
                                                               findbins$binhi[1:length(unique(findbins$binhi)) -
                                                                                1], Inf), dig.lab=6))
          }
          else if (bracket == "left") {
            Norm$Bins = as.character(cut(Norm[, 2], breaks = c(-Inf,
                                                               findbins$binlo[1:length(unique(findbins$binlo))],
                                                               Inf), right = FALSE, dig.lab=6))
          }
        }
        final <- Norm
      }
    }
    Bins <- data.frame(final, stringsAsFactors = TRUE)
    Bins[, 3] <- as.factor(Bins[, 3])
    colnames(Bins)[3] <- paste(colnames(Bins)[2], "Bins",
                               sep = "_")
    return(c(Bins))
  }

  ## Wrapper function that calls BinFun1 function based on user function inputs
  BinFun2 <- function(varcol) {
    BinFun1(data, varcol, badcol = target, idcol = id, min.cat,
            num.bins, min.pts.bin, bracket, special.values)
  }

  ## Using apply function to call BinFun2 for each variable individually
  z <- lapply(varcol, BinFun2)

  ## Reduce the list into a single dataframe of the binned vlaues
  Merged = Reduce(function(x, y) merge(x, y, all.x = T, by = id_char, sort = sort_id),z)
  Merged2 = merge(data[, c(id, target)], Merged, by = id_char, sort = sort_id)
  Merged2 = Merged2[, c(1, 2, seq(4, ncol(Merged2), by = 2))]
  findLo = function(vec) {
    if (sapply(strsplit(vec, ","), length) == 1) {
      return(NA)
    }
    tmp = strsplit(vec, ",")
    lo = as.numeric(tmp[[1]][1])
    return(lo)
  }
  reorder_levels = function(vars) {
    x = gsub("\\[|\\]|\\(|\\)", "", levels(vars))
    vec = sapply(x, findLo)
    NewBins = factor(vars, levels(vars)[order(vec)])
    return(NewBins)
  }


  Merged2[, 3:ncol(Merged2)] = as.data.frame(lapply(Merged2[,
                                                            3:ncol(Merged2), drop = FALSE], reorder_levels))
  return(Merged2)

}


#' @title WOE Transformation
#' @description Function that calculates the WOE for each bin and the information value for each variable.
#' @param data Dataframe of binned variables.
#' @param id ID variable.
#' @param target A binary target variable.
#' @param varcol Vector of variables to have WOE transformation.
#'
#' @return A list with the following components.
#' \item{Bin}{Dataframe with the binned variables and their WOE values.}
#' \item{WOE}{Dataframe with the WOE values.}
#' \item{IV}{Each attribute and their associated information values.}
#' \item{vars}{A list containing the different WOE values for each attribute.}
#' @export
#'
#' @examples
#' mydata <- ISLR::Default
#' mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
#' mydata$default<-ifelse(mydata$default=="Yes",1,0) ## Creating numeric binary target variable

#' binned <- BinProfet(mydata, id= "ID", target= "default", num.bins = 5) ## Binning variables

#' WOE_dat <- WOEProfet(binned, "ID", "default", 3:5)

#' head(WOE_dat$Bin)
#' head(WOE_dat$WOE)
#' WOE_dat$IV
#' head(WOE_dat$vars$income)

WOEProfet <- function (data, id, target, varcol)
{
  colnames(data) <- gsub("\\.", "_", colnames(data))

  ## Ifelse statement to identify if the target input is the column number or column name
  ##    If it is the column name, store the name and convert target input to column number
  if (is.character(target)) {
    tar_char <- gsub("\\.", "_", target)
    target <- match(gsub("\\.", "_", target), colnames(data))
  } else if (is.numeric(target)) {
    tar_char <- colnames(data)[target]
  }

  ## Ifelse statement to identify if the ID input is the column number or column name
  ##    If it is the column name, store the name and convert ID input to column number
  if (is.character(id)) {
    id_char <- gsub("\\.", "_", id)
    id <- match(gsub("\\.", "_", id), colnames(data))
  } else if (is.numeric(id)) {
    id_char <- colnames(data)[id]
  }

  ## If the varcol input is missing, use all columns except target and ID columns
  if (missing(varcol)) {
    varcol <- (1:ncol(data))[c(-id, -target)]
  }

  ## If varcol input is characters, convert to column numbers
  if (is.character(varcol) == TRUE) {
    varcol <- match(gsub("\\.", "_", varcol), colnames(data))
  }


  ## Function used to calculate WOE for each bin. Wrapper functions are used to access this function and will
  ##    pass in one binned variable at a time. This function will be called for every variable in varcol input
  WOEFun1 <- function(data, idcol, targetcol, varcol) {

    data2 <- data[, c(idcol, targetcol, varcol)]
    data2$Bins <- as.character(data2[, 3])

    ## Aggregated the number of "Good" and "Bad" instances that occur for each bin
    NumBad <- stats::aggregate(data2[, 2] ~ Bins, data = data2,
                               FUN = sum)
    NumGood <- stats::aggregate((ifelse(data2[, 2] == 1,
                                        0, 1)) ~ Bins, data = data2, FUN = sum)

    ## Calculating the WOE for each bin
    IVF1_i <- (NumBad[, 2]/sum(NumBad[, 2])) - (NumGood[,
                                                        2]/sum(NumGood[, 2]))
    IVF2_i <- log((NumBad[, 2]/sum(NumBad[, 2]))/(NumGood[,
                                                          2]/sum(NumGood[, 2])))
    IVF3_i <- log(((NumBad[, 2] + 0.5)/(sum(NumBad[, 2]) +
                                          0.5))/((NumGood[, 2] + 0.5)/(sum(NumGood[, 2]) +
                                                                         0.5)))
    IVF23_i <- ifelse(IVF2_i == -Inf | IVF2_i == Inf, IVF3_i,
                      IVF2_i)

    ## Calculating the IV for each bin
    IVF_i <- IVF1_i * IVF23_i

    ## Attaching WOE to ID, Target, and bin and renaming columns
    Temp <- data.frame(cbind(NumBad[, 1], IVF23_i))
    Temp <- plyr::rename(Temp, c(V1 = "Bins", IVF23_i = "woe"))
    Temp2 <- plyr::join(data2, Temp, by = "Bins", type = "left",
                        match = "all")

    ##Further dataframe management to obtain right format, naming structure, and contain ID, bin, and WOE
    WOE <- data.frame(Temp2)
    colnames(WOE)[5] <- paste(gsub("_Bins", "", colnames(WOE)[3]), "WOE", sep = "_")
    WOE[, 5] <- as.numeric(as.character(WOE[, 5]))
    WOE2 <- WOE[, c(1, 3, 5)]
    return(c(WOE2))
  }

  ## Function used to calculate the Information Value for each variable. Wrapper functions are used to access this function and will
  ##    pass in one binned variable at a time. This function will be called for every variable in varcol input
  IVFun1 <- function(data, idcol, targetcol, varcol) {
    data2 <- data[, c(idcol, targetcol, varcol)]
    data2$Bins <- as.character(data2[, 3])

    ## Aggregated the number of "Good" and "Bad" instances that occur for each bin
    NumBad <- stats::aggregate(data2[, 2] ~ Bins, data = data2,
                               FUN = sum)
    NumGood <- stats::aggregate((ifelse(data2[, 2] == 1,
                                        0, 1)) ~ Bins, data = data2, FUN = sum)

    ##Calculate WOE for each bin and subsequently IV for each bin
    IVF1_i <- (NumBad[, 2]/sum(NumBad[, 2])) - (NumGood[,
                                                        2]/sum(NumGood[, 2]))
    IVF2_i <- log((NumBad[, 2]/sum(NumBad[, 2]))/(NumGood[,
                                                          2]/sum(NumGood[, 2])))
    IVF3_i <- log(((NumBad[, 2] + 0.5)/(sum(NumBad[, 2]) +
                                          0.5))/((NumGood[, 2] + 0.5)/(sum(NumGood[, 2]) +
                                                                         0.5)))
    IVF23_i <- ifelse(IVF2_i == -Inf | IVF2_i == Inf, IVF3_i,
                      IVF2_i)
    IVF_i <- IVF1_i * IVF23_i

    ## Sum up IV values to obtain IV for the variable
    IVF <- sum(IVF_i)
    IVF2 <- c(colnames(data2[3]), IVF)
    return(c(IVF2))
  }

  ## Wrapper Function for WOEFun1. Uses inputs obtained from WOEFun3 function call.
  WOEFun2 <- function(varcol) {
    WOEFun1(data, idcol = id, targetcol = target, varcol)
  }

  ## Using lapply function to pass in each variable individually. Returns a list WOE2 data frame from WOEFun1
  z <- lapply(varcol, WOEFun2)
  Merged = Reduce(function(x, y) merge(x, y, all.x = T, by = id_char),
                  z)

  ## Attaching the ID and Target variables
  Merged2 = merge(data[, c(id_char, tar_char)], Merged, by = id_char)

  ## Obtaining ID, Target, and WOE variables
  Merged3 = Merged2[, c(1, 2, seq(4, ncol(Merged2), by = 2))]

  ## Wrapper Function for IVFun1. Uses inputs obtained from WOEFun3 function call.
  IVFun2 <- function(varcol) {
    IVFun1(data = data, idcol = id, targetcol = target, varcol)
  }

  ## Using lapply function to pass in each variable individually.
  IV_and_Var <- lapply(varcol, IVFun2)
  IV = NULL

  ## Creating a dataframe from the returned list that includes the variable and the corresponding IV.
  IV_and_Var2 <- data.frame(Variable = sapply(IV_and_Var, "[",
                                              1), IV = as.numeric(sapply(IV_and_Var, "[", 2)))
  IV_and_Var2$Variable = as.character(IV_and_Var2$Variable)

  Merged2_bin <- Merged2 %>% dplyr::select(c(1,2,seq(3, ncol(Merged2), by = 2)))

  Merged2_tmp <- Merged2 %>% dplyr::rename(ID = dplyr::all_of(id_char),
                                           Target = dplyr::all_of(tar_char))

  ## Function that generates a sql statement that displays the distinct bins and the corresponding WOE for each variable.
  VarWOE = function(var) {
    sql_state = paste("Select", var, ",", gsub("_Bins", "_WOE", var),
                      ",", "avg(Target) as TargetRate,", "count(ID) as Freq",
                      "from", deparse(substitute(Merged2_tmp)), "group by",
                      var, ",", gsub("_Bins", "_WOE", var), sep = " ")
    chk = sqldf::sqldf(sql_state)
    return(chk)
  }

  ## Using Apply function to generate sql statements for each variable individually
  ##  returning a list that will show the distinct bins and corresponding WOE for each inputted variable.
  woeList = lapply(colnames(data)[varcol], VarWOE)
  names(woeList) = gsub("_Bins", "", colnames(data)[varcol])
  item <- list(Bin = Merged2_bin, WOE = Merged3, IV = IV_and_Var2,
               vars = woeList)
  return(item)
}



#' @title Scorecard Builder
#' @description Function that fits a logistic regression models and scores points for each bin and calculates observations' total score.
#' @param object A WOEProfet object or a Var_select object that containing dataframes with binned and WOE values.
#' @param id ID variable.
#' @param target A binary target variable.
#' @param GLModel A generalized linear model, glm object.
#' @param PDO Points to Double Odds.
#' @param BaseOdds Base Odds.
#' @param BasePts Base Points.
#' @param reverse Logical. If FALSE, higher points corresponds to a lower probability of being target.
#'
#' @return A scorecard dataframe.
#' @export
#'
#' @examples
#' mydata <- ISLR::Default
#' mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
#' mydata$default<-ifelse(mydata$default=="Yes",1,0) ## Creating numeric binary target variable
#'
#' binned <- BinProfet(mydata, id= "ID", target= "default", num.bins = 5) ## Binning variables
#'
#' WOE_dat <- WOEProfet(binned, "ID","default", 3:5) ## WOE transformation of bins
#'
#' md <- glm(default ~ student_WOE+balance_WOE+income_WOE, data=WOE_dat$WOE, family="binomial")
#' summary(md)
#'
#' Score_dat <- ScorecardProfet(object=WOE_dat, id="ID", target="default", GLModel=md,
#'                                  PDO = 50, BaseOdds = 10, BasePts = 1000, reverse = FALSE)
#'
#' Score_dat ## Less points means more likely to default

ScorecardProfet <- function (object, id, target, GLModel, PDO = 100, BaseOdds = 10,
                             BasePts = 1000, reverse = FALSE)
{
  dataWoe = object[[2]]


  ## Ifelse statement to identify if the target input is column names
  ##    If it is obtain the column numbers
  if (is.character(target) == TRUE) {
    tar_num <- match(target, colnames(dataWoe))
  } else if (is.numeric(target) == TRUE) {
    tar_num <- target
    target <- colnames(dataWoe)[target]
  }

  ## If ID input is column names, convert it to column numbers
  if (is.character(id) == TRUE) {
    id <- match(id, colnames(dataWoe))
  }

  varcol <- colnames(dataWoe)[c(-id, -tar_num)]

  ## Calculating the factor and offset based on user inputs
  factor = PDO/log(2)
  offset = BasePts - (factor * log(BaseOdds))
  logmod <- GLModel
  modelcoef = as.numeric(logmod$coefficients)
  modelterms = as.factor(labels(logmod$coefficients))
  a = modelcoef[1]
  n = length(modelcoef) - 1
  datBin = object[[1]]

  if (reverse == TRUE) {
    modelcoef <- -modelcoef
  }

  ## Function to calculate scorecard points and final score
  calc_points <- function(datBin, dataWoe, varcol, modelcoef, modelterms,
                          a, n, factor, offset) {
    pat = gsub("WOE", "", varcol[1])
    score_bins = data.frame(bin=datBin[, colnames(datBin)[which(stringr::str_detect(colnames(datBin),pat))]],
                            woe=dataWoe[, colnames(dataWoe)[which(stringr::str_detect(colnames(dataWoe),pat))]])

    colnames(score_bins) = c(paste(pat,"Bins",sep=""), paste(pat,"WOE",sep=""))
    score_bins$score = round(-(dataWoe[, varcol] * modelcoef[which(modelterms ==
                                                                     varcol)] + a/n) * factor + offset/n, 0)
    colnames(score_bins)[3] = paste(varcol, "Points", sep = "_")
    return(score_bins)
  }
  card_wrapper <- function(varcol) {
    calc_points(datBin, dataWoe, varcol, modelcoef, modelterms,
                a, n, factor, offset)
  }
  z = lapply(varcol, card_wrapper)
  tmp = data.frame(ID = datBin[, id], default = datBin[, target], as.data.frame(z))
  colnames(tmp)[1:2] = colnames(datBin)[1:2]
  tmp$Score = rowSums(tmp[, seq(5, ncol(tmp), by = 3)], na.rm = TRUE)
  lvl_order <- function(varcol) {
    tmp_lvls = as.numeric(tmp[, gsub("_WOE", "_Bins", varcol)]) #getting the bin cols
    return(tmp_lvls)
  }
  lvls = lapply(varcol, lvl_order)
  lvls2 = as.data.frame(lvls)
  name_lvls <- function(varcol) {
    cnames = paste(gsub("_WOE", "_Bins", varcol), "Level", sep = "_")
    return(cnames)
  }
  colnames(lvls2) = sapply(varcol, name_lvls)
  tmp2 = data.frame(tmp, lvls2)
  scard <- function(varcol) {
    attribute = gsub("_WOE", "", varcol)
    sql_state = paste(paste("Select '", attribute, "' as Attribute,", sep = ""),
                      paste(attribute, "Bins", sep = "_"), "as Bins,",
                      paste(attribute, "WOE", sep = "_"), "as WOE,",
                      paste(attribute, "WOE_Points", sep = "_"), "as Points from",
                      deparse(substitute(tmp2)), "group by", paste(attribute,
                                                                   "Bins_Level", sep = "_"))
    return(sqldf::sqldf(sql_state))
  }
  chk = lapply(varcol, scard)
  chk2 = do.call("rbind", chk)
  return(chk2)
}




#' @title Hierarchical Variable Clustering
#' @description Function that implements hierarchical clustering on the variables to be used as a form of variable selection.
#' @param object A WOEProfet object containing dataframes with binned and WOE values.
#' @param id ID variable.
#' @param target A binary target variable.
#' @param num_clusts Number of desired clusters.
#' @param method Clustering method to be used. This should be one of "ward.D", "ward.D2", "single", "average", "mcquitty", "median",or "centroid".
#'
#' @return A dataframe indicating the assigned clusters for the predictor variables.
#' @export
#'
#' @examples
#' mydata <- ISLR::Default
#' mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
#' mydata$default<-ifelse(mydata$default=="Yes",1,0) ## Creating numeric binary target variable

#' ## create two new variables from bivariate normal
#' sigma <- matrix(c(45000,-3000,-3000, 55000), nrow = 2)
#' set.seed(10)
#' newvars <- MASS::mvrnorm(nrow(mydata),
#'                          mu=c(1000,200), Sigma=sigma)
#' mydata$newvar1 <- newvars[,1]
#' mydata$newvar2 <- newvars[,2]
#' binned <- BinProfet(mydata, id= "ID", target= "default", num.bins = 5) ## Binning variables
#' WOE_dat <- WOEProfet(binned, "ID","default")
#' ## Cluster variables by WOEClust_hclust
#' clusters <- WOEclust_hclust(WOE_dat, id="ID", target="default", num_clusts=3)
#' clusters

WOEclust_hclust <- function (object, id, target, num_clusts, method = "ward.D")
{
  dat = object[[2]]

  ## Adjusting ID, target inputs to be column numbers
  if (is.character(id) == TRUE) {
    id = match(id, colnames(dat))
  }
  if (is.character(target) == TRUE) {
    target = match(target, colnames(dat))
  }
  woe_trans = t(dat[, -c(id, target)])
  dist.probes = stats::dist(woe_trans)
  probes.complete = stats::hclust(dist.probes, method = method)
  groups = stats::cutree(probes.complete, k = num_clusts)
  Memberships <- data.frame(groups)
  Memberships <- cbind(rownames(Memberships), Memberships)
  rownames(Memberships) <- NULL
  names(Memberships) <- c("Variable", "Group")
  Memberships$Variable = as.character(Memberships$Variable)
  infoVal = object[[3]]
  Memberships$IV = round(infoVal[, 2], 4)
  Memberships = Memberships[order(Memberships$Group, -Memberships$IV),
  ]
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
#' @examples clusters <- WOEclust_kmeans(WOE_dat, id="ID", target="default", num_clusts=3)
#' @examples clusters
#'
#' @export
#'
#' @export
WOEclust_kmeans<-function(object,id,target,num_clusts){

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



#' @title Visualizing WOE and Target Rates
#' @description Function generating three plots: WOE value for each bin, target rate for each bin, and the frequency for each bin.
#' @param data Dataframe containing binned values and a binary target variable.
#' @param target A numeric binary target variable.
#' @param var  The desired WOE binned attribute to visualize.
#' @param color A hexadecimal value representing a specific color.
#'
#' @details A list of the hexadecimal colors can be found at this link http://www.sthda.com/sthda/RDoc/images/hextable.gif
#'
#' @export
#'
#' @examples
#' mydata <- ISLR::Default
#' mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
#' mydata$default<-ifelse(mydata$default=="Yes",1,0) ## Creating numeric binary target variable

#' binned <- BinProfet(mydata, id= "ID", target= "default", num.bins = 5) ## Binning variables

#' WOEplotter(binned, target= "default", var= "income_Bins")

#' ##--Changing Colors------------------------------
#' WOEplotter(binned, target= "default", var= "income_Bins", color = "#33FF33")


WOEplotter <- function (data, target, var, color = "#0066CC")
{
  ## Converting var and target inputs to the column names
  if (is.character(var) == FALSE) {
    var = colnames(data)[var]
  }
  if (is.character(target) == FALSE) {
    target = colnames(data)[target]
  }
  dat = data[, c(target, var)]

  ## Calculating "good" and "bad" events for each bin
  NumBad <- stats::aggregate(dat[, 1] ~ dat[, 2], data = dat, FUN = sum)
  NumGood <- stats::aggregate((ifelse(dat[, 1] == 1, 0, 1)) ~ dat[,
                                                                  2], data = dat, FUN = sum)

  ## Calculating the WOE for each bin and IV for the variable
  IVF1_i <- (NumBad[, 2]/sum(NumBad[, 2])) - (NumGood[, 2]/sum(NumGood[,
                                                                       2]))
  IVF2_i <- log((NumBad[, 2]/sum(NumBad[, 2]))/(NumGood[, 2]/sum(NumGood[,
                                                                         2])))
  IVF3_i <- log(((NumBad[, 2] + 0.5)/(sum(NumBad[, 2]) + 0.5))/((NumGood[,
                                                                         2] + 0.5)/(sum(NumGood[, 2]) + 0.5)))
  IVF23_i <- ifelse(IVF2_i == -Inf | IVF2_i == Inf, IVF3_i,
                    IVF2_i)
  IVF_i <- IVF1_i * IVF23_i
  IVF <- round(sum(IVF_i), 4)

  ## Creating the dataframe to contain the bins, WOE, bad rate, and frequency
  Temp <- data.frame(NumBad[, 1], IVF23_i)
  colnames(Temp) = c("Bins", "woe")
  dat$Bins = dat[, 2]
  Temp2 <- merge(dat, Temp, by = "Bins", all.x = TRUE)
  dat = Temp2[, c(1, 2, 4)]
  WeOfEv = dat[, 3]
  FinBins = dat[, 1]
  dat[, 4] <- ifelse(dat[, 2] == 1, 0, 1)
  Check2 <- data.frame(stats::aggregate(dat[, 2] ~ dat[, 3] + dat[,
                                                                  1], data = dat, FUN = mean))
  WoE = Bins = TargetRate = Freq = NULL
  names(Check2) <- c("WoE", "Bins", "TargetRate")
  Check3 <- data.frame(table(dat[, 1]))
  names(Check3) <- c("Bins", "Freq")
  Check4 <- merge(Check2, Check3, by = "Bins")

  ### Visualization for the WOE bins

  ## Barplot for the WOE for each
  a <- ggplot2::ggplot() + ggplot2::geom_bar(data = Check4,
                                             ggplot2::aes(x = Bins, y = WoE), fill = color,
                                             stat = "identity") + ggplot2::theme(legend.position = "none",
                                                                                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = "", y = "WoE")

  ## Barplot for the bin frequency
  b <- ggplot2::ggplot() + ggplot2::geom_bar(data = Check4,
                                             ggplot2::aes(x = Bins, y = Freq), fill = color,
                                             stat = "identity") + ggplot2::theme(legend.position = "none",
                                                                                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = "", y = "Bin Frequency")

  ## Barplot for the target rate
  c <- ggplot2::ggplot() + ggplot2::geom_bar(data = Check4,
                                             ggplot2::aes(x = Bins, y = TargetRate),
                                             fill = color, stat = "identity") + ggplot2::theme(legend.position = "none",
                                                                                               axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = "", y = "Target Rate")

  ## Command arranging the order of the three barplots and placing the variable IV at the top
  e <- gridExtra::grid.arrange(a, c, b, ncol = 3, nrow = 1,
                               top = paste(var, "\n IV=", IVF))
}



#' @title Custom Binning Numeric Variables
#' @description Function that bins a numeric variable based on user inputted breaks, plots the information on the new bins,
#' and returns a list contains a dataframe of the newly binned values and id column and more items.
#'
#' @param data Dataframe containing the target variable and desired numeric variables to be binned.
#' @param var A specific numeric attribute to be binned. Must be specified.
#' @param id The unique id variable in the dataframe. Must be specified.
#' @param target A binary target variable. Must be specified.
#' @param breaks A vector of breakpoints for the desired bins. Must be specified.
#' @param right_bracket Logical. Specifying whether the intervals are closed on the right or the left.
#' @param color A hexadecimal value representing a specific color.
#' @param plot Logical. The default is FALSE which does not generate the plots.
#'
#' @return A list with the following components.
#' \item{NewBin}{Dataframe with the binned variable.}
#' \item{BinWOE}{Dataframe with target, binned variable, and WOE values for the bins.}
#' \item{IV}{Information value of the newly binned variable.}
#' \item{vars}{Dataframe with binned variable, WOE values for the bins, Target Rate for each bin, and observation count for each bin.}
#' @export
#'
#' @examples
#' mydata <- ISLR::Default
#' mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
#' mydata$default <- ifelse(mydata$default=="Yes", 1, 0) ## target coded with 1, 0
#'
#' WC_1 <- WOE_customNum(data= mydata, var="balance", id= "ID", target = "default",
#'                       breaks= seq(0,3000,1000))
#' head(WC_1$NewBin)
#' head(WC_1$BinWOE)
#' WC_1$IV
#' WC_1$vars
#'
#' WC_2 <- WOE_customNum(data= mydata, var="income", id= "ID", target = "default",
#'                       breaks=seq(0,75000, 15000))
#' head(WC_2$NewBin)
#' head(WC_2$BinWOE)
#' WC_2$IV
#' WC_2$vars


WOE_customNum <- function (data, var, id, target, breaks, right_bracket = F, color = "#0066CC", plot = FALSE)
{
  ## Obtaining var input column names
  if (is.numeric(var) == TRUE) {
    var = colnames(data)[var]
  }
  if (is.numeric(id) == TRUE) {
    id = colnames(data)[id]
  }
  if (missing(target)) {
    print("ERROR: Target variable input is missing.")
    return()
  }

  ## Using the use inputted values to create new bins based on the desired cutpoints
  var_bins = cut(data[, var], breaks, right = right_bracket, dig.lab=6)
  levels_bins = c(levels(var_bins), "Missing")
  var_bins = as.character(var_bins)
  var_bins = ifelse(is.na(var_bins), "Missing", var_bins)

  var_bins_fac = factor(var_bins, levels = levels_bins)
  data$var_bins = var_bins_fac
  var_bins = var_bins_fac
  colnames(data)[match(deparse(substitute(var_bins)), colnames(data))] = paste(var,
                                                                               "_Bins", sep = "")
  var_name = paste(var, "_Bins", sep = "")
  dat = data.frame(data[, c(id,target)], var_bins)

  ## Calculating "good" and "bad" events for each bin
  NumBad <- stats::aggregate(dat[, 2] ~ dat[, 3], data = dat, FUN = sum)
  NumGood <- stats::aggregate((ifelse(dat[, 2] == 1, 0, 1)) ~ dat[,
                                                                  3], data = dat, FUN = sum)

  ## Calculating the WOE for each bin and IV for the variable
  IVF1_i <- (NumBad[, 2]/sum(NumBad[, 2])) - (NumGood[, 2]/sum(NumGood[,
                                                                       2]))
  IVF2_i <- log((NumBad[, 2]/sum(NumBad[, 2]))/(NumGood[, 2]/sum(NumGood[,
                                                                         2])))
  IVF3_i <- log(((NumBad[, 2] + 0.5)/(sum(NumBad[, 2]) + 0.5))/((NumGood[,
                                                                         2] + 0.5)/(sum(NumGood[, 2]) + 0.5)))
  IVF23_i <- ifelse(IVF2_i == -Inf | IVF2_i == Inf, IVF3_i,
                    IVF2_i)
  IVF_i <- IVF1_i * IVF23_i
  IVF <- round(sum(IVF_i), 4)

  ## Creating the dataframe to contain the bins, WOE, bad rate, and frequency
  Temp <- data.frame(NumBad[, 1], IVF23_i)
  colnames(Temp) = c("Bins", "woe")
  dat$Bins = dat[, 3]

  Temp2 <- plyr::join(dat, Temp, by = 'Bins', match = 'all')
  dat = Temp2[, c(4, 2, 5)]
  WeOfEv = dat[, 3]
  FinBins = dat[, 1]
  dat[, 4] <- ifelse(dat[, 2] == 1, 0, 1)
  Check2 <- data.frame(stats::aggregate(dat[, 2] ~ dat[, 3] + dat[,
                                                                  1], data = dat, FUN = mean))
  WoE = Bins = TargetRate = Freq = NULL
  names(Check2) <- c("WoE", "Bins", "TargetRate")
  Check3 <- data.frame(table(dat[, 1]))
  names(Check3) <- c("Bins", "Freq")
  Check4 <- merge(Check2, Check3, by = "Bins")

  ### Visualization for the WOE bins

  ## Barplot for the WOE for each
  a <- ggplot2::ggplot() + ggplot2::geom_bar(data = Check4,
                                             ggplot2::aes(x = Bins, y = WoE), fill = color,
                                             stat = "identity") + ggplot2::theme(legend.position = "none",
                                                                                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = "", y = "WoE")

  ## Barplot for the bin frequency
  b <- ggplot2::ggplot() + ggplot2::geom_bar(data = Check4,
                                             ggplot2::aes(x = Bins, y = Freq), fill = color,
                                             stat = "identity") + ggplot2::theme(legend.position = "none",
                                                                                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = "", y = "Bin Frequency")

  ## Barplot for the target rate
  c <- ggplot2::ggplot() + ggplot2::geom_bar(data = Check4,
                                             ggplot2::aes(x = Bins, y = TargetRate),
                                             fill = color, stat = "identity") + ggplot2::theme(legend.position = "none",
                                                                                               axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = "", y = "Target Rate")

  ## Command arranging the order of the three barplots and placing the variable IV at the top
  if (plot == TRUE) {
    e <- gridExtra::grid.arrange(a, c, b, ncol = 3, nrow = 1,
                                 top = paste(var_name, "\n IV=", IVF))
  }

  newbin <- data.frame(data[,id], var_bins_fac)
  names(newbin) <- c(id, paste(var, "_Bins", sep = ""))
  item <- list(NewBin = newbin, BinWOE = Temp2[, c(1,2,4,5)], IV = IVF, vars = Check4)
  return(item)
}


#' @title Custom Binning Factor Variables
#' @description Function that bins a factor variable based on user inputted factor levels, plots the information on the new bins,
#' and returns a list contains a dataframe of the newly binned values and id column and more items.
#' @param data Dataframe containing the target variable and desired factor variables to be binned.
#' @param var A specific factor attribute to be binned.
#' @param id The unique id variable in the dataframe. Must be specified.
#' @param target A binary target variable. Must be specified.
#' @param new_levels A vector the same length as the number of levels for the categorical variable containing the new factor levels. Must be specified.
#' @param color A hexadecimal value representing a specific color.
#' @param plot Logical. The default is FALSE which does not generate the plots.
#'
#' @return A list with the following components.
#' \item{NewBin}{Dataframe with the binned variable.}
#' \item{BinWOE}{Dataframe with target, binned variable, and WOE values for the bins.}
#' \item{IV}{Information value of the newly binned variable.}
#' \item{vars}{Dataframe with binned variable, WOE values for the bins, Target Rate for each bin, and observation count for each bin.}
#' @export
#'
#' @examples
#' mydata <- ISLR::Default
#' mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
#' mydata$default <- ifelse(mydata$default=="Yes", 1, 0) ## target coded with 1, 0

#' ## WOE_customFactor
#' custom1 <- WOE_customFac(data=mydata, var="student", id ="ID", target="default",
#'                          new_levels=c("Student : No","Student : Yes"))
#' head(custom1$NewBin)
#' head(custom1$BinWOE)
#' custom1$IV
#' custom1$vars
#' ## --------------------------
#' mydata$balance_cat <- cut(mydata$balance, breaks = c(-1,400,800,1200,1600,2000,2400,2800),
#'                           labels = c("Very-Low","Low","Med-Low","Med",
#'                           "Med-High","High","Very-High"))
#' custom2 <- WOE_customFac(data=mydata, var="balance_cat", id ="ID", target="default",
#'                          new_levels=c(1,1,2,2,2,3,3))
#' head(custom2$NewBin)
#' head(custom2$BinWOE)
#' custom2$IV
#' custom2$vars

WOE_customFac <- function (data, var, id, target, new_levels, color = "#0066CC", plot = FALSE)
{
  ## Creating default values for function inputs
  if (is.numeric(var) == TRUE) {
    var = colnames(data)[var]
  }
  if (is.numeric(id) == TRUE) {
    id = colnames(data)[id]
  }
  if (missing(color)) {
    color = "#0066CC"
  }
  if (missing(target)) {
    print("ERROR: Target variable input is missing.")
    return()
  }

  ## Storing the old factor variables
  var_bins = data[, var]
  old_levels = levels(var_bins)

  fact = data.frame(A = as.character(old_levels), B = as.character(new_levels))

  counter = unique(fact[, 2])
  lookup = NULL
  for (i in counter) {
    tmp = subset(fact, fact$B == i)
    lookup = rbind(lookup, c(i, paste(tmp$A, collapse = "/"))) #combine levels at this step (if exists)
  }
  lookup = data.frame(lookup)

  if(is.numeric(new_levels) == TRUE){
    new_levels = as.character(lookup[match(new_levels, lookup[, 1]), 2])
    levels(var_bins) = new_levels

    var_bins_sort <- data.frame(A=as.numeric(counter), B=levels(var_bins))
    var_bins_sort <- var_bins_sort[order(var_bins_sort$A),]
    new_levels <- var_bins_sort[,2]
    var_bins <- factor(var_bins, levels = new_levels)

  } else if (is.numeric(new_levels) == FALSE){
    new_levels = as.character(lookup[match(new_levels, lookup[, 1]), 1]) #for char
    levels(var_bins) = new_levels
  }

  var_name = paste(var, "_Bins", sep = "")
  dat = data.frame(data[, c(id,target)], var_bins)

  ## Calculating "good" and "bad" events for each bin
  NumBad <- stats::aggregate(dat[, 2] ~ dat[, 3], data = dat, FUN = sum)
  NumGood <- stats::aggregate((ifelse(dat[, 2] == 1, 0, 1)) ~ dat[,
                                                                  3], data = dat, FUN = sum)

  ## Calculating the WOE for each bin and IV for the variable
  IVF1_i <- (NumBad[, 2]/sum(NumBad[, 2])) - (NumGood[, 2]/sum(NumGood[,
                                                                       2]))
  IVF2_i <- log((NumBad[, 2]/sum(NumBad[, 2]))/(NumGood[, 2]/sum(NumGood[,
                                                                         2])))
  IVF3_i <- log(((NumBad[, 2] + 0.5)/(sum(NumBad[, 2]) + 0.5))/((NumGood[,
                                                                         2] + 0.5)/(sum(NumGood[, 2]) + 0.5)))
  IVF23_i <- ifelse(IVF2_i == -Inf | IVF2_i == Inf, IVF3_i,
                    IVF2_i)
  IVF_i <- IVF1_i * IVF23_i
  IVF <- round(sum(IVF_i), 4)

  ## Creating the dataframe to contain the bins, WOE, bad rate, and frequency
  Temp <- data.frame(NumBad[, 1], IVF23_i)
  colnames(Temp) = c("Bins", "woe")
  #  dat$Bins = dat[, 3]
  colnames(dat)[3] = "Bins"
  #  Temp2 <- merge(dat, Temp, by = "Bins", all.x = TRUE)
  Temp2 <- plyr::join(dat, Temp, by = 'Bins', match = 'all')
  dat = Temp2[, c(3, 2, 4)] #Bins, target, woe
  WeOfEv = dat[, 3]
  FinBins = dat[, 1]
  dat[, 4] <- ifelse(dat[, 2] == 1, 0, 1)
  Check2 <- data.frame(stats::aggregate(dat[, 2] ~ dat[, 3] + dat[,
                                                                  1], data = dat, FUN = mean))
  WoE = Bins = TargetRate = Freq = NULL
  names(Check2) <- c("WoE", "Bins", "TargetRate")
  Check3 <- data.frame(table(dat[, 1]))
  names(Check3) <- c("Bins", "Freq")
  Check4 <- merge(Check2, Check3, by = "Bins")

  ### Visualization for the WOE bins

  ## Barplot for the WOE for each
  a <- ggplot2::ggplot() + ggplot2::geom_bar(data = Check4,
                                             ggplot2::aes(x = Bins, y = WoE), fill = color,
                                             stat = "identity") + ggplot2::theme(legend.position = "none",
                                                                                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = "", y = "WoE")

  ## Barplot for the bin frequency
  b <- ggplot2::ggplot() + ggplot2::geom_bar(data = Check4,
                                             ggplot2::aes(x = Bins, y = Freq), fill = color,
                                             stat = "identity") + ggplot2::theme(legend.position = "none",
                                                                                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = "", y = "Bin Frequency")

  ## Barplot for the target rate
  c <- ggplot2::ggplot() + ggplot2::geom_bar(data = Check4,
                                             ggplot2::aes(x = Bins, y = TargetRate),
                                             fill = color, stat = "identity") + ggplot2::theme(legend.position = "none",
                                                                                               axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = "", y = "Target Rate")

  ## Command arranging the order of the three barplots and placing the variable IV at the top
  if (plot == TRUE) {
    e <- gridExtra::grid.arrange(a, c, b, ncol = 3, nrow = 1,
                                 top = paste(var_name, "\n IV=", IVF))
  }

  newbin <- data.frame(data[,id], var_bins)
  names(newbin) <- c(id, paste(var, "_Bins", sep = ""))
  item <- list(NewBin = newbin,
               BinWOE = Temp2,
               IV = IVF,
               vars = Check4)
  return(item)
}


#' @title Select variables or filter variables by information value
#' @description Function that selects specified variables or filters variables based on information value for WOEProfet object or WOE_StepAIC object.
#' @param object WOEProfet object.
#' @param id ID variable.
#' @param target A binary target variable.
#' @param varcol Vector of variables to be selected or removed. Character or numeric.
#' @param IVfilter Threshold of variables' Information Value.
#'
#' @return A list with the following components.
#' \item{Bin}{Dataframe with ID, Target, and selected binned variables.}
#' \item{WOE}{Dataframe with ID, Target, and WOE values for selected binned variables.}
#' \item{IV}{Information value of the selected binned variables.}
#' \item{vars}{List containing a dataframe for each variable that consists of Bin, WOE, Target Rate, and observation count.}
#' @export
#'
#' @examples
#' mydata <- ISLR::Default
#' mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
#' mydata$default<-ifelse(mydata$default=="Yes",1,0) ## Creating numeric binary target variable

#' binned <- BinProfet(mydata, id= "ID", target= "default", num.bins = 5) ## Binning variables

#' WOE_dat <- WOEProfet(binned, "ID", "default", 3:5) ## WOEProfet object

#' WOE_dat$IV #IV item, the row index will be used for filtering variables

#' # To remove the income variable from the WOEProfet object
#' ## Select the first two variables based on the IV item
#' subWOE1 <- Var_select(WOE_dat, id= "ID", target= "default", varcol= c(1,2))
#' ##  Or remove the third variable based on the IV item
#' subWOE2 <- Var_select(WOE_dat, id= "ID", target= "default", varcol= -3)

#' ## Filter the WOEProfet object based on variables' information values
#' subWOE3 <- Var_select(WOE_dat, id= "ID", target= "default", IVfilter = 0.05)


Var_select <- function (object, id, target, varcol, IVfilter)
{
  bin = object[[1]]
  woe = object[[2]]
  IVdata = object[[3]]


  ## Creating default values for function inputs
  if (is.character(target)) {
    tar_char <- target
    target <- match(target, colnames(woe))
  } else if (is.numeric(target)) {
    tar_char <- colnames(woe)[target]
  }
  if (is.character(id)) {
    id_char <- id
    id <- match(id, colnames(woe))
  } else if (is.numeric(id)) {
    id_char <- colnames(woe)[id]
  }
  if (missing(varcol)) {
    varcol <- 1:nrow(IVdata)
  }
  if (is.character(varcol)) {
    varcol <- match(varcol, IVdata[,1])
  }
  if (missing(IVfilter)) {
    IVfilter <- 0
  }

  IVdata <- IVdata %>% dplyr::slice(varcol)
  IV = NULL
  IV_filtered <- IVdata %>% dplyr::filter(IV >= IVfilter) %>%
    dplyr::arrange(dplyr::desc(IV))

  filter_string <- IV_filtered[, 1]

  ## Selecting the data for the BIN and WOE dataframes
  bin2 <- bin %>% dplyr::select(c(dplyr::all_of(id_char), dplyr::all_of(tar_char)),
                                dplyr::contains(c(filter_string)))

  woe2 <- woe %>% dplyr::select(c(1, 2, dplyr::contains(c(gsub("_Bins", "_WOE", filter_string)))))

  binwoe <- dplyr::left_join(bin2, woe2[,-2], by=id_char) %>%
    dplyr::rename(ID = dplyr::all_of(id_char), Target = dplyr::all_of(tar_char))

  ## Creating the dataframes for each variable in the vars list
  VarWOE = function(var) {
    sql_state = paste("Select", var, ",", gsub("_Bins", "_WOE", var),
                      ",", "avg(Target) as TargetRate,", "count(ID) as Freq",
                      "from", deparse(substitute(binwoe)), "group by",
                      var, ",", gsub("_Bins", "_WOE", var), sep = " ")
    chk = sqldf::sqldf(sql_state)
    return(chk)
  }
  woeList = lapply(filter_string, VarWOE)
  names(woeList) = gsub("_Bins", "", IV_filtered[, 1])

  item <- list(Bin = bin2, WOE = woe2, IV = IV_filtered, vars = woeList)

}


#' @title Score a Validation Data Set
#' @description Function that scores the validation set using the scorecard from the ScorecardProfet object created by
#' the training set.
#' @param data The validation data set, which should be binned in the same way as the scorecard in the card argument.
#' @param card A ScorecardProfet object. The object should be created by using the training set split from the same dataframe
#' as the validation set.
#' @param id ID variable.
#' @param target A binary target variable.
#'
#' @return A dataframe of scored validation set.
#' @export
#'
#' @examples
#' mydata <- ISLR::Default
#' mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
#' mydata$default<-ifelse(mydata$default=="Yes",1,0) ## Creating numeric binary target variable
#'
#' binned <- BinProfet(mydata, id= "ID", target= "default", num.bins = 5) ## Binning variables
#'
#' WOE_dat <- WOEProfet(binned, "ID","default", 3:5) ## WOE transformation of bins
#'
#' md <- glm(default ~ student_WOE+balance_WOE+income_WOE, data=WOE_dat$WOE, family="binomial")
#' summary(md)
#'
#' Score_card <- ScorecardProfet(object=WOE_dat, id="ID", target="default", GLModel=md,
#'                                  PDO = 50, BaseOdds = 10, BasePts = 1000, reverse = FALSE)
#' Score_card ## scorecard
#'
#' ## Scoring the data
#' # variable names needs to be the same as the Attributes on scorecard
#' colnames(binned)
#' colnames(binned)[3:5] <- c("student", "balance", "income") #change the variable name
#' Score_dat = ScoreDataProfet(data=binned, card=Score_card, id="ID", target="default") #scoring data
#' head(Score_dat)


ScoreDataProfet <- function (data, card, id, target)
{
  ## Creating default values for function inputs
  score = card
  #`%>%` <- magrittr::`%>%`
  if (is.character(target) == TRUE) {
    tar_char <- target
    target <- match(target, colnames(data))
  } else if (is.numeric(target) == TRUE) {
    tar_char <- colnames(data)[target]
  }

  if (is.character(id) == TRUE) {
    id_char <- id
    id <- match(id, colnames(data))
  } else if (is.numeric(id) == TRUE) {
    id_char <- colnames(data)[id]
  }

  varcol <- unique(score$Attribute)

  Fun1 <- function(data, id, target, varcol, score)
  {
    if (!is.numeric(data[, varcol])) { #For Non Numeric variables
      tb = score[score$Attribute == varcol,] #subset for each variable

      test1 <- dplyr::left_join(tb, data[,c(id_char,varcol)]
                                ,by=c("Bins" = varcol), multiple = "all")

      df <- test1 %>% dplyr::select(dplyr::all_of(id_char), "Bins", "Points")
      colnames(df)[c(2,3)] <- c(varcol, paste(varcol, "Points", sep = "_"))
      return(df)
    }

    else if (is.numeric(data[,varcol]) == TRUE) {
      print("The binned variables should not be numeric")
      stop()
    }
  }
  Fun2 <- function(varcol) {
    Fun1(data, id, target, varcol, score)
  }
  z <- lapply(varcol, Fun2)
  Merged = Reduce(function(x, y) merge(x, y, all.x = T, by = id_char), z)
  Merged2 = merge(data[, c(id_char, tar_char)], Merged, by = id_char, sort = F)
  Merged2$Score = rowSums(Merged2[, seq(4, ncol(Merged2), by = 2)], na.rm = TRUE)
  #might need to add an argument for only one variable situation
  return(Merged2)
}
