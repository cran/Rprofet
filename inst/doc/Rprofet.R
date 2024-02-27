## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

library(dplyr)
library(kableExtra)
library(ROCit)

## ----load package and data, echo=FALSE----------------------------------------
library(Rprofet)
data = load(file="lending_club.rda")
data = lending_club
rm(lending_club)

## ----data-preview, echo=FALSE-------------------------------------------------
#dim(data)
#head(data[1:10])

kableExtra::kbl(head(data[,1:10]), 
                caption = "Head of first 10 columns of lending club data", 
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = c("striped", "scale_down"))

#splitting data
set.seed(112233)
##splitting data
n <- nrow(data)
split <- sample(c(rep(0, 0.8 * n),
                  rep(1, 0.2 * n)))

train <- data[split == 0, ] 
validation <- data[split == 1, ]

tb = data.frame(Dataset=c('Original','Training','Validation'), Rows=0, Columns=0, "TargetRate"=0)
tb[1,2:3] = dim(data)
tb[2,2:3] = dim(train)
tb[3,2:3] = dim(validation)
tb[1,4] = round(table(data$bad)[2]/table(data$bad)[1],3)
tb[2,4] = round(table(train$bad)[2]/table(train$bad)[1],3)
tb[3,4] = round(table(validation$bad)[2]/table(validation$bad)[1],3)

## ----data-set-summary, echo=FALSE---------------------------------------------
#datasets summary
kableExtra::kbl(tb, caption = "Dimension of datasets",
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = "striped")

## ----var-bf-bin, fig.cap = "Original Distribution of Month since recent inquiry", fig.height=3, fig.width=4.5, echo=FALSE, warning=FALSE----
ggplot2::ggplot(data = train,  ggplot2::aes(x = mths_since_recent_inq)) + 
  ggplot2::geom_histogram(binwidth=1, color="white", fill = "#0066CC") +
  ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))+
  ggplot2::scale_x_continuous(breaks = seq(0, 25, 5))

## ----coarse-binning-----------------------------------------------------------
binData = BinProfet(data = train, id = "ID", target = "bad", num.bins = 10,
                        min.pts.bin = 200)

## ----head-of-binned-data, echo=FALSE------------------------------------------
kableExtra::kbl(head(binData[1:10]), 
                caption = "Head of first 10 columns of the binned data", 
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position"))

## ----var-af-bin, fig.cap = "Distribution of binned Months since recent inquiry", fig.height=3, fig.width=4.5, echo=FALSE, warning=FALSE----
ggplot2::ggplot(data = binData,  ggplot2::aes(x = mths_since_recent_inq_Bins)) +
  ggplot2::geom_bar(color="white", fill = "#0066CC", width=0.7) +
  ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

## ----binned-var-ex, echo=FALSE, include=FALSE---------------------------------
kableExtra::kbl(table(binData$mths_since_recent_inq_Bins), 
                col.names = c("Variable Bins", "Frequency"), 
                caption = "Frequency of each bin for annual income",
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = "striped")

## ----WoePlot, fig.cap = "WOE transformed Months since recent inquiry for each bin", fig.height=3.5, fig.width=5, message=FALSE, echo=FALSE----
WOEdata = WOEProfet(data = binData, id = "ID", target = "bad")
data = WOEdata$vars$mths_since_recent_inq
data = data[,c(1,2)]
ggplot2::ggplot(data = data, ggplot2::aes(x = mths_since_recent_inq_Bins, y = mths_since_recent_inq_WOE)) +
  ggplot2::geom_bar(stat = "identity", fill = "#0066CC", width=0.7) +
  ggplot2::geom_text(ggplot2::aes(label = round(mths_since_recent_inq_WOE,3)), vjust = 1.1, size = 3, colour = "black") +
  ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::labs(x = "", y = "WOE")

## ----WOE-transformation, message=FALSE, eval=FALSE----------------------------
#  WOEdata = WOEProfet(data = binData, id = "ID", target = "bad")

## ----eval=FALSE---------------------------------------------------------------
#  head(WOEdata$IV[order(-WOEdata$IV$IV),],10)

## ----WOE-ex, echo=FALSE-------------------------------------------------------
kableExtra::kbl(head(WOEdata$IV[order(-WOEdata$IV$IV),],10), 
                caption = "Top 10 variables with the highest IV",
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = c("striped", "hold_position"))

## ----vars-in-WOEProfet, echo=FALSE--------------------------------------------
kableExtra::kbl(WOEdata$vars$verification_status, 
                caption = "vars output for Verification Status variable",
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = c("striped","hold_position"))

## ----filter-WOE-object-by-IV, message=FALSE-----------------------------------
subWOEdata = Var_select(WOEdata, "ID", "bad", IVfilter = 0.02)

## ----IVfilter-tb, echo=FALSE--------------------------------------------------
kableExtra::kbl(subWOEdata$IV, 
                caption = "Filtered variables with IV greater than 0.02",
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = c("striped","hold_position"))

## ----IVfilter-tb2, echo=FALSE, eval=FALSE-------------------------------------
#  # # table on the left
#  # t1 = subWOEdata$IV[1:15,]
#  # # table on the right
#  # t2 = subWOEdata$IV[16:29,]
#  #
#  # kableExtra::kable(list(t1,t2),
#  #                 caption = "Filtered variables with IV greater than 0.02",
#  #                 booktabs = T,
#  #                 linesep = "") %>%
#  #   kable_styling(latex_options = c("striped","hold_position"))

## ----data-ex, echo=FALSE, fig.cap="WOE transformed dataset A transpose", out.width = '70%', fig.align='center'----
knitr::include_graphics("datawoe.jpg")

## ----K-means-clustering-------------------------------------------------------
set.seed(4172018)
km_cluster = WOEclust_kmeans(subWOEdata, "ID", "bad", num_clusts = 10)

## ----Kmeans, echo=FALSE-------------------------------------------------------
kableExtra::kbl(head(km_cluster,10), 
                caption = "Head of first 10 rows of clustered variables", 
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = c("striped","hold_position"))

## ----extract the top n variables from each cluster, message=FALSE-------------
top2_km <- km_cluster %>% 
  group_by(Group) %>%
  top_n(n = 2)

## ----selectedVars, echo=FALSE-------------------------------------------------
kableExtra::kbl(top2_km, 
                caption = "Variables selected by K-means clustering", 
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = c("striped","hold_position"))

## ----hierarchical clusterng, message=FALSE------------------------------------
h_cluster <- WOEclust_hclust(subWOEdata, 'ID', 'bad', num_clusts = 10) 

top2_h <- h_cluster %>% 
  group_by(Group) %>%
  top_n(n = 2)

## ----selectedVars2, echo=FALSE------------------------------------------------
kableExtra::kbl(top2_h, 
                caption = "Variables selected by hierarchical clustering", 
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = c("striped","hold_position"))

## ----WOEPlotter, fig.cap = "WOEplotter Output", fig.height=3.5, fig.width=6.5----
WOEplotter(binData, 'bad', 'acc_open_past_24mths_Bins')

## ----custom-num, fig.cap = "Customized Binning for Numeric Variable ", fig.height=3.5, fig.width=6.5----
result <- WOE_customNum(train, "acc_open_past_24mths", "ID", 
                             "bad", breaks = c(0,2,3,4,5,6,7,9,Inf), plot = T)

## ----WOEPlotter2, fig.cap = "WOEplotter Output", fig.height=3.5, fig.width=6.5----
WOEplotter(binData, "bad", "home_ownership_Bins")

## ----custom-char, fig.cap = "Customized Binning for Factor Variable ", fig.height=3.5, fig.width=6.5----
WOE = WOE_customFac(train, "home_ownership", "ID", "bad", new_levels = c(1,2,2)
                    ,plot = T)

## ----ex-fine-turning----------------------------------------------------------
#acc_open_past_24mths
#WOEplotter(binData, "bad", 'acc_open_past_24mths_Bins')
acc_open_past_24mths_new <- WOE_customNum(train, "acc_open_past_24mths", 
                                               "ID", "bad", 
                                               breaks = c(0,2,3,4,5,6,7,9,Inf)
                                          )$NewBin
#dti
#WOEplotter(binData, 'bad', "dti_Bins")
dti_new <- WOE_customNum(train, "dti", "ID", 'bad',
                              breaks = c(0,11,16,20,25,30,Inf))$NewBin
#num_tl_op_past_12m
#WOEplotter(binData, "bad", 'num_tl_op_past_12m_Bins')
num_tl_op_past_12m_new <- WOE_customNum(train, "num_tl_op_past_12m", "ID", 
                                             "bad", 
                                             breaks = c(0,1,2,3,4,5,8,Inf),
                                             )$NewBin

## ----fine-tuning-the-selected-variables, echo=FALSE---------------------------
#using the variables selected from K-Means Clustering and fine-tuning if needed
#selectedVars
# first 3 vars are commented out since the chunk above already shows them as examples 
# #acc_open_past_24mths
# #WOEplotter(binData, "bad", 'acc_open_past_24mths_Bins')
# acc_open_past_24mths_new <- WOE_customNum(train, "acc_open_past_24mths", 
#                                                "ID", "bad", 
#                                                breaks = c(0,2,3,4,5,6,7,9,Inf))$NewBin
# #dti
# #WOEplotter(binData, 'bad', "dti_Bins")
# dti_new <- WOE_customNum(train, "dti", "ID", 'bad',
#                               breaks = c(0,11,16,20,25,30,Inf))$NewBin
# #num_tl_op_past_12m
# #WOEplotter_New(binData, "bad", 'num_tl_op_past_12m_Bins')
# num_tl_op_past_12m_new <- WOE_customNum(train, "num_tl_op_past_12m", "ID", 
#                                              "bad", 
#                                              breaks = c(0,1,2,3,4,5,8,Inf),)$NewBin
#inq_last_6mths
#WOEplotter(binData, "bad", 'inq_last_6mths_Bins')
inq_last_6mths_new <- WOE_customNum(train, "inq_last_6mths", "ID", "bad",
                                         breaks = c(0,1,2,3,Inf),)$NewBin
#total_bc_limit
#WOEplotter(binData, target = 'bad', var = "total_bc_limit_Bins")
total_bc_limit_new <- WOE_customNum(train, 'total_bc_limit', "ID", 'bad',
                                         breaks = c(0,3600,7500,11500,16500,
                                                    23900,32400,49000,Inf))$NewBin
#bc_open_to_buy
#WOEplotter(binData, 'bad', 'bc_open_to_buy_Bins')
bc_open_to_buy_new <- WOE_customNum(train, "bc_open_to_buy", "ID", 'bad',
                                         breaks = c(0,330,900,1750,2810,4250,
                                                    6600,9500,14500,25300,Inf))$NewBin
#mths_since_recent_bc
#WOEplotter(binData, target = 'bad', var = "mths_since_recent_bc_Bins")
mths_since_recent_bc_new <- WOE_customNum(train, 'mths_since_recent_bc', 
                                               "ID", 'bad', 
                                               breaks = c(0,4,8,14,19,27,42,84,
                                                          Inf))$NewBin
#mths_since_recent_inq
#WOEplotter(binData, "bad", 'mths_since_recent_inq_Bins')
mths_since_recent_inq_new <- WOE_customNum(train, "mths_since_recent_inq", 
                                                "ID", "bad",
                                                breaks = c(0,1,2,4,7,10,12,15,
                                                           22,Inf))$NewBin
#mo_sin_rcnt_tl
#WOEplotter(binData, target = 'bad', var = "mo_sin_rcnt_tl_Bins")
mo_sin_rcnt_tl_new <- WOE_customNum(train, 'mo_sin_rcnt_tl', "ID", 'bad',
                                         breaks = c(0,1,2,3,5,8,10,14,24,Inf))$NewBin
#tot_hi_cred_lim
#WOEplotter(binData, 'bad', "tot_hi_cred_lim_Bins")
tot_hi_cred_lim_new <- WOE_customNum(train, "tot_hi_cred_lim", "ID", 'bad',
                                          breaks = c(2500,28000,42000,61000,
                                                     93200,132000,220000,
                                                     330000,Inf))$NewBin
#annual_inc
#WOEplotter(binData, "bad", "annual_inc_Bins")
annual_inc_new <- WOE_customNum(train, "annual_inc", "ID", 'bad',
                                     breaks = c(6000,32100,40000,48500,55000,
                                                65100,75000,90000,125000,160000,
                                                Inf))$NewBin
#verification_status
#WOEplotter(binData, "bad", "verification_status_Bins")
verification_status_new <- binData[,c("ID", "verification_status_Bins")]

#emp_length
#WOEplotter(binData, 'bad', "emp_length_Bins")
emp_length_new <- WOE_customFac(train, "emp_length", "ID", "bad",
                                         new_levels = c(1,1,4,1,2,2,2,3,3,3,3,5))$NewBin
  
#bc_util
#WOEplotter(binData, 'bad', "bc_util_Bins")
bc_util_new <- WOE_customNum(train, "bc_util", "ID", 'bad',
                                  breaks = c(0,17,34,55,65,74,89,96,Inf))$NewBin
#percent_bc_gt_75
#WOEplotter(binData, 'bad', "percent_bc_gt_75_Bins")
percent_bc_gt_75_new <- WOE_customNum(train, "percent_bc_gt_75",
                                           "ID", 'bad',
                                           breaks = c(0,2,21,37,55,69,81,Inf))$NewBin
#open_il_12m
#WOEplotter(binData, 'bad', "open_il_12m_Bins")
open_il_12m_new <- WOE_customNum(train, "open_il_12m",
                                             "ID", "bad",
                                             breaks = c(0,1,2,Inf))$NewBin

#mths_since_rcnt_il
#WOEplotter(binData, "bad", "mths_since_rcnt_il_Bins")
mths_since_rcnt_il_new <- WOE_customNum(train, "mths_since_rcnt_il",
                                             "ID", "bad",
                                             breaks = c(1,7,14,30,Inf))$NewBin
# mths_since_rcnt_il_new <- WOE_customNum(train, "mths_since_rcnt_il",
#                                              "ID", "bad",
#                                              breaks = c(1,Inf))$NewBin

## ----create-new-binned-data-with-fine-tuned-selected-variables----------------
#get the id and target variables from the original binned data
binData_id_tar <- data.frame(binData[,c(1,2)])
#extract the selected variable names
vars_name = top2_km$Variable
#rename the selected variable names (we recommend naming the variables in a 
#consistent way, like what we did in the chuck above)
vars = gsub("_WOE","_new",vars_name)
#write a function to get new variable names
Fun2 <- function(vars) {
  get(vars)
}
#obtain a list of lists, each sub-list contains a dataframe with fine-tuned 
#variables and ID variable
var_list = lapply(vars, Fun2)
#left join each sub-list from the list above on ID variable
binData_new <- binData_id_tar %>%
  left_join(var_list %>% purrr::reduce(left_join, by='ID'), by="ID")

## ----recalculate-the-WOE-after-fine-tuning, message=FALSE, results="hide"-----
WOEdata_new = WOEProfet(binData_new, "ID", "bad")

## ----dti, fig.cap = "WOE and Target Rate for each attribute of dti", fig.height=3.1, fig.width=7, echo=FALSE----
data = WOEdata_new$vars$dti
a = ggplot2::ggplot(data = data, ggplot2::aes(x = dti_Bins, y = dti_WOE)) +
  ggplot2::geom_bar(stat = "identity", fill = "#0066CC", width=0.7) +
  ggplot2::geom_text(ggplot2::aes(label = round(dti_WOE,3)), vjust = 1.1, size = 2.8, colour = "black") +
  ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::labs(x = "", y = "WOE")

b = ggplot2::ggplot(data = data, ggplot2::aes(x = dti_Bins, y = TargetRate)) +
  ggplot2::geom_bar(stat = "identity", fill = "#0066CC", width=0.7) +
  ggplot2::geom_text(ggplot2::aes(label = round(TargetRate,3)), vjust = 1.1, size = 2.8, colour = "black") +
  ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::labs(x = "", y = "Target Rate")

c <- gridExtra::grid.arrange(a, b, ncol = 2, nrow = 1)

## ----bc-util, fig.cap = "WOE and Target Rate for each attribute of bc_util ", fig.height=4, fig.width=9, echo=FALSE----
data = WOEdata_new$vars$bc_util
a = ggplot2::ggplot(data = data, ggplot2::aes(x = bc_util_Bins, y = bc_util_WOE)) +
  ggplot2::geom_bar(stat = "identity", fill = "#0066CC", width=0.7) +
  ggplot2::geom_text(ggplot2::aes(label = round(bc_util_WOE,3)), vjust = 1.1, size = 2.8, colour = "black") +
  ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::labs(x = "", y = "WOE")

b = ggplot2::ggplot(data = data, ggplot2::aes(x = bc_util_Bins, y = TargetRate)) +
  ggplot2::geom_bar(stat = "identity", fill = "#0066CC", width=0.7) +
  ggplot2::geom_text(ggplot2::aes(label = round(TargetRate,3)), vjust = 1.1, size = 2.8, colour = "black") +
  ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::labs(x = "", y = "Target Rate")

c <- gridExtra::grid.arrange(a, b, ncol = 2, nrow = 1)

## ----WOE-glm, fig.cap = "Genearl linear models with WOE variables", fig.height=5, fig.width=9, echo=FALSE----
par(mfrow = c(1,2))

acc = setNames(aggregate(WOEdata_new$WOE$bad ~ WOEdata_new$WOE$dti_WOE, FUN = mean),
               c("dti_WOE","bad"))
acc$logit_p = log(acc$bad/(1-acc$bad))
plot(acc$dti_WOE, acc$logit_p,
     xlab="WOE", ylab="log-odds of Target Rate")
md_acc = glm(bad ~ dti_WOE, data = WOEdata_new$WOE, family = "binomial")
abline(md_acc$coefficients, col="red", lwd = 1.5)
#abline(h=md_acc$coefficients[1])

msri = setNames(aggregate(WOEdata_new$WOE$bad ~ WOEdata_new$WOE$bc_util_WOE, FUN = mean),
                c("bc_util_WOE","bad")) #bad rate for each bin's woe
msri$logit_p = log(msri$bad/(1-msri$bad)) #logit(p)
#md_msri = glm(bad ~ bc_util_WOE, data = WOEdata_new$WOE, family = "binomial")
#plot(test$`WOEdata_new$WOE$bc_util_WOE`, test$logit_p)
points(msri$bc_util_WOE, msri$logit_p, pch=2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
legend("bottomright", legend=c("Model with dti_WOE", "Model with bc_util_WOE"),
      pch = c(1,2), cex=0.7)


md_2v = glm(bad ~ dti_WOE+bc_util_WOE,
            data=WOEdata_new$WOE, family = "binomial")
plot(acc$dti_WOE, acc$logit_p, xlab="WOE", ylab="log-odds of Target Rate")
points(msri$bc_util_WOE, msri$logit_p, pch=2)
abline(md_acc$coefficients, col="red", lwd = 1.5)
abline(md_2v$coefficients[1], md_2v$coefficients[2], col="orange2", lwd = 1.5)
abline(md_2v$coefficients[1], md_2v$coefficients[3], col="darkgreen", lwd = 1.5)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
legend("bottomright", title="Model:",
       legend=c("with 1 WOE variable", "with 2 variable: dti_WOE", 
                               "with 2 variable: bc_util_WOE"),
       col=c("red", "orange2", "darkgreen"), lty=1, cex=0.7)


## ----further-variable-selection-using-stepwise-algorithm----------------------
fullModel = glm(bad~., data = WOEdata_new$WOE[,-1], family = "binomial")
stepModel = step(fullModel, direction = "both", trace = F)
summary(stepModel)

## ----corrplot, echo=FALSE, fig.cap = "Correlation plot for highly correlated variables", fig.height=4, fig.width=5----
dat = Var_select(WOEdata_new, "ID", "bad", varcol = -c(5,15,16)) #vars in the reduced model by stepwise selection
df_cor <- dat$WOE[,-c(1,2)] #remove id and target columns
corr <- cor(df_cor)
corr[lower.tri(corr,diag=TRUE)] <- NA  #Prepare to drop duplicates and correlations of 1
corr[corr == 1] <- NA #drop perfect correlations
corr <- as.data.frame(as.table(corr)) #Turn into a 3-column table
corr <- na.omit(corr) #remove the NA values from above
corr <- subset(corr, abs(Freq) > 0.3) #select significant values
corr <- corr[order(-abs(corr$Freq)),] #Sort by highest correlation
#turn corr back into matrix in order to plot with corrplot
mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
#plot correlations visually
corrplot::corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ",
                   tl.cex = 0.7, addCoef.col = 1, number.cex = 0.8, tl.srt = 45)

## ----remove-negative-coefficient----------------------------------------------
selected_var = Var_select(WOEdata_new, "ID", "bad", 
                          varcol = c("acc_open_past_24mths_Bins","num_tl_op_past_12m_Bins",
                                     "mths_since_recent_inq_Bins","bc_open_to_buy_Bins",
                                     "mths_since_recent_bc_Bins","dti_Bins", 
                                     "tot_hi_cred_lim_Bins", "annual_inc_Bins", 
                                     "inq_last_6mths_Bins","verification_status_Bins",
                                     "bc_util_Bins","emp_length_Bins",
                                     "mths_since_rcnt_il_Bins"))

new_Model = glm(bad~., data = selected_var$WOE[,-1], family = "binomial")
summary(new_Model)

## ----create-scorecard, message=FALSE------------------------------------------
scorecard = ScorecardProfet(selected_var, id = 'ID', target = 'bad', 
                                GLModel = new_Model, PDO = 200, BaseOdds = 10,
                                BasePts = 1000, reverse = FALSE)

## ----final-scorecard, echo=FALSE----------------------------------------------
kableExtra::kbl(scorecard, 
                caption = "Final Scorecard", 
                longtable = T,
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = c("striped","repeat_header"))

## ----valid-ex, fig.cap = "\\label{fig:valid_ex}Binning variable in the validation set", fig.height=3, fig.width=6.5----
(names <- unique(scorecard$Attribute))
valid <- validation %>% dplyr::select(ID, bad, all_of(names))

acc_open_past_24mths = WOE_customNum(valid, "acc_open_past_24mths", "ID", "bad", 
                                      breaks = c(0,2,3,4,5,6,7,9,Inf),
                                      right_bracket = F, plot = T)$NewBin

## ----bin-the-validation-data-in-the-same-manner, include=FALSE----------------
#get the predictor names in the final scorecard
(names <- unique(scorecard$Attribute))
#get a subset of validation set with variables on the scorecard
valid <- validation %>% dplyr::select(ID, bad, all_of(names))
#bin each variable in the same way based on scorecard
#simply copy the codes from the fune-tuning chunk and rplace train with valid
#acc_open_past_24mths
acc_open_past_24mths <- WOE_customNum(valid, "acc_open_past_24mths", 
                                               "ID", "bad", 
                                               breaks = c(0,2,3,4,5,6,7,9,Inf)
                                      )$NewBin
#dti
dti <- WOE_customNum(valid, "dti", "ID", 'bad',
                              breaks = c(0,11,16,20,25,30,Inf))$NewBin
#num_tl_op_past_12m
num_tl_op_past_12m <- WOE_customNum(valid, "num_tl_op_past_12m", "ID", 
                                             "bad", 
                                             breaks = c(0,1,2,3,4,5,8,Inf)
                                    )$NewBin
#inq_last_6mths
inq_last_6mths <- WOE_customNum(valid, "inq_last_6mths", "ID", "bad",
                                         breaks = c(0,1,2,3,Inf))$NewBin
#bc_open_to_buy
bc_open_to_buy <- WOE_customNum(valid, "bc_open_to_buy", "ID", 'bad',
                                         breaks = c(0,330,900,1750,2810,4250,
                                                    6600,9500,14500,25300,Inf)
                                )$NewBin
#mths_since_recent_bc
mths_since_recent_bc <- WOE_customNum(valid, 'mths_since_recent_bc', 
                                               "ID", 'bad', 
                                               breaks = c(0,4,8,14,19,27,42,84,
                                                          Inf))$NewBin
#mths_since_recent_inq
mths_since_recent_inq <- WOE_customNum(valid, "mths_since_recent_inq", 
                                                "ID", "bad",
                                                breaks = c(0,1,2,4,7,10,12,15,
                                                           22,Inf))$NewBin
#tot_hi_cred_lim
tot_hi_cred_lim <- WOE_customNum(valid, "tot_hi_cred_lim", "ID", 'bad',
                                          breaks = c(2500,28000,42000,61000,
                                                     93200,132000,220000,
                                                     330000,Inf))$NewBin
#annual_inc
annual_inc <- WOE_customNum(valid, "annual_inc", "ID", 'bad',
                                     breaks = c(6000,32100,40000,48500,55000,
                                                65100,75000,90000,125000,160000,
                                                Inf))$NewBin
#verification_status -- this variable stays the same
verification_status <- valid[,c("ID", "verification_status")]

#emp_length
emp_length <- WOE_customFac(valid, "emp_length", "ID", "bad",
                                         new_levels = c(1,1,4,1,2,2,2,3,3,3,3,5)
                            )$NewBin
  
#bc_util
bc_util <- WOE_customNum(valid, "bc_util", "ID", 'bad',
                                  breaks = c(0,17,34,55,65,74,89,96,Inf))$NewBin

#mths_since_rcnt_il
mths_since_rcnt_il <- WOE_customNum(valid, "mths_since_rcnt_il",
                                             "ID", "bad",
                                             breaks = c(1,7,14,30,Inf))$NewBin

#obtain a list of lists, each sub-list contains a dataframe with fine-tuned 
#variables and ID variable
test = lapply(names, Fun2)
#left join each sub-list from the list above on ID variable
binData_valid <- valid[,c("ID","bad")] %>%
  left_join(test %>% purrr::reduce(left_join, by='ID'), by="ID")

## ----binning-validation-ex----------------------------------------------------
colnames(binData_valid) <- gsub("_Bins","", colnames(binData_valid))
valid_score <- ScoreDataProfet(binData_valid, scorecard, "ID", "bad")

## ----scored-data, echo=FALSE--------------------------------------------------
vs = head(valid_score,4)
kableExtra::kbl(vs[,c(1:7)], 
                caption = "Head of the scored validation data", 
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position"))

kableExtra::kbl(vs[,c(8:15)], 
                #caption = "Head of the scored validation data", 
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position"))

kableExtra::kbl(vs[,c(16:22)], 
                #caption = "Head of the scored validation data", 
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position"))

kableExtra::kbl(vs[,c(23:29)], 
                #caption = "Head of the scored validation data", 
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position"))

## ----print-wide-table, results="asis", echo=FALSE, eval=FALSE-----------------
#  #this doesn't look better than simply using head()
#  test = head(valid_score,5)
#  m <- matrix(1:ncol(test), 5)
#  
#  for (i in 1:ncol(m)) {
#   cat(kable(test[, m[, i]], 'latex', booktabs=TRUE), "\\newline")
#  }

## ----gains-tb-train, echo=FALSE-----------------------------------------------
#ROCit
#training set gains table
colnames(selected_var$Bin) <- gsub("_Bins","", colnames(selected_var$Bin))
train_score = ScoreDataProfet(selected_var$Bin, scorecard, "ID", "bad")
gain_train <- gainstable(score = -train_score$Score,
                       class = train_score$bad,
                       negref = 0,
                       ngroup = 10)

#validation set gains table
gain_valid <- gainstable(score = -valid_score$Score, 
                         class = valid_score$bad,
                         negref = 0, 
                         ngroup = 10)
# gain_train
# gain_valid


## 1.Calculate the Average Points
##-----------------------------##-----------------------------##
## Training Set
os = sort(train_score$Score, decreasing = T)
obs = length(os)/10
b = seq(obs, length(os), obs)
n = ntile(desc(os),10)
#table(n)
tb = data.frame(os, n)
avg_pt_t = tb %>% group_by(n) %>% summarise(Avg_point = mean(os))
#avg_pt_t
#-------------------------------------------------------------------#
## Validation Set
os = sort(valid_score$Score, decreasing = T)
obs = length(os)/10
b = seq(obs, length(os), obs)
n = ntile(desc(os),10)
#table(n)
tb = data.frame(os, n)
avg_pt_v = tb %>% group_by(n) %>% summarise(Avg_point = mean(os))
#avg_pt_v

##-----------------------------##-----------------------------##
## 2.Add the Avg Points column to the gains tables
#gain_train
train_gtb = as.data.frame(matrix(unlist(gain_train), nrow=length(unlist(gain_train[1]))))
nm = names(gain_train)
names(train_gtb) = nm
train_gtb$`Avg Score` = round(avg_pt_t$Avg_point, 0)
kableExtra::kbl(train_gtb,
                caption = "Gains table for the training data",
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position"))

## ----gains-tb-test, echo=FALSE------------------------------------------------
#gain_valid
valid_gtb = as.data.frame(matrix(unlist(gain_valid), nrow=length(unlist(gain_valid[1]))))
nm = names(gain_valid)
names(valid_gtb) = nm
valid_gtb$`Avg Score` = round(avg_pt_v$Avg_point, 0)
kableExtra::kbl(valid_gtb, 
                caption = "Gains table for the validation data", 
                booktabs = T,
                linesep = "") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position"))

## ----gains, fig.cap = "Gains Charts", echo=FALSE, fig.height=4, fig.width=9----
par(mfrow = c(1, 2))
train.depth = c(0, (gain_train$Bucket)*10)
train.gaincurve = c(0,(gain_train$CCapRate)*100)
plot(train.depth,train.gaincurve,col='cyan3',pch=16,xlim=c(0,100),ylim=c(0,100), cex.main=0.8,
     main='Gains Chart for Training Set', xlab='Percentile',ylab='% of Total Response')
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
lines(train.depth,train.gaincurve,col='cyan3',lty=1)
axis(1,at=c(10,30,50,70,90))
points(train.depth,train.depth,col='red',pch=16)
lines(train.depth,train.depth,col='red',lty=1)
legend('topleft', legend=c('Logistic Model','Baseline'),
       col=c('cyan3','red'), pch=16,lty=1, cex=0.7)


valid.depth = c(0, (gain_valid$Bucket)*10)
valid.gaincurve = c(0,(gain_valid$CCapRate)*100)
plot(valid.depth,valid.gaincurve,col='cyan3',pch=16,xlim=c(0,100),ylim=c(0,100), cex.main=0.8,
     main='Gains Chart for Validation Set', xlab='Percentile',ylab='% of Total Response')
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
lines(valid.depth,valid.gaincurve,col='cyan3',lty=1)
axis(1,at=c(10,30,50,70,90))
points(valid.depth,valid.depth,col='red',pch=16)
lines(valid.depth,valid.depth,col='red',lty=1)
legend('topleft', legend=c('Logistic Model','Baseline'),
       col=c('cyan3','red'), pch=16,lty=1, cex=0.7)

## ----confusion-m, echo=FALSE, fig.cap="Confusion Matrix", out.width = '40%', fig.align='center'----
knitr::include_graphics("confusion_m.jpg")

## ----model-roc, fig.cap = "ROC Curves for Training and Validation Sets", message=FALSE, fig.height=4, fig.width=9, echo=FALSE----
par(mfrow = c(1, 2))
#-----------------ROCit-------------------#
# roc_train <- rocit(score = step_result$step_pred, class = step_result$bad,
#                        negref = 0)
# #plot(roc_train$FPR, roc_train$TPR, type='l')
# plot(roc_train, YIndex = F,  values = F, col = c(2,4))
# roc_train$AUC

# gainstable(roc_train, breaks = seq(10,100,10))

roc_train <- rocit(score = -train_score$Score,
                   class = train_score$bad,
                   negref = 0)
#plot(roc_train, YIndex = F,  values = F, col = c(2,4))
#roc_train$AUC

#Validation Set
roc_valid <- rocit(score = -valid_score$Score, class = valid_score$bad,
                       negref = 0) 
#plot(roc_valid, YIndex = F,  values = F, col = c(2,4))
#roc_valid$AUC

#create my own ROC plots
plot(roc_train$FPR, roc_train$TPR, type = 'l', col='red', lwd=2,
     xlab = '1-Specificity (FPR)', ylab = 'Sensitivity (TPR)',
     main = 'ROC Curve for the Training Set', cex.main=0.8)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
abline(0,1,lty=2, col='deepskyblue')
legend('bottomright', legend=c('Empirical ROC curve', 'Baseline'),
       col=c('red','deepskyblue'), pch=16,lty=1,cex=0.7)

plot(roc_valid$FPR, roc_valid$TPR, type = 'l', col='red', lwd=2,
     xlab = '1-Specificity (FPR)', ylab = 'Sensitivity (TPR)',
     main = 'ROC Curve for the Validation Set', cex.main=0.8,)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
abline(0,1,lty=2, col='deepskyblue')
legend('bottomright', legend=c('Empirical ROC curve', 'Baseline'),
       col=c('red','deepskyblue'), pch=16,lty=1,cex=0.7)

## ----roc, fig.cap = "ROC Curves", fig.height=4, fig.width=5, echo=FALSE-------
plot(roc_train$FPR, roc_train$TPR, type='l', main="ROC Curves Comparison",
     xlab="FPR", ylab="TPR", lwd = 2, cex.main=0.8)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
segments(x0 = 0, y0 = 0, x1 = 0, y1 = 1, col = "darkgreen", lwd = 2)
segments(x0 = 0, y0 = 1, x1 = 1, y1 = 1, col = "darkgreen", lwd = 2)
segments(x0 = 0, y0 = 0, x1 = 1, y1 = 1, col = "red", lwd = 2)
#plot(roc_valid$FPR, roc_valid$TPR, type='l', add=T)
lines(roc_valid$FPR, roc_valid$TPR, col='orange', lwd = 2)
legend("bottomright", legend=c("perfect","random guess","training model",
                               "validation model"),
       col=c("darkgreen","red","black","orange"),
       lty=1, cex=0.7, lwd = 2)

## ----KS, fig.cap = "KS Plots", echo=FALSE, fig.height=4, fig.width=9----------
#ROCit---KS plots and KS statistics
#ks_train = ksplot(roc_train)
#ks_train$`KS stat`
#ks_valid = ksplot(roc_valid)
#ks_valid$`KS stat`

#make my own KS plots
par(mfrow = c(1, 2))

measure.t <- measureit(score = -train_score$Score, class = train_score$bad, negref = 0,
                     measure = c("ACC", "SENS", "TPR", "FPR"))
depth <- measure.t$Depth
plot(depth, measure.t$TPR, type='l', xlab='Percentile', ylab='',
     main='Training set K-S plot', col='darkorange2', lwd = 2, cex.main=0.8)
lines(depth, measure.t$FPR, col='navy', lwd = 2)
#which.max(abs(measure.t$TPR-measure.t$FPR)) #486
segments(x0 = depth[486], y0 = measure.t$FPR[486], 
         x1 = depth[486], y1 = measure.t$TPR[486], col = "deeppink", lwd = 2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
legend('bottomright', legend=c('True Positive Rate','False Positive Rate', 'KS Stat'),
       col=c('darkorange2','navy', 'deeppink'), pch=16,lty=1, cex=0.7, lwd = 2) 
ks_train = max(abs(measure.t$TPR-measure.t$FPR)) #KS statistics

measure.v <- measureit(score = -valid_score$Score, class = valid_score$bad, negref = 0,
                       measure = c("ACC", "SENS", "TPR", "FPR"))
depth <- measure.v$Depth
plot(depth, measure.v$TPR, type='l', xlab='Percentile', ylab='',
     main='Validation set K-S plot', col='darkorange2', lwd = 2, cex.main=0.8)
lines(depth, measure.v$FPR, col='navy', lwd = 2)
#which.max(abs(measure.v$TPR-measure.v$FPR)) #452
segments(x0 = depth[452], y0 = measure.v$FPR[452], 
         x1 = depth[452], y1 = measure.v$TPR[452], col = "deeppink", lwd = 2)
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 1)
legend('bottomright', legend=c('True Positive Rate','False Positive Rate', 'KS Stat'),
       col=c('darkorange2','navy', 'deeppink'), pch=16,lty=1, cex=0.7, lwd = 2) 
ks_valid = max(abs(measure.v$TPR-measure.v$FPR))#KS statistics

## ----AUC-KS-tb, echo=FALSE----------------------------------------------------
tb = data.frame(Data=c('Training Data','Validation Data'), AUC=0, KS=0)
tb[1,2] = round(roc_train$AUC, 3)
tb[2,2] = round(roc_valid$AUC, 3)
tb[1,3] = round(ks_train, 3)
tb[2,3] = round(ks_valid, 3)
kableExtra::kbl(tb, caption = "Fit statistics table",
                booktabs = T,
                linesep = "")

