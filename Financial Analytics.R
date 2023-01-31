library(gmodels)
library(vcd)
library(smbinning)
library(dplyr)
library(stringr)
library(shades)
library(latticeExtra)
library(plotly)

# Creating good column for smbinning
accepts = read.csv('/Users/lhuynh/Documents/NCSU-IAA/Statistics & Data/Financial Analytics 1/Financial Analytics Data/accepts.csv')
accepts$good = abs(accepts$bad - 1)
table(accepts$good, useNA = "always")

# Convert numeric factor variables
accepts$bankruptcy = as.factor(accepts$bankruptcy)
accepts$used_ind = as.factor(accepts$used_ind)
accepts$purpose = as.factor(accepts$purpose)

# Split into train and test
set.seed(12345)
train_id = sample(seq_len(nrow(accepts)), size = floor(0.75*nrow(accepts)))

train = accepts[train_id,]
test = accepts[-train_id,]

# Bin bureau_score
result = smbinning(df=train,y="good",x="bureau_score")
result$ivtable
result$cuts

# Bar plot of bureau_score smbinning metrics
smbinning.plot(result,option = "dist",sub = "Bureau Score")
smbinning.plot(result,option = "goodrate",sub = "Bureau Score")
smbinning.plot(result,option = "badrate",sub = "Bureau Score")

# Weight of Evidence plot
woe.plot = smbinning.plot(result,option = "WoE",sub = "Bureau Score")

# WoE values for factor variables w/out needing to rebin
result1 = smbinning.factor(df = train, y = "good", x = "purpose")
result1$ivtable

# Information value summary
iv_summary = smbinning.sumiv(df = train, y = "good")
iv_summary
smbinning.sumiv.plot(iv_summary)

# Binning of Continuous Variables - IV >= 0.1 #
num_names <- names(train)[sapply(train, is.numeric)] # Gathering the names of numeric variables in data #
names1 = c("app_id","tot_open_tr","tot_rev_tr","loan_amt","tot_income","bad","weight")
names2 = c("tot_derog","tot_tr","age_oldest_tr","tot_rev_debt","tot_rev_line","rev_util",     
           "bureau_score","purch_price","msrp","down_pyt","ltv","loan_term")
result_all_sig <- list() # Creating empty list to store all results #

for(i in 1:length(num_names)){
  check_res <- smbinning(df = train, y = "good", x = num_names[i])
  
  if(check_res[1] == "Uniques values < 5") {
    next
  }
  else if(check_res[1] == "No significant splits") {
    next
  }
  else if(check_res$iv < 0.1) {
    next
  }
  else {
    result_all_sig[[num_names[i]]] <- check_res
  }
}
