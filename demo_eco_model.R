# Essential packages
library(foreign)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
#library(truncnorm)
options(scipen = 999) # Avoid scientific notation
set.seed(229)
options(warn = 2) # Turn to 0 to continue with warnings

setwd("U:/THESIS SCRIPTING/R/")

#####
# Useful functions
#####
# Get rid of outliers
no_outliers <- function(x) {
  x <- na.omit(x)
  Q <- quantile(x, probs=c(.25, .75), na.rm = FALSE)
  iqr <- IQR(x)
  up <-  Q[2]+1.5*iqr # Upper Range  
  low<- Q[1]-1.5*iqr # Lower Range
  
  eliminated <- subset(x, x > (Q[1] - 1.5*iqr) & x < (Q[2]+1.5*iqr))
  
  return (eliminated)
}

# Graph original data and projections (by SEL)
plt_my <- function(x, years, variab, tlines){
  # Plot empty graphs
  line_types <- c(1, 2, 3)
  cols <- c('green', 'yellow', 'red')
  
  # Prepare y label
  my_ylab <- paste("Avg. annual", variab, sep = " ")
  
  # If actual data
  if (tlines == T){
    # establish Y limit
    myYlim <- range(x[[variab]])
    
    # Empty plot
    plot(1, type="b", xlab="Years", ylab=my_ylab, 
         xlim=years, ylim=myYlim)
    
    # Add Lines
    for (i in 1:length(SELs)) {
      # Data lines
      lines(x[x[["sel"]] == SELs[i],][["year"]], 
            x[x[["sel"]] == SELs[i],][[variab]], 
            pch = 1, col = "black", type = "b", lty = line_types[i])
      
      # Trend Lines
      lines(x[x[["sel"]] == SELs[i],][["year"]],
            predict(lm(x[x[["sel"]] == SELs[i],][[variab]] ~ 
                         x[x[["sel"]] == SELs[i],][["year"]])),
            col = cols[i])
    }
    
    # Add a legend
    legend("topright", legend=c("SEL B", "SEL C", "SEL D"),
           col="black", lty = 1:3, cex=0.8)
  }
  
  # If on projected data
  else{
    # Stablish y limits
    myYlim <- range(c(max(x[-1]), min(x[-1])))
    
    # Plot empty graph
    plot(1, type="b", xlab="Years", ylab=my_ylab, 
         xlim=years, ylim=myYlim)
    
    # Add lines
    for (i in 1:length(SELs)) {
      # Data lines
      lines(x[["year"]], 
            x[[SELs[i]]], 
            pch = 1, col = "black", type = "b", lty = line_types[i]) #1
    }
    
    # Add a legend
    legend("bottomright", legend=c("SEL B", "SEL C", "SEL D"),
           col="black", lty = 1:3, cex=0.8)
    
  }
  
}

# Project trend lines
proj_trend <- function(x, years){
  pred <- data.frame(year=years)
  
  v1 <- x
  v2 <- "year"
  
  fm <- as.formula(paste(v1, "~", v2, sep=""))
  
  for (i in 1:length(SELs)) {
    pred[[SELs[i]]] <- predict(lm(fm,
                                  data=all_means[all_means$sel == SELs[i],]),
                               newdata=pred)
    
    # Here 5 is the number of years
    pred[[SELs[i]]][1:5] <- all_means[all_means$sel == SELs[i],][[x]]
  }
  
  return(pred)
}

# define function to get intervals
int_extr <- function(x, n_cats){
  out <- sapply(str_extract_all(levels(cut(x, n_cats)), 
                                "-?[0-9]+(\\.[0-9]+)?"), function(y) as.numeric(y))
  int_nums <- c(out[1,1], out[2,])
  int_nums[1] <- -Inf
  int_nums[length(int_nums)] <- Inf
  
  return(int_nums)
}

# define function to get mean of each interval
mean_int <- function(z, n_cats){
  out <-  sapply(str_extract_all(levels(cut(z, n_cats)), 
                                 "-?[0-9]+(\\.[0-9]+)?"), 
                 function(x) mean(as.numeric(x)))
  return(out)
}

# Calculate TPMs (by SEL)
get_tpm <- function(in_df, var_nam, t0_t1){
  varname1 <- paste(var_nam, t0_t1[1], sep = "_")
  varname2 <- paste(var_nam, t0_t1[2], sep = "_")
  
  combo_df <- as.data.frame(table(in_df[[varname1]], in_df[[varname2]]))
  names(combo_df) <- c("state0", "state1", "combo_cnt")
  solo_df <- as.data.frame(table(in_df[[varname1]]))
  names(solo_df) <- c("state0", "cnt")
  
  combo_df <- merge(combo_df, solo_df)
  combo_df$probs <- combo_df$combo_cnt / combo_df$cnt
  
  return(combo_df)
}

#####
# GET PROBS FOR HH LEVEL TABLE
#####
# Import non panel filtered data
filen <- list("INPUT DATA/FILTERED/NON-PANEL/SUMARIA_2015.Rds", 
              "INPUT DATA/FILTERED/NON-PANEL/SUMARIA_2016.Rds",
              "INPUT DATA/FILTERED/NON-PANEL/SUMARIA_2017.Rds", 
              "INPUT DATA/FILTERED/NON-PANEL/SUMARIA_2018.Rds",
              "INPUT DATA/FILTERED/NON-PANEL/SUMARIA_2019.Rds")
filt <- lapply(filen, readRDS)

# Import panel filtered data
hhs <- readRDS(file="INPUT DATA/FILTERED/PANEL/sumaria-2015-2019-panel.Rds")

# (For non-panel data:)
# (divide both among the respective means for SEL)
SELs <- c("B", "C", "D")
com_names <- c("income_pg", "exp_pm", "sel", "year")

all_means <- as.data.frame(matrix(nrow = 0, ncol = length(com_names)))
names(all_means) <- com_names


for (i in 1:length(SELs)) {
  crrt_means <- as.data.frame(matrix(nrow = length(filt), ncol = length(com_names)))
  names(crrt_means) <- com_names
  
  inc_SEL <- c()
  exp_SEL <- c()
  
  for (j in 1:length(filt)) {
    inc_m <- mean(no_outliers(na.omit(filt[[j]][filt[[j]]$ESTRSOCIAL == SELs[i],]$INCOME_PG)))
    exp_m <- mean(no_outliers(na.omit(filt[[j]][filt[[j]]$ESTRSOCIAL == SELs[i],]$EXP_PM)))
    
    inc_SEL <- c(inc_SEL, inc_m)
    exp_SEL <- c(exp_SEL, exp_m)
  }
  
  crrt_means$income_pg <- inc_SEL
  crrt_means$exp_pm <- exp_SEL
  crrt_means$sel <- SELs[i]
  crrt_means$year <- c(2015:2019)
  
  all_means <- rbind(all_means, crrt_means)
}

# Predict (trend projection)
pred_inc <- proj_trend("income_pg", c(2015:2040))
pred_exp <- proj_trend("exp_pm", c(2015:2040))

# (create luckiness categories for all years)
# Create empty list to append vectors
rinc <- list()
rexp <- list ()

# Create variables for each dataframe
for (i in 1:length(filt)) {
  filt[[i]]$REL_INC <- NA # Create vector within DF
  filt[[i]]$REL_EXP <- NA # Create vector within DF
}

for (j in 1:length(SELs)) {
  # Create empty vectors to collect data by variable of interest
  all_rinc_sel <- c()
  all_rexp_sel <- c()
  
  # Iterate through each DF within list
  for (i in 1:length(filt)) {
    out_rinc <- filt[[i]][filt[[i]]$ESTRSOCIAL == SELs[j],]$INCOME_PG / 
      all_means[all_means$sel == SELs[j],]$income_pg[i] # Relative savings
    
    filt[[i]][filt[[i]]$ESTRSOCIAL == SELs[j],]$REL_INC <- out_rinc # Assign to DF
    all_rinc_sel <- c(all_rinc_sel, out_rinc) # Assign to vector
    
    out_rexp <- filt[[i]][filt[[i]]$ESTRSOCIAL == SELs[j],]$EXP_PM / 
      all_means[all_means$sel == SELs[j],]$exp_pm[i] # Relative number of HH members
    
    filt[[i]][filt[[i]]$ESTRSOCIAL == SELs[j],]$REL_EXP <- out_rexp # Assign to DF
    all_rexp_sel <- c(all_rexp_sel, out_rexp) # Assign to vector
  }
  # Append vector to external list
  rinc[[j]] <- all_rinc_sel
  rexp[[j]] <- all_rexp_sel
}

# Group all relative columns by SEL to take out outliers
out_inc <- lapply(rinc, no_outliers)
out_exp <- lapply(rexp, no_outliers)

# Create breaks from there
tot_cats <- 5

cats_inc <- lapply(out_inc, int_extr, tot_cats)
cats_exp <- lapply(out_exp, int_extr, tot_cats)

# Create average for each interval
intvm_inc <- lapply(out_inc, mean_int, tot_cats)
intvm_exp <- lapply(out_exp, mean_int, tot_cats)

# Apply intervals categorization to non-panel data
for (j in 1:length(filt)){
  # Categorize HHM
  cts_mph <- c(-Inf, 1, 2, 3, 4, 5, 6, Inf)
  filt[[j]]$MIEPERHO_C <- cut(filt[[j]]$MIEPERHO, cts_mph, 1:7) # 7 or more becomes 7
  
  # Income and expenses
  filt[[j]]$REL_INC_C <- NA
  filt[[j]]$REL_EXP_C <- NA
  
  for (i in seq(SELs)){
    # Categorize relative income
    filt[[j]][filt[[j]]$ESTRSOCIAL == SELs[i],]$REL_INC_C <- 
      cut(filt[[j]][filt[[j]]$ESTRSOCIAL == SELs[i],]$REL_INC,
          cats_inc[[i]], labels = (1:5))
    
    # Categorize relative expenses
    filt[[j]][filt[[j]]$ESTRSOCIAL == SELs[i],]$REL_EXP_C <- 
      cut(filt[[j]][filt[[j]]$ESTRSOCIAL == SELs[i],]$REL_EXP,
          cats_exp[[i]], labels = (1:5))
  }
  # Turn to numbers to categorical
  filt[[j]]$REL_INC_C <- as.factor(filt[[j]]$REL_INC_C)
  filt[[j]]$REL_EXP_C <- as.factor(filt[[j]]$REL_EXP_C)
}

# GET NUMBER OF CASES PER INCOME AND EXPENSES CATEGORY
as.data.frame(table(filt[[1]]$ESTRSOCIAL, filt[[1]]$REL_INC_C))
as.data.frame(table(filt[[1]]$ESTRSOCIAL, filt[[1]]$REL_EXP_C))

# (calculate distribution probabilities of inc/hhm categories for 2015)
synt_or <- filt[[1]] # Only 2015

prob_inc <- as.data.frame(matrix(nrow = tot_cats, ncol = length(SELs)))
names(prob_inc) <- SELs

prob_hmem <- as.data.frame(matrix(nrow = 7, ncol = length(SELs))) # Up to 7 HHm
names(prob_hmem) <- SELs

# For nested expenses categories
inc_c <- levels(synt_or$REL_EXP_C)
nmas <- c("inc_cat", "exp_cat", "sel", "probs")
exp_pr <- as.data.frame(matrix(ncol = length(nmas), nrow = 0))
names(exp_pr) <- nmas

for (i in seq_along(SELs)) {
  # Income
  this_probs <- as.data.frame(table(synt_or[synt_or$ESTRSOCIAL == SELs[i],]$REL_INC_C)/
                                sum(table(synt_or[synt_or$ESTRSOCIAL == SELs[i],]$REL_INC_C)))
  prob_inc[[SELs[i]]] <- this_probs$Freq
  
  # HH members
  hhm_probs <- as.data.frame(table(synt_or[synt_or$ESTRSOCIAL == SELs[i],]$MIEPERHO_C)/
                               sum(table(synt_or[synt_or$ESTRSOCIAL == SELs[i],]$MIEPERHO_C)))
  prob_hmem[[SELs[i]]] <- hhm_probs$Freq
  
  # Expenses
  for (j in seq(inc_c)){
    exp_df <- as.data.frame(matrix(ncol = length(nmas), nrow = length(levels(synt_or$REL_EXP_C))))
    names(exp_df) <- nmas
    
    exp_probs <- as.data.frame(table(synt_or[synt_or$ESTRSOCIAL == SELs[i] &
                                               synt_or$REL_INC_C == inc_c[j],]$REL_EXP_C)/
                                sum(table(synt_or[synt_or$ESTRSOCIAL == SELs[i] &
                                                    synt_or$REL_INC_C == inc_c[j],]$REL_EXP_C)))
    
    exp_df$exp_cat <- exp_probs$Var1
    exp_df$probs <- exp_probs$Freq
    exp_df$inc_cat <- inc_c[j]
    exp_df$sel <- SELs[i]
    
    exp_pr <- rbind(exp_pr, exp_df)
  }
}

# (For panel data:)
# (Apply luckiness categories limits by SEL (inc/exp) to every year)
hhs$REL_INC_15 <- NA

hhs$REL_INC_16 <- NA

for (i in seq(SELs)){
  hhs[hhs$estrsocial_15 == SELs[i],]$REL_INC_15 <- 
    hhs[hhs$estrsocial_15 == SELs[i],]$INCOME_PG_15 / all_means[all_means$sel == SELs[i],]$income_pg[1]
  
  hhs[hhs$estrsocial_16 == SELs[i],]$REL_INC_16 <- 
    hhs[hhs$estrsocial_16 == SELs[i],]$INCOME_PG_16 / all_means[all_means$sel == SELs[i],]$income_pg[2]
}

# Cut based on the limits by SEL (from non-panel datasets)
hhs$REL_INC_C_15 <- NA

hhs$REL_INC_C_16 <- NA

for (i in seq(SELs)){
  # Categorize relative savings
  hhs[hhs$estrsocial_15 == SELs[i],]$REL_INC_C_15 <- cut(hhs[hhs$estrsocial_15 == SELs[i],]$REL_INC_15,
                                                         cats_inc[[i]], labels = (1:5))
  hhs[hhs$estrsocial_16 == SELs[i],]$REL_INC_C_16 <- cut(hhs[hhs$estrsocial_16 == SELs[i],]$REL_INC_16,
                                                         cats_inc[[i]], labels = (1:5))
}

# Transform to levels
hhs$REL_INC_C_15 <- as.factor(hhs$REL_INC_C_15)

hhs$REL_INC_C_16 <- as.factor(hhs$REL_INC_C_16)

# (calculate transition probabilities among categories based on 15-16)
inc_tpms <- list()

for (i in seq(SELs)){
  inc_tpm <- get_tpm(hhs[hhs$estrsocial_15 == SELs[i],], "REL_INC_C", c(15, 16))
  inc_tpms[[i]] <- inc_tpm
}

# Save probabbilities as csv
# Save cuts for income
imc_df <- as.data.frame(matrix(ncol = 2, nrow = 0))
names(imc_df) <- c("cuts", "SEL")

for (it in seq(1:length(cats_exp))){
  abd_df <- as.data.frame(matrix(ncol = 2, nrow = 6))
  names(abd_df) <- c("cuts", "SEL")
  abd_df$cuts <- cats_exp[[it]]
  abd_df$SEL <- it
  imc_df <- rbind(imc_df, abd_df)
}

write.csv(imc_df, "INPUT DATA/FILTERED/PROBS/INDV/inc_cuts.csv", 
          row.names=FALSE)

# Save cuts for expenses
imd_df <- as.data.frame(matrix(ncol = 2, nrow = 0))
names(imd_df) <- c("cuts", "SEL")

for (it in seq(1:length(cats_exp))){
  abd_df <- as.data.frame(matrix(ncol = 2, nrow = 6))
  names(abd_df) <- c("cuts", "SEL")
  abd_df$cuts <- cats_exp[[it]]
  abd_df$SEL <- it
  imd_df <- rbind(imd_df, abd_df)
}

write.csv(imd_df, "INPUT DATA/FILTERED/PROBS/INDV/exp_cuts.csv", 
          row.names=FALSE)

# Dist income categories
write.csv(prob_inc, "INPUT DATA/FILTERED/PROBS/INDV/INC_CAT_PROB.csv", 
          row.names=FALSE)

# Dist household members
write.csv(prob_hmem, "INPUT DATA/FILTERED/PROBS/INDV/HH_MEM_PROBS.csv", 
          row.names=FALSE)

# Distribution probs for expenses by income category
write.csv(exp_pr, "INPUT DATA/FILTERED/PROBS/INDV/EXP_CAT_PROB.csv", 
          row.names=FALSE)

# Transition probs for income classes
for (it in seq(1:length(inc_tpms))){
  inc_tpms[[it]]$SEL <- it
}

full_tpmic <- bind_rows(inc_tpms)

write.csv(full_tpmic, "INPUT DATA/FILTERED/PROBS/INDV/INC_TRS_PROBS.csv", 
          row.names=FALSE)

# Summary
# all_means: means of income and expenses per generator and member per SEL per year
# pred_inc: projection of the means of income/generators by sel unitl 2040
# pred_exp: projection of the means of expenses/members by sel unitl 2040
# cats_inc: Luckiness categories breaks income
# cats_exp: Luckiness categories breaks expenses
# intvm_inc: Interval means for income
# intvm_exp: Interval means for expenses
# prob_inc: Distribution probabilities for income categories by SEL (from 2015 data)
# prob_hmem: Distribution probabilities for household members by SEL (from 2015 data)
# exp_pr: Distribution probs for expenses by income category
# inc_tpms: list of transition probabilities data frames for income classes by SEL

#####
# GET PROBS FOR INDIVIDUAL LEVEL DATA
#####
# Import filtered module 500 non panel
ind_dta <- readRDS(file="INPUT DATA/FILTERED/NON-PANEL/Enaho01A-2015-500.Rds")

# Import filtered module 200 non panel
genage_dta <- readRDS("INPUT DATA/FILTERED/NON-PANEL/Enaho01-2015-200.Rds")

# Import module 500 filtered panel data
igen_dta <- readRDS("INPUT DATA/FILTERED/PANEL/enaho01-2015-2019-500-panel_01.Rds")

# Import module 200 filtered panel data
my_indv <- readRDS("INPUT DATA/FILTERED/PANEL/enaho01-2015-2019-200-panel.Rds")

# (import birth dist probs)
birth_dta <- read.csv("INPUT DATA/FILTERED/NOT ENAHO/ENDES/projected_children_agecat.csv")
# adapt birth distribution probs
birth_dta$Num_child <- ifelse(birth_dta$Num_child == "5+", "5", birth_dta$Num_child)
birth_dta$Num_child <- as.numeric(birth_dta$Num_child)
birth_dta$Age_cat_w <- as.numeric(as.factor(birth_dta$Age_cat))
birth_dta$Age_cat_z <- birth_dta$Age_cat_w + 3
thisy_brths <- birth_dta[birth_dta$Year == 2015,]


# (import transition births)
rel_birth <- read.csv("INPUT DATA/FILTERED/NOT ENAHO/ENDES/add_children_prbs.csv")
# adapt rel birth trans probs
rel_birth$Num_child <- ifelse(rel_birth$Num_child == "5+", 
                              "5", rel_birth$Num_child)
rel_birth$Age_cat_w <- as.numeric(as.factor(rel_birth$Age_cat))
rel_birth$Age_cat_z <- rel_birth$Age_cat_w + 3


# (Import death probs)
death_dta <- read.csv("INPUT DATA/FILTERED/NOT ENAHO/LIVE_DEATH/DEATH_PROBS_l.csv")
# adapt death event probs
death_dta$Gender <- ifelse(death_dta$Gender == "Female", "mujer", "hombre")
death_dta$Gender <- as.factor(death_dta$Gender)
death_dta$Age_cat_w <- as.numeric(factor(death_dta$Age_cat,
                              levels = c("0-4", "5-9", "10-14", "15-19",
                                         "20-24", "25-29", "30-34", "35-39",
                                         "40-44", "45-49", "50-54", "55-59",
                                         "60-64", "65-69", "70-74", "75-79",
                                         ">79")))

# (Calculate distribution probs for age, gender and income generator
# from HH chief and the rest from 2015)
no_chief <- genage_dta[genage_dta$P203 != "Jefe/Jefa",]
on_chief <- genage_dta[genage_dta$P203 == "Jefe/Jefa",]

# Calculate gender distribution for chief and no chief
gen_pr <- as.data.frame(table(no_chief$P207)/sum(table(no_chief$P207)))
gen_ch <- as.data.frame(table(on_chief$P207)/sum(table(on_chief$P207)))

# Calculate age categories prob dist for chief by gender
ag_ch_m <- as.data.frame(table(on_chief[on_chief$P207 == "hombre",]$P208_c)/
                           sum(table(on_chief[on_chief$P207 == "hombre",]$P208_c)))
ag_ch_f <-as.data.frame(table(on_chief[on_chief$P207 == "mujer",]$P208_c)/
                          sum(table(on_chief[on_chief$P207 == "mujer",]$P208_c)))

# Calculate age categories prob dist for no chief by gender
ag_nc_m <- as.data.frame(table(no_chief[no_chief$P207 == "hombre",]$P208_c)/
                           sum(table(no_chief[no_chief$P207 == "hombre",]$P208_c)))
ag_nc_f <-as.data.frame(table(no_chief[no_chief$P207 == "mujer",]$P208_c)/
                          sum(table(no_chief[no_chief$P207 == "mujer",]$P208_c)))

# Join gender probs in one dataframe
roles <- c("Chief", "Non-chief")
genders <- as.character(gen_pr$Var1)

n_agcat <- c("age_cat", "prob")
names(ag_ch_m) <- n_agcat
ag_ch_m$gender <- genders[1]
ag_ch_m$role <- roles[1]

names(ag_ch_f) <- n_agcat
ag_ch_f$gender <- genders[2]
ag_ch_f$role <- roles[1]

names(ag_nc_m) <- n_agcat
ag_nc_m$gender <- genders[1]
ag_nc_m$role <- roles[2]

names(ag_nc_f) <- n_agcat
ag_nc_f$gender <- genders[2]
ag_nc_f$role <- roles[2]

mlist <- list(ag_ch_m, ag_ch_f, ag_nc_m, ag_nc_f)

age_cat_prs <- do.call("rbind", mlist)

# Determine probabilities of being an income generator at Tn by age/gender for
# chiefs and non-chiefs separetly
chief_cat <- "Jefe/Jefa del hogar"
ind_dta$P207 <- as.factor(ind_dta$P207)
gen_cats <- levels(ind_dta$P207)
age_cats <- levels(ind_dta$P208_c)

n_chr <- c("role", "gender", "age_c", "probs")

my_chars <- as.data.frame(matrix(nrow = 0,
                                 ncol = length(n_chr)))
names(my_chars) <- n_chr

for (i in seq(gen_cats)) {
  for (j in seq(age_cats)) {
    mychars_ig <- as.data.frame(table(ind_dta[ind_dta$P203 == chief_cat &
                                                ind_dta$P207 == gen_cats[i] &
                                                ind_dta$P208_c == age_cats[j],]$INC_GEN)/
                                  sum(table(ind_dta[ind_dta$P203 == chief_cat &
                                                      ind_dta$P207 == gen_cats[i] &
                                                      ind_dta$P208_c == age_cats[j],]$INC_GEN)))
    
    mychars_in <- as.data.frame(table(ind_dta[ind_dta$P203 != chief_cat &
                                                ind_dta$P207 == gen_cats[i] &
                                                ind_dta$P208_c == age_cats[j],]$INC_GEN)/
                                  sum(table(ind_dta[ind_dta$P203 != chief_cat &
                                                      ind_dta$P207 == gen_cats[i] &
                                                      ind_dta$P208_c == age_cats[j],]$INC_GEN)))
    
    th_chars <- as.data.frame(matrix(nrow = 2,
                                     ncol = 4))
    names(th_chars) <- n_chr
    
    if (nrow(mychars_ig) > 0) {
      th_chars[1,]$probs <- mychars_ig[mychars_ig$Var1 == "Yes",]$Freq
    } else {
      th_chars[1,]$probs <- 0
    }
    
    if (nrow(mychars_in) > 0) {
      th_chars[2,]$probs <- mychars_in[mychars_in$Var1 == "Yes",]$Freq
    } else {
      th_chars[2,]$probs <- 0
    }
    
    th_chars$gender <- gen_cats[i]
    th_chars[1,]$role <- roles[1]
    th_chars[2,]$role <- roles[2]
    th_chars$age_c <- age_cats[j]
    
    my_chars <- rbind(my_chars, th_chars)
  }
}

# Determine transition probabilities between income generator states by role and gender
j_vars <- c("t0", "t1", "cnt", "combo_cnt", "probs", "roles", "gender", "age_c")
my_tchar <- as.data.frame(matrix(nrow = 0,
                                 ncol = length(j_vars )))
names(my_tchar) <- j_vars

for (i in seq(gen_cats)) {
  for (j in seq(age_cats)) {
    hh_chief15 <- as.data.frame(table(igen_dta[igen_dta$p203_15 == 1 &
                                                 igen_dta$p207_15 == gen_cats[i] &
                                                 igen_dta$p208_c_15 == age_cats[j],]$inc_gen_15, 
                                      useNA = "ifany"))
    
    hh_chief1516 <- as.data.frame(table(igen_dta[igen_dta$p203_15 == 1 &
                                                   igen_dta$p207_15 == gen_cats[i] &
                                                   igen_dta$p208_c_15 == age_cats[j],]$inc_gen_15,
                                        igen_dta[igen_dta$p203_15 == 1 &
                                                   igen_dta$p207_15 == gen_cats[i] &
                                                   igen_dta$p208_c_15 == age_cats[j],]$inc_gen_16, 
                                        useNA = "ifany"))
    
    no_chief15 <- as.data.frame(table(igen_dta[igen_dta$p203_15 != 1 &
                                                 igen_dta$p207_15 == gen_cats[i] &
                                                 igen_dta$p208_c_15 == age_cats[j],]$inc_gen_15, 
                                      useNA = "ifany"))
    
    no_chief1516 <- as.data.frame(table(igen_dta[igen_dta$p203_15 != 1 &
                                                   igen_dta$p207_15 == gen_cats[i] &
                                                   igen_dta$p208_c_15 == age_cats[j],]$inc_gen_15,
                                        igen_dta[igen_dta$p203_15 != 1 &
                                                   igen_dta$p207_15 == gen_cats[i] &
                                                   igen_dta$p208_c_15 == age_cats[j],]$inc_gen_16,
                                        useNA = "ifany"))
    
    if (nrow(hh_chief1516) > 0) {
      names(hh_chief15) <- c("t0", "cnt")
      names(hh_chief1516) <- c("t0", "t1", "combo_cnt")
      mychars_ig <- merge(hh_chief1516, hh_chief15, by = "t0")
      mychars_ig$probs <- mychars_ig$combo_cnt / mychars_ig$cnt
      mychars_ig$roles <- roles[1]
      mychars_ig$gender <- gen_cats[i]
      mychars_ig$age_c <- age_cats[j]
    } else{
      mychars_ig <- as.data.frame(matrix(ncol = 2, nrow = 4))
      names(mychars_ig) <- c("t0", "t1")
      mychars_ig$t0 <- c("No", "No", "Yes", "Yes")
      mychars_ig$t1 <- c("No", "Yes", "No", "Yes")
      mychars_ig$combo_cnt <- 0
      mychars_ig$cnt <- 0
      mychars_ig$probs <- 0
      mychars_ig$roles <- roles[1]
      mychars_ig$gender <- gen_cats[i]
      mychars_ig$age_c <- age_cats[j]
    }
    
    if (nrow(no_chief1516) > 0) {
      names(no_chief15) <- c("t0", "cnt")
      names(no_chief1516) <- c("t0", "t1", "combo_cnt")
      mychars_in <- merge(no_chief1516, no_chief15, by = "t0")
      mychars_in$probs <- mychars_in$combo_cnt / mychars_in$cnt
      mychars_in$roles <- roles[2]
      mychars_in$gender <- gen_cats[i]
      mychars_in$age_c <- age_cats[j]
    } else {
      mychars_in <- as.data.frame(matrix(ncol = 2, nrow = 4))
      names(mychars_in) <- c("t0", "t1")
      mychars_in$t0 <- c("No", "No", "Yes", "Yes")
      mychars_in$t1 <- c("No", "Yes", "No", "Yes")
      mychars_in$combo_cnt <- 0
      mychars_in$cnt <- 0
      mychars_in$probs <- 0
      mychars_in$roles <- roles[2]
      mychars_in$gender <- gen_cats[i]
      mychars_in$age_c <- age_cats[j]
    }
    
    my_tchar <- rbind(my_tchar, mychars_ig)
    my_tchar <- rbind(my_tchar, mychars_in)
  }
}

# Determine probabilities of moving out by age category and gender
# Now, out of those, how many have the "se fue a otro hogar" class in the next year
my_indv$p217_c_16 <- ifelse(my_indv$p217_16 == "se fue a otro hogar",
                            "left", "other")

# Get the probs by age category and gender
x_df <- as.data.frame(table(my_indv$p207_15, my_indv$age_cat_15))
colnames(x_df) <- c("gender", "age_cat", "tot_count")

y_df <- as.data.frame(table(my_indv[my_indv$p217_c_16 == "left",]$p207_15,
                            my_indv[my_indv$p217_c_16 == "left",]$age_cat_15))
colnames(y_df) <- c("gender", "age_cat", "count_left")

xy_df <- merge(y_df, x_df)
xy_df$prob <- xy_df$count_left/xy_df$tot_count

# Save probability tables as csv files
# Gender no chief
write.csv(gen_pr, "INPUT DATA/FILTERED/PROBS/INDV/GENDER_NOCHIEF.csv", 
          row.names=FALSE)

# Gender chief
write.csv(gen_ch, "INPUT DATA/FILTERED/PROBS/INDV/GENDER_CHIEF.csv", 
          row.names=FALSE)

# age category probability distribution for chief male
write.csv(ag_ch_m, "INPUT DATA/FILTERED/PROBS/INDV/AGEC_CHIEF_MALE.csv", 
          row.names=FALSE)
# age category probability distribution for chief female
write.csv(ag_ch_f, "INPUT DATA/FILTERED/PROBS/INDV/AGEC_CHIEF_FMALE.csv", 
          row.names=FALSE)

# age category probability distribution for non chief male
write.csv(ag_nc_m, "INPUT DATA/FILTERED/PROBS/INDV/AGEC_NCHIEF_MALE.csv", 
          row.names=FALSE)

# age category probability distribution for non chief female
write.csv(ag_nc_f, "INPUT DATA/FILTERED/PROBS/INDV/AGEC_NCHIEF_FMALE.csv", 
          row.names=FALSE)

# probability of being an income generator by age, gender and role
my_charsw <- spread(my_chars, age_c, probs)

write.csv(my_charsw, "INPUT DATA/FILTERED/PROBS/INDV/YES_INC_GEN.csv", 
          row.names=FALSE)

# transition probability for income generation category
write.csv(my_tchar, "INPUT DATA/FILTERED/PROBS/INDV/INC_GEN_TRANS.csv", 
          row.names=FALSE)

# probability of moving out by age category and gender
write.csv(xy_df, "INPUT DATA/FILTERED/PROBS/INDV/MOVEOUT_PROBS.csv", 
          row.names=FALSE)

# Summary
#birth_dta: Children assignment probabilities by woman/age
#rel_birth: relative additional children according to the current number of children
#death_dta: death probabilities by age and gender
#gen_pr: gender distribution prob no chief
#gen_ch: gender distribution prob chief
#ag_ch_m: age category probability distribution for chief male
#ag_ch_f: age category probability distribution for chief female
#ag_nc_m: age category probability distribution for non chief male
#ag_nc_f: age category probability distribution for non chief female
#age_cat_prs: age category prob dist all together
#my_chars: probability of being an income generator by age, gender and role
#my_tchar: transition probability for income generation category by age, gender and role
#xy_df: probability of moving out by age category and gender


#####
# SET UP AND SIMULATE ECONOMIC IN HH LEVEL TABLE
#####
# (On synthetic data:)
# (Assing a unique code to each dwelling, assign SEL according to proportions)
vars_ag <- c("ID", "SEL", "Inc_cat_0", "Exp_cat_0", "HHmem_0")

total_ag <- 48
sel_b <- 0.2
sel_c <- 0.3
sel_d <- 0.5

# Create an empty dataframe with nr. of agents
my_agents <- as.data.frame(matrix(nrow = total_ag, ncol = length(vars_ag)))
names(my_agents) <- vars_ag

# Assing ID
my_agents$ID <- 1:total_ag

# Assign SEL
my_agents$SEL <- sample(SELs, total_ag, replace = T, 
                        prob = c(sel_b, sel_c, sel_d))

# Export IDs + SELs
write.csv(my_agents, "OUTPUTDATA/POST TABLES/idsANDsels.csv", 
          row.names=FALSE)

# (Apply distribution probs to synthetic data to have categorical state at t0)
nmem_cats <- levels(synt_or$MIEPERHO_C) # HH member relative categories
inc_cats <- levels(synt_or$REL_INC_C) # income relative categories
exp_cats <- levels(synt_or$REL_EXP_C) # expenses relative

for (i in seq_along(SELs)) {
  # HH members
  my_agents[my_agents$SEL == SELs[i],]$HHmem_0 <- sample(nmem_cats, 
                                                         nrow(my_agents[my_agents$SEL == SELs[i],]),
                                                         replace = T,
                                                         prob = prob_hmem[[SELs[i]]])
  
  # Income (and expeses which are the same category)
  my_agents[my_agents$SEL == SELs[i],]$Inc_cat_0 <- sample(inc_cats, 
                                                           nrow(my_agents[my_agents$SEL == SELs[i],]),
                                                           replace = T,
                                                           prob = prob_inc[[SELs[i]]])
  
  # Expenses
  for (j in seq(inc_c)){
    my_agents[my_agents$SEL == SELs[i] &
                my_agents$Inc_cat_0 == inc_c[j],]$Exp_cat_0 <- sample(exp_cats, 
                                                             nrow(my_agents[my_agents$SEL == SELs[i] &
                                                                              my_agents$Inc_cat_0 == inc_c[j],]),
                                                             replace = T,
                                                             prob = exp_pr[exp_pr$inc_cat == inc_c[j] &
                                                                             exp_pr$sel == SELs[i],]$probs)
  }
}

# (With trans probs, simulate inc/exp categories for 25 years)
# Income and expenses categories are linked. The are the same for both
nr_yrs <- 25

for (k in 0:(nr_yrs - 1)){
  # Saving categories
  inc_cat0 <- paste("Inc_cat", k, sep = "_")
  inc_cat1 <- paste("Inc_cat", k + 1, sep = "_")
  
  #exp_cat0 <- paste("Exp_cat", k, sep = "_")
  exp_cat1 <- paste("Exp_cat", k + 1, sep = "_")
  
  # Create empty variables
  my_agents[[inc_cat1]] <- NA
  my_agents[[exp_cat1]] <- NA
  
  for (l in 1:length(SELs)){
    my_ss <- my_agents[my_agents$SEL == SELs[l],]
    
    # Assign a category per each row
    for (m in 1:nrow(my_ss)){
      # Income
      opts <- inc_tpms[[l]][inc_tpms[[l]]$state0 == my_ss[m,][[inc_cat0]],]$state1
      probs <- inc_tpms[[l]][inc_tpms[[l]]$state0 == my_ss[m,][[inc_cat0]],]$probs
      
      my_cat <- sample(opts, 1, replace = T, prob = probs)
      my_ss[m,][[inc_cat1]] <- my_cat
      
      # Expenses (according to income)
      opts2 <- exp_cats
      probs2 <- exp_pr[exp_pr$inc_cat == my_cat &
                         exp_pr$sel == SELs[l],]$probs
      
      my_cat2 <- sample(opts2, 1, replace = T, prob = probs2)
      my_ss[m,][[exp_cat1]] <- my_cat2
    }
    # Replace in outer data frame
    my_agents[my_agents$SEL == SELs[l],][[inc_cat1]] <- my_ss[[inc_cat1]]
    my_agents[my_agents$SEL == SELs[l],][[exp_cat1]] <- my_ss[[exp_cat1]]
  }
  my_agents[[inc_cat1]] <- as.factor(my_agents[[inc_cat1]])
  my_agents[[exp_cat1]] <- as.factor(my_agents[[exp_cat1]])
}

# (With projected mean, calculate continuous inc/exp for all Ts)
for (i in 0:(nrow(pred_inc) - 1)){
  # Get categorical variable names
  icat_T0 <- paste("Inc_cat", i, sep = "_")
  ecat_T0 <- paste("Exp_cat", i, sep = "_")
  
  # Create empty continuous variables
  inc_t0 <- paste("Income_pg", i, sep = "_")
  exp_t0 <- paste("Expenses_pm", i, sep = "_")
  
  my_agents[[inc_t0]] <- NA
  my_agents[[exp_t0]] <- NA
  
  for (j in seq_along(SELs)) {
    # Get average of the year by SEL
    SEL_avginc <- pred_inc[i+1,][[SELs[j]]]
    SEL_avgexp <- pred_exp[i+1,][[SELs[j]]]
    
    # Get average of the interval by SEL (abc is the df with averages by interval
    # row = cats and cols = SELs)
    inc_n <- as.numeric(as.character(my_agents[my_agents$SEL == SELs[j],][[icat_T0]]))
    avgi_int <- intvm_inc[[j]][inc_n]
    
    exc_n <- as.numeric(as.character(my_agents[my_agents$SEL == SELs[j],][[ecat_T0]]))
    avge_int <- intvm_exp[[j]][exc_n]
    
    # Assign continuous
    my_agents[my_agents$SEL == SELs[j],][[inc_t0]] <- SEL_avginc * avgi_int
    my_agents[my_agents$SEL == SELs[j],][[exp_t0]] <- SEL_avgexp * avge_int
  }
}

#####
# SET UP AND SIMULATE DEMOGRAPHIC IN INDV LEVEL TABLE
#####
# (On synthetic data:)
# (Create a synthetic table with as many entries as people in the neighbourhood)
vars_mm <- c("dw_id", "hh_id_0", "numper", "role_0", "gender", "age_cat_0", "age_0", 
             "inc_gen_0", "children_0")
my_members <- as.data.frame(matrix(nrow = sum(as.numeric(as.character(
  my_agents$HHmem_0))), ncol = length(vars_mm)))
names(my_members) <- vars_mm

# Assign dwelling, person and hh id
my_members$numper <- 1:nrow(my_members)
my_members$dw_id <- rep(my_agents$ID, my_agents$HHmem_0)
my_members$hh_id_0 <- as.numeric(paste(rep(my_agents$ID, my_agents$HHmem_0), 
                                       "000", sep = ""))

# (Assign one HH chief role per dwelling)
my_members[match(unique(my_members$hh_id_0), my_members$hh_id_0),]$role_0 <- roles[1]
my_members[is.na(my_members$role),]$role_0 <- roles[2]

# (Assign gender by role, and generator status)
my_members$gender <- ifelse(my_members$role_0 == roles[1], 
                            sample(as.character(gen_ch$Var1), 
                                   nrow(my_members[my_members$role_0 == roles[1],]),
                                   replace = T,
                                   prob = gen_ch$Freq),
                            sample(as.character(gen_pr$Var1), 
                                   nrow(my_members[my_members$role_0 != roles[1],]), 
                                   replace = T, 
                                   prob = gen_pr$Freq))

# (Assign age category and generator status by role and gender)
for (i in seq(roles)){
  for (j in seq(genders)){
    # Age categories
    my_members$age_cat_0 <- as.factor(my_members$age_cat_0)
    levels(my_members$age_cat_0) <- as.character(1:17)
    
    my_members[my_members$role == roles[i] &
                 my_members$gender == genders[j],]$age_cat_0 <- 
      sample(as.character(levels(age_cat_prs$age_cat)),
             nrow(my_members[my_members$role == roles[i] &
                               my_members$gender == genders[j],]),
             replace = T,
             prob = age_cat_prs[age_cat_prs$role == roles[i] &
                                  age_cat_prs$gender == genders[j],]$prob)
    for (k in seq(age_cats)){
      # Income generator status
      if (my_chars[my_chars$role == roles[i] &
                   my_chars$gender == genders[j] &
                   my_chars$age_c == age_cats[k],]$probs > 0){
        
        my_members[my_members$role == roles[i] &
                     my_members$gender == genders[j] &
                     my_members$age_cat_0 == age_cats[k],]$inc_gen_0 <- 
          sample(c("Yes", "No"),
                 nrow(my_members[my_members$role == roles[i] &
                                   my_members$gender == genders[j] &
                                   my_members$age_cat_0 == age_cats[k],]),
                 replace = T,
                 prob = c(my_chars[my_chars$role == roles[i] &
                                   my_chars$gender == genders[j] &
                                   my_chars$age_c == age_cats[k],]$probs,
                          1 - my_chars[my_chars$role == roles[i] &
                                         my_chars$gender == genders[j] &
                                         my_chars$age_c == age_cats[k],]$probs))
      } else if (nrow(my_members[my_members$role == roles[i] &
                                 my_members$gender == genders[j] &
                                 my_members$age_cat_0 == age_cats[k],]) > 0) {
        my_members[my_members$role == roles[i] &
                     my_members$gender == genders[j] &
                     my_members$age_cat_0 == age_cats[k],]$inc_gen_0 <- "No"
      }
    }
  }
}

# (To every women according to their age category, assign a number of children 
# according to children probs)
fert_age <- my_members[my_members$gender == "mujer" & 
                         as.numeric(as.character(my_members$age_cat_0)) > 3 &
                         as.numeric(as.character(my_members$age_cat_0)) <= 10,]

for (k in 1:nrow(fert_age)) {
  fert_age[k,]$children_0 <- sample(unique(thisy_brths$Num_child),
                                    1,
                                    replace = T,
                                    prob = thisy_brths[thisy_brths$Age_cat_z ==
                                                       as.numeric(
                                                         as.character(
                                                           fert_age[k,]$age_cat_0)),]$probs)
}

my_members$children_0 <- 0
my_members[my_members$gender == "mujer" & 
             as.numeric(as.character(my_members$age_cat_0)) > 3 &
             as.numeric(as.character(my_members$age_cat_0)) <= 10,]$children_0 <- fert_age$children_0


# (Transform categorical to continuous age)
ag_cat <- levels(as.factor(as.numeric(my_members$age_cat_0)))

cut_sers <- seq(-1, 79, 5)
cut_sers <- c(cut_sers, Inf)

for (i in 1:nrow(my_members)){
  for (j in seq(ag_cat)){
    if (my_members[i,]$age_cat_0 == ag_cat[j]){
      my_members[i,]$age_0 <- sample((cut_sers[j] + 1):(cut_sers[j] + 5), 
                                     1,
                                     replace = T)
    }
  }
}

# Add aditional variables
my_members$die_0 <- "ALIVE"
my_members$m_out_0 <- "NO"
my_members$new_births_0 <- 0


state_0 <- my_members

# The following are to remove variables once sim is over?
#rm(state_1)
#rm(newcomers) # There is also newcomers 3, so when I reach that, I encounter a fail
#rm(nbrns)
#rm(babies)

# This year death data (we will only use 2019)
thisy_death <- death_dta[death_dta$Year == 2019,]

# SIMULATE
for (year in 1:nr_yrs){
  # Filter relative births data for this year of simulation
  thisy_rbrt <- rel_birth[rel_birth$Year == year + 2014,]
  
  # Filter distribution births for new members for this year of sim
  thisy_brt <- birth_dta[birth_dta$Year == year + 2014,]
  
  # Determine chronological year
  this_y <- year
  last_y <- this_y - 1
  
  # All needed variable names
  # Age (continuous)
  thisy_agev <- paste("age", this_y, sep = "_")
  lasty_agev <- paste("age", last_y, sep = "_")
  
  # Age (categorical)
  lasty_agecv <- paste("age_cat", last_y, sep = "_")
  thisy_agecv <- paste("age_cat", this_y, sep = "_")
  
  # HH code
  thisy_hhcode <- paste("hh_id", this_y, sep = "_")
  lasty_hhcode <- paste("hh_id", last_y, sep = "_")
  
  # Deaths and newborns
  thisy_diev <- paste("die", this_y, sep = "_")
  lasty_diev <- paste("die", last_y, sep = "_")
  
  lasty_child <- paste("children", last_y, sep = "_")
  thisy_child <- paste("children", this_y, sep = "_")
  
  new_born <- paste("new_births", this_y, sep = "_")
  
  # Move outs
  thisy_mout <- paste("m_out", this_y, sep = "_")
  lasty_mout <- paste("m_out", last_y, sep = "_")
  
  # Role
  role_thisy <- paste("role", this_y, sep = "_")
  role_lasty <- paste("role", last_y, sep = "_")
  
  # Income generator status
  thisy_igen <- paste("inc_gen", this_y, sep = "_")
  lasty_igen <- paste("inc_gen", last_y, sep = "_")
  
  # List of variables for HH level tables
  #vars_ag
  
  # List of variables for new individual level tables
  vars_turn <- c("dw_id", thisy_hhcode, "numper", role_thisy, 
                 "gender", thisy_agecv, thisy_agev, thisy_igen, thisy_child)
  
  # Determine current age according to last year's
  state_0[[thisy_agev]] <- state_0[[lasty_agev]] + 1
  
  # Categorize current age
  cut_sers <- seq(-1, 79, 5)
  cut_sers <- c(cut_sers, Inf)
  state_0[[thisy_agecv]] <- cut(state_0[[thisy_agev]],
                                breaks=cut_sers,
                                labels = 1:(length(cut_sers) - 1),
                                include.lowest = TRUE)
  
  # Assign HH code
  state_0[[thisy_hhcode]] <- ifelse(is.na(state_0[[lasty_hhcode]]), NA, state_0[[lasty_hhcode]])
  
  # Determine if individual dies or moves out
  state_0[[thisy_diev]] <- 0
  state_0[[thisy_mout]] <- 0
  
  for (i in 1:nrow(state_0)){
    my_agecat <- state_0[i,][[thisy_agecv]]
    mygen <- state_0[i,]$gender
    
    
    if (!is.na(my_agecat) & !is.na(mygen)){
      # Isolate my death probs according to age cat and gender
      my_pr_die <- thisy_death[thisy_death$Gender == mygen & 
                                 thisy_death$Age_cat_w == as.numeric(
                                 as.character(my_agecat)),]$Probs
      
      # Die?
      opts_die <- c("ALIVE", "DEAD")
      alive_prob <- 1 - my_pr_die
      all_probsd <- c(alive_prob, my_pr_die)
      
      outcome <- sample(opts_die, 1, replace = T, prob = all_probsd)
      
      # Isolate my move_out probs according to age cat and gender
      my_pr_mout <- xy_df[xy_df$gender == mygen &
                            xy_df$age_cat == my_agecat,]$prob
      
      # Move out?
      opts_mout <- c("NO", "YES")
      stay_prob <- 1 - my_pr_die
      all_pmo <- c(stay_prob, my_pr_mout)
      
      moutcome <- sample(opts_mout, 1, replace = T, prob = all_pmo)
      
    } else {
      outcome <- NA
      moutcome <- NA
    }
    # Save outcome
    state_0[i,][[thisy_diev]] <- outcome
    state_0[i,][[thisy_mout]] <- moutcome
  }
  
  # When TRUE die or move out, delete the new HH id
  state_0[[thisy_hhcode]][state_0[[thisy_diev]] == "DEAD"] <- NA
  state_0[[thisy_hhcode]][state_0[[thisy_mout]] == "YES"] <- NA
  
  # If they died or moved out before, keep them out
  if (nrow(state_0[state_0[[lasty_diev]] == "DEAD" &
                   !is.na(state_0[[lasty_diev]]),]) > 0){
    state_0[state_0[[lasty_diev]] == "DEAD" &
              !is.na(state_0[[lasty_diev]]),][[thisy_diev]] <- "DEAD"
  }
  
  if (nrow(state_0[state_0[[lasty_mout]] == "YES" &
                   !is.na(state_0[[lasty_mout]]),]) > 0){
    state_0[state_0[[lasty_mout]] == "YES" &
              !is.na(state_0[[lasty_mout]]),][[thisy_mout]] <- "YES"
  }
  
  # Check dwellings that do not have members (after considering deaths)
  empty_hh1 <- setdiff(levels(as.factor(state_0[[lasty_hhcode]])), 
                       levels(as.factor(state_0[[thisy_hhcode]]))) # Ids of empty HHs
  
  empty_dw1 <- c()
  if (length(empty_hh1) > 0){
    for (ed in 1:length(empty_hh1)){
      mm_cc <- paste(unlist(strsplit(empty_hh1[ed],""))[-c((nchar(empty_hh1[ed])-2):
                                                             nchar(empty_hh1[ed]))],
                     collapse = "")
      empty_dw1 <- c(empty_dw1, mm_cc)
    }
    empty_dw1 <- as.numeric(empty_dw1)
  }
  
  # HERE SHOULD GO MOVE INs
  
  # Check that every household has one chief
  state_0[[role_thisy]] <- state_0[[role_lasty]]
  chf_cnt <- as.data.frame(table(state_0[[thisy_hhcode]], state_0[[role_thisy]]))
  no_chf_cds <- chf_cnt[chf_cnt$Var2 == roles[1] & chf_cnt$Freq < 1,]$Var1
  no_chf_cds <- as.numeric(as.character(no_chf_cds))
  
  # Check that these households still have members (otherwise they will be replenished later)
  if (length(empty_dw1) > 0){
    no_head <- setdiff(no_chf_cds, empty_dw1)
  } else {
    no_head <- no_chf_cds
  }

  # If they do, check that at least one member is on age category >= 4, if none, delete hh code and members (all die)
  no_head <- as.numeric(as.character(no_head))
  if (length(no_head) > 0){
    for (p in 1:length(no_head)){
      # Members of one of those hhs
      #state_0 <- state_0[complete.cases(state_0),]
      hh_hl_mems <- state_0[!is.na(state_0[[thisy_hhcode]]) &
                              state_0[[thisy_hhcode]] == no_head[p],]
      
      # Check if adults on the HH (age category >= 4)
      if (max(as.numeric(
        as.character(hh_hl_mems[[thisy_agecv]])
        ), na.rm=T) >= 4){
        
        nr_row_eldest <- which(hh_hl_mems[[thisy_agev]] == max(as.numeric(
          as.character(hh_hl_mems[[thisy_agev]])), na.rm=T))
        
        #hh_hl_mems[nr_row_eldest,][[role_thisy]] <- roles[1]
 
        state_0[!is.na(state_0[[thisy_hhcode]]) &
                  state_0[[thisy_hhcode]] == no_head[p],][[role_thisy]][nr_row_eldest] <- roles[1]
      
        } else{
        # If no adults, get rid of all members
        state_0[!is.na(state_0[[thisy_hhcode]]) &
                  state_0[[thisy_hhcode]] == no_head[p],][[thisy_hhcode]] <- NA
      }
    }
  }
  
  # Check again dwellings that do not have members (after considering children)
  empty_hh2 <- setdiff(levels(as.factor(state_0[[lasty_hhcode]])), 
                      levels(as.factor(state_0[[thisy_hhcode]]))) # Ids of empty HHs
  
  empty_dw2 <- c()
  if (length(empty_hh2) > 0){
    for (ed in 1:length(empty_hh2)){
      mm_cc <- paste(unlist(strsplit(empty_hh2[ed],""))[-c((nchar(empty_hh2[ed])-2):
                                                            nchar(empty_hh2[ed]))],
                     collapse = "")
      empty_dw2 <- c(empty_dw2, mm_cc)
    }
    empty_dw2 <- as.numeric(empty_dw2)
  }
  
  # Check if women gives birth
  st_0_fec <- state_0[state_0$gender == "mujer" & 
                       as.numeric(as.character(state_0[[thisy_agecv]])) > 3 &
                        as.numeric(as.character(state_0[[thisy_agecv]])) <= 10,]
  
  st_0_fec[[thisy_child]] <- NA
  st_0_fec[[new_born]] <- NA
  
  # Make sure the string exists in case is needed later
  babies_ts <- paste("babies", this_y, sep = "_")
  
  for (k in 1:nrow(st_0_fec)) {
    child_ly <- st_0_fec[k,][[lasty_child]]
    curr_agc <- as.numeric(as.character(st_0_fec[k,][[thisy_agecv]]))
    
    add_chld_pr <- thisy_rbrt[thisy_rbrt$Age_cat_z == curr_agc &
                                thisy_rbrt$Num_child == child_ly,]$Add_chl_pyy
    prbs <- c(1 - add_chld_pr, add_chld_pr)
    elec <- c(0, 1)
    
    chld_incr <- sample(elec,
                        1,
                        replace = T,
                        prob = prbs)
    
    st_0_fec[k,][[new_born]] <- chld_incr # New children this year
    st_0_fec[k,][[thisy_child]] <- chld_incr + st_0_fec[k,][[lasty_child]] # accumulated children
  }
  
  state_0[[thisy_child]] <- state_0[[lasty_child]]
  state_0[[new_born]] <- 0
  
  state_0[state_0$gender == "mujer" & 
            as.numeric(as.character(state_0[[thisy_agecv]])) > 3 &
            as.numeric(as.character(state_0[[thisy_agecv]])) <= 10,][[thisy_child]] <- st_0_fec[[thisy_child]]
  
  state_0[state_0$gender == "mujer" & 
            as.numeric(as.character(state_0[[thisy_agecv]])) > 3 &
            as.numeric(as.character(state_0[[thisy_agecv]])) <= 10,][[new_born]] <- st_0_fec[[new_born]]
  
  tst <- as.formula(paste(". ~", thisy_hhcode))
  nbrns <- aggregate(tst,data=state_0[,c(thisy_hhcode, new_born)],FUN=sum) # newborns by hh code
  nbrns <- nbrns[nbrns[[new_born]] > 0,]
  
  # If there are children
  if (nrow(nbrns) > 0) {
    # Get dwelling codes
    codes_chars <- as.character(nbrns[[thisy_hhcode]])
    mm_bb <- c()
    for (ba in 1:length(codes_chars)){
      mm_cc <- paste(unlist(strsplit(codes_chars[ba],""))[-c((nchar(codes_chars[ba])-2):
                                                               nchar(codes_chars[ba]))],
                     collapse = "")
      mm_bb <- c(mm_bb, mm_cc)
    }
    
    # Create new table with only babies
    babies <- as.data.frame(matrix(nrow = sum(as.numeric(as.character(
      nbrns[[new_born]]))), ncol = length(vars_turn)))
    names(babies) <- vars_turn
    
    babies$numper <- paste(1:nrow(babies), this_y, sep = "_")
    babies$dw_id <- rep(as.numeric(mm_bb), nbrns[[new_born]])
    
    babies[[thisy_hhcode]] <- rep(nbrns[[thisy_hhcode]], nbrns[[new_born]])
    babies[[role_thisy]] <- roles[2]
    babies$gender <- sample(as.character(gen_ch$Var1), 
                            nrow(babies),
                            replace = T,
                            prob = c(0.5, 0.5))
    
    babies[[thisy_agecv]] <- as.factor(babies[[thisy_agecv]])
    levels(babies[[thisy_agecv]]) <- levels(state_0$age_cat_0)
    babies[[thisy_agecv]] <- as.character(1)
    
    babies[[thisy_agev]] <- 0
    babies[[thisy_igen]] <- "No"
    babies[[thisy_child]] <- 0
    babies[[thisy_diev]] <- "ALIVE"
    babies[[new_born]] <- 0
    
    # Prepare to Add list of babies to main list
    state_0$numper <- as.character(state_0$numper)
    
    babies[[thisy_hhcode]] <- as.numeric(
      as.character(babies[[thisy_hhcode]]))
    
    # Assign name to babies df related to the number of iteration
    assign(babies_ts, babies, envir = .GlobalEnv)
  }
  
  # Update income generating status
  state_0[[thisy_igen]] <- NA
  pot_answ <- c("Yes", "No")
  
  for (y in seq(roles)){
    for (w in seq(genders)){
      for (v in seq(age_cats)){
        for (z in seq(pot_answ)){
          if(length(state_0[!is.na(state_0[[lasty_igen]]) &
                          state_0[[lasty_igen]] == pot_answ[z] &
                          state_0[[role_lasty]] == roles[y] &
                          state_0$gender == genders[w] &
                          state_0[[lasty_agecv]] == age_cats[v],][[lasty_igen]]) > 0) {
            
            opts <- my_tchar[my_tchar$t0 == pot_answ[z] &
                               my_tchar$roles == roles[y] &
                               my_tchar$gender == genders[w] &
                               my_tchar$age_c == age_cats[v],]$t1
            
            probs <- my_tchar[my_tchar$t0 == pot_answ[z] &
                                my_tchar$roles == roles[y] &
                                my_tchar$gender == genders[w] &
                                my_tchar$age_c == age_cats[v],]$probs
            
            if (length(opts[!is.na(opts)]) == 1) {
              opts <- c("No", "Yes")
              probs <- c(0, 1)
            } else if (length(opts[!is.na(opts)]) == 0){
              opts <- c("No", "Yes")
              probs <- c(1, 0)
            }
            
            opts[1] <- ifelse(probs[1] == 0 & probs [2] == 0,
                           "No", opts[1])
            
            opts[2] <- ifelse(probs[1] == 0 & probs [2] == 0,
                              "Yes", opts[2])
            
            probs[1] <- ifelse(probs[1] == 0 & probs [2] == 0,
                            c(1, 0), probs[1])
            
            state_0[!is.na(state_0[[lasty_igen]]) &
                      state_0[[lasty_igen]] == pot_answ[z] &
                      state_0[[role_lasty]] == roles[y] &
                      state_0$gender == genders[w] &
                      state_0[[lasty_agecv]] == age_cats[v],][[thisy_igen]] <-
              sample(opts, 
                     nrow(state_0[!is.na(state_0[[lasty_igen]]) &
                                    state_0[[lasty_igen]] == pot_answ[z] &
                                    state_0[[role_lasty]] == roles[y] &
                                    state_0$gender == genders[w] &
                                    state_0[[lasty_agecv]] == age_cats[v],]),
                     replace = T,
                     prob = probs)
          }
          }
        }
      }
    }


  # Create new households to populate empty dwellings
  # Make sure the string exists to evaluate later
  newcom_ts <- paste("newcomers", this_y, sep = "_")
  
  if (length(empty_dw2) > 0){
    # HH table
    new_hhs <-  as.data.frame(matrix(nrow = length(empty_dw2), 
                                         ncol = length(vars_ag)))
    names(new_hhs) <- vars_ag
    
    new_hhs$ID <- empty_dw2
    new_hhs$SEL <- my_agents[my_agents$ID %in% empty_dw2,]$SEL
    
    # Assign number of HH members
    for (l in seq(SELs)){
      if (nrow(new_hhs[new_hhs$SEL == SELs[l],])){
        new_hhs[new_hhs$SEL == SELs[l],]$HHmem_0 <- sample(nmem_cats, 
                                                           nrow(new_hhs[new_hhs$SEL == SELs[l],]),
                                                           replace = T,
                                                           prob = prob_hmem[[SELs[l]]])
      }
    }
    
    # Create a new members' table acoording to the number of members
    newcomers <- as.data.frame(matrix(nrow = sum(as.numeric(as.character(
      new_hhs$HHmem_0))), ncol = length(vars_turn)))
    names(newcomers) <- vars_turn
    
    # Basic ID codes
    newcomers$dw_id <- rep(new_hhs$ID, new_hhs$HHmem_0)
    if (exists(babies_ts)){ # & nrow(eval(parse(text = babies_ts))) > 0
      low_nbr <- nrow(eval(parse(text = babies_ts)))
    } else {
      low_nbr <- 0
    }
    newcomers$numper <- paste((low_nbr + 1):(low_nbr + nrow(newcomers)),
                              this_y, sep = "_") # So they dont repeat with babies
    if (this_y < 10){
      lead <- "00"
    } else {
      lead <- "0"
    }
    lead <- paste(lead, this_y, sep = "")
    
    newcomers[[thisy_hhcode]] <- as.numeric(paste(rep(new_hhs$ID, new_hhs$HHmem_0), 
                                                  lead, sep = ""))
    
    # (Assign one HH chief role per dwelling)
    newcomers[match(unique(newcomers[[thisy_hhcode]]), newcomers[[thisy_hhcode]]),][[role_thisy]] <- roles[1]
    
    if (nrow(newcomers[is.na(newcomers[[role_thisy]]),]) > 0) {
      newcomers[is.na(newcomers[[role_thisy]]),][[role_thisy]] <- roles[2]
    }
    
    # (Assign gender by role, and generator status)
    newcomers$gender <- ifelse(newcomers[[role_thisy]] == roles[1], 
                               sample(as.character(gen_ch$Var1), 
                                      nrow(newcomers[newcomers[[role_thisy]] == roles[1],]),
                                      replace = T,
                                      prob = gen_ch$Freq),
                               sample(as.character(gen_pr$Var1), 
                                      nrow(newcomers[newcomers[[role_thisy]] != roles[1],]), 
                                      replace = T, 
                                      prob = gen_pr$Freq))
    
    # (Assign age category and generator status by role and gender)
    for (ab in seq(roles)){
      for (cd in seq(genders)){
        # Age categories
        newcomers[[thisy_agecv]] <- as.factor(newcomers[[thisy_agecv]])
        levels(newcomers[[thisy_agecv]]) <- levels(state_0$age_cat_0)
        
        newcomers[newcomers[[role_thisy]] == roles[ab] &
                    newcomers$gender == genders[cd],][[thisy_agecv]] <- 
          sample(as.character(levels(age_cat_prs$age_cat)),
                 nrow(newcomers[newcomers[[role_thisy]] == roles[ab] &
                                  newcomers$gender == genders[cd],]),
                 replace = T,
                 prob = age_cat_prs[age_cat_prs$role == roles[ab] &
                                      age_cat_prs$gender == genders[cd],]$prob)
        for (ef in seq(age_cats)){
          # Income generator status
          if (my_chars[my_chars$role == roles[ab] &
                       my_chars$gender == genders[cd] &
                       my_chars$age_c == age_cats[ef],]$probs > 0){
            
            newcomers[newcomers[[role_thisy]] == roles[ab] &
                        newcomers$gender == genders[cd] &
                        newcomers[[thisy_agecv]] == age_cats[ef],][[thisy_igen]] <- 
              sample(c("Yes", "No"),
                     nrow(newcomers[newcomers[[role_thisy]] == roles[ab] &
                                      newcomers$gender == genders[cd] &
                                      newcomers[[thisy_agecv]] == age_cats[ef],]),
                     replace = T,
                     prob = c(my_chars[my_chars$role == roles[ab] &
                                         my_chars$gender == genders[cd] &
                                         my_chars$age_c == age_cats[ef],]$probs,
                              1 - my_chars[my_chars$role == roles[ab] &
                                             my_chars$gender == genders[cd] &
                                             my_chars$age_c == age_cats[ef],]$probs))
          } else if (nrow(newcomers[newcomers[[role_thisy]] == roles[ab] &
                                    newcomers$gender == genders[cd] &
                                    newcomers[[thisy_agecv]] == age_cats[ef],]) > 0) {
            newcomers[newcomers[[role_thisy]] == roles[ab] &
                        newcomers$gender == genders[cd] &
                        newcomers[[thisy_agecv]] == age_cats[ef],][[thisy_igen]] <- "No"
          }
        }
      }
    }
    
    # (To every women according to their age category, assign a number of children 
    # according to children probs)
    fert_age <- newcomers[newcomers$gender == "mujer" & 
                            as.numeric(as.character(newcomers[[thisy_agecv]])) > 3 &
                            as.numeric(as.character(newcomers[[thisy_agecv]])) <= 10,]
    
    newcomers[[thisy_child]] <- 0 # by default, everybody has 0 children
    
    if(nrow(fert_age) > 0){ # unless they fall in this category, where the value is changed
      for (gh in 1:nrow(fert_age)) {
        fert_age[gh,][[thisy_child]] <- sample(unique(thisy_brt$Num_child),
                                               1,
                                               replace = T,
                                               prob = thisy_brt[thisy_brt$Age_cat_z ==
                                                                  as.numeric(
                                                                    as.character(
                                                                      fert_age[gh,][[thisy_agecv]])),]$probs)
      }
      newcomers[newcomers$gender == "mujer" & 
                  as.numeric(as.character(newcomers[[thisy_agecv]])) > 3 &
                  as.numeric(as.character(newcomers[[thisy_agecv]])) <= 10,][[thisy_child]] <- fert_age[[thisy_child]]
    }
    
    # (Transform categorical to continuous age)
    ag_cat <- levels(as.factor(1:17))
    
    for (ij in 1:nrow(newcomers)){
      for (kl in seq(ag_cat)){
        if (as.numeric(as.character(newcomers[ij,][[thisy_agecv]])) == ag_cat[kl]){
          newcomers[ij,][[thisy_agev]] <- 
            sample((cut_sers[kl] + 1):(cut_sers[kl] + 5), 
                                         1,
                                         replace = T)
        }
      }
    }
    # Prepare to Add list of newcomers to main list
    newcomers[[thisy_hhcode]] <- as.numeric(
      as.character(newcomers[[thisy_hhcode]]))
    
    newcomers[[thisy_diev]] <- "ALIVE"
    newcomers[[new_born]] <- 0
    
    # Assign a name according to ts
    assign(newcom_ts, newcomers, envir = .GlobalEnv)
  }
  
  # Save the dataframe for all years
  if (exists("state_1")){
    # intersect the lists of variable names
    ns1 <- names(state_0)
    ns2 <- names(state_1)
    commonn <- intersect(ns1, ns2) # are we includin hh code here?
    
    # Update common columns
    com <- state_0 %>% dplyr::select(all_of(commonn))
    # DOWN HERE IS THE PROBLEM
    state_1 <- state_1 %>% rows_update(com, by = "numper") # updates common rows with modified t0
    
    # Then join but avoiding the common columns
    notcom <- commonn[!commonn == "numper"] # take out numper
    difcols <- state_0 %>% dplyr::select(-all_of(notcom))
    state_1 <- full_join(state_1, difcols, by = "numper") # keeps records from state_1 and adds state_0
    
    # Then add new observations at the bottom of table (if they exist)
    if (exists(babies_ts)){
      state_1 <- bind_rows(state_1, eval(parse(text = babies_ts)))
    }
    
    if (exists(newcom_ts)){
      state_1 <- bind_rows(state_1, eval(parse(text = newcom_ts)))
    }
    
  } else {
    # Just add new observations at the end of the table (only if they exist)
    state_1 <- state_0
    
    if (exists(babies_ts)){
      state_1 <- bind_rows(state_1, eval(parse(text = babies_ts)))
    }
    
    if (exists(newcom_ts)){
      state_1 <- bind_rows(state_1, eval(parse(text = newcom_ts)))
    }
  }
  
  # New members do not appear beacuse they do not have complete cases!
  #state_0 <- state_1[complete.cases(state_1),]
  state_0 <- state_1
}

# Reveal mumber of members by dwelling
numpas <- paste("hh_id", 0:nr_yrs, sep = "_")
#numpas <- "dw_id"
inc_vars <- paste("inc_gen", 0:nr_yrs, sep = "_")
full_df <- data.frame(hhcode = unique(state_1$hh_id_0))

for (i in seq(numpas)){ #  There is only one numpas
  # Get variable names for joined df
  curr_n <- paste("hhmem", i - 1, sep = "_")
  curr_n2 <- paste("inc_gen", i - 1, sep = "_")
  
  # Create data frames from tables
  fdf <- as.data.frame(table(state_1[[numpas[i]]]))
  inc_byhh <- as.data.frame(table(state_1[[numpas[i]]], state_1[[inc_vars[i]]]))
  
  # rename variables in created dfs
  names(fdf) <- c("hhcode", curr_n)
  names(inc_byhh) <- c("hhcode", "answer", "inc_gen")

  # Filter double variable data frame
  only_incg <- inc_byhh[inc_byhh$answer == "Yes",][,-2]
  names(only_incg) <- c("hhcode", curr_n2)
  
  # Merge with external global data frame
  together <- merge(fdf, only_incg, by = "hhcode", all=T)
  full_df <- merge(full_df, together, by = "hhcode", all=T)
}

# Now create a new variable with the number of dwelling
full_df$ID <- substr(full_df$hhcode, 1, nchar(full_df$hhcode)-3)
full_df <- full_df[!is.na(full_df$ID),]

# Filter only hhmem by dwelling ID
only_mem_nms <- c("ID", paste("hhmem", 0:25, sep = "_"))
only_mem <- full_df[,only_mem_nms]

mems_pop <- only_mem %>%
  group_by(ID) %>%
  summarise(across(c(paste("hhmem", 0:25, sep = "_")), sum, na.rm = T))

mems_long <- gather(mems_pop, year_, members, hhmem_0:hhmem_25, factor_key=TRUE)
mems_long$year_ <- as.character(mems_long$year_)
mems_long$year <- substring(mems_long$year_, 
                            regexpr("_", mems_long$year_) + 1, 
                            nchar(mems_long$year_))
mems_long$ID <- as.numeric(mems_long$ID)
mems_long$year <- as.numeric(mems_long$year)
mems_long <- mems_long[,c("ID", "year", "members")]

# Now do the same but only with income generators
only_igs_nms <- c("ID", paste("inc_gen", 0:25, sep = "_"))
only_igs <- full_df[,only_igs_nms]

igs_pop <- only_igs %>%
  group_by(ID) %>%
  summarise(across(c(paste("inc_gen", 0:25, sep = "_")), sum, na.rm = T))

igs_long <- gather(igs_pop, year_, income_g, inc_gen_0:inc_gen_25, factor_key=TRUE)
igs_long$year_ <- as.character(igs_long$year_)
igs_long$year <- sub(".*([A-Za-z]+_[A-Za-z]+_)", "\\2", igs_long$year_)
igs_long$ID <- as.numeric(igs_long$ID)
igs_long$year <- as.numeric(igs_long$year)
igs_long <- igs_long[,c("ID", "year", "income_g")]

# Join both long formats
pops_long <- merge(igs_long, mems_long)

# Join Wide formats
dw_pop <- merge(mems_pop, igs_pop)

# ATTRACTIVENESS CAN BE DEFINED AS A RESULT OF OCCUPANCY RATIO OR PER CAPITA BALANCE
# VARIATION FROM T0 TO T1 AND OCCUPANCY RATIO/SAVINGS PER CAPITA AT T0

#####
# JOIN PREVIOUS TABLES
#####

# On the HH level table, append the data of HH members and income generators
results <- merge(my_agents, dw_pop, by = "ID")

# For every year. Do the multiplication by the numbers per capita
for (i in 0:nr_yrs){
  # HOUSEHOLD INCOME
  y_incm <- paste("Income_pg", i, sep = "_")
  y_genr <- paste("inc_gen", i, sep = "_")
  hh_inc <- paste("hh_inc", i, sep = "_")
  results[[hh_inc]] <- results[[y_incm]] * results[[y_genr]]
  
  # HOUSEHOLD EXPENSES
  y_exp <- paste("Expenses_pm", i, sep = "_")
  y_hhm <- paste("hhmem", i, sep = "_")
  hh_exp <- paste("hh_exp", i, sep = "_")
  results[[hh_exp]] <- results[[y_exp]] * results[[y_hhm]]
  
  # HOUSEHOLD SAVINGS
  y_savs <- paste("savings", i, sep = "_")
  results[[y_savs]] <- results[[hh_inc]] - results[[hh_exp]]
}

f_outcome <- results[,c("ID", "SEL", paste("hhmem", 0:nr_yrs, sep = "_"),
                        paste("hh_inc", 0:nr_yrs, sep = "_"),
                        paste("savings", 0:nr_yrs, sep = "_"))]


#####
# Export required CSV
#####
# First only household members
#mems_long already set on the last part of the model

# Then only yearly savings
sel_outcome <- f_outcome[,c("ID", paste("savings", 0:25, sep = "_"))]
sel_out_lg <- gather(sel_outcome, year_, savings, savings_0:savings_25,
                     factor_key=TRUE)
sel_out_lg$year_ <- as.character(sel_out_lg$year_)
sel_out_lg$year <- as.numeric(substring(sel_out_lg$year_, 
                                        regexpr("_", sel_out_lg$year_) + 1, 
                                        nchar(sel_out_lg$year_)))

sel_out_lg <- sel_out_lg[,c("ID", "year", "savings")]

# Then join both long formats
mems_savs_long <- merge(mems_long, sel_out_lg)

# Finally save as csv
write.csv(mems_savs_long, "OUTPUTDATA/POST TABLES/mems_savs.csv", 
          row.names=FALSE)

# The other model inputs the number of rooms
# Calculates the overcrowding factor
# Then it gets the need
# Calculates accumulated savings
# Calculates if the available means cover the need
# Determines if loan and construction
# Thus it ends with year, agent, number of rooms.

#####
# Financial/construction simulation
#####
# General inputs
need_thld <- c(-Inf, 0.5, 1, Inf) # To categorize need in urgent, need, no need
need_ctgs <- c("Urgent", "Need", "No Need")
meansavingsT0 <- mean(no_outliers(f_outcome$savings_0)) # Mean of savings 2015
loan_cap <- 4 # Fraction of the (disposable) income that a HH can ask as loan
interest <- 0.15 # Annual interest rate for loans
noneed_thr <- 0.01 # Percentage of the savings needed to build when "no need" category builds anyways
#max_rpy_yrs <- 10 # Max number of years in which the loan can be repaid
invs_max <- 0.6 # max percentage of the total savings to be invested in extension
# also, max percentage of the total savings used to cancel a debt

# You could define a minimum annual repayment amount, so no all debts are divided
# among the max repayment years

# Set starting variables (T0)
f_outcome$dw_size_0 <- 2 # Initial size of the dwelling
f_outcome$loan_tp_0 <- 0 # Assume that all have no previous loan to pay
f_outcome$acc_sav_0 <- f_outcome$savings_0
f_outcome$acc_rpy_0 <- 0

# Building costs
#f_outcome$bld_cst_0 <- 20000 # Original test
#f_outcome$bld_cst_0 <- 5345.48 # Coast 2015
#f_outcome$bld_cst_0 <- 5271.58 # Highlands 2015
#f_outcome$bld_cst_0 <- 6148.75 # Amazon 2015
f_outcome$bld_cst_0 <- 10120 # Check annex

#inflt <- 0.05 # Legacy setting
inflt <- 0.028 # Average inflation of direct construction cost

full_sim <- NA
full_sim <- f_outcome

# Simulate for every year
for (i in 1:nr_yrs){
  # Take from the economic demographic model
  hhm_tn <- paste("hhmem", i, sep = "_")
  savs_tn <- paste("savings", i, sep = "_") 
  inc_tn <- paste("hh_inc", i, sep = "_")
  
  # Update accumulated values from the past
  acc_savs_1tn <- paste("acc_sav", i - 1, sep = "_") 
  acc_savs_tn <- paste("acc_sav", i, sep = "_")
  
  #acc_yrpy_1tn <- paste("acc_rpy", i - 1, sep = "_")
  #acc_yrpy_tn <- paste("acc_rpy", i, sep = "_")
  
  # Update financial variables from the past
  bld_cost_1tn <- paste("bld_cst", i - 1, sep = "_")
  bld_cost_tn <- paste("bld_cst", i, sep = "_")
  
  loan_tp_1tn <- paste("loan_tp", i - 1, sep = "_")
  loan_tp_tn <- paste("loan_tp", i, sep = "_")
  
  # Model gets
  occ_rat_tn <- paste("occ_rat", i, sep = "_")
  need_tn <- paste("need", i, sep = "_")
  rpy_cp_tn <- paste("rpy", i, sep = "_")
  bld_cap_tn <- paste("bld_cap", i, sep = "_")
  
  # Loop generates future values
  dwsize_1tn <- paste("dw_size", i - 1, sep = "_")
  dwsize_tn <- paste("dw_size", i, sep = "_")
  
  ####
  full_sim[[dwsize_tn]] <- full_sim[[dwsize_1tn]] # By default
  
  # Update financial variables from the past
  full_sim[[bld_cost_tn]] <- full_sim[[bld_cost_1tn]] + 
    (full_sim[[bld_cost_1tn]] * inflt)
  
  full_sim[[loan_tp_tn]] <- full_sim[[loan_tp_1tn]] + 
    (full_sim[[loan_tp_1tn]] * interest)
  
  #full_sim[[acc_yrpy_tn]] <- full_sim[[acc_yrpy_1tn]]
  
  # Update accumulated savings
  full_sim[[acc_savs_tn]] <- full_sim[[acc_savs_1tn]] + full_sim[[savs_tn]]
  
  # Model gets
  # occupancy ratio
  full_sim[[occ_rat_tn]] <- full_sim[[dwsize_tn]]/full_sim[[hhm_tn]]
  
  # Current need
  full_sim[[need_tn]] <- cut(full_sim[[occ_rat_tn]], need_thld, need_ctgs)
  
  # Modify variables according to assumed behaviour
  # Repay existing debt with positive savings according to max investment ratio
  #full_sim[[acc_savs_tn]] <- ifelse(full_sim[[acc_savs_tn]] * invs_max >= full_sim[[loan_tp_tn]],
                                    #(full_sim[[acc_savs_tn]] * invs_max) - full_sim[[loan_tp_tn]],
                                    #full_sim[[acc_savs_tn]])
  
  #full_sim[[loan_tp_tn]] <- ifelse(full_sim[[acc_savs_tn]] * invs_max >= full_sim[[loan_tp_tn]],
  #0,
  #full_sim[[loan_tp_tn]])
  
  # Repay existing debt when positive savings according to max investment ratio
  full_sim[[acc_savs_tn]] <- ifelse(full_sim[[acc_savs_tn]] > 0 & 
                                      full_sim[[loan_tp_tn]] > 0, 
                                    ifelse(full_sim[[acc_savs_tn]] * invs_max >= full_sim[[loan_tp_tn]], 
                                           (full_sim[[acc_savs_tn]] * invs_max) - full_sim[[loan_tp_tn]], 
                                           0), full_sim[[acc_savs_tn]])
  
  full_sim[[loan_tp_tn]] <- ifelse(full_sim[[acc_savs_1tn]] + full_sim[[savs_tn]] > 0 & 
                                     full_sim[[loan_tp_tn]] > 0, 
                                   ifelse((full_sim[[acc_savs_1tn]] + full_sim[[savs_tn]]) * invs_max >= full_sim[[loan_tp_tn]], 
                                          0, full_sim[[loan_tp_tn]] - ((full_sim[[acc_savs_1tn]] + full_sim[[savs_tn]]) * invs_max)),
                                   full_sim[[loan_tp_tn]])
  
  
  #full_sim[[acc_yrpy_tn]] <- ifelse(full_sim[[loan_tp_tn]] < 1, 
                                    #0, full_sim[[acc_yrpy_tn]])
  
  # Pay agreed annual repayment if enough accumulated savings
  #full_sim[[acc_savs_tn]] <- ifelse(full_sim[[acc_savs_tn]] >= full_sim[[acc_yrpy_tn]],
                                    #full_sim[[acc_savs_tn]] - full_sim[[acc_yrpy_tn]],
                                    #full_sim[[acc_savs_tn]])
  
  #full_sim[[loan_tp_tn]] <- ifelse(full_sim[[acc_savs_tn]] >= full_sim[[acc_yrpy_tn]],
                                   #full_sim[[loan_tp_tn]] - full_sim[[acc_yrpy_tn]],
                                   #full_sim[[loan_tp_tn]])
  
  # Get current repayment capacity (a fraction of gross income - debts)
  # Based on this years income
  #full_sim[[rpy_cp_tn]] <- (full_sim[[inc_tn]] - full_sim[[acc_yrpy_tn]]) * loan_cap
  
  # Based on this year Balance
  full_sim[[rpy_cp_tn]] <- full_sim[[savs_tn]] * loan_cap
  
  # Based on this years accumulated savings
  #full_sim[[rpy_cp_tn]] <- (full_sim[[acc_savs_tn]] - full_sim[[acc_yrpy_tn]]) * loan_cap
  
  full_sim[[rpy_cp_tn]] <- ifelse(full_sim[[rpy_cp_tn]] > 0, 
                                  full_sim[[rpy_cp_tn]], 0)
  
  # Get building economic capacity Boolean
  full_sim[[bld_cap_tn]] <- ifelse(full_sim[[acc_savs_tn]] * invs_max >=
                                   full_sim[[bld_cost_tn]],
                                   "True", "False")
  
  # After paying my debts, I determine if I still have enough money to build
  # Determine simple construction activity
  full_sim[[dwsize_tn]] <- ifelse(full_sim[[bld_cap_tn]] == "True" &
                                     full_sim[[need_tn]] == "Urgent",
                                   full_sim[[dwsize_1tn]] + 1, 
                                   full_sim[[dwsize_tn]]) # When urgent and enough money
  
  full_sim[[dwsize_tn]] <- ifelse(full_sim[[bld_cap_tn]] == "True" &
                                     full_sim[[need_tn]] == "Need",
                                   full_sim[[dwsize_1tn]] + 1, 
                                   full_sim[[dwsize_tn]]) # When need and enough money
  
  full_sim[[dwsize_tn]] <- ifelse(full_sim[[acc_savs_tn]] * noneed_thr >
                                     full_sim[[bld_cost_tn]] &
                                    full_sim[[occ_rat_tn]] <= 2, # & ratio < 2
                                   full_sim[[dwsize_1tn]] + 1, 
                                   full_sim[[dwsize_tn]]) # When no need and above threshold
  
  # Take money from the accumulated savings of those that build without loan
  full_sim[[acc_savs_tn]] <- ifelse(full_sim[[dwsize_tn]] > full_sim[[dwsize_1tn]],
                                    full_sim[[acc_savs_tn]] - full_sim[[bld_cost_tn]],
                                    full_sim[[acc_savs_tn]])
  
  # Determine loan-based construction activity
  full_sim[[dwsize_tn]] <- ifelse(full_sim[[bld_cap_tn]] == "False" &
                                     full_sim[[need_tn]] == "Urgent" &
                                    full_sim[[acc_savs_tn]] > 0 &
                                    (full_sim[[acc_savs_tn]] * invs_max) + # investment capacity
                                    full_sim[[rpy_cp_tn]] >=  # debt capacity
                                     full_sim[[bld_cost_tn]], # building cost
                                   full_sim[[dwsize_1tn]] + 1, # Build one room
                                   full_sim[[dwsize_tn]]) # else, keep previous value
  
  # Add loan amount record to those that have asked for it
  full_sim[[loan_tp_tn]] <- ifelse(full_sim[[bld_cap_tn]] == "False" &
                                      full_sim[[need_tn]] == "Urgent" &
                                     full_sim[[acc_savs_tn]] > 0 &
                                     (full_sim[[acc_savs_tn]] * invs_max) + # investment capacity
                                     full_sim[[rpy_cp_tn]] >=  # debt capacity
                                     full_sim[[bld_cost_tn]], # building cost
                                    full_sim[[loan_tp_tn]] + # previous debts
                                      (full_sim[[bld_cost_tn]] - 
                                         (full_sim[[acc_savs_tn]]*invs_max)), # Loan as much as needed
                                    full_sim[[loan_tp_tn]]) # Keep previous value if false
  
  # Add current annual repayment to accumulated annual repayments
  #full_sim[[acc_yrpy_tn]] <- ifelse(full_sim[[bld_cap_tn]] == "False" &
                                      #full_sim[[need_tn]] == "Urgent" &
                                      #full_sim[[acc_savs_tn]] > 0 &
                                      #(full_sim[[acc_savs_tn]] * invs_max) + # investment capacity
                                      #full_sim[[rpy_cp_tn]] >=  # debt capacity
                                      #full_sim[[bld_cost_tn]], # building cost
                                    #full_sim[[acc_yrpy_tn]] + # previous values
                                      #(full_sim[[rpy_cp_tn]] * # Debt capacity
                                         #interest), # repayment years
                                    #full_sim[[acc_yrpy_tn]])
  
  # Just bring debt to present
  # and compare with 0.6 of current savings
  # if this is a positive number, pay that part of the debt
}

#####
# Export test result for loans in CSV
#####
# Export the full table
write.csv(full_sim, "OUTPUTDATA/POST TABLES/fulltable_loans.csv", 
          row.names=FALSE)

# Select relevant variables
slct_fvars <- c("ID", paste("dw_size", 0:25, sep = "_"))
slct_fsim <- full_sim[,slct_fvars]

# From wide to long
f_sim_long <- gather(slct_fsim, year_, dw_size, dw_size_0:dw_size_25, factor_key=TRUE)
f_sim_long$year_ <- as.character(f_sim_long$year_)
f_sim_long$year <- as.numeric(sub(".*([A-Za-z]+_[A-Za-z]+_)", "\\2", 
                                  f_sim_long$year_))

f_sim_long <- f_sim_long[,c("ID", "year", "dw_size")]

# Save as csv
write.csv(f_sim_long, "OUTPUTDATA/POST TABLES/years_dwsize.csv", 
          row.names=FALSE)

#####
# Graph results
#####
plt_h = 700 # Height
plt_w = 1200 # Width
plt_res = 100 # Resolution

# Graph means of income per generator and expenses per member
n_plt <- plt_my(all_means, c(2015, 2019), "income_pg", T)
m_plt <- plt_my(all_means, c(2015, 2019), "exp_pm", T)

ggplot(data=all_means, 
       aes(x=year, y=income_pg, group=sel, color = sel)) +
  geom_line()+
  geom_point() +
  geom_line(aes(y = exp_pm, color = sel), linetype = "longdash")

# Graph projection of income per generator and expenses per member
p_plt <- plt_my(pred_inc, c(2015, 2040), "income_pg", F)
q_plt <- plt_my(pred_exp, c(2015, 2040), "exp_pm", F)

# wide to long
lng_predinc <- gather(pred_inc, sel, avg_incpg, B:D, factor_key=TRUE)
lng_predexp <- gather(pred_exp, sel, avg_exppc, B:D, factor_key=TRUE)
incexp_proj <- merge(lng_predinc, lng_predexp)
names(incexp_proj) <- c("Year", "SEL", "avg_incpg", "avg_exppc")

# Make sure the strings for line categories are the same in the aes and scale_linetype
jpeg(file="OUTPUTDATA/PRE GRAPHS/PROJ_INCEXP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(data=incexp_proj, 
       aes(x=Year, y=avg_incpg, group=SEL, color = SEL, linetype = "Average rel. Inc.")) +
  geom_line() +
  geom_point(aes(shape=SEL)) +
  geom_line(aes(y = avg_exppc, color = SEL, linetype = "Average rel. Exp.")) +
  geom_point(aes(y = avg_exppc, color = SEL, shape=SEL)) +
  scale_linetype_manual("Type",values=c("Average rel. Inc."="solid","Average rel. Exp."="dotted")) +
  labs (title="Projection of income per income generator and expenses per capita
by SEL (2015-2040)",
        x = "Year",
        y = "Soles (s/)")
dev.off()

# members and income generating members
drop_id <- dw_pop[,-1]
# HH mem
matplot(t(drop_id[,!names(drop_id) %in% inc_vars]),type="l")
# Income generators
matplot(t(drop_id[,names(drop_id) %in% inc_vars]),type="l")

# Dw size
vars_sel <- paste("dw_size", 0:25, sep = "_")
my_ag2 <- full_sim[,vars_sel]

jpeg(file = "dw_size.jpeg", width = plt_w, height = plt_h, res = plt_res)
matplot(t(my_ag2),type="l", xlab="Years",ylab="Number of Bedrooms", 
             main="Number of bedrooms per dwelling")
dev.off()


# Accumulated debt
vars_sel <- paste("loan_tp", 0:25, sep = "_")
my_ag3 <- full_sim[,vars_sel] # negative agents are 8, 9

jpeg(file = "acc_debt.jpeg", width = plt_w, height = plt_h, res = plt_res)
matplot(t(my_ag3),type="l", xlab="Years",ylab="Accumulated debt (in PEN)", 
        main="Accumulated debt per agent")
dev.off()

# Accumulated savings
vars_sel <- paste("savings", 0:25, sep = "_")
my_ag4 <- full_sim[,vars_sel]

jpeg(file = "acc_savings.jpeg", width = plt_w, height = plt_h, res = plt_res)
matplot(t(my_ag4),type="l", xlab="Years",ylab="Accumulated savings (in PEN)", 
        main="Accumulated savings per agent")
dev.off()

# Occupancy ratios
vars_sel <- paste("occ_rat", 1:25, sep = "_")
my_ag5 <- full_sim[,vars_sel]

jpeg(file = "ind_occupancy_ratios.jpeg", width = plt_w, height = plt_h, res = plt_res)
matplot(t(my_ag5),type="l", xlab="Years",ylab="Occupancy ratios (in rooms/people)", 
        main="Occupancy ratios per dwelling")
dev.off()

# NEED
nds <- paste("need", 1:25, sep = "_")
a_nds <- full_sim[,nds]

all_cnt <- data.frame(matrix(nrow = 0, ncol = 3))
names(all_cnt) <- c("cats", "cnt", "year")
for (i in 1:25){
  curr_cnt <- as.data.frame(table(a_nds[,i]))
  names(curr_cnt) <- c("cats", "cnt")
  curr_cnt$year <- i
  all_cnt <- rbind(all_cnt, curr_cnt)
}

all_cnt <- all_cnt[all_cnt$cnt != 0,]

jpeg(file = "need_evolution.jpeg", width = plt_w, height = plt_h, res = plt_res)
ggplot(all_cnt, aes(year, cats)) +
  geom_point(data = all_cnt, aes(size = cnt), colour = "red") + 
  xlab("Year") + ylab("Categories") + ggtitle("Evolution of the 'need' categories") +
  guides(size=guide_legend(title="Number\nof agents"))
dev.off()

# Average number of rooms per year by SEL
dw_szs <- paste("dw_size", 0:25, sep ="_")
dw_szs <- c(dw_szs, "SEL")
a_szs <- full_sim[,dw_szs]

#dw_nor <- paste("dw_size", 0:25, sep ="_")
#colMeans(full_sim[,dw_nor]) # AVERAGE OCCUPANTS/BEDROOMS

all_szs <- data.frame(matrix(nrow = 0, ncol = 3))
names(all_szs) <- c("SEL", "avg", "year")

for (i in 1:26){
  for (j in seq(SELs)){
    my_inter <- a_szs[a_szs$SEL == SELs[j],]
    avg <- mean(my_inter[,i])
    curr_df <- data.frame(matrix(nrow = 1, ncol = 3))
    names(curr_df) <- c("SEL", "avg", "year")
    curr_df$SEL <- SELs[j]
    curr_df$avg <- avg
    curr_df$year <- i
    all_szs <- rbind(all_szs, curr_df)
  }
}

jpeg(file = "Average_rooms_sel.jpeg", width = plt_w, height = plt_h, res = plt_res)
ggplot(all_szs, aes(year, avg)) +
  geom_line(data = all_szs, aes(colour = SEL)) + 
  xlab("Year") + ylab("Average rooms") + ggtitle("Average number of rooms by socio economic level")
dev.off()


# Average occupancy ratio by SEL
dw_or <- paste("occ_rat", 1:25, sep ="_")
dw_or <- c(dw_or, "SEL")
df_or <- full_sim[,dw_or]

dw_nor <- paste("occ_rat", 1:25, sep ="_")
colMeans(full_sim[,dw_nor]) # AVERAGE OCCUPANTS/BEDROOMS

all_szs <- data.frame(matrix(nrow = 0, ncol = 3))
names(all_szs) <- c("SEL", "avg", "year")

for (i in 1:25){
  for (j in seq(SELs)){
    my_inter <- df_or[df_or$SEL == SELs[j],]
    avg <- mean(my_inter[,i])
    curr_df <- data.frame(matrix(nrow = 1, ncol = 3))
    names(curr_df) <- c("SEL", "avg", "year")
    curr_df$SEL <- SELs[j]
    curr_df$avg <- avg
    curr_df$year <- i
    all_szs <- rbind(all_szs, curr_df)
  }
}

jpeg(file = "Average_occupancy_sel.jpeg", width = plt_w, height = plt_h, res = plt_res)
ggplot(all_szs, aes(year, avg)) +
  geom_line(data = all_szs, aes(colour = SEL)) + 
  xlab("Year") + ylab("Occupancy ratio (rooms/people)") +
  ggtitle("Average occupancy ratio by Socio Economic level")
dev.off()


# Average debt by SEL
dbt <- paste("loan_tp", 1:25, sep = "_")
dbt <- c(dbt, "SEL")
a_dbt <- full_sim[,dbt]

all_dbt <- data.frame(matrix(nrow = 0, ncol = 3))
names(all_dbt) <- c("SEL", "avg", "year")

for (i in 1:25){
  for (j in seq(SELs)){
    my_inter <- a_dbt[a_dbt$SEL == SELs[j],]
    avg <- mean(my_inter[,i])
    curr_df <- data.frame(matrix(nrow = 1, ncol = 3))
    names(curr_df) <- c("SEL", "avg", "year")
    curr_df$SEL <- SELs[j]
    curr_df$avg <- avg
    curr_df$year <- i
    all_dbt <- rbind(all_dbt, curr_df)
  }
}

jpeg(file = "Average_debt_sel.jpeg", width = plt_w, height = plt_h, res = plt_res)
ggplot(all_dbt, aes(year, avg)) +
  geom_line(data = all_dbt, aes(colour = SEL)) + 
  xlab("Year") + ylab("Average Accumulated debt (PEN)") +
  ggtitle("Average Accumulated debt by Socio Economic level")
dev.off()

# Average acc savs by SEL
savv <- paste("acc_sav", 1:25, sep = "_")
savv <- c(savv, "SEL")
a_sav <- full_sim[,savv]

all_sav <- data.frame(matrix(nrow = 0, ncol = 3))
names(all_sav) <- c("SEL", "avg", "year")

for (i in 1:25){
  for (j in seq(SELs)){
    my_inter <- a_sav[a_sav$SEL == SELs[j],]
    avg <- mean(my_inter[,i])
    curr_df <- data.frame(matrix(nrow = 1, ncol = 3))
    names(curr_df) <- c("SEL", "avg", "year")
    curr_df$SEL <- SELs[j]
    curr_df$avg <- avg
    curr_df$year <- i
    all_sav <- rbind(all_sav, curr_df)
  }
}

jpeg(file = "Average_accsavs_sel.jpeg", width = plt_w, height = plt_h, res = plt_res)
ggplot(all_sav, aes(year, avg)) +
  geom_line(data = all_sav, aes(colour = SEL))+ 
  xlab("Year") + ylab("Average Accumulated savings (PEN)") +
  ggtitle("Average Accumulated savings by Socio Economic level")
dev.off()

#####
# HERE START THE GRAPHS OF THE SOCIO-ECONOMIC MODEL
#####

# Average HH members by SEL
hm_m <- paste("hhmem", 0:25, sep = "_")
hm_m <- c(hm_m, "SEL")
a_hm <- full_sim[,hm_m]

all_hm <- data.frame(matrix(nrow = 0, ncol = 3))
names(all_hm) <- c("SEL", "avg", "year")

for (i in 1:25){
  for (j in seq(SELs)){
    my_inter <- a_hm[a_hm$SEL == SELs[j],]
    avg <- mean(my_inter[,i])
    curr_df <- data.frame(matrix(nrow = 1, ncol = 3))
    names(curr_df) <- c("SEL", "avg", "year")
    curr_df$SEL <- SELs[j]
    curr_df$avg <- avg
    curr_df$year <- i
    all_hm <- rbind(all_hm, curr_df)
  }
}

sum(a_hm$hhmem_25)

jpeg(file="OUTPUTDATA/PRE GRAPHS/out_avg_members_sel.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(all_hm, aes(year, avg)) +
  geom_line(data = all_hm, aes(colour = SEL))+
  geom_point(aes(shape = SEL, colour = SEL))+
  labs (title="Average Number of household members by SEL",
        x = "Year of simulation",
        y = "Number of Household members")
dev.off()

# Average Income generators
ic_m <- paste("inc_gen", 0:25, sep = "_")
ic_m <- c(ic_m, "SEL")
ic_hm <- results[,ic_m]

all_ic <- data.frame(matrix(nrow = 0, ncol = 3))
names(all_ic) <- c("SEL", "avg", "year")

for (i in 1:25){
  for (j in seq(SELs)){
    my_inter <- ic_hm[ic_hm$SEL == SELs[j],]
    avg <- mean(my_inter[,i])
    curr_df <- data.frame(matrix(nrow = 1, ncol = 3))
    names(curr_df) <- c("SEL", "avg", "year")
    curr_df$SEL <- SELs[j]
    curr_df$avg <- avg
    curr_df$year <- i
    all_ic <- rbind(all_ic, curr_df)
  }
}


ggplot(all_ic, aes(year, avg)) +
  geom_line(data = all_ic, aes(colour = SEL))+
  geom_point(aes(shape = SEL, colour = SEL))+
  labs (title="Average Number of income generators per household by SEL",
        x = "Year of simulation",
        y = "Number of Income generators")


# Average proportion of income generators/total memebers
ig_and_mem <- c(hm_m, ic_m)
ig_and_mem <- ig_and_mem[!duplicated(ig_and_mem )]

ig_hmem <- results[,ig_and_mem]

for (i in 0:25){
  thisyvr <- paste("hhmem", i, sep = "_")
  thisyvr2 <- paste("inc_gen", i, sep = "_")
  thisyvr3 <- paste("ratio", i, sep = "_")
  
  ig_hmem[[thisyvr3]] <- ig_hmem[[thisyvr2]] / ig_hmem[[thisyvr]]
}

only_rats <- paste("ratio", 0:25, sep = "_")
only_rats <- c(only_rats, "SEL")
rats_df <- ig_hmem[,only_rats]

all_rat <- data.frame(matrix(nrow = 0, ncol = 3))
names(all_rat) <- c("SEL", "avg", "year")

for (i in 1:25){
  for (j in seq(SELs)){
    my_inter <- rats_df[rats_df$SEL == SELs[j],]
    avg <- mean(my_inter[,i])
    curr_df <- data.frame(matrix(nrow = 1, ncol = 3))
    names(curr_df) <- c("SEL", "avg", "year")
    curr_df$SEL <- SELs[j]
    curr_df$avg <- avg
    curr_df$year <- i
    all_rat <- rbind(all_rat, curr_df)
  }
}

jpeg(file="OUTPUTDATA/PRE GRAPHS/out_ratio_incgens_sel.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(all_rat, aes(year, avg)) +
  geom_line(data = all_rat, aes(colour = SEL))+
  geom_point(aes(shape = SEL, colour = SEL))+
  labs (title="Average Ratio income generators/total HH members by SEL",
        x = "Year of simulation",
        y = "Ratio Income generators/total HH members")
dev.off()

# Average disposable income (savings) by SEL
savs_m <- paste("savings", 0:25, sep = "_")
savs_m <- c(savs_m, "SEL")
a_savs <- full_sim[,savs_m]

all_sv <- data.frame(matrix(nrow = 0, ncol = 3))
names(all_sv) <- c("SEL", "avg", "year")

for (i in 1:25){
  for (j in seq(SELs)){
    my_inter <- a_savs[a_savs$SEL == SELs[j],]
    avg <- mean(my_inter[,i])
    curr_df <- data.frame(matrix(nrow = 1, ncol = 3))
    names(curr_df) <- c("SEL", "avg", "year")
    curr_df$SEL <- SELs[j]
    curr_df$avg <- avg
    curr_df$year <- i
    all_sv <- rbind(all_sv, curr_df)
  }
}

jpeg(file="OUTPUTDATA/PRE GRAPHS/out_yearly_savings_sel.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(all_sv, aes(year, avg)) +
  geom_line(data = all_sv, aes(colour = SEL))+
  geom_point(aes(shape = SEL, colour = SEL))+
  labs (title="Average yearly savings per household by SEL",
        x = "Year of simulation",
        y = "Savings in Soles (s/)")
dev.off()

# Savings boxplots by sel


# Age pyramid for T0 and T25
#t0
pir_vars <- c("gender", "age_cat", "count")
pir_t0 <- as.data.frame(table(state_1[!is.na(state_1$hh_id_0),]$gender, 
                              state_1[!is.na(state_1$hh_id_0),]$age_cat_0))
names(pir_t0) <- pir_vars
pir_t0$gender <- ifelse(pir_t0$gender == "hombre", "Male", "Female")

#t10
pir_t10 <- as.data.frame(table(state_1[!is.na(state_1$hh_id_10),]$gender, 
                              state_1[!is.na(state_1$hh_id_10),]$age_cat_10))
names(pir_t10) <- pir_vars
pir_t10$gender <- ifelse(pir_t10$gender == "hombre", "Male", "Female")

#t20
pir_t20 <- as.data.frame(table(state_1[!is.na(state_1$hh_id_20),]$gender, 
                               state_1[!is.na(state_1$hh_id_20),]$age_cat_20))
names(pir_t20) <- pir_vars
pir_t20$gender <- ifelse(pir_t20$gender == "hombre", "Male", "Female")

#t25
pir_t25 <- as.data.frame(table(state_1[!is.na(state_1$hh_id_25),]$gender, 
                               as.numeric(as.character(state_1[!is.na(state_1$hh_id_25),]$age_cat_25))))
names(pir_t25) <- pir_vars
pir_t25$gender <- ifelse(pir_t25$gender == "hombre", "Male", "Female")

# t0
jpeg(file="OUTPUTDATA/PRE GRAPHS/out_pyr_t0.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pir_t0 %>% mutate(
  count = ifelse(gender == "Male", count*(-1),
                 count*1)) %>%
  ggplot(aes(x = age_cat, y = count, fill=gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-20,20),
                     breaks = seq(-20, 20, by = 5)) +
  labs(title = "Neighbourhood population at T0", x = "Age Categories",
     y = "Population") +
  guides(fill=guide_legend(title="Gender"))
dev.off()

pir_t10 %>% mutate(
  count = ifelse(gender == "Male", count*(-1),
                 count*1)) %>%
  ggplot(aes(x = age_cat, y = count, fill=gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-20,20),
                     breaks = seq(-20, 20, by = 5)) +
  labs(title = "Neighbourhood population at T10", x = "Age Categories",
       y = "Population") +
  guides(fill=guide_legend(title="Gender"))

pir_t20 %>% mutate(
  count = ifelse(gender == "Male", count*(-1),
                 count*1)) %>%
  ggplot(aes(x = age_cat, y = count, fill=gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-20,20),
                     breaks = seq(-20, 20, by = 5)) +
  labs(title = "Neighbourhood population at T20", x = "Age Categories",
       y = "Population") +
  guides(fill=guide_legend(title="Gender"))

jpeg(file="OUTPUTDATA/PRE GRAPHS/out_pyr_t25.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pir_t25 %>% mutate(
  count = ifelse(gender == "Male", count*(-1),
                 count*1)) %>%
  ggplot(aes(x = age_cat, y = count, fill=gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-20,20),
                     breaks = seq(-20, 20, by = 5)) +
  labs(title = "Neighbourhood population at T25", x = "Age Categories",
       y = "Population") +
  guides(fill=guide_legend(title="Gender"))
dev.off()

# Total population evolution
pop_ysel <- as.data.frame(matrix(ncol = 3, nrow = 0))
names(pop_ysel) <- c("Year", "SEL", "Pop")

for (ss in seq(SELs)){
  for (yz in 0:nr_yrs){
    pop_y <- as.data.frame(matrix(ncol = 3, nrow = 1))
    names(pop_y) <- c("Year", "SEL", "Pop")
    
    hhm_thisy <- paste("hhmem", yz, sep = "_")
    pop_y$Pop <- sum(full_sim[full_sim$SEL == SELs[ss],][[hhm_thisy]])
    
    pop_y$Year <- yz
    pop_y$SEL <- SELs[ss]
    
    pop_ysel <- rbind(pop_ysel, pop_y)
  }
}

jpeg(file="OUTPUTDATA/PRE GRAPHS/out_popevol_sel.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(pop_ysel, aes(Year, Pop)) +
  geom_line(data = pop_ysel, aes(colour = SEL))+
  geom_point(aes(shape = SEL, colour = SEL))+
  labs (title="Population evolution by SEL",
        x = "Year of simulation",
        y = "Population")
dev.off()

# Distribution of per capita income and expenses classes by HH
selected_ag <- paste("Inc_cat", 0:25, sep = "_")
selected_ag <- c(selected_ag, "SEL")

to_incomecl <- my_agents[,selected_ag]
to_incomecl$Inc_cat_0 <- as.factor(to_incomecl$Inc_cat_0)

to_inc_long <- gather(to_incomecl, Year, Income_Cat, 
                      Inc_cat_0:Inc_cat_25, factor_key=TRUE)
to_inc_long$year <- as.numeric(sub(".*([A-Za-z]+_[A-Za-z]+_)", "\\2", 
                                   to_inc_long$Year))

jpeg(file="OUTPUTDATA/PRE GRAPHS/out_incomecat_sel.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(to_inc_long, aes(year, Income_Cat)) +
  geom_point(aes(colour = SEL))+
  geom_count(aes(colour = SEL))+
  facet_grid(vars(SEL))+
  labs (title="Number of households by income and SEL categories",
        x = "Year of simulation",
        y = "Income Category")
dev.off()