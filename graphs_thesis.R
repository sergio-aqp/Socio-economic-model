"'This file graphs the information contained in the base datasets
before the construction of the model '"

# Essential packages
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

# Set working directory
setwd("U:/THESIS SCRIPTING/R/")

# Graphic inputs
plt_h = 700 # Height
plt_w = 1200 # Width
plt_res = 100 # Resolution

#####
# IMPORT FILTERED DATASETS
#####
# Household level Non-panel data SUMARIA 2015-2019
filen <- list("INPUT DATA/FILTERED/NON-PANEL/SUMARIA_2015.Rds", 
              "INPUT DATA/FILTERED/NON-PANEL/SUMARIA_2016.Rds",
              "INPUT DATA/FILTERED/NON-PANEL/SUMARIA_2017.Rds", 
              "INPUT DATA/FILTERED/NON-PANEL/SUMARIA_2018.Rds",
              "INPUT DATA/FILTERED/NON-PANEL/SUMARIA_2019.Rds")
filt <- lapply(filen, readRDS)

# Household level Panel data SUMARIA 2015-2019
hhs <- readRDS(file="INPUT DATA/FILTERED/PANEL/sumaria-2015-2019-panel.Rds")

# Individual level non-panel module 500 (income generator status)
ind_dta <- readRDS(file="INPUT DATA/FILTERED/NON-PANEL/Enaho01A-2015-500.Rds")

# Individual level non-panel module 200 (general information)
genage_dta <- readRDS(file="INPUT DATA/FILTERED/NON-PANEL/Enaho01-2015-200.Rds")

# Individual level panel data module 500 (transition probs income generator status)
igen_dta <- readRDS(file="INPUT DATA/FILTERED/PANEL/enaho01-2015-2019-500-panel_01.Rds")

# Individual level panel data module 200 (move-out event probs)
my_indv <- readRDS(file="INPUT DATA/FILTERED/PANEL/enaho01-2015-2019-200-panel.Rds")



#####
# Prepare household level data to Plot
#####

# Number of participants per year per SEL in non panel data
daf <- as.data.frame(matrix(ncol = 3, nrow = 0))
names(daf) <- c("Var1", "Freq", "df")
for (i in 1:length(filt)){
  adsd <- as.data.frame(table(filt[[i]]$ESTRSOCIAL))
  adsd$df <- i + 2014
  daf <- rbind(daf, adsd)
}

names(daf) <- c("SEL", "Households", "Year")

# Number of household members in non-panel data
nrr_mems <- as.data.frame(matrix(ncol = 3, nrow = 0))
names(nrr_mems) <- c("Nr_mem", "SEL", "Year")

for (i in 1:length(filt)){
  r_mem <- as.data.frame(matrix(ncol = 3, nrow = nrow(filt[[i]])))
  names(r_mem) <- c("Nr_mem", "SEL", "Year")
  r_mem$Nr_mem <- filt[[i]]$MIEPERHO
  r_mem$SEL <- filt[[i]]$ESTRSOCIAL
  r_mem$Year <- 2014 + i
  nrr_mems <- rbind(nrr_mems, r_mem)
}

# Number of household members in panel data
hhs_2 <- hhs[c("mieperho_15", "mieperho_16", "estrsocial_15")]
hhs_3 <- gather(hhs_2, Year, Nr_mem, mieperho_15:mieperho_16, 
                factor_key=TRUE)
hhs_3$Year <- ifelse(hhs_3$Year == "mieperho_15", 2015, 2016)

# Income expenses and yearly balance FOR NON PANEL DATA
ie_df <- as.data.frame(matrix(ncol = 4, nrow = 0))
names(ie_df) <- c("Income", "Expenses", "SEL", "Year")

for (i in 1:length(filt)){
  r_mem <- as.data.frame(matrix(ncol = 4, nrow = nrow(filt[[i]])))
  names(r_mem) <- c("Income", "Expenses", "SEL", "Year")
  r_mem$SEL <- filt[[i]]$ESTRSOCIAL
  r_mem$Income <- filt[[i]]$INGMO2HD
  r_mem$Expenses <- filt[[i]]$GASHOG1D
  r_mem$Year <- 2014 + i
  ie_df <- rbind(ie_df, r_mem)
}

ie_df$Balance <- ie_df$Income - ie_df$Expenses

# Income, expenses and balance for every year on panel data
hhs_4 <- hhs[c("ingmo2hd_15", "ingmo2hd_16",  "estrsocial_15")]
hhs_5 <- gather(hhs_4, Year, income, ingmo2hd_15:ingmo2hd_16, 
                factor_key=TRUE)
hhs_5$Year <- ifelse(hhs_5$Year == "ingmo2hd_15", 2015, 2016)

hhs_6 <- hhs[c("gashog1d_15", "gashog1d_16",  "estrsocial_15")]
hhs_6b <- gather(hhs_6, Year_2, expenses, gashog1d_15:gashog1d_16, 
                 factor_key=TRUE)
hhs_6b$Year_2 <- ifelse(hhs_6b$Year_2 == "gashog1d_15", 2015, 2016)

hhs_6c <- merge(hhs_5, hhs_6b, by = 'row.names', 
                all = TRUE)[c("estrsocial_15.x", "Year", 
                              "income", "expenses")]
names(hhs_6c) <- c("estrsocial_15", "Year", 
                   "income", "expenses")

hhs_6c$Balance <- hhs_6c$income - hhs_6c$expenses

# Number of income generating members vs total members in non-panel data
mems_df <- as.data.frame(matrix(ncol = 4, nrow = 0))
names(mems_df) <- c("HH_members", "Inc_generator", "SEL", "Year")

for (i in 1:length(filt)){
  r_mem <- as.data.frame(matrix(ncol = 4, nrow = nrow(filt[[i]])))
  names(r_mem) <- c("HH_members", "Inc_generator", "SEL", "Year")
  r_mem$SEL <- filt[[i]]$ESTRSOCIAL
  r_mem$HH_members <- filt[[i]]$MIEPERHO
  r_mem$Inc_generator <- filt[[i]]$PERCEPHO
  r_mem$Year <- 2014 + i
  mems_df <- rbind(mems_df, r_mem)
}

mems_df$Ratio <- mems_df$Inc_generator / mems_df$HH_members

# Number of income generating members vs total members in panel data
hhs_7 <- hhs[c("mieperho_15", "mieperho_16", "estrsocial_15")]
hhs_8 <- gather(hhs_7, Year, HH_members, mieperho_15:mieperho_16, 
                factor_key=TRUE)
hhs_8$Year <- ifelse(hhs_8$Year == "mieperho_15", 2015, 2016)

hhs_9 <- hhs[c("percepho_15", "percepho_16", "estrsocial_15")]
hhs_10 <- gather(hhs_9, Year_2, Income_gen, percepho_15:percepho_16, 
                 factor_key=TRUE)
hhs_10$Year_2 <- ifelse(hhs_10$Year_2 == "percepho_15", 2015, 2016)

hhs_11 <- merge(hhs_8, hhs_10, by = 'row.names', 
                all = TRUE)[c("estrsocial_15.x", "Year", 
                              "HH_members", "Income_gen")]
names(hhs_11) <- c("estrsocial_15", "Year", 
                   "HH_members", "Income_gen")
hhs_11$Ratio <- hhs_11$Income_gen / hhs_11$HH_members

# Income per income generator and expenses per capita complete
out_icpc <- as.data.frame(matrix(nrow = 0, ncol = 3))
names(out_icpc) <- c("INCOME_PG", "EXP_PM", "ESTRSOCIAL")
for (i in 1:length(filt)){
  new_filt <- filt[[i]][,c("INCOME_PG", "EXP_PM", "ESTRSOCIAL")]
  new_filt$Year <- i + 2014
  out_icpc <- rbind(out_icpc, new_filt)
}

# Means of Income per income generator and expenses per capita together
no_outliers <- function(x) {
  x <- na.omit(x)
  Q <- quantile(x, probs=c(.25, .75), na.rm = FALSE)
  iqr <- IQR(x)
  up <-  Q[2]+1.5*iqr # Upper Range  
  low<- Q[1]-1.5*iqr # Lower Range
  
  eliminated <- subset(x, x > (Q[1] - 1.5*iqr) & x < (Q[2]+1.5*iqr))
  
  return (eliminated)
}

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

all_mns_lg <- gather(all_means, inc_exp, avg_soles, income_pg:exp_pm, 
                     factor_key=TRUE)

names(all_mns_lg) <- c("SEL", "YEAR", "Category", "SOLES")

# Income per income generator and expenses per capita on Panel data
hhs_12 <- hhs[,c("INCOME_PG_15", "INCOME_PG_16", "estrsocial_15")]
hhs_13 <- gather(hhs_12, Year, Income_pg, INCOME_PG_15:INCOME_PG_16, 
                 factor_key=TRUE)
hhs_13$Year <- ifelse(hhs_13$Year == "INCOME_PG_15", 2015, 2016)

hhs_14 <- hhs[,c("EXP_PM_15", "EXP_PM_16", "estrsocial_15")]
hhs_15 <- gather(hhs_14, Year_2, Expenses_pm, EXP_PM_15:EXP_PM_16, 
                 factor_key=TRUE)
hhs_15$Year_2 <- ifelse(hhs_15$Year_2 == "EXP_PM_15", 2015, 2016)

hhs_16 <- merge(hhs_13, hhs_15, by = 'row.names', 
                all = TRUE)[c("estrsocial_15.x", "Year", 
                              "Income_pg", "Expenses_pm")]
names(hhs_16) <- c("estrsocial_15", "Year", 
                   "Income_pg", "Expenses_pm")
hhs_16$Ratio <- hhs_16$Expenses_pm / hhs_16$Income_pg

# Average number of household members on non panel data and inc_gen ratio
var1 <- "PERCEPHO"
var2 <- "MIEPERHO"

for (i in 1:length(filt)) {
  filt[[i]]$RATIO_WORK <- filt[[i]][[var1]] / filt[[i]][[var2]]
} # Send this to the filter?

com_names2 <- c("hh_members", "ratio_ig", "sel", "year")

all_means2 <- as.data.frame(matrix(nrow = 0, ncol = length(com_names2)))
names(all_means2) <- com_names2

for (i in 1:length(SELs)) {
  crrt_means <- as.data.frame(matrix(nrow = length(filt), ncol = length(com_names2)))
  names(crrt_means) <- com_names2
  
  mem_SEL <- c()
  gen_SEL <- c()
  
  for (j in 1:length(filt)) {
    mem_m <- mean(no_outliers(na.omit(filt[[j]][filt[[j]]$ESTRSOCIAL == SELs[i],]$MIEPERHO)))
    gen_m <- mean(no_outliers(na.omit(filt[[j]][filt[[j]]$ESTRSOCIAL == SELs[i],]$RATIO_WORK)))
    
    mem_SEL <- c(mem_SEL, mem_m)
    gen_SEL <- c(gen_SEL, gen_m)
  }
  
  crrt_means$hh_members <- mem_SEL
  crrt_means$ratio_ig <- gen_SEL
  crrt_means$sel <- SELs[i]
  crrt_means$year <- c(2015:2019)
  
  all_means2 <- rbind(all_means2, crrt_means)
}

all_mns_lg2 <- gather(all_means2, mem_cat, number_people, hh_members:ratio_ig, 
                      factor_key=TRUE)

#####
# PLOT HOUSEHOLD LEVEL DATA
#####


# Plot number of participants per year per SEL on non panel data
jpeg(file="OUTPUTDATA/PRE GRAPHS/PARTICIPANTS_HH_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(daf, aes(x = Year, y = Households, fill = SEL)) + 
  geom_bar(position = "dodge", stat = "identity")
dev.off()

# Plot number of household members in non-panel data
jpeg(file="OUTPUTDATA/PRE GRAPHS/MEMBERS_PER_HH_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(nrr_mems, aes(x=factor(Year), y=Nr_mem, fill = SEL)) + 
  geom_boxplot() + 
  xlab("Year") + 
  ylab("Number of Household members")
dev.off()

# Plot number of participant households per SEL on panel data
jpeg(file="OUTPUTDATA/PRE GRAPHS/PARTICIPANTS_HH_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(hhs, aes(x = estrsocial_15, fill = estrsocial_15)) +
  geom_bar() +
  xlab("Socio Economic Level") +
  ylab("Households") + 
  theme(legend.position = "none")
dev.off()

# Table of the previous data
fur_chpt <- as.data.frame(table(hhs$estrsocial_15))
names(fur_chpt) <- c("SEL", "Participating households")
write.csv(fur_chpt, "OUTPUTDATA/PRE GRAPHS/panel_participants.csv", 
          row.names=FALSE)

# Plot number of household members in panel data
jpeg(file="OUTPUTDATA/PRE GRAPHS/MEMBERS_PER_HH_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(hhs_3, aes(x=factor(Year), y=Nr_mem, fill = estrsocial_15)) + 
  geom_boxplot() + 
  xlab("Year") + 
  ylab("Number of Household members") +
  guides(fill=guide_legend(title="SEL"))
dev.off()

# Plot income per household for non panel data
jpeg(file="OUTPUTDATA/PRE GRAPHS/INCOME_HH_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(ie_df, aes(x=factor(Year), y=Income, fill = SEL)) + 
  geom_boxplot() + 
  xlab("Year") + 
  ylab("Household income (in Soles)")
dev.off()

# Plot expenses per household on non panel data
jpeg(file="OUTPUTDATA/PRE GRAPHS/EXPENSES_HH_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(ie_df, aes(x=factor(Year), y=Expenses, fill = SEL)) + 
  geom_boxplot() + 
  xlab("Year") + 
  ylab("Household expenses (in Soles)")
dev.off()

# Plot yearly balance per household on non panel data
jpeg(file="OUTPUTDATA/PRE GRAPHS/BALANCE_HH_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(ie_df, aes(x=factor(Year), y=Balance, fill = SEL)) + 
  geom_boxplot() + 
  xlab("Year") + 
  ylab("Household Yearly balance (in Soles)")
dev.off()

# Plot Income per household on panel data
jpeg(file="OUTPUTDATA/PRE GRAPHS/INCOME_HH_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(hhs_6c, aes(x=factor(Year), y=income, fill = estrsocial_15)) + 
  geom_boxplot() + 
  xlab("Year") + 
  ylab("Household Income (in Soles)") +
  guides(fill=guide_legend(title="SEL"))
dev.off()

# Plot expenses per household on panel data
jpeg(file="OUTPUTDATA/PRE GRAPHS/EXPENSES_HH_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(hhs_6c, aes(x=factor(Year), y=expenses, fill = estrsocial_15)) + 
  geom_boxplot() + 
  xlab("Year") + 
  ylab("Household Expenses (in Soles)") +
  guides(fill=guide_legend(title="SEL"))
dev.off()

# Plot yearly balance per household on panel data
jpeg(file="OUTPUTDATA/PRE GRAPHS/BALANCE_HH_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(hhs_6c, aes(x=factor(Year), y=Balance, fill = estrsocial_15)) + 
  geom_boxplot() + 
  xlab("Year") + 
  ylab("Household yearly Balance (in Soles)") +
  guides(fill=guide_legend(title="SEL"))
dev.off()

# Plot number of income generation members in non panel data
jpeg(file="OUTPUTDATA/PRE GRAPHS/NR_INCOMEGEN_HH_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(mems_df, aes(x=factor(Year), y=Inc_generator, fill = SEL)) + 
  geom_boxplot() + 
  xlab("Year") + 
  ylab("Income generators per Household")
dev.off()

# Plot ratio between income generating members and all members in non panel
jpeg(file="OUTPUTDATA/PRE GRAPHS/RAT_INCOMEGEN_HH_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(mems_df, aes(x=factor(Year), y=Ratio, fill = SEL)) + 
  geom_boxplot() + 
  xlab("Year") + 
  ylab("Ratio income generators over total number of household members")
dev.off()

# Plot number of income generating members in panel data
jpeg(file="OUTPUTDATA/PRE GRAPHS/NR_INCOMEGEN_HH_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(hhs_11, aes(x=factor(Year), y=Income_gen, fill = estrsocial_15)) + 
  geom_boxplot() + 
  xlab("Year") + 
  ylab("Number of income generators per household") +
  guides(fill=guide_legend(title="SEL"))
dev.off()

# Plot ratio between income generating members and all members in panel
jpeg(file="OUTPUTDATA/PRE GRAPHS/RAT_INCOMEGEN_HH_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(hhs_11, aes(x=factor(Year), y=Ratio, fill = estrsocial_15)) + 
  geom_boxplot() + 
  xlab("Year") + 
  ylab("Ratio income generators over total number of household members") +
  guides(fill=guide_legend(title="SEL"))
dev.off()

# Plot income per income generator full data
jpeg(file="OUTPUTDATA/PRE GRAPHS/BOX_IPIG_HH_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(out_icpc, aes(x=factor(Year), y=INCOME_PG, fill = ESTRSOCIAL)) + 
  geom_boxplot() + 
  xlab("Year") + 
  ylab("Income per income generator") +
  guides(fill=guide_legend(title="SEL"))
# When plotting it raises an warning because there are certain points with an
# infinite number. This is because some households without income generators (0) 
# register an income
dev.off()

# Plot expenses per capita full data
jpeg(file="OUTPUTDATA/PRE GRAPHS/BOX_EPHM_HH_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(out_icpc, aes(x=factor(Year), y=EXP_PM, fill = ESTRSOCIAL)) + 
  geom_boxplot() + 
  xlab("Year") + 
  ylab("Expenses per household members") +
  guides(fill=guide_legend(title="SEL"))
dev.off()

# Plot the evolution of the means of income per income generator and so together
jpeg(file="OUTPUTDATA/PRE GRAPHS/LINE_IPIG_EPHM_HH_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(data=all_mns_lg, 
       aes(x=YEAR, y=SOLES, color = SEL, linetype = Category)) +
  geom_line() +
  geom_point() + 
  ylab("Soles (S/)")
dev.off()

# Plot income per income members on panel data
jpeg(file="OUTPUTDATA/PRE GRAPHS/BOX_IPIG_HH_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(hhs_16, aes(x=factor(Year), y=Income_pg, fill = estrsocial_15)) + 
  geom_boxplot() + 
  xlab("Year") + 
  ylab("Income per Income generator members") +
  guides(fill=guide_legend(title="SEL"))
# When plotting it raises an warning because there are certain points with an
# infinite number. This is because some households without income generators (0) 
# register an income
dev.off()

# Plot expenses per capita on panel data
jpeg(file="OUTPUTDATA/PRE GRAPHS/BOX_EPHM_HH_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(hhs_16, aes(x=factor(Year), y=Expenses_pm, fill = estrsocial_15)) + 
  geom_boxplot() + 
  xlab("Year") + 
  ylab("Expenses for each household member") +
  guides(fill=guide_legend(title="SEL"))
dev.off()

# Plot average household members on non-panel data
jpeg(file="OUTPUTDATA/PRE GRAPHS/AVG_HHM_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(data=all_mns_lg2[all_mns_lg2$mem_cat == "hh_members",], 
       aes(x=year, y=number_people, color = sel)) +
  geom_line() +
  geom_point() + 
  ylab("Average Household members")
dev.off()

# Plot average ratio income generators over total members non-panel data
jpeg(file="OUTPUTDATA/PRE GRAPHS/AVG_IGM_RATIO_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(data=all_mns_lg2[all_mns_lg2$mem_cat == "ratio_ig",], 
       aes(x=year, y=number_people, color = sel)) +
  geom_line() +
  geom_point() + 
  ylab("Average Ratio income generators/total members")
dev.off()

# Plot absolute variation in the number of household members 2015-2016 PANEL DATA
hhs$dif_mem <- hhs$mieperho_16 - hhs$mieperho_15

jpeg(file="OUTPUTDATA/PRE GRAPHS/ABSVAR_HHMEM_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(hhs, aes(x=estrsocial_15, y=dif_mem)) + 
  geom_count() + 
  xlab("SEL") + 
  ylab("Absolute variation in number of Household members 2015-2016")
dev.off()

# Plot relative variation in the number of household members 2015-2016 PANEL DATA
hhs$reldif_mem <- hhs$mieperho_16 / hhs$mieperho_15

jpeg(file="OUTPUTDATA/PRE GRAPHS/RELVAR_HHMEM_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(hhs, aes(x=estrsocial_15, y=reldif_mem)) + 
  geom_count() + 
  xlab("SEL") + 
  ylab("Relative variation in number of Household members 2015-2016")
dev.off()

# Plot absolute variation in the yearly household balance 2015-2016
hhs$balance15 <- hhs$ingmo2hd_15 - hhs$gashog1d_15
hhs$balance16 <- hhs$ingmo2hd_16 - hhs$gashog1d_16
hhs$balancedif <- hhs$balance16 - hhs$balance15

jpeg(file="OUTPUTDATA/PRE GRAPHS/ABSVAR_BAL_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(hhs, aes(x=estrsocial_15, y=balancedif)) + 
  geom_boxplot() + 
  xlab("SEL") + 
  ylab("Absolute variation in yearly household balance 2015-2016 (s/)")
dev.off()

# Plot relative variation in the yearly household balance 2015-2016
hhs$relbalancedif <- hhs$balance16 / hhs$balance15

jpeg(file="OUTPUTDATA/PRE GRAPHS/RELVAR_BAL_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(hhs, aes(x=estrsocial_15, y=relbalancedif)) + 
  geom_count() + 
  xlab("SEL") + 
  ylab("Relative variation in yearly household balance 2015-2016 (s/)")
dev.off()

# Ratio expenses/income (I have done difference but not ratio)






#####
# Graphs at individual level
#####
# Total number of participants 2015 by SEL (non panel data) - a table is enough for this

# P204 = HOUSEHOLD MEMBER ALREADY FILTERED ON THE OTHER FILE
# P207 = GENDER
# P208A = AGE
# P209 = COUPLED?

# Participants in module 200, by age and gender

###
# DO THIS IN FILTER FILE!
# Turn P208A to standard categories
# Generate standard cuts
cut_sers <- seq(-1, 79, 5)
cut_sers <- c(cut_sers, Inf)

# Create labels
labs_agcat <- cut_sers[-c(length(cut_sers))]
labs_agcat[1] <- 0
lb_cat2 <- c()
for (i in 1:(length(labs_agcat)-1)){
  if (i == 1){
    my_lb <- paste(labs_agcat[i], labs_agcat[i+1], sep = "-")
  } else {
    my_lb <- paste(labs_agcat[i] + 1, labs_agcat[i+1], sep = "-")
  }
  lb_cat2 <- c(lb_cat2, my_lb)
}
lb_cat2 <- c(lb_cat2, ">79")

# Cut data in categories
genage_dta$AGE_CAT <- cut(genage_dta$P208A,
                              breaks=cut_sers,
                              labels = lb_cat2,
                              include.lowest = TRUE)

# ON THE MODEL, GENDERS ARE ON SPANISH!
# Translate gender categories
genage_dta$P207 <- ifelse(genage_dta$P207 == "hombre", "Male", "Female")

###

# Count cases
pir_vars <- c("Age_Cat", "Gender", "Count")
pir1 <- as.data.frame(table(genage_dta$AGE_CAT, 
                              genage_dta$P207))
names(pir1) <- pir_vars

# Total number of participants
sum(pir1$Count)

# Plot data
jpeg(file="OUTPUTDATA/PRE GRAPHS/PARTICIPANTS_IND_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pir1 %>% mutate(
  count = ifelse(Gender == "Male", Count*(-1),
                 Count*1)) %>%
  ggplot(aes(x = Age_Cat, y = count, fill=Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-2000,2000),
                     breaks = seq(-2000, 2000, by = 200)) +
  labs(title = "ENAHO 2015 m200 Participant profile", x = "Age Categories",
       y = "Number of Participants") 
dev.off()

# Non panel data module 200 by chief, non chief category
# P203 = rel with family chief
###
genage_dta$CHF_CAT <- ifelse(genage_dta$P203 == "Jefe/Jefa", "Chief", "Non-chief")
###

pir2_vars <- c("Age_Cat", "Gender", "Chief", "Count")
pir2 <- as.data.frame(table(genage_dta$AGE_CAT, 
                            genage_dta$P207,
                            genage_dta$CHF_CAT))
names(pir2) <- pir2_vars

# Plot data
jpeg(file="OUTPUTDATA/PRE GRAPHS/CHIEFS_IND_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pir2 %>% mutate(
  count = ifelse(Gender == "Male", Count*(-1),
                 Count*1)) %>%
  ggplot(aes(x = Age_Cat, y = count, fill=Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-1800,1800),
                     breaks = seq(-1800, 1800, by = 600)) +
  labs(title = "ENAHO 2015 m200 household chiefs", x = "Age Categories",
       y = "Number of HH chiefs") +
  facet_grid(cols = vars(Chief))
dev.off()

# The same now with income generators (Module 500, only age > 14)
# Cut age in categories for module 500
# First category (10-14) becomes only 14 (on MODEL DELETE THAT CATEGORY)
ind_dta$AGE_CAT <- cut(ind_dta$P208A,
                          breaks=cut_sers,
                          #labels = lb_cat3,
                       labels = lb_cat2,
                          include.lowest = TRUE)
ind_dta$AGE_CAT <- droplevels(ind_dta$AGE_CAT)
# The first category ("10-14") become on "14" (CHECK ON MODEL!)
levels(ind_dta$AGE_CAT)[1] <- "14"


# ON THE MODEL, GENDERS ARE ON SPANISH!
# Translate gender categories
ind_dta$P207 <- ifelse(ind_dta$P207 == "hombre", "Male", "Female")
###

ind_dta$INC_GEN <- "No"
less_idta <- ind_dta[ind_dta$P203 != "Trabajador Hogar" &
                       ind_dta$P203 != "Pensionista" &
                       ind_dta$P208A >= 14, ]
less_idta$INC_GEN <- 
  ifelse(!is.na(less_idta$I524A1) | 
           !is.na(less_idta$D529t) |
           !is.na(less_idta$I530A) |
           !is.na(less_idta$I538A1) |
           !is.na(less_idta$D540t) |
           !is.na(less_idta$I541a) |
           !is.na(less_idta$D544t) |
           !is.na(less_idta$D556t1) |
           !is.na(less_idta$D556t2) |
           !is.na(less_idta$D557t) |
           !is.na(less_idta$D558t), "IG", "Non-IG")

ind_dta[ind_dta$P203 != "Trabajador Hogar" &
          ind_dta$P203 != "Pensionista" &
          ind_dta$P208A >= 14, ]$INC_GEN <- less_idta$INC_GEN
####

pir3_vars <- c("Age_Cat", "Gender", "IG", "Count")
pir3 <- as.data.frame(table(ind_dta$AGE_CAT, 
                            ind_dta$P207,
                            ind_dta$INC_GEN))
names(pir3) <- pir3_vars

# Plot data
jpeg(file="OUTPUTDATA/PRE GRAPHS/IGS_IND_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pir3 %>% mutate(
  count = ifelse(Gender == "Male", Count*(-1),
                 Count*1)) %>%
  ggplot(aes(x = Age_Cat, y = count, fill=Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-1800,1800),
                     breaks = seq(-1800, 1800, by = 600)) +
  labs(title = "ENAHO 2015 m500 Income Generators", x = "Age Categories",
       y = "Number of IGs") +
  facet_grid(cols = vars(IG))
dev.off()

# Only chiefs, income and not income generators facet
# Add chief category to module 500
ind_dta$CHF_CAT <- ifelse(ind_dta$P203 == "Jefe/Jefa del hogar", "Chief", "Non-chief")

pir4_vars <- c("Age_Cat", "Gender", "IG", "Chief", "Count")
pir4 <- as.data.frame(table(ind_dta$AGE_CAT, 
                            ind_dta$P207,
                            ind_dta$INC_GEN,
                            ind_dta$CHF_CAT))
names(pir4) <- pir4_vars

sum(pir4$Count)

# Plot data
jpeg(file="OUTPUTDATA/PRE GRAPHS/CH_IG_IND_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pir4 %>% mutate(
  count = ifelse(Gender == "Male", Count*(-1),
                 Count*1)) %>%
  ggplot(aes(x = Age_Cat, y = count, fill=Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-900,900),
                     breaks = seq(-900, 900, by = 300)) +
  labs(title = "ENAHO 2015 m500 Chief/Income generator characterization", x = "Age Categories",
       y = "Number of people") +
  facet_grid(vars(IG), vars(Chief))
dev.off()

# Proportions! out of the total people in age/gender category, how many fall
# in chief/non chief. IG non-IG
# For this first count totals by age category and gender
# Then divide the count on T4 by this number

# Proportion of participants by age and gender categories who are income generators
pir5_vars <- c("Age_Cat", "Gender", "Count_Tot")
pir5 <- as.data.frame(table(ind_dta$AGE_CAT, 
                            ind_dta$P207))
names(pir5) <- pir5_vars

pir6 <- merge(pir3, pir5)
pir6$Prop <- pir6$Count / pir6$Count_Tot

# Plot data
jpeg(file="OUTPUTDATA/PRE GRAPHS/IGS_PROP_IND_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pir6[pir6$IG == "IG",] %>% mutate(
  count = ifelse(Gender == "Male", Prop*(-1),
                 Prop*1)) %>%
  ggplot(aes(x = Age_Cat, y = count, fill=Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-1,1),
                     breaks = seq(-1, 1, by = 0.2)) +
  labs(title = "ENAHO 2015 m500 Proportion of Income Generators", x = "Age Categories",
       y = "Proportion of IGs")
dev.off()

# Proportion of participants by age and gender categories who are chiefs
pir7 <- merge(pir5, pir2)
pir7$Prop <- pir7$Count / pir7$Count_Tot

jpeg(file="OUTPUTDATA/PRE GRAPHS/CHF_PROP_IND_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pir7[pir7$Chief == "Chief",] %>% mutate(
  count = ifelse(Gender == "Male", Prop*(-1),
                 Prop*1)) %>%
  ggplot(aes(x = Age_Cat, y = count, fill=Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-1,1),
                     breaks = seq(-1, 1, by = 0.2)) +
  labs(title = "ENAHO 2015 m500: Proportion of HH chiefs by age and gender", x = "Age Categories",
       y = "Proportion of HH chiefs")
dev.off()

# Proportion of Chief participants by age and gender categories who are IGs
#pir2 counts chiefs
#pir4 counts chiefs and IGs
pir8 <- pir2
names(pir8) <- c("Age_Cat", "Gender", "Chief", "Count_Tot")
pir9 <- merge(pir8, pir4)
pir9$Prop <- pir9$Count / pir9$Count_Tot

jpeg(file="OUTPUTDATA/PRE GRAPHS/COMP_IG_CHF_IND_NP.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pir9[pir9$IG == "IG",] %>% mutate(
  count = ifelse(Gender == "Male", Prop*(-1),
                 Prop*1)) %>%
  ggplot(aes(x = Age_Cat, y = count, fill=Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-1,1),
                     breaks = seq(-1, 1, by = 0.2)) +
  labs(title = "ENAHO 2015 m500: Proportion of income generators among HH Chiefs and non-HH Chiefs",
       x = "Age Categories",
       y = "Proportion of Income generators") + 
  facet_grid(cols = vars(Chief))
dev.off()

# THE ABOVE ones BUT FROM PANEL DATA
# First general profiles over the two years
# Change category names
levels(my_indv$age_cat_15) <- lb_cat2
levels(my_indv$age_cat_16) <- lb_cat2
# Translate genders to english
my_indv$p207_15 <- ifelse(my_indv$p207_15 == "hombre", "Male", "Female") # New comers have NA!
# But here we have not considered newcomers
# 2016 has NAS

# create table as dataframe
pir10_15 <- as.data.frame(table(my_indv$age_cat_15,
                                my_indv$p207_15))
names(pir10_15) <- pir_vars
pir10_15$Year <- "2015"

pir10_16 <- as.data.frame(table(my_indv$age_cat_16,
                                my_indv$p207_15))
names(pir10_16) <- pir_vars
pir10_16$Year <- "2016"

pir10 <- rbind(pir10_15, pir10_16)
names(pir10_15) <- c("Age_Cat", "Gender", "Count_15", "Year")
names(pir10_16) <- c("Age_Cat", "Gender", "Count_16", "Year")

pir11 <- merge(pir10_15[,-c(ncol(pir10_15))],
               pir10_16[,-c(ncol(pir10_16))])
pir11$Dif <- pir11$Count_16 - pir11$Count_15

# plot one year on top of the other
jpeg(file="OUTPUTDATA/PRE GRAPHS/PART_COMP_IND_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pir11 %>% mutate(
  count15 = ifelse(Gender == "Male", Count_15*(-1),
                 Count_15*1),
  count16 = ifelse(Gender == "Male", Count_16*(-1),
                   Count_16*1))%>%
  ggplot() +
  geom_bar(aes(x = Age_Cat, y = count15, fill=Gender, alpha = 0.75),stat = "identity") +
  geom_bar(aes(x = Age_Cat, y = count16, fill=Gender, alpha = 0.75),stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-500,500),
                     breaks = seq(-500, 500, by = 100)) +
  labs(title = "ENAHO PANEL m200: Characterization of panel individuals 2015-2016", 
       x = "Age Categories",
       y = "Participants")
dev.off()

# Plot one year next to the other
jpeg(file="OUTPUTDATA/PRE GRAPHS/PART_COMP_IND_P2.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pir10 %>% mutate(
  count = ifelse(Gender == "Male", Count*(-1),
                   Count*1))%>%
  ggplot(aes(x = Age_Cat, y = count, fill=Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-500,500),
                     breaks = seq(-500, 500, by = 100)) +
  labs(title = "ENAHO PANEL m200: Characterization of panel individuals 2015-2016", 
       x = "Age Categories",
       y = "Participants") +
  facet_grid(cols = vars(Year))
dev.off()

# Then changing status (BUT AGE CATEGORIES ARE MOVING)
# profile of people that move out
leavers <- my_indv[my_indv$p217_16 == "se fue a otro hogar",]
#newcomers <- my_indv[is.na(my_indv$p207_15),] # we do not have newcomers

pir11_15 <- as.data.frame(table(leavers$age_cat_15,
                             leavers$p207_15))
names(pir11_15) <- pir_vars

# Number of participant
sum(pir11_15$Count)

jpeg(file="OUTPUTDATA/PRE GRAPHS/MOUT_IND_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pir11_15 %>% mutate(
  count = ifelse(Gender == "Male", Count*(-1),
                 Count*1))%>%
  ggplot(aes(x = Age_Cat, y = count, fill=Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-100,100),
                     breaks = seq(-100, 100, by = 10)) +
  labs(title = "ENAHO PANEL m200: Characterization of participants moving out in 2016", 
       x = "Age Categories",
       y = "Participants moving out in 2016")
dev.off()

# Proportion of people that move out
my_indv$M_OUT <- ifelse(my_indv$p217_16 == "se fue a otro hogar",
                        "LEFT", "OTHER")
pir12 <- as.data.frame(table(my_indv$age_cat_15,
                    my_indv$p207_15,
                    my_indv$M_OUT))
names(pir12) <- c("Age_Cat", "Gender", "Leaver", "Part_Count")

pir13 <- merge(pir12, pir10_15[,-c(ncol(pir10_15))])
pir13$Prop <- pir13$Part_Count / pir13$Count_15

jpeg(file="OUTPUTDATA/PRE GRAPHS/MOUT_PROP_IND_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pir13[pir13$Leaver == "LEFT",] %>% mutate(
  count = ifelse(Gender == "Male", Prop*(-1),
                 Prop*1))%>%
  ggplot(aes(x = Age_Cat, y = count, fill=Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-0.4,0.4),
                     breaks = seq(-0.4, 0.4, by = 0.05)) +
  labs(title = "ENAHO PANEL m200: Proportion of move-out individuals in relation to the total by category",
       x = "Age Categories",
       y = "Proportion of move-out individuals")
dev.off()

# On module 500
###
# Determine the income generators each year
less_idta_15 <- igen_dta[igen_dta$p203_15 != 8 & # Those that can be income generator
                           igen_dta$p203_15 != 9 &
                           igen_dta$p208a_15 >= 14, ]
less_idta_15$inc_gen_15 <- 
  ifelse(!is.na(less_idta_15$i524a1_15) | # Income generator conditions
           !is.na(less_idta_15$d529t_15) |
           !is.na(less_idta_15$i530a_15) |
           !is.na(less_idta_15$i538a1_15) |
           !is.na(less_idta_15$d540t_15) |
           !is.na(less_idta_15$i541a_15) |
           !is.na(less_idta_15$d544t_15) |
           !is.na(less_idta_15$d556t1_15) |
           !is.na(less_idta_15$d556t2_15) |
           !is.na(less_idta_15$d557t_15) |
           !is.na(less_idta_15$d558t_15), "Yes", "No")

less_idta_16 <- igen_dta[igen_dta$p203_16 != 8 &
                           igen_dta$p203_16 != 9 &
                           igen_dta$p208a_16 >= 14, ]
less_idta_16$inc_gen_16 <- 
  ifelse(!is.na(less_idta_16$i524a1_16) | 
           !is.na(less_idta_16$d529t_16) |
           !is.na(less_idta_16$i530a_16) |
           !is.na(less_idta_16$i538a1_16) |
           !is.na(less_idta_16$d540t_16) | #
           !is.na(less_idta_16$i541a_16) |
           !is.na(less_idta_16$d544t_16) |
           !is.na(less_idta_16$d556t1_16) |
           !is.na(less_idta_16$d556t2_16) |
           !is.na(less_idta_16$d557t_16) |
           !is.na(less_idta_16$d558t_16), "Yes", "No")

igen_dta$inc_gen_15 <- "No"
igen_dta$inc_gen_16 <- "No"

igen_dta[!is.na(igen_dta$p203_15) &
           igen_dta$p203_15 != 8 &
           igen_dta$p203_15 != 9 &
           igen_dta$p208a_15 >= 14, ]$inc_gen_15 <- less_idta_15$inc_gen_15


igen_dta[igen_dta$p203_16 != 8 &
           igen_dta$p203_16 != 9 &
           igen_dta$p208a_16 >= 14, ]$inc_gen_16 <- less_idta_16$inc_gen_16

# Classify ages in the module 500
levels(igen_dta$p208_c_15) <- lb_cat2
levels(igen_dta$p208_c_16) <- lb_cat2

# Drop unused levels (as only from age 14)
igen_dta$p208_c_15 <- droplevels(igen_dta$p208_c_15)
igen_dta$p208_c_16 <- droplevels(igen_dta$p208_c_16)

# The first category ("10-14") become on "14" (CHECK ON MODEL!)
levels(igen_dta$p208_c_15)[1] <- "14"
levels(igen_dta$p208_c_16)[1] <- "14"

# ON THE MODEL, GENDERS ARE ON SPANISH!
# Translate gender categories
igen_dta$p207_15 <- ifelse(igen_dta$p207_15 == "hombre", "Male", "Female")
###

# Profile of income generator both years
pir14 <- as.data.frame(table(igen_dta$p208_c_15, 
                             igen_dta$p207_15, 
                             igen_dta$inc_gen_15))
names(pir14) <- c("Age_Cat", "Gender", "IG", "Part_Count")
pir14$Year <- "2015"

pir17 <- as.data.frame(table(igen_dta$p208_c_16, 
                             igen_dta$p207_15, 
                             igen_dta$inc_gen_15))
names(pir17) <- c("Age_Cat", "Gender", "IG", "Part_Count")
pir17$Year <- "2016"

pir18 <- rbind(pir14, pir17)

pir19 <- merge(pir10, pir18)
pir19$Prop <- pir19$Part_Count / pir19$Count

# Plot data
jpeg(file="OUTPUTDATA/PRE GRAPHS/IG_PROP_IND_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pir19[pir19$IG == "Yes",] %>% mutate(
  count = ifelse(Gender == "Male", Prop*(-1),
                 Prop*1))%>%
  ggplot(aes(x = Age_Cat, y = count, fill=Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-1,1),
                     breaks = seq(-1, 1, by = 0.2)) +
  labs(title = "ENAHO PANEL m500: Proportion of income generators by age and gender", 
       x = "Age Categories",
       y = "Proportion of income generators") +
  facet_grid(cols = vars(Year))
dev.off()

# Plot data
jpeg(file="OUTPUTDATA/PRE GRAPHS/IG_IND_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pir19[pir19$IG == "Yes",] %>% mutate(
  count = ifelse(Gender == "Male", Part_Count*(-1),
                 Part_Count*1))%>%
  ggplot(aes(x = Age_Cat, y = count, fill=Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-300,300),
                     breaks = seq(-300, 300, by = 100)) +
  labs(title = "ENAHO PANEL m500: Income generators by age and gender", 
       x = "Age Categories",
       y = "Number of Income generators") +
  facet_grid(cols = vars(Year))
dev.off()

# Plot one on top of the other
names(pir14) <- c("Age_Cat", "Gender", "IG", "Count_15", "Year")
names(pir17) <- c("Age_Cat", "Gender", "IG", "Count_16", "Year")
pir20 <- merge(pir14[,-c(ncol(pir14))],
               pir17[,-c(ncol(pir17))])

# plot one year on top of the other (Because category 14 is only one age
# and people age, there is a sudden change)
jpeg(file="OUTPUTDATA/PRE GRAPHS/IG_COMP_IND_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pir20 %>% mutate(
  count15 = ifelse(Gender == "Male", Count_15*(-1),
                   Count_15*1),
  count16 = ifelse(Gender == "Male", Count_16*(-1),
                   Count_16*1))%>%
  ggplot() +
  geom_bar(aes(x = Age_Cat, y = count16, fill=Gender, alpha = 0.5),stat = "identity") +
  geom_bar(aes(x = Age_Cat, y = count15, fill=Gender, alpha = 0.5),stat = "identity") +
  
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-500,500),
                     breaks = seq(-500, 500, by = 100)) +
  labs(title = "ENAHO PANEL m500: Absolute change in income generators 2015-2016",
       x = "Age Categories",
       y = "Absolute change in income generators")
dev.off()

# One on top of the other with proportions



# profile of people that lost a job
lost_ig <- igen_dta[igen_dta$inc_gen_15 == "Yes" &
                      igen_dta$inc_gen_16 == "No",]
pilostig <- as.data.frame(table(lost_ig$p208_c_15,
                    lost_ig$p207_15))

names(pilostig) <- c("Age_Cat", "Gender", "Count")


jpeg(file="OUTPUTDATA/PRE GRAPHS/IG_LOST_IND_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pilostig %>% mutate(
  count = ifelse(Gender == "Male", Count*(-1),
                 Count*1))%>%
  ggplot(aes(x = Age_Cat, y = count, fill=Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-80,80),
                     breaks = seq(-80, 80, by = 20)) +
  labs(title = "ENAHO PANEL m500: Individuals losing IG status 2015-2016", 
       x = "Age Categories",
       y = "Number of people losing IG status")
dev.off()

# profile of people that got a job
earn_ig <- igen_dta[igen_dta$inc_gen_15 == "No" &
                      igen_dta$inc_gen_16 == "Yes",]

piearnig <- as.data.frame(table(earn_ig$p208_c_15,
                                earn_ig$p207_15))

names(piearnig) <- c("Age_Cat", "Gender", "Count")

jpeg(file="OUTPUTDATA/PRE GRAPHS/IG_EARN_IND_P.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
piearnig %>% mutate(
  count = ifelse(Gender == "Male", Count*(-1),
                 Count*1))%>%
  ggplot(aes(x = Age_Cat, y = count, fill=Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-100,100),
                     breaks = seq(-100, 100, by = 20)) +
  labs(title = "ENAHO PANEL m500: Individuals earning IG status 2015-2016", 
       x = "Age Categories",
       y = "Number of people earning IG status")
dev.off()





#####
# Non-ENAHO data
#####
# Total population data
pops <- read.csv("INPUT DATA/ORIGINAL/NOT ENAHO/estimated_projected_pop_30jun_5year_agecat_gender.csv")
pops$Age_cat <- factor(pops$Age_cat, levels = unique(pops$Age_cat))

# A faceted graph of the evolution of each age category by gender
jpeg(file="OUTPUTDATA/PRE GRAPHS/POP_EVO_INEI_LN.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(pops, aes(x=as.numeric(Year), y=Pop, 
                 colour = Age_cat, group = Age_cat)) + 
  geom_point() + 
  geom_line() +
  xlab("Year") + 
  ylab("Population") +
  facet_grid(vars(Gender))
dev.off()

# A point size graph of the evolution of each age category by gender
jpeg(file="OUTPUTDATA/PRE GRAPHS/POP_EVO_INEI_PTS.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(pops, aes(x=as.numeric(Year), y=Age_cat, 
                 size = Pop)) + 
  geom_point() + 
  xlab("Year") + 
  ylab("Population") +
  facet_grid(cols = vars(Gender))
dev.off()

# A population pyramid 2015 only
pops15 <- pops[pops$Year == "2015",]

jpeg(file="OUTPUTDATA/PRE GRAPHS/POP_PYR_15_INEI.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pops15 %>% mutate(
  count = ifelse(Gender == "Male", Pop*(-1),
                   Pop*1))%>%
  ggplot(aes(x = Age_cat, y = count, fill = Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-1500000,1500000),
                     breaks = seq(-1500000, 1500000, by = 500000)) +
  labs(title = "Total projected population for Peru for 2015",
       x = "Age Categories",
       y = "Population")
dev.off()

# A population pyramid 2015-2020
pops20 <- pops[pops$Year == "2020",]

pops15$Pop15 <- pops15$Pop
pops20$Pop20 <- pops20$Pop

pops_wide <- merge(pops15[,c("Age_cat", "Gender", "Pop15")],
                   pops20[,c("Age_cat", "Gender", "Pop20")])

jpeg(file="OUTPUTDATA/PRE GRAPHS/POP_PYR_1522_INEI.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
pops_wide %>% mutate(
  count15 = ifelse(Gender == "Male", Pop15*(-1),
                   Pop15*1),
  count20 = ifelse(Gender == "Male", Pop20*(-1),
                   Pop20*1))%>%
  ggplot() +
  geom_bar(aes(x = Age_cat, y = count15, fill = NA, alpha = 1/10),stat = "identity") +
  geom_bar(aes(x = Age_cat, y = count20, fill=Gender, alpha = 1/10),stat = "identity") +
  
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-1500000,1500000),
                     breaks = seq(-1500000, 1500000, by = 500000)) +
  labs(title = "Projected Population for Peru in 2015 and 2020 by age and gender category",
       x = "Age Categories",
       y = "Population")+
  guides(fill=guide_legend(title="Population 
in 2020"), alpha=guide_legend(title="Population 
in 2015", label=F))
dev.off()

#####
# Deaths data
#####
dth <- read.csv("INPUT DATA/ORIGINAL/NOT ENAHO/Deaths1819.csv")
dth$Age_cat <- ifelse(dth$Age_cat == "\"05-09\"", "5-9", dth$Age_cat)
dth$Age_cat <- ifelse(dth$Age_cat == "\"10-14\"", "10-14", dth$Age_cat)
dth$Age_cat <- ifelse(dth$Age_cat == "05-09", "05-09", dth$Age_cat)
dth$Age_cat <- factor(dth$Age_cat, levels = unique(dth$Age_cat))

# Pyramid of deaths in 2018-2019
dth18 <- dth[dth$Year == "2018",]
dth19 <- dth[dth$Year == "2019",]

dth18$Deaths18 <- dth18$Deaths
dth19$Deaths19 <- dth19$Deaths

dth_wide <- merge(dth18[,c("Age_cat", "Gender", "Deaths18")],
                  dth19[,c("Age_cat", "Gender", "Deaths19")])

jpeg(file="OUTPUTDATA/PRE GRAPHS/DTH_PYR_1819_INEI.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
dth_wide %>% mutate(
  count18 = ifelse(Gender == "Male", Deaths18*(-1),
                   Deaths18*1),
  count19 = ifelse(Gender == "Male", Deaths19*(-1),
                   Deaths19*1))%>%
  ggplot() +
  geom_bar(aes(x = Age_cat, y = count19, fill = NA, alpha = 1/10),stat = "identity") +
  geom_bar(aes(x = Age_cat, y = count18, fill=Gender, alpha = 1/10),stat = "identity")+
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-30000,30000),
                     breaks = seq(-30000, 30000, by = 10000)) +
  labs(title = "Deaths registered in Peru from any cause by gender and age category 2018-2019",
       x = "Age Categories",
       y = "Registered Deaths")+
  guides(fill=guide_legend(title="Registered in
2018"), alpha=guide_legend(title="Registered in
2019", label=F))
dev.off()

# Now interpolate the data of population between 2015-2020
pops$Year <- as.numeric(pops$Year)
pops_proy <- pops
for (lev in unique(pops$Age_cat)){
  for (o_lev in unique(pops$Gender)){
    for (yy in 2016:2019){
      my_pops <- pops[pops$Age_cat == lev & 
                        pops$Gender == o_lev,]
      model <- lm(Pop ~ Year, my_pops)
      y_new = approx(my_pops$Year, my_pops$Pop, xout=yy)
      
      my_row <- as.data.frame(matrix(nrow = 1, ncol = 4))
      names(my_row) <- c("Age_cat", "Gender", "Year", "Pop")
      my_row$Age_cat <- lev
      my_row$Gender <- o_lev
      my_row$Year <- yy
      my_row$Pop <- round(y_new$y)
      pops_proy <- rbind(pops_proy, my_row)
    }
  }
}

# and get the probability of death in 2018-2019
pops1819 <- pops_proy[pops_proy$Year == 2018 | pops_proy$Year == 2019,]
dprob_1819 <- merge(pops1819, dth[,c(1:4)])

dprob_1819$Probs <- dprob_1819$Deaths / dprob_1819$Pop

dprob18 <- dprob_1819[dprob_1819$Year == 2018,]
dprob19 <- dprob_1819[dprob_1819$Year == 2019,]

dprob18$Probs18 <- dprob18$Probs
dprob19$Probs19 <- dprob19$Probs

probs1819d <- merge(dprob18[,c("Age_cat", "Gender", "Probs18")],
                    dprob19[,c("Age_cat", "Gender", "Probs19")])

probs1819d$Age_cat <- factor(probs1819d$Age_cat, levels = unique(pops$Age_cat))

# Comparative probabilities 2018-2019
jpeg(file="OUTPUTDATA/PRE GRAPHS/DTH_PROBS_1819_INEI.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
probs1819d %>% mutate(
  count18 = ifelse(Gender == "Male", Probs18*(-1),
                   Probs18*1),
  count19 = ifelse(Gender == "Male", Probs19*(-1),
                   Probs19*1))%>%
  ggplot() +
  geom_bar(aes(x = Age_cat, y = count19, fill = NA, alpha = 1/10),stat = "identity") +
  geom_bar(aes(x = Age_cat, y = count18, fill=Gender, alpha = 1/10),stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-0.1,0.1),
                     breaks = seq(-0.1, 0.1, by = 0.05)) +
  labs(title = "Proportion of registered death individuals out of the total population by gender and age category 2018-2019",
       x = "Age Categories",
       y = "Death probability")+
  guides(fill=guide_legend(title="Gender 2018"), alpha=guide_legend(title="2019",
                                                                    label=F))
dev.off()

# Only probabilities in 2019
dprob_1819$Age_cat <- factor(dprob_1819$Age_cat, levels = unique(pops$Age_cat))
jpeg(file="OUTPUTDATA/PRE GRAPHS/DTH_PROBS_19_INEI.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
dprob_1819[dprob_1819$Year == 2019,] %>% mutate(
  count = ifelse(Gender == "Male", Probs*(-1),
                   Probs*1))%>%
  ggplot(aes(x = Age_cat, y = count, fill=Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_y_continuous(limits = c(-0.1,0.1),
                     breaks = seq(-0.1, 0.1, by = 0.05)) +
  labs(title = "Proportion of registered death individuals out of the total population
by gender and age category 2019",
       x = "Age Categories",
       y = "Death probability")
dev.off()

#####
# Fertility
#####
# Children by age category
chxw <- read.csv("INPUT DATA/ORIGINAL/NOT ENAHO/ENDES/children_agecat.csv")
names(chxw) <- c("Age_cat", "Year", as.character(0:9), "10+")

# to long format
chxw_l <- gather(chxw, Num_child, Percentage, "0":"10+", factor_key=TRUE)

# As all complete 100%: Stacked bars grouped by year
jpeg(file="OUTPUTDATA/PRE GRAPHS/BTH_PROBS_BYAGE.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(chxw_l[chxw_l$Year == 2015 |
                chxw_l$Year == 2020,], aes(fill=Num_child, y=Percentage, x=Age_cat)) + 
  geom_bar(position="fill", stat="identity")+
  facet_grid(rows = vars(Year))+
  labs(title = "Number of children born alive by mother's age category for 2015 and 2020",
       x = "Mother's Age Categories",
       y = "Percentage")+
  guides(fill=guide_legend(title="Number of
Children"))+
  scale_fill_brewer(palette="Set3")
dev.off()

# Number of children for the oldest cohort
jpeg(file="OUTPUTDATA/PRE GRAPHS/BTH_PROBS_OLDEST.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(chxw_l[chxw_l$Age_cat == "45-49 ",], 
       aes(color=Num_child, y=Percentage, x=Year, group = Num_child)) + 
  geom_point() + geom_line() +
  labs(title = "Evolution of the proportion of women by total number of children born alive for 
those aged between 45 and 49 years old in Peru. Source: ENDES")+
  guides(color=guide_legend(title="Number 
of Children"))+
  scale_color_brewer(palette="Set3")
dev.off()

# Months after the last child
int_chd <- read.csv("INPUT DATA/ORIGINAL/NOT ENAHO/ENDES/child_interval.csv")
names(int_chd) <- c("Child_ord", "Year", "7-17", "18-23", "24-35", "36-47", "48+")

int_chd_l <- gather(int_chd, Months_from_birth, Percentage, "7-17":"48+", factor_key=TRUE)

# Stacked bars
jpeg(file="OUTPUTDATA/PRE GRAPHS/FURTH_BIRTHS_1.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(int_chd_l[int_chd_l$Year == 2015 |
                   int_chd_l$Year == 2020,],
       aes(fill=Months_from_birth, 
           y=Percentage, x=Child_ord)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_brewer(palette="Accent") +
  labs(title = "Proportion of further children births according to time elapsed since
last birth and ordinal number of further birth for 2015 and 2020",
       x = "Further birth ordinal number",
       y = "Percentage")+
  guides(fill=guide_legend(title="Months elapsed
since last birth"))+
  facet_grid(rows = vars(Year))
dev.off()

# Sided bars
jpeg(file="OUTPUTDATA/PRE GRAPHS/FURTH_BIRTHS_2.jpeg", 
     width=plt_w, height=plt_h, res = plt_res)
ggplot(int_chd_l[int_chd_l$Year == 2015 |
                   int_chd_l$Year == 2020,],
       aes(x=Months_from_birth, 
           y=Percentage, fill=Child_ord)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_brewer(palette="Accent") +
  labs(title = "Proportion of further children births according to time elapsed since
last birth and ordinal number of further birth for 2015 and 2020",
       x = "Months elapsed since last birth",
       y = "Percentage")+
  guides(fill=guide_legend(title="Further birth 
ordinal number"))+
  facet_grid(rows = vars(Year))
dev.off()


