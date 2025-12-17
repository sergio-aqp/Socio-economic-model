' This script filters the data from the original datasets downloaded from
INEIs webpage https://proyectos.inei.gob.pe/microdatos/ according to the 
following criteria:
- Only B,C & D socio economic levels
- Only intermediate sized cities (20k - 100k dwellings)
It also assigns the income generator category to individuals according to the
formula on the sumaria dictionary'

# Essential packages
library(foreign)
library(dplyr)
library(tidyr)
setwd("U:/THESIS SCRIPTING/R/INPUT DATA/")

#####
# Household level: NON-PANEL SUMARIA
#####
s_varsH <- c("MIEPERHO",
             "ESTRSOCIAL",
             "ESTRATO",
             "INGMO2HD",
             "GASHOG1D",
             "PERCEPHO")

filen <- c("ORIGINAL/NON-PANEL/SUMARIA-2015.sav", "ORIGINAL/NON-PANEL/SUMARIA-2016.sav", 
           "ORIGINAL/NON-PANEL/SUMARIA-2017.sav", "ORIGINAL/NON-PANEL/SUMARIA-2018.sav", 
           "ORIGINAL/NON-PANEL/SUMARIA-2019.sav")

res <- sapply(filen, read.spss, to.data.frame = T) # Open all spss files
filt <- lapply(res, function(x) x[,s_varsH]) # Only selected columns
filt <- lapply(filt, function(x) x[x[["ESTRSOCIAL"]] == "B" |
                                     x[["ESTRSOCIAL"]] == "C" |
                                     x[["ESTRSOCIAL"]] == "D",])

# Only medium size cities
filt <- lapply(filt, function(x) x[x[["ESTRATO"]] == "De 20,001 a 100,000 viviendas" |
                                     x[["ESTRATO"]] == "De 10,001 a 20,000 viviendas" |
                                     x[["ESTRATO"]] == "De 100 000 a 499 999 habitantes" |
                                     x[["ESTRATO"]] ==  "De 50 000 a 99 999 habitantes" |
                                     x[["ESTRATO"]] == "De 100,000 a 499,999 habitantes"|
                                     x[["ESTRATO"]] == "De 50,000 a 99,999 habitantes"|
                                     x[["ESTRATO"]] == "de 100,000 a 499,999 habitantes" |
                                     x[["ESTRATO"]] == "de 50,000 a 99,999 habitantes"
                                   ,])

# Add variables income per capita/ratio generators total members and
# drop unused Socio-economic levels
var1 <- "INGMO2HD"
var2 <- "PERCEPHO"

var3 <- "GASHOG1D"
var4 <- "MIEPERHO"

for (i in 1:length(filt)) {
  filt[[i]]$ESTRSOCIAL <- droplevels(filt[[i]]$ESTRSOCIAL)
  filt[[i]]$INCOME_PG <- filt[[i]][[var1]] / filt[[i]][[var2]]
  filt[[i]]$EXP_PM <- filt[[i]][[var3]] / filt[[i]][[var4]]
}


# Save
for (i in 1:length(filt)){
  datafn <- paste("FILTERED/NON-PANEL/SUMARIA", i + 2014, sep = "_")
  datafn <- paste(datafn, ".Rds", sep = "")
  myobjct <- filt[[i]]
  saveRDS(myobjct,file=datafn)
}

#####
# Household level: PANEL SUMARIA
#####
sumaria <- read.spss('ORIGINAL/PANEL/sumaria-2015-2019-panel.sav', use.value.label=TRUE, 
                     to.data.frame=TRUE)

s_varsH <- c("numpanh",
             "mieperho_15", "mieperho_16", # household members
             "estrsocial_15", "estrsocial_16", # Social strata
             "ingmo2hd_15", "ingmo2hd_16", # Income
             "gashog1d_15", "gashog1d_16", # Expenses
             "percepho_15", "percepho_16", # members with income
             "estrato_15", "estrato_16") # City size

# Select variables
full_v <- sumaria %>% select(all_of(s_varsH))

# Select participating SELs
all_v <- full_v[full_v$estrsocial_15 == "b" |
                  full_v$estrsocial_16 == "b" |
                  full_v$estrsocial_15 == "c" |
                  full_v$estrsocial_16 == "c" |
                  full_v$estrsocial_15 == "d" |
                  full_v$estrsocial_16 == "d",]

# Drop levels and turn to uppercase
estrsocial <- c("estrsocial_15", "estrsocial_16")

for (i in seq(estrsocial)){
  all_v[[estrsocial[i]]] <- droplevels(all_v[[estrsocial[i]]])
  all_v[[estrsocial[i]]] <- as.factor(toupper(as.character(all_v[[estrsocial[i]]])))
}

# Select only those that live in medium sized cities
all_v <- all_v[all_v$estrato_15 == "de 10,001 a 20,000 viviendas" |
                 all_v$estrato_15 == "de 20,001 a 100,000 viviendas" |
                 all_v$estrato_16 == "de 10,001 a 20,000 viviendas" |
                 all_v$estrato_16 == "de 20,001 a 100,000 viviendas" 
               ,]

# Select only those that have money/hh_mem data for 2015-2016
hhs <- all_v[!is.na(all_v$ingmo2hd_15) &
               !is.na(all_v$ingmo2hd_16) &
               !is.na(all_v$gashog1d_15) &
               !is.na(all_v$gashog1d_16),]

hhs <- hhs[!is.na(hhs$mieperho_15) &
             !is.na(hhs$mieperho_16),]

# Add variables income per generator and expeses per capita
hhs$INCOME_PG_15 <- hhs$ingmo2hd_15 / hhs$percepho_15
hhs$EXP_PM_15 <- hhs$gashog1d_15 / hhs$mieperho_15

hhs$INCOME_PG_16 <- hhs$ingmo2hd_16 / hhs$percepho_16
hhs$EXP_PM_16 <- hhs$gashog1d_16 / hhs$mieperho_16

# Save
saveRDS(hhs,file="FILTERED/PANEL/sumaria-2015-2019-panel.Rds")

#####
# Individual level: Module 500 Non-panel - income generator
#####
ind_dta <- read.spss ("ORIGINAL/NON-PANEL/Enaho01A-2015-500.sav", to.data.frame = T)

ind_dta <- ind_dta[ind_dta$ESTRATO == "De 10,001 a 20,000 viviendas" |
                     ind_dta$ESTRATO == "De 20,001 a 100,000 viviendas",]

ind_dta <- ind_dta[ind_dta$P204 == "Si",]

cho_vars <- c("P207", "P208A", "P204", "P203", "P208A", "I524A1",
              "I530A", "I538A1", "D529t", "D540t", "I541a", "D544t",  # I536, I543
              "D556t1", "D556t2", "D557t" ,"D558t")

ind_dta <- ind_dta[,cho_vars]

cut_sers <- seq(-1, 79, 5)
cut_sers <- c(cut_sers, Inf)

ind_dta$P208_c <- cut(ind_dta$P208A,
                      breaks = cut_sers, 
                      include.lowest = TRUE,
                      labels = 1:(length(cut_sers) - 1))

ind_dta$P207 <- tolower(ind_dta$P207) # gender to lower to fit imported transition probs

# Determine Income generators in non-panel data
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
           !is.na(less_idta$D558t), "Yes", "No")

ind_dta[ind_dta$P203 != "Trabajador Hogar" &
          ind_dta$P203 != "Pensionista" &
          ind_dta$P208A >= 14, ]$INC_GEN <- less_idta$INC_GEN

# Save
saveRDS(ind_dta,file="FILTERED/NON-PANEL/Enaho01A-2015-500.Rds")

#####
# Individual level: Module 200 Non-panel - general characteristics
#####
genage_dta <- read.spss("ORIGINAL/NON-PANEL/Enaho01-2015-200.sav", to.data.frame = T)

# Only intermediate and large cities
genage_dta <- genage_dta[genage_dta$ESTRATO == "De 10,001 a 20,000 viviendas" |
                           genage_dta$ESTRATO == "De 20,001 a 100,000 viviendas",]

# Only those that have data on age and gender
genage_dta <- genage_dta[!is.na(genage_dta$P207) &
                           !is.na(genage_dta$P208A),]

# Classify ages
genage_dta$P208_c <- cut(genage_dta$P208A,
                         breaks = cut_sers, 
                         include.lowest = TRUE,
                         labels = 1:(length(cut_sers) - 1))

genage_dta$P207 <- tolower(genage_dta$P207) # gender to lower to fit imported transition probs

# Save
saveRDS(genage_dta,file="FILTERED/NON-PANEL/Enaho01-2015-200.Rds")

#####
# Individual level: Panel Module 500 - Income generators
#####
# To get income generation status transition probs
incg_dta <- read.spss("ORIGINAL/PANEL/enaho01-2015-2019-500-panel_01.sav", to.data.frame = T)

s_varsH <- c("perpanel_1516", # If participated in panel 15-16
             "estrato_15", "estrato_16", # City size
             "P203_15", "P203_16", # Role
             "p208a_15", "p208a_16", # Age
             "p207_15", "p207_16", # Gender
             "I524A1_15", "I524A1_16",
             "D529t_15", "D529t_16",
             "I530A_15", "I530A_16",
             "I538A1_15", "I538A1_16",
             "D540t_15", "D540t_16",
             "I541a_15", "I541a_16",
             "D544t_15", "D544t_16",
             "D556t1_15", "D556t1_16",
             "D556t2_15", "D556t2_16",
             "D557t_15", "D557t_16",
             "D558t_15", "D558t_16")

s_varsH <- tolower(s_varsH)

igen_dta <- incg_dta[,s_varsH]

igen_dta <- igen_dta[igen_dta$perpanel_1516 == 1 & # Only people that participated
                       !is.na(igen_dta$perpanel_1516) & # in 15-16
                       !is.na(igen_dta$p203_15) & # Role info 2015
                       !is.na(igen_dta$p203_16) & # role info 2016
                       !is.na(igen_dta$p208a_15) & # age 2015
                       !is.na(igen_dta$p208a_16) & # age 2016
                       !is.na(igen_dta$p207_15) & # gender 2015
                       !is.na(igen_dta$p207_16),]  # gender 2016

igen_dta <- igen_dta[igen_dta$estrato_15 == 2 | # Only intermediate cities
                       igen_dta$estrato_15 == 3 |
                       igen_dta$estrato_16 == 2 |
                       igen_dta$estrato_16 == 3,]

# Classify ages
igen_dta$p208_c_15 <- cut(igen_dta$p208a_15,
                          breaks = cut_sers, 
                          include.lowest = TRUE,
                          labels = 1:(length(cut_sers) - 1))

igen_dta$p208_c_16 <- cut(igen_dta$p208a_16,
                          breaks = cut_sers, 
                          include.lowest = TRUE,
                          labels = 1:(length(cut_sers) - 1))

# Turn gender to character (in this dataset is numeric)
igen_dta$p207_15 <- ifelse(igen_dta$p207_15 == 1, "hombre", "mujer")
igen_dta$p207_16 <- ifelse(igen_dta$p207_16 == 1, "hombre", "mujer")

# Determine income generators in panel data (2015 and 2016)
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

# Save
saveRDS(igen_dta,file="FILTERED/PANEL/enaho01-2015-2019-500-panel_01.Rds")

#####
# Individual level: Panel Module 200 - general characteristics
#####
# To get move in/move out probabilites
mod200 <- read.spss('ORIGINAL/PANEL/enaho01-2015-2019-200-panel.sav', 
                    use.value.label=TRUE, to.data.frame=TRUE)

# Filter the dataset at individual level
s_varsI <- c("p208a_15", "p208a_16", # age
             "p207_15", "p207_16", # sex
             "p217_15", "p217_16", # move in, move out
             "estrato_15", "estrato_16",
             "p216_16", # Persona nueva 
             "hpanel_1516") # Household participating in 2 years

indv <- mod200 %>% dplyr::select(all_of(s_varsI))

# Only intermediate cities
indv <- indv[indv$estrato_15 == "de 10,001 a 20,000 viviendas" |
               indv$estrato_15 == "de 20,001 a 100,000 viviendas" |
               indv$estrato_16 == "de 10,001 a 20,000 viviendas" |
               indv$estrato_16 == "de 20,001 a 100,000 viviendas",]

# Assign age category to people
indv$age_cat_15 <- cut(indv$p208a_15,
                       breaks=cut_sers,
                       labels = 1:(length(cut_sers) - 1),
                       include.lowest = TRUE)

indv$age_cat_16 <- cut(indv$p208a_16,
                       breaks=cut_sers,
                       labels = 1:(length(cut_sers) - 1),
                       include.lowest = TRUE)

# Only HHs participating in 2015-2016
indv <- indv[indv$hpanel_1516 == 1,]

# Only work with those that have answered in 2015
my_indv <- indv[!is.na(indv$p208a_15) & !is.na(indv$p207_15),]

# save
saveRDS(my_indv,file="FILTERED/PANEL/enaho01-2015-2019-200-panel.Rds")




#####
# Project births and death probs
#####
# This is good projection but make less categories! only up to 4 children 
# then 5+
# Sum before turning to long

# Import data
your_df <- read.csv("ORIGINAL/NOT ENAHO/ENDES/children_agecat.csv")

# Trim spaces in age categories
your_df$Age_cat <- trimws(your_df$Age_cat)

# Change names of the categories
names(your_df) <- c("Age_cat", "Year", as.character(0:9), "10+")

# Change year 2020 for 2019
your_df$Year <- ifelse(your_df$Year == 2020, 2019, your_df$Year)

# Cut number of children in 5+
your_df$`5+` <- your_df$`5` + your_df$`6` + your_df$`7` + 
  your_df$`8` + your_df$`9` + your_df$`10+`

# Only take relevant variables
mod_df <- your_df[, c("Age_cat", "Year", "0", "1", "2", "3", "4", "5+")]

# Transform to long format
chxw_l <- gather(mod_df, Num_child, Percentage, "0":"5+", factor_key=TRUE)

# Create empty data frame to accumulate data
out_df <- as.data.frame(matrix(ncol = 4, nrow = 0))
names(out_df) <- c("Year", "Percentage", "Age_cat", "Num_child")

# Iterate through age and hild number categories to predictions until 2040
for (age_c in unique(chxw_l$Age_cat)){
  for (num_c in unique(chxw_l$Num_child)){
    my_lm <- lm(Percentage ~ Year, chxw_l[chxw_l$Age_cat==age_c &
                                            chxw_l$Num_child==num_c,]) # Only for X0
    new_df <- data.frame(Year = c(2020:2040))
    new_df$Percentage <- predict(my_lm, newdata = new_df)
    new_df$Age_cat <- age_c
    new_df$Num_child <- num_c
    out_df <- rbind(out_df, new_df)
  }
}

# Alter those predicions above 100% or below 0%
out_df$Percentage <- ifelse(out_df$Percentage > 100, 100, out_df$Percentage)
out_df$Percentage <- ifelse(out_df$Percentage < 0, 0, out_df$Percentage)

# Join predictions with actual data
out_df <- rbind(chxw_l, out_df)

# Plot one age category to see trend
library(ggplot2)

ggplot(out_df[out_df$Age_cat == "45-49",], 
       aes(color=Num_child, y=Percentage, x=Year, group = Num_child)) + 
  geom_point(aes(shape = Num_child)) + geom_line() +
  labs(title = "Evolution of the proportion of women by total number of children born alive for 
those aged between 45 and 49 years old in Peru. Source: ENDES")+
  guides(color=guide_legend(title="Number 
of Children"))+
  scale_color_brewer(palette="Set2")

# Add probabilities of adding one child to current count
out_df$Add_child <- NA
out_df$Num_child_n <- as.numeric(as.character(out_df$Num_child))
out_df$Num_child_n <- ifelse(is.na(out_df$Num_child_n), 5, out_df$Num_child_n)

for (age_c in unique(out_df$Age_cat)){
  for (nm_c in unique(out_df$Num_child_n)){
    for (yy in unique(out_df$Year)){
      nm_c <- as.numeric(nm_c)
      out_df[out_df$Num_child_n == nm_c &
               out_df$Age_cat == age_c &
               out_df$Year == yy,]$Add_child <- sum(out_df[as.numeric(out_df$Num_child_n) > nm_c &
                                                             out_df$Age_cat == age_c &
                                                             out_df$Year == yy,]$Percentage)
    }
  }
}


# When none child, the prob of having one is the negation of keeping 0
out_df[out_df$Num_child == 0,]$Add_child <- 100 -  out_df[out_df$Num_child == 0,]$Percentage

# The max probabilities are at the max age category, cut the rest of probs at that limit
for (age_c in unique(out_df$Age_cat)){
  for (num_c in unique(out_df$Num_child)){
    for (yy in unique(out_df$Year)){
      out_df[out_df$Year == yy &
               out_df$Num_child == num_c &
               out_df$Age_cat == age_c,]$Add_child <- ifelse(out_df[out_df$Year == yy &
                                                                      out_df$Num_child == num_c &
                                                                      out_df$Age_cat == age_c,]$Add_child > 
                                                               out_df[out_df$Year == yy &
                                                                        out_df$Num_child == num_c &
                                                                        out_df$Age_cat == "45-49",]$Add_child,
                                                             out_df[out_df$Year == yy &
                                                                      out_df$Num_child == num_c &
                                                                      out_df$Age_cat == "45-49",]$Add_child,
                                                             out_df[out_df$Year == yy &
                                                                      out_df$Num_child == num_c &
                                                                      out_df$Age_cat == age_c,]$Add_child)
    }
  }
}

# Turn probs to numbers between 0 to 1
out_df$Add_child_1 <- out_df$Add_child/100

# Divide the probs in five, one for each year of the age category
out_df$Add_chl_pyy <- out_df$Add_child_1/5

# Export birth distribution probs for sim
brth_dist <- out_df[,c("Age_cat", "Year", "Num_child", "Percentage")]
brth_dist$probs <- brth_dist$Percentage/100
write.csv(brth_dist, "FILTERED/NOT ENAHO/ENDES/projected_children_agecat.csv", 
          row.names=FALSE)

# Export birth distribution probs for document
ggplot(brth_dist, aes(Year, Age_cat)) + geom_tile(aes(fill = probs)) + 
  facet_grid(vars(Num_child)) + scale_fill_gradient(
    low = "white", high = "black", na.value = NA) +
  labs(title = "Projected probabilties of giving birth a number of children (as facets)
by women's age category between 2015 and 2040",
       x = "Year",
       y = "Fertile women age categories")+
  guides(fill=guide_legend(title="Probabilties"))

# Export birth transition probs for model
brth_trns <- out_df[,c("Age_cat", "Year", "Num_child", "Add_child_1",
                       "Add_chl_pyy")]
write.csv(brth_trns, "FILTERED/NOT ENAHO/ENDES/add_children_prbs.csv", 
          row.names=FALSE)

# Export birth transition probs for document
ggplot(brth_trns, aes(Year, Age_cat)) + geom_tile(aes(fill = Add_child_1)) + 
  facet_grid(vars(Num_child)) + scale_fill_gradient(
    low = "white", high = "black", na.value = NA) +
  labs (title="Probabilities of adding one child to children curently born alive 
per women by age and number of current children categories",
        x = "Year",
        y = "Fertile women age categories") +
  guides(fill=guide_legend(title="Probabilties"))

#####
# Death probs
#####
# Death probs were not projected because there is only three years of usable data
# 2017, 2018 and 2019. I have access to the two latest. I could ask for 2017
# but would be worthless. Can you project with three years?

# Get event probabilities of death by age and gender categories
# Population data
pops <- read.csv("ORIGINAL/NOT ENAHO/estimated_projected_pop_30jun_5year_agecat_gender.csv")
pops$Age_cat <- factor(pops$Age_cat, levels = unique(pops$Age_cat))

# Interpolate the population data 2015-2020
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

# Death data
dth <- read.csv("ORIGINAL/NOT ENAHO/Deaths1819.csv")

dth$Age_cat <- ifelse(dth$Age_cat == "\"05-09\"", "5-9", dth$Age_cat)
dth$Age_cat <- ifelse(dth$Age_cat == "\"10-14\"", "10-14", dth$Age_cat)
dth$Age_cat <- ifelse(dth$Age_cat == "05-09", "05-09", dth$Age_cat)
dth$Age_cat <- factor(dth$Age_cat, levels = unique(dth$Age_cat))

# Get the probability of death in 2018-2019
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

# Save death probs 18-19 wide
write.csv(probs1819d, "FILTERED/NOT ENAHO/LIVE_DEATH/DEATH_PROBS_w.csv", 
          row.names=FALSE)

# Save death probs long format
dth_long <- gather(probs1819d, Year, Probs, Probs18:Probs19)
dth_long$Year <- ifelse(dth_long$Year == "Probs18", 2018, 2019)

write.csv(dth_long, "FILTERED/NOT ENAHO/LIVE_DEATH/DEATH_PROBS_l.csv", 
          row.names=FALSE)
