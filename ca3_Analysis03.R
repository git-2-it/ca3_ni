# ca3_ni

# Several datasets required apart from the prescription data itslef
# Practice data
# Post code data (for practice geo-location)

library(dplyr)

#------------------
# functions setup
#------------------
# Correlation panel
panel.cor <- function(x, y){
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- round(cor(x, y), digits=2)
        txt <- paste0("R = ", r)
        cex.cor <- 0.8/strwidth(txt)
        text(0.6, 0.6, txt, cex = cex.cor * r)
}
# Customize upper panel
upper.panel<-function(x, y){
        points(x,y, pch = 19 ) # , col = my_cols[iris$Species])
}

# # Create the plots
# pairs(pr_main[pair_check], 
#       lower.panel = panel.cor,
#       upper.panel = upper.panel)

# --------------------------------------------------
# Get practise data
# --------------------------------------------------

docs_in <- read.csv("data/cleaned_practice_data.csv")

head(docs_in)

# Examine structure of the file
str(docs_in)


# --------------------------------------------------
# Read prescription items data
# --------------------------------------------------

# Open data file to review contents
#p_data <- read.csv("data/prescription_summary.csv")
p_data <- read.csv("data/summary_info.csv")

#p_data <- subset(p_data, Practice != 157)

head(p_data)

# look at the structure of the file
str(p_data)

summary(p_data)
head(p_data)

# ------------------------------------------------------
# Check the practice list for missing data values in either dataset


p_data$Practice <- as.factor(p_data$Practice)
docs_in$PracNo <- as.factor(docs_in$PracNo)

str(p_data)
str(docs_in)

colSums(is.na(p_data))


# Join practice and items data
# pr_main is main practice data
pr_main <- subset(docs_in, 
                  select = c(PracNo, PracticeName, Address1, Postcode, LCG, Registered_Patients, Town, County))
str(pr_main)
colnames(pr_main)[colnames(pr_main) == "PracNo"] <-"Practice"
colnames(pr_main)

pr_main2 <- subset(p_data, select = c("Practice", "MonthlyAvg_P", "Sum_GT") )
pr_main2 <-unique(pr_main2)

#pr_main$Practice <- as.numeric(pr_main$Practice)
#pr_main2$Practice <- as.numeric(pr_main2$Practice)
#pr_main2$Practice <- as.factor(pr_main2$Practice)
#pr_main$Practice <- as.factor(pr_main$Practice)

pr_main  <- left_join(pr_main, pr_main2, 
                      by = c("Practice" = "Practice")
)


# Plot some visuals to help understand the shape of the data set

library("viridis")
# hold the param settings
# Store default plot settings
opar <- par()


par(mfrow = c(1, 2))

# Plot items prescribed vs patients
scatter.smooth(x = pr_main$Registered_Patients,
               y = pr_main$Sum_GT,
               main = "Patient vs Total Prescribed Item numbers",
               xlab = "Patients",
               ylab = "# Items",
)


text(pr_main$Registered_Patients, pr_main$Sum_GT,
     labels= pr_main$Practice,
     cex=0.9, font=2)

scatter.smooth(x = pr_main$Registered_Patients,
               y = pr_main$MonthlyAvg_P,
               main = "Patient vs Avg Prescribed Item numbers",
               xlab = "Patients",
               ylab = "# Items",
)
text(pr_main$Registered_Patients, pr_main$MonthlyAvg_P,
     labels= pr_main$Practice,
     cex=0.9, font=2)


# Prac no 157
chk  <- subset(pr_main, pr_main$Practice == 157)
chk

# Uni Health centre - remove
pr_main <- subset(pr_main, Practice != 157)

par(mfrow = c(1, 1))

# Replot, colour coding
scatter.smooth(x = pr_main$Registered_Patients,
               y = pr_main$Sum_GT,
               main = "Patient vs Prescribed Item numbers",
               sub = "By LCG",
               xlab = "Patients",
               ylab = "# Items",
               col = pr_main$LCG,
)

scatter.smooth(x = pr_main$Registered_Patients,
               y = pr_main$Sum_GT,
               main = "Patient vs Prescription numbers",
               sub = "By Town",
               xlab = "Patients",
               ylab = "# Prescriptions",
               col = pr_main$Town
)

par(mfrow = c(1, 1))

# Confirmed no obvious patterns by town/LCG

#--------------------------------------
# Check for outliers
#--------------------------------------

# par(mfrow = c(1, 3))

boxplot(x = pr_main$Registered_Patients,
        main = "Registered_Patients",
        sub = paste("outlier rows: ", 
                    boxplot.stats(x = pr_main$Registered_Patients)$out)
)

# A few larger than normal practices, need to identify which they are
chk  <- unique(subset(pr_main, pr_main$Registered_Patients > 14000 
                      #                      select = c("PracNo", "Registered_Patients", "Town", "County")
))
chk
# 5 larger than average practices, based on town names its a mix of urban and rural practices
# the largest practice is PracNo 306 in Castlederg with over 15 000 patients, 12 times larger 
# than the smallest practice, PracNo 55 @ 1198 patients
# Larger practices becomming the new norm (source)
# The data can't be removed, too valuable

# Check total items prescribed
boxplot(x = pr_main$Sum_GT,
        main = "Sum_GT",
        sub = paste("outlier rows: ", 
                    boxplot.stats(x = pr_main$Sum_GT )$out)
)
# A few larger than normal practices, need to identify which they are
chk  <- unique(subset(pr_main, pr_main$Sum_GT > 600000, 
))
chk

# At least one at 800 000 items, note and keep

# Check Avg items prescribed
boxplot(x = pr_main$MonthlyAvg_P,
        main = "MonthlyAvg_P",
        sub = paste("outlier rows: ", 
                    boxplot.stats(x = pr_main$MonthlyAvg_P )$out)
)
# A few larger than normal practices, need to identify which they are
chk  <- unique(subset(pr_main, pr_main$MonthlyAvg_P > 50000, 
))
chk

# Similar list to before for overall totals, not surprising


# Create items / patient ratios for total and average items

pr_main$item_total_patient_ratio <- pr_main$Sum_GT / pr_main$Registered_Patients
pr_main$items_avg_patient_ratio <- pr_main$MonthlyAvg_P / pr_main$Registered_Patients

# Check for outliers and view dist curves
par(mfrow = c(1, 2))

boxplot(x = pr_main$item_total_patient_ratio,
        main = "Total Items / Patient",
        sub = paste("outlier rows: ", 
                    boxplot.stats(x = pr_main$item_total_patient_ratio)$out)
)

# Outliers on both end of the scale, look at the dist curve
dense_p <- density(pr_main$item_total_patient_ratio)

plot(dense_p,
     main = "Kernel Density of Total Items / Patient"
) 
polygon(dense_p, col="red", border="red")


boxplot(x = pr_main$items_avg_patient_ratio,
        main = "Avg Items / Patient",
        sub = paste("outlier rows: ", 
                    boxplot.stats(x = pr_main$items_avg_patient_ratio)$out)
)

# Outliers on both end of the scale, look at the dist curve
dense_p <- density(pr_main$items_avg_patient_ratio)

plot(dense_p,
     main = "Kernel Density of Avg Items / Patient"
) 
polygon(dense_p, col="red", border="red")


# Hist is more graphic?
hist(pr_main$items_avg_patient_ratio,
     breaks=20, col="red")



#---------------------------------------------------------------------------
# Categorise the practices data 
#---------------------------------------------------------------------------

# Use double-tab for structure of sapply()
numeric_variable_list <- sapply(pr_main, is.numeric)
numeric_variable_list["year"] <- FALSE
numeric_variable_list

# create a subset of the numerical data
numerical_data <- pr_main[numeric_variable_list]
colnames(numerical_data)

# build summary table
numerical_summary <- do.call(cbind, lapply(numerical_data, summary))
numerical_summary


# Categories
#---------------------------------------------------------------------------

# Use to categorise practices by size using patient number quartiles
# Categorisation by quartiles

# Practice size, patients
patient_1stq  <- numerical_summary["1st Qu.",'Registered_Patients']
patient_mean  <- numerical_summary["Mean",'Registered_Patients']
patient_3rdq  <- numerical_summary["3rd Qu.",'Registered_Patients']

pr_main <- pr_main %>% mutate(practice_size = 
                                      case_when(Registered_Patients <= patient_1stq  ~ '1',
                                                Registered_Patients > patient_1stq  & Registered_Patients <= patient_mean ~ '2',
                                                Registered_Patients > patient_mean  & Registered_Patients <= patient_3rdq ~ '3',
                                                Registered_Patients > patient_3rdq ~ '4'
                                      ))

# Practice category, items total
Sum_GT_1stq  <- numerical_summary["1st Qu.",'Sum_GT']
Sum_GT_mean  <- numerical_summary["Mean",'Sum_GT']
Sum_GT_3rdq  <- numerical_summary["3rd Qu.",'Sum_GT']

pr_main <- pr_main %>% mutate(items_total_category = 
                                      case_when(Sum_GT <= Sum_GT_1stq  ~ '1',
                                                Sum_GT > Sum_GT_1stq  & Sum_GT <= Sum_GT_mean ~ '2',
                                                Sum_GT > Sum_GT_mean  & Sum_GT <= Sum_GT_3rdq ~ '3',
                                                Sum_GT > Sum_GT_3rdq ~ '4'
                                      ))

# Items total Patient
item_total_patient_ratio_1stq  <- numerical_summary["1st Qu.",'item_total_patient_ratio']
item_total_patient_ratio_mean  <- numerical_summary["Mean",'item_total_patient_ratio']
item_total_patient_ratio_3rdq  <- numerical_summary["3rd Qu.",'item_total_patient_ratio']

pr_main <- pr_main %>% mutate(ratio_avg = 
                                      case_when(item_total_patient_ratio <= item_total_patient_ratio_1stq  ~ '1',
                                                item_total_patient_ratio > item_total_patient_ratio_1stq  & item_total_patient_ratio <= item_total_patient_ratio_mean ~ '2',
                                                item_total_patient_ratio > item_total_patient_ratio_mean  & item_total_patient_ratio <= item_total_patient_ratio_3rdq ~ '3',
                                                item_total_patient_ratio > item_total_patient_ratio_3rdq ~ '4'
                                      ))


# Items avg patient
items_avg_patient_ratio_1stq  <- numerical_summary["1st Qu.",'items_avg_patient_ratio']
items_avg_patient_ratio_mean  <- numerical_summary["Mean",'items_avg_patient_ratio']
items_avg_patient_ratio_3rdq  <- numerical_summary["3rd Qu.",'items_avg_patient_ratio']

pr_main <- pr_main %>% mutate(items_total_ratio = 
                                      case_when(items_avg_patient_ratio <= items_avg_patient_ratio_1stq  ~ '1',
                                                items_avg_patient_ratio > items_avg_patient_ratio_1stq  & items_avg_patient_ratio <= items_avg_patient_ratio_mean ~ '2',
                                                items_avg_patient_ratio > items_avg_patient_ratio_mean  & items_avg_patient_ratio <= items_avg_patient_ratio_3rdq ~ '3',
                                                items_avg_patient_ratio > items_avg_patient_ratio_3rdq ~ '4'
                                      ))


str(pr_main)

# reorder based on avg item patient ratios
plot_data <- pr_main
plot_data$Practice <- factor(plot_data$Practice, 
                             levels = plot_data$Practice[order(plot_data$items_avg_patient_ratio)])

library("ggplot2") 

plot <- ggplot(plot_data, aes(x = Practice, y = items_avg_patient_ratio, fill=LCG) ) + 
        geom_bar(stat = "identity" ) + scale_fill_viridis_d() +
        ggtitle("Avg Items per patient per month") + 
        labs(title="Avg Items per patient \nCoded by LCG",
             x ="Practice No", y = "Avg Items")

print(plot)

# Majority of data appears spread over the LCGs, with exceptions at the higher end of the scale
# Note overall curve shape, matches tailed dist curve - kurtosis

# Notable outliying data at less than 20 items/patient
remove_practices  <- subset(pr_main, pr_main$items_avg_patient_ratio  < 2 )
remove_practices

# These are all flagged as 2018 practices, ie practices that existed in 2018 but not subsequently
# Most likely they have closed, merged or otherwise changed in the interviening period
# Difficult to know why, or which came first, low ratio == low amt of business -> closure
# closure upcoming == practice wind down -> low ratios
# Either way, remove at they outlie too far

pr_main <- pr_main[!pr_main$Practice %in% remove_practices$Practice, ]

# Rereun data
#---------------------------------------------------------------------------

numerical_data <- pr_main[numeric_variable_list]

# build summary table
numerical_summary <- do.call(cbind, lapply(numerical_data, summary))

# Use to categorise practices by size using patient number quartiles
# Categorisation by quartiles

# Practice size, patients
patient_1stq  <- numerical_summary["1st Qu.",'Registered_Patients']
patient_mean  <- numerical_summary["Mean",'Registered_Patients']
patient_3rdq  <- numerical_summary["3rd Qu.",'Registered_Patients']

pr_main <- pr_main %>% mutate(practice_size = 
                                      case_when(Registered_Patients <= patient_1stq  ~ '1',
                                                Registered_Patients > patient_1stq  & Registered_Patients <= patient_mean ~ '2',
                                                Registered_Patients > patient_mean  & Registered_Patients <= patient_3rdq ~ '3',
                                                Registered_Patients > patient_3rdq ~ '4'
                                      ))

# Practice category, items total
Sum_GT_1stq  <- numerical_summary["1st Qu.",'Sum_GT']
Sum_GT_mean  <- numerical_summary["Mean",'Sum_GT']
Sum_GT_3rdq  <- numerical_summary["3rd Qu.",'Sum_GT']

pr_main <- pr_main %>% mutate(items_total_category = 
                                      case_when(Sum_GT <= Sum_GT_1stq  ~ '1',
                                                Sum_GT > Sum_GT_1stq  & Sum_GT <= Sum_GT_mean ~ '2',
                                                Sum_GT > Sum_GT_mean  & Sum_GT <= Sum_GT_3rdq ~ '3',
                                                Sum_GT > Sum_GT_3rdq ~ '4'
                                      ))

# Items total Patient
item_total_patient_ratio_1stq  <- numerical_summary["1st Qu.",'item_total_patient_ratio']
item_total_patient_ratio_mean  <- numerical_summary["Mean",'item_total_patient_ratio']
item_total_patient_ratio_3rdq  <- numerical_summary["3rd Qu.",'item_total_patient_ratio']

pr_main <- pr_main %>% mutate(ratio_avg = 
                                      case_when(item_total_patient_ratio <= item_total_patient_ratio_1stq  ~ '1',
                                                item_total_patient_ratio > item_total_patient_ratio_1stq  & item_total_patient_ratio <= item_total_patient_ratio_mean ~ '2',
                                                item_total_patient_ratio > item_total_patient_ratio_mean  & item_total_patient_ratio <= item_total_patient_ratio_3rdq ~ '3',
                                                item_total_patient_ratio > item_total_patient_ratio_3rdq ~ '4'
                                      ))


# Items avg patient
items_avg_patient_ratio_1stq  <- numerical_summary["1st Qu.",'items_avg_patient_ratio']
items_avg_patient_ratio_mean  <- numerical_summary["Mean",'items_avg_patient_ratio']
items_avg_patient_ratio_3rdq  <- numerical_summary["3rd Qu.",'items_avg_patient_ratio']

pr_main <- pr_main %>% mutate(items_total_ratio = 
                                      case_when(items_avg_patient_ratio <= items_avg_patient_ratio_1stq  ~ '1',
                                                items_avg_patient_ratio > items_avg_patient_ratio_1stq  & items_avg_patient_ratio <= items_avg_patient_ratio_mean ~ '2',
                                                items_avg_patient_ratio > items_avg_patient_ratio_mean  & items_avg_patient_ratio <= items_avg_patient_ratio_3rdq ~ '3',
                                                items_avg_patient_ratio > items_avg_patient_ratio_3rdq ~ '4'
                                      ))


str(pr_main)

# reorder based on avg item patient ratios
plot_data <- pr_main
plot_data$Practice <- factor(plot_data$Practice, 
                             levels = plot_data$Practice[order(plot_data$items_avg_patient_ratio)])

plot <- ggplot(plot_data, aes(x = Practice, y = items_avg_patient_ratio, fill=LCG) ) + 
        geom_bar(stat = "identity" ) + scale_fill_viridis_d() +
        ggtitle("Avg Items per patient per month") + 
        labs(title="Avg Items per patient \nCoded by LCG",
             x ="Practice No", y = "Avg Items")

print(plot)

## end rerun


par(mfrow = c(1, 2))

boxplot(x = pr_main$items_avg_patient_ratio,
        main = "Avg Items / Patient",
        sub = paste("outlier rows: ", 
                    boxplot.stats(x = pr_main$items_avg_patient_ratio)$out)
)

# Outliers on both end of the scale, look at the dist curve
dense_p <- density(pr_main$items_avg_patient_ratio)

plot(dense_p,
     main = "Kernel Density of Avg Items / Patient"
) 
polygon(dense_p, col="red", border="red")

par(mfrow = c(1, 1))

# Drop unused factors
pr_main <- droplevels(pr_main)


# re=check for NAs
colSums(is.na(pr_main))

# Reset practice id factor
pr_main$Practice <- as.factor(pr_main$Practice)

str(pr_main)

# plot <- ggplot(pr_main, aes(x = LCG, y = items_avg_patient_ratio , color = Registered_Patients))
# plot <- plot + geom_point() + scale_color_viridis()
# print((plot))

opar <- par(no.readonly = TRUE)

par(mfrow = c(1, 3))

hist(pr_main$Registered_Patients,  main = "Registered Patients", xlab = "")

hist(pr_main$Sum_GT ,  main = "Total  Items per Patient", xlab = "")

hist(pr_main$items_avg_patient_ratio ,  main = "Avg Items per Patient per Month", xlab = "")

par(mfrow = c(1, 1))

colnames(pr_main)
pr_main$practice_size <- as.numeric(pr_main$practice_size)
pr_main$LCG_int <- as.integer(pr_main$LCG)

pair_variables <- c("Registered_Patients",
                    "Sum_GT",
                    "items_avg_patient_ratio",
                    "MonthlyAvg_P",
                    "LCG_int"
)

pairs(pr_main[pair_variables],
      lower.panel = panel.cor,
      upper.panel = upper.panel)

library(corrplot)
corrplot(cor(pr_main[pair_variables]))

cor(pr_main$Registered_Patients, pr_main$Sum_GT)
cor(pr_main$Registered_Patients, pr_main$items_avg_patient_ratio)
cor(pr_main$Registered_Patients, pr_main$MonthlyAvg_P)

# Strong correllation bewtween  both > 0.93
# Registered_Patients : Sum_GT pr_main$MonthlyAvg_P)
# Weak, inverse, relationship, between registered patients and average items per patient per month, -0.1289
cor(pr_main$Sum_GT, pr_main$items_avg_patient_ratio)
# Also weak
# Slight negative correlation between items per month per patient and practise size
# but recall previous plot showing the larger raios were outliers

# No further interesting connections are obvious


#---------------------------------------------------------------------------
# Disease data
#---------------------------------------------------------------------------

d_data <- read.csv("data/registers_prevalence_gp_rdptd-tables-2019_per1000.csv")
str(d_data)

# Clean column names, eg replace spaces
colnames(d_data) <- gsub(" ", "", colnames(d_data))
colnames(d_data) <- gsub("\\.", "_", colnames(d_data))
colnames(d_data) <- gsub("\\_$", "", colnames(d_data))

# Actually, completely reset and summarise them
new_colnames <- c( "Practice",
                   "List_Size",
                   "X16",
                   "X17",
                   "X18",
                   "X50",
                   "Atrial_F",
                   "Asthma",
                   "HyperT",
                   "Cancer",
                   "Coron_HD",
                   "Pul_Disease",
                   "Cardio_Prvt",
                   "Dementia",
                   "Depression",
                   "Diabetes",
                   "Heart_F",
                   "Heart_F_LVSD",
                   "MHealth",
                   "Osteo",
                   "Palliative",
                   "Stroke",
                   "RArthritis"
)

colnames(d_data) <- new_colnames

# Normalise practice id in line with other datasets
d_data$Practice <- gsub("Z", "", d_data$Practice)
d_data$Practice <- as.integer(d_data$Practice)

colSums(is.na(d_data))

na_records <- d_data[!complete.cases(d_data),]
na_records

# One record missing data - remove it, keep complete only
d_data <- d_data[complete.cases(d_data),]

# Reset factors, dropping unused levels after cleanup
d_data <- droplevels(d_data)

# Remove extra columns (age based data)
d_data <- subset(d_data, select = -c(X16, X17, X18, X50, List_Size))
str(d_data)

# This data is majority numeric

pr_main$Practice <- as.character(pr_main$Practice)
d_data$Practice <- as.character(d_data$Practice)


# Join practice and disease data
pr_main  <- left_join(pr_main, d_data, 
                      by = c("Practice" = "Practice")
)

# re=check for NAs
colSums(is.na(pr_main))

# Reset practice id factor
pr_main$Practice <- as.factor(pr_main$Practice)

str(pr_main)

# plot <- ggplot(pr_main, aes(x = LCG, y = items_avg_patient_ratio , color = Registered_Patients))
# plot <- plot + geom_point() + scale_color_viridis()
# print((plot))

# check the pairing of each of the practices, using labels from 
# the disease data 
pair_variables  <- sapply(pr_main, is.numeric)
pair_variables["Practice"] <- FALSE

pair_variables["LCG_int"] <- TRUE
pair_variables["MonthlyAvg_P"] <- FALSE
pair_variables["Sum_GT"] <- FALSE
pair_variables["item_total_patient_ratio"] <- FALSE
pair_variables["MonthlyAvg_P"] <- FALSE
pair_variables["Registered_Patients"] <- FALSE

pair_variables

pair_dataA <- pr_main[pair_variables]
pair_variables <- colnames(pair_data)

# pair_data <- pr_main[pair_variables]
# 
# pair_check <- colnames(pair_data)
# pair_check
# 
# 
# 
# pairs(pr_main[pair_check] ,
#        lower.panel = panel.cor,
#        upper.panel = upper.panel)

split_pair <- split(pair_variables, c("A", "B", "C") )
split_pair$A
split_pair$B
split_pair$C

split_pair$A <- append(c("practice_size", "LCG_int"), split_pair$A, )
split_pair$B <- append(c("items_avg_patient_ratio", "LCG_int"), split_pair$B )
split_pair$C <- append(c("items_avg_patient_ratio", "practice_size"), split_pair$C )

pair_dataA <- pr_main[split_pair$A]
pair_checkA <- colnames(pair_dataA)
pair_checkA

pairs(pr_main[pair_checkA] ,
       lower.panel = panel.cor,
       upper.panel = upper.panel)

pair_dataB <- pr_main[split_pair$B]
pair_checkB <- colnames(pair_dataB)

pairs(pr_main[pair_checkB] ,
      lower.panel = panel.cor,
      upper.panel = upper.panel)

pair_dataC <- pr_main[split_pair$C]
pair_checkC <- colnames(pair_dataC)

#install.packages("psych")
library(psych)


pairs.panels(pair_dataA, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(pair_dataB, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(pair_dataC, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

#---------------------------------------------------------------------------

