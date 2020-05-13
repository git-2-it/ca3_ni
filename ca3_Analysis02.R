# ca3_ni

# Several datasets required apart from the prescription data itslef
# Practice data
# Post code data (for practice geo-location)

library(dplyr)

# --------------------------------------------------
# Get practise data
# --------------------------------------------------

docs_in <- read.csv("data/cleaned_practice_data.csv")
#docs_in <- subset(docs_in, PracNo != 157)

head(docs_in)

# Examine structure of the file
str(docs_in)


# --------------------------------------------------
# Read prescription data
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


# previously 327 in docs list vs 336 in items - careful about new/closed practices

# keep_cols <- c("Practice",
#                "BNF_Chapter",
#                "BNF_Section"
#                # , "BNF_Paragraph", "BNF_Sub_Paragraph"
# )


# Join practice and items data

# First get rid of superflous columns
str(docs_in)

# pr_main is main practice data
pr_main <- subset(p_data, select = c("Practice", "MonthlyAvg_P", "Sum_GT") )
pr_main <-unique(pr_main)

pr_main2 <- subset(docs_in, 
                  select = c(PracNo, PracticeName, Address1, Postcode, LCG, Registered_Patients, Town, County, year))

str(pr_main)
colnames(pr_main2)[colnames(pr_main2) == "PracNo"] <-"Practice"
colnames(pr_main2)

#pr_main$Practice <- as.numeric(pr_main$Practice)
#pr_main2$Practice <- as.numeric(pr_main2$Practice)
pr_main2$Practice <- as.factor(pr_main2$Practice)
pr_main$Practice <- as.factor(pr_main$Practice)

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
# Most likely they have closed in the interviening period
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


#---------------------------------------------------------------------------
# Prescription data
#---------------------------------------------------------------------------

# For reference - BNF Chapter code list

# Chapt	Description	                Short
# 1	 Gastro-Intestinal System       Gasto
# 2	 Cardiovascular System	        Cardio
# 3	 Respiratory System	        Resp
# 4	 Central Nervous System	        CNS
# 5	 Infections	                Infect
# 6 	 Endocrine System	        Endocrine
# 7	 Obstetrics,Gynae &             Obstrt Gyn UTr
#        Urinary Tract Disorders	          
# 8	 Malignant Disease              Malg ImmSup
#        & Immunosuppression	      
# 9	 Nutrition And Blood	        Blood Nutrit
# 10	 Musculoskeletal                Musco Skel
#        & Joint Diseases	
# 11	 Eye	                        Eye
# 12	 Ear, Nose And Oropharynx	ENT
# 13	 Skin	                        Skin
# 14	 Immunological Products         Immuno + Vaccine
#        & Vaccines	            
# 15	 Anaesthesia	                Anaest
# 18	 Preparations used              Prep Diag 
#        in Diagnosis	
# 19	 Other Drugs And                Other
#        Preparations	
# 20	 Dressings	                Dressings
# 21     Appliances		        Appliances
# 22	 Incontinence Appliances	Incont
# 23	 Stoma Appliances	        Stoma 
# Sections etc in Reference)

# cost/price data not relevant in this analysis - can remove (and ignore odd column names)
#
# Other fields to remove
# VTM_NM - Substance/Product name
# VMP_NM - Generic name
# AMP_NM - Branded / Generic name

# p_data <- read.csv("data/summary_info.csv")

col_names <- colnames(p_data)

# variables clean up/reduction
variables_list <- !logical(length(col_names))
variables_list <- setNames(variables_list, col_names)

# build remove list
removal_list <- sapply(col_names, function(x) grepl("Cost", x))
removal_list[c("VTM_NM", "VMP_NM", "AMP_NM")] <- TRUE
removal_list[c("Presentation", "Strength")] <- TRUE
removal_list[c("Total_Quantity")] <- TRUE

removal_list

variables_list[removal_list] <- FALSE
variables_list

p_data <- p_data[, variables_list]
str(p_data)

summary(p_data)

str(p_data)

# Align practice data with practices above 
# remove_practices 157, 267, 329, 517, 541, 581, 583

p_data  <- subset(p_data, Practice != 157)
p_data  <- subset(p_data, Practice != 267)
p_data  <- subset(p_data, Practice != 329)
p_data  <- subset(p_data, Practice != 517)
p_data  <- subset(p_data, Practice != 541)
p_data  <- subset(p_data, Practice != 581)
p_data  <- subset(p_data, Practice != 583)


# # reorder based on avg item patient ratios
# plot_data <- p_data
# plot_data$Practice <- factor(plot_data$Practice, 
#                              levels = plot_data$Practice[order(plot_data$items_avg_patient_ratio)])
# 
# 
# #plot_data <- pr_main[order(pr_main$avg_items_patient_ratio), ]
# plot_data <- p_main
# plot_data$PracNo <- factor(plot_data$PracNo, 
#                            levels = plot_data$PracNo[order(plot_data$item_patient_ratio)])
# 
# str(p_data)
# plot <- ggplot(p_data, aes(x = Practice, y = MonthlyAvg_C) ) + 
#         geom_bar(stat = "identity" , color = p_data$BNF_Chapter) + scale_color_viridis()
# 
# print(plot)
# 
# plot <- ggplot(p_data, aes(x = Practice, y = Sum_GT) ) + 
#         geom_bar(stat = "identity" ) + scale_color_viridis()
# 
# print(plot)

display_variables <- c("Practice", "BNF_Chapter", "BNF_Section")

p_data[which.max(p_data$Sum_GT), display_variables]

prop.table(table(p_data$BNF_Chapter))

# --------------------------------------------------------------------------------------
# Plotting values for initial comparisons
# --------------------------------------------------------------------------------------



p_data_HOLD <- p_data
                                               
barplot(height = prop.table(table(p_data$Chapter_lab)),
        main = "Item types by Chapter", 
        ylab = "Frequency", 
        xlab = "Region",
        col = "white")


p_data <- subset(p_data, select = c(Practice, Sum_GT, MonthlyAvg_P,
                                   BNF_Chapter, BNF_Section,
                                   MonthlyAvg_C, MonthlyAvg_CS
                                   ))
p_data <- unique(p_data)

# Reconfigure values for chapter labels
p_data <- p_data %>% mutate(Chapter_lab=dplyr::recode(BNF_Chapter, 
                                                      "1" = "Gasto",
                                                      "2" = "Cardio", 
                                                      "3" = "Resp", 
                                                      "4" = "CNS", 
                                                      "5" = "Infect",
                                                      "6" = "Endo",
                                                      "7" = "OGU",
                                                      "8" = "MalD ISp",
                                                      "9" = "Bld Nutrt",
                                                      "10" = "MuscSkel",
                                                      "11" = "Eye",
                                                      "12" = "ENT",
                                                      "13" = "Skin",
                                                      "14" = "Imm Vac",
                                                      "15" = "Anaest",
                                                      "18" = "Prp Dg",
                                                      "19" = "Other",
                                                      "20" = "Dress",
                                                      "21" = "Appls",
                                                      "22" = "Incont",
                                                      "23" = "Stoma "
))

barplot(height = prop.table(table(p_data$Chapter_lab)),
        main = "Item types by Chapter", 
        ylab = "Frequency", 
        xlab = "BNF Chapter",
        col = "white"
        ,las=2)


junk <- subset(p_data, select = c(BNF_Chapter, BNF_Section
))
junk

junk <- unique(junk)




#---------------------------------------------------------------------------

