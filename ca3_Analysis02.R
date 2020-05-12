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

# p_items is prescirption items data
p_items <- p_data

str(p_items)


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


# (Sections in Reference)

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


# Create some patient/items ratios

pr_main$item_total_patient_ratio <- pr_main$Sum_GT / pr_main$Registered_Patients
pr_main$items_avg_patient_ratio <- pr_main$MonthlyAvg_P / pr_main$Registered_Patients

boxplot(x = pr_main$item_total_patient_ratio,
        main = "item_total_patient_ratio",
        sub = paste("outlier rows: ", 
                    boxplot.stats(x = pr_main$item_total_patient_ratio)$out)
)

# Outliers on both end of the scale

boxplot(x = pr_main$items_avg_patient_ratio,
        main = "items_avg_patient_ratio",
        sub = paste("outlier rows: ", 
                    boxplot.stats(x = pr_main$items_avg_patient_ratio)$out)
)


# Plot distributions for the ratios


# Outliers for average items dispensed, need to identify which they are
chk  <- unique(subset(pr_main, pr_main$avg_items_patient_ratio > 2.6, 
))
chk


