# ca3_ni

# Several datasets required apart from the prescription data itslef
# Practice data
# Post code data (for practice geo-location)

# --------------------------------------------------
# Get practise data
# --------------------------------------------------

# Init required libraries
library("readr")
library("tidyr")
library("dplyr")

datapath <- "data/2years"


coltypes <- list(
  col_integer(), col_integer(), col_integer(), 
  col_character(), col_character(), col_character(), col_character(), col_character(),
  col_integer(), col_integer(), 
  col_number(), col_number(),
  col_character(), col_character(), col_character(), col_character(), col_character()
)

# Read in the data
twoyears_in <- list.files(path=datapath, full.names=TRUE, recursive = TRUE) %>% 
  lapply(read_csv, col_types = coltypes) %>%   bind_rows

# Check for notable problems or warnings 
problems(twoyears_in)
warnings()
warnings()
# Check Structure
str(twoyears_in)

# Sanity check on the inputs by counting number of years/month
# Expecting  12 090 116 (approx 450k per month @ 26 Jan2018 to date)
n_distinct(twoyears_in$Year)
n_distinct(twoyears_in$Month)
months <- unique(twoyears_in$Month)
months
years <- unique(twoyears_in$Year)
years

colSums(is.na(twoyears_in))
# Numbers of NA are low (in the columns of interest) comparative to dataaset size

# # NA data review, import useful libraries
# library(mice)
# library(VIM)
#
# junk <- twoyears_in[is.na(twoyears_in$Year),]
# 
# md.pattern(twoyears_in, rotate.names = FALSE)
# 
# missing_values <- aggr(twoyears_in, prop = c(FALSE), numbers = TRUE,
#                        labels=names(data),
#                        combined = TRUE
# )


str(twoyears_in)

# extra column warning because of extra comma at end of rows of some input files
# Get rid of the extra column
col_names <- colnames(twoyears_in)
col_names

# dataset review
str(twoyears_in)

# Not all are suitable types, eg Practice is an int, should be factor, 
# year and month are int, could be a combined date
# grab col names and replace blanks with underscore
col_names <- colnames(twoyears_in)
col_names <- gsub(" ", "_", col_names)
col_names <- gsub("-", "_", col_names)
col_names <- trimws(col_names)
colnames(twoyears_in) <- col_names
str(twoyears_in)

# cost/price data not relevant in this analysis - can remove (and ignore odd column names)
#
# Other fields to remove
# VTM_NM - Substance/Product name
# VMP_NM - Generic name
# AMP_NM - Branded / Generic name

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

twoyears_in <- twoyears_in[, variables_list]
str(twoyears_in)

summary(twoyears_in)

# Plot some visuals to help understand the shape of the data set
library("viridis")

orig_par <- par()

# plot practice  frequency stacked by  chapter
# Indication of volumes and types of items, not frequency per se
plot_table0 <- table(twoyears_in$BNF_Chapter )

barplot(plot_table0, 
        main = "Basic counts", 
        xlab = "Chapter", 
        ylab = "Frequency",
        col = viridis(5),
        cex.names = 0.75,
        beside=TRUE,
        args.legend=list(bty="n",horiz=TRUE),
)
# print(plot_table0)

# plot_table0 <- table(twoyears_in$BNF_Sub_Paragraph )
# 
# barplot(plot_table0, 
#         main = "Basic counts", 
#         xlab = "Chapter", 
#         ylab = "Frequency",
#         col = viridis(5),
#         cex.names = 0.75,
#         beside=TRUE,
#         args.legend=list(bty="n",horiz=TRUE),
# )


# twoyears_in <- subset(twoyears_in, select = -c(BNF.Code))

twoyears_in$BNF_Code[twoyears_in$BNF_Chapter == "-"] <- NA
twoyears_in$BNF_Code[twoyears_in$BNF_Chapter == "99"] <- NA

str(twoyears_in)
twoyears_in <- twoyears_in[complete.cases(twoyears_in), ]
str(twoyears_in)

twoyears_in$BNF_Chapter  <- as.integer(twoyears_in$BNF_Chapter)
twoyears_in$BNF_Paragraph  <- as.integer(twoyears_in$BNF_Paragraph)
twoyears_in$BNF_Section  <- as.integer(twoyears_in$BNF_Section)
twoyears_in$BNF_Sub_Paragraph  <- as.integer(twoyears_in$BNF_Sub_Paragraph)

colSums(is.na(twoyears_in))

twoyears_in$BNF_Chapter <- replace_na(twoyears_in$BNF_Chapter, 0)
twoyears_in$BNF_Paragraph <- replace_na(twoyears_in$BNF_Paragraph, 0)
twoyears_in$BNF_Section <- replace_na(twoyears_in$BNF_Section, 0)
twoyears_in$BNF_Sub_Paragraph <- replace_na(twoyears_in$BNF_Sub_Paragraph, 0)

colSums(is.na(twoyears_in))
# twoyears_in <- subset(twoyears_in, select = -c(Presentation))
#twoyears_in <- subset(twoyears_in, select = -c(Total_Quantity) )

keep_cols <- c("Practice","BNF_Chapter",
               "BNF_Section" #, "BNF_Paragraph", "BNF_Sub_Paragraph"
)

# create joined chapter/section code
# twoyears_in$BNF_X <- paste(formatC(twoyears_in$BNF_Chapter, width=2, flag="0") , 
#                            formatC(twoyears_in$BNF_Section, width=2, flag="0"),
#                            formatC(twoyears_in$BNF_Paragraph, width=2, flag="0"),
#                            formatC(twoyears_in$BNF_Sub_Paragraph, width=2, flag="0"),
#                            sep="")

# 
# attach(twoyears_in)

# --------------------------- 
# --------------------------- 
# Create sums and means, per category, WAC
# --------------------------- 

keep_cols1 <- c("Month", "Practice","BNF_Chapter",
                "BNF_Section")
keep_cols2 <- c("Practice","BNF_Chapter",
                "BNF_Section"
)

##

practice_items_sum_CSPS  <- aggregate(twoyears_in$Total_Items, 
                                      by=list(twoyears_in$Month,
                                              twoyears_in$Practice,
                                              twoyears_in$BNF_Chapter,
                                              twoyears_in$BNF_Section,
                                              twoyears_in$BNF_Paragraph,
                                              twoyears_in$BNF_Sub_Paragraph), 
                                      FUN=sum)
colnames(practice_items_sum_CSPS) <- append(keep_cols1, 
                                            c("BNF_Paragraph", "BNF_Sub_Paragraph","MonthlySum_CSPS"))

practice_items_mean_CSPS  <- aggregate(practice_items_sum_CSPS$MonthlySum_CSPS,
                                       by=list(
                                         practice_items_sum_CSPS$Practice,
                                         practice_items_sum_CSPS$BNF_Chapter,
                                         practice_items_sum_CSPS$BNF_Section,
                                         practice_items_sum_CSPS$BNF_Paragraph,
                                         practice_items_sum_CSPS$BNF_Sub_Paragraph),
                                       FUN=mean)
colnames(practice_items_mean_CSPS) <- append(keep_cols2, 
                                             c("BNF_Paragraph", "BNF_Sub_Paragraph","MonthlyAvg_CSPS"))

##

practice_items_sum_CSP  <- aggregate(practice_items_sum_CSPS$MonthlySum_CSPS,
                                     by=list(practice_items_sum_CSPS$Month,
                                             practice_items_sum_CSPS$Practice,
                                             practice_items_sum_CSPS$BNF_Chapter,
                                             practice_items_sum_CSPS$BNF_Section,
                                             practice_items_sum_CSPS$BNF_Paragraph
                                     ),
                                     FUN=sum)
colnames(practice_items_sum_CSP) <- append(keep_cols1, 
                                           c("BNF_Paragraph","MonthlySum_CSP"))

practice_items_mean_CSP  <- aggregate(practice_items_sum_CSP$MonthlySum_CSP,
                                      by=list(
                                        practice_items_sum_CSP$Practice,
                                        practice_items_sum_CSP$BNF_Chapter,
                                        practice_items_sum_CSP$BNF_Section,
                                        practice_items_sum_CSP$BNF_Paragraph
                                      ),
                                      FUN=mean)
colnames(practice_items_mean_CSP) <- append(keep_cols2, 
                                            c("BNF_Paragraph", "MonthlyAvg_CSP"))

##
practice_items_sum_CS  <- aggregate(practice_items_sum_CSP$MonthlySum_CSP,
                                    by=list(practice_items_sum_CSP$Month,
                                            practice_items_sum_CSP$Practice,
                                            practice_items_sum_CSP$BNF_Chapter,
                                            practice_items_sum_CSP$BNF_Section
                                    ),
                                    FUN=sum)
colnames(practice_items_sum_CS) <- append(keep_cols1, c("MonthlySum_CS"))

practice_items_mean_CS  <- aggregate(practice_items_sum_CS$MonthlySum_CS,
                                     by=list(
                                       practice_items_sum_CS$Practice,
                                       practice_items_sum_CS$BNF_Chapter,
                                       practice_items_sum_CS$BNF_Section
                                     ),
                                     FUN=mean)
colnames(practice_items_mean_CS) <- append(keep_cols2, c("MonthlyAvg_CS"))
##
practice_items_sum_C  <- aggregate(practice_items_sum_CS$MonthlySum_CS,
                                   by=list(practice_items_sum_CS$Month,
                                           practice_items_sum_CS$Practice,
                                           practice_items_sum_CS$BNF_Chapter
                                   ),
                                   FUN=sum)
colnames(practice_items_sum_C) <- c("Month", "Practice","BNF_Chapter", "MonthlySum_C")

practice_items_mean_C  <- aggregate(practice_items_sum_C$MonthlySum_C,
                                    by=list(practice_items_sum_C$Practice,
                                            practice_items_sum_C$BNF_Chapter
                                    ),
                                    FUN=mean)
colnames(practice_items_mean_C) <- c("Practice","BNF_Chapter", "MonthlyAvg_C")

## 

practice_items_sum_P  <- aggregate(practice_items_sum_C$MonthlySum_C, 
                                   by=list(practice_items_sum_C$Month,
                                           practice_items_sum_C$Practice), 
                                   FUN=sum)
colnames(practice_items_sum_P) <- c("Month", "Practice", "MonthlySum_P")

practice_items_mean_P  <- aggregate(practice_items_sum_P$MonthlySum_P,
                                    by=list(practice_items_sum_P$Practice
                                    ),
                                    FUN=mean)
colnames(practice_items_mean_P) <- c("Practice", "MonthlyAvg_P")

practice_items_sum_GT  <- aggregate(practice_items_sum_P$MonthlySum_P, 
                                    by=list(
                                      practice_items_sum_P$Practice), 
                                    FUN=sum)
colnames(practice_items_sum_GT) <- c("Practice", "Sum_GT")

###########################################################

# write.csv(file="data/2year.csv", x=twoyears_in, quote=TRUE, row.names = FALSE)

means_table <- practice_items_mean_CSPS
str(means_table)

means_table <- left_join(means_table, practice_items_mean_CSP,
                         by = c("Practice" = "Practice", 
                                "BNF_Chapter" = "BNF_Chapter",
                                "BNF_Section" = "BNF_Section",
                                "BNF_Paragraph"= "BNF_Paragraph"
                         ))

means_table <- left_join(means_table, practice_items_mean_CS,
                         by = c("Practice" = "Practice", 
                                "BNF_Chapter" = "BNF_Chapter",
                                "BNF_Section" = "BNF_Section"
                         ))

means_table <- left_join(means_table, practice_items_mean_C,
                         by = c("Practice" = "Practice", 
                                "BNF_Chapter" = "BNF_Chapter"
                         ))

means_table <- left_join(means_table, practice_items_mean_P,
                         by = c("Practice" = "Practice"
                         ))

means_table <- left_join(means_table, practice_items_sum_GT,
                         by = c("Practice" = "Practice"
                         ))

# --------------------------- 

str(twoyears_in)

write.csv(file="data/summary_info.csv", x=means_table, quote=TRUE, row.names = FALSE)
#write.csv(file="data/prescription_summary.csv", x=practice_items, quote=TRUE, row.names = FALSE)
write.csv(file="data/prescription_sub.csv", x=twoyears_in, quote=TRUE, row.names = FALSE)


