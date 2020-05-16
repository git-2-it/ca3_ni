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
# datapath <- "data/test"

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
# Will be removed after other checks

# # NA data review, import useful libraries
# library(mice)
# library(VIM)
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
        main = "Basic item frequencies by BNF Chapter", 
        sub = "Includes non-relevant Chapter data",
        xlab = "Chapter", 
        ylab = "Frequency",
        col = viridis(5),
        cex.names = 0.75,
        beside=TRUE,
        args.legend=list(bty="n",horiz=TRUE),
)

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


###########################################
# Sum and Avg for Practices and Chapters
# Sum and Avg for Practices
###########################################

# Practices and chapters
# Get sum for each, by year/month
practice_items_sum_GT_YMPC  <- aggregate(twoyears_in$Total_Items,
                                       by=list(
                                         twoyears_in$Year,
                                         twoyears_in$Month,
                                         twoyears_in$Practice,
                                         twoyears_in$BNF_Chapter
                                       ),
                                       FUN=sum)
colnames(practice_items_sum_GT_YMPC) <- c("Year", "Month", "Practice","BNF_Chapter", "Total_Items") 

# count entries by practice, chapter - and year and month
tally_PC <- practice_items_sum_GT_YMPC %>% group_by(Practice, BNF_Chapter) %>% tally(name = "PC_count")

practice_items_sum_GT_PC  <- aggregate(practice_items_sum_GT_YMPC$Total_Items,
                                       by=list(
                                         practice_items_sum_GT_YMPC$Practice,
                                         practice_items_sum_GT_YMPC$BNF_Chapter
                                       ),
                                       FUN=sum)
colnames(practice_items_sum_GT_PC) <- c("Practice", "BNF_Chapter", "Total_Items_PC") 

# Divide items summed by practice and chapter 
# by number of entries of year and month to get monthly average  
practice_chapter_month <- left_join(practice_items_sum_GT_PC, tally_PC, 
                                    by = c("Practice" = "Practice", "BNF_Chapter" = "BNF_Chapter")
)

# create the average
practice_chapter_month$PC_Month_Avg <- practice_chapter_month$Total_Items_PC / practice_chapter_month$PC_count

str(practice_chapter_month)

practice_chapter_month <- select(practice_chapter_month, -c(PC_count))


##### now practices only
practice_items_sum_GT_YMP  <- aggregate(practice_items_sum_GT_YMPC$Total_Items,
                                         by=list(
                                           practice_items_sum_GT_YMPC$Year,
                                           practice_items_sum_GT_YMPC$Month,
                                           practice_items_sum_GT_YMPC$Practice
                                         ),
                                         FUN=sum)
colnames(practice_items_sum_GT_YMP) <- c("Year", "Month", "Practice", "Total_Items") 

practice_items_sum_GT_P  <- aggregate(practice_items_sum_GT_YMPC$Total_Items,
                                      by=list(
                                        practice_items_sum_GT_YMPC$Practice
                                      ),
                                      FUN=sum)
colnames(practice_items_sum_GT_P) <- c("Practice", "Total_Items_P") 

tally_P <- practice_items_sum_GT_YMP %>% group_by(Practice) %>% tally(name = "P_count")

practice_month <- left_join(practice_items_sum_GT_P, tally_P, 
                                    by = c("Practice" = "Practice")
)

practice_month$P_Month_Avg <- practice_month$Total_Items_P / practice_month$P_count

practice_month <- select(practice_month, -c(P_count))

##### end practices only

# Join practice info at the different levels
practice_table <- left_join(practice_month, practice_chapter_month,
                         by = c("Practice" = "Practice"
                         ))


str(twoyears_in)

write.csv(file="data/practice_summary_info.csv", x=practice_table, quote=TRUE, row.names = FALSE)
write.csv(file="data/prescription_sub.csv", x=twoyears_in, quote=TRUE, row.names = FALSE)

head(practice_table, n=10)

practice_table$BNF_Chapter <- as.factor(practice_table$BNF_Chapter)

twoyears_in$BNF_Chapter <- as.factor(twoyears_in$BNF_Chapter)
plot_table0 <- table(twoyears_in$BNF_Chapter)

library("ggplot2") 

par(mfrow = c(1, 2))

ggplot(practice_table, aes(x = BNF_Chapter, y = Total_Items_PC, fill = BNF_Chapter) ) + 
  geom_bar(stat = "identity" ) + scale_fill_viridis_d() +
  ggtitle("Items dispensed") + 
  labs(title="Total items dispensed per Chapter",
       x ="BNF Chapter", y = "# Items")   + theme(legend.position="none")



barplot(plot_table0, 
        main = "Basic item frequencies by BNF Chapter", 
        sub = "Includes non-relevant Chapter data",
        xlab = "Chapter", 
        ylab = "Frequency",
        col = viridis(5),
        cex.names = 0.75,
        beside=TRUE,
        args.legend=list(bty="n",horiz=TRUE),
)



