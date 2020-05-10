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

attach(twoyears_in)
practice_items_sum  <- aggregate(twoyears_in$Total_Items, 
                                 by=list(Practice,BNF_Chapter,
                                         BNF_Section), 
                                         # BNF_Paragraph, BNF_Sub_Paragraph),
                                 FUN=sum)
practice_items_month_sum  <- aggregate(twoyears_in$Total_Items, 
                                       by=list(Practice,
                                               Year, 
                                               Month
                                       ), 
                                       FUN=sum)
practice_items_mean  <- aggregate(twoyears_in$Total_Items, 
                                  by=list(Practice,
                                          BNF_Chapter,
                                          BNF_Section),
                                  FUN=mean)
detach(twoyears_in)

# Reuse colum names for the summarised dataset, making sure to label the calculated values
colnames(practice_items_sum) <- append(keep_cols, "ItemsTotal")
colnames(practice_items_mean) <- append(keep_cols, "MonthlyAvg_ChSn")
colnames(practice_items_month_sum) <- c("Practice","Year", "Month", "Total_Items")

practice_items_mean2 <- aggregate(practice_items_month_sum$Total_Items , 
                                  by = list(practice_items_month_sum$Practice ), 
                                  mean)
colnames(practice_items_mean2) <- c("Practice","MonthlyAvg")


practice_items <- left_join(practice_items_sum, practice_items_mean )
practice_items <- left_join(practice_items, practice_items_mean2)

str(practice_items)



str(twoyears_in)

write.csv(file="data/prescription_summary.csv", x=practice_items, quote=TRUE, row.names = FALSE)
write.csv(file="data/prescription_sub.csv", x=twoyears_in, quote=TRUE, row.names = FALSE)



