# ca3_ni

# Several datasets required apart from the prescription data itslef
# Practice data
# Post code data (for practice geo-location)

library(dplyr)

# --------------------------------------------------
# Get practise data
# --------------------------------------------------

docs_in <- read.csv("data/gp-practice-reference-file--january-2020.csv")

head(docs_in)

# Examine structure of the file
str(docs_in)

# A clean dataset, no missing values etc though the address information is inconsistant
# Required fields - 
# PracNo 
# Postcode
# LCG (Local Commissioning Group) - group responsible for addressing health and social care needs
# Registered Patients
# Practise Name not required - remove

docs_in <- docs_in[, -2]

# library for improved data manuipulation
library(dplyr)

# Want to join this with postcode data to have town identifier

# --------------------------------------------------
# Read Postcode data
# --------------------------------------------------
postcodes_in <- read.csv("data/CleanNIPostcodeData.csv", header=TRUE, sep=",")
str(postcodes_in)

# Don't need all the post code detail, only higher level info. 
# Retain some columns - Postcode, x/y coords, country, town 
# Note, though co-orinate info may not be required later depending on 
# the location system to be used it will be retained, as a low cost operation
postcodes_clean <- postcodes_in[,c("Postcode","Town","County", "x.coordinates", "y.coordinates")]
str(postcodes_clean)

# Remove blank Towns (a small proportion of the data)
# This to minimise the number of postcode references with a blank town value
postcodes_clean$Town <- as.character(postcodes_clean$Town)
postcodes_clean$Town[postcodes_clean$Town == ""] <- NA

postcodes_clean <- postcodes_clean[complete.cases(postcodes_clean),]
str(postcodes_clean)

# Then de-duplicate the remaining data by postcode
postcodes_clean <- postcodes_clean[!duplicated(postcodes_clean$Postcode), ] 
str(postcodes_clean)

# This has reduced post code list from almost a million rows down 
# to approx 47k without reducing the level of useful data

# Deal with missing data
# NOTE: There should be a better way of doing this than manually, API sources do not
# easily yield teh post town name though with the small amount of data, its not 
# onerous and its quicker to manually search.
# The  x/y co-ordinates are not important at this time, if
# they are required later, then will retrieve that from an API reference source
# Create fake data fame of missing data to be appended to the postcode info.
temp_lookup <- data.frame(
  c("BT41NS" , "BT308RD", "BT252AT", "BT294LN", "BT414BS", 
    "BT388TP","BT399HL", "BT670LQ", "BT670DD", "BT670QA"),
  c("BELFAST", "DOWNPATRICK", "DROMARA", "CRUMLIN", "ANTRIM",
    "GREENISLAND", "BALLYCLARE", "MOIRA", "AGHALEE", "MOIRA"),
  c("DOWN", "DOWN", "DOWN", "ANTRIM", "ANTRIM",
    "ANTRIM", "ANTRIM", "DOWN", "ANTRIM", "DOWN"),
  NA, NA)

colnames(temp_lookup) <- colnames(postcodes_clean)

postcodes_clean <- bind_rows(postcodes_clean, temp_lookup)

# Noting the format difference between the 2 datasets, practices data includes a 
# space in the postcode
# Normalise this to same format as postcodes dataset, ie; remove the space.
docs_in$Postcode <-   gsub('\\s+', '',docs_in$Postcode)

# docs_in$Postcode <- as.factor(docs_in$Postcode)
docs_in$Postcode <- as.character(docs_in$Postcode)

docs_in <- left_join(docs_in, postcodes_clean, 
                     by = c("Postcode" = "Postcode")
)
docs_in[,"Town"] = toupper(docs_in[,"Town"])

str(docs_in)

# The postcode database has found all practice address?
# Id the missing values

docs_in$Town <- as.character(docs_in$Town)
docs_in$Town[docs_in$Town == ""] <- NA

# junk <- docs_in[!complete.cases(docs_in),]
colSums(is.na(docs_in))

# A number of x/y coordinates are NA
# Can't get rid of those rows, but retain the data (a small amount) for potential
# use later if using geo-plot

# --------------------------------------------------
# Save clean practice dataset to file

write.csv(file="data/cleaned_practice_data.csv", x=docs_in, quote=TRUE, row.names = FALSE)

