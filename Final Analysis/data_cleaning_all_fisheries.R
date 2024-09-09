# in this script, I read in cleaned fishery-specific data files, plot historgams, and identify potential outliers

# CALIFORNIA SPINY LOBSTER
lobster <- read_excel("~/Desktop/CaliforniaSpinyLobster_Cleaned.xlsx")
# create data frame that only includes the entries where Include = "yes"
lobster.yes <- subset(lobster, lobster$Include=="yes")

# make a histogram plot of algorithm estimates to look for outliers
hist(lobster.yes$Algorithm_Estimate_mm, breaks = 50)
# there seems to be one outlier - let's locate it
lob.alg.outlier <- subset(lobster.yes, lobster.yes$Algorithm_Estimate_mm > 600)
lob.alg.outlier # SpecimenID 697 - this likely didn't correctly capture the reference object
# will change SpecimenID 697 from yes to no

# make a histogram plot of adjusted estimates to look for outliers
hist(lobster.yes$Adjusted_Estimate_mm, breaks = 50) # no outliers observed



# NORTHERN CALIFORNIA RED ABALONE
AbaloneClean <- read_excel("~/Desktop/AbaloneCleaned_January2023.xlsx")
unique(AbaloneClean$Abalone_ID)
length(unique(AbaloneClean$Abalone_ID)) # there are 42 individual abalone specimens

# make Abalone_ID values be continuous 1-42
AbaloneClean$Abalone_ID <- as.integer(as.factor(AbaloneClean$Abalone_ID))
unique(AbaloneClean$Abalone_ID)

# make user_id variable assigning a value 1-13 to the different participants
AbaloneClean$user_id <- as.integer(as.factor(AbaloneClean$User_Name))
sort(unique(AbaloneClean$user_id))

# write out csv of new file
write.csv(AbaloneClean, file = "/Users/kirstenelstner/Desktop/abaloneclean.csv")


# read in cleaned abalone file 
abalone <- read_excel("~/Desktop/AbaloneCleanedJanuary2023.xls")

# make a histogram plot of algorithm estimates to look for outliers
hist(abalone$Algorithm_Estimate_mm, breaks = 50)

# make a histogram plot of algorithm estimates to look for outliers
hist(abalone$Adjusted_Estimate_mm, breaks = 50)
# there seems to be 3 outliers
abalone.outliers <- subset(abalone, abalone$Adjusted_Estimate_mm > 400)
abalone.outliers

# plot histogram with outliers removed
abalone.outliers.rm <- subset(abalone, abalone$Adjusted_Estimate_mm < 400)
# algorithm estimates
hist(abalone.outliers.rm$Algorithm_Estimate_mm, breaks = 50)
# adjusted estimates
hist(abalone.outliers.rm$Adjusted_Estimate_mm, breaks = 50)


# PENSHELL SCALLOP

# first merge algorithm estimates and adjusted estimates (merge by Specimen_ID)

# read in data
algorithm_estimates <- read_excel("~/Desktop/algorithm estimates.xlsx")
adjusted_estimates <- read_excel("~/Desktop/adjusted estimates.xlsx")

penshell_merged <- merge(algorithm_estimates, adjusted_estimates, by = "Specimen_ID")
penshell_merged

# write out csv
write.csv(penshell_merged, file = "/Users/kirstenelstner/Desktop/penshell_merged.csv")


# re-import penshell merged dataset
Penshell_MERGED <- read.csv("~/Desktop/penshell_merged.csv")
penshell.yes <- subset(Penshell_MERGED, Included == "yes")
length(penshell.yes$Record)


# make histogram plot of algorithm estimates to look for outliers
hist(penshell.yes$Algorithm_Estimate_mm[penshell.yes$Algorithm_Estimate_mm < 200], breaks = 50)

# make histogram plots for adjusted estimates to look for outliers
hist(penshell.yes$Adjusted_Estimate_mm, breaks = 50)

