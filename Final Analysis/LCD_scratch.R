### Least Common Denominator Analysis ###

# Goals:
# 1) Make box and whisker plot of Algorithm/Adjusted Total Error Across All Fisheries 
# 2) Make box and whisker plot of Algorithm/Adjusted Percent Error Across All Fisheries
# 3) Calculate accuracy and precision metrics for each fishery to characterize software performance
# 4) Regress Algorithm Total Error & Algorithm Percent Error against True Size
# 5) Examine and rates of algorithm failure using binomial logistic regressions

##################################################################################

# TASK 1 - Total Error Box and Whisker Plots

# load packages
library(ggplot2)
library(ggpubr)

# import Error Spreadsheet
ErrorData <- read_excel("Final Data/CombinedFisheries_Error.xlsx")
ErrorData$Estimate_Type <- factor(ErrorData$Estimate_Type, levels = c("Algorithm", "Adjusted"))
cols <-c("gray", "red")

# Total Error Boxplot (No Outliers)
TotalError.no_out = ggplot(dat=ErrorData,
                         aes(x=Fishery, 
                             y=Total_Error_mm, 
                             fill=Estimate_Type)) + 
  geom_boxplot(outlier.shape=NA) +
  coord_cartesian(ylim=c(-100, 120)) +
  xlab("Fishery") +
  ylab("Deviation from True Size (mm)")+
  ggtitle("Total Error Across Fisheries") +
  scale_fill_manual(values = cols) +
  theme_bw()
)
TotalError.no_out + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  guides(fill=guide_legend(title="Algorithm Estimate Type")) +
  theme(plot.title = element_text(hjust = 0.5))


# Total Error Boxplot (With Outliers)
TotalError.out = ggplot(dat=ErrorData,
                           aes(x=Fishery, 
                               y=Total_Error_mm, 
                               fill=Estimate_Type)) + 
  geom_boxplot() +
  coord_cartesian(ylim=c(-300, 700)) +
  xlab("Fishery") +
  ylab("Deviation from True Size (mm)")+
  ggtitle("Total Error Across Fisheries") +
  scale_fill_manual(values = cols) +
  theme_bw()
)
TotalError.out + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  guides(fill=guide_legend(title="Algorithm Estimate Type")) +
  theme(plot.title = element_text(hjust = 0.5))


##################################################################################

# TASK 2 - Percent Error Box and Whisker Plots

# Percent Error Boxplot (No Outliers)
PercentError.no_out = ggplot(dat=ErrorData,
                           aes(x=Fishery, 
                               y=Percent_Error, 
                               fill=Estimate_Type)) + 
  geom_boxplot(outlier.shape=NA) +
  coord_cartesian(ylim=c(-100, 120)) +
  xlab("Fishery") +
  ylab("Percent Deviation from True Size (%)")+
  ggtitle("Percent Error Across Fisheries") +
  scale_fill_manual(values = cols) +
  theme_bw()
)

PercentError.no_out + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  guides(fill=guide_legend(title="Algorithm Estimate Type")) +
  theme(plot.title = element_text(hjust = 0.5))


# Percent Error Boxplot (With Outliers)
PercentError.out = ggplot(dat=ErrorData,
                        aes(x=Fishery, 
                            y=Percent_Error, 
                            fill=Estimate_Type)) + 
  geom_boxplot() +
  coord_cartesian(ylim=c(-200, 800)) +
  xlab("Fishery") +
  ylab("Percent Deviation from True Size (mm)")+
  ggtitle("Percent Error Across Fisheries") +
  scale_fill_manual(values = cols) +
  theme_bw()
)
PercentError.out + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  guides(fill=guide_legend(title="Algorithm Estimate Type")) +
  theme(plot.title = element_text(hjust = 0.5))



##################################################################################

# TASK 3 - Accuracy and Precision Metrics

# read in each fishery dataset 
abalone <- read_excel("Final Data/Fishery-Specific Data/AbaloneCleanedJanuary2023.xls")
finfish <- read_excel("Final Data/Fishery-Specific Data/CaliforniaFinfish_March2_Merged&Cleaned.xlsx")
lobster <- read_excel("Final Data/Fishery-Specific Data/CaliforniaSpinyLobster_Cleaned_Jan2023.xlsx")
penshell <- read_excel("Final Data/Fishery-Specific Data/Penshell_Cleaned_January2023.xls")

# subset to only include yes's
abalone.yes <- subset(abalone, Include == "yes")
finfish.yes <- subset(finfish, Include == "yes")
lobster.yes <- subset(lobster, Include == "yes")
penshell.yes <- subset(penshell, Included == "yes")

# abalone accuracy/precision metrics
mean(abalone.yes$Algorithm_Total_Error_mm) # mean algorithm TE = 23.306
sd(abalone.yes$Algorithm_Total_Error_mm) # sd algorithm TE = 38.254

mean(abalone.yes$Adjusted_Total_Error_mm) # mean adjusted TE = -1.655
sd(abalone.yes$Adjusted_Total_Error_mm) # sd adjusted TE = 13.061

mean(abalone.yes$Algorithm_Percent_Error) # mean algorithm PE = 10.705
sd(abalone.yes$Algorithm_Percent_Error) # sd algorithm PE = 17.518

mean(abalone.yes$Adjusted_Percent_Error) # mean adjusted PE = -0.660
sd(abalone.yes$Adjusted_Percent_Error) # sd adjusted PE = 5.906


# finfish accuracy/precision metrics
mean(finfish.yes$Algorithm_Total_Error_mm) # mean algorithm TE = 29.622
sd(finfish.yes$Algorithm_Total_Error_mm) # sd algorithm TE = 49.254

mean(finfish.yes$Adjusted_Total_Error_mm) # mean adjusted TE = 41.013
sd(finfish.yes$Adjusted_Total_Error_mm) # sd adjusted TE = 22.271

mean(finfish.yes$Algorithm_Percent_Error) # mean algorithm PE = 10.999
sd(finfish.yes$Algorithm_Percent_Error) # sd algorithm PE = 16.552

mean(finfish.yes$Adjusted_Percent_Error) # mean adjusted PE = 14.209
sd(finfish.yes$Adjusted_Percent_Error) # sd adjusted PE = 7.697


# lobster accuracy/precision metrics
mean(lobster.yes$Algorithm_Total_Error_mm) # mean algorithm TE = 17.579
sd(lobster.yes$Algorithm_Total_Error_mm) # sd algorithm TE = 28.227

mean(lobster.yes$Adjusted_Total_Error_mm) # mean adjusted TE = 1.712
sd(lobster.yes$Adjusted_Total_Error_mm) # sd adjusted TE = 8.427

mean(lobster.yes$Algorithm_Percent_Error) # mean algorithm PE = 18.015
sd(lobster.yes$Algorithm_Percent_Error) # sd algorithm PE = 26.873

mean(lobster.yes$Adjusted_Percent_Error) # mean adjusted PE = 1.604
sd(lobster.yes$Adjusted_Percent_Error) # sd adjusted PE = 8.354



# penshell accuracy/precision metrics
mean(penshell.yes$Algorithm_Total_Error_mm) # mean algorithm TE = 12.159
sd(penshell.yes$Algorithm_Total_Error_mm) # sd algorithm TE = 83.893

mean(penshell.yes$Adjusted_Total_Error_mm) # mean adjusted TE = 1.926
sd(penshell.yes$Adjusted_Total_Error_mm) # sd adjusted TE = 14.640

mean(penshell.yes$Algorithm_Percent_Error) # mean algorithm PE = 9.017
sd(penshell.yes$Algorithm_Percent_Error) # sd algorithm PE = 57.661

mean(penshell.yes$Adjusted_Percent_Error) # mean adjusted PE = 1.664
sd(penshell.yes$Adjusted_Percent_Error) # sd adjusted PE = 11.864


##################################################################################################

# TASK 4 - Perform Two-Way ANOVA (or non-parametric equivalent) for Total Error Across Fisheries
# Still in Progress

# check assumptions

# normality

# Abalone Algorithm TE
hist(abalone.yes$Algorithm_Total_Error_mm) 
shapiro.test(abalone.yes$Algorithm_Total_Error_mm) # not normal

# Abalone Adjusted TE
hist(abalone.yes$Adjusted_Total_Error_mm) 
shapiro.test(abalone.yes$Adjusted_Total_Error_mm) # not normal

# Finfish Algorithm TE
hist(finfish.yes$Algorithm_Total_Error_mm)
shapiro.test(finfish.yes$Algorithm_Total_Error_mm) # not normal

# Finfish Adjusted TE
hist(finfish.yes$Adjusted_Total_Error_mm)
shapiro.test(finfish.yes$Adjusted_Total_Error_mm) # not normal

# Lobster Algorithm TE
hist(lobster.yes$Algorithm_Total_Error_mm)
shapiro.test(lobster.yes$Algorithm_Total_Error_mm) # not normal

# Lobster Adjusted TE
hist(lobster.yes$Adjusted_Total_Error_mm)
shapiro.test(lobster.yes$Adjusted_Total_Error_mm) # not normal

# Penshell Algorithm TE
hist(penshell.yes$Algorithm_Total_Error_mm)
shapiro.test(penshell.yes$Algorithm_Total_Error_mm) # not normal

# Penshell Adjusted TE
hist(penshell.yes$Adjusted_Total_Error_mm)
shapiro.test(penshell.yes$Adjusted_Total_Error_mm) # not normal



ErrorData <- read_excel("Final Data/CombinedFisheries_Error.xlsx") # read in error data 


TE.anova <- aov(Total_Error_mm ~ Fishery*Estimate_Type, data = ErrorData)
summary(TE.anova)

# from the anova table, we know
  # 1. Fishery has an effect on total error magnitude
  # 2. Estimate Type has an effect on total error magnitude
  # 3. The interaction between Fishery & Estimate Type has an effect on total error magnitude

tukey.Fishery <- HSD.test(TE.anova, "Fishery", group=TRUE)
print(tukey.Fishery)

tukey.EstimateType <- HSD.test(TE.anova, "Estimate_Type", group = TRUE)
print(tukey.EstimateType)

tukey.interaction <- HSD.test(TE.anova, c("Fishery", "Estimate_Type"), console=TRUE)
print(tukey.interaction)

par(mfrow=c(2,2))
plot(TE.anova)

library(nortest)

ad.test(resid(TE.anova))




######################################################################################################


# Task 5 

library(rethinking)



