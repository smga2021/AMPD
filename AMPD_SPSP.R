---
#AMPD Clean Data

library(readr)
library(dplyr)
library(tidyverse)
library(car)
library(ggplot2)
install.packages("viridis")
library(viridis)

## Importing data set
AMPD_Prolific_January_25_2025_13_43 <- read_csv("Downloads/AMPD Prolific_January 31, 2025_15.21.csv")
View(AMPD_Prolific_January_31_2025_15_21)

## Making clean data frame with only people who consented
AMPDClean_1_ <- AMPD_Prolific_January_25_2025_13_43 %>% filter(Q16.1 == "Complete Study")

## Control data frame and download to delete unneccessary columns in Excel
Controldf <- AMPDClean_1_ %>% filter(Condition == "Control")

## Recoding all values to numeric forms
Controldf$AQ1_An...58 <- case_match(Controldf$AQ1_An...58,
                           "1 - none at all" ~ 1,
                           "2" ~ 2,
                           "3" ~ 3,
                           "4" ~ 4,
                           "5" ~ 5,
                           "6" ~ 6,
                           "7" ~ 7,
                           "8" ~ 8,
                           "9 - very much" ~ 9)

Controldf$AQ2_D...59 <- case_match(Controldf$AQ2_D...59,
                           "1"  ~ 1,
                           "2" ~ 2,
                           "3" ~ 3,
                           "4" ~ 4,
                           "5" ~ 5,
                           "6" ~ 6,
                           "7" ~ 7,
                           "8" ~ 8,
                           "9 - very much" ~ 9)

Controldf$AQ3_F...60 <- case_match(Controldf$AQ3_F...60,
                           "1 - not at all" ~ 1,
                           "2" ~ 2,
                           "3" ~ 3,
                           "4" ~ 4,
                           "5" ~ 5,
                           "6" ~ 6,
                           "7" ~ 7,
                           "8" ~ 8,
                           "9 - very much" ~ 9)

Controldf$AQ4_An...61 <- case_match(Controldf$AQ4_An...61,
                           "1 - not at all" ~ 1,
                           "2" ~ 2,
                           "3" ~ 3,
                           "4" ~ 4,
                           "5" ~ 5,
                           "6" ~ 6,
                           "7" ~ 7,
                           "8" ~ 8,
                           "9 - very much" ~ 9)

Controldf$AQ5_C...62 <- case_match(Controldf$AQ5_C...62,
                           "1 - not at all" ~ 1,
                           "2" ~ 2,
                           "3" ~ 3,
                           "4" ~ 4,
                           "5" ~ 5,
                           "6" ~ 6,
                           "7" ~ 7,
                           "8" ~ 8,
                           "9 - very much" ~ 9)

Controldf$Q8.7_2...63<- case_match(Controldf$Q8.7_2...63,
                           "1 - not at all" ~ 1,
                           "2" ~ 2,
                           "3" ~ 3,
                           "4" ~ 4,
                           "5" ~ 5,
                           "6" ~ 6,
                           "7" ~ 7,
                           "8" ~ 8,
                           "9 - very much" ~ 9)

Controldf$AQ7_Av...64 <- case_match(Controldf$AQ7_Av...64,
                           "1" ~ 1,
                           "2" ~ 2,
                           "3" ~ 3,
                           "4" ~ 4,
                           "5" ~ 5,
                           "6" ~ 6,
                           "7" ~ 7,
                           "8" ~ 8,
                           "9 - definitely would help" ~ 9)

Controldf$AQ8_H...65 <- case_match(Controldf$AQ8_H...65,
                           "1 - not at all" ~ 1,
                           "2" ~ 2,
                           "3" ~ 3,
                           "4" ~ 4,
                           "5" ~ 5,
                           "6" ~ 6,
                           "7" ~ 7,
                           "8" ~ 8,
                           "9 - very much" ~ 9)   

Controldf$AQ9_P...66 <- case_match(Controldf$AQ9_P...66,
                           "1 - not at all" ~ 1,
                           "2" ~ 2,
                           "3" ~ 3,
                           "4" ~ 4,
                           "5" ~ 5,
                           "6" ~ 6,
                           "7" ~ 7,
                           "8" ~ 8,
                           "9 - very much" ~ 9)  

## Reverse Coding for item 7
Controldf$AQ7_R <- 10 - Controldf$AQ7_Av...64

#Control, composite score
Controldf$AQ9Score <- rowMeans(Controldf[,c("AQ1_An...58", "AQ2_D...59","AQ3_F...60","AQ4_An...61", "AQ5_C...62", "Q8.7_2...63", "AQ7_R", "AQ8_H...65", "AQ9_P...66")])

mean(Controldf$AQ9Score)
sd(Controldf$AQ9Score)

## AMPD data frame 
AMPDdf_1_ <- AMPDClean_1_ %>% filter(Condition == "AMPD")

## Recoding all values to numeric forms for AMPD condition
recodevalues <-  c("9 - very much" = 9, "8" = 8, "7" = 7, "6" = 6, "5" = 5, "4" = 4, "3" = 3, "2" = 2, "1 - none at all" = 1)
AMPDdf_1_$AQ1_An...43 <- recodevalues[AMPDdf_1_$AQ1_An...43]

recodevalues1 <-  c("9 - very much" = 9, "8" = 8, "7" = 7, "6" = 6, "5" = 5, "4" = 4, "3" = 3, "2" = 2, "1 - not at all" = 1)
AMPDdf_1_$AQ2_D...44 <- recodevalues1[AMPDdf_1_$AQ2_D...44]
AMPDdf_1_$AQ3_F...45 <- recodevalues1[AMPDdf_1_$AQ3_F...45]
AMPDdf_1_$AQ4_An...46 <- recodevalues1[AMPDdf_1_$AQ4_An...46]
AMPDdf_1_$AQ5_C...47 <- recodevalues1[AMPDdf_1_$AQ5_C...47]
AMPDdf_1_$Q8.7_2...48 <- recodevalues1[AMPDdf_1_$Q8.7_2...48]
AMPDdf_1_$AQ8_H...50 <- recodevalues1[AMPDdf_1_$AQ8_H...50]
AMPDdf_1_$AQ9_P...51 <- recodevalues1[AMPDdf_1_$AQ9_P...51]

recodevalues2 <-  c("9 - definitely would help" = 9, "8" = 8, "7" = 7, "6" = 6, "5" = 5, "4" = 4, "3" = 3, "2" = 2, "1 - definitely would not help" = 1)
AMPDdf_1_$AQ7_Av...49 <- recodevalues2[AMPDdf_1_$AQ7_Av...49]

## Reverse Coding for item 7
AMPDdf_1_$AQ7_R <- 10 - AMPDdf_1_$AQ7_Av...49

##AMPD, composite score
AMPDdf_1_$AQ9Score <- rowMeans(AMPDdf_1_[,c("AQ1_An...43", "AQ2_D...44","AQ3_F...45","AQ4_An...46", "AQ5_C...47", "Q8.7_2...48", "AQ7_R", "AQ8_H...50", "AQ9_P...51")])

mean(AMPDdf_1_$AQ9Score)
sd(AMPDdf_1_$AQ9Score)

## DSM data frame and download to delete unneccessary columns in Excel
DSMdf_1_ <- AMPDClean_1_ %>% filter(Condition == "DSM")

## Recoding all values to numeric forms for DSM condition
DSMdf_1_$AQ1_An...23 <- recodevalues[DSMdf_1_$AQ1_An...23]

DSMdf_1_$AQ2_D...24 <- recodevalues1[DSMdf_1_$AQ2_D...24]
DSMdf_1_$AQ3_F...25 <- recodevalues1[DSMdf_1_$AQ3_F...25]
DSMdf_1_$AQ4_An...26 <- recodevalues1[DSMdf_1_$AQ4_An...26]
DSMdf_1_$AQ5_C...27 <- recodevalues1[DSMdf_1_$AQ5_C...27]
DSMdf_1_$Q8.7_2...28 <- recodevalues1[DSMdf_1_$Q8.7_2...28]
DSMdf_1_$AQ8_H...30 <- recodevalues1[DSMdf_1_$AQ8_H...30]
DSMdf_1_$AQ9_P...31 <- recodevalues1[DSMdf_1_$AQ9_P...31]

DSMdf_1_$AQ7_Av...29 <- recodevalues2[DSMdf_1_$AQ7_Av...29]

## Reverse Coding for item 7

DSMdf_1_$AQ7_R <- 10 - DSMdf_1_$AQ7_Av...29

##DSM, composite score
DSMdf_1_$AQ9Score <- rowMeans(DSMdf_1_[,c("AQ1_An...23", "AQ2_D...24","AQ3_F...25","AQ4_An...26", "AQ5_C...27", "Q8.7_2...28", "AQ7_R", "AQ8_H...30", "AQ9_P...31")])

mean(DSMdf_1_$AQ9Score)
sd(DSMdf_1_$AQ9Score)

## One-way Anova, create a combined data frame with composite scores

# Create the Condition vector
Condition <- c(rep("Control", 130), rep("AMPD", 133), rep("DSM", 135))

# Generate random data for AQ9_scores based on the means and standard deviations for each group
AQ9_scores <- c(
  rnorm(130, mean = 4.94188, sd = 1.145231),   # Control group
  rnorm(133, mean = 4.859649, sd = 1.244434),    # AMPD group
  rnorm(135, mean = 4.873251, sd = 1.124067)   # DSM group
)

# Combine into a data frame
Compositedf <- data.frame(Condition = factor(Condition), AQ9_scores)

# Perform the one-way ANOVA
anova_result <- aov(AQ9_scores ~ Condition, data = Compositedf)

# Display ANOVA result
summary(anova_result)


## Assumptions of ANOVA
leveneTest(AQ9_scores ~ Condition, data = Compositedf)

anova_result$coefficients

TukeyHSD(anova_result)

lm.model <- lm(AQ9_scores ~ Condition)
summary(lm.model)

## Residual Plots
plot(residuals(anova_result))

## Boxplot

plot1 <- ggplot(Compositedf, aes(x = as.factor(Condition), y = AQ9_scores, fill = Condition)) +
  geom_boxplot(outlier.shape = 16, outlier.colour = "gray", width = 0.5, 
               color = "black", lwd = 0.75) +  # Simple black border with narrower width
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral")) +  # Subtle solid colors
  labs(title = "Average Stigmatizing Attitudes Score",
       x = "Condition", 
       y = "Average AQ-9 Scores") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Horizontal axis labels for clarity
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and bold the title
    axis.title = element_text(size = 14, face = "bold"),  # Bold axis titles
    axis.text = element_text(size = 12),  # Slightly smaller axis text for readability
    legend.position = "none"  # Remove the legend (if not necessary)
  )

# Display plot
print(plot1)

# Comprehension tests

#AMPD - Recoding values
comprecode <-  c("Strongly disagree" = -3, "Disagree" = -2, "Somewhat disagree" = -1, "Neither agree nor disagree" = 0, "Somewhat agree" = 1, "Agree" = 2, "Strongly agree" = 3)

AMPDdf_1_$Comprehension_1 <- comprecode[AMPDdf_1_$Comprehension_1]
AMPDdf_1_$Comprehension_2 <- comprecode[AMPDdf_1_$Comprehension_2]
AMPDdf_1_$Comprehension_3 <- comprecode[AMPDdf_1_$Comprehension_3]

#DSM - Recoding values

DSMdf_1_$Q145_1 <- comprecode[DSMdf_1_$Q145_1]
DSMdf_1_$Q145_2 <- comprecode[DSMdf_1_$Q145_2]
DSMdf_1_$Q145_3 <- comprecode[DSMdf_1_$Q145_3]

#Correlations per item for AMPD
anna <- cor.test(AMPDdf_1_$Comprehension_1, AMPDdf_1_$AQ9Score)
model <- cor.test(AMPDdf_1_$Comprehension_2, AMPDdf_1_$AQ9Score)
attention <- cor.test(AMPDdf_1_$Comprehension_3, AMPDdf_1_$AQ9Score)

#Correlations per item for DSM
anna2 <- cor.test(DSMdf_1_$Q145_1, DSMdf_1_$AQ9Score)
model2 <-cor.test(DSMdf_1_$Q145_2, DSMdf_1_$AQ9Score)
attention2 <- cor.test(DSMdf_1_$Q145_3, DSMdf_1_$AQ9Score)

# Writing three condition dataframes to csv files in order to merge
write.csv(AMPDdf_1_, "~/Downloads/AMPDdata.csv", row.names = FALSE)
write.csv(DSMdf_1_, "~/Downloads/DSMdata.csv", row.names = FALSE)
write.csv(Controldf, "~/Downloads/Condata.csv", row.names = FALSE)

# Import cleaned data
Alldata <- read_csv("Alldata.csv")
View(Alldata)

#Renaming data frame and its variables
knowdf <- Alldata %>% 
  rename(bpdme = Q10.2, bpdknow = Q10.5)

# Filtering for only Yes/No Answers for BPD Knowledge and for BPD diagnosis 
knowdf <- knowdf %>% filter(bpdknow != "Prefer not to answer")
knowdf <- knowdf %>% filter(bpdme != "I prefer not to answer")

# t-test for BPD Knowledge between Yes or No answers
t.test(AQ9Score ~ bpdknow, paired=FALSE, var.equal=FALSE, conf.level=0.95, data = knowdf)

# t-test for BPD diagnosis between Yes or No answers
t.test(AQ9Score ~ bpdme, paired=FALSE, var.equal=FALSE, conf.level=0.95, data = knowdf)
