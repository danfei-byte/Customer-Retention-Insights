setwd("/Users/dynamite/Desktop/MSMA/Action Learning Project/dataset")
# import csv and explore holistic data pattern
library(tidyverse)
library(readr) # use readr package to load data
library(skimr) 
d2 <- read_csv("d2.csv", na = c("", " ", "NULL", "."))
skim(d2) # provide a detailed summary of data

# Format variables
## Convert characters to numeric
d2$PHN_CREDITSAT[d2$PHN_CREDITSAT == "NA"] <- NA
d2$PHN_DWNPYMTSAT[d2$PHN_DWNPYMTSAT == "NA"] <- NA
d2$PHN_FINSAT[d2$PHN_FINSAT == "NA"] <- NA
d2$PHN_TRADESAT[d2$PHN_TRADESAT == "NA"] <- NA

columns_to_convert <- c("PHN_CREDITSAT", "PHN_DWNPYMTSAT", "PHN_FINSAT", "PHN_TRADESAT") # list column names to convert
d2[columns_to_convert] <- lapply(d2[columns_to_convert], as.numeric)
sapply(d2[columns_to_convert], class)

library(ggplot2)
library(dplyr)
#use heatmap to find response patterns in satisfaction
# first create a tabulated table
class(d2$MODEL_SCORE) # make sure Model_score is numeric for ease of next steps, if not, please convert first
d2$MODEL_SCORE <- as.numeric(d2$MODEL_SCORE)
sums <- d2 %>%
  group_by(MODEL_SCORE) %>%
  summarise(across(c(COVERRECSAT, DATASPEEDSAT, PRICEPLANSAT, BILLPYMTSAT, CUSTSERVSAT, 
                     EASEMGMTSAT, FEATPERKSSAT, PHNUPGDSAT, PHN_TRADESAT, PHN_CREDITSAT, 
                     PHN_FINSAT, PHN_DWNPYMTSAT), ~ mean(.x, na.rm = TRUE)))

sums <- subset(sums, !is.na(MODEL_SCORE)) # remove the row where Model_score=NA

freq_table <- d2 %>% # add frequency column
  group_by(MODEL_SCORE) %>%
  summarise(Freq = n())

sums <- left_join(sums, freq_table, by = "MODEL_SCORE") #join the frequency column to the original summary

freq_col <- grep("^Freq", colnames(sums), value = TRUE) # Find the frequency column (currently there are freq.y, freq.x, etc)
sums <- sums %>% # Reorder the columns as needed
  rename(Freq = all_of(freq_col)) %>%
  select(MODEL_SCORE, Freq, COVERRECSAT, DATASPEEDSAT, PRICEPLANSAT, BILLPYMTSAT,
         CUSTSERVSAT, EASEMGMTSAT, FEATPERKSSAT, PHNUPGDSAT,
         PHN_TRADESAT, PHN_CREDITSAT, PHN_FINSAT, PHN_DWNPYMTSAT)
print(sums, width = Inf)


## second, create the heat map
install.packages("reshape2")
library(reshape2)

sums_heat <- melt(sums[, -2],  # Melt the data and round the mean values to 2 decimal places
                  id.vars = "MODEL_SCORE", 
                  variable.name = "variable", 
                  value.name = "average_score")
sums_heat$average_score <- round(sums_heat$average_score, 2) # Round the mean_value for visualization
sums_heat$MODEL_SCORE <- factor(sums_heat$MODEL_SCORE, levels = 1:14) #convert model_score to factor to main order and labels

ggplot(sums_heat, aes(x = variable, y = MODEL_SCORE, fill = average_score)) +
  geom_tile(color = "white") + # Add tile border for clarity
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = average_score), color = "black", size = 3) + # Display values
  theme_minimal() +
  labs(title = "Satisfaction Scores by Cohort", 
       x = "Satisfaction", 
       y = "Cohort") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) + # Rotate x-axis labels
  scale_y_discrete(breaks = 1:20) # Use discrete scale for MODEL_SCORE