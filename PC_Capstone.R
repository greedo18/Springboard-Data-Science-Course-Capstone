# December 2016- Springboard.com Capstone script for Pierre Carpentier
# Fundamentals of Data Science

# load kaggle data sets for US healthcare policies.
library(readr)
BenefitsCostSharing <- read_csv("C:/Users/greed/OneDrive/Documents/Springboard/Capstone/health-insurance-marketplace-release-2016-01-20-15-52-37/health-insurance-marketplace/BenefitsCostSharing.csv")
BusinessRules <- read_csv("C:/Users/greed/OneDrive/Documents/Springboard/Capstone/health-insurance-marketplace-release-2016-01-20-15-52-37/health-insurance-marketplace/BusinessRules.csv")
Network <- read_csv("C:/Users/greed/OneDrive/Documents/Springboard/Capstone/health-insurance-marketplace-release-2016-01-20-15-52-37/health-insurance-marketplace/Network.csv")
PlanAttributes <- read_csv("C:/Users/greed/OneDrive/Documents/Springboard/Capstone/health-insurance-marketplace-release-2016-01-20-15-52-37/health-insurance-marketplace/PlanAttributes.csv")
Rate <- read_csv("C:/Users/greed/OneDrive/Documents/Springboard/Capstone/health-insurance-marketplace-release-2016-01-20-15-52-37/health-insurance-marketplace/Rate.csv")
ServiceArea <- read_csv("C:/Users/greed/OneDrive/Documents/Springboard/Capstone/health-insurance-marketplace-release-2016-01-20-15-52-37/health-insurance-marketplace/ServiceArea.csv")
Crosswalk2016 <- read_csv("C:/Users/greed/OneDrive/Documents/Springboard/Capstone/health-insurance-marketplace-release-2016-01-20-15-52-37/health-insurance-marketplace/Crosswalk2016.csv")

# Exploratory merging of the data

# Take plans from current business Year as the other years are expired plans and no longer available
library(dplyr)
library(tidyr)

Rate1 <- Rate %>% filter(BusinessYear == "2016") %>% arrange(PlanId) 
# Keep only interesting columns and sort Age by groups of 10 year buckets
Rate1 <- data.frame(Rate1$StateCode, Rate1$IssuerId, Rate1$PlanId, Rate1$RatingAreaId, Rate1$Tobacco, Rate1$Age, Rate1$IndividualRate)
Rate1 <- arrange(Rate1, Rate1.Age)
Rate2 <- Rate1 %>% filter(Rate1.Age != "0-20")
Rate2$Rate1.Age <- as.numeric(as.character(Rate2$Rate1.Age))

#Expand age buckets using the mutate function
Rate3 <- Rate2 %>% mutate("21-30" = between(Rate1.Age, 21, 30)) %>% mutate("31-40" = between(Rate1.Age, 31, 40)) %>% mutate ("41-50" = between(Rate1.Age, 41, 50)) %>% mutate("51-60" = between(Rate1.Age, 51, 60)) %>% mutate("60+" = Rate1.Age > 60)

#Now to add basic benefits data to the Rates
Benefits1 <- BenefitsCostSharing %>% filter(BusinessYear == 2016)
Attributes1 <- PlanAttributes %>% filter(BusinessYear == 2016)
MetalLevel <- Crosswalk2016 %>% group_by(PlanID_2016, MetalLevel_2016) %>% summarise()
names(MetalLevel)[1] <- paste("Rate1.PlanId")
Rate4 <- left_join(Rate3, MetalLevel, by = "Rate1.PlanId")

# Get Plan attributes in order to setup cost calculation
Attributes2 <- Attributes1 %>% group_by(HIOSProductId, AVCalculatorOutputNumber, TEHBCombInnOonIndividualMOOP, TEHBDedCombInnOonIndividual) %>% summarise()
names(Attributes2)[1] <- paste("Rate1.PlanIdshort")
Rate4 <- Rate4 %>% mutate(Rate1.PlanIdshort = strtrim(Rate4$Rate1.PlanId, 10))
Rate5 <- left_join(Rate4, Attributes2, by = "Rate1.PlanIdshort")