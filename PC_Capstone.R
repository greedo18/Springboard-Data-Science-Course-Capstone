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
Attributes1 <- PlanAttributes %>% filter(BusinessYear == 2016 & DentalOnlyPlan == "No")
MetalLevel <- Crosswalk2016 %>% group_by(PlanID_2016, MetalLevel_2016) %>% summarise()
names(MetalLevel)[1] <- paste("Rate1.PlanId")
Rate4 <- left_join(Rate3, MetalLevel, by = "Rate1.PlanId")

# Get Plan attributes in order to setup cost calculation
Attributes2 <- Attributes1 %>% group_by(HIOSProductId, AVCalculatorOutputNumber, TEHBCombInnOonIndividualMOOP, TEHBDedCombInnOonIndividual, BenefitPackageId, FormularyId) %>% summarise()
names(Attributes2)[1] <- paste("Rate1.PlanIdshort")
Rate4 <- Rate4 %>% mutate(Rate1.PlanIdshort = strtrim(Rate4$Rate1.PlanId, 10))

# Split Rates by age buckets and Metal Level to setup yearly max spending for individuals
Rate21to30 <- Rate4 %>% filter(Rate4[8] == TRUE) %>% group_by(Rate1.PlanIdshort, MetalLevel_2016) %>% mutate(AVG.IndRate = mean(Rate1.IndividualRate)) %>% arrange(Rate1.PlanId)
Rate31to40 <- Rate4 %>% filter(Rate4[9] == TRUE) %>% group_by(Rate1.PlanIdshort, MetalLevel_2016) %>% mutate(AVG.IndRate = mean(Rate1.IndividualRate)) %>% arrange(Rate1.PlanId)
Rate41to50 <- Rate4 %>% filter(Rate4[10] == TRUE) %>% group_by(Rate1.PlanIdshort, MetalLevel_2016) %>% mutate(AVG.IndRate = mean(Rate1.IndividualRate)) %>% arrange(Rate1.PlanId)
Rate51to60 <- Rate4 %>% filter(Rate4[11] == TRUE) %>% group_by(Rate1.PlanIdshort, MetalLevel_2016) %>% mutate(AVG.IndRate = mean(Rate1.IndividualRate)) %>% arrange(Rate1.PlanId)
Rate60plus <- Rate4 %>% filter(Rate4[12] == TRUE) %>% group_by(Rate1.PlanIdshort, MetalLevel_2016) %>% mutate(AVG.IndRate = mean(Rate1.IndividualRate)) %>% arrange(Rate1.PlanId)

#Bind them together to get the final Individual RAte file
RateFinal <- bind_rows(Rate21to30, Rate31to40, Rate41to50, Rate51to60, Rate60plus)

# Group by Short ID and Metal LEvel to get short table versions of the data by age group
Rate21to30G <- Rate21to30 %>% group_by(Rate1.StateCode, Rate1.PlanIdshort, MetalLevel_2016, AVG.IndRate) %>% summarise() %>% arrange(Rate1.PlanIdshort)
Rate21to30G$Age <- "21to30"
Rate31to40G <- Rate31to40 %>% group_by(Rate1.StateCode, Rate1.PlanIdshort, MetalLevel_2016, AVG.IndRate) %>% summarise() %>% arrange(Rate1.PlanIdshort)
Rate31to40G$Age <- "31to40"
Rate41to50G <- Rate41to50 %>% group_by(Rate1.StateCode, Rate1.PlanIdshort, MetalLevel_2016, AVG.IndRate) %>% summarise() %>% arrange(Rate1.PlanIdshort)
Rate41to50G$Age <- "41to50"
Rate51to60G <- Rate51to60 %>% group_by(Rate1.StateCode, Rate1.PlanIdshort, MetalLevel_2016, AVG.IndRate) %>% summarise() %>% arrange(Rate1.PlanIdshort)
Rate51to60G$Age <- "51to60"
Rate60plusG <- Rate60plus %>% group_by(Rate1.StateCode, Rate1.PlanIdshort, MetalLevel_2016, AVG.IndRate) %>% summarise() %>% arrange(Rate1.PlanIdshort)
Rate60plusG$Age <- "60 plus"
#Bind them together to get shortened version for data exploration
RateFinalG <- bind_rows(Rate21to30G, Rate31to40G, Rate41to50G, Rate51to60G, Rate60plusG)

#Now to do some visualizations
library(ggplot2)
ggplot(RateFinalG, aes(x = Age, y = AVG.IndRate, col = MetalLevel_2016)) + geom_point() + facet_wrap(~Rate1.StateCode)
ggplot(RateFinalG, aes(x = Age, y = AVG.IndRate, col = MetalLevel_2016)) + geom_jitter()
ggplot(RateFinalG, aes(x = Age, y = AVG.IndRate, fill = MetalLevel_2016)) + geom_bar(position = "dodge", stat = "identity") + coord_flip()
ggplot(RateFinalG, aes(x = Age, y = AVG.IndRate, fill = MetalLevel_2016)) + geom_bar(position = "dodge", stat = "identity") + coord_flip() + facet_wrap(~Rate1.StateCode)

#Need to add more attributes plus remove NA's and group Low, High and Catastrophic, and Bronze, Silver, GOld, Platinum together
#Note: Performed Data manipualtion for Attributes 2 in excel, explained in final report
Attributes2 <- read_csv("C:/Users/greed/OneDrive/Documents/Springboard/Capstone/Attributes2.csv", 
                        +     col_types = cols(TEHBCombInnOonIndividualMOOP = col_number(), 
                                               +         TEHBDedCombInnOonIndividual = col_number()))
Attributes3 <- Attributes2 %>% group_by(Rate1.PlanIdshort, MetalLevel_2016) %>% summarise(AVG.AV = mean(AVCalculatorOutputNumber), AVG.MOOP = mean(TEHBCombInnOonIndividualMOOP))
RateFinaljoin <- left_join(RateFinalG, Attributes3, by = c("Rate1.PlanIdshort", "MetalLevel_2016"))
#Note: Perform some data manipulation in Excel, explained in final report
RateFinaljoin <- read_csv("C:/Users/greed/OneDrive/Documents/Springboard/Capstone/RateFinaljoin.csv")
RateFinaljoinfilter <- RateFinaljoin %>% filter(MaxOOPYearly != "NA")

#Some more visual exploration
ggplot(RateFinaljoinfilter, aes(x = MinOOPYearly, y = MaxOOPYearly, col = MetalLevel_2016)) + geom_point()
ggplot(RateFinaljoinfilter, aes(x = MinOOPYearly, y = MaxOOPYearly, col = MetalLevel_2016)) + geom_point() + facet_wrap(~Age)
#creating a map
library(mapdata)
library(maps)
RateFinaljoinfilter$region <- state.name[match(RateFinaljoinfilter$Rate1.StateCode, state.abb)]
RateFinaljoinfilter$region <- tolower(RateFinaljoinfilter$region)
TotalMap <- merge(states, RateFinaljoinfilter, by = "region")
ggplot() + geom_polygon(data = TotalMap, aes(x=long, y=lat, group=group, fill=TotalMap$MaxOOPYearly), colour = "white") + scale_fill_continuous(low="thistle2",high="darkred", guide="colorbar") + theme_bw()
