#tidyverse
library(tidyverse)
library(fixest)
library(stargazer)
library(did)
library(modelsummary)
library(kableExtra)

#set project directory
setwd("./airbnbproject")

#parallel trends testing




  #home values: slope comparison

    #manipulating and preparing database for slope comparison
HVparallel <- read_csv("homevaluedb.csv")
HVparallel <- HVparallel %>%
  pivot_longer(
    cols = -City,
    names_to = "Date",
    values_to = "HV"
  ) 

    #convert slope values to monthly rather than daily rate of change
HVparallel <- HVparallel %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%y"),
    YearMonth = format(Date, "%Y-%m")
  ) %>%
  filter(as.Date(Date, format = "%m/%d/%y") <= as.Date("2023-08-31"))

HVparallel <- HVparallel %>%
  group_by(City) %>%
  arrange(Date) %>%
  mutate(MonthIndex = row_number()) %>%
  ungroup()

    #calculating all city slopes
HVslope <- HVparallel %>%
  group_by(City) %>%
  summarize(
    Slope = coef(lm(HV ~ MonthIndex))[2]
  ) %>%
  arrange(abs(Slope))

    #make variable filtering each NY borough slope
HVmanhattan_slope <- HVslope %>% filter(City == "Manhattan") %>% pull(Slope)
HVqueens_slope <- HVslope %>% filter(City == "Queens") %>% pull(Slope)
HVbrooklyn_slope <- HVslope %>% filter(City == "Brooklyn") %>% pull(Slope)
HVny_slope <- HVslope %>% filter(City == "New York") %>% pull(Slope)

    #compare city slopes against NY borough slope
HV_slope_comparison <- HVslope %>%
  mutate(
    DistanceFromNY = abs(Slope - HVny_slope),
    DistanceFromQueens = abs(Slope - HVqueens_slope),
    DistanceFromBrooklyn = abs(Slope - HVbrooklyn_slope),
    DistanceFromManhattan = abs(Slope - HVmanhattan_slope)
                  )


  #home values: visual comparison



manhattan_HV_control <- c("Manhattan", "Phoenix", "Boston", "Washington", "Denver", "Chicago", "Detroit", "Honolulu", "Los Angeles")
#consider removing detroit, los angeles, chicago, maybe honolulu
ggplot(HVparallel %>% filter(City %in% manhattan_HV_control), aes(x = Date, y = HV, color = City)) +
  geom_line(size = 1) +
  labs(
    title = "Home Value Over Time by City",
    x = "Month",
    y = "Home Value",
    color = "City"
  ) +
  theme_minimal()



queens_HV_control <- c("Queens", "Detroit", "Philadelphia", "Chicago", "Saint Paul","Baltimore")
#removed honolulu, long beach, minneapolis, consider readding
#this city is dedicated to sara
ggplot(HVparallel %>% filter(City %in% queens_HV_control), aes(x = Date, y = HV, color = City)) +
  geom_line(size = 1) +
  labs(
    title = "Home Value Over Time by City",
    x = "Month",
    y = "Home Value",
    color = "City"
  ) +
  theme_minimal()



brooklyn_HV_control <- c("Brooklyn","Houston","Dallas","Atlanta","Saint Louis","Baltimore")
#removed san diego and tampa, consider removing baltimore, columbia
ggplot(HVparallel %>% filter(City %in% brooklyn_HV_control), aes(x = Date, y = HV, color = City)) +
  geom_line(size = 1) +
  labs(
    title = "Home Value Over Time by City",
    x = "Month",
    y = "Home Value",
    color = "City"
  ) +
  theme_minimal()



ny_HV_control <- c("New York","Baltimore","Saint Paul", "Philadelphia", "Atlanta","Detroit","Chicago")
#removed honolulu, consider removing Saint Paul and Chicago
ggplot(HVparallel %>% filter(City %in% ny_HV_control), aes(x = Date, y = HV, color = City)) +
  geom_line(size = 1) +
  labs(
    title = "Home Value Over Time by City",
    x = "Month",
    y = "Home Value",
    color = "City"
  ) +
  theme_minimal()







  #rent values: slope comparison
    #manipulating and preparing database for slope comparison
RVparallel <- read_csv("rentvaluedb.csv")
RVparallel <- RVparallel %>%
  pivot_longer(
    cols = -City,
    names_to = "Date",
    values_to = "RV"
  ) 

    #convert slope values to monthly rather than daily rate of change
RVparallel <- RVparallel %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%y"),
    YearMonth = format(Date, "%Y-%m")
  ) %>%
  filter(as.Date(Date, format = "%m/%d/%y") <= as.Date("2023-08-31"))

RVparallel <- RVparallel %>%
  group_by(City) %>%
  arrange(Date) %>%
  mutate(MonthIndex = row_number()) %>%
  ungroup()

    #calculating all city slopes
RVslope <- RVparallel %>%
  group_by(City) %>%
  summarize(
    Slope = coef(lm(RV ~ MonthIndex))[2]
  ) %>%
  arrange(abs(Slope))

    #make variable filtering each NY borough slope
RVmanhattan_slope <- RVslope %>% filter(City == "Manhattan") %>% pull(Slope)
RVqueens_slope <- RVslope %>% filter(City == "Queens") %>% pull(Slope)
RVbrooklyn_slope <- RVslope %>% filter(City == "Brooklyn") %>% pull(Slope)
RVny_slope <- RVslope %>% filter(City == "New York") %>% pull(Slope)

    #compare city slopes against NY borough slope
RV_slope_comparison <- RVslope %>%
  mutate(
    DistanceFromNY = abs(Slope - RVny_slope),
    DistanceFromQueens = abs(Slope - RVqueens_slope),
    DistanceFromBrooklyn = abs(Slope - RVbrooklyn_slope),
    DistanceFromManhattan = abs(Slope - RVmanhattan_slope)
  )





  #rent values: visual comparison



manhattan_RV_control <- c("Manhattan", "Boston","San Diego","Miami","Chicago","Honolulu")
#removed miami
ggplot(RVparallel %>% filter(City %in% manhattan_RV_control), aes(x = Date, y = RV, color = City)) +
  geom_line(size = 1) +
  labs(
    title = "Rent Value Over Time by City",
    x = "Month",
    y = "Rent Value",
    color = "City"
  ) +
  theme_minimal()



queens_RV_control <- c("Queens", "San Diego","Boston","Chicago", "Miami")
#removed los angeles, honolulu, miami
ggplot(RVparallel %>% filter(City %in% queens_RV_control), aes(x = Date, y = RV, color = City)) +
  geom_line(size = 1) +
  labs(
    title = "Rent Value Over Time by City",
    x = "Month",
    y = "Rent Value",
    color = "City"
  ) +
  theme_minimal()



brooklyn_RV_control <- c("Brooklyn", "Boston","San Diego","Chicago","Anaheim", "Honolulu")
#removed miami, la
ggplot(RVparallel %>% filter(City %in% brooklyn_RV_control), aes(x = Date, y = RV, color = City)) +
  geom_line(size = 1) +
  labs(
    title = "Rent Value Over Time by City",
    x = "Month",
    y = "Rent Value",
    color = "City"
  ) +
  theme_minimal()



ny_RV_control <- c("New York", "Boston","San Diego","Chicago","Anaheim")
#removed miami, honolulu, los angeles,
ggplot(RVparallel %>% filter(City %in% ny_RV_control), aes(x = Date, y = RV, color = City)) +
  geom_line(size = 1) +
  labs(
    title = "Rent Value Over Time by City",
    x = "Month",
    y = "Rent Value",
    color = "City"
  ) +
  theme_minimal()






#did regression




  #home values: preparing and cleaning dataset to merge
HVdb <- read_csv("homevaluedb.csv")
HVdb <- HVdb %>%
  pivot_longer(
    cols = -City,
    names_to = "Date",
    values_to = "Home_Value"
  ) 
RVdb <- read_csv("rentvaluedb.csv")
RVdb <- RVdb %>%
  pivot_longer(
    cols = -City,
    names_to = "Date",
    values_to = "Rent_Value"
  )

  #merge databases
diddb <- left_join(HVdb,RVdb, by = c("City","Date"))

  #prepare dummy variables, filtered cities, 
didcontrol <- paste(unique(c(manhattan_HV_control, queens_HV_control, brooklyn_HV_control, ny_HV_control, manhattan_RV_control, queens_RV_control, brooklyn_RV_control, ny_RV_control)))
diddb <- diddb %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%y"),
    post = if_else(Date > as.Date("2023-08-31"), 1, 0),
    nyc = if_else(City %in% c("Brooklyn", "Queens", "Manhattan", "New York"), 1, 0),
    postxnyc = post*nyc
  ) %>%
  filter(City %in% didcontrol)

  #merge cpi, ur, lf
cpi <- read_csv("cpi.csv")
cpi <- cpi %>%
  pivot_longer(
    cols = -Date,
    names_to = "City",
    values_to = "CPI"
  ) 
cpi <- cpi %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))
diddb <- left_join(diddb,cpi, by = c("City","Date"))

UR <- read_csv("unemploymentrate.csv")
UR <- UR %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))
diddb <- left_join(diddb,UR, by = c("City","Date"))

LF <- read_csv("laborforce.csv")
LF <- LF %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))
diddb <- left_join(diddb,LF, by = c("City","Date"))

airroi <- read_csv("airbnb_data.csv")
airroi <- airroi %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))
diddb <- left_join(diddb,airroi, by = c("City","Date"))

#summary statistic of database
stargazer(as.data.frame(select(diddb, where(is.numeric))),
          type = "text",
          title = "Summary Statistics Table",
          summary.stat = c("mean", "sd", "min", "max", "n"),
          digits = 2)









  #do DiD Zillow regressions for each NYC area and outcome



    #queens


      #home value
        #simple regression (no fixed effects, only dummy)
QsHV <- feols(Home_Value ~ postxnyc + UR + LF + CPI | post + nyc, 
             data = diddb[diddb$City %in% queens_HV_control, ],
             cluster = ~Date)
summary(QsHV)
        #fixed effects regression
QfHV <- feols(Home_Value ~ postxnyc + UR + LF + CPI | City + Date, 
              data = diddb[diddb$City %in% queens_HV_control, ],
              cluster = ~Date)
summary(QfHV)

      #rent value
QsRV <- feols(Rent_Value ~ postxnyc + UR + LF + CPI | post + nyc, 
              data = diddb[diddb$City %in% queens_RV_control, ],
              cluster = ~Date)
summary(QsRV)

QfRV <- feols(Rent_Value ~ postxnyc + UR + LF + CPI | City + Date, 
              data = diddb[diddb$City %in% queens_RV_control, ],
              cluster = ~Date)
summary(QfRV)



    #brooklyn


      #home value
BsHV <- feols(Home_Value ~ postxnyc + UR + LF + CPI | post + nyc, 
              data = diddb[diddb$City %in% brooklyn_HV_control, ],
              cluster = ~Date)
summary(BsHV)

BfHV <- feols(Home_Value ~ postxnyc + UR + LF + CPI | City + Date, 
              data = diddb[diddb$City %in% brooklyn_HV_control, ],
              cluster = ~Date)
summary(BfHV)
     
      #rent value
BsRV <- feols(Rent_Value ~ postxnyc + UR + LF + CPI | post + nyc, 
              data = diddb[diddb$City %in% brooklyn_RV_control, ],
              cluster = ~Date)
summary(BsRV)

BfRV <- feols(Rent_Value ~ postxnyc + UR + LF + CPI | City + Date, 
              data = diddb[diddb$City %in% brooklyn_RV_control, ],
              cluster = ~Date)
summary(BfRV)



    #manhattan 



      #home value
MsHV <- feols(Home_Value ~ postxnyc + UR + LF + CPI | post + nyc, 
              data = diddb[diddb$City %in% manhattan_HV_control, ],
              cluster = ~Date)
summary(MsHV)

MfHV <- feols(Home_Value ~ postxnyc + UR + LF + CPI | City + Date, 
              data = diddb[diddb$City %in% manhattan_HV_control, ],
              cluster = ~Date)
summary(MfHV)



      #rent value
MsRV <- feols(Rent_Value ~ postxnyc + UR + LF + CPI | post + nyc, 
              data = diddb[diddb$City %in% manhattan_RV_control, ],
              cluster = ~Date)
summary(MsRV)

MfRV <- feols(Rent_Value ~ postxnyc + UR + LF + CPI | City + Date, 
              data = diddb[diddb$City %in% manhattan_RV_control, ],
              cluster = ~Date)
summary(MfRV)


    #new york


      #home value
NYsHV <- feols(Home_Value ~ postxnyc + postxnyc + UR + LF + CPI | post + nyc, 
              data = diddb[diddb$City %in% ny_HV_control, ],
              cluster = ~Date)
summary(NYsHV)

NYfHV <- feols(Home_Value ~ postxnyc + UR + LF + CPI | City + Date, 
              data = diddb[diddb$City %in% ny_HV_control, ],
              cluster = ~Date)
summary(NYfHV)

      #rent value
NYsRV <- feols(Rent_Value ~ postxnyc + UR + LF + CPI | post + nyc, 
              data = diddb[diddb$City %in% ny_RV_control, ],
              cluster = ~Date)
summary(NYsRV)

NYfRV <- feols(Rent_Value ~ postxnyc + UR + LF + CPI | City + Date, 
              data = diddb[diddb$City %in% ny_RV_control, ],
              cluster = ~Date)
summary(NYfRV)






















      #PLACEBO zillow

didplacebo <- diddb %>%
  filter(Date <= as.Date("2023-08-31")) %>%
  mutate(post = if_else(Date > as.Date("2022-11-30"), 1, 0),
         postxnyc = post*nyc)



#queens


#home value
#simple regression (no fixed effects, only dummy)
QsHVP <- feols(Home_Value ~ postxnyc + UR + LF + CPI | post + nyc, 
              data = didplacebo[didplacebo$City %in% queens_HV_control, ],
              cluster = ~Date)
summary(QsHVP)
#fixed effects regression
QfHVP <- feols(Home_Value ~ postxnyc + UR + LF + CPI | City + Date, 
              data = didplacebo[didplacebo$City %in% queens_HV_control, ],
              cluster = ~Date)
summary(QfHVP)

#rent value
QsRVP <- feols(Rent_Value ~ postxnyc + UR + LF + CPI | post + nyc, 
              data = didplacebo[didplacebo$City %in% queens_RV_control, ],
              cluster = ~Date)
summary(QsRVP)

QfRVP <- feols(Rent_Value ~ postxnyc + UR + LF + CPI | City + Date, 
              data = didplacebo[didplacebo$City %in% queens_RV_control, ],
              cluster = ~Date)
summary(QfRVP)



#brooklyn


#home value
BsHVP <- feols(Home_Value ~ postxnyc + UR + LF + CPI | post + nyc, 
              data = didplacebo[didplacebo$City %in% brooklyn_HV_control, ],
              cluster = ~Date)
summary(BsHVP)

BfHVP <- feols(Home_Value ~ postxnyc + UR + LF + CPI | City + Date, 
              data = didplacebo[didplacebo$City %in% brooklyn_HV_control, ],
              cluster = ~Date)
summary(BfHVP)

#rent value
BsRVP <- feols(Rent_Value ~ postxnyc + UR + LF + CPI | post + nyc, 
              data = didplacebo[didplacebo$City %in% brooklyn_RV_control, ],
              cluster = ~Date)
summary(BsRVP)

BfRVP <- feols(Rent_Value ~ postxnyc + UR + LF + CPI | City + Date, 
              data = didplacebo[didplacebo$City %in% brooklyn_RV_control, ],
              cluster = ~Date)
summary(BfRVP)



#manhattan 



#home value
MsHVP <- feols(Home_Value ~ postxnyc + UR + LF + CPI | post + nyc, 
              data = didplacebo[didplacebo$City %in% manhattan_HV_control, ],
              cluster = ~Date)
summary(MsHVP)

MfHVP <- feols(Home_Value ~ postxnyc + UR + LF + CPI | City + Date, 
              data = didplacebo[didplacebo$City %in% manhattan_HV_control, ],
              cluster = ~Date)
summary(MfHVP)



#rent value
MsRVP <- feols(Rent_Value ~ postxnyc + UR + LF + CPI | post + nyc, 
              data = didplacebo[didplacebo$City %in% manhattan_RV_control, ],
              cluster = ~Date)
summary(MsRVP)

MfRVP <- feols(Rent_Value ~ postxnyc + UR + LF + CPI | City + Date, 
              data = didplacebo[didplacebo$City %in% manhattan_RV_control, ],
              cluster = ~Date)
summary(MfRVP)


#new york


#home value
NYsHVP <- feols(Home_Value ~ postxnyc + postxnyc + UR + LF + CPI | post + nyc, 
               data = didplacebo[didplacebo$City %in% ny_HV_control, ],
               cluster = ~Date)
summary(NYsHVP)

NYfHVP <- feols(Home_Value ~ postxnyc + UR + LF + CPI | City + Date, 
               data = didplacebo[didplacebo$City %in% ny_HV_control, ],
               cluster = ~Date)
summary(NYfHVP)

#rent value
NYsRVP <- feols(Rent_Value ~ postxnyc + UR + LF + CPI | post + nyc, 
               data = didplacebo[didplacebo$City %in% ny_RV_control, ],
               cluster = ~Date)
summary(NYsRVP)

NYfRVP <- feols(Rent_Value ~ postxnyc + UR + LF + CPI | City + Date, 
               data = didplacebo[didplacebo$City %in% ny_RV_control, ],
               cluster = ~Date)
summary(NYfRVP)
















  #airbnb regression, only nyc, but 6 variables
ny_airbnb_control <- c("New York", "Chicago", "Philadelphia", "San Diego", "Boston" )
#removed "Los Angeles", "Miami"

      #occupancy
NYsO <- feols(Occupancy ~ postxnyc + postxnyc + UR + LF + CPI | post + nyc, 
               data = diddb[diddb$City %in% ny_airbnb_control, ],
               cluster = ~Date)
summary(NYsO)

NYfO <- feols(Occupancy ~ postxnyc + UR + LF + CPI | City + Date, 
               data = diddb[diddb$City %in% ny_airbnb_control, ],
               cluster = ~Date)
summary(NYfO)

      # property revenue
NYsPR <- feols(Property_Revenue ~ postxnyc + UR + LF + LFC + CPI | post + nyc, 
               data = diddb[diddb$City %in% ny_airbnb_control, ],
               cluster = ~Date)
summary(NYsPR)

NYfPR <- feols(Property_Revenue ~ postxnyc + UR + LF + LFC + CPI | City + Date, 
               data = diddb[diddb$City %in% ny_airbnb_control, ],
               cluster = ~Date)
summary(NYfPR)

      #entire home revenue
NYsEHR <- feols(Entire_Home_Revenue ~ postxnyc + postxnyc + UR + LF + CPI | post + nyc, 
              data = diddb[diddb$City %in% ny_airbnb_control, ],
              cluster = ~Date)
summary(NYsEHR)

NYfEHR <- feols(Entire_Home_Revenue ~ postxnyc + UR + LF + CPI | City + Date, 
              data = diddb[diddb$City %in% ny_airbnb_control, ],
              cluster = ~Date)
summary(NYfEHR)


    #private room revenue
NYsPRR <- feols(Private_Room_Revenue ~ postxnyc + postxnyc + UR + LF + CPI | post + nyc, 
                data = diddb[diddb$City %in% ny_airbnb_control, ],
                cluster = ~Date)
summary(NYsPRR)

NYfPRR <- feols(Private_Room_Revenue ~ postxnyc + UR + LF + CPI | City + Date, 
                data = diddb[diddb$City %in% ny_airbnb_control, ],
                cluster = ~Date)
summary(NYfPRR)

      #listings entire homes
NYsLEH <- feols(Listings_Entire_Homes ~ postxnyc + postxnyc + UR + LF + CPI | post + nyc, 
                data = diddb[diddb$City %in% ny_airbnb_control, ],
                cluster = ~Date)
summary(NYsLEH)

NYfLEH <- feols(Listings_Entire_Homes ~ postxnyc + UR + LF + CPI | City + Date, 
                data = diddb[diddb$City %in% ny_airbnb_control, ],
                cluster = ~Date)
summary(NYfLEH)

      #listings private rooms
NYsLPR <- feols(Listings_Private_Rooms ~ postxnyc + postxnyc + UR + LF + CPI | post + nyc, 
                data = diddb[diddb$City %in% ny_airbnb_control, ],
                cluster = ~Date)
summary(NYsLPR)

NYfLPR <- feols(Listings_Private_Rooms ~ postxnyc + UR + LF + CPI | City + Date, 
                data = diddb[diddb$City %in% ny_airbnb_control, ],
                cluster = ~Date)
summary(NYfLPR)


















      #PLACEBO airbnb

#occupancy
NYsOP <- feols(Occupancy ~ postxnyc + postxnyc + UR + LF + CPI | post + nyc, 
              data = didplacebo[didplacebo$City %in% ny_airbnb_control, ],
              cluster = ~Date)
summary(NYsOP)

NYfOP <- feols(Occupancy ~ postxnyc + UR + LF + CPI | City + Date, 
              data = didplacebo[didplacebo$City %in% ny_airbnb_control, ],
              cluster = ~Date)
summary(NYfOP)

# property revenue
NYsPRP <- feols(Property_Revenue ~ postxnyc + UR + LF + LFC + CPI | post + nyc, 
               data = didplacebo[didplacebo$City %in% ny_airbnb_control, ],
               cluster = ~Date)
summary(NYsPRP)

NYfPRP <- feols(Property_Revenue ~ postxnyc + UR + LF + LFC + CPI | City + Date, 
               data = didplacebo[didplacebo$City %in% ny_airbnb_control, ],
               cluster = ~Date)
summary(NYfPRP)

#entire home revenue
NYsEHRP <- feols(Entire_Home_Revenue ~ postxnyc + postxnyc + UR + LF + CPI | post + nyc, 
                data = didplacebo[didplacebo$City %in% ny_airbnb_control, ],
                cluster = ~Date)
summary(NYsEHRP)

NYfEHRP <- feols(Entire_Home_Revenue ~ postxnyc + UR + LF + CPI | City + Date, 
                data = didplacebo[didplacebo$City %in% ny_airbnb_control, ],
                cluster = ~Date)
summary(NYfEHRP)


#private room revenue
NYsPRRP <- feols(Private_Room_Revenue ~ postxnyc + postxnyc + UR + LF + CPI | post + nyc, 
                data = didplacebo[didplacebo$City %in% ny_airbnb_control, ],
                cluster = ~Date)
summary(NYsPRRP)

NYfPRRP <- feols(Private_Room_Revenue ~ postxnyc + UR + LF + CPI | City + Date, 
                data = didplacebo[didplacebo$City %in% ny_airbnb_control, ],
                cluster = ~Date)
summary(NYfPRRP)

#listings entire homes
NYsLEHP <- feols(Listings_Entire_Homes ~ postxnyc + postxnyc + UR + LF + CPI | post + nyc, 
                data = didplacebo[didplacebo$City %in% ny_airbnb_control, ],
                cluster = ~Date)
summary(NYsLEHP)

NYfLEHP <- feols(Listings_Entire_Homes ~ postxnyc + UR + LF + CPI | City + Date, 
                data = didplacebo[didplacebo$City %in% ny_airbnb_control, ],
                cluster = ~Date)
summary(NYfLEHP)

#listings private rooms
NYsLPRP <- feols(Listings_Private_Rooms ~ postxnyc + postxnyc + UR + LF + CPI | post + nyc, 
                data = didplacebo[didplacebo$City %in% ny_airbnb_control, ],
                cluster = ~Date)
summary(NYsLPRP)

NYfLPRP <- feols(Listings_Private_Rooms ~ postxnyc + UR + LF + CPI | City + Date, 
                data = didplacebo[didplacebo$City %in% ny_airbnb_control, ],
                cluster = ~Date)
summary(NYfLPRP)








#linear model for new york
airroinyc <- read_csv("airroi.csv")
sedb <- diddb %>%
  filter(City == "New York")
airroinyc <- airroinyc %>% mutate(
  Date = as.Date(Date, format = "%m/%d/%y"))
linearNYC <- left_join(sedb,airroinyc, by = c("Date"))

HVmodelNYC <- lm(Home_Value ~ `occupancy` + `Property Revenue` + `Entire Home Revenue` + `Private Room Revenue` + `Listings Entire Homes` + `Listings Private Rooms`, data = linearNYC)
summary(HVmodelNYC)

RVmodelNYC <- lm(Rent_Value ~ `occupancy` + `Property Revenue` + `Entire Home Revenue` + `Private Room Revenue` + `Listings Entire Homes` + `Listings Private Rooms`, data = linearNYC)
summary(RVmodelNYC)

HVmodel <- lm(Home_Value ~ `Occupancy` + `Property_Revenue` + `Entire_Home_Revenue` + `Private_Room_Revenue` + `Listings_Entire_Homes` + `Listings_Private_Rooms`, data = diddb)
summary(HVmodel)

RVmodel <- lm(Rent_Value ~ `Occupancy` + `Property_Revenue` + `Entire_Home_Revenue` + `Private_Room_Revenue` + `Listings_Entire_Homes` + `Listings_Private_Rooms`, data = diddb)
summary(RVmodel)












#FINAL SUMMARY of DiD Model


  #home value effects, values = (Q/M/B/NY) (s/f) (HV/RV) (_/P)
HVmodels <- list("Queens" = QsHV, "Manhattan" = MsHV, "Brooklyn" = BsHV, "New York" = NYsHV,
                 "Queens" = QfHV, "Manhattan" = MfHV, "Brooklyn" = BfHV, "New York" = NYfHV)
modelsummary(HVmodels, stars = TRUE, output = "kableExtra") %>%
  add_header_above(c(" " = 1, "Home Value Basic DiD Model" = 4, "Home Value Two-Way Fixed Effects Model" = 4))

  #rent value effects
RVmodels <- list("Queens" = QsRV, "Manhattan" = MsRV, "Brooklyn" = BsRV, "New York" = NYsRV,
                 "Queens" = QfRV, "Manhattan" = MfRV, "Brooklyn" = BfRV, "New York" = NYfRV)
modelsummary(RVmodels, stars = TRUE, output = "kableExtra") %>%
  add_header_above(c(" " = 1, "Rent Value Basic DiD Model" = 4, "Rent Value Two-Way Fixed Effects Model" = 4))



  #placebo home
HVmodelP <- list("Queens" = QsHVP, "Manhattan" = MsHVP, "Brooklyn" = BsHVP, "New York" = NYsHVP,
                 "Queens" = QfHVP, "Manhattan" = MfHVP, "Brooklyn" = BfHVP, "New York" = NYfHVP)
modelsummary(HVmodelP, stars = TRUE, output = "kableExtra") %>%
  add_header_above(c(" " = 1, "Home Value Placebo Basic DiD Model" = 4, "Home Value Placebo Two-Way Fixed Effects Model" = 4))

  #placebo rent
RVmodelP <- list("Queens" = QsRVP, "Manhattan" = MsRVP, "Brooklyn" = BsRVP, "New York" = NYsRVP,
                 "Queens" = QfRVP, "Manhattan" = MfRVP, "Brooklyn" = BfRVP, "New York" = NYfRVP)
modelsummary(RVmodelP, stars = TRUE, output = "kableExtra") %>%
  add_header_above(c(" " = 1, "Rent Value Placebo Basic DiD Model" = 4, "Rent Value Placebo Two-Way Fixed Effects Model" = 4))




   #airbnb effects values = NY (s/f) (O/PR/EHR/PRR/LEH/LPR) (_/P)
airbnbModels <- list("Occupancy Rate" = NYsO, "Property Revenue" = NYsPR, "Market Revenue Entire-Home" = NYsEHR, "Market Revenue Private-Room" = NYsPRR,
                 "Active Listings Entire-Home " = NYsLEH, "Active Listings Private-Room" = NYsLPR, "Occupancy Percent" = NYfO, "Property Revenue" = NYfPR, "Market Revenue Entire-Home" = NYfEHR, "Market Revenue Private-Room" = NYfPRR,
                 "Active Listings Entire-Home " = NYfLEH, "Active Listings Private-Room" = NYfLPR)
modelsummary(airbnbModels, stars = TRUE, output = "kableExtra") %>%
  add_header_above(c(" " = 1, "Airbnb Basic DiD Model" = 6, "Airbnb Two-Way Fixed Effects DiD Model" = 6))

  #placebo airbnb
airbnbModelsP <- list("Occupancy Rate" = NYsOP, "Property Revenue" = NYsPRP, "Market Revenue Entire-Home" = NYsEHRP, "Market Revenue Private-Room" = NYsPRRP,
                      "Active Listings Entire-Home " = NYsLEHP, "Active Listings Private-Room" = NYsLPRP, "Occupancy Percent" = NYfOP, "Property Revenue" = NYfPRP, "Market Revenue Entire-Home" = NYfEHRP, "Market Revenue Private-Room" = NYfPRRP,
                      "Active Listings Entire-Home " = NYfLEHP, "Active Listings Private-Room" = NYfLPRP)
modelsummary(airbnbModelsP, stars = TRUE, output = "kableExtra") %>%
  add_header_above(c(" " = 1, "Airbnb Placebo Basic DiD Model" = 6, "Airbnb Placebo Two-Way Fixed Effects DiD Model" = 6))


