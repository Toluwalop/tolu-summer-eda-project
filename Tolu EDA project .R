# Purpose: To show how drug prices vary bystate 
library(tidyverse)
library(usmap)
library(ggplot2)
library(dplyr)
library(plotly)
library(maps)

# Reading my data
presc_claims <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/health/eda_projects/medicare_partd_presc_claims.csv")

# How the price of Medical oncology drugs Varies By state
presc_new <- presc_claims %>%
  select(State, OpioidFlag, TotalDrugCost, BrandName)%>%
  aggregate(TotalDrugCost ~ OpioidFlag + State  , mean) %>%
  filter(State %in% c("CA", "FL", "GA", "KS", "KY", "MA", "MD", "MO", "NC",  "NH", "NJ", "NV", "NY", "OK", "OR", "SC", "UT", "VA", "VT", "WV") )%>%
  filter(TotalDrugCost > 2000)
presc_new 

#Spine bar chart showing the cost by state with individual opioid and non-opioid proportions
presc_new %>%
  ggplot(aes(x = reorder (State, -TotalDrugCost),
             y = TotalDrugCost,
             fill = OpioidFlag)) + #<<
  geom_col() + theme_bw()+
  ggthemes::scale_fill_colorblind()


# Aggregate: showing the various medical oncology drugs and how they vary by state
NewTabl <- presc_new %>%
  select(State, TotalDrugCost) %>%
  aggregate(TotalDrugCost ~ State, mean)
  #filter(TotalDrugCost > 1000) %>%
  #filter(State %in% c("AZ","ME", "MI", "MO", "NC", "ND", "NH", "NJ", "NV", "NY", "OK", "OR", "PA", "SC", "SD", "TN", "UT", "VA", "VT", "WV") )
NewTabl

# Raw bar chart
NewTabl %>%
  ggplot (aes (x=reorder (State, -TotalDrugCost) ,y= TotalDrugCost))+
  geom_bar(stat ="identity")+
  theme_bw()

# Graphic showing drug cost variation by state on US map

colnames(NewTabl)[1] <- 'state'
plot_usmap(data = NewTabl, values = "TotalDrugCost", color = "blue", include = c("CA", "FL", "GA", "KS", "KY", "MA", "MD", "MO", "NC",  "NH", "NJ", "NV", "NY", "OK", "OR", "SC", "UT", "VA", "VT", "WV") ) + 
  scale_fill_continuous(low = "white", high = "blue", name = "Drug Cost Variation", label = scales::comma) + 
  labs(title = "USA Region", subtitle = "Drug Cost Variation by State") +
  theme(legend.position = "right")
#include =unique(NewTabl$State)









  










