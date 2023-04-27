#------------------------------------------------------------------------
#' ---
#' title: "UNICEF Data Visualisation"
#' subtitle: "Visualisation in R language"
#' #### date: "27 April 2023"
#' ##### author: “Shivani Kumar”
#' output: "html_document"
#' 
#' ---
#' # Story
#'
#' The dashboad displays 4 graphs that depicts the relationship between GDP, Population, Observation Value over the given time frame. 
#' From the data provided, it is seen that GDP has a direct relationship on observation value. This signifies with a higher level of GDP, women are able to make better informed decisions with respect to sexual relations, contraceptive use, and reproductive health care due to better availbility of health resources. 
#' Based on the graph representing the country time stamp and its related data with GDP and Population, it is seen that with a decreasein GDP there is also a significant decrease in observation value. With lower per capita income affecting the economy, women are resorted to making choices that are in line with the existing and limited resources.
#' Another interesting factor to notice from the graph represting observation rate  from top 5 shows that not all of these countries fall under the tier of Developed countries. Despite not being a fully developed country with advanced resources available to aide women healthcare in different aspects, women from these countries are able to make an informed decision for themselves with the existing resources available which is derived from the economic position of the country.
#'
#'
#'  **This report consistds of four charts which are mentioned below.**
#'  
#'  * Scatter plot with regression line
#'  * World Map 
#'  * Bar chart
#'  * Time series 
#' 
#-----------------------------------------------
#Required Packages

# install.packages("stringr", "ggplot2","maps", "gridExtra","plotly","scales",
#                  "RColorBrewer","ggiraph","tidyr","dplyr","Hmisc","forcats","ggthemes","ggrepel", "mapproj","rmarkdown")

#--------------------------------------------
#Importing Libraries

library(ggplot2)
library(maps)
library(gridExtra)
library(plotly)
library(scales) 
library(stringr) 
library(Hmisc) 
library(forcats) 
library(ggthemes) 
library(RColorBrewer)
library(ggiraph)
library(tidyr)
library(dplyr)
library(ggrepel)



#-----------------------------------------------------------------------
#Importing Files
indict_1_grpd <- read.csv("/cloud/project/unichef_data_indicator_1_grpd.csv", header = TRUE)
View(indict_1_grpd)

world_data <- ggplot2::map_data('world')
world_data <- ggplot2::fortify(world_data)
View(world_data)

merged_map <- merge(x = indict_1_grpd, y = world_data, by.x = "country", by.y = "region")
View(merged_map)

merged_map_1 <- merged_map %>% distinct(merged_map$country, .keep_all = TRUE)
View(merged_map_1)

merged_map_1$pop_mil <- merged_map_1$Population..total / 1000000
merged_map_1$gdp_in_10k <- merged_map_1$GDP.per.capita..constant.2015.US.. / 10000
merged_map_1$obs_val_in_10k <- merged_map_1$obs_true_val / 10000
#--------------------------------------------------------------------
#World Map

WorldData <- map_data('world') %>% fortify
View(WorldData)

world_df <- select(indict_1_grpd, region = country, "obs_value" = obs_true_val)
View(world_df)

world_df_1 <- left_join(world_df, WorldData, by = "region")
View(world_df_1)

plt_1 <- ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5) + 
  geom_map(data = world_df, map=WorldData, 
           aes(fill=obs_value,map_id=region),
           colour="#7f7f7f", size=0.5) + geom_text_repel() +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="Obs Value", title="World Map", x="", y="") + 
  theme_bw()

plt_1 

#--------------------------------------------------------------------------------
# Time Series
time_s <- select(merged_map_1, time_period, pop_mil, obs_true_val, GDP.per.capita..constant.2015.US..)
View(time_s)

time_s_grp <- time_s %>% group_by(time_period) %>% 
  summarise(avg_pop_mil= mean(pop_mil),
            avg_obs_true_val = mean(obs_true_val),
            avg_gdp = mean(GDP.per.capita..constant.2015.US..),
            .groups = 'drop')
View(time_s_grp)

fig_pop<- ggplot(data = time_s_grp, aes(x = time_period, y = avg_pop_mil))+ geom_line(color = "#00AFBB", size = 2) +
  theme(panel.background = element_rect(fill='#e5ecf6'))

fig_obs<- ggplot(data = time_s_grp, aes(x = time_period, y = avg_obs_true_val))+ geom_line(color = "#00AFBB", size = 2)+
  theme(panel.background = element_rect(fill='#e5ecf6'))

fig_gdp<- ggplot(data = time_s_grp, aes(x = time_period, y = avg_gdp))+ geom_line(color = "#00AFBB", size = 2)+
  theme(panel.background = element_rect(fill='#e5ecf6'))

grid.arrange(fig_pop,fig_obs,fig_gdp, ncol = 1)

#-------------------------------------------------------------------------------------------
# Bar plot

fig <- plot_ly(data = merged_map_1, x = merged_map_1$country , y = merged_map_1$obs_true_val, 
               type = "bar", color = "orange") %>% 
  layout(xaxis = list(categoryorder = "total descending")) %>%
  layout(title = 'Countries by Obs Value', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Country'), 
         yaxis = list(title = 'Obs Value'))

fig

#----------------------------------------------------------------------
# Scatter Plot

fit <- lm(obs_true_val ~ gdp_in_10k, data = merged_map_1)

merged_map_1 %>% 
  plot_ly(x = ~gdp_in_10k) %>% 
  add_markers(y = ~obs_true_val) %>% 
  add_lines(x = ~gdp_in_10k, y = fitted(fit))%>%
  layout(showlegend = F)%>%
  layout(title = 'Scatter plot of GDP Per Capita and Observation Values', plot_bgcolor = "#e5ecf6")
