library(tidyverse)
library(tidyr)
library(dplyr)  
library(ggplot2)
library(plotly)
library(viridis)
library(hrbrthemes)
library(gapminder)
library(htmlwidgets)
library(gridExtra)
library(maps)
library(gganimate)

df_broadband <- broadband_penetration_by_country
df_mobile_subs <- mobile_cellular_subscriptions_per_100_people
df_indv_using_internet <- share_of_individuals_using_the_internet
df_num_internet_users <- number_of_internet_users_by_country
df_continents <- continents2

#Renaming columns
colnames(df_broadband) <- c("Country", "Code", "Year", "Fixed_Broadband_Subs_Per_100_People")
colnames(df_mobile_subs) <- c("Country", "Code", "Year", "Mobile_Cellular_Subs_Per_100_People")
colnames(df_indv_using_internet) <- c("Country", "Code", "Year", "Percent_of_People_Using_Internet")
colnames(df_continents) <- c("Country",	"Alpha-2",	"Alpha-3",	"Country-code",	"ISO_3166-2",	"Region",	"Sub-region", "Intermediate-region",	"Region-code",	"Sub-region-code", "Intermediate-region-code")
colnames(df_num_internet_users) <- c("Country", "Code", "Year", "Number_of_Internet_Users")
df_mobile_subs

#Removing null values in country code and filtering data to specific years
df_mobile_subs_countries <- df_mobile_subs %>% filter(Code !=  "")
df_mobile_subs_countries2009 <- df_mobile_subs_countries %>% filter(Year == 2009)
df_mobile_subs_countries2019 <- df_mobile_subs_countries %>% filter(Year == 2019)

df_mobile_subscription_years <- inner_join(df_mobile_subs_countries2009,df_mobile_subs_countries2019,by=c("Country","Code"))
df_mobile_subscription_combined_change <- mutate(df_mobile_subscription_years, Percentage_Change  = ((Mobile_Cellular_Subs_Per_100_People.y - Mobile_Cellular_Subs_Per_100_People.x)))

#Arranging data from descending order to get top 10 countries
df_mobile_subs_change_ordered <- df_mobile_subscription_combined_change %>% arrange(desc(df_mobile_subscription_combined_change$Percentage_Change))
df_mobile_subs_change_top10 <- df_mobile_subs_change_ordered %>% head(20)


#Renaming the columns
colnames(df_mobile_subs_change_top10)[4] <- "Cellular Subscriptions 2009"
colnames(df_mobile_subs_change_top10)[6] <- "Cellular Subscriptions 2019"

df_mobile_subs_plot <- pivot_longer(df_mobile_subs_change_top10,cols = c("Cellular Subscriptions 2009","Cellular Subscriptions 2019"),names_to = "Mobile_Cellular_Subs_per_100_people", values_to = "Subscriptions_per_100_people")

#Plotting top 10 countries in 2009 and 2019 with cellular subscriptions/100 people
p1 <- ggplot(df_mobile_subs_plot,aes(fill=Mobile_Cellular_Subs_per_100_people,
                                           x=Country,
                                           y=Subscriptions_per_100_people,
                                           group = Mobile_Cellular_Subs_per_100_people))+
  geom_bar(position = "dodge",stat="identity") + 
  ggtitle("Countries with Highest Cellular subscriptions/100 people") + theme(plot.title = element_text(size = 15)) +
  theme(legend.title = element_text(size = 0),legend.key.size = unit(0.1, "cm"),legend.key.height = unit(.5, "cm"),legend.key.width = unit(.5, "cm"),legend.position = c(0.3,.91))+
  theme(axis.text.x=element_text(size=10, angle=90,hjust = 0.95,vjust = 0.2)) 

#Arranging data from ascending order to get bottom 10 countries
df_mobile_subs_change_ordered_asc <- df_mobile_subscription_combined_change %>% arrange(df_mobile_subscription_combined_change$Percentage_Change)
df_mobile_subs_change_bottom10 <- df_mobile_subs_change_ordered_asc %>% head(20)

#Renaming the columns
colnames(df_mobile_subs_change_bottom10)[4] <- "Cellular Subscriptions 2009"
colnames(df_mobile_subs_change_bottom10)[6] <- "Cellular Subscriptions 2019"

df_mobile_subs_plot_bottom10 <- pivot_longer(df_mobile_subs_change_bottom10,cols = c("Cellular Subscriptions 2009","Cellular Subscriptions 2019"),names_to = "Mobile_Cellular_Sub_per_100_people", values_to = "Subscriptions_per_100_people")

#Plotting bottom 10 countries in 2009 and 2019 with cellular subscriptions/100 people
p2 <- ggplot(df_mobile_subs_plot_bottom10,aes(fill=Mobile_Cellular_Sub_per_100_people,
                                           x=Country,
                                           y=Subscriptions_per_100_people,
                                           group = Mobile_Cellular_Sub_per_100_people))+
  geom_bar(position = "dodge",stat="identity") + 
  ggtitle("Countries with Lowest Cellular subscriptions/100 people") + theme(plot.title = element_text(size = 15)) +
  theme(legend.title = element_text(size = 0),legend.key.size = unit(0.1, "cm"),legend.key.height = unit(.5, "cm"),legend.key.width = unit(.5, "cm"),legend.position = c(0.3,.91))+
  theme(axis.text.x=element_text(size=10, angle=90,hjust = 0.95,vjust = 0.2)) 

#Bar graph displays
grid.arrange(p1, p2, ncol = 2)


#Repeating previous process for brandpand penetraion dataset
df_broadband_countries <- df_broadband %>% filter(Code !=  "")
df_broadband_countries_2000 <- df_broadband_countries %>% filter(Year == 2000)
df_broadband_countries_2009 <- df_broadband_countries %>% filter(Year == 2009)
df_broadband_countries_2019 <- df_broadband_countries %>% filter(Year == 2019)

#Joining the tables to get the records for only 2009 and 2019
df_broadband_combined <- inner_join(df_broadband_countries_2009,df_broadband_countries_2019,by=c("Country","Code"))
df_broadband_combined_change <- mutate(df_broadband_combined, Percentage_Change  = (Fixed_Broadband_Subs_Per_100_People.y - Fixed_Broadband_Subs_Per_100_People.x))

#Arranging data in descending order to see which countries improved most
broadband_combined_change_ordered <- df_broadband_combined_change %>% arrange(desc(df_broadband_combined_change$Percentage_Change))

#Top 10 countries to have improved the most
broadband_penetration_top10 <- broadband_combined_change_ordered %>% head(20)

#Renaming the columns
colnames(broadband_penetration_top10)[4] <- "Fixed Broadband Subscription 2009"
colnames(broadband_penetration_top10)[6] <- "Fixed Broadband Subscription 2019"

#Pivoting the 2009 and 2019 data
df_broadband_penetration_for_plot <- pivot_longer(broadband_penetration_top10,cols = c("Fixed Broadband Subscription 2019","Fixed Broadband Subscription 2009"),names_to = "Fixed_Broadband_Subs_Per_100_People", values_to = "Subscriptions_per_100_people")

p3 <- ggplot(df_broadband_penetration_for_plot,aes(fill=Fixed_Broadband_Subs_Per_100_People,
                                     x=Country,
                                     y=Subscriptions_per_100_people,
                                     group = Fixed_Broadband_Subs_Per_100_People))+
  geom_bar(position = "dodge",stat="identity") + 
  ggtitle("Countries with Highest Broadband subscriptions/100 people") + theme(plot.title = element_text(size = 15)) +
  theme(legend.title = element_text(size = 0),legend.key.size = unit(0.1, "cm"),legend.key.height = unit(.5, "cm"),legend.key.width = unit(.5, "cm"),legend.position = c(0.3,.91))+
  theme(axis.text.x=element_text(size=10, angle=90,hjust = 0.95,vjust = 0.2)) 


#Arranging data from ascending order to get bottom 10 countries
broadband_penetration_asc <- df_broadband_combined_change %>% arrange(df_broadband_combined_change$Percentage_Change)
broadband_penetration_bottom10 <- broadband_penetration_asc %>% head(20)
broadband_penetration_bottom10
#Renaming the columns
colnames(broadband_penetration_bottom10)[4] <- "Fixed Broadband Subscription 2009"
colnames(broadband_penetration_bottom10)[6] <- "Fixed Broadband Subscription 2019"

broadband_plot_bottom10 <- pivot_longer(broadband_penetration_bottom10,cols = c("Fixed Broadband Subscription 2019","Fixed Broadband Subscription 2009"),names_to = "Fixed_Broadband_Subs_Per_100_People", values_to = "Subscriptions_per_100_people")

#Plotting bottom 10 countries in 2009 and 2019 with fixed broadband subscriptions/100 people
p4 <- ggplot(broadband_plot_bottom10,aes(fill=Fixed_Broadband_Subs_Per_100_People,
                                                   x=Country,
                                                   y=Subscriptions_per_100_people,
                                                   group = Fixed_Broadband_Subs_Per_100_People))+
  geom_bar(position = "dodge",stat="identity") + 
  ggtitle("Countries with Lowest Broadband subscriptions/100 people") + theme(plot.title = element_text(size = 15)) +
  theme(legend.title = element_text(size = 0),legend.key.size = unit(0.1, "cm"),legend.key.height = unit(.5, "cm"),legend.key.width = unit(.5, "cm"),legend.position = c(0.5,.91))+
  theme(axis.text.x=element_text(size=10, angle=90,hjust = 0.95,vjust = 0.2)) 

#Bar graph displays
grid.arrange(p3, p4, ncol = 2)             

world_map <- map_data("world")

#Merging the world map with the broadband penetration by country 
world_map = merge(world_map,df_broadband_countries_2009,by.x="region",by.y = "Country")

# Plotting the heatmap of the world for the year 2009
p5 <- ggplot(world_map, aes(map_id = region, fill = Fixed_Broadband_Subs_Per_100_People)) + 
    geom_map(map = world_map, size = 0.1) + 
    scale_fill_gradient(low = "#FFFFFF", high = "#cc4c02", name = "Fixed Broadband Subscriptions") + 
    expand_limits(x = world_map$long, y = world_map$lat) + 
    ggtitle("Broadband Subscriptions/100 People in 2009") + 
    theme(legend.title = element_text(size = 0),legend.key.size = unit(0.1, "cm"),legend.key.height = unit(.7, "cm"),legend.key.width = unit(.5, "cm"),legend.position = c(0.1,.3))

world_map1 <- map_data("world")
world_map1 = merge(world_map1,df_broadband_countries_2019,by.x="region",by.y = "Country")

# Plotting the heatmap of the world for the year 2019
p6 <- ggplot(world_map1, aes(map_id = region, fill = Fixed_Broadband_Subs_Per_100_People)) + 
  geom_map(map = world_map1, size = 0.1) + 
  scale_fill_gradient(low = "#FFFFFF", high = "#cc4c02", name = "Fixed Broadband Subscriptions") + 
  expand_limits(x = world_map$long, y = world_map$lat) + 
  ggtitle("Broadband Subscriptions/100 People in 2019") + 
  theme(legend.title = element_text(size = 0),legend.key.size = unit(0.1, "cm"),legend.key.height = unit(.7, "cm"),legend.key.width = unit(.5, "cm"),legend.position = c(0.1,.3))

#World map displays
grid.arrange(p5, p6, ncol = 1)  

#Reapeating the previous process with individuals using the internet
df_indv_using_internet <- df_indv_using_internet %>% filter(Code !=  "")
indv_using_internet2009 <- df_indv_using_internet %>% filter(Year == 2009)
indv_using_internet2019 <- df_indv_using_internet %>% filter(Year == 2019)

world_map2 <- map_data("world")
world_map2 = merge(world_map2,indv_using_internet2009,by.x="region",by.y = "Country")

# Plotting the heatmap of the world for the year 2009
p7 <- ggplot(world_map2, aes(map_id = region, fill = Percent_of_People_Using_Internet)) + 
  geom_map(map = world_map2, size = 0.1) + 
  scale_fill_gradient(low = "#FFFFFF", high = "#cc4c02", name = "Percent of Internet Users") + 
  expand_limits(x = world_map$long, y = world_map$lat) + 
  ggtitle("Percent of Internet users in 2009") + 
  theme(legend.title = element_text(size = 0),legend.key.size = unit(0.1, "cm"),legend.key.height = unit(.7, "cm"),legend.key.width = unit(.5, "cm"),legend.position = c(0.1,.3))

world_map3 <- map_data("world")
world_map3 = merge(world_map3,indv_using_internet2019,by.x="region",by.y = "Country")

# Plotting the heatmap of the world for the year 2019
p8 <- ggplot(world_map3, aes(map_id = region, fill = Percent_of_People_Using_Internet)) + 
  geom_map(map = world_map3, size = 0.1) + 
  scale_fill_gradient(low = "#FFFFFF", high = "#cc4c02", name = "Percent of Internet Users") + 
  expand_limits(x = world_map$long, y = world_map$lat) + 
  ggtitle("Percent of Internet users in 2019") + 
  theme(legend.title = element_text(size = 0),legend.key.size = unit(0.1, "cm"),legend.key.height = unit(.7, "cm"),legend.key.width = unit(.5, "cm"),legend.position = c(0.1,.3))

grid.arrange(p5, p6,p7,p8, ncol = 2)

#Finding the sum of users using the internet throughout the years
users_per_year <- aggregate(Percent_of_People_Using_Internet ~ Year, df_indv_using_internet, sum)

p <- ggplot(users_per_year, aes(fill=Percent_of_People_Using_Internet,x = Year, y = Percent_of_People_Using_Internet, group=Percent_of_People_Using_Internet)) +
  geom_bar(position = "dodge",stat="identity")+
  ggtitle("Percent of Internet Users from 1990-2019") + theme(plot.title = element_text(size = 15)) +
  theme(legend.title = element_text(size = 0),legend.key.size = unit(0.1, "cm"),legend.key.height = unit(.5, "cm"),legend.key.width = unit(.5, "cm"))+
  theme(axis.text.x=element_text(size=10, angle=90,hjust = 0.95,vjust = 0.2)) 
p

# Creating an animation dsiplay
p_animated <- p +
  transition_reveal(Year)

# Save animated graph
animate(p_animated, nframes = 40, fps = 5, width = 600, height = 400, renderer = gifski_renderer("progressive_line_chart.gif"))

df_indv_using_internet=na.omit(df_indv_using_internet)

#Filtering internet user data by year 2009
users_top10 <- df_indv_using_internet %>% filter(Year == 2009)
users_top10 <- users_top10 %>% head(10)

ggplot(users_top10, aes(x = "", y = Percent_of_People_Using_Internet, fill = Country)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_discrete(name = "Country") +
  ggtitle("Percent of People Using the Internet by Country")

# Creating 3D scatter plot
fig <- plot_ly(x = indv_using_internet2009$Year, y = indv_using_internet2009$Percent_of_People_Using_Internet, z = indv_using_internet2009$Country, type = "scatter3d", mode = "markers", marker = list(size = 5))

# Customize the plot
fig <- fig %>% layout(scene = list(xaxis = list(title = "X-axis"), yaxis = list(title = "Y-axis")))
fig

indv_using_internet_continents <- inner_join(indv_using_internet2009,df_continents,by="Country")

#Grouping the dataset by region
indv_using_internet_continents_grouped <- indv_using_internet_continents %>% group_by(Region) %>% summarise(Mean_internet_users = mean(Percent_of_People_Using_Internet))
head(indv_using_internet_continents_grouped)

#Plotting the Pie-Chart for broadband penetration
t1 <- ggplot(indv_using_internet_continents_grouped,aes(x="",y=Mean_internet_users,fill=Region)) + 
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  theme_void() + 
  ggtitle("Mean Internet Users in different Regions of the World in 2009") + 
  theme(plot.title = element_text(hjust = 0.3,size = 15,face = "bold")) +
  guides(fill  = guide_legend(reverse = TRUE))+
  scale_fill_brewer(name = "Regions") + 
  theme(legend.text = element_text(size=10),legend.title = element_text(hjust = 0.5,size = 10,face = "bold")) + 
  geom_text(aes(label = paste0(round(indv_using_internet_continents_grouped$Mean_internet_users,digits = 2))),position = position_stack(vjust = 0.5),size = 3,face="bold") + 
  theme(legend.box.background = element_rect(color="black", size=1),legend.box.margin = margin(25, 6, 6, 6))

indv_using_internet_continents2019 <- inner_join(indv_using_internet2019,df_continents,by="Country")

#repeating previous process for individuals usign the internet
indv_using_internet_continents_group2019 <- indv_using_internet_continents2019 %>% group_by(Region) %>% summarise(Mean_internet_users = mean(Percent_of_People_Using_Internet))
head(indv_using_internet_continents_group2019)

#Plotting the Pie-Chart for internet users
t2 <- ggplot(indv_using_internet_continents_group2019,aes(x="",y=Mean_internet_users,fill=Region)) + 
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  theme_void() + 
  ggtitle("Mean Internet Users in different Regions of the World in 2019") + 
  theme(plot.title = element_text(hjust = 0.3,size = 15,face = "bold")) +
  guides(fill  = guide_legend(reverse = TRUE))+
  scale_fill_brewer(name = "Regions") + 
  theme(legend.text = element_text(size=10),legend.title = element_text(hjust = 0.5,size = 10,face = "bold")) + 
  geom_text(aes(label = paste0(round(indv_using_internet_continents_group2019$Mean_internet_users,digits = 2))),position = position_stack(vjust = 0.5),size = 3,face="bold") + 
  theme(legend.box.background = element_rect(color="black", size=1),legend.box.margin = margin(25, 6, 6, 6))

grid.arrange(t1, t2, ncol = 1) 

data2 <- combined_subs %>% filter(Year=="2000") %>% dplyr::select(-Year)
p2 <- data2 %>% arrange(desc(Mobile_Cellular_Subs_Per_100_People)) %>%
mutate(country = factor(Country, Country)) %>%

# Text for tooltip
mutate(text = paste("Country: ", Country, "\nCellular Subscriptions: ", Mobile_Cellular_Subs_Per_100_People, "\nFixed Broadband Subscriptions: ", Fixed_Broadband_Subs_Per_100_People, sep="")) %>%

ggplot( aes(x=Fixed_Broadband_Subs_Per_100_People, y=Mobile_Cellular_Subs_Per_100_People, size = Mobile_Cellular_Subs_Per_100_People, color = Country, text=text)) +
geom_point(alpha=0.7) +
scale_size(range = c(1.4, 19), name="Cellular Subscriptions") +
scale_color_viridis(discrete=TRUE, guide=FALSE) +
ggtitle("Mobile Cellular vs Fixed Broadband Subscriptions \n per 100 People in 2000")+
theme_ipsum() +
theme(legend.position="none")

# turning ggplot interactive
pp2 <- ggplotly(p2, tooltip="text")
pp2
saveWidget(pp2, file=paste0( getwd(),"/downloads/bubble2000.html"))

#repeating process with 2009
combined_subs = inner_join(df_broadband,df_mobile_subs, by=c("Country","Code","Year"))
data <- combined_subs %>% filter(Year=="2009") %>% dplyr::select(-Year)


p <- data %>% arrange(desc(Mobile_Cellular_Subs_Per_100_People)) %>%
mutate(country = factor(Country, Country)) %>%

mutate(text = paste("Country: ", Country, "\nCellular Subscriptions: ", Mobile_Cellular_Subs_Per_100_People, "\nFixed Broadband Subscriptions: ", Fixed_Broadband_Subs_Per_100_People, sep="")) %>%

ggplot( aes(x=Fixed_Broadband_Subs_Per_100_People, y=Mobile_Cellular_Subs_Per_100_People, size = Mobile_Cellular_Subs_Per_100_People, color = Country, text=text)) +
geom_point(alpha=0.7) +
scale_size(range = c(1.4, 19), name="Cellular Subscriptions") +
scale_color_viridis(discrete=TRUE, guide=FALSE) +
ggtitle("Mobile Cellular vs Fixed Broadband Subscriptions \n per 100 People in 2009")+
theme_ipsum() +
theme(legend.position="none")

pp <- ggplotly(p, tooltip="text")
pp

saveWidget(pp, file=paste0( getwd(),"/downloads/bubble2009.html"))

#repeating process with 20190
data1 <- combined_subs %>% filter(Year=="2019") %>% dplyr::select(-Year)
p1 <- data1 %>% arrange(desc(Mobile_Cellular_Subs_Per_100_People)) %>%
mutate(country = factor(Country, Country)) %>%

mutate(text = paste("Country: ", Country, "\nCellular Subscriptions: ", Mobile_Cellular_Subs_Per_100_People, "\nFixed Broadband Subscriptions: ", Fixed_Broadband_Subs_Per_100_People, sep="")) %>%

ggplot( aes(x=Fixed_Broadband_Subs_Per_100_People, y=Mobile_Cellular_Subs_Per_100_People, size = Mobile_Cellular_Subs_Per_100_People, color = Country, text=text)) +
geom_point(alpha=0.7) +
scale_size(range = c(1.4, 19), name="Cellular Subscriptions") +
scale_color_viridis(discrete=TRUE, guide=FALSE) +
ggtitle("Mobile Cellular vs Fixed Broadband Subscriptions \n per 100 People in 2019")+
theme_ipsum() +
theme(legend.position="none")

pp1 <- ggplotly(p1, tooltip="text")
pp1
saveWidget(pp1, file=paste0( getwd(),"/downloads/bubble2019.html"))

