rm(list = ls())

library("dplyr")
library("stringr")
library("tigris")
library("sp")
library("ggplot2")
library("rgdal")
library("maptools")
library(viridis)
gpclibPermit()
setwd("/Users/brinaseidel/Documents/School/2018 Fall Semester/Introduction to Data Science/Personal Response_to_Resistance/")

# Read in the data
crimes <- read.csv("Input Data/clean_data.csv")

# Calculate total crimes and resistance by census tract
counts <- crimes %>% 
  group_by(tract) %>%
  summarize(resistance_count=sum(Target1), crimes=n(), income=mean(B19113_001E))
counts$TRACTCE <- str_pad(counts$tract, 6, pad = "0")

# Download census tracts
austin <- tracts(state = 'TX', county = c(453, 491))

# Merge with the data
to_map <- fortify(austin, region = "TRACTCE")
merged <- left_join(to_map, counts, by= c("id"="TRACTCE"))
merged <- merged[!is.na(merged$crimes), ]
merged$resistance_count <- merged$resistance_count+1 # So that we can use a log scale without having zeroes

# Map total crimes
ggplot(merged) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=crimes), size=0.2, color="white")  + 
  scale_fill_gradientn(colours = viridis(6),
                       breaks = c(1, 10, 100, 1000, 10000),
                       trans = "log10") +
  ggtitle("Total Crimes, 2009-2016") +
  theme_void() + theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5))

# Map resistence
ggplot(merged) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=resistance_count), size=0.2, color="white")  + 
  scale_fill_gradientn(colours = viridis(6),
                       breaks = c(1, 10, 100, 1000, 10000),
                       trans = "log10") +
  ggtitle("Instances of Resistance, 2009-2016") +
  theme_void() +  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5))

# Map income
ggplot(merged) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=income), size=0.2, color="white")  + 
  scale_fill_gradientn(colours = rev(terrain.colors(6))) +
  ggtitle("Median Household Income ($)") +
  theme_void() +  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5))
