##Loading the required packages:

library(tidyverse)
library(DataExplorer)
library(caret)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggpubr)
library(skimr) #SKIM function - For statistical summary
library(EnvStats) #Roseners test for outliers
library(gclus) #Scatterplot Matrices from the glus Package 
library(scatterplot3d)  #Visualization
library(plotly) #world map
library(readxl) #Read UN dataset
library(superml)
library(RColorBrewer) #Bar chart
library(base64)
suppressWarnings
suppressMessages
options(error = expression(NULL))

# Data retrieval
## Data source Identification:

#Link: https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv
#Link: http://hdr.undp.org/en/data
# OWID data set is extracted from the database and saved as data_Covid. Similarly, UN data set is extracted and saved as un_edu_data

#Extracting the OWID dataset:
if (!dir.exists("data")) { dir.create("data") } 
if (!file.exists("data/owid-covid-data.csv")) 
  download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv",destfile = "data/owid-covid-data.csv")
data_Covid <- read_csv("data/owid-covid-data.csv", 
                       col_types = cols(
                         .default = col_double(),  
                         iso_code = col_character(),  
                         continent = col_character(),
                         location = col_character(),  
                         date = col_date(format = ""), 
                         tests_units = col_character()
                       )) 
#Extracting UN dataset for education index:
if (!file.exists("data/un_edudata.xlsx")) 
  download.file("http://hdr.undp.org/sites/default/files/2020_statistical_annex_all.xlsx",destfile ="data/un_edudata.xlsx",
                mode = "wb")
un_edu_data <- read_xlsx("data/un_edudata.xlsx", sheet = 2, range = "A5:O200")

# Data processing

## Data Transformation
### Data translation

#Selecting only required variables in data set with date filtered as maximum date which takes the updated date.
data_owid <- data_Covid %>% 
          select(continent, location, date, population,  median_age, aged_65_older, aged_70_older, gdp_per_capita, 
          extreme_poverty,cardiovasc_death_rate, diabetes_prevalence, female_smokers, male_smokers,
          handwashing_facilities, hospital_beds_per_thousand, life_expectancy, human_development_index, 
          total_deaths_per_million,total_deaths,iso_code) %>% 
          group_by(location) %>% filter(date == max(date)) %>% # Take the most recent date per country
          group_by() #removing groupby

#Insight : Only the required predictor variables with latest date is selected which will help us to achieve our goal of find the linear dependency between HDI and total COVID deaths. Since the raw data is already in csv format, there is no data transformation performed here.

#Checking Number of rows and columns in the dataset:
dim(data_owid)

#Insight : OWID dataset has 237 observations with 20 variables. UN dataset has 195 observations with 15 variables.

#Summary statistics
summary(data_owid)

#Insight : Summary provides details about the statistical analysis of each of the features. It provides Minimum value, 1st Quantile, Median, Mean, 3rd Quantile, maximum values and the number of NA values to validate our data set. With respect to our response variable, I can observe that there is huge difference between the minimum and maximum value of the variables as 3.101 and 6006.490 respectively. This shows how widely our data is spread and how extreme our observations are.

#Removing missing values in y variable - total_deaths
data_owid <- data_owid %>% select(continent: iso_code)%>% 
          filter(!is.na(total_deaths_per_million))

#Insight : I select total_deaths_per_million as our target variable. While checking for the NA values, it shows that there are 39 values which are NA. It is advisable to remove the NA values completely rather then performing analysis with the missing values in the dependent variable which might result in biased output.

#Clean UN education dataset and calculate index variable:
#Rename education variables
names(un_edu_data)[2] <- "location"
names(un_edu_data)[7] <- "expected_yr_Schooling"
names(un_edu_data)[9] <- "mean_yr_Schooling"
#Filter and select only the relevant education variables
un_edu_data <- un_edu_data %>%
  select(location,expected_yr_Schooling,mean_yr_Schooling) 
un_edu_data <- un_edu_data %>%
  filter(!is.na(location)) %>% 
  filter(location !="Country") %>% 
  filter(!is.na(expected_yr_Schooling))

#Computing Education index from UN data set:
#Change col type as numeric for finding average
un_edu_data$expected_yr_Schooling <- as.numeric(as.character(un_edu_data$expected_yr_Schooling))  
un_edu_data$mean_yr_Schooling <- as.numeric(as.character(un_edu_data$mean_yr_Schooling))  
sapply(un_edu_data, typeof)
#Calculate Education index
un_edu_data <- un_edu_data %>%
  mutate(education_index = (expected_yr_Schooling + mean_yr_Schooling)/2) %>%
  select(location, education_index)
apply(is.na(un_edu_data),2, sum)

#Insight : Education index is calculated by summing up the expected year schooling and mean year schooling and diving it by 2. These are done in the UN data set as I will be using this variable further in our project as it is the component of HDI.

## Data Pre-processing
### Checking Outliers

#Visualizing outliers with box plot
ggplot(data_owid) +
  aes(x = "", y = total_deaths_per_million) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#Insight : The box plot displays how the variable - total_deaths_per_million is dispersed with five components,
#1. Minimum
#2. First quartile (Q1)
#3. Median
#4. Third quartile (Q3)
#5. Maximum
# From the box plot, I can observe that there is an outlier toward the range of 6000. The minimum observation and the maximum observation in
# the total death per million lies between 3.101 to 6006.490, with median towards the 1st quartile. This shows that the data points are right
# skewed. This means a wider range of observations of total deaths per million are more spread between the median and the 3rd quartile.
# Moreover, the data are condensed towards the 1st quartile and the median.

#Statistical test for detecting outliers:
#There are three formal techniques which is used to detect outliers.

#Grubbs's test: This test identifies if the lower observation or the highest observation is an outlier in a dataset
#Dixon's test: This is similar to Grubb's test. Once the outlier is identified, this test will be performed on the identified outlier.
# Rosner's test: This test can be performed to identify multiple outlier at the same time, which Grubbs's test and Dixon's test fails to perform. This test also has an advantage to avoiding masking, where an outlier which lies close to another outlier can go unidentified.

#Rosner’s test - total_deaths_per_million
data_owid_death_out <- rosnerTest(data_owid$total_deaths_per_million,
                                  k = 3)
data_owid_death_out
data_owid_death_out$all.stats

#Insight : The result of Rosner's test states that there is one outlier identified in lines 142 which has a value of 6006.490 with a mean of 901.2832 and standard deviation of 970.2075.

data_owid_total_deaths_mill <- data_owid[order(-data_owid$total_deaths_per_million),] 
head(data_owid_total_deaths_mill,2)

#Insight : The above tables states that Peru has the highest number of death which is 6006.490 and Tanzania has lowest death of 3.101

#Removing outliers from total_deaths_per_million:
data_owid <- data_owid %>% filter(total_deaths_per_million < 4000)

#Visualize the log transform of the total death variable:
ggplot(data_owid) +
  aes(x = total_deaths_per_million) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal() +
  xlab("Total Number of Deaths per million")+
  ggtitle('Histogram of Logarithm of Total Number of Deaths per million')

#Insight : The above histogram illustrates that the outlier is removed.

### Handling missing values
#Missing values:** Missing values are observation which has no data. Processing it without taking care of it might lead to poor performance
#of the statistical test and machine learning models. It might lead to more biased variable leading to inaccurate results.

# Missing values are of 3 types.
#1.Missing completely at random (MCAR) : There is no relationship between the observed and unobserved values which are missing.
#2.Missing at random : There is a relationship between the observed and unobserved values as there exist no pattern between them.
#3.Missing not at random : The missing data is related to the unobserved data. 4. Structurally missing data: There is a logical reason behind the non-existance of data.

#Approaches to handle missing data:
#1.Deletion methods to remove missing values.
#2.Regression analysis to systematically eliminate data.
#3.Data imputation techniques.

#Check NA values in the dataset:
apply(is.na(data_owid),2, sum)
#Checking the percentage of the missing values:
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(data_owid, 2, p)

#Insight : There is a general assumption that 25 to 30% of missing values are allowed. it is advisable to discard it if the variable is insignificant.

#Treating NA values in continent variable:
data_owid <- data_owid %>% select(continent: iso_code) %>% 
  group_by(location) %>% filter(date == max(date)) %>% # Take the most recent date per country
  group_by() #%>% # Remove the grouping
  data_owid <- data_owid %>% 
  filter(!is.na(continent))
  
#Insight : Comparing the NA values of continent with the location, it can be observed that the location has insignificant values named as world, high income, low income. This shows that missing values in the continent variable are Structurally missing data. Hence these values can be dropped for further analysis.
  
#Treating NA values in HDI variable:
data_owid_hdi <- data_owid %>% select(location, human_development_index)%>% 
    filter(is.na(human_development_index))
data_owid <- data_owid %>% 
    filter(!is.na(human_development_index))
  
#Insight : There are 5 countries which has missing values namely Kosovo, Monaco, San Marino, Somalia and Taiwan. Since it was only 5 countries, measures was made to fill the missing value from the UN dataset. However, it was
#noticed that HDI value is missing for these 5 countries in the UN dataset itself. Hence dropping the NA values and proceeding with the further analysis.
  
#Treating NA values for median_age, aged_65_older and aged_70_older variables:
#Imputing median_age, aged_65_older and aged_70_older with mean
mean_imputer <- function(x) 
{x = round(replace(x, is.na(x) , mean(x, na.rm = TRUE)), digits=0)}
data_owid <- data_owid %>% mutate_at(5:7, mean_imputer)

#Treating NA values for handwashing_facilities variable:
#Removing handwashing_facilities since it has more than 50% of the data missing
data_owid <- data_owid %>% 
  select(-handwashing_facilities)
  head(data_owid,2)

#Treating NA values for female_smokers and male_smokers with median:
median_imputer <- function(x) 
{x = round(replace(x, is.na(x) , median(x, na.rm = TRUE)), digits=0)}
data_owid <- data_owid %>% mutate_at(12:13, median_imputer)

#Treating NA values for extreme_poverty :
data_owid_extPov <- data_owid %>% 
  select(location, extreme_poverty) %>% 
  filter(is.na(extreme_poverty))
data_owid <- data_owid %>% 
  select(-extreme_poverty)

#Insight : Extreme poverty is an vital element which contribute to the GDP of a country which shows an adverse effect on HDI. The missing dimensions
#were approached to be filled with the data available in the world bank dataset. While analyzing the missing values in the data set, it was
#observed that values are missing for both countries with high HDI and low HDI. This scenario can be considered as Missing Missing completely
#at random (MCAR). Missing values were approached to be filled from the world bank dataset, but it was found that most of the countries does not
#have extreme poverty value recorded in the world bank itself. Here I decided to drop the variable because treating the missing values with
#imputation method might cause more bias which is not a wise approach.

#Treating NA values for hospital_beds_per_thousand by imputing median:
data_owid_bed <- data_owid %>% select(location, hospital_beds_per_thousand)%>% 
  filter(is.na(hospital_beds_per_thousand))
data_owid <- data_owid %>% mutate_at(13, median_imputer)

#Treating NA values for gdp_per_capita by imputing mean:
data_owid_gdp <- data_owid %>% select(location, gdp_per_capita,human_development_index) %>% 
  filter(is.na(gdp_per_capita))
data_owid <- data_owid %>% mutate_at(8, mean_imputer)

#Treating missing values for cardiovasc_death_rate anddiabetes_prevalence by imputing median:
data_owid_cardio <- data_owid %>% select(location, cardiovasc_death_rate) %>% 
  filter(is.na(cardiovasc_death_rate))
data_owid_diabetes <- data_owid %>% select(location, diabetes_prevalence) %>% 
  filter(is.na(diabetes_prevalence))
data_owid <- data_owid %>% mutate_at(9:10, mean_imputer)

### Joining dataset - UN and OWID

#Replacing the country name of UN dataset with OWID for joining both the datasets 
un_edu_data$location[un_edu_data$location == "Viet Nam"] <- "Vietnam"
un_edu_data$location[un_edu_data$location == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
un_edu_data$location[un_edu_data$location == "Timor-Leste"] <- "Timor"
un_edu_data$location[un_edu_data$location == "Tanzania (United Republic of)"] <- "Tanzania"
un_edu_data$location[un_edu_data$location == "Syrian Arab Republic"] <- "Syria"
un_edu_data$location[un_edu_data$location == "Syrian Arab Republic"] <- "Syria"
un_edu_data$location[un_edu_data$location == "Korea (Republic of)"] <- "South Korea"
un_edu_data$location[un_edu_data$location == "Russian Federation"] <- "Russia"
un_edu_data$location[un_edu_data$location == "Palestine, State of"] <- "Palestine"
un_edu_data$location[un_edu_data$location == "Moldova (Republic of)"] <- "Moldova"
un_edu_data$location[un_edu_data$location == "Lao People's Democratic Republic"] <- "Laos"
un_edu_data$location[un_edu_data$location == "Iran (Islamic Republic of)"] <- "Iran"
un_edu_data$location[un_edu_data$location == "Hong Kong, China (SAR)"] <- "Hong Kong"
un_edu_data$location[un_edu_data$location == "Eswatini (Kingdom of)"] <- "Eswatini"
un_edu_data$location[un_edu_data$location == "Congo (Democratic Republic of the)"] <- "Democratic Republic of Congo"
un_edu_data$location[un_edu_data$location == "Côte d'Ivoire"] <- "Cote d'Ivoire"
un_edu_data$location[un_edu_data$location == "Cabo Verde"] <- "Cape Verde"
un_edu_data$location[un_edu_data$location == "Brunei Darussalam"] <- "Brunei"
un_edu_data$location[un_edu_data$location == "Bolivia (Plurinational State of)"] <- "Bolivia"

#Insight : Approach was taken to join the dataset by considering the ISO code.However, there is no ISO code given in the UN data set to map with OWID
#data set. Hence, I consider country names, but there were country names mismatch in the UN and OWID dataset. In order, to rectify it I
#replace the country names of the UN dataset to match with the OWID dataset so I can join it to extract the education index for our
#analysis. There are 18 entries with country names mismatch. Hence, I map them to correct country name.

#Joining UN and OWID datasets for extracting education index:
data_owid <- left_join(data_owid, un_edu_data, by = c('location'))
head(data_owid,2)

#Insight : Education index is added as a new column to the OWID dataset.

#Final check of missing values:
apply(is.na(data_owid),2, sum)

#Insight : There is no any NA values present in our dataset. Our code is clean now for further analysis.

## Exploratory Data Analysis
### Statistical summary

#Summary statistics with SKIM function
skim(data_owid)

#Insight : SKIM function provides an overview of the tidy dataset I have for analysis. After pre-processing the OWID dataset is reduced to 180
#observations and 19 variables. This overview represents clearly the column type and its frequency in our dataset. There are 3 column types
#namely character, date and numeric. The Variable type and their characters are also described here with the data distribution in a series of histogram for each variable separately.

### Visualization
#World map of country with their Total death:
l <- list(color = toRGB("black"), width = 0.5) # light black boundaries
# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)
fig <- plot_geo(data_owid)
fig <- fig %>% add_trace(
  z = ~total_deaths, color = ~total_deaths, colors = 'Reds',
  text = ~location, locations = ~iso_code, marker = list(line = l)
)
fig <- fig %>% colorbar(title = 'Covid Death - count', tickprefix = '')
fig <- fig %>% layout(
  title = 'Covid Death - Worldwide',
  geo = g
)
fig

#Insight : The world map portrays the total death cases in each of the country.
#I have taken the variable "total death" only for this visualization, in order to show the intensity of the covid death across countries.

#World map of country with thier HDI:
l <- list(color = toRGB("black"), width = 0.5) # light black boundaries
# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'robinson')
)
fig <- plot_geo(data_owid)
fig <- fig %>% add_trace(
  z = ~human_development_index, color = ~human_development_index, colors = 'Greens',
  text = ~location, locations = ~iso_code, marker = list(line = l)
)
fig <- fig %>% colorbar(title = 'Human Development Index', tickprefix = '')
fig <- fig %>% layout(
  title = 'Human Development Index - Worldwide',
  geo = g
)
fig

#Insight : The world map illustrates the HDI of each country.

#Scatterplot Matrices:
scatter_owid <- data_owid[c(8,14,16,15,19)] #Selecting only the required variables
scatter_owid.r <- abs(cor(scatter_owid)) #Getting the correlation
scatter_owid.col <- dmat.color(scatter_owid.r) #Getting colours for variables
scatter_owid.o <- order.single(scatter_owid.r) 
cpairs(scatter_owid, scatter_owid.o, panel.colors=scatter_owid.col, gap=.5,
       main="Scatterplot - Total death per million, HDI and its components" )

#Insight : The required variables which corresponds to HDI and total death per million is selected to plot the scatter plot. This illustrates how the data is plotted against the other variable. I can clearly see that when
#there is increase in HDI, life expectancy, and education index there is increase in total death as well.

#scatterplot3d
attach(data_owid) 
scatterplot3d(median_age,human_development_index,total_deaths_per_million, 
              pch=16, 
              highlight.3d=TRUE,
              type="h", 
              main="3D Scatterplot",
              phi = 0,
              ticktype = "detailed"
)

#Insight : A 3D scatter plot is plotted with numerical variables. Here x and y axis is taken as median_age and human_development_index with respect to our
#target variable in the z axis which is total_deaths_per_million. Here I can see all the data points are rising with increase in total deaths
#which also denotes a positive correlation between them. Here we can observe that as median age increases the HDI increases. This also
#indicates that total death cases increases as the HDI increases.

#Checking the data distribution - quantile-quantile plot
DataExplorer::plot_qq(data_owid[4:10])
DataExplorer::plot_qq(data_owid[11:16])

#Insight : Here I check the distribution of our data with the help of QQ plot. If the data is normally distributed, the points in the QQ-normal plot lie
#on a straight diagonal line. According to the qq plot the following variables are normally distributed: Aged_65_older Aged_70_older
#Cardiovasc_deaths_rates Diabetes_prevalence Life expectancy Human_Development_Index Total_deaths_per_million

#Scatterplot - HDI with total death with respect to each Continent
ggplot(data = data_owid) + 
  geom_point(mapping = aes(x = human_development_index, y = total_deaths_per_million, color = continent))

#Insight : This scatter plot is plotted to see the relationship of total_deaths_per_million and human_development_index with respect to
#each continent. Here I can observe that Europe has high HDI and it has contributed to more death. In contrast, I have Africa which has low HDI
#and low death. Continent like Asia, South America, North America and Oceania are moderate in HDI and total death.

#Top 5 countries with high HDI and low 5 countries with less HDI
#select relevant variables for plotting
Owid_data_Graph <- data_owid %>% 
  select(iso_code,location,median_age,aged_70_older,cardiovasc_death_rate,diabetes_prevalence,life_expectancy,gdp_per_capita,education_index,human_development_index,total_deaths_per_million)
OWiD_hdi <- Owid_data_Graph %>%
  select(location,human_development_index)

#Select top5 and bottom 5 countries by HDI
OWiD_high_hdi <- OWiD_hdi  %>% top_n(5)
OWiD_low_hdi <- OWiD_hdi  %>% top_n(-5)

#Combine top and bottom HDI countries
OWiD_high_low<-bind_rows(OWiD_high_hdi, OWiD_low_hdi)

#Color for bar chart
coul <- brewer.pal(5, "Set2") 
barplot(OWiD_high_low$human_development_index,
        main = "Top 5 and low 5 HDI Countries",
        ylab = "HDI",
        xlab = "Countries",
        ylim=c(0,1),
        names.arg = OWiD_high_low$location,
        col = coul,
        width=c(1),
        density=c(20),
        las=1,
        horiz = FALSE
)

#Insight : The above bar plot illustrates the countries which has high HDI and low HDI. Hong Kong, Iceland, Ireland, Norway and Switzerland are countries
#with high HDI. Burundi, Central African Republic, Chad, Niger and South Sudan are countries with low HDI.

#Top 5 countries (HDI components with total death per million)
#High HDI countries, plot all HDI parameters
#Select - Top5 HDI countries
OWiD_all_Param_Top <- Owid_data_Graph[order(-Owid_data_Graph$human_development_index),][1:5,]

#apply log to scale down the index values
OWiD_all_Param_Top$total_deaths_per_million <- round(log(OWiD_all_Param_Top$total_deaths_per_million),2)
OWiD_all_Param_Top$gdp_per_capita <- round(log(OWiD_all_Param_Top$gdp_per_capita),2)
OWiD_all_Param_Top$life_expectancy <- round(log(OWiD_all_Param_Top$life_expectancy),2)
OWiD_all_Param_Top$education_index <- round(log(OWiD_all_Param_Top$education_index),2)
OWiD_all_Param <- OWiD_all_Param_Top  %>%
  select(location,life_expectancy,gdp_per_capita,education_index,total_deaths_per_million)

#convert to Pivot longer for plot
OWiD_all_Param_PL <- OWiD_all_Param %>%
  pivot_longer(cols=-c("location"), names_to = "Index" , values_to = "Log_Index")
ggplot_all_param <- ggplot(data=OWiD_all_Param_PL, aes(x=location, y=Log_Index, fill=Index)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Log_Index), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  scale_fill_manual(values = coul)+
  theme_minimal()
ggplot_all_param + ggtitle("Top 5 HDI countries for HDI Parameters")

#Insight : I plot a bar graph taking country with high HDI to visualize how life_expectancy, gdp_per_capita and education_index care are with
#respect to the total death. Here we can observe that all the components plotted here are relatively similar, except for Ireland and Switzerland
#which shows slight increase in deaths despite being comparatively similar to other countries.

## Applied log for all variables to fit them into scale for the bar chart visualization.

#Low 5 countries (HDI components with total death per million)
#Select - Low 5 HDI countries
OWiD_all_Param_Top <- Owid_data_Graph[order(Owid_data_Graph$human_development_index),][1:5,]

#apply log to scale down the index values
OWiD_all_Param_Top$total_deaths_per_million <- round(log(OWiD_all_Param_Top$total_deaths_per_million),2)
OWiD_all_Param_Top$gdp_per_capita <- round(log(OWiD_all_Param_Top$gdp_per_capita),2)
OWiD_all_Param_Top$life_expectancy <- round(log(OWiD_all_Param_Top$life_expectancy),2)
OWiD_all_Param_Top$education_index <- round(log(OWiD_all_Param_Top$education_index),2)
OWiD_all_Param <- OWiD_all_Param_Top  %>%
  select(location,life_expectancy,gdp_per_capita,education_index,total_deaths_per_million)

#convert to Pivot longer for plot
OWiD_all_Param_PL <- OWiD_all_Param %>%
  pivot_longer(cols=-c("location"), names_to = "Index" , values_to = "Log_Index")
ggplot_all_param <- ggplot(data=OWiD_all_Param_PL, aes(x=location, y=Log_Index, fill=Index)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Log_Index), vjust=1.6, color="white", 
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  scale_fill_manual(values = coul)+
  theme_minimal()
ggplot_all_param + ggtitle("Low 5 HDI countries for HDI Parameters")

#Insight : I plot a bar graph taking country with high low to visualize how life_expectancy, gdp_per_capita and education_index care are with
#respect to the total death. Here we can observe that Central African Republic has contributed to more death even though their other
#components are relatively similar to other countries and Burundi has less deaths.

## Applied log for all variables to fit them into scale for the bar chart visualization.

# Top 5 and low 5 countries with high cardiac death rate and their covid death
#Select - Top 5 HDI countries
OWiD_all_cardiac_top <- Owid_data_Graph[order(-Owid_data_Graph$human_development_index),][1:5,]

#select required variables cardiac_death and covid death
OWiD_all_cardiac_covid_high <- OWiD_all_cardiac_top  %>%
  select(location,cardiovasc_death_rate,total_deaths_per_million,human_development_index)

#Select - bottom 5 HDI countries
OWiD_all_cardiac_bottom <- Owid_data_Graph[order(Owid_data_Graph$human_development_index),][1:5,]

#select required variables cardiac_death and covid death
OWiD_all_cardiac_covid_bottom <- OWiD_all_cardiac_bottom  %>%
  select(location,cardiovasc_death_rate,total_deaths_per_million,human_development_index)

#Combine top and bottom HDI countries
OWiD_all_cardiac_top<-bind_rows(OWiD_all_cardiac_covid_high, OWiD_all_cardiac_covid_bottom)

#Apply log to to fit to the bar chart scale
OWiD_all_cardiac_top$cardiovasc_death_rate <- round(log(OWiD_all_cardiac_top$cardiovasc_death_rate),2)
OWiD_all_cardiac_top$total_deaths_per_million <- round(log(OWiD_all_cardiac_top$total_deaths_per_million),2)

#Convert to Pivot longer for plot
OWiD_all_cardiac_top <- OWiD_all_cardiac_top %>%
  pivot_longer(cols=-c("location"), names_to = "Index" , values_to = "value")

#Horizontal Plot
ggplot(OWiD_all_cardiac_top[order(OWiD_all_cardiac_top$Index, decreasing = T),],
       aes(value,  y=reorder(location, -value), label= round(value,2), fill=factor(Index, levels=c("total_deaths_per_million","cardiovasc_death_rate","human_development_index")))) + 
  geom_bar(stat = "identity") +
  scale_fill_discrete(name = "Category", labels = c("total_deaths_per_million","cardiovasc_death_rate","human_development_index"))+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14)) +
  ylab("Countries") + 
  ggtitle("Top 5 and bottom 5 HDI countries with corresponding cardiac death rate and covid_death")

#Insight : Seeing the trend of the top 5 and bottom 5 countries on HDI and their cardiac death and covid death, there is a visible trend which shows,
# a) for the high HDI countries, the cardiac death is relatively low but the covid death is relatively high.
# b) for the low HDI countries, the cardiac death is relatively high but the covid death is relatively low.

#Insight : Applied log to cardiac death rate and covid death variable to fit them in the bar chart. This chart is to show the the 3 variables and their trend, not the actual value.

# Top 5 and low 5 countries with high diabetes prevalence and their covid death
#Select -  Top 5 HDI countries
OWiD_all_diabetes_top <- Owid_data_Graph[order(-Owid_data_Graph$human_development_index),][1:5,]

#select required variables cardiac_death and diabetes prevalence
OWiD_all_diabetes_top <- OWiD_all_diabetes_top  %>%
  select(location,diabetes_prevalence,total_deaths_per_million,human_development_index)

#Select - bottom 5 HDI countries
OWiD_all_diabetes_bottom <- Owid_data_Graph[order(Owid_data_Graph$human_development_index),][1:5,]

#select required variables cardiac_death and diabetes prevalence
OWiD_all_diabetes_bottom <- OWiD_all_diabetes_bottom  %>%
  select(location,diabetes_prevalence,total_deaths_per_million,human_development_index)

#Combine top and bottom HDI countries
OWiD_all_diabetes<-bind_rows(OWiD_all_diabetes_top, OWiD_all_diabetes_bottom)

#Apply log to to fit to the bar chart scale
OWiD_all_diabetes$total_deaths_per_million <- round(log(OWiD_all_diabetes$total_deaths_per_million),2)

#Convert to Pivot longer for plot
OWiD_all_diabetes <- OWiD_all_diabetes %>%
  pivot_longer(cols=-c("location"), names_to = "Index" , values_to = "value")

#Horizontal Plot
ggplot(OWiD_all_diabetes[order(OWiD_all_diabetes$Index, decreasing = T),],
       aes(value,  y=reorder(location, -value), label= round(value,2), fill=factor(Index, levels=c("diabetes_prevalence","total_deaths_per_million","human_development_index")))) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name = "Category", labels = c("diabetes_prevalence","total_deaths_per_million","human_development_index"))+
  scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16)) +
  ylab("Countries") + 
  ggtitle("Top 5 and bottom 5 HDI countries with corresponding diabetes prevalance and covid_death")

#Insight : Diabetes prevalence for high and low HDI countries are not hugely varied. No clear trend been seen between diabetic prevalence and covid
#death for high and low HDI countries. Hence the covid death may not be related to diabetes prevalence, but need to be analysed with other related parameters.

#Insight : Applied log to covid death variable to fit all variables in the bar chart. This chart is to show the the 3 variables and their trend, not the actual value.

# Top 5 and bottom 5 countries with Aged-70_older and their median age and death per million
#Select - 5 aged_70_older
OWiD_all_aged_top <- Owid_data_Graph[order(-Owid_data_Graph$human_development_index),][1:5,]

#select required variables cardiac_death and diabetes prevalence
OWiD_all_aged_top <- OWiD_all_aged_top  %>%
  select(location,median_age,aged_70_older,total_deaths_per_million,human_development_index)

#Select - bottom 5 aged_70_older
OWiD_all_aged_bottom <- Owid_data_Graph[order(Owid_data_Graph$human_development_index),][1:5,]

#select required variables cardiac_death and diabetes prevalence
OWiD_all_aged_bottom <- OWiD_all_aged_bottom  %>%
  select(location,median_age,aged_70_older,total_deaths_per_million,human_development_index)

#Combine top and bottom aged_70_older
OWiD_all_aged<-bind_rows(OWiD_all_aged_top, OWiD_all_aged_bottom)

#Apply log to to fit to the bar chart scale
OWiD_all_aged$total_deaths_per_million <- round(log(OWiD_all_aged$total_deaths_per_million),2)

#Convert to Pivot longer for plot
OWiD_all_aged <- OWiD_all_aged %>%
  pivot_longer(cols=-c("location"), names_to = "Index" , values_to = "value")

#Horizontal Plot
ggplot(OWiD_all_aged[order(OWiD_all_aged$Index, decreasing = T),],
       aes(value,  y=reorder(location, -value), label= round(value,2), fill=factor(Index, levels=c("aged_70_older","median_age","total_deaths_per_million","human_development_index")))) + 
  geom_bar(stat = "identity") +
  scale_fill_discrete(name = "Category", labels = c("aged_70_older","median_age","total_deaths_per_million","human_development_index"))+
  scale_x_continuous(breaks=c(10,20,30,40,50,60,70,80)) +
  ylab("Countries") + 
  ggtitle("Top 5 and bottom 5 HDI countries with corresponding aged_70_order and median age and total deaths per million")

#Insight : Median age and aged_70_older are related variables is a known fact. From this bar chart we can infer that, higher the HDI means Higher the median age and higher the covid death.
# Applied log to covid death variable to fit all variables in the bar chart scale. This chart is to show the the 4 variables and their trend, not the actual value.

### Correlation

owid_corr <- cor(data_owid[,c(4:16)])
corrplot(owid_corr, method = "circle")

#Insight : Correlation defines how strong the variables are related to each other.
#There are 3 types of correlation,
#1.Positive correlation
#2.Negative correlation
#3.Zero correlation

#Correlation coefficient is a statistical measure to identify the type of correlation existing within the variables. From the correlation plot. We
#can observe from the correlation plot that median_age, aged_65_older and aged_70_older are more correlate to each other and also shows a high
#positive correlation. Female_smokers also shows a high positive correlation with variables related to age. cardiovasc_death_rate shows
#negative correlation. However, they are low so it can be ignored.

# Analysis

## Hypothesis testing: Hypothesis testing is a statistical test performed to prove an idea or an assumption by using sample data. The test provides evidence
#concerning the plausibility of the hypothesis. The motive of the project is to identify the correlation between Human Development Index and the
#total deaths caused by COVID-19 pandemic. This can be found by performing the hypothesis testing. The selected variables are
#quantitative data, hence we can perform Spearman's correlation test which provides us correlation as well as the direction of the relationship between two continuous variables.

#Hypothesis Statement:
##H0 (Null hypothesis) :** There is no linear relationship between HDI and number of death
##H1 (Alternate hypothesis) :** There is a linear relationship between HDI and number of death
##The H0 and H1 are mutually exclusive, and only one can be true. However, one of the two hypothesis will always be true
##It is required to check the normality of the data before a hypothesis is performed.Here I have chosen Shapiro-Wilk normality test for evaluation
##as they are widely recommended and more powerful than the other tests. Shapiro-Wilk normality testdepends on the correlation between the data and their normal scores.

##Shapiro-Wilk normality test

#Normality test - Shapiro-Wilk
shapiro.test(data_owid$human_development_index)
#p-value =  1.416e-05
shapiro.test(data_owid$total_deaths_per_million)
#p-value = 5.283e-12

#From the output, the two p-values are less than the significance level of 0.05 implying that the distribution of the data is significantly
#different from normal distribution, which means the data is not normally distributed.

#Hypothesis testing:
#1.HDI vs Total Death - All countries
## I perform Spearman's correlation test because from the Shapiro-Wilk normality test it can be observed that the data does not follow normal distribution.
hdi_corr_all <- cor.test(data_owid$human_development_index, data_owid$total_deaths_per_million, 
                         method = "spearman", exact = FALSE)
hdi_corr_all

#Insight : The p-value here is 2.2e-16, which is less than the significance level alpha = 0.05. From this it can concluded that HDI and total deaths are
#significantly correlated with a correlation coefficient of 0.6109514. Hereby, we reject H0 and accept H1.

#2.HDI vs Total Death - High HDI and low HDI:
#Assumption : Countries with HDI 0.7 and above are considered to be countries with high HDI Countries with HDI 0.5 and below are considered to be countries with low HDI

#Hypothesis Test - Country with high HDI vs Total Death
#Selecting countries with high HDI
OWiD_high_hdi <- data_owid %>% filter(human_development_index > 0.7)
OWiD_high_hdi

#Spearman's correlation test:
hdi_corr_high <- cor.test(OWiD_high_hdi$human_development_index, OWiD_high_hdi$total_deaths_per_million, 
                          method = "spearman", exact = FALSE)
hdi_corr_high

#Insight:Here p-value = 0.5004 and Correlation coefficient = 0.6403101. Here we fail to reject H0. It can be concluded that there is no linear relation between HDI and number of deaths in countries with high HDI.

#3. HDI vs Total Death - Country with low HDI vs Total Death

#Selecting countries with low HDI
OWiD_low_hdi <- data_owid %>% filter(human_development_index < 0.5)
OWiD_low_hdi

#Spearman's correlation test :
hdi_corr_low <- cor.test(OWiD_low_hdi$human_development_index, OWiD_low_hdi$total_deaths_per_million, 
                         method = "spearman", exact = FALSE)
hdi_corr_low

#Insight : Here p-value = 0.0003852 and Correlation coefficient = 0.7453452 p value is less than 5%, hence we reject H0 and accepting H1 which states there is linear relation between HDI and total death in countries with low HDI

#4. HDI vs Total Death - Continent Wave 1 - European region

#Extracting data with date of Wave 1
data_owid_wave1 <- data_Covid %>% 
  select(continent, location, date, human_development_index, total_deaths_per_million) %>% 
  group_by(location) %>% filter(date == "2020-06-01") %>% # Take the most recent date per country
  group_by() #removing groupby

#Insight :Creating another dataset filtering date of 2020-06-01 which denotes the first wave of the pandemic.

#Renaming the variables:
colnames(data_owid_wave1)[6] <- "total_deaths_per_million_Wave1"
head(data_owid_wave1,1)

#Rename the column - total_deaths_per_million_Wave as total_deaths_per_million_Wave1 which would be used in further analysis while joining both the dataset - data_owid and data_owid_wave1

#Joining both the dataset
data_owid_wave1 <- left_join(data_owid, data_owid_wave1, by = c('location'))
head(data_owid_wave1,2)

apply(is.na(data_owid_wave1),2, sum)

#Insight : 19 countries does not have data as of 2020-06-01. This might be a reason that wave 1 would not have started for these countries as of June 2020.

#Removing NA from total_deaths_per_million.y
data_owid_wave1 <- data_owid_wave1 %>% filter(!is.na(total_deaths_per_million.y))

#Filtering continent - Europe
data_owid_wave1 <- data_owid_wave1 %>% 
  filter(continent.x == "Europe")

#Spearman's correlation test:
hdi_corr_europe <- cor.test(data_owid_wave1$human_development_index.y, data_owid_wave1$total_deaths_per_million.y,
                            method = "spearman", exact = FALSE)
hdi_corr_europe

#Insight : Here p-value = 0.02015 and Correlation coefficient = 0.463047, here p value is less than 5%, hence we reject H0 and accepting H1 which states there is linear relation between HDI and total death in European countries when first wave occurred.

### Hypothesis test - Table and comparison

#Creating Matrix to add correlation result:
table_corr<- matrix(0,4,4) # entry, rows, columns
row.names(table_corr) <- c("All_countries", "Countries_High_HDI ", "Countries_low_HDI ", "Wave_1_Europe")
colnames(table_corr) <- c("P-value","T-test","Correlation", "H0/H1")
#Result for all countries
table_corr[1,1] <- round(hdi_corr_all$p.value,3)
table_corr[1,2] <- round(hdi_corr_all$statistic,3)
table_corr[1,3] <- round(hdi_corr_all$estimate,3)
table_corr[1,4] <- "H1"
#Result for High HDI countries
table_corr[2,1] <- round(hdi_corr_high$p.value,3)
table_corr[2,2] <- round(hdi_corr_high$statistic,3)
table_corr[2,3] <- round(hdi_corr_high$estimate,3)
table_corr[2,4] <- "H0"
#Result for Low HDI countries
table_corr[3,1] <- round(hdi_corr_low$p.value,3)
table_corr[3,2] <- round(hdi_corr_low$statistic,3)
table_corr[3,3] <- round(hdi_corr_low$estimate,3)
table_corr[3,4] <- "H1"
#Result for wave1 - Europe
table_corr[4,1] <- round(hdi_corr_europe$p.value,3)
table_corr[4,2] <- round(hdi_corr_europe$statistic,3)
table_corr[4,3] <- round(hdi_corr_europe$estimate,3)
table_corr[4,4] <- "H1"
table_corr <- as.data.frame(table_corr)

### Summarizing the hypothesis results:

table_corr

## Plotting the correlation obtained in the hypothesis results:

#human_development_index and total_deaths_per_million - All countries
ggscatter(data_owid, x = "human_development_index", y = "total_deaths_per_million", 
          add = "reg.line", conf.int = TRUE, 
          color = "blue",
          palette = NULL,
          cor.coef = TRUE, 
          cor.method = "spearman",
          xlab = "Human Development Index", ylab = "Deaths per million",
          title = 'Human Development Index and Deaths per million - All countries')

#Insight : We can observe from the Spearman's Correlation test between the HDI and total deaths in all country that H0 is rejected since p-value is less
#than significance value of 0.05. This brings to a conclusion that both the variables are linearly dependent. Furthermore, by visualizing the
#result through scatter plot, we can conclude saying the variables arepositively correlated. The Spearman's correlation coefficient r which is 0.61 which again justifies that it is far away from 0 which makes the
#linear relationship more stronger between the HDI and total deaths. It can be interfered that as HDI increases the total death also increases.

#Plot for High HDI:
ggscatter(OWiD_high_hdi, x = "human_development_index", y = "total_deaths_per_million", 
          add = "reg.line", conf.int = TRUE, 
          color = "red",
          palette = NULL,
          cor.coef = TRUE, 
          cor.method = "spearman",
          xlab = "Human Development Index", ylab = "Deaths per million",
          title = 'Human Development Index and Deaths per million - High HDI countries')

#Insight : The Spearman's Correlation test between the country with high HDI and total deaths concludes that H1 is accepted and we failed to reject H0
#since p-value is more than significance value of 0.05. This concludes that both the variables are not linearly dependant. With the visualizing
#of scatter plot, we can conclude saying the variables are not correlated with each other and they do not project any linear dependencies between
#them. To makes this statement even stronger, the Spearman's correlation coefficient r which is 0.064 which displays zero correlation between the two variables.

## Linear Regression - Modelling:
#Insight : From the hypothesis testing, we can see there is no relation between the total deaths per million and the HDI in countries with high HDI. Hence
#to identify what are the other contributing factors leading to covid death, I now perform a linear regression between countries with high HDI and covid deaths.

#Feature selection:
#Selecting only required data as per correlation plot:
data_owid <- data_owid %>%
  filter(human_development_index > 0.7) %>%
  select (population,  median_age, gdp_per_capita, cardiovasc_death_rate, 
          diabetes_prevalence, male_smokers,hospital_beds_per_thousand, life_expectancy, 
          human_development_index, education_index, total_deaths_per_million)

#Insight : Select only required variables to see the relationship between death and HDI.

#Normalise the data points:
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data_owid$population<-normalize(data_owid$population)
data_owid$median_age<-normalize(data_owid$median_age)
data_owid$gdp_per_capita<-normalize(data_owid$gdp_per_capita)
data_owid$cardiovasc_death_rate<-normalize(data_owid$cardiovasc_death_rate)
data_owid$diabetes_prevalence<-normalize(data_owid$diabetes_prevalence)
data_owid$male_smokers<-normalize(data_owid$male_smokers)
data_owid$hospital_beds_per_thousand<-normalize(data_owid$hospital_beds_per_thousand)
data_owid$life_expectancy<-normalize(data_owid$life_expectancy)
data_owid$human_development_index<-normalize(data_owid$human_development_index)
data_owid$education_index<-normalize(data_owid$education_index)

#Insight : Since the dataset is not normalized as per the qq plot, performing a normality function to normalize data, which is essential for performing linear regression.

#Splitting the data
set.seed(1234)
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}
data_train <- create_train_test(data_owid, 0.8, train = TRUE)
data_test <- create_train_test(data_owid, 0.8, train = FALSE)
dim(data_train)
dim(data_test)

## Here the dataset is split into two datasets. 1. Train 2. Test

#Fit, train and predicting the model
fitControl <- trainControl(method = "none")
lm_train_pred <- train(total_deaths_per_million ~., 
                       data = data_train,
                       method = "lm",
                       trControl = fitControl)
lm_train_pred
lm_pred = predict(lm_train_pred, data_test)
summary(lm_train_pred)

#Insight : From the linear regression summary it can be confirmed that
#a.Cardiac_death_rate and male_Smokers are not statistically significant because their p-values are greater than the usual significance level of 0.05

#b.The HDI coefficient in the regression equation is 2137.95. This coefficient represents the mean increase of total deaths per million for every additional one increase in HDI, which means if HDI increases by 0.1, the average total deaths per million increases by 2137.95. It is also interesting to find that median_age shows a high positive correlation with covid death. The coefficient of median age in the regression equation is 2572.14. If median age increases by 1, the average total deaths per million increases by 2572.14.

#c.There is a negative coefficient for variables such as gdp_per_capita, diabetes_prevalence, life_expectancy and education_index which suggests that as these variables increases, the covid death decreases.

# Conclusion : From the overall analysis, identified that there exist a linear relation between HDI and covid death. However, while exploring deep into
#separate countries with high HDI and low HDI we can observe that, high HDI countries has no relationship with covid death. Hence performing a
#linear relation to identify what are the other factors contributing to death in countries with high HDI. It can be concluded from the linear
#model that median age variable (aged population) is one of the prominent factor which contributes to total death, which means high median age
#contributes to high covid deaths.