---
title: "Correlation & Heat Map Visualization"
date: "01/03/2020"
output:
  pdf_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, fig.height=3, fig.width=8)
library(tidyverse)
library(readr)
library('data.table')
library('Rmisc')
library('ggpubr')
library('Hmisc')
library('dplyr')
library('data.table')
library('plyr')
library(RColorBrewer)
library('AggregateR')
library(pastecs)
library(RColorBrewer)
library(ggplot2)
library(rworldmap)
library("cowplot")
```

# 1. Understanding the Data

*The purpose of the report* is to examine the relationship between the treatments & death rate of HIV infected people having TB. This analysis is centered around the datasets *TB_burden_countries* and *TB_outcomes*.

*TB_burden_countries* dataset records the data from year 2000-2018 for all the WHO countries under six regions. The dataset describes the incidence and mortality for various TB patients, the number of HIV positive and patients having MDR/RR TB (i.e. drug resistant TB). From this table, the focus is on **e_inc_num** and **e_mort_tbhiv_num** variables.

*TB_outcomes* dataset records the resutls of various treatment outcomes (if the patients were cured or not), the focus is on the **tbhiv_succ** variable.

Firstly, we created 2 dataframes by importing csv files, and tidying the datasets by filtering, cleaning and removing the NA (Blank) values (using *is.na(Variable_Name)* function). 

```{r echo=FALSE}
df_outcomes_src<-read.csv("D:/Documents/TB_outcomes.csv",header =TRUE)

df_countries_src<-read.csv("D:/Documents/TB_burden_countries.csv",header =TRUE)
```

**Structure of Dataset:**

By joining and aggregating along the countries and year we made a merged dataframe for our analysis. Following is the structure of the new dataset.

```{r echo=FALSE}

df_outcomes<-df_outcomes_src[!(is.na(df_outcomes_src$tbhiv_succ) | df_outcomes_src$tbhiv_succ==""), ]
#dim(df_outcomes)

df_countries<-df_countries_src[!(is.na(df_countries_src$e_mort_tbhiv_num) | df_countries_src$e_mort_tbhiv_num==""), ]
df_countries<-filter(df_countries, year %in% c(2012,2013,2014,2015,2016,2017))
#dim(df_countries)

df_outcomes<-arrange(df_outcomes, year)
df_countries<-arrange(df_countries, year)


merged_set<-merge(x=df_countries,y=df_outcomes,by=c("country","year"),all.x=TRUE)

#dim(merged_set)
#summary(merged_set)
#select(merged_set, country, year, e_mort_tbhiv_num,tbhiv_succ )

merged_set2<-merged_set[!(is.na(merged_set$tbhiv_succ) | merged_set$tbhiv_succ==""), ]

merged_set2%>% select(country,year,g_whoregion.x,e_mort_tbhiv_num,tbhiv_succ)%>% str
```

**country **\
*datatype:* factor with 216 levels  \
*description:* List of WHO countries 
**year**\
*datatype:* numeric \
*description:* year 2012-2017 for which the data is collected and analysed\
**g_whoregion.x**\
*datatype:* factor with 6 levels \
*description:* 6 WHO sub-regions under which all countries are seggregated\
**e_mort_tbhiv_num**\
*datatype:* numeric \
*description:* Estimated number of deaths from TB in people who are HIV positive\
**tbhiv_succ **\
*datatype:* numeric \
*description:* Outcome (treatment success/completed) for HIV positive TB cases (all types)\

```{r echo=FALSE}
#merging and correlation 
dim(merged_set2)
```

The following World Heat Mapshows the percentage of incident cases of Tuberculosis. 

```{r echo=FALSE}

merged_set4 <- mutate(merged_set2, e_inc_perc = e_inc_num/e_pop_num*100*100)
mapped_data <- joinCountryData2Map(merged_set4, joinCode = "ISO3", nameJoinColumn = "iso3.x")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapParams <- mapCountryData( mapped_data, nameColumnToPlot = "e_inc_perc", mapTitle="Percentage of Incident TB for all forms (including HIV)", addLegend=FALSE )
do.call( addMapLegend, c(mapParams, legendWidth=0.5, legendMar = 2))
#mapCountryData(mapped_data, nameColumnToPlot = "e_inc_perc")
```

# 2. Summarizing the Variable Selection

*a. Identification of variables*

From the merged dataset, we have chosen two variables, *e_mort_tbhiv_num* and *tbhiv_succ*. Our assumption is that more successfull the treatments are, lesser will be the mortality rate. 

#Measures of central tendency
```{r echo=FALSE}
central_tendency_mean<-function(x){
 return(round(mean(x),digits=3))
  }
central_tendency_median<-function(x){
  return(median(x))
}
central_tendency_mode<-function(x){
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
  paste(ux[tab == max(tab)],collapse=" ")
}
ct<-function(x,y){
  cDT = data.table(
  Measure = c("Mean","Median","Mode"),
  "|" = c("|","|","|"),
  tbhiv_succ = c(central_tendency_mean(x), central_tendency_median(x), central_tendency_mode(x)),
  "|" = c("|","|","|"),
  e_mort_tbhiv_num = c(central_tendency_mean(y), central_tendency_median(y),central_tendency_mode(y))
  )
  return(cDT)
}

ct(merged_set2$tbhiv_succ,merged_set2$e_mort_tbhiv_num)
```

#Measure of Dispersion:
```{r echo=FALSE}
dispersion_variance<-function(x){
  return(round(var(x),digits=3))
}
dispersion_sd<-function(x){
  return(round(sd(x),digits=3))
}

disper<-function(x,y){
  dDT = data.table(
  Measure = c("Variance","Standard Deviation","Inter-Quartile Range (IQR)", "Range"),
  "|" = c("|","|","|","|"),
  tbhiv_succ = c(dispersion_variance(x), dispersion_sd(x), IQR(x), paste(range(x),collapse=" ")),
  "|" = c("|","|","|","|"),
  e_mort_tbhiv_num = c(dispersion_variance(y), dispersion_sd(y), IQR(y), paste(range(y),collapse=" "))
)
  return(dDT)
}

disper(merged_set2$tbhiv_succ, merged_set2$e_mort_tbhiv_num)
```

*b. Assumptions for Normality Test and log Transformation*\

*e_mort_tbhiv_num - normality check*

```{r echo=FALSE}

stat.desc(merged_set2$e_mort_tbhiv_num, basic = FALSE, norm = TRUE)
```
The skew.2SE(4.005480e+01) and kurt.2SE (1.470515e+02) values are >1, indicating skweness. Further confirming the results visually using histogram and Q-Q plot, we see that the data is right skewed.

*tbhiv_succ - normality check*
```{r echo=FALSE}
stat.desc(merged_set2$tbhiv_succ, basic = FALSE, norm = TRUE)
```
The skew.2SE(6.437803e+01) and kurt.2SE (3.698983e+02) values are >1, indicating skweness. Further confirming the results visually using histogram and Q-Q plot, we see that the data is right skewed.

```{r echo=FALSE}
par(mfrow=c(2,4))

hist(merged_set2$e_mort_tbhiv_num, ylim = c(0,900),main="Mortality: HIV TB patients")
hist(log(merged_set2$e_mort_tbhiv_num), xlim = c(0,12),main="LOG of Mortality: HIV TB patients")
hist(merged_set2$tbhiv_succ, ylim = c(0,900),main="Treatment success: HIV TB patients")
hist(log(merged_set2$tbhiv_succ), xlim = c(0,12),main="LOG Treatment success: HIV TB patients")
qqnorm(merged_set2$e_mort_tbhiv_num, ylim = c(0,20))
qqnorm(log(merged_set2$e_mort_tbhiv_num), ylim=c(0,20))
qqnorm(merged_set2$tbhiv_succ, ylim=c(0,20))
qqnorm(log(merged_set2$tbhiv_succ), ylim=c(0,20))

```
*Shapiro test*
```{r echo=FALSE}
shapiro.test(merged_set2$e_mort_tbhiv_num)
```
Since the p-value is too low we can conclude that the data is not normal.

Upon further analysis of the skewness, there were many outliers identified as shown by the below box plots. 

```{r echo=FALSE}

g1<-ggplot(merged_set2, aes(x=g_whoregion.x, y=e_mort_tbhiv_num)) + 
  geom_boxplot(outlier.colour="red") + scale_y_continuous(limits=c(0,25000))

g2<-ggplot(merged_set2, aes(x=g_whoregion.x, y=tbhiv_succ)) + 
  geom_boxplot(outlier.colour="red") + scale_y_continuous(limits=c(0,10000))

plot_grid(g1, g2, nrow=1)
```
We did not find it appropriate to remove these outliers as the sample population of each region varies distinctly. Some regions have high mortality and HIV-TB incidence than others. This is the region for skewness and is necessary for our analysis.

*c. Appropriate Correlation Test*
**Correlation** is Way of measuring the extent to which **e_mort_tbhiv_num** and **tbhiv_succ** are related. It measures the pattern of responses across variables. We have found the correlation on aggregated data by years.

*Spearman Correlation test*
**Since our data is non parametric we cannot perform Pearson's correlation test and therefore testing our assumption using Spearman test.

```{r echo=FALSE}
agg1<-aggregate(cbind(e_mort_tbhiv_num, tbhiv_succ) ~ year, data = merged_set2, sum)
cor(agg1$e_mort_tbhiv_num,agg1$tbhiv_succ,method='spearman')

 
```
From Spearman's test the Rho is -0.2571 which indicates negative correlation.

*Kendall Correlation test*
```{r echo=FALSE}
cor(agg1$e_mort_tbhiv_num,agg1$tbhiv_succ,method="kendall")
 
```
From Kendall's test p value is very small and true tau is not equal to zero. Thus, test statitics prove that x(e_mort_tbhiv_num) and y(tbhiv_succ) are slightly negatively  correlated which confirms our hypothesis.


```{r echo=FALSE}
agg1 %>% ggplot(aes(tbhiv_succ,e_mort_tbhiv_num))+geom_point(size=1,alpha=1,color="red")+geom_smooth(size=1,linetype=1,method="lm",se=TRUE)
```

*e. Identify the question for hypothesis (before analysis)*

# 3. Analysis

To corroborate the negative correlation between the number of mortality in HIV patients with Tuberculosis, and the Number of Successful treatments in HIV patients with Tuberculosis, we determine the based on the 6 main sub-regions  as follows:\
```{r echo=FALSE}

new_df<-aggregate(cbind(e_mort_tbhiv_num, tbhiv_succ) ~ year+g_whoregion.x, data = merged_set2, mean)

df <- data.frame(x = new_df$year, y = new_df$e_mort_tbhiv_num, z = new_df$tbhiv_succ, a=new_df$g_whoregion.x)
```

```{r echo=FALSE}


a1<-df%>% filter(a=="AFR")%>%
ggplot( aes(x)) + facet_grid(.~a) +
  geom_line(aes(y = y, colour = "Mortality"))+ 
  geom_line(aes(y = z, colour = "Treated"))+theme(legend.position="bottom")


a2<-df%>% filter(a=="AMR")%>%
ggplot( aes(x)) + facet_grid(.~a) +
   geom_line(aes(y = y, colour = "Mortality"))+ 
  geom_line(aes(y = z, colour = "Treated"))+theme(legend.position="bottom")

a3<-df%>% filter(a=="EMR")%>%
ggplot( aes(x)) + facet_grid(.~a) +
  geom_line(aes(y = y, colour = "Mortality"))+ 
  geom_line(aes(y = z, colour = "Treated"))+theme(legend.position="bottom")

a4<-df%>% filter(a=="EUR")%>%
ggplot( aes(x)) + facet_grid(.~a) +
  geom_line(aes(y = y, colour = "Mortality"))+ 
  geom_line(aes(y = z, colour = "Treated"))+theme(legend.position="bottom")

a5<-df%>% filter(a=="WPR")%>%
ggplot( aes(x)) + facet_grid(.~a) +
  geom_line(aes(y = y, colour = "Mortality"))+ 
  geom_line(aes(y = z, colour = "Treated"))+theme(legend.position="bottom")

a6<-df%>% filter(a=="SEA")%>%
ggplot( aes(x)) + facet_grid(.~a) +
  geom_line(aes(y = y, colour = "Mortality"))+ 
  geom_line(aes(y = z, colour = "Treated"))+theme(legend.position="bottom")

```
```{r echo=FALSE}

plot_grid(a1,a2,a3, nrow=1)
plot_grid(a4,a5,a6, nrow=1)

```


Conclusion and Future Scope

1. Using Spearman's Correlation  test we get co-relation of -0.2571 i.e Estimated number of deaths from TB in people who are HiV positive (aggregated e_mort_tbhiv_num) is negatively co-related with Treatment success for HIV positive TB cases ( aggregated tbhiv_succ), which confirms ours asumption. 

2. For region AFR (Africa), the mortality rate decreases with increasing treatment success.

3. For region AMR (America), the mortality rate is almost constant over time.

4. For EMR region, mortality increases intially from 2011 to 2013 and decreases steeply over the years while treatment success slightly increased.

5. For European regions, the mortality has been increasing due to increase in HIV and also, the treatment success rate has increased drastically. This, is the only region which doesn't have negative correlation as shown in graphs.

6. For WPR, it follows our assumption completely, Mortality rate has decreased drastically with high increase in Treatment success rate. 

7. For SEA, mortality rate has decreased sharply also the treatment success rate has been decreasing gradually.

8. AS per our assumption, mortality rate decreases with increase in the treatment success rates, aggregated over the years as evident from our overall plot. This is because, as the technology has improved, TB is more likely to be cured than older times. 


