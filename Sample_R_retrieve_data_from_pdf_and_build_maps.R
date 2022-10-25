# This project comprises two parts

# PART 1: BUILD A MAP

library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)   
library(leaflet) 
library(ggplot2) 
library("usmap")

### 2
# DoD for Midage 1970
colnames(normalized_dod)[2]="fips"
deathsdespair70=subset(normalized_dod, year=="1970")
A = plot_usmap(regions = "counties", data = deathsdespair70, values = "ndod_midlife",exclude=c("AK")) + 
  labs(title = "US Counties",
       subtitle = "Deaths of Despair by County (Ages 30-55) - Year 1970") + 
  scale_fill_stepsn(name="Deaths by 100k pop",breaks=c(10,20,40,60,80,100,120,140,160,180,200),
                    colours=c("lightgoldenrodyellow","orange","orange1","brown","darkred"),na.value="white") +
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))
# DoD for Midage 2019
colnames(normalized_dod)[2]="fips"
deathsdespair19=subset(normalized_dod, year=="2019")
B = plot_usmap(regions = "counties", data = deathsdespair19, values = "ndod_midlife",exclude=c("AK")) + 
  labs(title = "US Counties",
       subtitle = "Deaths of Despair by County (Ages 30-55) - Year 2019") + 
  scale_fill_stepsn(name="Deaths by 100k pop",breaks=c(10,20,40,60,80,100,120,140,160,180,200),
                    colours=c("lightgoldenrodyellow","orange","orange1","brown","darkred"),na.value="white") +
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

library("ggpubr")
ggarrange(A, B, labels = c("",""))

### 2
# Evolution of DoD by Educational Level
ggplot(data=total_means[21:50,], aes(x=year)) +
  geom_line(aes(y = mdod_hsdrop, colour = "High School Dropouts",linetype="High School Dropouts")) +
  geom_line(aes(y = mdod_hsgrad, colour = "Less than a College Degree",linetype="Less than a College Degree")) +
  geom_line(aes(y = mdod_somecol, colour = "College Graduates",linetype="College Graduates")) +
  labs(x="Year",
       y="",
       linetype = "",
       color = "") +
  scale_colour_manual(name="",
                      breaks = c("High School Dropouts", "Less than a College Degree", "College Graduates"),
                      values = c("red","blue", "black")) +
  scale_linetype_manual(breaks = c("High School Dropouts", "Less than a College Degree", "College Graduates"),
                        values = c("solid", "solid", "solid")) +
  ggtitle("Evolution of Deaths of Despair by Educational Level (1990-2019)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="bottom",
        plot.title = element_text(face = "bold", size = 12),
        legend.background = element_rect(fill = "white", size = 4, colour = "white"),
        axis.ticks = element_line(colour = "lightblue", size = 0.2),
        panel.grid.major = element_line(colour = "lightblue", size = 0.2),
        panel.grid.minor = element_blank())

# Evolution of DoD (Overall and) by Cause of Death
ggplot(data=total_means, aes(x=year)) +
  geom_line(aes(y = mdod, colour = "Total DoD",linetype="Total DoD")) +
  geom_line(aes(y = movds, colour = "Overdose",linetype="Overdose")) +
  geom_line(aes(y = malc, colour = "Alcoholic Liver Disease",linetype="Alcoholic Liver Disease")) +
  geom_line(aes(y = msuicide, colour = "Suicide",linetype="Suicide")) +
  labs(x="Year",
       y="",
       linetype = "",
       color = "") +
  scale_colour_manual(name="",
                      breaks = c("Total DoD", "Overdose", "Alcoholic Liver Disease", "Suicide"),
                      values = c("black","red", "orange", "blue")) +
  scale_linetype_manual(breaks = c("Total DoD", "Overdose", "Alcoholic Liver Disease", "Suicide"),
                        values = c("solid", "solid", "solid", "solid")) +
  ggtitle("Evolution of Deaths of Despair per 100k Population (1973-2019)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="bottom",
        plot.title = element_text(face = "bold", size = 12),
        legend.background = element_rect(fill = "white", size = 8, colour = "white"),
        axis.ticks = element_line(colour = "lightblue", size = 0.2),
        panel.grid.major = element_line(colour = "lightblue", size = 0.2),
        panel.grid.minor = element_blank())

# Evolution of DoD by Gender
A = ggplot(data=total_means, aes(x=year)) +
  geom_line(aes(y = mdod_male, colour = "Male",linetype="Male")) +
  geom_line(aes(y = mdod_female, colour = "Female",linetype="Female")) +
  labs(x="Year",
       y="",
       linetype = "",
       color = "") +
  scale_colour_manual(name="",
                      breaks = c("Male", "Female"),
                      values = c("blue", "red")) +
  scale_linetype_manual(breaks = c("Male", "Female"),
                        values = c("solid", "solid")) +
  ggtitle("Evolution of Deaths of Despair by Gender (1973-2019)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="bottom",
        plot.title = element_text(face = "bold", size = 12),
        legend.background = element_rect(fill = "white", size = 4, colour = "white"),
        axis.ticks = element_line(colour = "lightblue", size = 0.2),
        panel.grid.major = element_line(colour = "lightblue", size = 0.2),
        panel.grid.minor = element_blank())

# Evolution of DoD by Age Group
B = ggplot(data=total_means, aes(x=year)) +
  geom_line(aes(y = mdod_male_young, colour = "Male (15-29 years)",linetype="Male (15-29 years)")) +
  geom_line(aes(y = mdod_male_midlife, colour = "Male (30-54 years)",linetype="Male (30-54 years)")) +
  geom_line(aes(y = mdod_male_old, colour = "Male (+55 years)",linetype="Male (+55 years)")) +
  labs(x="Year",
       y="",
       linetype = "",
       color = "") +
  scale_colour_manual(name="",
                      breaks = c("Male (15-29 years)", "Male (30-54 years)", "Male (+55 years)"),
                      values = c("black","red", "blue")) +
  scale_linetype_manual(breaks = c("Male (15-29 years)", "Male (30-54 years)", "Male (+55 years)"),
                        values = c("solid", "solid", "solid")) +
  ggtitle("Evolution of Deaths of Despair for Male per Age Group (1973-2019)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="bottom",
        plot.title = element_text(face = "bold", size = 12),
        legend.background = element_rect(fill = "white", size = 4, colour = "white"),
        axis.ticks = element_line(colour = "lightblue", size = 0.2),
        panel.grid.major = element_line(colour = "lightblue", size = 0.2),
        panel.grid.minor = element_blank())

# Plot means analysis


ggplot(data=means_analysis_graph, aes(x=year_set)) +
  geom_line(aes(y = mdod_male_midlife0, colour = "Losers",linetype="Losers")) +
  geom_line(aes(y = mdod_male_midlife1, colour = "Winner",linetype="Winner")) +
  labs(x="Year",
       y="",
       linetype = "",
       color = "") +
  scale_colour_manual(name="",
                      breaks = c("Losers", "Winner"),
                      values = c("black","red")) +
  scale_linetype_manual(breaks = c("Losers", "Winner"),
                        values = c("solid", "solid")) +
  ggtitle("Evolution of Deaths of Despair for Male per Age Group (1973-2019)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="bottom",
        plot.title = element_text(face = "bold", size = 12),
        legend.background = element_rect(fill = "white", size = 4, colour = "white"),
        axis.ticks = element_line(colour = "lightblue", size = 0.2),
        panel.grid.major = element_line(colour = "lightblue", size = 0.2),
        panel.grid.minor = element_blank())


ggarrange(A, B, labels = c("","",""))



# PART 2: RETRIEVE OLD DATA FROM PDF FORMAT

#install.packages("stringr")
#library("stringr")
library("tabulizer")
library("tidyverse")
library(writexl)

# ALABAMA
# Alabama 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=9,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=10,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:19,])
colnames(y_b)=c("County","B")
df1_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","disability_workers","8","9","10","11"))
append=rbind(df1,df1_b)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Alabama_2003.xlsx")
#Alabama 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=7,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=8,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:9,])
colnames(y_b)=c("County","B")
df1_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","disability_workers","8","9","10","11"))
append=rbind(df1,df1_b)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Alabama_2002.xlsx")
#Alabama 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=7,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=8,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:9,])
colnames(y_b)=c("County","B")
df1_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","disability_workers","8","9","10","11"))
append=rbind(df1,df1_b)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Alabama_2001.xlsx")
#Alabama 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=10,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=11,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:6,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
append=rbind(final_data,final_data_b)
alabama_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(alabama_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Alabama_2000.xlsx")
#Alabama 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=7,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=8,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:6,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
append=rbind(final_data,final_data_b)
alabama_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(alabama_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Alabama_1999.xlsx")


# ALASKA
# Alaska 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=11,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:37,])
colnames(y)=c("B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Alaska_2003.xlsx")
#Alaska 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=9,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:35,])
colnames(y)=c("B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Alaska_2002.xlsx")
#Alaska 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=9,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:35,])
colnames(y)=c("B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Alaska_2001.xlsx")
#Alaska 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=12,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:32,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
alaska_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(alaska_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Alaska_2000.xlsx")
#Alaska 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=9,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:32,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
alaska_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(alaska_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Alaska_1999.xlsx")


# ARIZONA
# Arizona 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=12,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:23,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Arizona_2003.xlsx")
#Arizona 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=10,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:23,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Arizona_2002.xlsx")
#Arizona 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=10,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:23,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Arizona_2001.xlsx")
#Arizona 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=13,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:20,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
arizona_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(arizona_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Arizona_2000.xlsx")
#Arizona 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=10,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:20,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
arizona_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(arizona_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Arizona_1999.xlsx")


# ARKANSAS
# Arkansas 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=13,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=14,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:27,])
colnames(y_b)=c("County","B")
df1_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","disability_workers","8","9","10","11"))
append=rbind(df1,df1_b)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Arkansas_2003.xlsx")
#Arkansas 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=11,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=12,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:17,])
colnames(y_b)=c("County","B")
df1_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","disability_workers","8","9","10","11"))
append=rbind(df1,df1_b)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Arkansas_2002.xlsx")
#Arkansas 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=11,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=12,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:17,])
colnames(y_b)=c("County","B")
df1_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","disability_workers","8","9","10","11"))
library(writexl)
append=rbind(df1,df1_b)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Arkansas_2001.xlsx")
#Arkansas 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=14,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=15,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:14,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
arkansas_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(arkansas_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Arkansas_2000.xlsx")
#Arkansas 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=11,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=12,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:14,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
arkansas_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(arkansas_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Arkansas_1999.xlsx")


# CALIFORNIA
# California 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=15,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=16,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:10,])
colnames(y_b)=c("County","B")
df1_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","disability_workers","8","9","10","11"))
append=rbind(df1,df1_b)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/California_2003.xlsx")
#California 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=13,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:66,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(df1)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(df1,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/California_2002.xlsx")
#California 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=13,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:66,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(df1)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(df1,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/California_2001.xlsx")
#California 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=16,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:63,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
california_2000=data.frame(County=final_data[,1],disability_workers=final_data[,2])
write_xlsx(california_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/California_2000.xlsx")
#California 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=13,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:63,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
california_1999=data.frame(County=final_data[,1],disability_workers=final_data[,2])
write_xlsx(california_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/California_1999.xlsx")


# COLORADO
# Colorado 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=17,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("B")
x=data.frame(separate(y,B,sep=" ", into = c("A","1","2","3","4","5","6","7","8","9","10")))
colnames(x)=c("A","1", "2","3","4","5","6","7","8","9","10")
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=18,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:16,])
colnames(y_b)=c("A","B")
x_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10"))
colorado_2003=rbind(x,x_b)
colnames(colorado_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(colorado_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Colorado_2003.xlsx")
#Colorado 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=14,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[10:72,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(df1)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(df1,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Colorado_2002.xlsx")
#Colorado 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=14,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[10:72,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Colorado_2001.xlsx")
#Colorado 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=17,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:68,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
colorado_2000=data.frame(County=final_data[,1],disability_workers=final_data[,2])
write_xlsx(colorado_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Colorado_2000.xlsx")
#Colorado 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=14,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:68,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
colorado_1999=data.frame(County=final_data[,1],disability_workers=final_data[,2])
write_xlsx(colorado_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Colorado_1999.xlsx")


# CONNECTICUT
# Connecticut 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=19,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:16,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Connecticut_2003.xlsx")
#Connecticut 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=15,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:16,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Connecticut_2002.xlsx")
#Connecticut 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=15,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:16,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Connecticut_2001.xlsx")
#Connecticut 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=18,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:13,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
connecticut_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(connecticut_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Connecticut_2000.xlsx")
#Connecticut 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=15,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:13,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
connecticut_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(connecticut_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Connecticut_1999.xlsx")


# DELAWARE
# Delaware 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=20,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:11,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Delaware_2003.xlsx")
#Delaware 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=16,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:11,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Delaware_2002.xlsx")
#Delaware 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=16,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:11,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Delaware_2001.xlsx")
#Delaware 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=19,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:8,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
delaware_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(delaware_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Delaware_2000.xlsx")
#Delaware 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=16,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:8,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
delaware_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(delaware_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Delaware_1999.xlsx")



# FLORIDA
# Florida 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=21,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("A")
x=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11","B"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=22,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:19,])
colnames(y_b)=c("A","B")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
florida_2003=rbind(x,x_b)
colnames(florida_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(florida_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Florida_2003.xlsx")
#Florida 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=17,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("A")
x=separate(y,A,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=18,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:9,])
colnames(y_b)=c("County","B")
x_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","disability_workers","8","9","10","11"))
florida_2002=rbind(x,x_b)
colnames(florida_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(florida_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Florida_2002.xlsx")
#Florida 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=17,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=18,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:9,])
colnames(y_b)=c("County","B")
df1_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","disability_workers","8","9","10","11"))
append=rbind(df1,df1_b)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Florida_2001.xlsx")
#Florida 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=21,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=22,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:6,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
append=rbind(final_data,final_data_b)
florida_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(florida_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Florida_2000.xlsx")
#Florida 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=18,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=19,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:6,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
append=rbind(final_data,final_data_b)
florida_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(florida_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Florida_1999.xlsx")


# GEORGIA
# Georgia 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=23,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("A")
x=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=24,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:67,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#c
gpi_table_c=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=25,method="decide")
df_c=data.frame(gpi_table_c)
y_c=data.frame(df_c[8:51,])
colnames(y_c)=c("A")
x_c=separate(y_c,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
georgia_2003=rbind(x,x_b,x_c)
colnames(georgia_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(georgia_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Georgia_2003.xlsx")
#Georgia 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=19,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("A")
x=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=20,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:77,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#c
gpi_table_c=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=21,method="decide")
df_c=data.frame(gpi_table_c)
y_c=data.frame(df_c[8:31,])
colnames(y_c)=c("A")
x_c=separate(y_c,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
georgia_2002=rbind(x,x_b,x_c)
colnames(georgia_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(georgia_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Georgia_2002.xlsx")
#Georgia 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=19,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("A")
x=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
df1=data.frame(County=x[,1],disability_workers=x[,8])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=20,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:77,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
df1_b=data.frame(County=x_b[,1],disability_workers=x_b[,8])
#c
gpi_table_c=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=21,method="decide")
df_c=data.frame(gpi_table_c)
y_c=data.frame(df_c[8:31,])
colnames(y_c)=c("A")
x_c=separate(y_c,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
df1_c=data.frame(County=x_c[,1],disability_workers=x_c[,8])
georgia_2001=rbind(x,x_b,x_c)
colnames(georgia_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(georgia_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Georgia_2001.xlsx")
#Georgia 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=23,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=24,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:74,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
#c
gpi_table_c=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=25,method="decide")
df_c=data.frame(gpi_table_c)
y_c=data.frame(df_c[5:28,])
x_c=data.frame(County=gsub("\\.", "", y_c[,1]),wd=y_c[,5])
df1_c=separate(x_c,wd, sep=" ", into = c("disability_workers", "2"))
final_data_c=data.frame(County=df1_c[,1],disability_workers=df1_c[,2])
georgia_2000=rbind(final_data,final_data_b,final_data_c)
write_xlsx(georgia_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Georgia_2000.xlsx")
#Georgia 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=20,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=21,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:74,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
#c
gpi_table_c=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=22,method="decide")
df_c=data.frame(gpi_table_c)
y_c=data.frame(df_c[5:28,])
x_c=data.frame(County=gsub("\\.", "", y_c[,1]),wd=y_c[,5])
df1_c=separate(x_c,wd, sep=" ", into = c("disability_workers", "2"))
final_data_c=data.frame(County=df1_c[,1],disability_workers=df1_c[,2])
georgia_1999=rbind(final_data,final_data_b,final_data_c)
write_xlsx(georgia_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Georgia_1999.xlsx")



# HAWAII
# Hawaii 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=26,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:12,])
colnames(y)=c("A","B")
df1=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Hawaii_2003.xlsx")
#Hawaii 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=22,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:12,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Hawaii_2002.xlsx")
#Hawaii 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=22,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:12,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Hawaii_2001.xlsx")
#Hawaii 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=26,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:9,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
hawaii_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(hawaii_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Hawaii_2000.xlsx")
#Hawaii 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=23,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:10,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
hawaii_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(hawaii_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Hawaii_1999.xlsx")



# IDAHO
# Idaho 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=27,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:52,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Idaho_2003.xlsx")
#Idaho 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=23,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:52,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Idaho_2002.xlsx")
#Idaho 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=23,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:52,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Idaho_2001.xlsx")
#Idaho 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=27,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:49,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
idaho_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(idaho_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Idaho_2000.xlsx")
#Idaho 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=24,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:49,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
idaho_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(idaho_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Idaho_1999.xlsx")


# ILLINOIS
# Illinois 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=28,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("A","11")
x=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=29,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:54,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
illinois_2003=rbind(x,x_b)
colnames(illinois_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(illinois_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Illinois_2003.xlsx")
#Illinois 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=24,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=25,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:44,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
illinois_2002=rbind(x,x_b)
colnames(illinois_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(illinois_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Illinois_2002.xlsx")
#Illinois 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=24,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=25,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:44,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
illinois_2001=rbind(x,x_b)
colnames(illinois_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(illinois_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Illinois_2001.xlsx")
#Illinois 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=28,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=29,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:41,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
append=rbind(final_data,final_data_b)
illinois_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(illinois_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Illinois_2000.xlsx")
#Illinois 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=25,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=26,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:41,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
append=rbind(final_data,final_data_b)
illinois_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(illinois_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Illinois_1999.xlsx")


# INDIANA
# Indiana 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=30,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("A","12")
x=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=31,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:44,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11","12"))
indiana_2003=rbind(x,x_b)
colnames(indiana_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(indiana_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Indiana_2003.xlsx")
#Indiana 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=26,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=27,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:34,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
indiana_2002=rbind(x,x_b)
colnames(indiana_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(indiana_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Indiana_2002.xlsx")
#Indiana 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=26,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=27,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:34,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
indiana_2001=rbind(x,x_b)
colnames(indiana_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(indiana_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Indiana_2001.xlsx")
#Indiana 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=30,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=31,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:31,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
indiana_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(indiana_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Indiana_2000.xlsx")
#Indiana 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=27,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=28,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:31,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
indiana_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(indiana_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Indiana_1999.xlsx")



# IOWA
# Iowa 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=32,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("A","11")
x=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=33,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:51,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
iowa_2003=rbind(x,x_b)
colnames(iowa_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(iowa_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Iowa_2003.xlsx")
#Iowa 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=28,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=29,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:41,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
iowa_2002=rbind(x,x_b)
colnames(iowa_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(iowa_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Iowa_2002.xlsx")
#Iowa 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=28,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=29,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:41,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
iowa_2001=rbind(x,x_b)
colnames(iowa_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(iowa_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Iowa_2001.xlsx")
#Iowa 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=32,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=33,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:38,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
iowa_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(iowa_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Iowa_2000.xlsx")
#Iowa 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=29,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=30,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:38,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
iowa_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(iowa_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Iowa_1999.xlsx")



# KANSAS
# Kansas 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=34,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("A","11")
x=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=35,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:57,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
library(writexl)
kansas_2003=rbind(x,x_b)
colnames(kansas_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(kansas_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Kansas_2003.xlsx")
#Kansas 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=30,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=31,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:47,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
library(writexl)
kansas_2002=rbind(x,x_b)
colnames(kansas_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(kansas_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Kansas_2002.xlsx")
#Kansas 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=30,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
df1=data.frame(County=x[,1],disability_workers=x[,8])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=31,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:47,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
library(writexl)
kansas_2001=rbind(x,x_b)
colnames(kansas_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(kansas_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Kansas_2001.xlsx")
#Kansas 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=34,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=35,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:44,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
kansas_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(kansas_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Kansas_2000.xlsx")
#Kansas 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=31,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=32,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:44,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
kansas_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(kansas_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Kansas_1999.xlsx")



# KENTUCKY
# Kentucky 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=36,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=37,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:67,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
#c
gpi_table_c=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=38,method="decide")
df_c=data.frame(gpi_table_c)
y_c=data.frame(df_c[8:12,])
colnames(y_c)=c("1","B")
x_c=separate(y_c,B, sep=" ", into = c( "2","3","4","5","6","7","8","9","10","11"))
kentucky_2003=rbind(x,x_b,x_c)
colnames(kentucky_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(kentucky_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Kentucky_2003.xlsx")
#Kentucky 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=32,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=33,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:62,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
kentucky_2002=rbind(x,x_b)
colnames(kentucky_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(kentucky_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Kentucky_2002.xlsx")
#Kentucky 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=32,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=33,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:62,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
library(writexl)
kentucky_2001=rbind(x,x_b)
colnames(kentucky_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(kentucky_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Kentucky_2001.xlsx")
#Kentucky 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=36,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=37,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:59,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
kentucky_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(kentucky_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Kentucky_2000.xlsx")
#Kentucky 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=33,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=34,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:59,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
kentucky_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(kentucky_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Kentucky_1999.xlsx")




# LOUISIANA
# Louisiana 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=39,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=40,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[8:16,])
colnames(y)=c("1","B")
df1_b=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
append=data.frame(rbind(df1,df1_b))
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Louisiana_2003.xlsx")
#Louisiana 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=34,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:72,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Louisiana_2002.xlsx")
#Louisiana 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=34,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:72,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Louisiana_2001.xlsx")
#Louisiana 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=38,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:69,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
louisiana_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(louisiana_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Louisiana_2000.xlsx")
#Louisiana 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=35,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:69,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
louisiana_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(louisiana_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Louisiana_1999.xlsx")




# MAINE
# Maine 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=41,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:24,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Maine_2003.xlsx")
#Maine 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=35,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:24,])
colnames(y)=c("B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Maine_2002.xlsx")
#Maine 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=35,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:24,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Maine_2001.xlsx")
#Maine 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=39,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:21,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
maine_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(maine_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Maine_2000.xlsx")
#Maine 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=36,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:21,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
maine_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(maine_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Maine_1999.xlsx")




# MARYLAND
# Maryland 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=42,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:32,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Maryland_2003.xlsx")
#Maryland 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=36,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:32,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Maryland_2002.xlsx")
#Maryland 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=36,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:32,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Maryland_2001.xlsx")
#Maryland 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=40,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:29,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
maryland_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(maryland_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Maryland_2000.xlsx")
#Maryland 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=37,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:29,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
maryland_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(maryland_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Maryland_1999.xlsx")



# MASSACHUSETTS
# Massachusetts 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=43,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[10:23,])
colnames(y)=c("B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Massachusetts_2003.xlsx")
#Massachusetts 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=37,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:22,])
colnames(y)=c("B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Massachusetts_2002.xlsx")
#Massachusetts 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=37,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:22,])
colnames(y)=c("A")
df1=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Massachusetts_2001.xlsx")
#Massachusetts 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=41,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:19,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
massachusetts_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(massachusetts_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Massachusetts_2000.xlsx")
#Massachusetts 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=38,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:19,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
massachusetts_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(massachusetts_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Massachusetts_1999.xlsx")



#MICHIGAN
# Michigan 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=44,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=45,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:35,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
michigan_2003=rbind(x,x_b)
colnames(michigan_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(michigan_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Michigan_2003.xlsx")
#Michigan 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=38,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
df1=data.frame(County=x[,1],disability_workers=x[,8])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=39,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:25,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
michigan_2002=rbind(x,x_b)
colnames(michigan_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(michigan_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Michigan_2002.xlsx")
#Michigan 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=38,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=39,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:25,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
michigan_2001=rbind(x,x_b)
colnames(michigan_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(michigan_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Michigan_2001.xlsx")
#Michigan 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=42,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=43,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:22,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
michigan_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(michigan_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Michigan_2000.xlsx")
#Michigan 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=39,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=40,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:22,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
michigan_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(michigan_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Michigan_1999.xlsx")



#MINNESOTA
# Minnesota 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=46,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=47,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:39,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
minnesota_2003=rbind(x,x_b)
colnames(minnesota_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(minnesota_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Minnesota_2003.xlsx")
#Minnesota 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=40,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=41,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:29,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
minnesota_2002=rbind(x,x_b)
colnames(minnesota_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(minnesota_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Minnesota_2002.xlsx")
#Minnesota 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=40,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=41,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:29,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
minnesota_2001=rbind(x,x_b)
colnames(minnesota_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(minnesota_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Minnesota_2001.xlsx")
#Minnesota 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=44,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=45,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:26,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
minnesota_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(minnesota_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Minnesota_2000.xlsx")
#Mineesota 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=41,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=42,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:26,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
minnesota_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(minnesota_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Minnesota_1999.xlsx")




#MISSISSIPPI
# Mississippi 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=48,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=49,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:34,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
mississippi_2003=rbind(x,x_b)
colnames(mississippi_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(mississippi_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Mississippi_2003.xlsx")
#Mississippi 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=42,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","disability_workers","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=43,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:24,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","disability_workers","8","9","10","11"))
mississippi_2002=rbind(x,x_b)
colnames(mississippi_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(mississippi_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Mississippi_2002.xlsx")
#Mississippi 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=42,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","disability_workers","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=43,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:24,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","disability_workers","8","9","10","11"))
mississippi_2001=rbind(x,x_b)
colnames(mississippi_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(mississippi_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Mississippi_2001.xlsx")
#Mississippi 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=46,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=47,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:21,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
mississippi_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(mississippi_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Mississippi_2000.xlsx")
#Mississippi 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=43,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=44,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:21,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
mississippi_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(mississippi_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Mississippi_1999.xlsx")



#MISSOURI
# Missouri 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=50,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:68,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=51,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:62,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
missouri_2003=rbind(x,x_b)
colnames(missouri_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(missouri_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Missouri_2003.xlsx")
#Mississippi 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=44,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","disability_workers","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=45,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:57,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","disability_workers","8","9","10","11"))
missouri_2002=rbind(x,x_b)
colnames(missouri_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(missouri_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Missouri_2002.xlsx")
#Mississippi 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=44,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","disability_workers","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=45,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:57,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","disability_workers","8","9","10","11"))
missouri_2001=rbind(x,x_b)
colnames(missouri_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(missouri_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Missouri_2001.xlsx")
#Mississippi 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=48,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=49,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:54,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
append=rbind(final_data,final_data_b)
missouri_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(missouri_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Missouri_2000.xlsx")
#Mississippi 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=45,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=46,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:54,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
append=rbind(final_data,final_data_b)
missouri_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(missouri_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Missouri_1999.xlsx")



# MONTANA
# Montana 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=52,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:64,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Montana_2003.xlsx")
#Montana 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=46,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:64,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Montana_2002.xlsx")
#Montana 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=46,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:64,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Montana_2001.xlsx")
#Montana 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=50,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:61,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
montana_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(montana_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Montana_2000.xlsx")
#Montana 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=47,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:61,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
montana_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(montana_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Montana_1999.xlsx")




#NEBRASKA
# Nebraska 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=53,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
df1=data.frame(County=x[,1],disability_workers=x[,8])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=54,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:45,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
nebraska_2003=rbind(x,x_b)
colnames(nebraska_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(nebraska_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Nebraska_2003.xlsx")
#Nebraska 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=47,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=48,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:35,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","disability_workers","8","9","10","11"))
nebraska_2002=rbind(x,x_b)
colnames(nebraska_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(nebraska_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Nebraska_2002.xlsx")
#Nebraska 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=47,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=48,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:35,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
nebraska_2001=rbind(x,x_b)
colnames(nebraska_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(nebraska_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Nebraska_2001.xlsx")
#Nebraska 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=51,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=52,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:32,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
nebraska_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(nebraska_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Nebraska_2000.xlsx")
#Nebraska 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=48,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=49,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:32,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
nebraska_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(nebraska_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Nebraska_1999.xlsx")



# NEVADA
# Nevada 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=55,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:25,])
colnames(y)=c("B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Nevada_2003.xlsx")
#Nevada 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=49,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:25,])
colnames(y)=c("B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Nevada_2002.xlsx")
#Nevada 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=49,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:25,])
colnames(y)=c("B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Nevada_2001.xlsx")
#Nevada 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=53,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:22,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
nevada_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(nevada_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Nevada_2000.xlsx")
#Nevada 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=50,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:22,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
nevada_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(nevada_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Nevada_1999.xlsx")



# NEW HAMPSHIRE
# New Hampshire 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=56,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[10:19,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/New_Hampshire_2003.xlsx")
#New Hampshire 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=50,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:18,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/New_Hampshire_2002.xlsx")
#New Hampshire 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=50,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:18,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/New_Hampshire_2001.xlsx")
#New Hampshire 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=54,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:15,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
new_hampshire_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(new_hampshire_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/New_Hampshire_2000.xlsx")
#New Hampshire 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=51,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:15,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
new_hampshire_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(new_hampshire_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/New_Hampshire_1999.xlsx")




# NEW JERSEY
# New Jersey 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=57,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:29,])
colnames(y)=c("B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/New_Jersey_2003.xlsx")
#New Jersey 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=51,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:29,])
colnames(y)=c("B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/New_Jersey_2002.xlsx")
#New Jersey 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=51,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:29,])
colnames(y)=c("B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/New_Jersey_2001.xlsx")
#New Jersey 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=55,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:26,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
new_jersey_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(new_jersey_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/New_Jersey_2000.xlsx")
#New Jersey 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=52,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:26,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
new_jersey_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(new_jersey_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/New_Jersey_1999.xlsx")




# NEW MEXICO
# New Mexico 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=58,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:41,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/New_Mexico_2003.xlsx")
#New Mexico 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=52,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:41,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/New_Mexico_2002.xlsx")
#New Mexico 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=52,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:41,])
colnames(y)=c("A","B")
df1=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","disability_workers","8","9","10","11"))
append=data.frame(df1)
colnames(append)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(append,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/New_Mexico_2001.xlsx")
#New Mexico 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=56,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:38,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
new_mexico_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(new_mexico_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/New_Mexico_2000.xlsx")
#New Mexico 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=53,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:38,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
new_mexico_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(new_mexico_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/New_Mexico_1999.xlsx")



#NEW YORK
# New York 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=59,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=60,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:14,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
new_york_2003=rbind(x,x_b)
colnames(new_york_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(new_york_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/New_York_2003.xlsx")
#New York 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=53,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:70,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/New_York_2002.xlsx")
#New York 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=53,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:70,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/New_York_2001.xlsx")
#New York 2000
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=57,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:67,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
new_york_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(new_york_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/New_York_2000.xlsx")
#New York 1999
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=54,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:67,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
new_york_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(new_york_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/New_York_1999.xlsx")





#NORTH CAROLINA
# North Carolina 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=61,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[10:64,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=62,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:52,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
north_carolina_2003=rbind(x,x_b)
colnames(north_carolina_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(north_carolina_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/North_Carolina_2003.xlsx")
#North Carolina 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=54,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","disability_workers","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=55,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:42,])
colnames(y_b)=c("B")
x_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","disability_workers","8","9","10","11"))
north_carolina_2002=rbind(x,x_b)
colnames(north_carolina_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(north_carolina_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/North_Carolina_2002.xlsx")
#North Carolina 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=54,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=55,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:42,])
colnames(y_b)=c("B")
x_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
north_carolina_2001=rbind(x,x_b)
colnames(north_carolina_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(north_carolina_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/North_Carolina_2001.xlsx")
#North Carolina 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=58,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=59,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:39,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
north_carolina_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(north_carolina_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/North_Carolina_2000.xlsx")
#North Carolina 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=55,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=56,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:39,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
north_carolina_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(north_carolina_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/North_Carolina_1999.xlsx")


#NORTH DAKOTA
# North Dakota 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=63,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[10:62,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/North_Dakota_2003.xlsx")
#North Dakota 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=56,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:61,])
colnames(y)=c("A","B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/North_Dakota_2002.xlsx")
#North Dakota 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=56,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:61,])
colnames(y)=c("A","B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/North_Dakota_2001.xlsx")
#North Dakota 2000
gpi_table=extract_tables("//Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=60,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:58,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
north_dakota_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
library(writexl)
write_xlsx(north_dakota_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/North_Dakota_2000.xlsx")
#North Dakota 1999
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=57,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:58,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
north_dakota_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(north_dakota_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/North_Dakota_1999.xlsx")



#OHIO
# Ohio 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=64,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("A","11")
x=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=65,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:40,])
colnames(y_b)=c("B")
x_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
ohio_2003=rbind(x,x_b)
colnames(ohio_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(ohio_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Ohio_2003.xlsx")
#Ohio 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=57,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=58,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:30,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
ohio_2002=rbind(x,x_b)
colnames(ohio_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(ohio_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Ohio_2002.xlsx")
#Ohio 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=57,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=58,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:30,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
ohio_2001=rbind(x,x_b)
colnames(ohio_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(ohio_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Ohio_2001.xlsx")
#Ohio 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=61,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=62,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:27,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
append=rbind(final_data,final_data_b)
ohio_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(ohio_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Ohio_2000.xlsx")
#Ohio 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=58,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=59,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:27,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
ohio_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(ohio_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Ohio_1999.xlsx")




#OKLAHOMA
# Oklahoma 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=66,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=67,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:29,])
colnames(y_b)=c("B")
x_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
oklahoma_2003=rbind(x,x_b)
colnames(oklahoma_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(oklahoma_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Oklahoma_2003.xlsx")
#Oklahoma 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=59,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
df1=data.frame(County=x[,1],disability_workers=x[,8])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=60,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:19,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
oklahoma_2002=rbind(x,x_b)
colnames(oklahoma_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(oklahoma_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Oklahoma_2002.xlsx")
#Oklahoma 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=59,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=60,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:19,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
oklahoma_2001=rbind(x,x_b)
colnames(oklahoma_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(oklahoma_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Oklahoma_2001.xlsx")
#Oklahoma 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=63,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=64,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:16,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
oklahoma_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(oklahoma_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Oklahoma_2000.xlsx")
#Oklahoma 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=60,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=61,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:16,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
oklahoma_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(oklahoma_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Oklahoma_1999.xlsx")





#OREGON
# Oregon 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=68,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:44,])
colnames(y)=c("A","B")
x=separate(y,A,sep=" ", into = c("County","1","2","3","4","5","6","7","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Oregon_2003.xlsx")
#Oregon 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=61,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:44,])
colnames(y)=c("A")
x=separate(y,A,sep=" ", into = c("County","1","2","3","4","5","6","7","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Oregon_2002.xlsx")
#Oregon 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=61,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:44,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","7","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Oregon_2001.xlsx")
#Oregon 2000
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=65,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:41,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
oregon_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
library(writexl)
write_xlsx(oregon_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Oregon_2000.xlsx")
#Oregon 1999
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=62,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:41,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
oregon_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(oregon_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Oregon_1999.xlsx")




#PENNSYLVANIA
# Pennsylvania 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=69,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[10:64,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=70,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:19,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
pennsylvania_2003=rbind(x,x_b)
colnames(pennsylvania_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(pennsylvania_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Pennsylvania_2003.xlsx")
#Pennsylvania 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=62,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=63,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:9,])
colnames(y_b)=c("B")
x_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
pennsylvania_2002=rbind(x,x_b)
colnames(pennsylvania_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(pennsylvania_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Pennsylvania_2002.xlsx")
#Pennsylvania 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=62,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=63,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:9,])
colnames(y_b)=c("B")
x_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
pennsylvania_2001=rbind(x,x_b)
colnames(pennsylvania_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(pennsylvania_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Pennsylvania_2001.xlsx")
#Pennsylvania 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=66,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=67,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:6,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
append=rbind(final_data,final_data_b)
pennsylvania_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(pennsylvania_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Pennsylvania_2000.xlsx")
#Pennsylvania 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=63,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=64,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:6,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
append=rbind(final_data,final_data_b)
pennsylvania_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(pennsylvania_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Pennsylvania_1999.xlsx")



#RHDDE ISLAND
# Rhode Island 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=71,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[10:14,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","7","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Rhode_Island_2003.xlsx")
#Rhode Island 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=64,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:13,])
colnames(y)=c("A","B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","7","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Rhode_Island_2002.xlsx")
#Rhode Island 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=64,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:13,])
colnames(y)=c("A","B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","7","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Rhode_Island_2001.xlsx")
#Rhode Island 2000
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=70,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:10,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
Rhode_Island_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
library(writexl)
write_xlsx(Rhode_Island_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Rhode_Island_2000.xlsx")
#Rhode Island 1999
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=67,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:10,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
Rhode_Island_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(Rhode_Island_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Rhode_Island_1999.xlsx")


#SOUTH CAROLINA
# South Carolina 2003
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=72,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[10:55,])
colnames(y)=c("A","B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","7","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/South_Carolina_2003.xlsx")
#South Carolina 2002
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=65,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:54,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","7","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/South_Carolina_2002.xlsx")
#South Carolina 2001
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=65,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:54,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","7","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/South_Carolina_2001.xlsx")
#South Carolina 2000
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=71,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:51,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
South_Carolina_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(South_Carolina_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/South_Carolina_2000.xlsx")
#South Carolina 1999
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=68,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:51,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
South_Carolina_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(South_Carolina_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/South_Carolina_1999.xlsx")




#SOUTH DAKOTA
# South Dakota 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=73,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[10:64,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=74,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:18,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
South_Dakota_2003=rbind(x,x_b)
colnames(South_Dakota_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(South_Dakota_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/South_Dakota_2003.xlsx")
#South Dakota 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=66,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:68,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=67,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:13,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
South_Dakota_2002=rbind(x,x_b)
colnames(South_Dakota_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(South_Dakota_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/South_Dakota_2002.xlsx")
#South Dakota 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=66,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:72,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
df1=data.frame(County=x[,1],disability_workers=x[,8])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=67,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:9,])
colnames(y_b)=c("B")
x_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
library(writexl)
South_Dakota_2001=rbind(x,x_b)
colnames(South_Dakota_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(South_Dakota_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/South_Dakota_2001.xlsx")
#South Dakota 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=72,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:69,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=73,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:6,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
South_Dakota_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(South_Dakota_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/South_Dakota_2000.xlsx")
#South Dakota 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=69,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=70,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:6,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
South_Dakota_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(South_Dakota_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/South_Dakota_1999.xlsx")



#TENNESSEE
# Tennessee 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=75,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=76,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:47,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
Tennessee_2003=rbind(x,x_b)
colnames(Tennessee_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(Tennessee_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Tennessee_2003.xlsx")
#Tennessee 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=68,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","disability_workers","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=69,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:37,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","disability_workers","8","9","10","11"))
Tennessee_2002=rbind(x,x_b)
colnames(Tennessee_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(Tennessee_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Tennessee_2002.xlsx")
#Tennessee 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=68,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=69,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:37,])
colnames(y_b)=c("B")
x_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
Tennessee_2001=rbind(x,x_b)
colnames(Tennessee_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(Tennessee_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Tennessee_2001.xlsx")
#Tennessee 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=74,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=75,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:34,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
Tennessee_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(Tennessee_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Tennessee_2000.xlsx")
#Tennessee 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=71,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=72,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:34,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
Tennessee_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(Tennessee_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Tennessee_1999.xlsx")



# TEXAS
# Texas 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=77,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("A")
x=separate(y,A,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=78,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:67,])
colnames(y_b)=c("A","11")
x_b=separate(y_b,A, sep=" ", into = c("1","2","3","4","5","6","7","8","9","10"))
#c
gpi_table_c=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=79,method="decide")
df_c=data.frame(gpi_table_c)
y_c=data.frame(df_c[8:67,])
colnames(y_c)=c("A")
x_c=separate(y_c,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
#d
gpi_table_d=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=80,method="decide")
df_d=data.frame(gpi_table_d)
y_d=data.frame(df_d[8:67,])
colnames(y_d)=c("A","11")
x_d=separate(y_d,A, sep=" ", into = c("1","2","3","4","5","6","7","8","9","10"))
#e
gpi_table_e=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=81,method="decide")
df_e=data.frame(gpi_table_e)
y_e=data.frame(df_e[8:26,])
colnames(y_e)=c("B")
x_e=separate(y_e,B, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
Texas_2003=rbind(x,x_b,x_c,x_d,x_e)
colnames(Texas_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(Texas_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Texas_2003.xlsx")
#Texas 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=70,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=71,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:72,])
colnames(y_b)=c("B")
x_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
#c
gpi_table_c=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=72,method="decide")
df_c=data.frame(gpi_table_c)
y_c=data.frame(df_c[8:72,])
colnames(y_c)=c("1","B")
x_c=separate(y_c,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#d
gpi_table_d=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=73,method="decide")
df_d=data.frame(gpi_table_d)
y_d=data.frame(df_d[8:66,])
colnames(y_d)=c("B")
x_d=separate(y_d,B, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
Texas_2002=rbind(x,x_b,x_c,x_d)
colnames(Texas_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(Texas_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Texas_2002.xlsx")

#Texas 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=70,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=71,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:72,])
colnames(y_b)=c("B")
x_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
#c
gpi_table_c=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=72,method="decide")
df_c=data.frame(gpi_table_c)
y_c=data.frame(df_c[8:72,])
colnames(y_c)=c("1","B")
x_c=separate(y_c,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#d
gpi_table_d=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=73,method="decide")
df_d=data.frame(gpi_table_d)
y_d=data.frame(df_d[8:66,])
colnames(y_d)=c("A")
x_d=separate(y_d,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
Texas_2001=rbind(x,x_b,x_c,x_d)
colnames(Texas_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(Texas_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Texas_2001.xlsx")
#Texas 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=76,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=77,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:69,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
#c
gpi_table_c=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=78,method="decide")
df_c=data.frame(gpi_table_c)
y_c=data.frame(df_c[5:69,])
x_c=data.frame(County=gsub("\\.", "", y_c[,1]),wd=y_c[,5])
df1_c=separate(x_c,wd, sep=" ", into = c("disability_workers", "2"))
final_data_c=data.frame(County=df1_c[,1],disability_workers=df1_c[,2])
#d
gpi_table_d=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=79,method="decide")
df_d=data.frame(gpi_table_d)
y_d=data.frame(df_d[5:63,])
x_d=data.frame(County=gsub("\\.", "", y_d[,1]),wd=y_d[,5])
df1_d=separate(x_d,wd, sep=" ", into = c("disability_workers", "2"))
final_data_d=data.frame(County=df1_d[,1],disability_workers=df1_d[,2])
library(writexl)
Texas_2000=rbind(final_data,final_data_b,final_data_c,final_data_d)
write_xlsx(Texas_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Texas_2000.xlsx")
#Texas 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=73,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=74,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:69,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
#c
gpi_table_c=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=75,method="decide")
df_c=data.frame(gpi_table_c)
y_c=data.frame(df_c[5:69,])
x_c=data.frame(County=gsub("\\.", "", y_c[,1]),wd=y_c[,5])
df1_c=separate(x_c,wd, sep=" ", into = c("disability_workers", "2"))
final_data_c=data.frame(County=df1_c[,1],disability_workers=df1_c[,2])
#d
gpi_table_d=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=76,method="decide")
df_d=data.frame(gpi_table_d)
y_d=data.frame(df_d[5:63,])
x_d=data.frame(County=gsub("\\.", "", y_d[,1]),wd=y_d[,5])
df1_d=separate(x_d,wd, sep=" ", into = c("disability_workers", "2"))
final_data_d=data.frame(County=df1_d[,1],disability_workers=df1_d[,2])
library(writexl)
Texas_1999=rbind(final_data,final_data_b,final_data_c,final_data_d)
write_xlsx(Texas_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Texas_1999.xlsx")




#RHDDE ISLAND
# Rhode Island 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=71,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[10:14,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","7","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Rhode_Island_2003.xlsx")
#Rhode Island 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=64,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:13,])
colnames(y)=c("A","B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","7","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Rhode_Island_2002.xlsx")
#Rhode Island 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=64,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:13,])
colnames(y)=c("A","B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","7","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Rhode_Island_2001.xlsx")
#Rhode Island 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=70,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:10,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
Rhode_Island_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(Rhode_Island_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Rhode_Island_2000.xlsx")
#Rhode Island 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=67,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:10,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
Rhode_Island_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(Rhode_Island_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Rhode_Island_1999.xlsx")


#UTAH
# Utah 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=82,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:37,])
colnames(y)=c("A","B")
x=separate(y,A,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Utah_2003.xlsx")
#Utah 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=74,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:37,])
colnames(y)=c("A","B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Utah_2002.xlsx")
#Utah 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=74,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:37,])
colnames(y)=c("A","B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Utah_2001.xlsx")
#Utah 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=80,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:34,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
Utah_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(Utah_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Utah_2000.xlsx")
#Utah 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=77,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:34,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
Utah_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(Utah_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Utah_1999.xlsx")




#VERMONT
# Vermont 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=83,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:22,])
colnames(y)=c("A")
x=separate(y,A,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Vermont_2003.xlsx")
#Vermont 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=75,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:22,])
colnames(y)=c("A","B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Vermont_2002.xlsx")
#Vermont 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=75,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:22,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Vermont_2001.xlsx")
#Vermont 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=81,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:19,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
Vermont_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
library(writexl)
write_xlsx(Vermont_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Vermont_2000.xlsx")
#Vermont 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=78,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:19,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
Vermont_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(Vermont_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Vermont_1999.xlsx")




#VIRGINIA
# Virginia 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=84,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=85,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:67,])
colnames(y_b)=c("B")
x_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
#c
gpi_table_c=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=86,method="decide")
df_c=data.frame(gpi_table_c)
y_c=data.frame(df_c[8:27,])
colnames(y_c)=c("1","B")
x_c=separate(y_c,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
Virginia_2003=rbind(x,x_b,x_c)
colnames(Virginia_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(Virginia_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Virginia_2003.xlsx")
#Virginia 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=76,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=77,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:72,])
colnames(y_b)=c("A")
x_b=separate(y_b,A, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
#c
gpi_table_c=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=78,method="decide")
df_c=data.frame(gpi_table_c)
y_c=data.frame(df_c[8:12,])
colnames(y_c)=c("1","B")
x_c=separate(y_c,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
Virginia_2002=rbind(x,x_b,x_c)
colnames(Virginia_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(Virginia_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Virginia_2002.xlsx")
#Virginia 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=76,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("1","B")
x=separate(y,B,sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=77,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:72,])
colnames(y_b)=c("B")
x_b=separate(y_b,B, sep=" ", into = c("1", "2","3","4","5","6","7","8","9","10","11"))
#c
gpi_table_c=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=78,method="decide")
df_c=data.frame(gpi_table_c)
y_c=data.frame(df_c[8:12,])
colnames(y_c)=c("1","B")
x_c=separate(y_c,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
Virginia_2001=rbind(x,x_b,x_c)
colnames(Virginia_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(Virginia_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Virginia_2001.xlsx")
#Virginia 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=83,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=84,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:70,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
#c
gpi_table_c=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=85,method="decide")
df_c=data.frame(gpi_table_c)
y_c=data.frame(df_c[5:8,])
x_c=data.frame(County=gsub("\\.", "", y_c[,1]),wd=y_c[,5])
df1_c=separate(x_c,wd, sep=" ", into = c("disability_workers", "2"))
final_data_c=data.frame(County=df1_c[,1],disability_workers=df1_c[,2])
library(writexl)
append=rbind(final_data,final_data_b,final_data_c)
Virginia_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(Virginia_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Virginia_2000.xlsx")
#Virginia 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=80,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=81,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:70,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
#c
gpi_table_c=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=82,method="decide")
df_c=data.frame(gpi_table_c)
y_c=data.frame(df_c[5:8,])
x_c=data.frame(County=gsub("\\.", "", y_c[,1]),wd=y_c[,5])
df1_c=separate(x_c,wd, sep=" ", into = c("disability_workers", "2"))
final_data_c=data.frame(County=df1_c[,1],disability_workers=df1_c[,2])
library(writexl)
append=rbind(final_data,final_data_b,final_data_c)
Virginia_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(Virginia_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Virginia_1999.xlsx")



#WASHINGTON
# Washington 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=87,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:47,])
colnames(y)=c("A")
x=separate(y,A,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Washington_2003.xlsx")
#Washington 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=79,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:47,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Washington_2002.xlsx")
#Washington 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=79,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:47,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Washington_2001.xlsx")
#Washington 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=86,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:44,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
Washington_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
library(writexl)
write_xlsx(Washington_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Washington_2000.xlsx")
#Washington 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=83,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:44,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
Washington_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(Washington_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Washington_1999.xlsx")




#WEST VIRGINIA
# Washington 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=88,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[10:64,])
colnames(y)=c("A","B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/West_Virginia_2003.xlsx")
#West Virginia 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=80,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("A","B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/West_Virginia_2002.xlsx")
#West Virginia 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=80,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("A","B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/West_Virginia_2001.xlsx")
#West Virginia 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=87,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:60,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
West_Virginia_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
library(writexl)
write_xlsx(West_Virginia_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/West_Virginia_2000.xlsx")
#West Virginia 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=84,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:60,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
West_Virginia_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(West_Virginia_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/West_Virginia_1999.xlsx")



#WISCONSIN
# Wisconsin 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=89,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:63,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=90,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:24,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
Wisconsin_2003=rbind(x,x_b)
colnames(Wisconsin_2003)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(Wisconsin_2003,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Wisconsin_2003.xlsx")
#Wisconsin 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=81,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=82,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:14,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
Wisconsin_2002=rbind(x,x_b)
colnames(Wisconsin_2002)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(Wisconsin_2002,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Wisconsin_2002.xlsx")
#Wisconsin 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=81,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:73,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("1","2","3","4","5","6","7","8","9","10","11"))
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=82,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[8:14,])
colnames(y_b)=c("1","B")
x_b=separate(y_b,B, sep=" ", into = c("2","3","4","5","6","7","8","9","10","11"))
Wisconsin_2001=rbind(x,x_b)
colnames(Wisconsin_2001)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(Wisconsin_2001,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Wisconsin_2001.xlsx")
#Wisconsin 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=88,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=89,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:11,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
Wisconsin_2000=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(Wisconsin_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Wisconsin_2000.xlsx")
#Wisconsin 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=85,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:70,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
final_data=data.frame(County=df1[,1],disability_workers=df1[,2])
#b
gpi_table_b=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=86,method="decide")
df_b=data.frame(gpi_table_b)
y_b=data.frame(df_b[5:11,])
x_b=data.frame(County=gsub("\\.", "", y_b[,1]),wd=y_b[,5])
df1_b=separate(x_b,wd, sep=" ", into = c("disability_workers", "2"))
final_data_b=data.frame(County=df1_b[,1],disability_workers=df1_b[,2])
library(writexl)
append=rbind(final_data,final_data_b)
Wisconsin_1999=data.frame(County=append[,1],disability_workers=append[,2])
write_xlsx(Wisconsin_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Wisconsin_1999.xlsx")




#WYOMING
# Wyoming 2003
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2003.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=91,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:31,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2003/Wyoming_2003.xlsx")
#Wyoming 2002
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2002.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=83,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:31,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2002/Wyoming_2002.xlsx")
#Wyoming 2001
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2001.pdf",check.rows="TRUE",fix.empty.names="TRUE",check.columns="TRUE",guess="TRUE",pages=83,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[9:31,])
colnames(y)=c("B")
x=separate(y,B,sep=" ", into = c("County","1","2","3","4","5","6","disability_workers","8","9","10","11"))
colnames(x)=(c("1","2","3","4","5","6","7","8","9","10","11"))
write_xlsx(x,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2001/Wyoming_2001.xlsx")
#Wyoming 2000
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc2000.pdf",pages=90,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:28,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
Wyoming_2000=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(Wyoming_2000,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/2000/Wyoming_2000.xlsx")
#Wyoming 1999
#a
gpi_table=extract_tables("/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/oasdi_sc1999.pdf",pages=87,method="decide")
df=data.frame(gpi_table)
y=data.frame(df[6:28,])
x=data.frame(County=gsub("\\.", "", y[,1]),wd=y[,5])
df1=separate(x,wd, sep=" ", into = c("disability_workers", "2"))
Wyoming_1999=data.frame(County=df1[,1],disability_workers=df1[,2])
write_xlsx(Wyoming_1999,"/Users/carmenarbaizar/Dropbox/WorkWithCarmen/data/Health data/Disability Data/disability_workers/raw_disability_data/raw_intermediate_disability_data/1999/Wyoming_1999.xlsx")







