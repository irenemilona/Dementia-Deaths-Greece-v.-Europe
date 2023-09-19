# Install the appropriate libraries
#install.packages("openxlsx")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("extrafont")
#install.packages("ggalt")
#install.packages("ggthemes")
#install.packages("ggpubr")
#install.packages("ggrepel")
#install.packages("patchwork")
#install.packages("lemon")
#install.packages("writexl")

# Load the libraries
library(openxlsx)
library(ggplot2)
library(dplyr)
library(extrafont)
library(ggalt)
library(ggthemes)
library(ggpubr)
library(ggrepel)
library(patchwork)
library(lemon)
library(writexl)

# Load the dementia deaths file
dementia_deaths <- read.xlsx("C:\\Users\\irene\\Desktop\\3rd_Semester\\Data Visualization and Communication\\Project\\dementia deaths.xlsx", detectDates = TRUE)
dementia_deaths<-as.data.frame(dementia_deaths)

# Remove the columns we do not need
dementia_deaths <- dementia_deaths[,-5]

# Rename the column OBS_VALUE to ddeaths (dementia deaths) and TIME_PERIOD to year
dementia_deaths <- dementia_deaths %>% rename('ddeaths' = 'OBS_VALUE') %>% rename('year' = 'TIME_PERIOD')
#View(dementia_deaths)

# Load the dementia deaths by age data (less than 65, greater than 65)
age_data <- age_data <- read.csv("C:\\Users\\irene\\Desktop\\3rd_Semester\\Data Visualization and Communication\\Project\\age.csv", header=TRUE)
age_data <- as.data.frame(age_data)

# Remove the columns we do not need
age_data <- age_data[,-c(1:4,7,8,12)]

# Create a new categorical column for the age data
# 1 is for those who dead from dementia and their age where greater than 65
# 2 is for those who dead from dementia and their age where less than 65
age_data['age_numeric']<- as.numeric(factor(age_data$age))
#View(age_data)

# Rename the column OBS_VALUE to ddeaths_per_age (dementia deaths per age) and TIME_PERIOD to year
age_data <- age_data  %>% rename('ddeaths_per_age' = 'OBS_VALUE') %>% rename('year' = 'TIME_PERIOD')
#View(age_data )

# Load the total deaths data per country and year
total_deaths <- read.csv("C:\\Users\\irene\\Desktop\\3rd_Semester\\Data Visualization and Communication\\Project\\total_deaths.csv", header=TRUE)
total_deaths <- as.data.frame(total_deaths)
#View(total_deaths)

# Remove the columns we do not need
total_deaths <- total_deaths[,-c(1:4,8)]

# We observe that from the line 578 till the end instead of the absolute value of total deaths we have the rate of them
# Since there is no need to keep the rate for our analysis we decided to drop these lines
total_deaths<-total_deaths[1:577, ]

# Rename the column OBS_VALUE to tdeaths (total deaths) and TIME_PERIOD to year
total_deaths <- total_deaths  %>% rename('tdeaths' = 'OBS_VALUE') %>% rename('year' = 'TIME_PERIOD') 
#View(total_deaths)

# Merge the above dataframes
# Merge dementia_deaths & age_data
merged_data1 <- left_join(dementia_deaths,age_data, by = c("geo","year","sex"))
#View(merged_data1)

# Merge merged_data1 & total_deaths
merged_data2 <- left_join(merged_data1,total_deaths, by = c("geo","year"))
#View(merged_data2)

# Frequency table for the year
table(merged_data2$year)

# We observe that we have some records for the year 2021. Since our interest based only for the years between 2011-2020 we drop the year 2021 from our dataframe
# The merged_data dataframe contains information only for the years 2011-2020 
merged_data<-merged_data2[merged_data2$year != "2021", ]
#View(merged_data)

# Frequency table for the column 'year' of the merged_data (without 2021)
table(merged_data$year)

# Frequency table for the column 'geo' of the merged_data (without 2021)
table(merged_data$geo)

# We observe that for some countries we have less than 40 records
# Thus, we decided to keep only those who have 40 records and drop the others
# Also, we drop EU28 since we do not have info for the UK and EU27_2020 since we drop some of the countries contained in it
# Drop the records for LI, SI, RO, PT, TR, UK, EU27_2020, EU28
final_data1 <- merged_data[!grepl("LI", merged_data$geo),]
final_data2 <- final_data1[!grepl("SI", final_data1$geo),]
final_data3 <- final_data2[!grepl("RO", final_data2$geo),]
final_data4 <- final_data3[!grepl("PT", final_data3$geo),]
final_data5 <- final_data4[!grepl("TR", final_data4$geo),]
final_data6 <- final_data5[!grepl("UK", final_data5$geo),]
final_data7 <- final_data6[!grepl("EU27_2020", final_data6$geo),]
final_data <- final_data7[!grepl("EU28", final_data7$geo),]
#View(final_data)

# Check for duplicates
final_data[duplicated(final_data), ]

# Check for NaN values
any(is.na(final_data)) # We do not have Nan Values

# Find NaN values
which(is.na(final_data), arr.ind=TRUE) # In case we have find the position

# Create a dataframe only for Greece
el <- final_data[final_data$geo == 'EL',]
#View(el)

# Create a dataframe with all the other counties we have kept except Greece
df_without_el <- final_data[!grepl("EL", final_data$geo),]
#View(df_without_el)

# Now we contstruct a new dataframe which contains the sum of all the countries we have kept expect Greece (EU27)
# By using groupby function
EU27 <- df_without_el %>% group_by(year,sex,age,age_numeric) %>% summarize(across(c(ddeaths,ddeaths_per_age,tdeaths),sum))
#View(EU27)

# Now we construct a dataframe which contains the sum of EU27 for the 40 rows and we concatanate it with our final_data which contains information of all countries
# Create a new column 'geo' in order the two dataframes to have the same columns
EU27['geo'] <- rep('EU27',40)

#df_combo contains all the areas (and Greece) and the EU27
df_combo <- rbind(final_data, EU27)
#View(df_combo)

# Create a new column which contains the ratio ddeaths/tdeaths per country
# Set scaling factor for total deaths
# dementia deaths per 100k total deaths
# df_combo contains EU27 and all the other countries separately
df_combo["dd_per_td"] <- (df_combo$ddeaths_per_age / df_combo$tdeaths) * 100000
df_combo["dd_per_td"] <- round(df_combo$dd_per_td,3)
#View(df_combo)

# Create a new column which contains the ratio ddeaths/tdeaths per country (final_data)
# Set scaling factor for total deaths
# dementia deaths per 100k total deaths
# final_data contains all the other countries separately
final_data["dd_per_td"] <- (final_data$ddeaths_per_age / final_data$tdeaths) * 100000
final_data["dd_per_td"] <- round(final_data$dd_per_td,3)
#View(final_data)


####### Plots ##########
###### Plot 1 #########
# Trellis for Males by age and year
p1 <- ggplot() +
  geom_line(data = final_data[final_data$sex == 'M' & final_data$age_numeric == '1',], aes(x = year, y = dd_per_td, color = "Age 1"), lwd = 1, color='navy', alpha = 0.8) + 
  geom_point(data = final_data[final_data$sex == 'M' & final_data$age_numeric == '1',], aes(x = year, y = dd_per_td, color = "Age 1"), alpha = 0.5) +
  geom_line(data = final_data[final_data$sex == 'M' & final_data$age_numeric == '2',], aes(x = year, y = dd_per_td, color = "Age 2"), lwd = 1, color='azure4', alpha = 0.8) + 
  geom_point(data = final_data[final_data$sex == 'M' & final_data$age_numeric == '2',], aes(x = year, y = dd_per_td, color = "Age 2"), alpha = 0.5) +
  facet_wrap(~geo, nrow = 4, ncol = 7) +
  #theme_void(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 6, family = ("Georgia"), face='bold'),
    axis.text.y = element_text(size = 6, face='bold'), 
    axis.title = element_text(size = 9, family = "Georgia"),
    legend.position = "right",  # Position the legend at the bottom
    plot.title = element_text(margin = margin(b = 20))) +  # Increase the margin between title and plot
  labs(title = "Dementia Deaths per 100K Deaths",
    subtitle = "For Males per Year",  
    x = "Year",
    y = "Dementia Death Rate") +
  scale_color_manual(name ='  Age Group', 
                     labels = c("Over 65", "Under 65  "),
                     values = c("navy", "azure4"))
p1<- p1 + theme(text = element_text(size = 9, family = ("Georgia")),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, face='bold', family = "Georgia", size = 15), 
    plot.subtitle = element_text(hjust = 0.5, vjust = 6.9, size = 12, color = "darkslategrey"),
    legend.background = element_rect(fill = "white", color = "grey"),
    legend.box = "horizontal") +
    #legend.background  = element_rect(fill = "lightgrey", color = NA)) +
  scale_x_continuous(breaks = seq(min(final_data$year), max(final_data$year), by = 1)) +
  scale_y_continuous(breaks = c(0,750,1500,2250,2626))
p1
#ggsave(sprintf("Trellis_plotMales.png"),height = 5, width = 8.5, device = "png", type = "cairo", dpi = 700)

######## Plot 2 ########
# Trellis for Females by age and year
final_data <- final_data %>%
  mutate(age_modified = recode(age, "Y_GE65" = "Over 65","Y_LT65" = "Under 65"))

p2 <- ggplot(final_data[final_data$sex == 'F',]) +
  geom_linerange(aes(x = factor(year), ymin = 0, ymax = dd_per_td, color = age_modified), position = position_dodge(width = 1)) +
  geom_point(aes(x = factor(year), y = dd_per_td, color = age_modified), position = position_dodge(width = 1), size = 1) +
  facet_wrap(~geo, nrow = 4, ncol = 7) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, size = 6, family = "Georgia", face='bold'),
        axis.text.y = element_text(size = 6,family = "Georgia", face = 'bold'), 
        legend.position = "right") 

p2 <- p2 + theme(text = element_text(size = 9, family = ("Georgia")),
           plot.margin = margin(0.9, 0.5, 0.5, 0.5, "cm"),
           axis.ticks = element_blank(),
           panel.background = element_blank(),
           plot.title = element_text(hjust = 0.5, vjust=5, face='bold', family = "Georgia", size = 15), 
           plot.subtitle = element_text(hjust = 0.5, vjust = 6.9, size = 12, color = "darkslategrey"),  
           legend.background = element_rect(fill = "white", color = "grey"),
           legend.box = "horizontal") +
  labs(title = "Dementia Deaths per 100K Deaths",
       subtitle = "For Females per Year",  
       x = "Year",
       y = "Dementia Death Rate") +
  scale_color_manual(name = " Age Group",
                     values = c("Over 65"='deeppink', "Under 65"='darkslategrey'),
                     guide = "legend")
p2
#ggsave(sprintf("Trellis_plotFemales.png"),height = 5, width = 8.5, device = "png", type = "cairo", dpi = 700)

########  Plot 3 #########
# Scatter Plot (Over 65)
# Create a dataframe without age group (only contains the  dementia deaths over 65)
df_over65 <- final_data[final_data$age_numeric == 1,]
#View(df_over65)
# Group By
df_total <- df_over65 %>% group_by(geo) %>%
  summarize(across(c(ddeaths,tdeaths), sum))

# Due to the fact we don't have the total deaths per gender but for the total population when summarizing we have to divide the total deaths with 2
df_total$tdeaths <- df_total$tdeaths/2
#View(df_total)

# Part 1
p3_1 <- ggplot(df_total, aes(x = tdeaths, y = ddeaths, label = geo)) +
  geom_point(alpha = 0.3, size = 1.5, shape = 25, fill = "black", color = "black") +
  geom_point(data = subset(df_total, geo == "EL"),
             alpha = 0.8, size = 1.5, shape = 25, fill = "red", color = "black") +
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.4, size = 7, family = "Georgia", face = 'bold'),
        axis.text.y = element_text(size = 7, face = 'bold', family = "Georgia"), panel.border = element_rect(color = "black", fill=NA),
        plot.title = element_text(hjust = 0.5,family = 'Georgia', size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, family = 'Georgia', color = "darkslategrey")) +
  labs(title = "Dementia Deaths v. Total Deaths", subtitle = "Age Group Over 65\nPer Country for 2011 - 2020",
       x = "Total Deaths", y = "Dementia Deaths") +
  #theme(legend.position = "none") +
  geom_text_repel(nudge_y = 220, size = 3) + 
  scale_x_continuous(breaks = seq(0, max(df_total$tdeaths),by = 1000000), 
                     labels = c(0,"1M","2M","3M","4M","5M","6M","7M","8M","9M")) +
  scale_y_continuous(breaks = seq(0, max(df_total$ddeaths), by = 100000), 
                     labels = c(0, "100K", "200K", "300K"))
p3_1
# Part 2
p3_2 <- ggplot(df_total, aes(x = tdeaths, y = ddeaths, label = geo)) +
  geom_point(alpha = 0.3, size = 1.5, shape = 25, fill = "black", color = "black") +
  geom_point(data = subset(df_total, geo == "EL"),
             alpha = 0.8, size = 1.5, shape = 25, fill = "red", color = "black") +
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.4, size = 7, family = "Georgia", face = 'bold'),
        axis.text.y = element_text(size = 7, face = 'bold', family = "Georgia"), 
        panel.border = element_rect(color = "black", fill=NA),
        plot.title = element_text(hjust = 0.5,family = 'Georgia', size = 13, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_text_repel(nudge_y = 220, size = 3) +
  scale_x_continuous(breaks = seq(0, 1600000,by = 800000), 
                     labels = c(0,"0.8M","1.6M"), limits = c(0,1600000)) +
  scale_y_continuous(breaks = seq(0, 120000, by = 40000), 
                     labels = c(0, "40K", "80K", "120K"), limits = c(0,120000)) 

# Merged plot
p3_1+annotation_custom(ggplotGrob(p3_2),xmin = 4500000, xmax = 9000000, ymin = 0, ymax = 140000) +
  annotate("text", x = 6500000, y = 145000, label = "Zooming in", 
           color = "black",family='Georgia', size = 4, hjust = 0, vjust = 0)

#ggsave(sprintf("scatter.png"),height = 5, width = 8, device = "png", type = "cairo", dpi = 700)

####### Plot 4 #######
# Lineplot Greece - Europe
# Create a dataframe with EU27 and Greece
combo <- df_combo[df_combo$geo == 'EU27' 
                  |df_combo$geo == 'EL',]
combo1 <- combo[combo$age_numeric == '1',] # ddeaths OVER 65
combo2 <- combo[combo$age_numeric == '2',] # ddeaths UNDER 65
#View(combo1)

el["dd_per_td"] <- (el$ddeaths_per_age / el$tdeaths) * 100000
el["dd_per_td"] <- round(el$dd_per_td,3)

combo1["dd_per_td"] <- (combo1$ddeaths_per_age / combo1$tdeaths) * 100000
combo1["dd_per_td"] <- round(combo1$dd_per_td,3)

combo2["dd_per_td"] <- (combo2$ddeaths_per_age / combo2$tdeaths) * 100000
combo2["dd_per_td"] <- round(combo2$dd_per_td,3)
#View(combo1)

#lineplot
p4 <- ggplot() +
  geom_line(data = combo1[combo1$sex == 'M' & combo1$geo == 'EL',], aes(x = year, y = dd_per_td, color = "Males", linetype = "Greece"), lwd = 1, alpha = 0.8) + 
  geom_line(data = combo1[combo1$sex == 'F' & combo1$geo == 'EL',], aes(x = year, y = dd_per_td, color = "Females", linetype = "Greece"), lwd = 1, alpha = 0.8) + 
  geom_line(data = combo1[combo1$sex == 'M' & combo1$geo == 'EU27',], aes(x = year, y = dd_per_td, color = "Males", linetype = "Europe"), lwd = 1, alpha = 0.6) + 
  geom_line(data = combo1[combo1$sex == 'F' & combo1$geo == 'EU27',], aes(x = year, y = dd_per_td, color = "Females", linetype = "Europe"), lwd = 1, alpha = 0.6) + 
  theme_tufte() +
  theme(axis.text.x = element_text(size = 7, family = "Georgia", face = 'bold'),
        axis.text.y = element_text(size = 7, family = "Georgia", face = 'bold'), 
        legend.position = c(0.80,0.12),
        plot.title = element_text(margin = margin(b = 20))) +
  labs(title = "Dementia Deaths per 100K Deaths",
       subtitle = "Greece v. Europe",
       x = "Year",
       y = "Dementia Death Rate (log)",
       color = "Age Group") +
  scale_linetype_manual(name = "Area",
                        values = c("Greece" = "solid", "Europe" = "dotdash")) +
  scale_color_manual(name = "Gender",
                     values = c("Males" = "navy", "Females" = "deeppink")) +
  scale_y_log10() +
  theme(text = element_text(size = 9, family = "Georgia"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        #axis.ticks = element_blank(),
        #panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust=1.5, face='bold', family = "Georgia", size = 15), 
        plot.subtitle = element_text(hjust = 0.5, vjust = 6.9, size = 12, color = "darkslategrey"),  
        legend.box = "horizontal",
        panel.background = element_rect(fill = "white", color = "black")) +
  scale_x_continuous(breaks = seq(min(combo1$year), max(combo1$year), by = 1))
p4

#ggsave(sprintf("lineplot.png"),height = 5, width = 7, device = "png", type = "cairo", dpi = 700)


###### Plot 5 ########
# Violin Plot for Males - Females, Greece - Europe
# Create the plot only for males and females over 65
p5<-ggplot(combo1, aes(x = combo1$geo , y = combo1$dd_per_td, fill=combo1$sex)) +
  geom_violin(alpha = 0.5,trim=FALSE,aes(fill=combo1$sex)) +
  geom_boxplot(width=0.05,alpha = 0.5, position = position_dodge(width=0.9), show.legend = FALSE) + 
  labs(title = "Dementia Deaths per 100K Deaths",
       subtitle = "Females and Males 65+\n2011 - 2020",  # Add your subtitle here
       x = " ",
       y = "Dementia Death Rate") +
  scale_fill_manual(labels = c("Females", "Males"), values = c("deeppink","navy"), 
                    name = "Gender") +
  scale_x_discrete(labels = c("Greece", "Europe")) + 
  theme(panel.background = element_blank(),
        text = element_text(size = 9, family = "Georgia"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 9, family = "Georgia"),
        axis.title.y = element_text(size=9, family = "Georgia"),
        #axis.title.x = element_text(size=7, family = "Georgia", face = "bold"),
        plot.title = element_text(hjust = 0.5,family = 'Georgia', size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, family = 'Georgia', size = 12, color = "darkslategrey"),
        legend.background = element_rect(fill = "white", color = "grey"),
        legend.box = "horizontal"
        )
p5

#ggsave(sprintf("violinplot.png"),height = 5, width = 7, device = "png", type = "cairo", dpi = 700)

###### Plot 6 #######
# Pointplot
p6 <- ggplot(df_over65, aes(x = geo, y = dd_per_td, color = sex)) + 
  geom_point(alpha = 0.6, show.legend = TRUE) +
  stat_summary(geom = "point", fun = "mean", colour = "#DA450E", size = 4, alpha = 0.9) +
  #geom_point(x = 10, y = mean(df_over65$dd_per_td[df_over65$geo == 'EL']), color = "", size = 4) 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.4, vjust = 0.3, size = 7, family = "Georgia", face='bold'),
        axis.text.y = element_text(size = 7,family = "Georgia", face = "bold"), 
        legend.position = "right",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) +
  labs(title = "Dementia Deaths per 100K Deaths",
       subtitle = "Comparison with the Means of Each Country\n2011 - 2020",  # Add your subtitle here
       x = "Area",
       y = "Dementia Death Rate") +
  scale_color_manual(labels = c("Females", "Males"), values = c("deeppink","navy"), name = "Gender") 

p6 <- p6 + theme(text = element_text(size = 9.5, family = "Georgia"),
  panel.border = element_rect(color = "white", fill=NA),
  axis.ticks.y = element_blank(),
  axis.text = element_text(),
  axis.title.y = element_text(size=9.5, family = "Georgia"),
  axis.title.x = element_text(size=9.5, family = "Georgia"),
  plot.title = element_text(hjust = 0.5, family = 'Georgia', size = 15),
  plot.subtitle = element_text(hjust = 0.5, vjust = 1.5, color = "darkslategrey", size = 11.5, family = 'Georgia'),
  legend.background = element_rect(fill = "white", color = "grey"),
  legend.box = "horizontal") +
  geom_rect(xmin = 9.5, xmax = 10.5, ymin = -Inf, ymax = Inf, alpha = 0.002, color = "transparent", show.legend = FALSE)
p6
#ggsave(sprintf("dotplot.png"),height = 5, width = 7, device = "png", type = "cairo", dpi = 700)


####### Plot 7 #######
#lollipop

# Compute the mean by gender by country
x<-as.data.frame(tapply(df_over65$dd_per_td,df_over65[,c(1,2)],mean, na.rm = TRUE))
# View(x)
# x transpose 
x<- as.data.frame(t(x))
# Add the geo in the dataframe
x$geo <- unique(df_over65$geo)

x$f_mean <- rep(mean(x$F),28)
x$m_mean <- rep(mean(x$M),28)
#View(x)

# Diff
x$diff_f <- x$F - x$f_mean
x$diff_m <- x$M - x$m_mean
#View(x)

# df Males
df_males <- x[,c(2,3,5,7)]
#View(df_males)
#sorting the differences df_males
df_males<- df_males %>%
  arrange(diff_m) %>%
  mutate(geo = factor(geo, levels = .$geo))
# View(df_males)
# Create a column for the sign
df_males$sign_m <- c(rep("Negative",14),rep("Positive",14))
#View(df_males)

# Df females
df_females <- x[,c(1,3,4,6)]
#View(df_females)
#sorting  the differences df_females
df_females<- df_females %>%
  arrange(diff_f) %>%
  mutate(geo = factor(geo, levels = .$geo))
#View(df_females)
# Create a column for the sign
df_females$sign_f <- c(rep("Negative",14),rep("Positive",14))
# View(df_females)


breaks_values <- pretty(df_females$diff_f)


# Lollipop for Males
p7_1 <- ggplot(data = df_males, aes(x = geo, y = diff_m, color = sign_m))+
  geom_segment(aes(x = geo, y = 0, xend = geo, yend = diff_m)) +
  geom_point() +
  coord_flip()+
  scale_y_continuous(breaks = seq(-2000,3000,by=500),
                     limits = c(-2000,2700),
                     labels = abs(seq(-2000,3000,by=500)))+
  annotate("text", x = 13.7, y = 1000, label = "Below Average", 
           color = "#008FFF", size = 2, hjust = -0.2, vjust = .75) +
  annotate("text", x = 15.3, y = 1000, label = "Over Average", 
           color = "navy", size = 2, hjust = -0.2, vjust = -.1) +
  geom_segment(aes(x = 14, xend = 14, y = 1500, yend = 1000),
               arrow = arrow(length = unit(0.17,"cm")), color = "#008FFF") +
  geom_segment(aes(x = 15, xend = 15, y = 1000, yend = 1500),
               arrow = arrow(length = unit(0.17,"cm")), color = "navy") +
  scale_color_manual(values=c("#008FFF","navy"))+
  scale_fill_manual(breaks=c('Females', 'Males'),values=c("#008FFF","navy"))+
  geom_hline(yintercept=0, lwd = 0.8, color='brown2') +
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        text = element_text(family = "Georgia"),
        plot.title = element_text(size = 15, margin = margin(b = 8), hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "darkslategrey",hjust=0.5, margin = margin(b = 6)),
        axis.text.x=element_text(angle = 0, size = 6.5, face='bold', color="black"),
        axis.text.y=element_text(size = 6.5,
                                 color = ifelse(df_males$geo == 'EL', "blue", "black"),face='bold'),
        strip.text.x = element_text(size=6,
                                    face="bold"),
        axis.title=element_text(size=8))+
  labs(title="Difference of Means of Dementia Deaths per Country with the European Mean", 
       subtitle="For Males Per 100K Deaths",  
       x="",
       y="")
p7_1


# Lollipop for Females
p7_2 <- ggplot(data = df_females, aes(x = geo, y = diff_f, color = sign_f))+
  geom_segment(aes(x = geo, y = 0, xend = geo, yend = diff_f)) +
  geom_point() +
  coord_flip()+
  scale_y_continuous(breaks = seq(-2000,3000,by=500),
                     limits = c(-2000,2700),
                     labels = abs(seq(-2000,3000,by=500)))+
  annotate("text", x = 13.7, y = 1000, label = "Below Average", 
           color = "#FF7000", size = 2, hjust = -0.2, vjust = .75) +
  annotate("text", x = 15.3, y = 1000, label = "Over Average", 
           color = "deeppink", size = 2, hjust = -0.2, vjust = -.1) +
  geom_segment(aes(x = 14, xend = 14, y = 1500, yend = 1000),
               arrow = arrow(length = unit(0.17,"cm")), color = "#FF7000") +
  geom_segment(aes(x = 15, xend = 15, y = 1000, yend = 1500),
               arrow = arrow(length = unit(0.17,"cm")), color = "deeppink") +
  scale_color_manual(values=c("#FF7000","deeppink"))+
  scale_fill_manual(breaks=c('Females', 'Males'),values=c("#FF7000","deeppink"))+
  geom_hline(yintercept=0, lwd = 0.8, color='brown2') +
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        text = element_text(family = "Georgia"),
        #plot.title = element_text(size = 13, margin = margin(b = 8), hjust = 0.5),
        plot.subtitle = element_text(size = 12, color = "darkslategrey",hjust=0.5, margin = margin(b = 6)),
        #plot.caption = element_text(size = 8, margin = margin(t = 10), color = "", hjust = 0),
        axis.text.x=element_text(angle = 0, size = 6.5, face='bold', color="black"),
        axis.text.y=element_text(size = 6.5,
                                 color = ifelse(df_females$geo == 'EL', "blue", "black"),face='bold'),
        axis.title.x = element_text(size = 8, family = 'Georgia'),
        strip.text.x = element_text(size=6,
                                    face="bold"))+
  labs(subtitle="For Females Per 100K Deaths",  
       x="", 
       y="Difference of Mean")
p7_2
# Combine the two lollipops
ggarrange(p7_1, p7_2, ncol = 1,nrow=2)

#ggsave(sprintf("lollipop.png"),height = 8, width = 8, device = "png", type = "cairo", dpi = 700)


#################### Preprocessing for the age groups ############################
#### DEMENTIA DEATHS PER AGE GROUP
# age group 65 - 74
ddeaths1 <- read.csv("C:/Users/irene/Desktop/3rd_Semester/Data Visualization and Communication/Project/ddeaths_65_74.csv")
ddeaths1 <- as.data.frame(ddeaths1)

ddeaths1 <- ddeaths1 %>%
  group_by(geo,TIME_PERIOD, sex) %>%
  summarize(across(OBS_VALUE,sum))

ddeaths1$age <- rep("65-74",716)

# Rename the columns
ddeaths1 <- ddeaths1  %>% rename('ddeaths' = 'OBS_VALUE') %>% rename('year' = 'TIME_PERIOD') 
#View(ddeaths1)

# age group 75 - 84
ddeaths2 <- read.csv("C:/Users/irene/Desktop/3rd_Semester/Data Visualization and Communication/Project/ddeaths_75_84.csv")
ddeaths2 <- as.data.frame(ddeaths2)

ddeaths2 <- ddeaths2 %>%
  group_by(geo,TIME_PERIOD, sex) %>%
  summarize(across(OBS_VALUE,sum))

ddeaths2$age <- rep("75-84",716)
# Rename the columns
ddeaths2 <- ddeaths2  %>% rename('ddeaths' = 'OBS_VALUE') %>% rename('year' = 'TIME_PERIOD') 
#View(ddeaths2)

# age group 85 +
ddeaths3 <- read.csv("C:/Users/irene/Desktop/3rd_Semester/Data Visualization and Communication/Project/ddeaths_85_plus.csv")
ddeaths3 <- as.data.frame(ddeaths3)
#View(ddeaths3)
ddeaths3 <- ddeaths3 %>%
  group_by(geo,TIME_PERIOD, sex) %>%
  summarize(across(OBS_VALUE,sum))

ddeaths3$age <- rep("85+",716)
# Rename the columns
ddeaths3 <- ddeaths3  %>% rename('ddeaths' = 'OBS_VALUE') %>% rename('year' = 'TIME_PERIOD') 
#View(ddeaths3)

# Merge age 
ddeaths_per_age <- rbind(ddeaths1, ddeaths2, ddeaths3)
#View(ddeaths_per_age)

# Ordered data
ddeaths_per_age <- with(ddeaths_per_age, ddeaths_per_age[order(sex, geo, year),])
#View(ddeaths_per_age)
#Keep only the unique geo from final_data
ddeaths_per_age <- ddeaths_per_age[ddeaths_per_age$geo %in% unique(final_data$geo),]

# Drop the year 2021
ddeaths_per_age <- ddeaths_per_age[ddeaths_per_age$year != "2021",]
#View(ddeaths_per_age)

# Merge the two dataframes of total deaths and dementia deaths
# does not include EU27. It includes all the other geo
final_data_per_age <- right_join(total_deaths,ddeaths_per_age, by=c("geo","year"))
#View(final_data_per_age)
#View(final_data)


#################################################################
# Create a dataframe only for Greece
el_per_age <- final_data_per_age[final_data_per_age$geo == 'EL',]
#View(el_per_age)

# Create a dataframe with all the other counties we have kept except Greece
df_without_el_per_age <- final_data_per_age[!grepl("EL", final_data_per_age$geo),]
#View(df_without_el_per_age)

# Now we contstruct a new dataframe which contains the sum of all the countries we have kept expect Greece (EU27)
# By using groupby function
EU27 <- df_without_el_per_age %>% group_by(year,sex,age) %>% summarize(across(c(ddeaths,tdeaths),sum))
#View(EU27)

# Now we construct a dataframe which contains the sum of EU27 for the 60 rows and we concatenate it with our final_data_perage which contains information of all countries
# Create a new column 'geo' in order the two dataframes to have the same columns
EU27['geo'] <- rep('EU27',60)

# df_allinfo contains info both for EU27 and each country
df_allinfo <- rbind(final_data_per_age, EU27)
#View(df_allinfo)

# DEATH RATE
# Create a new column which contains the ratio ddeaths/tdeaths per country
# Set scaling factor for total deaths
# dementia deaths per 100K total deaths
# df_allinfo contains EU27 and all the other countries separately
df_allinfo["dd_per_td"] <- (df_allinfo$ddeaths / df_allinfo$tdeaths) * 100000
df_allinfo["dd_per_td"] <- round(df_allinfo$dd_per_td,3)
#View(df_allinfo)

# Create a new column which contains the ratio ddeaths/tdeaths per country (final_data_per_age)
# Set scaling factor for total deaths
# dementia deaths per 100K total deaths
#final_data_per_age contains all the other countries separately (not EU27)
final_data_per_age["dd_per_td"] <- (final_data_per_age$ddeaths / final_data_per_age$tdeaths) * 100000
final_data_per_age["dd_per_td"] <- round(final_data_per_age$dd_per_td,3)
#View(final_data_per_age)

#### Plot 8 #####
# Balloon plot
df_combo_per_age <- df_allinfo[df_allinfo$geo == 'EL' | df_allinfo$geo == "EU27",]
#View(df_combo_per_age)

# Group By geo, sex and age
df_grouped <- df_combo_per_age %>%
  group_by(geo, sex, age) %>%
  summarize(across(c(ddeaths,tdeaths),sum))
#View(df_grouped)

# Create the two new columns for the df_grouped
df_grouped["dd_per_td"] <- (df_grouped$ddeaths / df_grouped$tdeaths) * 100000
df_grouped["dd_per_td"] <- round(df_grouped$dd_per_td,3)
#View(df_grouped)

# Reorder x-axis based on "age" variable
# Define the desired order of the "age" variable
age_order <- c("65-74", "75-84", "85+")

# Convert "age" variable to factor with the desired order
df_grouped$age <- factor(df_grouped$age, levels = age_order)

# Create the balloon plot 
p8 <- ggplot(df_grouped, aes(x = sex, y = geo, size = dd_per_td)) +
  geom_point(shape = 24, color = "cadetblue4", fill = "cadetblue4") +
  facet_wrap(~age) +
  theme_bw() +
  theme(
    #axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8, family = "Georgia", face = 'bold'),
    plot.title = element_text(margin = margin(b = 20))) +
  labs(title = "Dementia Deaths per 100K Deaths\nGreece v. Europe",
    subtitle = "Age Group",
    x = "Gender",
    y = "") +
  scale_x_discrete(labels = c("F" = "\u2640", "M" = "\u2642")) +
  labs(size = "Dementia Death\n          Rate")  # Add the desired legend name here


p8 <- p8 + geom_text(aes(label = round(dd_per_td, 0)), color = "black", family = 'Georgia', vjust = 2, size = 2.5) +
  theme(text = element_text(size = 9, family = "Georgia"),
    axis.text.x = element_text(size = 13.5, face = 'bold', family = 'Times New Roman'),
    axis.text.y = element_text(size = 9, family = 'Georgia'),
    axis.title.x = element_text(size = 9, family = 'Georgia'),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 13, hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 9, color = "black"),
    legend.box = "horizontal") +
  scale_y_discrete(labels = c("Greece", "Europe"))
p8

#ggsave(sprintf("balloon.png"),height = 4, width = 5, device = "png", type = "cairo", dpi = 700)


### Plot 9 ####
# Bubble plot
# Group By geo, age and year
df_grouped_peryear <- df_combo_per_age %>%
  group_by(geo, age, year) %>%
  summarize(across(c(ddeaths,tdeaths),sum))
#View(df_grouped_peryear)

# Create the two new columns for the df_grouped
df_grouped_peryear["dd_per_td"] <- (df_grouped_peryear$ddeaths / df_grouped_peryear$tdeaths) * 100000
df_grouped_peryear["dd_per_td"] <- round(df_grouped_peryear$dd_per_td,3)
#View(df_grouped_peryear)

# boxplot
p9 <- ggplot(df_grouped_peryear[df_grouped_peryear$geo == 'EL' | df_grouped_peryear$geo == 'EU27',], 
              aes(x = geo, y = dd_per_td, color = age)) +
  geom_boxplot() +
  geom_jitter(aes(color = age),fill = "black", size=3, alpha=0.6) +
  theme(plot.title = element_text(size=11)) +
  ggtitle("A boxplot with jitter") +
  xlab("") +
  scale_color_manual(values = c("#53AE51","#5153AE","#AE5153"))

p9 <- p9 + theme_minimal() +
  theme(axis.text.y = element_text(size = 8, family = "Georgia", face = 'bold'),
        axis.text.x = element_text(size = 8, family = "Georgia", face = 'bold'),
        plot.title = element_text(margin = margin(b = 20)),
        axis.line = element_line(size = 0.2)) +
  labs(title = "Dementia Deaths per 100K Deaths",
       subtitle = "Greece v. Europe",
       x= "",
       y = "Dementia Death Rate") +
  theme(text = element_text(size = 9, family = "Georgia"),
        axis.text.y = element_text(size = 9, family = 'Georgia'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        axis.ticks = element_blank(),
        #panel.background = element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, vjust = 7, size = 12, color = "darkslategrey"),
        legend.background = element_rect(fill = 'white', color = 'grey')) +
  labs(color = "Age Group") + 
  scale_x_discrete(labels = c("Greece", "Europe"))
p9

#ggsave(sprintf("boxplot.png"),height = 7, width = 7, device = "png", type = "cairo", dpi = 700)


##### Plot 10 ######
# Bubble plot
# Greece
p10_1 <- ggplot(df_grouped_peryear[df_grouped_peryear$geo == "EL",], aes(x = year, y = df_grouped_peryear$dd_per_td[df_grouped_peryear$geo == 'EL'])) + 
  geom_jitter(aes(col=age), size = 3) + 
  geom_smooth(aes(col=age), method="lm", se=F) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8, family = "Georgia", face = 'bold'),
    axis.text.x = element_text(size = 8, family = "Georgia", face = 'bold'),
    plot.title = element_text(margin = margin(b = 20)),
    axis.line = element_line(size = 0.2)) +
  labs(subtitle = "Greece",
       x = "Year",
       y = "Dementia Death Rate") +
  ylim(c(0,1500)) +
  scale_x_continuous(breaks = seq(min(df_grouped_peryear$year), max(df_grouped_peryear$year), by = 1)) +
  scale_color_manual(values = c("#53AE51","#5153AE","#AE5153")) +
  theme(text = element_text(size = 9, family = "Georgia"),
        axis.title = element_text(size = 9, family = 'Georgia'),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.ticks = element_blank(),
    #plot.title = element_text(size = 15, hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12, color = "darkslategrey"),
    legend.background = element_rect(fill = "white", color = "grey"),
    legend.box = "horizontal") +
  labs(color = "Age Group") 
p10_1


# EU27 
p10_2 <- ggplot(df_grouped_peryear[df_grouped_peryear$geo == "EU27",], aes(x = year, y = df_grouped_peryear$dd_per_td[df_grouped_peryear$geo == 'EU27'])) + 
  geom_jitter(aes(col=age), size = 3) + 
  geom_smooth(aes(col=age), method="lm", se=F) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8, family = "Georgia", face = 'bold'),
        axis.text.x = element_text(size = 8, family = "Georgia", face = 'bold'),
        axis.title.x = element_text(size = 9, family = "Georgia"),
        plot.title = element_text(margin = margin(b = 20)),
        axis.line = element_line(size = 0.2)) +
  labs(subtitle = "Europe",
       x = "Year",
       y = "") +
  ylim(c(0,1500)) +
  scale_x_continuous(breaks = seq(min(df_grouped_peryear$year), max(df_grouped_peryear$year), by = 1)) +
  scale_color_manual(values = c("#53AE51","#5153AE","#AE5153")) +
  theme(text = element_text(size = 8, family = "Georgia"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        axis.ticks = element_blank(),
        #panel.background = element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 12, color = "darkslategrey"),
        legend.position = "none") 
p10_2
# Merge the two plots
p10 <- (p10_1 + p10_2) + # Create grid of plots with title
  plot_annotation(title = "Dementia Deaths per 100K Deaths") &
  theme(plot.title = element_text(hjust = 0.5, family = "Georgia", size = 15, face = 'bold'))
p10

#ggsave(sprintf("bubblelines.png"),height = 5, width = 10, device = "png", type = "cairo", dpi = 700)


# Download the final_data_per_age to use them in Tableu
write_xlsx(final_data_per_age, "C:\\Users\\irene\\Desktop\\3rd_Semester\\Data Visualization and Communication\\Project\\final.xlsx")


