### Recency Frequency Monetary Value Analysis

library(tidyverse)
library(urbnmapr)
library(urbnmapr)
library(vcd)
library(gplots)

#Import the dataset
data_user_profile <- read.csv("dataset/user_profile.csv")
data_first_session <- read.csv("dataset/first_session.csv")
data_user_engagement <- read.csv("dataset/user_engagement.csv")

#geographic distribution of the users
colnames(states) <- c("long", "lat","order", "hole", "piece", "group","state_fips","state","state_name")
states
data_states <- data_user_profile %>% group_by(state) %>% summarise(count = n()) %>% 
  left_join(.,states, by = "state")

ggplot(data_states,aes(long, lat, group = group, fill = count)) + 
  geom_polygon(color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(x = "", y = "",title = "Geographic distribution of users") +
  theme(title = element_text(size = 16)) 
ggsave("figs/geographic distribution of users.png")

#demographic distribution of the users
data_demographic <- data_user_profile %>% select(gender, age_bucket)
data_demographic[!(is.na(data_demographic$gender) | data_demographic$age_bucket==""),]
ggplot(data_demographic) +
  geom_bar(aes(age_bucket, fill = gender)) +
  coord_flip() +
  facet_grid(.~gender) +
  labs(y = "Number of users", x = "Age bucket of users", title = "Demographic distribution of the users")+
  scale_fill_brewer(palette="RdYlBu")+
  theme(title=element_text(size=16))

ggsave("figs/demographic distribution of the users.png")

#RFM model of the customers
profile.data <- data_user_profile
RFM_segs <- profile.data %>% select("tradelines_min_days_since_opened", "count_total_tradelines_opened_24_months", "total_tradelines_open_balance") %>% 
  mutate(recency_months = tradelines_min_days_since_opened /30, frequency_val = count_total_tradelines_opened_24_months, monetary_val = (total_tradelines_open_balance/count_total_tradelines_opened_24_months)/1000)

summary(RFM_segs$recency_months)
summary(RFM_segs$frequency_val)
summary(RFM_segs$monetary_val)

RFM_segs$Recency <- ordered(ifelse(RFM_segs$recency_months <= 6, "0-6",
                                   ifelse(RFM_segs$recency_months <= 12, "7-12",
                                          ifelse(RFM_segs$recency_months <= 18, "13-18",
                                                 ifelse(RFM_segs$recency_months <= 24, "19-24", "24+")))),
                            levels = c("0-6", "7-12", "13-18", "19-24", "24+"))

RFM_segs$Frequency <- ordered(ifelse(RFM_segs$frequency_val<= 1, "0-1",
                                     ifelse(RFM_segs$frequency_val <= 3, "3-2",
                                            ifelse(RFM_segs$frequency_val <= 7, "7-4", "8+"))),
                              levels = c("8+", "7-4", "3-2", "0-1"))

RFM_segs$Monetary <- ordered(ifelse(RFM_segs$monetary_val<= 5, "5-0",
                                    ifelse(RFM_segs$monetary_val <= 10, "10-6",
                                           ifelse(RFM_segs$monetary_val <= 15, "15-11",
                                                  ifelse(RFM_segs$monetary_val <= 20, "20-16", "20+")))),
                             levels = c("20+", "20-16", "15-11", "10-6", "5-0"))

RFM_st <- structable(~ Recency + Frequency + Monetary, data = RFM_segs)

mosaic_plot <- function(f) {
  mosaic(f, data = RFM_st,
         shade = TRUE,
         labeling_args = list(rot_labels = c(left = 60, top = 30), gp_labels = (gpar(fontsize = 10)),
                              just_labels = c(left = "center",
                                              top = "center")
         ),
         spacing = spacing_dimequal(unit(c(0.5, 0.8), "lines")),
         keep_aspect_ratio = TRUE
  )
}
jpeg("figs/mosaic_RF.png",
     width = 600, height = 480)
mosaic_plot(~ Recency + Frequency)
dev.off()

jpeg("figs/mosaic_FM.png",
     width = 600, height = 480)
mosaic_plot(~ Frequency + Monetary)
dev.off()

jpeg("figs/mosaic_RM.png",
     width = 600, height = 480)
mosaic_plot(~ Recency + Monetary)
dev.off()

jpeg("figs/mosaic_all.png", width = 1000, height = 1000)
pairs(RFM_st, lower_panel = pairs_assoc, shade = TRUE)
dev.off()
require(gplots)
# Recency by Frequence - Counts
jpeg("figs/balloon_RFC.png", width = 700, height = 480)
RxF <- as.data.frame(table(RFM_segs$Recency, RFM_segs$Frequency,
                           dnn = c("Recency_months", "Frequency")),
                     responseName = "Number_Customers")
with(RxF, balloonplot(Recency_months, Frequency, Number_Customers, zlab = "#
                      Customers"))
dev.off()

# Recency by Frequency - tatol balance (total balance sales to segment)

jpeg("figs/balloon_RFM.png", width = 700, height = 480)
VbyRxF <- (aggregate(RFM_segs$monetary_val,
                     by = list(Recency_months = factor(RFM_segs$Recency),
                               Frequency = RFM_segs$Frequency),
                     sum))
names(VbyRxF)[3] <- "Average_balance"
with(VbyRxF, balloonplot(Recency_months, Frequency, Average_balance, zlab =
                           "Average balance (000)"))
dev.off()

### Frequent Item Set Analysis and Link Association Analysis

users <- read.csv("user_profile.csv")
engagement <- read.csv("user_engagement.csv")
fSession <- read.csv("first_session.csv")

head(users)
dim(users)
dim(fSession)
head(fSession)
length(unique(fSession$user_id))

head(engagement)
dim(engagement)
length(unique(engagement$user_id))
hist(log(engagement$session_length), breaks = 10)
summary(engagement$session_length)
((max(engagement$session_length)/60)/60)/24


##################fSession############################
###############Answering Basic Questions#############
unique(fSession$action_type)
unique(fSession$page_name)

library(dplyr)
#Pages for click apply
unique(fSession %>% 
         filter(action_type == "CLICK_APPLY") %>%
         select(page_name))
#13 such pages exist

#number of people who clicked on apply
x <- (fSession %>% 
        filter(action_type == "CLICK_APPLY") %>%
        select(page_name))
#149954 people clicked on apply


#break the dataset up into those who applied and those who didn't apply
library(caret)
#Check to see if user profile credit scores are well balanced.
nearZeroVar(users$credit_score_bucket) #balanced

#Function to get credit score first value
credit_scores <- function(x) {
  x <- as.character(x)
  x <- unlist(strsplit(x, ","))
  x <- as.character(x[1])
  x <- substring(x, 2)
  return(as.numeric(unlist(x)))
}

users$credit_score <- sapply(users$credit_score_bucket,credit_scores)
head(users$credit_score)

users$credit_score <- ifelse(credit_score >= 720, "")

#credit score
library(dplyr)
users$credit_score <- case_when(
  users$credit_score_bucket == "(495.0, 500.0]" ~ "Poor",
  users$credit_score_bucket == "(500.0, 505.0]" ~ "Poor",
  users$credit_score_bucket == "(505.0, 510.0]" ~ "Poor",
  users$credit_score_bucket == "(510.0, 515.0]" ~ "Poor",
  users$credit_score_bucket == "(515.0, 520.0]" ~ "Poor", 
  users$credit_score_bucket == "(520.0, 525.0]" ~ "Poor",
  users$credit_score_bucket == "(525.0, 530.0]" ~ "Poor",
  users$credit_score_bucket == "(530.0, 535.0]" ~ "Poor",
  users$credit_score_bucket == "(535.0, 540.0]" ~ "Poor",
  users$credit_score_bucket == "(540.0, 545.0]" ~ "Poor",
  users$credit_score_bucket == "(545.0, 550.0]" ~ "Poor",
  users$credit_score_bucket == "(550.0, 555.0]" ~ "Fair",
  users$credit_score_bucket == "(555.0, 560.0]" ~ "Fair", 
  users$credit_score_bucket =="(560.0, 565.0]" ~ "Fair", 
  users$credit_score_bucket =="(565.0, 570.0]" ~ "Fair", 
  users$credit_score_bucket =="(570.0, 575.0]" ~ "Fair", 
  users$credit_score_bucket =="(575.0, 580.0]" ~ "Fair", 
  users$credit_score_bucket =="(580.0, 585.0]" ~ "Fair", 
  users$credit_score_bucket =="(585.0, 590.0]" ~ "Fair", 
  users$credit_score_bucket =="(590.0, 595.0]" ~ "Fair", 
  users$credit_score_bucket =="(595.0, 600.0]" ~ "Fair", 
  users$credit_score_bucket =="(600.0, 605.0]" ~ "Fair", 
  users$credit_score_bucket =="(605.0, 610.0]" ~ "Fair", 
  users$credit_score_bucket =="(610.0, 615.0]" ~ "Fair", 
  users$credit_score_bucket =="(615.0, 620.0]" ~ "Fair", 
  users$credit_score_bucket =="(620.0, 625.0]" ~ "Fair", 
  users$credit_score_bucket =="(625.0, 630.0]" ~ "Fair", 
  users$credit_score_bucket =="(630.0, 635.0]" ~ "Fair", 
  users$credit_score_bucket =="(635.0, 640.0]" ~ "Fair", 
  users$credit_score_bucket =="(640.0, 645.0]" ~ "Good", 
  users$credit_score_bucket =="(645.0, 650.0]" ~ "Good",
  users$credit_score_bucket == "(650.0, 655.0]" ~ "Good",
  users$credit_score_bucket == "(655.0, 660.0]" ~ "Good", 
  users$credit_score_bucket == "(660.0, 665.0]" ~ "Good", 
  users$credit_score_bucket == "(665.0, 670.0]" ~ "Good",
  users$credit_score_bucket == "(670.0, 675.0]" ~ "Good", 
  users$credit_score_bucket == "(675.0, 680.0]" ~ "Good", 
  users$credit_score_bucket == "(680.0, 685.0]" ~ "Good", 
  users$credit_score_bucket == "(685.0, 690.0]" ~ "Good", 
  users$credit_score_bucket == "(690.0, 695.0]" ~ "Good", 
  users$credit_score_bucket == "(695.0, 700.0]" ~ "Good",
  users$credit_score_bucket == "(700.0, 705.0]" ~ "Good",
  users$credit_score_bucket == "(705.0, 710.0]" ~ "Good", 
  users$credit_score_bucket == "(710.0, 715.0]" ~ "Good", 
  users$credit_score_bucket == "(715.0, 720.0]" ~ "Good", 
  users$credit_score_bucket == "(720.0, 725.0]" ~ "Excellent", 
  users$credit_score_bucket == "(725.0, 730.0]" ~ "Excellent", 
  users$credit_score_bucket == "(730.0, 735.0]" ~ "Excellent", 
  users$credit_score_bucket == "(735.0, 740.0]" ~ "Excellent", 
  users$credit_score_bucket == "(740.0, 745.0]" ~ "Excellent", 
  users$credit_score_bucket == "(745.0, 750.0]" ~ "Excellent",
  users$credit_score_bucket == "(750.0, 755.0]" ~ "Excellent", 
  users$credit_score_bucket == "(755.0, 760.0]" ~ "Excellent", 
  users$credit_score_bucket == "(760.0, 765.0]" ~ "Excellent", 
  users$credit_score_bucket == "(765.0, 770.0]" ~ "Excellent", 
  users$credit_score_bucket == "(770.0, 775.0]" ~ "Excellent", 
  users$credit_score_bucket == "(750.0, 755.0]" ~ "Excellent", 
  users$credit_score_bucket == "(755.0, 760.0]" ~ "Excellent", 
  users$credit_score_bucket == "(760.0, 765.0]" ~ "Excellent", 
  users$credit_score_bucket == "(765.0, 770.0]" ~ "Excellent", 
  users$credit_score_bucket == "(770.0, 775.0]" ~ "Excellent",
  users$credit_score_bucket == "(775.0, 780.0]" ~ "Excellent",
  users$credit_score_bucket == "(780.0, 785.0]" ~ "Excellent",
  users$credit_score_bucket == "(785.0, 790.0]" ~ "Excellent",
  users$credit_score_bucket == "(790.0, 795.0]" ~ "Excellent",
  users$credit_score_bucket == "(795.0, 800.0]" ~ "Excellent")

table(users$credit_score, useNA = "always")

################Attrribution Modeling#################

table(fSession$login_platform)
#Using Markov Chain Modeling
install.packages("purrrlyr")

library(tidyverse)
library(reshape2)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
install.packages("ChannelAttribution")
library(ChannelAttribution)
install.packages("markovchain")
library(markovchain)
library(visNetwork)
install.packages("expm")
library(expm)
library(stringr)
library(purrrlyr)

#We'll do attribution modelling based on credit scores
users_excellent <- users %>%
  filter(credit_score == "Excellent")
users_good <- users %>%
  filter(credit_score == "Good")
users_fair <- users %>%
  filter(credit_score == "Fair")
users_poor <- users %>%
  filter(credit_score == "Poor")

#Starting with Excellent
fSession_poor <- fSession %>%
  filter(user_id %in% users_poor$user_id)

fSession_poor <- fSession_poor %>%
  mutate(action_page = paste(action_type,page_name,sep=" - "))
unique(fSession_poor$action_page)

appliers <- fSession_poor %>%
  filter(action_type == "CLICK_APPLY") %>%
  select(user_id)
appliers <- unique(appliers)

#select only those that applied on first session
fSession_poor <- fSession_poor %>% 
  filter(user_id %in% appliers$user_id)
#sort by user id and timep stamp
fSession_poor$action_timestamp <- as.POSIXct(fSession_poor$action_timestamp)
fSession_poor <- fSession_poor %>% 
  arrange(user_id, action_timestamp)

library(arules)
library(readxl)
library(arulesViz)
library(tidyverse)
library(knitr)
library(grid)
library(lubridate)
library(plyr)
#First convert dataframe into transaction data so all action-page
#are together in one transaction in a row
#Group data by user id
#Need grouping and apply a func on it and store outpput in another dataframe
#Can can be done by ddply - below combine all prods from one InvoiceNo and date
#in one row with each item separated by ,

library(plyr)

#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)

transactionData <- ddply(fSession_poor,c("user_id"), 
                         function(dfl)paste(dfl$action_page,
                                            collapse = ","))
#The R func paste() concatenates vectors to character and separated
#results using collapse=[any optional character string]. ','is used

glimpse(transactionData)

#user_id  is of not any use in rule mining so set them to Null

#set column userr_id of dataframe transactionData

transactionData$user_id <- NULL


#Rename column to items

colnames(transactionData) <- c("items")

#Show Dataframe transactionData

glimpse(transactionData)

#Format is called basket format. To store this data into .csv file. write.csv()

write.csv(transactionData,"basket_transactions4.csv",
          quote=FALSE, row.names=TRUE)

#the above shows the location of the file name, quote: if TRUE it will surround character
#or factor column with double quotes. If FALSE nothing will be quoted
#row.names: either a logical value indicating whether the row names of x are to be written
#along with x, or a character vector of row names to be written

#now load the data into an object of transaction class
#done by read.transactions of arules package

tr<-read.transactions("basket_transactions.csv", 
                      format ='basket', sep=',')



'trObj<-as(dataframe.dat, "transactions")'

#view the tr transaction object
tr



summary(tr)


#install color package of R; include library RColorBrewer
install.packages("RColorBrewer")

library(RColorBrewer)
if(!require("RColorBrewer")) {install.packages("RColorBrewer")
  library(RColorBrewer)}

itemFrequencyPlot(tr,topN=20,type="absolute", col=brewer.pal(8,'Pastel2'),
                  main="Absolute Item Frequency Plot")



itemFrequencyPlot(tr,topN=15,type="relative", col=brewer.pal(8,'Pastel2'),
                  main="Relative Item Frequency Plot")


association.rules <-apriori(tr, parameter = list(supp=0.001,conf=0.8,maxlen=10))

summary(association.rules)


#print top 10 rules
inspect(association.rules[1:10])




shorter.association.rules <-apriori(tr, parameter = list(supp=0.001, conf=0.8, maxlen=5))


subset.rules <- which(colSums(is.subset(shorter.association.rules, shorter.association.rules))>1)

length(subset.rules) #>3913

subset.association.rules. <- shorter.association.rules[-subset.rules] #remove subset rules

#Finding rules related to given items


metal.association.rules <- apriori(tr,parameter = list(supp=0.001, conf=0.8),
                                   appearance = list(default='lhs',rhs="CLICK_APPLY - AOOP_OVERVIEW"))

inspect(head(metal.association.rules))



metal.association.rules <- apriori(tr,parameter = list(supp=0.001, conf=0.8),
                                   appearance = list(lhs='CLICK_APPLY - AOOP_OVERVIEW',default="rhs"))

inspect(head(metal.association.rules))

#VISUALIZING ASSOCIATION RULES


subRules <-shorter.association.rules[quality(shorter.association.rules)$confidence>0.4]
plot(subRules)


plot(subRules, method="two-key plot")


#Interactive scatter plot-to hover over each rule and see all measures
plotly_arules(subRules)



top10subRules <- head(subRules, n=10, by='confidence')

#make interactive plots using engine=htmlwidget parameter in plot

plot(top10subRules, method="graph", engine="htmlwidget")

saveAsGraph(head(subRules,n=1000, by="lift"), file="rules.graphml")



subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")


setwd("~/TRENDI/Datathon/dataset")
profile.data <- read.csv("user_profile.csv")
engagement.data <- read.csv("user_engagement.csv")
session.data <- read.csv("first_session.csv")

### PCA Analysis ###

###Bucket credit scores
library(dplyr)
profile.data$credit_score <- case_when(
  profile.data$credit_score_bucket == "(495.0, 500.0]" ~ "Poor",
  profile.data$credit_score_bucket == "(500.0, 505.0]" ~ "Poor",
  profile.data$credit_score_bucket == "(505.0, 510.0]" ~ "Poor",
  profile.data$credit_score_bucket == "(510.0, 515.0]" ~ "Poor",
  profile.data$credit_score_bucket == "(515.0, 520.0]" ~ "Poor", 
  profile.data$credit_score_bucket == "(520.0, 525.0]" ~ "Poor",
  profile.data$credit_score_bucket == "(525.0, 530.0]" ~ "Poor",
  profile.data$credit_score_bucket == "(530.0, 535.0]" ~ "Poor",
  profile.data$credit_score_bucket == "(535.0, 540.0]" ~ "Poor",
  profile.data$credit_score_bucket == "(540.0, 545.0]" ~ "Poor",
  profile.data$credit_score_bucket == "(545.0, 550.0]" ~ "Poor",
  profile.data$credit_score_bucket == "(550.0, 555.0]" ~ "Fair",
  profile.data$credit_score_bucket == "(555.0, 560.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(560.0, 565.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(565.0, 570.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(570.0, 575.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(575.0, 580.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(580.0, 585.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(585.0, 590.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(590.0, 595.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(595.0, 600.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(600.0, 605.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(605.0, 610.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(610.0, 615.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(615.0, 620.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(620.0, 625.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(625.0, 630.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(630.0, 635.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(635.0, 640.0]" ~ "Fair", 
  profile.data$credit_score_bucket =="(640.0, 645.0]" ~ "Good", 
  profile.data$credit_score_bucket =="(645.0, 650.0]" ~ "Good",
  profile.data$credit_score_bucket == "(650.0, 655.0]" ~ "Good",
  profile.data$credit_score_bucket == "(655.0, 660.0]" ~ "Good", 
  profile.data$credit_score_bucket == "(660.0, 665.0]" ~ "Good", 
  profile.data$credit_score_bucket == "(665.0, 670.0]" ~ "Good",
  profile.data$credit_score_bucket == "(670.0, 675.0]" ~ "Good", 
  profile.data$credit_score_bucket == "(675.0, 680.0]" ~ "Good", 
  profile.data$credit_score_bucket == "(680.0, 685.0]" ~ "Good", 
  profile.data$credit_score_bucket == "(685.0, 690.0]" ~ "Good", 
  profile.data$credit_score_bucket == "(690.0, 695.0]" ~ "Good", 
  profile.data$credit_score_bucket == "(695.0, 700.0]" ~ "Good",
  profile.data$credit_score_bucket == "(700.0, 705.0]" ~ "Good",
  profile.data$credit_score_bucket == "(705.0, 710.0]" ~ "Good", 
  profile.data$credit_score_bucket == "(710.0, 715.0]" ~ "Good", 
  profile.data$credit_score_bucket == "(715.0, 720.0]" ~ "Good", 
  profile.data$credit_score_bucket == "(720.0, 725.0]" ~ "Excellent", 
  profile.data$credit_score_bucket == "(725.0, 730.0]" ~ "Excellent", 
  profile.data$credit_score_bucket == "(730.0, 735.0]" ~ "Excellent", 
  profile.data$credit_score_bucket == "(735.0, 740.0]" ~ "Excellent", 
  profile.data$credit_score_bucket == "(740.0, 745.0]" ~ "Excellent", 
  profile.data$credit_score_bucket == "(745.0, 750.0]" ~ "Excellent",
  profile.data$credit_score_bucket == "(750.0, 755.0]" ~ "Excellent", 
  profile.data$credit_score_bucket == "(755.0, 760.0]" ~ "Excellent", 
  profile.data$credit_score_bucket == "(760.0, 765.0]" ~ "Excellent", 
  profile.data$credit_score_bucket == "(765.0, 770.0]" ~ "Excellent", 
  profile.data$credit_score_bucket == "(770.0, 775.0]" ~ "Excellent", 
  profile.data$credit_score_bucket == "(750.0, 755.0]" ~ "Excellent", 
  profile.data$credit_score_bucket == "(755.0, 760.0]" ~ "Excellent", 
  profile.data$credit_score_bucket == "(760.0, 765.0]" ~ "Excellent", 
  profile.data$credit_score_bucket == "(765.0, 770.0]" ~ "Excellent", 
  profile.data$credit_score_bucket == "(770.0, 775.0]" ~ "Excellent",
  profile.data$credit_score_bucket == "(775.0, 780.0]" ~ "Excellent",
  profile.data$credit_score_bucket == "(780.0, 785.0]" ~ "Excellent",
  profile.data$credit_score_bucket == "(785.0, 790.0]" ~ "Excellent",
  profile.data$credit_score_bucket == "(790.0, 795.0]" ~ "Excellent",
  profile.data$credit_score_bucket == "(795.0, 800.0]" ~ "Excellent",)

### Left join profile data on engagement data
p.e.data <- merge(x = engagement.data, y = profile.data, all.x = TRUE)

### Left join profile data on session data
p.s.data <- merge(x = session.data, y = profile.data, all.x = TRUE)

### PCA

e.freq <- p.e.data %>% group_by(user_id) %>% summarize(count=n())
p.e.data <- merge(x = p.e.data, y = e.freq, all.x = TRUE)

frequent.users.data <- p.e.data %>%
  filter(count > 20)

pca.fre.eng <- select(p.e.data, credit_score, total_tradelines_open_balance, gender, is_homeowner, login_platform)
pca.fre.eng <- pca.fre.eng[complete.cases(pca.fre.eng),]

pca.fre.eng$credit_score <- case_when(
  pca.fre.eng$credit_score == "Poor" ~ 1,
  pca.fre.eng$credit_score == "Fair" ~ 2,
  pca.fre.eng$credit_score == "Good" ~ 3,
  pca.fre.eng$credit_score == "Excellent" ~ 4)

pca.fre.eng$gender <- case_when(
  pca.fre.eng$gender == "Male" ~ 0,
  pca.fre.eng$gender == "Female" ~ 1,
  pca.fre.eng$gender == "Unisex" ~ 2)

pca.fre.eng$is_homeowner <- case_when(
  pca.fre.eng$is_homeowner == "False" ~ 0,
  pca.fre.eng$is_homeowner == "True" ~ 1)

pca.fre.eng$login_platform <- case_when(
  pca.fre.eng$login_platform == "Web" ~ 0,
  pca.fre.eng$login_platform == "Mobile Web" ~ 1,
  pca.fre.eng$login_platform == "Mobile App" ~ 2)

pca.fre.eng <- pca.fre.eng[complete.cases(pca.fre.eng),]

pca <- prcomp(pca.fre.eng, center = TRUE, scale=TRUE)
pca.fre.eng
plot(pca.fre.eng)
