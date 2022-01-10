
##########
##Author: Hannah C. Haacke
## year: 2022
##Published in: Population Space and Place
##Name of the paper: How to derive spatial agents: A mixed-method approach to model an elderly population with scarce data


library(foreign)
library(functional)
library(cluster)
library(NbClust)
library(ggplot2)
require(reshape2)
library(RColorBrewer)
library(colorRamps)
library(mclust)
library(fpc)
library(ggthemes)
library(scales)
library(dplyr)

setwd("E:/PhD/Ablauf ABM/1. Cluster Analysis")
my.data <- read.dta("SUF_DEAS_2014_1-0_de_Stata.dta")

#Create new matrix
#Find Berlin data (because the data is available for entire Germany, it has to be filtered first)
my.data <- my.data[my.data[, "bland_14"] == '11. Berlin',]

#Replace non-valid data (NA)
my.data[, grepl("-", my.data)] <- NA

###Exclude all ages below 65
my.data <- my.data[my.data$alter_14 >= 65, ]

#get certain columns
berlin.data <- data.frame(my.data$natdeutsch_14 #Nationality
                          , my.data$migrat_14 # Migration background
                          , my.data$alter_14 #Age in years, 2014
                          , my.data$hc1 # Gender 1 -> male
                          , my.data$hc25a # Country of birth
                          , my.data$hc27 # Highest school-leaving qualification
                          , my.data$hc29 # Vocational training and studies in Germany ("y"/"n")
                          , my.data$hc30 # Highest completion of trainings
                          , my.data$hc100 # Receiving pension
                          , my.data$hc102 # Employment as pensioneer
                          , my.data$hc106 # Situation bevor the first pension benefit
                          , my.data$hc116 # Branch of the last occupation
                          , my.data$hc200 # Marital status
                          , my.data$hc245 # Assessment health status partner
                          , my.data$hc280a # Confirmation: Married, living separately 
                          , my.data$hc290 # Assessment of living situation without partner
                          , my.data$hc300 # Number of own children or children raised by respondents
                          , my.data$hc317 # Great-grandchildren present
                          , my.data$hc319 # Assessment of the current relationship with the family
                          , my.data$hc322a # Type of house
                          , my.data$hc323 # Number of persons in household
                          , my.data$hc334 # Assessment of the current housing situation
                          , my.data$hc337_1 #Number of rooms
                          , my.data$hc339 # Type of living
                          , my.data$hc410 # Participation in groups: in general
                          , my.data$hc425_6 # Frequency last 12 months: friends and acquaintance
                          , my.data$hc433 # Frequency meeting certain groups of people
                          , my.data$hc700 # Are there persons that you can ask for advice
                          
                          , my.data$hc101 # Current employment status
                          , my.data$hc129 # Changes through transition to retirement
                          , my.data$hc136 # Claim to company pension
                          , my.data$hc211 # Partner has same nationality
                          , my.data$hc316k # Importance to become a granparent
                          , my.data$hc345 # How did you get the house/flat
                          , my.data$hc526a_1 # Personen who support the target person
                          , my.data$hc532 # Regular  Regular receipt of long-term care insurance benefits
                          , my.data$hc324b_3 # 3. Person in household
                          , my.data$hc25b # Year of arrival in Germany
                          , my.data$hc25c2 # mainly grew up in ... other country
                          
                          , my.data$hc25c # mainly grew up
                          , my.data$hc25f # German nationality since
                          , my.data$hc123 # Duration of employment until pension: number of years
                          , my.data$hc316j # Importance of the role as grandparent
                          , my.data$hc318_13 # No relatives still living
                          , my.data$hc332 # Duration since the target person lives in the city
                          , my.data$hc333 # Duration since the target person lives in the flat
                          , my.data$hc341_1 # Amount monthly rent
                          , my.data$hc348_1 # Known in the city: meeting place
                          , my.data$hc348_2 # Known in the city: Senior Citizens Advice Center
                          , my.data$hc348_3 # Known in the city: Care advice center
                          , my.data$hc404 # Participation in groups for elderly pensioner
                          , my.data$hc501 # Assessment of current state of health
                          , my.data$hc600 # Naming people in the personal network
                          , my.data$hc802neu #Monthly net household income
                          , my.data$hc804 # Assessment standard of living
                          , my.data$hc324_3 # 2. person in household
                          , my.data$hc423_1 # childcare: grandchidlren
                          , my.data$hc434_1 # Growing older for most people: everything gets worse
                          , my.data$hc434_2 # Growing older for most people: as much momentum as before
                          , my.data$hc434_3 # Growing older for most people: less useful
                          , my.data$hc434_4 # Growing older for most people: Life better than expected
                          , my.data$hc434_5 # Growing older for most people: just as happy as before
                          )

####Delete columns(variables) that no not contain enough values
na.threshold <- nrow(berlin.data) - 47
##Delete
berlin.clean <- as.data.frame(berlin.data[, colSums(is.na(berlin.data)) <= na.threshold])

############First step for clustering
data.dist <- daisy(berlin.clean, metric = "gower") #Calculation of distance. gower -> mixed data type

#Hierarchical clustering

##hclust
fit.average <- hclust(data.dist, method = "ward.D2")
plot(fit.average, hang = -1, cex = .8) # Plot


###scree plot to decide the number of clusters
plot(fit.average$height, type="o", ylab = "Sum of Squared within Group Variance", xlab ="Number Agglomeration-Steps")

##set the number of cluster
k <- 8

clusters <- cutree(fit.average, k) 
##number of observations per cluster
table(clusters)

##############Calculate percentage#########################################

# Personal data (gender, nationality, migration background, marital status)
cluster.table.personal <- with(berlin.clean, cbind(table(clusters, my.data.hc1), table(clusters, my.data.natdeutsch_14),
                                                   table(clusters, my.data.migrat_14),
                                                   table(clusters, my.data.hc200)))

get.hc1 <- cluster.table.personal[,6:7]
sum.hc1 <- rowSums(get.hc1)
entire.table.hc1 <- cbind(get.hc1, sum.hc1)
entire.per.table.hc1 <- apply(get.hc1, 2, function(x) 100/entire.table.hc1[, 3] * x)

get.natdeutsch <- cluster.table.personal[, 18:19]
sum.natdeutsch <- rowSums(get.natdeutsch)
entire.table.natdeutsch <- cbind(get.natdeutsch, sum.natdeutsch)
entire.per.table.natdeutsch <- apply(get.natdeutsch, 2, function(x) 100/entire.table.natdeutsch[, 3] * x)

get.migra <- cluster.table.personal[, 31:32]
sum.migra <- rowSums(get.migra)
entire.table.migra <- cbind(get.migra, sum.migra)
entire.per.table.migra <- apply(get.migra, 2, function(x) 100/entire.table.migra[, 3] * x)

get.beziehung <- cluster.table.personal[, 47:51]
sum.beziehung <- rowSums(get.beziehung)
entire.table.beziehung <- cbind(get.beziehung, sum.beziehung)
entire.per.table.beziehung <- apply(get.beziehung, 2, function(x) 100/entire.table.beziehung[, 6] * x)

personal_perc <- cbind(entire.per.table.hc1, entire.per.table.natdeutsch, entire.per.table.migra, entire.per.table.beziehung)

colnames(personal_perc) <- c("male", "female", "immigrants", "Germans", "without migr. BG", "with migr. BG", "married, liv. together", 
                             "married, not liv. tog.", "divorced", "widowed", "single")

##Health status partner
cluster.table.partner <- with(berlin.clean, cbind(table(clusters, my.data.hc245)))

get.gesPartner <- cluster.table.partner[, 6:10]
sum.gesPartner <- rowSums(get.gesPartner)
entire.table.gesPartner <- cbind(get.gesPartner, sum.gesPartner)
entire.per.table.gesPartner <- apply(get.gesPartner, 2, function(x) 100/entire.table.gesPartner[, 6] * x)

cluster.table.gesPartner.perc <- entire.per.table.gesPartner

#####Relationship to the family
cluster.table.bezVerwandte <- with(berlin.clean, cbind(table(clusters, my.data.hc319)))

get.bezVerwandte <- cluster.table.bezVerwandte[, 6:10]
sum.bezVerwandte <- rowSums(get.bezVerwandte)
entire.table.bezVerwandte <- cbind(get.bezVerwandte, sum.bezVerwandte)
entire.per.table.bezVerwandte <- apply(get.bezVerwandte, 2, function(x) 100/entire.table.bezVerwandte[, 6] * x)

cluster.table.bezVerwandte.perc <- entire.per.table.bezVerwandte

####Herkunftsländer
cluster.table.Herkunft <- with(berlin.clean, cbind(table(clusters, my.data.hc25a)))

get.Herkunft <- cluster.table.Herkunft[, 6:9]
sum.Herkunft <- rowSums(get.Herkunft)
entire.table.Herkunft <- cbind(get.Herkunft, sum.Herkunft)
entire.per.table.Herkunft <- apply(get.Herkunft, 2, function(x) 100/entire.table.Herkunft[, 5] * x)

cluster.table.Herkunft.perc <- entire.per.table.Herkunft

# Educational information (Highest school education, vocational education in Germany)
cluster.table.school <- with(berlin.clean, cbind(table(clusters, my.data.hc27), table(clusters, my.data.hc29)))

get.Schulbildung <- cluster.table.school[, 7:14]
sum.Schulbildung <- rowSums(get.Schulbildung)
entire.table.Schulbildung <- cbind(get.Schulbildung, sum.Schulbildung)
entire.per.table.Schulbildung <- apply(get.Schulbildung, 2, function(x) 100/entire.table.Schulbildung[, 9] * x)

get.Ausbildung <- cluster.table.school[, 28:31]
sum.Ausbildung <- rowSums(get.Ausbildung)
entire.table.Ausbildung <- cbind(get.Ausbildung, sum.Ausbildung)
entire.per.table.Ausbildung <- apply(get.Ausbildung, 2, function(x) 100/entire.table.Ausbildung[, ncol(entire.table.Ausbildung)] * x)

ausbildung_perc <- cbind(entire.per.table.Schulbildung, entire.per.table.Ausbildung)

colnames(ausbildung_perc) <- c("Secondary school (Hauptschule)", "Secondary school (Realschulabschluss)", "8th grade (GDR)", "10th grade (GDR)", 
                               "Fachhochschulreife", "Abitur", "Others", "No high school diploma", "Ed. in Germany", "Ed. in G. and other country",
                               "Ed. in other country", "No apprentenship or studies")


# Income (Old age pension, Situation before the first pension withdrawal)
cluster.table.income <- with(berlin.clean, cbind(table(clusters, my.data.hc100),  table(clusters, my.data.hc106)))

get.Altersrente <- cluster.table.income[, 6:7]
sum.Altersrente <- rowSums(get.Altersrente)
entire.table.Altersrente <- cbind(get.Altersrente, sum.Altersrente)
entire.per.table.Altersrente <- apply(get.Altersrente, 2, function(x) 100/entire.table.Altersrente[, 3] * x)

get.Beruf <- cluster.table.income[, 21:29]
sum.Beruf <- rowSums(get.Beruf)
entire.table.Beruf <- cbind(get.Beruf, sum.Beruf)
entire.per.table.Beruf <- apply(get.Beruf, 2, function(x) 100/entire.table.Beruf[, ncol(entire.table.Beruf)] * x)

einkommen_perc <- cbind(entire.per.table.Altersrente, entire.per.table.Beruf)

colnames(einkommen_perc) <- c("Pension", "No pension", "Employed", "Partial retirement", "Unemployed", "Early retirement", "Disability pension",
                              "Longer sick", "Sonst", "Homemaker", "Other")


# Employment (employed as pensioner, sector of the last company)
cluster.table.erwerbstaetigkeit <- with(berlin.clean, cbind(table(clusters, my.data.hc102), table(clusters, my.data.hc116)))

get.Erwerbstaetigkeit <- cluster.table.erwerbstaetigkeit[, 6:7]
sum.Erwerbstaetigkeit <- rowSums(get.Erwerbstaetigkeit)
entire.table.Erwerbstaetigkeit <- cbind(get.Erwerbstaetigkeit, sum.Erwerbstaetigkeit)
entire.per.table.Erwerbstaetigkeit <- apply(get.Erwerbstaetigkeit, 2, function(x) 100/entire.table.Erwerbstaetigkeit[, 3] * x)

get.Betrieb <- cluster.table.erwerbstaetigkeit[, 21:ncol(cluster.table.erwerbstaetigkeit)]
sum.Betrieb <- rowSums(get.Betrieb)
entire.table.Betrieb <- cbind(get.Betrieb, sum.Betrieb)
entire.per.table.Betrieb <- apply(get.Betrieb, 2, function(x) 100/entire.table.Betrieb[, ncol(entire.table.Betrieb)] * x)

cluster.table.erwerbstaetigkeit.perc <- cbind(entire.per.table.Erwerbstaetigkeit, entire.per.table.Betrieb)

# Partner (Marital status, number children)
cluster.table.kids <- with(berlin.clean, cbind(table(clusters, my.data.hc300)))

get.Kinder <- cluster.table.kids[, 1:ncol(cluster.table.kids)]
sum.Kinder <- rowSums(get.Kinder)
entire.table.Kinder <- cbind(get.Kinder, sum.Kinder)
entire.per.table.Kinder <- apply(get.Kinder, 2, function(x) 100/entire.table.Kinder[, ncol(entire.table.Kinder)] * x)

cluster.table.Kinder.perc <- cbind(entire.per.table.Kinder)


# Haushalt, wie viele Personen im Haushalt
cluster.table.wohnen <- with(berlin.clean, cbind(table(clusters, my.data.hc322a), table(clusters, my.data.hc323)))

get.wohnen <- cluster.table.wohnen[, 6:11]
sum.wohnen <- rowSums(get.wohnen)
entire.table.wohnen <- cbind(get.wohnen, sum.wohnen)
entire.per.table.wohnen <- apply(get.wohnen, 2, function(x) 100/entire.table.wohnen[, ncol(entire.table.wohnen)] * x)

get.PersHaushalt <- cluster.table.wohnen[, 21:ncol(cluster.table.wohnen)]
sum.PersHaushalt <- rowSums(get.PersHaushalt)
entire.table.PersHaushalt <- cbind(get.PersHaushalt, sum.PersHaushalt)
entire.per.table.PersHaushalt <- apply(get.PersHaushalt, 2, function(x) 100/entire.table.PersHaushalt[, ncol(entire.table.PersHaushalt)] * x)

wohnen_perc <- cbind(entire.per.table.wohnen, entire.per.table.PersHaushalt)

colnames(wohnen_perc) <- c("Private household", "Retirement home", "Residential complex w. assisted living", "Retirement home", "Nursing home",
                           "Care home", "2 Pers.", 
                           "3 Pers.", "4 Pers.", "5 Pers.", "Alone")

# who lives in the houshold and living situation
cluster.table.haushalt <- with(berlin.clean, cbind(table(clusters, my.data.hc324_3), table(clusters, my.data.hc334)))

get.Pers <- cluster.table.haushalt[, 1:80]
sum.Pers <- rowSums(get.Pers)
entire.table.Pers <- cbind(get.Pers, sum.Pers)
entire.per.table.Pers <- apply(get.Pers, 2, function(x) 100/entire.table.Pers[, 81] * x)

get.WohnSit <- cluster.table.haushalt[, 81:ncol(cluster.table.haushalt)]
sum.WohnSit <- rowSums(get.WohnSit)
entire.table.WohnSit <- cbind(get.WohnSit, sum.WohnSit)
entire.per.table.WohnSit <- apply(get.WohnSit, 2, function(x) 100/entire.table.WohnSit[, ncol(entire.table.WohnSit)] * x)

cluster.table.PersWohnen.perc <- cbind(entire.per.table.Pers, entire.per.table.WohnSit)

# Number rooms, ownership flat/rent
cluster.table.wohnsituation <- with(berlin.clean, cbind(table(clusters, my.data.hc337_1), table(clusters, my.data.hc339)))

get.Zimmer <- cluster.table.wohnsituation[, 1:6]
sum.Zimmer <- rowSums(get.Zimmer)
entire.table.Zimmer <- cbind(get.Zimmer, sum.Zimmer)
entire.per.table.Zimmer <- apply(get.Zimmer, 2, function(x) 100/entire.table.Zimmer[, 7] * x)

get.Mietverhaeltnis <- cluster.table.wohnsituation[, 12:16]
sum.Mietverhaeltnis <- rowSums(get.Mietverhaeltnis)
entire.table.Mietverhaeltnis <- cbind(get.Mietverhaeltnis, sum.Mietverhaeltnis)
entire.per.table.Mietverhaeltnis <- apply(get.Mietverhaeltnis, 2, function(x) 100/entire.table.Mietverhaeltnis[, ncol(entire.table.Mietverhaeltnis)] * x)

wohnstiuation_perc <- cbind(entire.per.table.Zimmer, entire.per.table.Mietverhaeltnis)
colnames(wohnstiuation_perc) <- c("1 room", "2 rooms", "3 rooms", "4 rooms", "5 rooms", "6 rooms", "Owner",  "Main tenant", "Sonst", "Rent-free",
                                  "Other")

##Network

cluster.table.gruppen <- with(berlin.clean, cbind(table(clusters, my.data.hc410), table(clusters, my.data.hc425_6)))

get.teilnaAllg <- cluster.table.gruppen[, 6:7]
sum.teilnaAllg <- rowSums(get.teilnaAllg)
entire.table.teilnaAllg <- cbind(get.teilnaAllg, sum.teilnaAllg)
entire.per.table.teilnaAllg <- apply(get.teilnaAllg, 2, function(x) 100/entire.table.teilnaAllg[, 3] * x)

get.gruTreff <- cluster.table.gruppen[, 21:26]
sum.gruTreff <- rowSums(get.gruTreff)
entire.table.gruTreff <- cbind(get.gruTreff, sum.gruTreff)
entire.per.table.gruTreff <- apply(get.gruTreff, 2, function(x) 100/entire.table.gruTreff[, ncol(entire.table.gruTreff)] * x)

teilnahmeGruppen_perc <- cbind(entire.per.table.teilnaAllg, entire.per.table.gruTreff)
colnames(teilnahmeGruppen_perc) <- c("Meets groups", "Does not meet groups", "Meets friends and relatives: daily", "Several times per week",
                                     "Once per week", "1 - 3 times per month", "Seldom", "Never")

#Relatives
cluster.table.freundeVerwandte <- with(berlin.clean, cbind(table(clusters, my.data.hc433), table(clusters, my.data.hc700)))

get.Freunde <- cluster.table.freundeVerwandte[, 1:12]
sum.Freunde <- rowSums(get.Freunde)
entire.table.Freunde <- cbind(get.Freunde, sum.Freunde)
entire.per.table.Freunde <- apply(get.Freunde, 2, function(x) 100/entire.table.Freunde[, 13] * x)

get.Rat <- cluster.table.freundeVerwandte[, 13:ncol(cluster.table.freundeVerwandte)]
sum.Rat <- rowSums(get.Rat)
entire.table.Rat <- cbind(get.Rat, sum.Rat)
entire.per.table.Rat <- apply(get.Rat, 2, function(x) 100/entire.table.Rat[, ncol(entire.table.Rat)] * x)

cluster.table.freundeVerwandte.perc <- cbind(entire.per.table.Freunde, entire.per.table.Rat)

# , my.data$hc25c # country in which the target person mainly grew up
cluster.aufgewachsen <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc25c)) # 6 = in eastern Germany/GDR, 7 = in west Germany/west Berlin, 8 = In the former German eastern territories, 9 = in another country

table.aufgewachsen <- table(cluster.aufgewachsen)
per.aufgew <- apply(table.aufgewachsen,1, function(x) 100/sum(x) * x)
per.aufgew <- t(per.aufgew)
colnames(per.aufgew) <- c("in eastern Germany/GDR", "in west Germany/west Berlin", "In the former German eastern territories", "in another country")

# , my.data$hc316j #Importance of role as grandparent
cluster.groseltern <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc316j)) # 6 = very important, 7 = important, 8 = less important , 9 = not important

table.groseltern <- table(cluster.groseltern)
per.grosseltern <- t(apply(table.groseltern, 1,  function(x) 100/sum(x) * x))
colnames(per.grosseltern) <- c("very important", "important", "less important" , "not important")

# , my.data$hc348_1 #Known in city:  Meeting place / multigenerational building
cluster.begegnung <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc348_1)) # 6 = yes, 7 = no

table.begegnung <- table(cluster.begegnung)
per.begegnung <- t(apply(table.begegnung, 1,  function(x) 100/sum(x) * x))
colnames(per.begegnung) <- c("meeting place known", "meeting place unknown")

# , my.data$hc348_2 # Known in city: Senior Citizens Advice Centre
cluster.seniorenb <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc348_2)) # 6 = yes, 7 = no
table.seniorenb <- table(cluster.seniorenb)
per.seniorenb <- t(apply(table.seniorenb, 1,  function(x) 100/sum(x) * x))
colnames(per.seniorenb) <- c("senior citizens place known", "senior citizens place unknown")

# , my.data$hc348_3 # Known in city:  Care advice centre
cluster.pflegeber <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc348_3))  # 6 = yes, 7 = no
table.pflegeber <- table(cluster.pflegeber)
per.pflegeber <- t(apply(table.pflegeber, 1,  function(x) 100/sum(x) * x))
colnames(per.pflegeber) <- c("care advice known", "care advice unknown")

# , my.data$hc404 # Participation in groups for older retired people 
cluster.gruppenAeltere <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc404))  # 6 = yes, 7 = no
table.gruppenAeltere <- table(cluster.gruppenAeltere)
per.gruppenAeltere <- t(apply(table.gruppenAeltere, 1,  function(x) 100/sum(x) * x))
colnames(per.gruppenAeltere) <- c("Groups for elderly known", "Groups for elderly unknown")

# , my.data$hc501 # Assessment of current state of health
cluster.gesundheit <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc501)) # 6 = very good, 7 = good, 8 = intermediate, 9 = bad, 10 = very bad
table.gesundheit <- table(cluster.gesundheit)
per.gesundheit <- t(apply(table.gesundheit, 1,  function(x) 100/sum(x) * x))
colnames(per.gesundheit) <- c("very good", "good", "intermediate", "bad", "very bad")

# , my.data$hc600 # Naming people in the personal network
cluster.PersNetzwerk <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc600)) # 6 = ja, 7 = nein
table.PersNetzwerk <- table(cluster.PersNetzwerk)
per.PersNetzwerk <- t(apply(table.PersNetzwerk, 1,  function(x) 100/sum(x) * x))
colnames(per.PersNetzwerk) <- c("People in the personal network available", "People in the personal network not available")

# , my.data$hc804 # Assessment standard of living
cluster.lebensstandard <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc804)) # 6 = sehr gut, 7 = gut, 8 = mittel, 9 = schlecht, 10 = sehr schlecht
table.lebensstandard <- table(cluster.lebensstandard)
per.lebensstandard <- t(apply(table.lebensstandard, 1,  function(x) 100/sum(x) * x))
colnames(per.lebensstandard) <- c("very good", "good", "intermediate", "bad", "very bad")

# , my.data$hc324_3 # 2. person in household
cluster.2.pers.hh <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc324_3)) # 11 = (Ehe-Partner), 16 = Ex-Partner, 17 = 1. Kind, 18 = 2. Kind, 20 = 4. Kind, 77 = Andere Person
table.2.pers.hh <- table(cluster.2.pers.hh)
per.2.pers.hh <- t(apply(table.2.pers.hh, 1,  function(x) 100/sum(x) * x))
colnames(per.2.pers.hh) <- c("2. Person in household: Partner", "Ex-Partner", "1. child", "2. child", "4. child", "Other person")

# , my.data$hc423_1 # Childcare: Grandchildren
cluster.EnkelBetreuung <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc423_1)) # 6 = not named, 7 = named
table.EnkelBetreuung <- table(cluster.EnkelBetreuung)
per.EnkelBetreuung <- t(apply(table.EnkelBetreuung, 1,  function(x) 100/sum(x) * x))
colnames(per.EnkelBetreuung) <- c("Grandchild care: not named", "named")

# , my.data$hc434_1 # Ageing for most people: everything worse
cluster.aelterwerden <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc434_1)) # 6 = true, 7 = rather true, 8 = rather not true, 9 = not true
table.aelterwerden <- table(cluster.aelterwerden)
per.aelterwerden <- t(apply(table.aelterwerden, 1,  function(x) 100/sum(x) * x))
colnames(per.aelterwerden) <- c("growing older: everything gets worse: true", "rather true", "rather not true", "not true")

# , my.data$hc434_2 # Ageing for most people: as much momentum as before
cluster.schwung <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc434_2)) # 6 = true, 7 = rather true, 8 = rather not true, 9 = not true
table.schwung <- table(cluster.schwung)
per.schwung <- t(apply(table.schwung, 1,  function(x) 100/sum(x) * x))
colnames(per.schwung) <- c("growing older: as much momentum as before: true", "rather true", "rather not true", "not true")

# , my.data$hc434_3 # Ageing for most people: Less useful
cluster.nuetzlich <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc434_3)) # 6 = true, 7 = rather true, 8 = rather not true, 9 = not true
table.nuetzlich <- table(cluster.nuetzlich)
per.nuetzlich <- t(apply(table.nuetzlich, 1,  function(x) 100/sum(x) * x))
colnames(per.nuetzlich) <- c("Ageing for most people: Less useful: true", "rather true", "rather not true", "not true")

# , my.data$hc434_4 # Ageing for most people: Life better than expected
cluster.lebenBesser <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc434_4)) # 6 = true, 7 = rather true, 8 = rather not true, 9 = not true
table.lebenBesser <- table(cluster.lebenBesser)
per.lebenBesser <- t(apply(table.lebenBesser, 1,  function(x) 100/sum(x) * x))
colnames(per.lebenBesser) <- c( "Ageing for most people: Life better than expected: true", "rather true", "rather not true", "not true")

# , my.data$hc434_5 # Ageing for most people: as happy as before
cluster.gluecklich <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc434_5)) # 6 = true, 7 = rather true, 8 = rather not true, 9 = not true
table.gluecklich <- table(cluster.gluecklich)
per.gluecklich <- t(apply(table.gluecklich, 1,  function(x) 100/sum(x) * x))
colnames(per.gluecklich) <- c("Ageing for most people: as happy as before: true", "rather true", "rather not true", "not true")

# , my.data$hc25f # German nationality since
berlin.clean$my.data.hc25f[berlin.clean$my.data.hc25f <= 1945] <- "before the end of WW2"
berlin.clean$my.data.hc25f[berlin.clean$my.data.hc25f > 1945 & berlin.clean$my.data.hc25f < 2000] <- "after WW2" 
berlin.clean$my.data.hc25f[berlin.clean$my.data.hc25f == 9996 ] <- "always"

cluster.staats <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc25f))
table.staats <- table(cluster.staats)
per.staats <- t(apply(table.staats, 1,  function(x) 100/sum(x) * x))

# , my.data$hc123 # Duration of employment until pension: number years
berlin.clean$my.data.hc123[berlin.clean$my.data.hc123 < 30] <- "0"
berlin.clean$my.data.hc123[berlin.clean$my.data.hc123 >= 30] <- "employment since 30 y"

cluster.erwerbstaetigk <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc123))
table.erwerbstaetigk <- table(cluster.erwerbstaetigk)
per.erwerbstaetigk <- t(apply(table.erwerbstaetigk, 1,  function(x) 100/sum(x) * x))

colnames(per.erwerbstaetigk) <- c("employment since less than 30 years", "employment since 30 years")

# , my.data$hc332 # Duration since target person has lives in this place - year
berlin.clean$my.data.hc332[berlin.clean$my.data.hc332 <= 1954] <- "> 60 y in Berlin"
berlin.clean$my.data.hc332[berlin.clean$my.data.hc332 > 1954 & berlin.clean$my.data.hc332 <= 1984] <- "30-60 y in B"
berlin.clean$my.data.hc332[berlin.clean$my.data.hc332 > 1984 & berlin.clean$my.data.hc332 <= 2004] <- "10-30 y in B"
berlin.clean$my.data.hc332[berlin.clean$my.data.hc332 > 2004  & berlin.clean$my.data.hc332 <= 2014 ] <- "< 10 y in B" 

cluster.jahreB <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc332))
table.jahreB <- table(cluster.jahreB)
per.jahreB <- t(apply(table.jahreB, 1,  function(x) 100/sum(x) * x))

# , my.data$hc333 # Duration since target person has lives in this flat - years
berlin.clean$my.data.hc333[berlin.clean$my.data.hc333 <= 1954] <- "> 60 years in flat"
berlin.clean$my.data.hc333[berlin.clean$my.data.hc333 > 1954 & berlin.clean$my.data.hc333 <= 1984] <- "30-60 years in flat"
berlin.clean$my.data.hc333[berlin.clean$my.data.hc333 > 1984 & berlin.clean$my.data.hc333 <= 2004] <- "10-30 years in flat"
berlin.clean$my.data.hc333[berlin.clean$my.data.hc333 > 2004  & berlin.clean$my.data.hc333 <= 2014 ] <- "< 10 years in flat"

cluster.jahreW <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc333))
table.jahreW <- table(cluster.jahreW)
per.jahreW <- t(apply(table.jahreW, 1,  function(x) 100/sum(x) * x))

# , my.data$hc341_1 # Amount monthly rent in Euro
berlin.clean$my.data.hc341_1 <- as.numeric(berlin.clean$my.data.hc341_1)

berlin.clean$my.data.hc341_1[berlin.clean$my.data.hc341_1 > 1000 & berlin.clean$my.data.hc341_1 <= 5000] <- "rent > 1000???"
berlin.clean$my.data.hc341_1[as.numeric(berlin.clean$my.data.hc341_1) > 750 & as.numeric(berlin.clean$my.data.hc341_1) < 1001] <- "rent 750-1000???"
berlin.clean$my.data.hc341_1[berlin.clean$my.data.hc341_1 <= 500] <- "rent < 500???"
berlin.clean$my.data.hc341_1[berlin.clean$my.data.hc341_1 > 500 & berlin.clean$my.data.hc341_1 <= 750] <- "rent 500-750???"
berlin.clean$my.data.hc341_1[berlin.clean$my.data.hc341_1 == 95] <- "rent < 500???"

cluster.miete <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc341_1))
table.miete <- table(cluster.miete)
per.miete <- t(apply(table.miete, 1,  function(x) 100/sum(x) * x))

# , my.data$hc802neu # Monthly net household income

berlin.clean$my.data.hc802neu[berlin.clean$my.data.hc802neu < 700] <- "Net household income <700"
berlin.clean$my.data.hc802neu[as.numeric(berlin.clean$my.data.hc802neu) >= 700 & as.numeric(berlin.clean$my.data.hc802neu) <=1100] <- "Net household income 700-1100"
berlin.clean$my.data.hc802neu[berlin.clean$my.data.hc802neu > 1100 & berlin.clean$my.data.hc802neu <= 1600] <- "Net household incomek 1100 - 1600"
berlin.clean$my.data.hc802neu[berlin.clean$my.data.hc802neu > 1600 & berlin.clean$my.data.hc802neu <= 1800] <- "Net household income 1600-1800 (average B)"
berlin.clean$my.data.hc802neu[berlin.clean$my.data.hc802neu > 1800 & berlin.clean$my.data.hc802neu <= 3000] <- "Net household income 1800 - 3000"
berlin.clean$my.data.hc802neu[as.numeric(berlin.clean$my.data.hc802neu) > 3000 & as.numeric(berlin.clean$my.data.hc802neu) < 300000] <- "Net household income > 3000"

cluster.haushaltseink <- as.data.frame(cbind(clusters, berlin.clean$my.data.hc802neu))
table.haushaltseink <- table(cluster.haushaltseink)
per.haushaltseink <- t(apply(table.haushaltseink, 1,  function(x) 100/sum(x) * x))

##########################Save as table ################################################

data.table <- cbind(personal_perc,
                    cluster.table.gesPartner.perc,
                    cluster.table.bezVerwandte.perc,
                    cluster.table.Herkunft.perc,
                    ausbildung_perc,
                    einkommen_perc,
                    cluster.table.erwerbstaetigkeit.perc,
                    cluster.table.Kinder.perc,
                    wohnen_perc,
                    cluster.table.PersWohnen.perc,
                    wohnstiuation_perc,
                    teilnahmeGruppen_perc,
                    cluster.table.freundeVerwandte.perc,
                    per.aufgew,
                    per.grosseltern,
                    per.begegnung,
                    per.seniorenb,
                    per.pflegeber,
                    per.gruppenAeltere,
                    per.gesundheit,
                    per.PersNetzwerk,
                    per.lebensstandard,
                    per.2.pers.hh,
                    per.EnkelBetreuung,
                    per.aelterwerden,
                    per.schwung,
                    per.nuetzlich,
                    per.lebenBesser,
                    per.gluecklich,
                    per.staats,
                    per.erwerbstaetigk,
                    per.jahreB,
                    per.jahreW,
                    per.miete,
                    per.haushaltseink
                    )



na.threshold.data.table <- nrow(data.table) - 1 # In dem Fall darf kein Wert = 0 sein
clean.data.table <- as.data.frame(data.table[, colSums(data.table == 0) <= na.threshold.data.table]) # Loeschen der Zeilen mit 0 Werten
clean.data.table <- as.data.frame(t(clean.data.table)) # Transformieren der Tabelle -> Cluster sind jetzt im Header
clean.data.table <- cbind(rownames(clean.data.table), clean.data.table) # Die rownames werden in die erste Spalte geschrieben, da sie sonst nicht im ggplot verwendet werden koennen

write.table(clean.data.table, file = "output/dataTable.csv", dec = ",", sep = ";")


################### generate plot ########################################

get.graph <- function(data.table, del){
  
  na.threshold.data.table <- nrow(data.table) - 1 # In dem Fall darf kein Wert = 0 sein
  clean.data.table <- as.data.frame(data.table[, colSums(data.table == 0) <= na.threshold.data.table]) # Loeschen der Zeilen mit 0 Werten
  clean.data.table <- as.data.frame(t(clean.data.table)) # Transformieren der Tabelle -> Cluster sind jetzt im Header
  clean.data.table <- cbind(rownames(clean.data.table), clean.data.table) # Die rownames werden in die erste Spalte geschrieben, da sie sonst nicht im ggplot verwendet werden koennen

  #Loop einbauen!
  if(del == "y"){
    clean.data.table[, 1] <- sub("\\w*", "", clean.data.table[, 1]) # remove numbers (careful if numeric values exist)
  }else{
    print("Loop worked")
  }

  dat.m.data.table <- melt(clean.data.table)  #"melt" the data -> only 3 column left
  
  print(dat.m.data.table)
  
  names(dat.m.data.table) <- c("variables", "cluster", "quantity") # rename header
  dat.m.data.table$variables <- factor(dat.m.data.table$variables, levels = unique(dat.m.data.table$variables)) # Set the labeling of the y-axis (otherwise the text gets sorted)

  colors.barplot <- c("#708090", "#D53E4F", "#FBB869", "#F0E442", "#0072B2", "#56B4E9", "#009E73", "#FF0000", "#CC79A7", "#2F4F4F", "grey", "#7FFFD4", 	"#DDA0DD", "yellow")

  plot.title <- substitute(data.table)
  
  ##barplot of the results
  ggplot(dat.m.data.table, aes(y = quantity, x = cluster, fill = variables)) +
    geom_bar( stat = "identity", colour = "white") +
    scale_fill_manual(values = colors.barplot)# +
#    ggtitle(plot.title)
  
  p <- ggplot(dat.m.data.table, aes(y = quantity, x = cluster, fill = variables)) +
          geom_bar( stat = "identity", colour = "white") +
          scale_fill_manual(values = colors.barplot)

  file.name <- paste("output/", plot.title, ".jpg", sep = "")

  ggsave(filename = file.name, plot = p)
  
}


##percentage
get.graph(personal_perc, "n")
get.graph(cluster.table.gesPartner.perc, "y")
get.graph(cluster.table.bezVerwandte.perc, "y")
get.graph(cluster.table.Herkunft.perc, "y")
get.graph(ausbildung_perc, "n")
get.graph(einkommen_perc, "n")
get.graph(cluster.table.erwerbstaetigkeit.perc, "y")
get.graph(cluster.table.Kinder.perc, "n")
get.graph(wohnen_perc, "n")
get.graph(cluster.table.PersWohnen.perc, "y")
get.graph(wohnstiuation_perc, "n")
get.graph(teilnahmeGruppen_perc, "n")
get.graph(cluster.table.freundeVerwandte.perc, "n")

#### Age###

cluster.table.age <- with(berlin.clean, cbind(table(clusters, my.data.alter_14)))

mean.cluster.1 <- (3*65 + 1*70 + 2*71 + 2*72 + 3*73 + 3*74 + 1*75 + 4*76 + 2*79 + 1*80 + 1*81 + 1*86)/sum(cluster.table.age[1,])
mean.cluster.2 <- (1*67 + 1*70 + 1*71 + 3*73 + 1*74 + 1*76 + 2*77 + 1*79 + 3*80 + 1*84 + 2*86 + 2*89 + 1*90)/sum(cluster.table.age[2,])
mean.cluster.3 <- (3*67 + 1*68 +4*73 + 2*74 + 1*75 +2*76 + 2*77 +1*78+1*80+ 2*81 +1*85 + 1*86+ 1*88 + 1*91 + 1*92)/sum(cluster.table.age[3,])
mean.cluster.4 <- (1*70 + 1*73 + 1*74 + 1*75+ 1*76+ 1*80 + 2*82 + 1*87)/sum(cluster.table.age[4,])
mean.cluster.5 <- (1*66 + 1*67 + 1*69 + 1*70 + 3*74 + 1*78 + 1*85)/sum(cluster.table.age[5,])
mean.cluster.6 <- (3*67 + 1*68 + 2*69 + 4*70 + 2*72 + 1*73 + 2*75 + 1*76 + 2*79 + 1*83 + 1*84 + 1*85 + 1*87)/sum(cluster.table.age[6,])
mean.cluster.7 <- (2*65 + 1*66+2*67 + 1*69 + 2*70 + 1*71 + 1*73 + 1*74 + 2*75 + 3*76 + 2*77+ 2*79+ 1*80 + 2*84)/sum(cluster.table.age[7,])
mean.cluster.8 <- (2*66+2*68+2*69+1*70+4*71+1*73+1*77+2*78+2*80+1*81+2*82+1*85+1*86)/sum(cluster.table.age[8,])

write.table(cluster.table.age, file = "output/alter.csv", sep = ";", row.names = FALSE)


#cluster.table.age <- as.numeric(cluster.table.age)

sum_6569 <- rowSums(cluster.table.age[,1:5])
sum_7074 <- rowSums(cluster.table.age[,6:10])
sum_7579 <- rowSums(cluster.table.age[,11:15])
sum_8084 <- rowSums(cluster.table.age[,16:20])
sum_8589 <- rowSums(cluster.table.age[,21:25])
sum_u90 <- rowSums(cluster.table.age[,20:ncol(cluster.table.age)])

all_age <- t(rbind(sum_6569, sum_7074, sum_7579, sum_8084, sum_8589, sum_u90))

sum.age <- rowSums(all_age)
entire.table.age <- cbind(all_age, sum.age)
entire.per.table.age <- apply(all_age, 2, function(x) 100/entire.table.age[, ncol(entire.table.age)] * x)


write.table(entire.per.table.age, file = "output/alter_per.csv", sep = ";", row.names = FALSE)


######

