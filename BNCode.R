###################################################
#data clean
####################################################
install.packages("readxl")
library(readxl)
dioxin <- read_excel("DioxinData.xlsx")
dioxin_ind <- read_excel("DioxinData.xlsx", sheet = "index")
dioxin_2008 <- read_excel("DioxinData.xlsx", sheet = "2008")
dioxin_2009 <- read_excel("DioxinData.xlsx", sheet = "2009")
dioxin_2010 <- read_excel("DioxinData.xlsx", sheet = "2010")
dioxin_2011 <- read_excel("DioxinData.xlsx", sheet = "2011")
dioxin_2012 <- read_excel("DioxinData.xlsx", sheet = "2012")
dioxin_2013 <- read_excel("DioxinData.xlsx", sheet = "2013")
dioxin_2014 <- read_excel("DioxinData.xlsx", sheet = "2014")
dioxin_2015 <- read_excel("DioxinData.xlsx", sheet = "2015")
dioxin_2016 <- read_excel("DioxinData.xlsx", sheet = "2016")
dioxin_2017 <- read_excel("DioxinData.xlsx", sheet = "2017")
dioxin_2018 <- read_excel("DioxinData.xlsx", sheet = "2018")
dioxin_2008 <- dioxin_2008[,c(-3,-4,-11)]
dioxin_2009 <- dioxin_2009[,c(-3,-4,-11)]
dioxin_2010 <- dioxin_2010[,c(-3,-4,-11)]
dioxin_2011 <- dioxin_2011[,c(-3,-4,-11)]
dioxin_2012 <- dioxin_2012[,c(-3,-4,-11)]
dioxin_2013 <- dioxin_2013[,c(-3,-4,-11)]
dioxin_2014 <- dioxin_2014[,c(-3,-4,-11)]
dioxin_2015 <- dioxin_2015[,c(-3,-4,-11)]
dioxin_2016 <- dioxin_2016[,c(-3,-4,-11)]
dioxin_2017 <- dioxin_2017[,c(-3,-4,-11)]
dioxin_2018 <- dioxin_2018[,c(-3,-4,-11)]
#dioxin_2018 data is not sufficient
dioxin_test<-rbind(dioxin_2008,dioxin_2009,dioxin_2010,dioxin_2011,dioxin_2012,
                   dioxin_2013,dioxin_2014,dioxin_2015,dioxin_2016,
                   dioxin_2017)
str(dioxin_test)
##################################################################
#data clean for animal species
#########################################################################
dioxin_test$animalSpecies<- as.factor(dioxin_test$animalSpecies)
levels(dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('bovine animal for fattening', '2', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('deer (farmed)', '5', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('deer tamed', '5', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('poultry other', '14', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('bovine animal', '1', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('cow', '1', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('broiler', '3', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('calf for fattening', '4', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('deer', '5', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('duck', '6', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('eel', '7', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('fish', '8', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('goat', '9', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('goose', '10', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('hen', '11', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('horse', '12', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('pig', '13', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('poultry', '14', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('rabbit', '15', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('sheep', '16', dioxin_test$animalSpecies)
dioxin_test$animalSpecies<- gsub('Trout', '17', dioxin_test$animalSpecies)
######################################################################################

###############################################################################
#data clean for product
#################################################
dioxin_test$product<- as.factor(dioxin_test$product)
levels(dioxin_test$product)
dioxin_test$product<- gsub('egg', '1', dioxin_test$product)
dioxin_test$product<- gsub('meat', '2', dioxin_test$product)
dioxin_test$product<- gsub('milk', '3', dioxin_test$product)
dioxin_test$product<- gsub('liver', '4', dioxin_test$product)
###################################################################################

###############################################################################
#data clean for sampling place
#################################################
dioxin_test$`sampling place`<- as.factor(dioxin_test$`sampling place`)
levels(dioxin_test$`sampling place`)

dioxin_test$`sampling place`<- gsub('slaughterhouse', '1', dioxin_test$`sampling place`)
dioxin_test$`sampling place`<- gsub('farm', '2', dioxin_test$`sampling place`)
dioxin_test$`sampling place`<- gsub('aquaculture', '3', dioxin_test$`sampling place`)

###############################################################################
#data clean for screening reults
#################################################
dioxin_test$screeningResults<- as.factor(dioxin_test$screeningResults)
levels(dioxin_test$screeningResults)
dioxin_test$screeningResults<- gsub('negative', '0', dioxin_test$screeningResults)
dioxin_test$screeningResults<- gsub('suspect', '1', dioxin_test$screeningResults)

###############################################################################
#data clean for GC reults
#################################################
dioxin_test$gcResults<- as.factor(dioxin_test$gcResults)
levels(dioxin_test$gcResults)
dioxin_test$gcResults<- gsub('n', '0', dioxin_test$gcResults)
dioxin_test$gcResults<- gsub('p', '1',dioxin_test$gcResults)

###############################################################################
#data clean for sample size
#################################################
dioxin_test$sampleSize<- as.factor(dioxin_test$sampleSize)
levels(dioxin_test$sampleSize)
dioxin_test$sampleSize<- gsub('196', '1',dioxin_test$sampleSize)
dioxin_test$sampleSize<- gsub('226', '2',dioxin_test$sampleSize)
dioxin_test$sampleSize<- gsub('254', '2',dioxin_test$sampleSize)
dioxin_test$sampleSize<- gsub('352', '3',dioxin_test$sampleSize)
dioxin_test$sampleSize<- gsub('366', '3',dioxin_test$sampleSize)
dioxin_test$sampleSize<- gsub('425', '4',dioxin_test$sampleSize)
dioxin_test$sampleSize<- gsub('340', '3',dioxin_test$sampleSize)
dioxin_test$sampleSize<- gsub('358', '3',dioxin_test$sampleSize)
dioxin_test$sampleSize<- gsub('379', '3',dioxin_test$sampleSize)
dioxin_test$sampleSize<- gsub('365', '3',dioxin_test$sampleSize)
###############################

##############################
#data clean for EU monitoring
###########################
dioxin_test$euMonitoring<- as.factor(dioxin_test$euMonitoring)
levels(dioxin_test$euMonitoring)
###############################
#pick nodes for BN
#####################################################################
dioxin_BN <- dioxin_test
str(dioxin_BN)
dioxin_BN$animalSpecies<- as.factor(dioxin_BN$animalSpecies)
dioxin_BN$product<-as.factor(dioxin_BN$product)
dioxin_BN$`sampling place`<-as.factor(dioxin_BN$`sampling place`)
dioxin_BN$screeningResults<-as.factor(dioxin_BN$screeningResults)
dioxin_BN$trimester<-as.factor(dioxin_BN$trimester)
dioxin_BN$year<-as.factor(dioxin_BN$year)
dioxin_BN$gcResults <-as.factor(dioxin_BN$gcResults)
dioxin_BN$sampleSize <- as.factor(dioxin_BN$sampleSize)
dioxin_BN$euMonitoring <- as.factor(dioxin_BN$euMonitoring)
str(dioxin_BN)
dioxin_BN <- as.data.frame(dioxin_BN)
names(dioxin_BN)[5]="place"
# delete certain nodes
dioxin_BN <- dioxin_BN[,-1]
dioxin_BN <- dioxin_BN[,-8]
#80%
nrow(dioxin_BN)
samp <- sample(nrow(dioxin_BN),2606)
dioxin_BN80 <- dioxin_BN[samp,]
dioxin_BN20 <- dioxin_BN[-samp,]
##########################################################
#Bayesian network
#####################################################
#bnlearn library
#########################################################
install.packages("bnlearn")
library(bnlearn)
install.packages("BiocManager")
BiocManager::install("Rgraphviz")
library("Rgraphviz")
install.packages("gRbase")
install.packages("gRain")
install.packages("bnclassify")
library("bnclassify")
library("gRbase")
library("gRain")
################################################
#structure learning
############################################################################
wl = c( "sampleSize", "euMonitoring")
bl= c("trimester", "euMonitoring")
bl = matrix(c("trimester", "euMonitoring", "euMonitoring", "trimester"), ncol = 2, byrow = TRUE)
tan = tree.bayes(dioxin_BN, "gcResults", blacklist = bl)
fitted = bn.fit(tan, dioxin_BN, method = "bayes")
pred = predict(fitted, dioxin_BN20)
bnclassify::accuracy(pred, dioxin_BN20$gcResults)
table(pred, dioxin_BN20[, "gcResults"]) 
par(mfrow = c(1,1))
graphviz.plot(tan)
################## TAN
tan = tree.bayes(dioxin_BN, "screeningResults")
fitted = bn.fit(tan, dioxin_BN, method = "bayes")
fitted$screeningResults
pred = predict(fitted, dioxin_BN20)
bnclassify::accuracy(pred, dioxin_BN20$screeningResults)
table(pred, dioxin_BN20[, "screeningResults"]) 
par(mfrow = c(1,1))
graphviz.plot(tan)
cv.nb = bn.cv(data = dioxin_BN,tan, runs = 10, method = "k-fold", folds = 10)

#####marginal distirbution
junction = compile(as.grain(fitted))
year<-querygrain(junction , nodes = "year", type = "marginal",
                 evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                 details = 0)
season<-querygrain(junction , nodes = "trimester", type = "marginal",
                   evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                   details = 0)
place<-querygrain(junction , nodes = "place", type = "marginal",
                  evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                  details = 0)
prodcut <-querygrain(junction , nodes = "product", type = "marginal",
                     evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                     details = 0)
sampleSize <- querygrain(junction , nodes = "sampleSize", type = "marginal",
                         evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                         details = 0)
animalSpecies <- querygrain(junction , nodes = "animalSpecies", type = "marginal",
                            evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                            details = 0)
screeningResults <- querygrain(junction , nodes = "screeningResults", type = "marginal",
                               evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                               details = 0)
gcResults <- querygrain(junction , nodes = "gcResults", type = "marginal",
                        evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                        details = 0)
euMonitroing <- querygrain(junction , nodes = "euMonitoring", type = "marginal",
                           evidence = NULL, exclude = TRUE, normalize = TRUE, result = "array",
                           details = 0)
###################################################################
#conditional probability with evidence screeningResults=1
#############################################################
### year
for (i in 2008:2017) {
  as.factor(i)
  
  prob= cpquery(fitted, event = (year== i),evidence=(screeningResults =="1" ),method = "ls", n= 10^6 )
  result<- c(prob, paste("year", i))
  print(result)
}

bn.fit.barchart (fitted$screeningResults)
###
###season trimester
for (i in 1:4) {
  as.factor(i)
  
  prob= cpquery(fitted, event = (trimester== i),evidence=(screeningResults =="1" ),method = "ls", n= 10^6 )
  result<- c(prob, paste("season", i))
  print(result)
}
###place control point
###season trimester
for (i in 1:3) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (place== i),evidence=(screeningResults =="1" ),method = "ls", n= 10^6 )
  result<- c(prob, paste("place", i))
  print(result)
}
#######product
for (i in 1:4) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (product== i),evidence=(screeningResults =="1" ),method = "ls", n= 10^6 )
  result<- c(prob, paste("product", i))
  print(result)
}

#######sample size
for (i in 1:4) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (sampleSize== i),evidence=(screeningResults =="1" ),method = "ls", n= 10^6 )
  result<- c(prob, paste("sampleSize", i))
  print(result)
}
#######animalspecies
for (i in 1:17) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (animalSpecies== i),evidence=(screeningResults =="1" ),method = "ls", n= 10^6 )
  result<- c(prob, paste("animalSpecies", i))
  print(result)
}
#######gc results
for (i in 0:2) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (gcResults== i),evidence=(screeningResults =="1" ),method = "ls", n= 10^6 )
  result<- c(prob, paste("gcResults", i))
  print(result)
}
##########eumonitoring
for (i in 0:31) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (euMonitoring== i),evidence=((animalSpecies == "1")&(product=="3")),method = "ls", n= 10^6 )
  result<- c(prob, paste("euMonitoring", i))
  print(result)
}
###################################################
##prediction CP
######################################################
cpquery(fitted, event =(screeningResults == "1"),evidence=((trimester=="1")&(product=="2")&(place=="1")&(sampleSize=3)), method = "ls",debug = TRUE, n= 10^6)
particles = cpdist(fitted, nodes = "year",evidence = (screeningResults == "1"))
prop.table(table(particles))
set.seed(0)
product1=cpquery(fitted, event =(product== "1"),evidence=(screeningResults == "1"), method = "ls",debug = TRUE, n= 10^6)
product2=cpquery(fitted, event =(product== "2"),evidence=(screeningResults == "1"), method = "ls",debug = TRUE, n= 10^6)
product3=cpquery(fitted, event =(product== "3"),evidence=(screeningResults == "1"), method = "ls",debug = TRUE, n= 10^6)
product4=cpquery(fitted, event =(product== "4"),evidence=(screeningResults == "1"), method = "ls",debug = TRUE, n= 10^6)
#####
gcResults1=cpquery(fitted, event =(gcResults== "1"),evidence=(screeningResults == "1"), method = "ls",debug = TRUE, n= 10^6)
gcResults2=cpquery(fitted, event =(gcResults== "2"),evidence=(screeningResults == "1"), method = "ls",debug = TRUE, n= 10^6)
gcResults3=cpquery(fitted, event =(gcResults== "3"),evidence=(screeningResults == "1"), method = "ls",debug = TRUE, n= 10^6)

cv.nb = bn.cv(data = dioxin_BN,tan, runs = 10, method = "k-fold", folds = 10)
#######
#cpquery  for the probability used in optimization
###########
#milk in one year
###########
for (i in 1:4) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (screeningResults== "1"),evidence=((trimester==i)&(animalSpecies=="1")&(product=="3")
                                                                   &(place=="2"))) 
  result<- c(prob, paste("milk_season", i))
  print(result)
}

###########
#bovine animal meat in one year
###########
for (i in 1:4) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (screeningResults== "1"),evidence=((trimester==i)&(animalSpecies=="1")&(product=="2")
                                                                   &(place=="1"))) 
  result<- c(prob, paste("bovinemeat_season", i))
  print(result)
}

###########
#broiler meat in one year
###########
for (i in 1:4) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (screeningResults== "1"),evidence=((trimester==i)&(animalSpecies=="3")&(product=="2")
                                                                   &(place=="1"))) 
  result<- c(prob, paste("broliermeat_season", i))
  print(result)
}

###########
#culf meat in one year
###########
for (i in 1:4) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (screeningResults== "1"),evidence=((trimester==i)&(animalSpecies=="4")&(product=="2")
                                                                   &(place=="1"))) 
  result<- c(prob, paste("culfmeat_season", i))
  print(result)
}

###########
#deer meat in one year
###########
for (i in 1:4) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (screeningResults== "1"),evidence=((trimester==i)&(animalSpecies=="5")&(product=="2")
                                                                   &(place=="1"))) 
  result<- c(prob, paste("deermeat_season", i))
  print(result)
}


###########
#pig meat in one year
###########
for (i in 1:4) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (screeningResults== "1"),evidence=((trimester==i)&(animalSpecies=="13")&(product=="2")
                                                                   &(place=="1"))) 
  result<- c(prob, paste("pigmeat_season", i))
  print(result)
}


###########
#sheep meat in one year
###########
for (i in 1:4) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (screeningResults== "1"),evidence=((trimester==i)&(animalSpecies=="16")&(product=="2")
                                                                   &(place=="1"))) 
  result<- c(prob, paste("sheepmeat_season", i))
  print(result)
}

###########
#hen egg in one year
###########
for (i in 1:4) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (screeningResults== "1"),evidence=((trimester==i)&(animalSpecies=="11")&(product=="1")
                                                                   &(place=="2"))) 
  result<- c(prob, paste("henegg_season", i))
  print(result)
}

###########
#poultry meat in one year
###########
for (i in 1:4) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (screeningResults== "1"),evidence=((trimester==i)&(animalSpecies=="14")&(product=="2")
                                                                   &(place=="1"))) 
  result<- c(prob, paste("poultrymeat_season", i))
  print(result)
}

################################################################################
#conditional probability for EU monitoring
#########################################################################
###########
#milk EU sample size
###########
for (i in 0:31) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (euMonitoring== i),evidence=((animalSpecies=="1")&(product=="3")
                                                                   )) 
  result<- c(prob, paste("milk_size", i))
  print(result)
}

milk <- c(1,rep(3,2),4,rep(5,2),rep(6,10),rep(7,12),rep(9,5),rep(10,10),rep(12,4),rep(13,6),rep(14,7),
          rep(16,25),rep(18,8),rep(19,8))
t.test(milk)
rep(3,2)
###########
#bovine animal meat EU sample size
###########
for (i in 0:31) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (euMonitoring== i),evidence=((animalSpecies=="1")&(product=="2")
                                                                   )) 
  result<- c(prob, paste("bovinemeat_size", i))
  print(result)
}

BM<-c(1,3,rep(4,2),rep(5,2),rep(6,9),rep(7,12),rep(9,5),rep(10,10),rep(12,5),rep(13,7),rep(14,6),
      rep(16,23),rep(18,9),rep(19,8))
t.test(BM)
###########
#broiler meat eu size
###########
for (i in 0:31) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (euMonitoring== i),evidence=((animalSpecies=="3")&(product=="2")
                                                                   )) 
  result<- c(prob, paste("broliermeat_season", i))
  print(result)
}
BRM <- c(rep(5,8),rep(6,4),rep(10,5),rep(11,5),rep(18,11),rep(20,16),rep(22,26),rep(24,13),rep(27,13))
t.test(BRM)

###########
#culf meat eu sample size
###########
for (i in 0:31) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (euMonitoring== i),evidence=((animalSpecies=="4")&(product=="2")
                                                                   )) 
  result<- c(prob, paste("culfmeat_size", i))
  print(result)
}
CM<-c(rep(6,13),rep(7,7),rep(11,10),rep(12,12),rep(13,15),rep(14,16),rep(15,15),rep(18,13))
t.test(CM)
###########
#deer meat eu size
###########
for (i in 0:31) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (euMonitoring== i),evidence=((animalSpecies=="5")&(product=="2")
                                                                   )) 
  result<- c(prob, paste("deermeat_size", i))
  print(result)
}

DM<-c(rep(0,38),rep(1,44),rep(2,19))
t.test(DM)
###########
#pig meat eu smaple size
###########
for (i in 0:31) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (euMonitoring== i),evidence=((animalSpecies=="13")&(product=="2")
                                                                  )) 
  result<- c(prob, paste("pigmeat_size", i))
  print(result)
}

PM<-c(rep(6,4),rep(9,6),rep(10,5),rep(15,12),rep(16,8),rep(20,15),rep(27,13),rep(28,12),rep(29,11),rep(31,15))
t.test(PM)
###########
#sheep meat eu size
###########
for (i in 0:31) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (euMonitoring== i),evidence=((animalSpecies=="16")&(product=="2")
                                                                  )) 
  result<- c(prob, paste("sheepmeat_season", i))
  print(result)
}
SM<- c(rep(3,8),rep(5,6),rep(7,8),rep(8,15),rep(9,41),rep(11,15),rep(13,8))
t.test(SM)
###########
#hen egg eu size
###########
for (i in 0:31) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (euMonitoring== i),evidence=((animalSpecies=="11")&(product=="1")
                                                                   )) 
  result<- c(prob, paste("henegg_season", i))
  print(result)
}
HE<-c(0,1,2,3,3,rep(9,6),rep(14,12),rep(16,32),rep(17,8),rep(18,17),rep(19,12),rep(24,8))
t.test(HE)
###########
#poultry meat eu monitoring
###########
for (i in 0:31) {
  as.factor(i)
  set.seed(0)
  prob= cpquery(fitted, event = (euMonitoring== i),evidence=((animalSpecies=="14")&(product=="2")
                                                                   )) 
  result<- c(prob, paste("poultrymeat_size", i))
  print(result)
}
POULM<-c(rep(0,16),rep(1,51),rep(2,33))
t.test(POULM)
