
if (!require("pacman")) install.packages("pacman")
#pacman will not accept a character vector so the same packages are repeated
pacman::p_load("ggplot2", "arules", "arulesViz", "graphics", "RColorBrewer")



setwd("C:/Users/Gebruiker/Downloads")
trans <- read.transactions("ElectronidexTransactions2017.csv",
                           format =  "basket",
                           sep = ",",
                           rm.duplicates = T)

transTyp <- read.transactions("TransactionsTypes.csv",
                           format =  "basket",
                           sep = ";",
                           rm.duplicates = F)

transX <- read.transactions("TransactionsX.csv",
                              format =  "basket",
                              sep = ";")


###  Exploration ###
trans
inspect(transTyp)
inspect(trans[9835])
inspect(head(transTyp))
head(transTyp)
head(trans)
length(trans)
size(trans)
LIST(transTyp)
LIST(trans)
itemLabels(transTyp)
itemLabels(trans[80])
summary(trans)
str(trans)

###   Further Exploration   ###
#plot the items that occur > 1500
itemFrequencyPlot(sort(trans,
                  type = "absolute",  
                  support = 1000))
                  
barplot(sort(itemFrequency(trans,
                           type = "relative",  
                           support = 0.1),
             decreasing=TRUE))

itemFrequencyPlot(trans,
                          topN=10,
                          col=brewer.pal(8,'Pastel2'),
                          main='Sales - Electronidex',
                          type="relative",
                          ylab="Item Frequency (Relative)") 

arules::itemFrequencyPlot(transX,
                          topN=10,
                          col=brewer.pal(8,'Pastel2'),
                          main='Sales - Electronidex - Tuned Set',
                          type="relative",
                          ylab="Item Frequency (Relative)") 

arules::itemFrequencyPlot(transTyp,
                          topN=10,
                          col=brewer.pal(8,'Pastel2'),
                          main='Sales - Electronidex - Product by Type',
                          type="relative",
                          ylab="Item Frequency (Relative)") 


itemFrequencyPlot(trans,
                  type = "relative",  
                  support = 0.1,
                  sort = "decreasing")


### ..and another explo ###
set.seed(123)

image(trans)
image(sample(transTyp, 125))

    ####################
   ### MAKE  RULES  ###
  ###  A PRIORI    ###
 ### KANT SAYS HI ###
####################

typRules <- apriori (transTyp,
                      parameter = list(supp = 0.05,
                                       conf = 0.5,
                                       minlen = 2))

moreRules <- apriori (trans,
                     parameter = list(supp = 0.005,
                                      conf = 0.5))

xRules <- apriori (transX,
                     parameter = list(supp = 0.03,
                                      conf = 0.5))


inspect(typRules)
str(moreRules)
summary(moreRules)

  #########################################
 ######   ?????????                 ######
#########################################


mac.a.Rules <- subset(xRules, 
                  lhs %in% "iMac")

mac.c.Rules <- subset(xRules, 
                      rhs %in% "iMac")


?subset
inspect(head(sort( mac.a.Rules,
                   by = "support")))

inspect(head(sort( mac.c.Rules,
                   by = "lift")))



  #####################################
 #### RULES FOR SPECIFIC PRODUCTS ####
#####################################

### Rules for iMac   ###
imacRules <- apriori (data = trans,
                      parameter = list (supp=0.005,
                                        conf = 0.5),
                      appearance = list (rhs="iMac"))
### Rules for HP Laptop   ###
hpRules <- apriori (data = trans,
                      parameter = list (supp=0.01,
                                        conf = 0.5),
                      appearance = list (rhs="HP Laptop"))
inspect(hpRules) 
### Rules for CYBERPOWER Gamer Desktop   ### NOTE SUPPORT *-10 ###
cyberRules <- apriori (data = trans,
                      parameter = list (supp=0.001,
                                        conf = 0.5),
                      appearance = list (rhs="CYBERPOWER Gamer Desktop"))

###   X   Rules for iMac    ###
imacRules <- apriori (data = transX,
                      parameter = list (supp=0.02,
                                        conf = 0.5),
                      appearance = list (rhs="iMac"))

imac.Rules <- apriori (data = transX,
                      parameter = list (supp=0.02,
                                        conf = 0.5),
                      appearance = list (lhs="iMac"))

is.redundant(imacRules)
imacRules <- imacRules[!is.redundant(imacRules)]

plot(imacRules, method = "graph")
plot(imacRules, method = "grouped")
?plot
plot(imac.Rules, method = "graph")
plot(imac.Rules, method = "grouped")

### Rules for GAMER   ###
gameRules <- apriori (data = transX,
                     parameter = list (supp=0.002,
                                       conf = 0.4,
                                       minlen = 2),
                     appearance = list (lhs ="CYBER Gamer"))

game.Rules <- apriori (data = transX,
                      parameter = list (supp=0.002,
                                        conf = 0.45,
                                        minlen = 2),
                      appearance = list (rhs ="CYBER Gamer"))

inspect(sort(game.Rules, by = "lift"))

inspect(head(sort( game.Rules, by = "lift")))

        

plot(game.Rules, method = "graph")


###  Rules for Lenovo Desktop Computer  ###
lenoRules <- subset(moreRules, 
                    rhs %in% "Lenovo Desktop Computer")

inspect(sort( lenoRules, by = "support"))

### rules lhs ASUS 2 Monitor   ###
a2Rules <- subset(moreRules, 
                    lhs %in% "ASUS 2 Monitor")

inspect(head(sort( a2Rules,
                   by = "support")))

inspect(head(sort( a2Rules,
                   by = "lift")))



   ###  ###############################  ###
  ###  ### PROCESS THOSE RULE SETS ###  ###
 ###  ###############################  ###

### Check Redundant Rules ### repeat rmv a couple of times ###
is.redundant(typRules)
typRules <- typRules[!is.redundant(typRules)]

is.redundant(imacRules)
moreRules <- moreRules[!is.redundant(moreRules)]

is.redundant(xRules)
xRules <- xRules[!is.redundant(xRules)]


 ###################################
###################################


keyRules <- subset(moreRules, lhs %pin% "Keyboard")
inspect(head(sort(keyRules,
                  by = "support")))

inspect(head(sort( moreRules,
              by = "lift")))

inspect(head(sort( moreRules,
                   by = "confidence")))


