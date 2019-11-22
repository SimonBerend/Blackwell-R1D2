# Get them packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load("ggplot2", "arules", "arulesViz",
               "graphics", "RColorBrewer", "e1071")



setwd("C:/Users/Gebruiker/Downloads")
trans <- read.transactions("ElectronidexTransactions2017.csv",
                           format =  "basket",
                           sep = ",",
                           rm.duplicates = T)

transTyp <- read.transactions("TransactionsTypes2.csv",
                           format =  "basket",
                           sep = ";")

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
size(transX)
LIST(transTyp)
LIST(trans)
itemLabels(transTyp)
itemLabels(trans[80])
summary(transX)
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
                          topN=17,
                          col=brewer.pal(8,'Pastel2'),
                          main='Sales - Electronidex - Tuned Set',
                          type="relative",
                          ylab="Item Frequency (Relative)") 

arules::itemFrequencyPlot(transTyp,
                          topN=14,
                          col=brewer.pal(8,'Pastel2'),
                          main='Sales - Electronidex - Product by Type',
                          type="relative",
                          ylab="Item Frequency (Relative)") 

?itemFrequencyPlot
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
                     parameter = list(supp = 0.01,
                                      conf = 0.5,
                                      minlen = 2
                                      ))


inspect(head(sort(moreRules, by = "lift")))
plot(head(sort(moreRules, by = "lift")),
     method = "graph",
     main = "Common transactions Electronidex")

plot(xRules, verbose = TRUE)

plot(xRules,
     method = "two-key plot",
     control = list (main = "1731 Transaction Rules",
                     col = wes_palette("Zissou1", 7, type = "continuous")))
     
     

inspect(head(sort(xRules, by = "support")))

### Check Redundant Rules ###
is.redundant(typRules)
typRules <- typRules[!is.redundant(typRules)]

is.redundant(moreRules)
moreRules <- moreRules[!is.redundant(moreRules)]

is.redundant(xRules)
xRules <- xRules[!is.redundant(xRules)]


  #####################################
 #### RULES FOR SPECIFIC PRODUCTS ####
#####################################

  ####################################
 ######         iMac           ######
####################################

imacRules <- apriori (data = transX,
                      parameter = list (supp=0.025,
                                        conf = 0.5),
                      appearance = list (rhs="iMac"))

imac.Rules <- apriori (data = transX,
                       parameter = list (supp=0.001,
                                         conf = 0.3,
                                         minlen = 2),
                       appearance = list (lhs="iMac"))

is.redundant(imacRules)
imacRules <- imacRules[!is.redundant(imacRules)]


plot(imacRules, method = "graph", main = "Patterns iMac (rhs)")
plot(imac.Rules, method = "graph", main = "Patterns iMac (lhs)")

inspect(sort(imacRules, by = "lift"))


  ####################################
 ######         Gamer         #######
####################################


gameRules <- apriori (data = transX,
                      parameter = list (supp=0.002,
                                        conf = 0.46,
                                        minlen = 2),
                      appearance = list (rhs ="CYBER Gamer"))


game.Rules <- apriori (data = transX,
                       parameter = list (supp=0.002,
                                         conf = 0.45,
                                         minlen = 2),
                       appearance = list (lhs ="CYBER Gamer"))


## R hs
inspect(sort(gameRules, by = "lift"))
plot(gameRules, method = "graph", main = "Patterns CYBER Gamer (rhs)")

## L hs
inspect(sort(game.Rules, by = "lift"))
plot(game.Rules, method = "graph", main = "Patterns CYBER Gamer (lhs)")




  ####################################
 ######       Accessories      ######
####################################


accRules <- apriori (data = transX,
                      parameter = list (supp=0.002,
                                        conf = 0.46,
                                        minlen = 2),
                      appearance = list (rhs ="Accessories"))


acc.Rules <- apriori (data = transX,
                       parameter = list (supp=0.002,
                                         conf = 0.45,
                                         minlen = 2),
                       appearance = list (lhs ="Accessories"))


## R hs
inspect(sort(accRules, by = "lift"))
plot(accRules, method = "graph", main = "Patterns Accessories (rhs)")

## L hs
inspect(sort(acc.Rules, by = "lift"))
plot(acc.Rules, method = "graph", main = "Patterns Accessories (lhs)")



