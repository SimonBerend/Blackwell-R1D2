install.packages("dplyr")
library(dplyr)

setwd("C:/Users/Gebruiker/Downloads")
msleep <- read.csv2("msleep_ggplot2.csv", 
                    sep = ",",
                    header = TRUE)
?read.csv2
summary(msleep)
head(msleep)


str(msleep)
msleep$sleep_total <- as.numeric(msleep$sleep_total)
msleep$sleep_rem <- as.numeric(msleep$sleep_rem) 
msleep$sleep_cycle <- as.numeric(msleep$sleep_cycle) 
msleep$awake <- as.numeric(msleep$awake) 
msleep$brainwt <- as.numeric(msleep$brainwt) 
msleep$bodywt <- as.numeric(msleep$bodywt)

# I want to get the numbers of rows inside a data frame.
# There are two ways to do it: 
count(x = msleep)
# Or
msleep %>% count()

# I want to summarize all the information to get the mean
# of the sleep_total variable 
summarise(.data = msleep, mean(sleep_total))
# Or
msleep %>% summarise(mean(sleep_total))

msleep %>% select(sleep_total)
msleep %>% select(starts_with("sl"))

str(msleep)

msleep %>% 
  group_by(vore) %>% 
  summarise(mean(sleep_total))

msleep %>%
  filter(sleep_total > 2 &
           sleep_total < 19 &
           conservation != "domesticated")

# Check if your filter is filtering `NA` for the variable **conservation**. If not, filter them.
# it is already filtering 'NA', but i dont know how to change that

voreNew <- msleep %>%
  filter(sleep_total > 2 &
           sleep_total < 19 &
           conservation != "domesticated") %>%
  mutate(brainwt/bodywt, na.rm = TRUE) %>% 
  group_by(vore) %>% 
  summarise(mean(brainwt/bodywt))

ratioBrainBody <- msleep %>%
  filter(sleep_total > 2 &
           sleep_total < 19 &
           conservation != "domesticated") %>%
  mutate(brainwt/bodywt)


sumRatBB <- ratioBrainBody %>%
  group_by(vore) %>% 
  summarise(MeanBB = mean(brainwt/bodywt, na.rm = TRUE),
            n = n())

sumRatBB
cleanSum <- na.omit(sumRatBB)

arrange( cleanSum, desc(MeanBB))


