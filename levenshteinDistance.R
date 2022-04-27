##########
# IR 413 
# Benet Post 
# 4/19/22
# Final Project - Lexical Similarity 
##########

# when we need to load in a file - Changed to my appropriate directory
setwd("/users/clairepost/downloads/ANTH202/finalProj/")

# First removing things out of your Global Enviroment (tab on the right)
rm(list = ls())

# necessary libraries
# set options
options(stringsAsFactors = F)
# installing additional libraries
install.packages("stringdist")
install.packages("hashr")
install.packages("flextable")
install.packages('gdata')
install.packages('sjmisc')
install.packages('RColorBrewer')
install.packages('viridis')
install.packages('writexl')
install.packages('plot.matrix')

# for reading excel sheets
install.packages("readxl")

# install klippy for copy-to-clipboard button in code chunks
install.packages("remotes")
remotes::install_github("rlesur/klippy")

install.packages("hrbrthemes")


# set options
options(stringsAsFactors = F)          # no automatic data transformation
options("scipen" = 100, "digits" = 12) # suppress math annotation
# activate packages
library(stringdist)
library(hashr)
library(tidyverse)
library(flextable)
library(readxl)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(gtable)
library(sjmisc)
library(RColorBrewer)
library(viridis)
library(writexl)
library(plot.matrix)
# activate klippy for copy-to-clipboard button
klippy::klippy()


# getting a sense of lexical similiarity scores #

# using examples here I am going through and seeing different ways to analyze text similiarity. 
# With a few examples, we can elucidate ways to measure lexical change

text1 = "The quick brown fox jumped over the wall"
text2 = "The fast brown fox leaped over the wall"
insert_ex = c("Marta","Martha")
del_ex = c("Genome","Gnome")
rep_ex = c("Tim","Tom")

# Using the seq_dist function along with hash function to calculate the Jaccard similarity word-wise
jac_sim_score = seq_dist(hash(strsplit(text1, "\\s+")), hash(strsplit(text2, "\\s+")), method = "jaccard",q=2)
print(paste0("The Jaccard similarity for the two texts is ",jac_sim_score))


# levenshtein distance #

# Insert edit
ins_edit = stringdist(insert_ex[1],insert_ex[2],method = "lv")
print(paste0("The insert edit distance for ",insert_ex[1]," and ",insert_ex[2]," is ",ins_edit))

# Delete edit
del_edit = stringdist(del_ex[1],del_ex[2],method = "lv")
print(paste0("The delete edit distance for ",del_ex[1]," and ",del_ex[2]," is ",del_edit))

# Replace edit
rep_edit = stringdist(rep_ex[1],rep_ex[2],method = "lv")
print(paste0("The replace edit distance for ",rep_ex[1]," and ",rep_ex[2]," is ",rep_edit))



# Considering my data set #

# all data
quechua <- read_excel("runasimi.xls")
# just two columns
quechuaSpan <- read_excel("spanishQuechua.xlsx")


# running different Levenshtein Distance analysis could be interseting
similarityMatrix = stringdist(quechuaSpan$`Hanan Runasimi`,quechuaSpan$Español, method = "lv")

# append the matrix to the dataset
quechuaSpan$levSim <- similarityMatrix
quechuaSpan

# I want to also look at other distributive methods than levenshtein

histBasic <- quechuaSpan %>%
  filter(quechuaSpan$levSim < 150) %>%
  ggplot(aes(x=quechuaSpan$levSim)) + 
  geom_histogram( binwidth=2, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") 

histBasic

histBasic20 <- quechuaSpan %>%
  filter(quechuaSpan$levSim < 20) %>%
  ggplot(aes(x=levSim)) + 
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count")

histBasic20

# Represent it
partsOfSpeech <- quechuaSpan %>%
  filter(quechuaSpan$levSim < 150) %>%
  ggplot(aes(x=levSim)) + 
  geom_histogram( binwidth=2, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  facet_wrap(~`Ima simi`)
  
view(partsOfSpeech)


# Looking at just the different parts of speech
table(quechuaSpan$`Ima simi`)

pos <- quechuaSpan %>%
  ggplot(aes(x=quechuaSpan$`Ima simi`)) +
  geom_bar()+
  coord_flip()
pos

# from these two views I can see that there are a TON of different types of parts of speech
# I want to clean these up to only have a select number of parts of speech
# that are less confusing and actually mean something to my data set

# proposed pair downs:
# starts with "adj." becomes just "adjective"
# starts with "adv." becomes just "adverb"
# starts with "art." becomes just "article"
# starts with "conj." becomes just "conjunction"
# starts with "expr." becomes just "expression"
# starts with "fon." becomes just "phoneme"
# starts with "interj." becomes just "interjection"
# starts with "p." becomes just "particple"
# starts with "num." becomes "numerals"
# starts with "postpos." becomes "preposition"
# starts with "prep." also becomes "preposition"
# starts with "pron" becomes "pronoun"
# starts with "suf." becomes "suffix"
# starts with "v." becomes just "verb"
# starts with "s." becomes just "singular noun"
# all others just become "other"


# some attempts before I figured out what worked
cleanedSpeech <- replace(quechuaSpan$`Ima simi`, 
          startsWith(quechuaSpan$`Ima simi`, 'adj.'), 
          "adjective") 

cleanedSpeech <- quechuaSpan %>%
  replace(quechuaSpan$`Ima simi`, 
          startsWith(quechuaSpan$`Ima simi`, 'adj.'), 
          "adjective") 

cleanedSpeech

# making a copy
cSpeech <- quechuaSpan

# I can change the parts of speech by adding it to the new column 
# cleaning up (works)

cSpeech$partsOfSpeech <- replace(cSpeech$`Ima simi`, 
                                 startsWith(cSpeech$`Ima simi`, 'adj.'), 
                                 "adjective")
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 'adv.'), 
                                 "adverb") 
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 'art.'), 
                                 "article") 
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 'conj.'), 
                                 "conjunction") 
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 'expr.'), 
                                 "expression") 
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 'fon.'), 
                                 "phoneme") 
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 'interj.'), 
                                 "interjection") 
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 'p.'), 
                                 "participle") 
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 'num.'), 
                                 "numerals") 
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 'postpos.'), 
                                 "preposition") 
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 'prep.'), 
                                 "preposition") 
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 'pron.'), 
                                 "pronoun") 
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 'suf.'), 
                                 "suffix") 
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 'v.'), 
                                 "verb") 
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 's.'), 
                                 "noun") 


# catching the stragglers
table(cSpeech$partsOfSpeech)

# ej., s, f.v.aux., and simply . are left
# more expressions = 'ej.'
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 'ej.'), 
                                 "expression") 
# . is an adjective
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, '.'), 
                                 "adjective") 
# s is supposed to be a noun
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 cSpeech$partsOfSpeech == "s", 
                                 "noun") 
# f.v.aux. are copular verbs
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 'f.v.'), 
                                 "copular verb") 




# Now let's make another plot with the differences wrapped for part of speech

pos <- cSpeech %>%
  filter(cSpeech$levSim < 10) %>%
  ggplot(aes(x=levSim)) + 
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  facet_wrap(~partsOfSpeech)
pos

posDensity <- cSpeech %>%
  filter(cSpeech$levSim < 10) %>%
  ggplot(aes(x=levSim)) + 
  geom_density( fill="#69b3a2", color="#e9ecef") + 
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  facet_wrap(~partsOfSpeech)
posDensity

posdhist <- cSpeech %>%
  filter(cSpeech$levSim < 20) %>%
  ggplot(aes(x=levSim, y = ..density..)) + 
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  facet_wrap(~partsOfSpeech)
posdhist



# article is messing up my charts, going to omit the 1 article
# also chaged the y - value to density
cSpeech$partsOfSpeech <- replace(cSpeech$partsOfSpeech, 
                                 startsWith(cSpeech$partsOfSpeech, 'article'), 
                                 "copular verb") 

posdhistgrid <- cSpeech %>%
  filter(cSpeech$levSim < 10) %>%
  ggplot(aes(x=levSim, y = ..density..)) + # changed y to density because counts are different on each type
  geom_histogram( binwidth=1, fill="#b5285e", color="#b5285e", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Density") +
  facet_wrap(~partsOfSpeech) +
  scale_fill_brewer()
posdhistgrid

# a box plot with facet wrap
boxSpeech <- cSpeech %>%
  filter(cSpeech$levSim < 50) %>%
  ggplot(aes(x=levSim)) + 
  geom_boxplot() +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  facet_wrap(~partsOfSpeech) +
  theme_minimal()
boxSpeech

# using median to look at boxplots
# Using median
boxMedian <- cSpeech %>% 
  mutate(class = fct_reorder(partsOfSpeech, levSim, .fun='median')) %>%
  ggplot( aes(x=reorder(partsOfSpeech, levSim), y=levSim, fill=partsOfSpeech)) + 
  geom_boxplot() 
boxMedian

# with all side by side box plots
boxSpeech <- cSpeech %>%
  filter(cSpeech$levSim < 50) %>%
  ggplot(aes(x=partsOfSpeech,y=levSim)) + 
  geom_boxplot() +
  xlab("Part of Speech") +
  ylab("Levenshtein Distance") +
  theme_minimal() 
boxSpeech

plot(cSpeech$levSim ~ cSpeech$partsOfSpeech, data=)

# looking at Spanish origin




# a special note 
# anything that includes (esp) is a word that comes from Spanish and has been
# already precoded for it!
str_contains(cSpeech$`Ima simi`, "(esp)")

# making a dummy variable for this
cSpeech$spanishOrgin <- grepl("esp", cSpeech$`Ima simi`)

cSpeech
# so could look at the number of words that have been coded to Spanish in Quechua
# in comparison with the rest
  
# let's make a visualization for seeing the true/false data

# I am thinking a geom_bar plot would be smart
barSpan <- ggplot(cSpeech, aes(fill=spanishOrgin, 
                           x=partsOfSpeech, y = spanishOrgin)) +
  geom_bar(position="fill", stat="identity")
barSpan


# I am not entirely sure that the y-axis here makes sense
# to make it the levSim
colSpan <- ggplot(cSpeech, aes(fill=spanishOrgin,  # something messed up with fill right now
          x=partsOfSpeech, y = levSim)) +
  geom_col(position="fill") +
  scale_fill_brewer(palette="Set1") # pretty color pallete
colSpan


# stacked instead so you can see counts
stackSpan <- ggplot(cSpeech, aes(fill=spanishOrgin,  # something messed up with fill right now
                               x=partsOfSpeech, y = levSim)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Set1") # pretty color pallete
stackSpan


# I need a variable just to tally things up,
# that seems to be the problem with these graphs currently
# so I will create a tally column just to count things
cSpeech$tally <- ifelse(cSpeech$`Hanan Runasimi` == TRUE, 0,1)


# stacked instead so you can see counts (fixed with tally)
stackSpan <- ggplot(cSpeech, aes(fill=spanishOrgin,  
                                 x=partsOfSpeech, y = tally)) + # fixed with tally
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Set1") # pretty color pallete
stackSpan

# now this should properly show proportions
colSpan <- ggplot(cSpeech, aes(fill=spanishOrgin,  
                               x=partsOfSpeech, y = tally)) + # fixed with tally
  geom_col(position="fill") +
  scale_fill_brewer(palette="Set1") + # pretty color pallette 
  scale_y_continuous(labels=scales::percent) + # to show percents on y axis
  xlab("Part of Speech") +
  ylab("Percent") +
  ggtitle("Spanish Origins of Different Parts of Speech in Quechua") +
  theme_light()+
  labs(fill="Spanish Origin") + # name of legend
  theme(legend.background 
        = element_rect(color="black", size=.5))  # changing appearance of legen
colSpan



# PART 2 #

# at this point I would like to gather some additional data and compare different languages #


# let's go back to the quechua data set and in the new columns

# parts of speech
quechua$partsOfSpeech <- cSpeech$partsOfSpeech
# lev sim for spanish
quechua$spanishLevSim <- cSpeech$levSim
# Spanish Origin
quechua$spanishOrigin <- cSpeech$spanishOrgin
# tally
quechua$tally <- cSpeech$tally


# Now I would like to do some further levenshtein distances for 
# english, german, and italian

quechua$germanLevSim <- stringdist(quechua$`Hanan Runasimi`,quechua$Deutsch, method = "lv")
quechua$englishLevSim <- stringdist(quechua$`Hanan Runasimi`,quechua$English, method = "lv")
quechua$italianLevSim <- stringdist(quechua$`Hanan Runasimi`,quechua$Italiano, method = "lv")


# Now I am curious what the different languages look like in similarity

# english similarity
englishSimHist <- quechua %>%
  filter(quechua$englishLevSim < 50) %>%
  ggplot(aes(x=englishLevSim, y = ..density..)) + 
  scale_y_continuous(labels=scales::percent) + # to show percents on y axis
  geom_histogram( binwidth=2, fill="#f01158", color="#e9ecef", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("English Similarity to Quechua") +
  facet_wrap(~partsOfSpeech) +
  theme_update()
englishSimHist

# spanish similarity
spanishSimHist <- quechua %>%
  filter(quechua$spanishLevSim < 50) %>%
  ggplot(aes(x=spanishLevSim, y = ..density..)) + 
  scale_y_continuous(labels=scales::percent) + # to show percents on y axis
  geom_histogram( binwidth=2, fill="#f01158", color="#e9ecef", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Spanish Similarity to Quechua") +
  facet_wrap(~partsOfSpeech) +
  theme_update()
spanishSimHist

#italian similarity
spanishSimHist <- quechua %>%
  filter(quechua$italianLevSim < 50) %>%
  ggplot(aes(x=italianLevSim, y = ..density..)) + 
  scale_y_continuous(labels=scales::percent) + # to show percents on y axis
  geom_histogram( binwidth=2, fill="#f01158", color="#e9ecef", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Italian Similarity to Quechua") +
  facet_wrap(~partsOfSpeech) +
  theme_update()
spanishSimHist

# German Sim
spanishSimHist <- quechua %>%
  filter(quechua$germanLevSim < 50) %>%
  ggplot(aes(x=germanLevSim, y = ..density..)) + 
  scale_y_continuous(labels=scales::percent) + # to show percents on y axis
  geom_histogram( binwidth=2, fill="#f01158", color="#e9ecef", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("German Similarity to Quechua") +
  facet_wrap(~partsOfSpeech) +
  theme_update()
spanishSimHist



# I would also like some views with smaller bin sizes


# english similarity
englishSimHist <- quechua %>%
  filter(quechua$englishLevSim < 15) %>%
  ggplot(aes(x=englishLevSim, y = ..density..)) + 
  scale_y_continuous(labels=scales::percent) + # to show percents on y axis
  geom_histogram( binwidth=1, fill="#f01158", color="#e9ecef", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("English Similarity to Quechua") +
  facet_wrap(~partsOfSpeech) +
  theme_update()
englishSimHist

# spanish similarity
spanishSimHist <- quechua %>%
  filter(quechua$spanishLevSim < 15) %>%
  ggplot(aes(x=spanishLevSim, y = ..density..)) + 
  scale_y_continuous(labels=scales::percent) + # to show percents on y axis
  geom_histogram( binwidth=1, fill="#f01158", color="#e9ecef", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Spanish Similarity to Quechua") +
  facet_wrap(~partsOfSpeech) +
  theme_update()
spanishSimHist

#italian similarity
italianSimHist <- quechua %>%
  filter(quechua$italianLevSim < 15) %>%
  ggplot(aes(x=italianLevSim, y = ..density..)) + 
  scale_y_continuous(labels=scales::percent) + # to show percents on y axis
  geom_histogram( binwidth=1, fill="#f01158", color="#e9ecef", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Italian Similarity to Quechua") +
  facet_wrap(~partsOfSpeech) +
  theme_update()
italianSimHist

# German Sim
germanSimHist <- quechua %>%
  filter(quechua$germanLevSim < 15) %>%
  ggplot(aes(x=germanLevSim, y = ..density..)) + 
  scale_y_continuous(labels=scales::percent) + # to show percents on y axis
  geom_histogram( binwidth=1, fill="#f01158", color="#e9ecef", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("German Similarity to Quechua") +
  facet_wrap(~partsOfSpeech) +
  theme_update()
germanSimHist



# general languges no facet wrap

germanGeneral <- quechua %>%
  filter(quechua$germanLevSim < 10) %>%
  ggplot(aes(x=germanLevSim)) + 
  geom_histogram( binwidth=1, fill="#f01158", color="#d871de", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("German Similarity to Quechua") +
  theme_update()
germanGeneral

spanishGeneral <- quechua %>%
  filter(quechua$spanishLevSim < 10) %>%
  ggplot(aes(x=spanishLevSim)) + 
  geom_histogram( binwidth=1, fill="#f01158", color="#d871de", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Spanish Similarity to Quechua") +
  theme_update()
spanishGeneral

italianGeneral <- quechua %>%
  filter(quechua$italianLevSim < 10) %>%
  ggplot(aes(x=italianLevSim)) + 
  geom_histogram( binwidth=1, fill="#f01158", color="#d871de", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Italian Similarity to Quechua") +
  theme_update()
italianGeneral


englishGeneral <- quechua %>%
  filter(quechua$englishLevSim < 10) %>%
  ggplot(aes(x=englishLevSim)) + 
  geom_histogram( binwidth=1, fill="#f01158", color="#d871de", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Engish Similarity to Quechua") +
  theme_update()
englishGeneral


# what about just english to spanish differences?

quechua$engSpanLevSim <- stringdist(quechua$English,quechua$Español, method = "lv")

englishSpanishSim <- quechua %>%
  filter(quechua$engSpanLevSim < 10) %>%
  ggplot(aes(x=engSpanLevSim)) + 
  geom_histogram( binwidth=1, fill="#1ac7c7", color="#d871de", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Engish Similarity to Spanish") +
  theme_update()
englishSpanishSim


# curious about the means
meanSpan <- mean(quechua$spanishLevSim)
meanEnglish <- mean(quechua$englishLevSim)
meanGer <- mean(quechua$germanLevSim)
meanIta <- mean(quechua$italianLevSim)

summary(quechua$spanishLevSim)
summary(quechua$englishLevSim)
summary(quechua$germanLevSim)
summary(quechua$italianLevSim)


# in reviewing these statistics I think there might be hidden bias within my data
# there might be an issue that Spanish has way more description
# and that is why it is getting messed up compared to the other languages

# I want to parse down the translations to the first ; semicolon sign

quechua$cleanSpanish <- quechua$Español
# gsub can get rid of anything behind the semicolon
# so now I will only look at the first translation
quechua$cleanSpanish <-gsub(";.*","", quechua$cleanSpanish)

quechua$cleanEnglish <-gsub(";.*","", quechua$English)
quechua$cleanItalian <- gsub(";.*","", quechua$Italiano)
quechua$cleanGerman <- gsub(";.*","", quechua$Deutsch)

# need new levsims
quechua$spanishLevSimC <- stringdist(quechua$`Hanan Runasimi`,quechua$cleanSpanish, method = "lv")
quechua$germanLevSimC <- stringdist(quechua$`Hanan Runasimi`,quechua$cleanGerman, method = "lv")
quechua$englishLevSimC <- stringdist(quechua$`Hanan Runasimi`,quechua$cleanEnglish, method = "lv")
quechua$italianLevSimC <- stringdist(quechua$`Hanan Runasimi`,quechua$cleanItalian, method = "lv")


# now to see similarities with new variables
summary(quechua$spanishLevSimC)
summary(quechua$englishLevSimC)
summary(quechua$germanLevSimC)
summary(quechua$italianLevSimC)


# running again to see differences

germanGeneral <- quechua %>%
  filter(quechua$germanLevSimC < 50) %>%
  ggplot(aes(x=germanLevSimC)) + 
  geom_histogram( binwidth=1, fill="#f01158", color="#d871de", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("German Similarity to Quechua") +
  theme_update()
germanGeneral

spanishGeneral <- quechua %>%
  filter(quechua$spanishLevSimC < 50) %>%
  ggplot(aes(x=spanishLevSimC)) + 
  geom_histogram( binwidth=1, fill="pink", color="black", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Spanish Similarity to Quechua") +
  theme_update()
spanishGeneral


# NOTICE #
# italian is really strangely similiar. more similiar than Spanish it seems
# Should investigate this. Was not expecting this result
# upon further inquiry, I believe it is being biased by the phoneme
# Spanish doesn't even look at phoneme as pos
# I think phoneme should be excluded from analysis
italianGeneral <- quechua %>%
  filter(quechua$italianLevSimC < 50) %>%
  ggplot(aes(x=italianLevSimC)) + 
  geom_histogram( binwidth=1, fill="#f01158", color="#d871de", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Italian Similarity to Quechua") +
  theme_update()
italianGeneral


englishGeneral <- quechua %>%
  filter(quechua$englishLevSimC < 50) %>%
  ggplot(aes(x=englishLevSimC)) + 
  geom_histogram( binwidth=1, fill="#f01158", color="#d871de", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Engish Similarity to Quechua") +
  theme_update()
englishGeneral



# Interesting to look at Italian and Spanish

# spanish similarity
spanishSimHist <- quechua %>%
  filter(quechua$spanishLevSimC < 15) %>%
  ggplot(aes(x=spanishLevSimC, y = ..density..)) + 
  scale_y_continuous(labels=scales::percent) + # to show percents on y axis
  geom_histogram( binwidth=1, fill="pink", color="black", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Spanish Similarity to Quechua") +
  facet_wrap(~partsOfSpeech) +
  theme_update()
spanishSimHist

#italian similarity
italianSimHist <- quechua %>%
  filter(quechua$italianLevSimC < 15) %>%
  ggplot(aes(x=italianLevSimC, y = ..density..)) + 
  scale_y_continuous(labels=scales::percent) + # to show percents on y axis
  geom_histogram( binwidth=1, fill="#3389d4", color="#e9ecef", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Italian Similarity to Quechua") +
  facet_wrap(~partsOfSpeech) +
  theme_update()
italianSimHist



# So the phoneme is coded differently in different parts of the data set
# I do not think they are really great for my data set anyway
# because I am looking at dictionary words not phonemes
# so I am going to create a new dataset without any phonemes
# so that the phonemes do not bias my data

# just phonemes
quechuaDic <- quechua %>%
  filter(quechua$partsOfSpeech == "phoneme")

# no phonemes
quechuaC <- quechua %>%
  filter(quechua$partsOfSpeech != "phoneme")


# now to see similarities with new variables
summary(quechuaC$spanishLevSimC)
summary(quechuaC$englishLevSimC)
summary(quechuaC$germanLevSimC)
summary(quechuaC$italianLevSimC)


# spanish similarity
spanishSimHist <- quechuaC %>%
  filter(quechuaC$spanishLevSimC < 15) %>%
  ggplot(aes(x=spanishLevSimC, y = ..density..)) + 
  scale_y_continuous(labels=scales::percent) + # to show percents on y axis
  geom_histogram( binwidth=1, fill="pink", color="black", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Proportion of Word Type") +
  ggtitle("Spanish Similarity to Quechua by Part of Speech") +
  facet_wrap(~partsOfSpeech) +
  theme_update()
spanishSimHist

#italian similarity
italianSimHist <- quechuaC %>%
  filter(quechuaC$italianLevSimC < 15) %>%
  ggplot(aes(x=italianLevSimC, y = ..density..)) + 
  scale_y_continuous(labels=scales::percent) + # to show percents on y axis
  geom_histogram( binwidth=1, fill="pink", color="black", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Proportion of Word Type") +
  ggtitle("Italian Similarity to Quechua by Part of Speech") +
  facet_wrap(~partsOfSpeech) +
  theme_update()
italianSimHist

italianGeneral <- quechuaC %>%
  filter(quechuaC$italianLevSimC < 50) %>%
  ggplot(aes(x=italianLevSimC)) + 
  geom_histogram( binwidth=1, fill="pink", color="black", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Italian Similarity to Quechua") +
  theme_update()
italianGeneral



#English
englishGeneral <- quechuaC %>%
  filter(quechuaC$englishLevSimC < 50) %>%
  ggplot(aes(x=englishLevSimC)) + 
  geom_histogram( binwidth=1, fill="#f01158", color="#d871de", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Engish Similarity to Quechua") +
  theme_update()
englishGeneral

germanGeneral <- quechuaC %>%
  filter(quechuaC$germanLevSimC < 50) %>%
  ggplot(aes(x=germanLevSimC)) + 
  geom_histogram( binwidth=1, fill="#f01158", color="#d871de", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("German Similarity to Quechua") +
  theme_update()
germanGeneral

spanishGeneral <- quechuaC %>%
  filter(quechuaC$spanishLevSimC < 50) %>%
  ggplot(aes(x=spanishLevSimC)) + 
  geom_histogram( binwidth=1, fill="#f01158", color="#d871de", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Spanish Similarity to Quechua") +
  theme_update()
spanishGeneral


# the nouns are have a huge amount of overlap
# in italian with Quechua



# some stacked with cleaned up data

# stacked instead so you can see counts (fixed with tally)
stackSpan <- ggplot(quechuaC, aes(fill=spanishOrigin,  
                                 x=partsOfSpeech, y = tally)) + # fixed with tally
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Set1") + # pretty color pallete
  xlab("Part of Speech") +
  ylab("Word Count") +
  ggtitle("Spanish Origins of Different Parts of Speech in Quechua") +
  theme_light()+
  labs(fill="Spanish Origin") + # name of legend
  theme(legend.background 
        = element_rect(color="black", size=.5))  # changing appearance of legend
stackSpan

# now this should properly show proportions
colSpan <- ggplot(quechuaC, aes(fill=spanishOrigin,  
                               x=partsOfSpeech, y = tally)) + # fixed with tally
  geom_col(position="fill") +
  scale_fill_brewer(palette="Set1") + # pretty color pallette 
  scale_y_continuous(labels=scales::percent) + # to show percents on y axis
  xlab("Part of Speech") +
  ylab("Percent") +
  ggtitle("Spanish Origins of Different Parts of Speech in Quechua") +
  theme_light()+
  labs(fill="Spanish Origin") + # name of legend
  theme(legend.background 
        = element_rect(color="black", size=.5))  # changing appearance of legend
colSpan


# I think it might be easier to see where there is exact overlap if I highlight the 0
# distance for Levenshtein Distance since this means that two words are an exact pairing 

quechuaC$levVal0 <- quechuaC %>%
  filter(quechuaC$spanishLevSimC == 0)
levVal0

# creating a dummy variable for exact match in Spanish
quechuaC$exactMatch <- ifelse(quechuaC$spanishLevSimC == 0, "match", "different")

# another one for italian
quechuaC$italianExactMatch <- ifelse(quechuaC$italianLevSimC == 0, 1, 0)


# cannot quite get this to work
# am trying to highlight the first bar
# would love some help right here
spanishSimHist <- quechuaC %>%
  filter(quechuaC$spanishLevSimC < 15) %>%
  ggplot(aes(x= spanishLevSimC, fill = as.factor(exactMatch))) + 
  scale_y_continuous(labels=scales::percent) + # to show percents on y axis
  theme(legend.position="none") + # get rid of unnecessary legend, want for the color change
  geom_bar(aes(y = ..prop..)) +
  xlab("Levenshtein Distance") +
  ylab("Word Count") +
  ggtitle("Spanish Similarity to Quechua") +
  facet_wrap(~partsOfSpeech) +
  theme_update()
spanishSimHist


# could create a lexical similarity matrix
# similar to this: https://en.wikipedia.org/wiki/Lexical_similarity

lexMax <- read_excel("spanishQuechua.xlsx")

# what I am thinking


# add the other columns in for the matrix
lexMax$engSpan <- stringdist(lexMax$cleanEnglish,lexMax$cleanSpanish, method = "lv")
lexMax$engIta <- stringdist(lexMax$cleanEnglish,lexMax$cleanItalian, method = "lv")
lexMax$engGer <- stringdist(lexMax$cleanEnglish,lexMax$cleanGerman, method = "lv")
lexMax$engEng <- stringdist(lexMax$cleanEnglish,lexMax$cleanEnglish, method = "lv")
lexMax$spanIta <- stringdist(lexMax$cleanSpanish,lexMax$cleanItalian, method = "lv")
lexMax$spanGer <- stringdist(lexMax$cleanSpanish,lexMax$cleanGerman, method = "lv")
lexMax$gerIta <- stringdist(lexMax$cleanItalian,lexMax$cleanGerman, method = "lv")

# getting the mean of similiarity scores
summary(lexMax$engSpan) # mean 11.8611712 of english / spanish
summary(lexMax$engIta) # mean 12.3787313 of english / italian
summary(lexMax$engGer) # mean 12.1276596 of  english / german
summary(lexMax$englishLevSimC) # mean 14.2496952 of english / quechua
summary(lexMax$spanishLevSimC) # mean 13.5451685 of spanish / quechua
summary(lexMax$italianLevSimC) # mean 10.1435482 of italian / quechua
summary(lexMax$germanLevSimC) # mean 14.9328273 of german / quechua
summary(lexMax$spanIta) # mean 9.41412672 spanish / italian
summary(lexMax$spanGer) # mean 13.5799347 spanish / german
summary(lexMax$gerIta) # mean 13.9819877 german / italian

# I put this data into a matrix on excel
langMatrix <- read_excel("lexSimMatrix.xlsx")

# similarity to other mesoamerican languages
matLang <- as.matrix(langMatrix[, -1]) # creat matrix from all but first column
rownames(matLang) <- c("Quechua", "Spanish", "Italian", "German", "English") # adding back rownames
matLang

# now will make a heat map of matrix
# issue with weird cut off on the map
pdf("LanguageHeatMap2", width=10, height=8)
plot(as.assoc(matLang), 
     col=magma,
     reorder=FALSE,
     breaks=c(9,10,11,12,13,14,15), 
     xlab="", ylab="", 
     key=list(side=4, font=1), axis.key=NULL, 
     spacing.key=c(.75,.5,0),
     main="Mean Levenshtein Score Between Languages") 

# saving
dev.off()


# similarity of other languages of Mesoamerica

# in Wanuku = 
quechua$WanukuLevSim <- stringdist(quechua$`Hanan Runasimi`,quechua$Wanuku, method = "lv")
summary(quechua$WanukuLevSim)

# in the Aymara language
quechuaC$AymaraLevSim <- stringdist(quechuaC$`Hanan Runasimi`, quechuaC$Aymara, method="lv")
summary(quechuaC$AymaraLevSim) # 9.17892157

# Santiago del Estero / Tukuman (Tucumán - Argentina)  
quechuaC$TukumanLevSim <- stringdist(quechuaC$`Hanan Runasimi`, quechuaC$Tukuman, method="lv")
summary(quechuaC$TukumanLevSim) # 1.29206882

tukumanSimHist <- quechuaC %>%
  filter(quechuaC$TukumanLevSim < 15) %>%
  ggplot(aes(x=TukumanLevSim, y = ..density..)) + 
  scale_y_continuous(labels=scales::percent) + # to show percents on y axis
  geom_histogram( binwidth=1, fill="pink", color="black", alpha=0.9) +
  xlab("Levenshtein Distance") +
  ylab("Proportion of Word Type") +
  ggtitle("Tukuman Similarity to Quechua by Part of Speech") +
  facet_wrap(~partsOfSpeech) +
  theme_update()
tukumanSimHist

# Chanka (Ayakuchu, Ayacucho/Peru), written with a, e, i, o, u
quechuaC$AyakuchuLevSim <- stringdist(quechuaC$`Hanan Runasimi`, quechuaC$Ayakuchu, method="lv")
summary(quechuaC$AyakuchuLevSim)

# Qhochapampa (Cochabamba/Bolivia), former Bolivian writing system with a, e, i, o, 
quechuaC$QhochapampaLevSim <- stringdist(quechuaC$`Hanan Runasimi`, quechuaC$Qhochapampa, method="lv")
summary(quechuaC$QhochapampaLevSim)

#	Qullasuyu (Cusco-Boliviano: list of words from Qusqu and Bolivia, written in official 
# Bolivian standard of Southern Quechua, i.e. with a, i, u and j)
quechuaC$QullasuyaLevSim <- stringdist(quechuaC$`Hanan Runasimi`, quechuaC$Qullasuyu, method="lv")
summary(quechuaC$QullasuyaLevSim)

# Cajamarca/Peru, written with a, e, i, o, u and mb, ng, nd 
quechuaC$CajamarcaLevSim <- stringdist(quechuaC$`Hanan Runasimi`, quechuaC$`Cajamarca (Coombs)`, method="lv")
summary(quechuaC$CajamarcaLevSim)

# Kichwa: Shukllachishka Kichwa (Quichua Unificado del Ecuador), written in Unified Kichwa standard of Ecuador  (18)
quechuaC$KichwaLevSim <- stringdist(quechuaC$`Hanan Runasimi`, quechuaC$`Quichua (esp. quillcai)`, method="lv")
summary(quechuaC$KichwaLevSim)

#	Imbabura (Quichua Ecuatoriano), written in Spanish writing system  (20)
quechuaC$ImbaburaLevSim <- stringdist(quechuaC$`Hanan Runasimi`, quechuaC$`Imbabura (runashimi killkay)`, method="lv")
summary(quechuaC$ImbaburaLevSim)

# Wanka: Wankayu (Junín/Peru: Huancayo)  (21)
quechuaC$WankayuLevSim <- stringdist(quechuaC$`Hanan Runasimi`, quechuaC$Wanka, method="lv")
summary(quechuaC$WankayuLevSim)

# 	Ankash: Pumapampa (Ancash/Peru: Pomabamba), written with a, i, u, ay, aw, uy  (23)
quechuaC$PumapampaLevSim <- stringdist(quechuaC$`Hanan Runasimi`, quechuaC$`Ankash (ay, aw)`, method="lv")
summary(quechuaC$PumapampaLevSim)

# I put this data into a matrix on excel
langMatrix2 <- read_excel("levSimMatrixMesoAmerica.xlsx")

# similarity to other mesoamerican languages
matLang2 <- as.matrix(langMatrix2[, -1]) # creat matrix from all but first column
rownames(matLang2) <- c("Qullasuya",
                       "Kichwa",
                       "Cajamarca",
                       "Qhochapampa",
                       "Wankayu",
                       "Huánaco",
                       "Tukuman",
                       "Pumapampa",
                       "Imbabura",
                       "Aymara") # adding back rownames
matLang2

# now will make a heat map of matrix
# issue with weird cut off on the map
pdf("LanguageHeatMapIndigenousLang", width=5, height=10)
plot(as.assoc(matLang2), 
     col=magma,
     breaks=c(0,1,2,3,4,5,6,10), 
     reorder=FALSE,
     xlab="", ylab="", 
     key=list(side=4, font=1), axis.key=NULL, 
     spacing.key=c(.5,1,1),
     main="Mean Levenshtein Score of South American Languages") 

# saving
dev.off()




# exporting my excel currently to look at 
write_xlsx(quechuaC, "/Users/clairepost/Downloads/ANTH202/finalProj/QuechuaInfo.xlsx")

