library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(corrplot)
library(randomForest)
library(ranger)

#Loading Analysis & Scoring Data into R
songs = read.csv(file = '/Users/hdvoor/Downloads/analysisData.csv')
scoringData = read.csv(file = '/Users/hdvoor/Downloads/scoringData.csv')

#PRE-CLEANSING THE DATA, WANT TO SEE CORRELATION BETWEEN VARIABLES
library(corrplot)
cor_plot<-songs_final1[,c("track_duration","danceability","energy" ,"key","loudness","mode","speechiness","acousticness","instrumentalness","liveness","valence","tempo","time_signature","rating")]
cor_plot=mutate_all(cor_plot, function(x) as.numeric(as.character(x)))
f1<-cor(cor_plot)
corrplot(f1, method = 'circle',type="lower", tl.cex=0.6)

#EXAMINING STATISTICAL SIGNIFICANCE OF REGRESSION COEFFICIENTS AND VARIANCE INFLATING FACTOR (VIF)
model_vif = lm(rating~track_duration+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature,train)
library(broom)
summary(model_vif) %>%
  tidy()

library(car)
vif(model_vif)

data.frame(Predictor = names(vif(model_vif)), VIF = vif(model_vif)) %>%
  ggplot(aes(x=VIF, y = reorder(Predictor, VIF), fill=VIF))+
  geom_col()+
  geom_vline(xintercept=5, color = 'gray', size = 1.5)+
  geom_vline(xintercept = 10, color = 'red', size = 1.5)+
  scale_fill_gradient(low = '#fff7bc', high = '#d95f0e')+
  scale_y_discrete(name = "Predictor")+
  scale_x_continuous(breaks = seq(5,30,5))+
  theme_classic()

#CLEANSING & TRANSFORMING THE ANALYSIS DATA
#Regex to remove special characters ([] & '')
songs$genre<-gsub("\\[|\\]","",as.character(songs$genre))
songs$genre
songs$genre<-gsub("'","",as.character(songs$genre))
songs$genre

#Separating genre's into separate rows so there is one genre per row and thus unique combos of id and genre
songs %<>% mutate(cleaned_genre = genre) %>% separate_rows(cleaned_genre, sep = ",")

#Create new column that shows whether the genre was present or absent
songs = songs %>% mutate(present = 1)

songs$cleaned_genre[songs$cleaned_genre == ""] <- "No Genre"

#Wide pivot using genre names as column names and the present column as values. Also, filling with 0 if absent
songs = songs %>%  pivot_wider(names_from = cleaned_genre, values_from = present, values_fill = 0)
songs

#Analyzing the genre columns from the pivot_wide
songs_model = songs %>% select(-(1:4))
songs_model

songs_model$track_explicit <- as.integer(as.logical(songs_model$track_explicit))
colnames(songs_model)

songs_final <- songs
colnames(songs_final) <- gsub(" ","",colnames(songs_final))
colnames(songs_final)

#CLEANSING & TRANSFORMING THE SCORING DATA
#Regex to remove special characters ([] & '')
scoringData$genre<-gsub("\\[|\\]","",as.character(scoringData$genre))
scoringData$genre
scoringData$genre<-gsub("'","",as.character(scoringData$genre))
scoringData$genre

#Separating genre's into separate rows so there is one genre per row and thus unique combos of id and genre
scoringData %<>% mutate(cleaned_genre = genre) %>% separate_rows(cleaned_genre, sep = ",")

#Create new column that shows whether the genre was present or absent
scoringData = scoringData %>% mutate(present = 1)

scoringData$cleaned_genre[scoringData$cleaned_genre == ""] <- "No Genre"

#Wide pivot using genre names as column names and the present column as values. Also, filling with 0 if absent
scoringData = scoringData %>%  pivot_wider(names_from = cleaned_genre, values_from = present, values_fill = 0)


scoringData_final <- scoringData
colnames(scoringData_final) <- gsub(" ","",colnames(scoringData_final))
colnames(scoringData_final)

#MAKING TRACK_EXPLICIT NUMERIC IN BOTH ANALYSIS DATA AND SCORING DATA
songs_final1$track_explicit <- as.integer(as.logical(songs_final1$track_explicit))
scoringData_final1$track_explicit <- as.integer(as.logical(scoringData_final1$track_explicit))

#REMOVING WHITESPACE IN EVERY COLUMN NAME (EX. ADULT STANDARD BECOMES ADULTSTANDARD)
songs_final1 <- songs_final
colnames(songs_final1) <- gsub("&","",colnames(songs_final1))
colnames(songs_final1) <- gsub("-","",colnames(songs_final1))
colnames(songs_final1)

scoringData_final1 <- scoringData_final
colnames(scoringData_final1) <- gsub("&","",colnames(scoringData_final1))
colnames(scoringData_final1) <- gsub("-","",colnames(scoringData_final1))



#MODELING
#Setting Seed and Splitting the Data
library(caTools)
set.seed(617)
split = sample.split(songs_final1$rating,SplitRatio = 0.8)
train = songs_final1[split,]
test = songs_final1[!split,]



#BEST ATTEMPT: RANGER MODEL - TOP 200 VARIABLES WITH SPECIAL CHARACTERS REMOVED
forest_ranger3 = ranger(rating~track_duration+track_explicit+danceability+
                          energy+loudness+acousticness+instrumentalness+
                          tempo+mellowgold+softrock+adultstandards+rock+
                          pop+soul+motown+folkrock+poprap+rap+albumrock+
                          classicrock+dancepop+quietstorm+brillbuildingpop+
                          rockandroll+hiphop+urbancontemporary+bubblegumpop+
                          country+postteenpop+funk+NoGenre+rb+contemporarycountry+
                          countryroad+newwavepop+countryrock+trap+southernsoul+
                          poprock+hardrock+disco+folk+southernhiphop+lounge+
                          brillbuildingpop+hippop+classicsoul+dancepop+rockabilly+
                          artrock+classicsoul+rhythmandblues+merseybeat+psychedelicrock+
                          rootsrock+newjackswing+singersongwriter+classicukpop+yachtrock+
                          newromantic+gangsterrap+doowop+newwave+easylistening+dancerock+
                          neosoul+bluesrock+moderncountryrock+heartlandrock+postgrunge+
                          permanentwave+postdisco+atlhiphop+vocaljazz+europop+neomellow+
                          britishinvasion+countrydawn+disco+memphissoul+pop+alternativemetal+
                          bubblegumpop+canadianpop+phillysoul+symphonicrock+hollywood+synthpop+
                          hiphop+hardcorehiphop+glammetal+traditionalfolk+dancerock+
                          nashvillesound+protopunk+modernrock+pianorock+metal+numetal+
                          northernsoul+jazzfunk+boyband+funk+soulblues+dirtysouthrap+
                          country+sunshinepop+glamrock+lilith+gleeclub+classicgirlgroup+
                          melodicrap+jazzblues+oklahomacountry+chicagosoul+alternativerock+
                          edm+progressiverock+eastcoasthiphop+canadianpop+miamihiphop+
                          electricblues+girlgroup+countrypop+viralpop+canadianhiphop+
                          chicagorap+torontorap+freestyle+classiccountrypop+classicgaragerock+
                          tropicalhouse+bluesrock+classiccanadianrock+hiphouse+queenshiphop+
                          powerpop+redneck+ukpop+westcoastrap+chicagosoul+southernrock+
                          deeppoprb+classiccountrypop+alternativerock+deepadultstandards+
                          latin+conscioushiphop+latinpop+electropop+alternativerb+
                          outlawcountry+blues+indiepop+eastcoasthiphop+poppunk+minneapolissound+
                          albumrock+artrock+deepadultstandards+melodicrap+hinrg+stompandholler+
                          swing+detroithiphop+neomellow+britishinvasion+arkansascountry+beachmusic+
                          contemporarycountry+indierock+phillyrap+tropical+neworleansrap+doowop+
                          alternativehiphop+newwavepop+britishblues+talentshow+classicukpop+
                          reggaeton+dirtysouthrap+countryrock+comic+funkrock+indietronica+oldschoolhiphop+
                          glamrock+latin+baroquepop+modernrock+gfunk+classicrock+acousticpop+
                          canadiansingersongwriter+australianpop+folk+souljazz+newjackswing+
                          eurodance+gfunk+raprock+australianrock+glammetal+nashvillesound+
                          vocalharmonygroup,data=train,num.trees=2000)
#TRAIN RMSE
pred_forest_ranger3 = predict(forest_ranger3, data=train, num.trees=2000)
(rrmse_forest_ranger3 = sqrt(mean((pred_forest_ranger3$predictions-train$rating)^2)))

#TEST RMSE
pred_test_forest_ranger3 = predict(forest_ranger3, data=test, num.tress=2000)
(rmse_test_forest_ranger3 = sqrt(mean((pred_test_forest_ranger3$predictions - test$rating)^2)))


#RANDOM FOREST CODE WITH TOP 200 GENRES BASED ON FREQUENCY - BEST RANDOM FOREST MODEL ATTEMPT
#note: for sake of length, not including the code for top 10, top 25, top 50, top 100 and top 300
bag = randomForest(rating ~ track_duration+track_explicit+danceability+
                     energy+loudness+acousticness+instrumentalness+
                     tempo+mellowgold+softrock+adultstandards+rock+
                     pop+soul+motown+folkrock+poprap+rap+albumrock+
                     classicrock+dancepop+quietstorm+brillbuildingpop+
                     rockandroll+hiphop+urbancontemporary+bubblegumpop+
                     country+postteenpop+funk+NoGenre+rb+contemporarycountry+
                     countryroad+newwavepop+countryrock+trap+southernsoul+
                     poprock+hardrock+disco+folk+southernhiphop+lounge+
                     brillbuildingpop+hippop+classicsoul+dancepop+rockabilly+
                     artrock+classicsoul+rhythmandblues+merseybeat+psychedelicrock+
                     rootsrock+newjackswing+singersongwriter+classicukpop+yachtrock+
                     newromantic+gangsterrap+doowop+newwave+easylistening+dancerock+
                     neosoul+bluesrock+moderncountryrock+heartlandrock+postgrunge+
                     permanentwave+postdisco+atlhiphop+vocaljazz+europop+neomellow+
                     britishinvasion+countrydawn+disco+memphissoul+pop+alternativemetal+
                     bubblegumpop+canadianpop+phillysoul+symphonicrock+hollywood+synthpop+
                     hiphop+hardcorehiphop+glammetal+traditionalfolk+dancerock+
                     nashvillesound+protopunk+modernrock+pianorock+metal+numetal+
                     northernsoul+jazzfunk+boyband+funk+soulblues+dirtysouthrap+
                     country+sunshinepop+glamrock+lilith+gleeclub+classicgirlgroup+
                     melodicrap+jazzblues+oklahomacountry+chicagosoul+alternativerock+
                     edm+progressiverock+eastcoasthiphop+canadianpop+miamihiphop+
                     electricblues+girlgroup+countrypop+viralpop+canadianhiphop+
                     chicagorap+torontorap+freestyle+classiccountrypop+classicgaragerock+
                     tropicalhouse+bluesrock+classiccanadianrock+hiphouse+queenshiphop+
                     powerpop+redneck+ukpop+westcoastrap+chicagosoul+southernrock+
                     deeppoprb+classiccountrypop+alternativerock+deepadultstandards+
                     latin+conscioushiphop+latinpop+electropop+alternativerb+
                     outlawcountry+blues+indiepop+eastcoasthiphop+poppunk+minneapolissound+
                     albumrock+artrock+deepadultstandards+melodicrap+hinrg+stompandholler+
                     swing+detroithiphop+neomellow+britishinvasion+arkansascountry+beachmusic+
                     contemporarycountry+indierock+phillyrap+tropical+neworleansrap+doowop+
                     alternativehiphop+newwavepop+britishblues+talentshow+classicukpop+
                     reggaeton+dirtysouthrap+countryrock+comic+funkrock+indietronica+oldschoolhiphop+
                     glamrock+latin+baroquepop+modernrock+gfunk+classicrock+acousticpop+
                     canadiansingersongwriter+australianpop+folk+souljazz+newjackswing+
                     eurodance+gfunk+raprock+australianrock+glammetal+nashvillesound+
                     vocalharmonygroup, data=train, mtry = 5, ntree = 2000)
#TRAIN RMSE
pred_train = predict(bag)
(pred_train_rmse = sqrt(mean((pred_train - train$rating)^2)))  

#TEST RMSE
pred_test = predict(bag, data=test)
(pred_test_rmse = sqrt(mean((pred_test - test$rating)^2)))


#FIRST EVER ATTEMPT - PRIOR TO CLEANING DATA - LINEAR REGRESSION
model1 = lm(rating~danceability+energy+acousticness+liveness+tempo, songs)

#TRAIN RMSE
pred_train = predict(model1)
(rmse_train = sqrt(mean((pred_train- train$rating)^2)))

#TEST RMSE
pred_test = predict(model1, newdata=test)
(rmse_test = sqrt(mean((pred_test - test$rating)^2)))


#SUBMISSION TEMPLATE
#submission template
pred_submission_FINAL=predict(forest_ranger3, data=scoringData_final1)
submissionFile_FINAL = data.frame(id = scoringData_final1$id, rating = pred_submission_FINAL)
write.csv(submissionFile_FINAL, 'sample_submission_FINAL.csv',row.names =F)



#--------------------------------------------#APPENDIX - OTHER ATTEMPTS THAT WERE RELATIVELY UNSUCCESSFUL-------------------------------------------------------------------





# RANDOM FOREST - Using top 25 genres
bag = randomForest(rating~track_duration + track_explicit + danceability + energy + loudness + acousticness + instrumentalness + tempo + time_signature + mellowgold + softrock + adultstandards + rock + pop + soul + motown + folkrock + poprap + rap + albumrock + classicrock + dancepop + quietstorm + brillbuildingpop + hiphop + urbancontemporary + bubblegumpop + country + funk + NoGenre + contemporarycountry,data=train, mtry = 6, ntree = 600)
pred_train_brf3 = predict(bag)
(rmse_train_brf3 = sqrt(mean((pred_train_brf3 - train$rating)^2)))


#RANDOM FOREST - Using top 50 genres
bag = randomForest(rating~track_duration+track_explicit+danceability+energy+loudness+acousticness+instrumentalness+tempo+time_signature+mellowgold+softrock+adultstandards+rock+pop+soul+motown+folkrock+poprap+rap+albumrock+classicrock+dancepop+quietstorm+brillbuildingpop+hiphop+urbancontemporary+bubblegumpop+country+funk+NoGenre+contemporarycountry+countryroad+newwavepop+countryrock+trap+southernsoul+poprock+hardrock+disco+folk+southernhiphop+lounge+brillbuildingpop+hippop+classicsoul+dancepop+rockabilly+artrock+classicsoul+rhythmandblues+merseybeat+psychedelicrock+rootsrock+newjackswing+classicukpop+yachtrock+newromantic+gangsterrap+newwave+easylistening+dancerock+neosoul+bluesrock+moderncountryrock+heartlandrock+permanentwave+atlhiphop+vocaljazz+europop+neomellow+britishinvasion+countrydawn+disco+memphissoul+pop+alternativemetal+bubblegumpop+canadianpop+phillysoul+symphonicrock+hollywood+synthpop+hiphop+hardcorehiphop+glammetal+traditionalfolk+dancerock+nashvillesound+protopunk+modernrock+pianorock+metal+numetal+northernsoul+jazzfunk+boyband+funk+soulblues+dirtysouthrap+country+sunshinepop+glamrock+lilith+gleeclub+classicgirlgroup+melodicrap+jazzblues+oklahomacountry+chicagosoul+alternativerock+edm+progressiverock+eastcoasthiphop+canadianpop+miamihiphop+electricblues+girlgroup+countrypop+viralpop+canadianhiphop+chicagorap+torontorap+freestyle+classiccountrypop+classicgaragerock+tropicalhouse+bluesrock+classiccanadianrock+hiphouse+queenshiphop+powerpop+redneck+ukpop+westcoastrap+chicagosoul+southernrock+classiccountrypop+alternativerock+deepadultstandards+latin+conscioushiphop+latinpop+electropop, data=train,mtry = 6, ntree = 600) 
pred6_train = predict(bag)
(pred6_train_rmse = sqrt(mean((pred6_train - train$rating)^2)))


pred_test_brf = predict(bag, newdata=test)
(rmse_test_brf = sqrt(mean((pred_test_brf - test$rating)^2)))



#RANDOM FOREST - Using top 75 genres
bag = randomForest(rating~track_duration+track_explicit+danceability+energy+loudness+acousticness+instrumentalness+tempo+time_signature+mellowgold+softrock+adultstandards+rock+pop+soul+motown+folkrock+poprap+rap+albumrock+classicrock+dancepop+quietstorm+brillbuildingpop+hiphop+urbancontemporary+bubblegumpop+country+funk+NoGenre+contemporarycountry+countryroad+newwavepop+countryrock+trap+southernsoul+poprock+hardrock+disco+folk+southernhiphop+lounge+brillbuildingpop+hippop+classicsoul+dancepop+rockabilly+artrock+classicsoul+rhythmandblues+merseybeat+psychedelicrock+rootsrock+newjackswing+classicukpop+yachtrock+newromantic+gangsterrap+newwave+easylistening+dancerock+neosoul+bluesrock+moderncountryrock+heartlandrock+permanentwave+atlhiphop+vocaljazz+europop+neomellow+britishinvasion+countrydawn+disco+memphissoul+pop+alternativemetal+bubblegumpop+canadianpop+phillysoul+symphonicrock+hollywood+synthpop+hiphop+hardcorehiphop+glammetal+traditionalfolk+dancerock+nashvillesound+protopunk+modernrock+pianorock+metal+numetal+northernsoul+jazzfunk+boyband+funk+soulblues+dirtysouthrap+country+sunshinepop+glamrock+lilith+gleeclub+classicgirlgroup+melodicrap+jazzblues+oklahomacountry+chicagosoul+alternativerock+edm+progressiverock+eastcoasthiphop+canadianpop+miamihiphop+electricblues+girlgroup+countrypop+viralpop+canadianhiphop+chicagorap+torontorap+freestyle+classiccountrypop+classicgaragerock+tropicalhouse+bluesrock+classiccanadianrock+hiphouse+queenshiphop+powerpop+redneck+ukpop+westcoastrap+chicagosoul+southernrock+classiccountrypop+alternativerock+deepadultstandards+latin+conscioushiphop+latinpop+electropop+outlawcountry+blues+indiepop+eastcoasthiphop+poppunk+minneapolissound+albumrock+artrock+deepadultstandards+melodicrap+stompandholler+swing+detroithiphop+neomellow+britishinvasion+arkansascountry+beachmusic+contemporarycountry+indierock+phillyrap+tropical+neworleansrap+alternativehiphop+newwavepop+britishblues+talentshow+classicukpop+reggaeton+dirtysouthrap+countryrock+comic+funkrock+indietronica+oldschoolhiphop+glamrock+latin+baroquepop+modernrock+gfunk+classicrock+acousticpop+australianpop+folk+souljazz+newjackswing+eurodance+gfunk+raprock,data=train,ntree=800) 
pred7_train = predict(bag)
(pred7_train_rmse = sqrt(mean((pred7_train - train$rating)^2)))
#14.69393 RMSE - NOT GOOD ENOUGH


#RANGER - TOP 25 VARIABLES
forest_ranger5 = ranger(rating~track_duration+track_explicit+danceability+energy+loudness+acousticness+instrumentalness+tempo+mellowgold+softrock+adultstandards+rock+pop+soul+motown+folkrock+poprap+rap+albumrock+classicrock+dancepop+quietstorm+brillbuildingpop+rockandroll+hiphop+urbancontemporary+bubblegumpop+country+postteenpop+funk+NoGenre+rb,data=train,num.trees=600)
pred_forest_ranger5 = predict(forest_ranger5, data=train, num.trees=600)
(rrmse_forest_ranger5 = sqrt(mean((pred_forest_ranger5$predictions-train$rating)^2)))
#RMSE 9.900065
pred_test_forest_ranger5 = predict(forest_ranger5, data=test, num.tress=600)
(rmse_test_forest_ranger5 = sqrt(mean((pred_test_forest_ranger5$predictions - test$rating)^2)))
#RMSE 14.86135

#RANGER - TOP 50 VARIABLES WITH SPECIAL CHARACTERS REMOVED
forest_ranger1 = ranger(rating~track_duration+track_explicit+danceability+energy+loudness+acousticness+instrumentalness+tempo+mellowgold+softrock+adultstandards+rock+pop+soul+motown+folkrock+poprap+rap+albumrock+classicrock+dancepop+quietstorm+brillbuildingpop+rockandroll+hiphop+urbancontemporary+bubblegumpop+country+postteenpop+funk+NoGenre+rb+contemporarycountry+countryroad+newwavepop+countryrock+trap+southernsoul+poprock+hardrock+disco+folk+southernhiphop+lounge+brillbuildingpop+hippop+classicsoul+dancepop+rockabilly+artrock+classicsoul+rhythmandblues+merseybeat+psychedelicrock+rootsrock+newjackswing+singersongwriter+classicukpop,data=train,num.trees=600)
pred_forest_ranger1 = predict(forest_ranger1, data=train, num.trees=600)
(rrmse_forest_ranger1 = sqrt(mean((pred_forest_ranger1$predictions-train$rating)^2)))
#RMSE 10.02679
pred_test_forest_ranger1 = predict(forest_ranger1, data=test, num.tress=600)
(rmse_test_forest_ranger1 = sqrt(mean((pred_test_forest_ranger1$predictions - test$rating)^2)))
#RMSE 14.73273


#ATTEMPT #14 - TOP 75 VARIABLES
forest_ranger6 = ranger(rating~track_duration+track_explicit+danceability+energy+loudness+acousticness+instrumentalness+tempo+mellowgold+softrock+adultstandards+rock+pop+soul+motown+folkrock+poprap+rap+albumrock+classicrock+dancepop+quietstorm+brillbuildingpop+rockandroll+hiphop+urbancontemporary+bubblegumpop+country+postteenpop+funk+NoGenre+rb+contemporarycountry+countryroad+newwavepop+countryrock+trap+southernsoul+poprock+hardrock+disco+folk+southernhiphop+lounge+brillbuildingpop+hippop+classicsoul+dancepop+rockabilly+artrock+classicsoul+rhythmandblues+merseybeat+psychedelicrock+rootsrock+newjackswing+singersongwriter+classicukpop+yachtrock+newromantic+gangsterrap+doowop+newwave+easylistening+dancerock+neosoul+bluesrock+moderncountryrock+heartlandrock+postgrunge+permanentwave+postdisco+atlhiphop+vocaljazz+europop+neomellow+britishinvasion+countrydawn+disco+memphissoul+pop+alternativemetal+bubblegumpop,data=train,num.trees=600)
pred_forest_ranger6 = predict(forest_ranger6, data=train, num.trees=600)
(rrmse_forest_ranger6 = sqrt(mean((pred_forest_ranger6$predictions-train$rating)^2)))
#RMSE
pred_test_forest_ranger6 = predict(forest_ranger6, data=test, num.tress=600)
(rmse_test_forest_ranger6 = sqrt(mean((pred_test_forest_ranger6$predictions - test$rating)^2)))
#RMSE 14.86135

#RANGER - TOP 100 VARIABLES WITH SPECIAL CHARACTERS REMOVED
forest_ranger2 = ranger(rating~track_duration+track_explicit+danceability+energy+loudness+acousticness+instrumentalness+tempo+mellowgold+softrock+adultstandards+rock+pop+soul+motown+folkrock+poprap+rap+albumrock+classicrock+dancepop+quietstorm+brillbuildingpop+rockandroll+hiphop+urbancontemporary+bubblegumpop+country+postteenpop+funk+NoGenre+rb+contemporarycountry+countryroad+newwavepop+countryrock+trap+southernsoul+poprock+hardrock+disco+folk+southernhiphop+lounge+brillbuildingpop+hippop+classicsoul+dancepop+rockabilly+artrock+classicsoul+rhythmandblues+merseybeat+psychedelicrock+rootsrock+newjackswing+singersongwriter+classicukpop+yachtrock+newromantic+gangsterrap+doowop+newwave+easylistening+dancerock+neosoul+bluesrock+moderncountryrock+heartlandrock+postgrunge+permanentwave+postdisco+atlhiphop+vocaljazz+europop+neomellow+britishinvasion+countrydawn+disco+memphissoul+pop+alternativemetal+bubblegumpop+canadianpop+phillysoul+symphonicrock+hollywood+synthpop+hiphop+hardcorehiphop+glammetal+traditionalfolk+dancerock+nashvillesound+protopunk+modernrock+pianorock+metal+numetal+northernsoul+jazzfunk+boyband+funk+soulblues+dirtysouthrap+country+sunshinepop+glamrock,data=train,num.trees=600)
pred_forest_ranger2 = predict(forest_ranger2, data=train, num.trees=600)
(rrmse_forest_ranger2 = sqrt(mean((pred_forest_ranger2$predictions-train$rating)^2)))
#RMSE 10.85201
pred_test_forest_ranger2 = predict(forest_ranger2, data=test, num.tress=600)
(rmse_test_forest_ranger2 = sqrt(mean((pred_test_forest_ranger2$predictions - test$rating)^2)))
#RMSE 14.68947



#RANGER - TOP 250 VARIABLES WITH SPECIAL CHARACTERS REMOVED
forest_ranger4 = ranger(rating~track_duration+track_explicit+danceability+energy+loudness+acousticness+instrumentalness+tempo+mellowgold+softrock+adultstandards+rock+pop+soul+motown+folkrock+poprap+rap+albumrock+classicrock+dancepop+quietstorm+brillbuildingpop+rockandroll+hiphop+urbancontemporary+bubblegumpop+country+postteenpop+funk+NoGenre+rb+contemporarycountry+countryroad+newwavepop+countryrock+trap+southernsoul+poprock+hardrock+disco+folk+southernhiphop+lounge+brillbuildingpop+hippop+classicsoul+dancepop+rockabilly+artrock+classicsoul+rhythmandblues+merseybeat+psychedelicrock+rootsrock+newjackswing+singersongwriter+classicukpop+yachtrock+newromantic+gangsterrap+doowop+newwave+easylistening+dancerock+neosoul+bluesrock+moderncountryrock+heartlandrock+postgrunge+permanentwave+postdisco+atlhiphop+vocaljazz+europop+neomellow+britishinvasion+countrydawn+disco+memphissoul+pop+alternativemetal+bubblegumpop+canadianpop+phillysoul+symphonicrock+hollywood+synthpop+hiphop+hardcorehiphop+glammetal+traditionalfolk+dancerock+nashvillesound+protopunk+modernrock+pianorock+metal+numetal+northernsoul+jazzfunk+boyband+funk+soulblues+dirtysouthrap+country+sunshinepop+glamrock+lilith+gleeclub+classicgirlgroup+melodicrap+jazzblues+oklahomacountry+chicagosoul+alternativerock+edm+progressiverock+eastcoasthiphop+canadianpop+miamihiphop+electricblues+girlgroup+countrypop+viralpop+canadianhiphop+chicagorap+torontorap+freestyle+classiccountrypop+classicgaragerock+tropicalhouse+bluesrock+classiccanadianrock+hiphouse+queenshiphop+powerpop+redneck+ukpop+westcoastrap+chicagosoul+southernrock+deeppoprb+classiccountrypop+alternativerock+deepadultstandards+latin+conscioushiphop+latinpop+electropop+alternativerb+outlawcountry+blues+indiepop+eastcoasthiphop+poppunk+minneapolissound+albumrock+artrock+deepadultstandards+melodicrap+hinrg+stompandholler+swing+detroithiphop+neomellow+britishinvasion+arkansascountry+beachmusic+contemporarycountry+indierock+phillyrap+tropical+neworleansrap+doowop+alternativehiphop+newwavepop+britishblues+talentshow+classicukpop+reggaeton+dirtysouthrap+countryrock+comic+funkrock+indietronica+oldschoolhiphop+glamrock+latin+baroquepop+modernrock+gfunk+classicrock+acousticpop+canadiansingersongwriter+australianpop+folk+souljazz+newjackswing+eurodance+gfunk+raprock+australianrock+glammetal+nashvillesound+vocalharmonygroup+mellowgold+neworleansblues+torchsong+indiepoptimism+motown+hippop+britishsoul+freakbeat+garagerock+pianoblues+electropop+funkmetal+rapmetal+surfmusic+jazz+undergroundhiphop+pubrock+indiefolk+texascountry+electrohouse+canadiancontemporaryrb+jazzfusion+electro+atltrap+europop+louisianablues+gangsterrap+poprap+smoothjazz+mexicanpop+traditionalcountry+permanentwave+softrock+candypop+americanfolkrevival+rhythmandblues+artpop+emo+traditionalblues+conscioushiphop+newamericana+metropopolis+canadiancountry+bubblegumdance+classicgaragerock+floridarap+janglepop+canadianrock+divahouse+vocalhouse+crunk,data=train,num.trees=5000)
pred_forest_ranger4 = predict(forest_ranger4, data=train, num.trees=5000)
(rrmse_forest_ranger4 = sqrt(mean((pred_forest_ranger4$predictions-train$rating)^2)))
#RMSE w/ 600 trees - 11.31211, RMSE w/800 trees - 11.27825
pred_test_forest_ranger4 = predict(forest_ranger4, data=test, num.tress=5000)
(rmse_test_forest_ranger4 = sqrt(mean((pred_test_forest_ranger4$predictions - test$rating)^2)))
#RMSE w/ 600 trees - 14.61119, RMSE w/ 800 trees - 14.59849




#ATTEMPT TO RUN FORWARD SELECTION WITH THE TOP 25 GENRES
start_mod = lm(rating~1,data=train)
empty_mod = lm(rating~1,data=train)
full_mod = lm(rating~track_duration + track_explicit + danceability + energy + loudness + acousticness + instrumentalness + tempo + time_signature + mellowgold + softrock + adultstandards + rock + pop + soul + motown + folkrock + poprap + rap + albumrock + classicrock + dancepop + quietstorm + brillbuildingpop + hiphop + urbancontemporary + bubblegumpop + country + funk + NoGenre + contemporarycountry,data=train)
forwardStepwise = step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='forward')

forwardStepwise$anova %>% 
  mutate(step_number = as.integer(rownames(forwardStepwise$anova))-1) %>%
  mutate(Step = as.character(Step))%>%
  ggplot(aes(x = reorder(Step,X = step_number), y = AIC))+
  geom_point(color = 'darkgreen', size = 2) + 
  scale_x_discrete(name = 'Variable Entering Model')+
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9))
