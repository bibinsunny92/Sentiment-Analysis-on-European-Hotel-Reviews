getwd()
options(warn=-1)
HotelData = read.delim2("Hotel_Reviews_R.txt",quote = '', stringsAsFactors = FALSE)

# Intsalling packages
install.packages("tm")
install.packages("SnowballC")
install.packages("leaflet")
install.packages("stringr")
install.packages("syuzhet")
install.packages("wordcloud")

library(tm)
library(SnowballC)
library(leaflet)
library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(syuzhet)
library(wordcloud)

str(HotelData)

# Creating unique hotel name and geo-locations

hotel.names = HotelData %>%
  select(Hotel_Name, Hotel_Address, lat, lng, Average_Score, Total_Number_of_Reviews) %>%
  filter(lat != 0 & lng != 0 & !duplicated(Hotel_Address)) %>%
  group_by(Hotel_Name, Hotel_Address, lat, lng, Average_Score) %>%
  summarise(Total_Number_of_Reviews = sum(Total_Number_of_Reviews))

# Current geogrphical cordinates are in char, converting them to numeric
points <- cbind(as.numeric(hotel.names$lng),as.numeric(hotel.names$lat)) 

#Create Leaflet Map

leaflet() %>% 
  addProviderTiles('OpenStreetMap.Mapnik',
                   options = providerTileOptions(noWrap = TRUE)) %>%
  addMarkers(data = points,
             popup = paste0("<strong>Hotel: </strong>",
                            hotel.names$Hotel_Name,                 
                            "<br><strong>Address: </strong>", 
                            hotel.names$Hotel_Address, 
                            "<br><strong>Average Score: </strong>", 
                            hotel.names$Average_Score, 
                            "<br><strong>Number of Reviews: </strong>", 
                            hotel.names$Total_Number_of_Reviews),
                            #"<br><strong>Percent Positive Review Words: </strong>",
                            #hotel.names$Pos_Word_Rate),
             clusterOptions = markerClusterOptions())

#Click on the Orange Bubbles to zoom into exact precise location.
# Now we would add an extra column i.e. country, to the datasets. We can then start asking questions such as which country/city has the highest distribution of highly rated hotels.\

#Extracting country information

hotel.names$Country = sapply(str_split(hotel.names$Hotel_Address," "),function(X) {X[length(X)]})
hotel.names$Country = str_replace(hotel.names$Country, "Kingdom","U.K")

# Adding Country information to our main dataset
HotelData = HotelData %>% left_join(hotel.names[,c(2,7)],by = 'Hotel_Address')

#### Distribution of Hotel Reviews
# Histogram to see how user reviews overall are distributed
HotelData %>% select(Hotel_Name, Average_Score, Country) %>% group_by(Hotel_Name, Average_Score, Country) %>% ggplot(aes(x = as.numeric(Average_Score))) + geom_histogram(color = 'blue',fill = 'blue', alpha =0.4, bins = 30) + labs(x = "Average Rating", y = "No of People who provided Rating", title = "Distribution of Ratings")

### 4.3 Average Review Score for each Country

#Let's see if there are some countries where hotels get high ratings on an average? Seems like all hotels have similar ratings with Spain, Austria and France having higher ratings than the rest .
Hotel_Data = na.omit(HotelData)

# Generating Boxplot
Hotel_Data %>% ggplot(aes(x = as.factor(Country), y = as.numeric(Average_Score))) + geom_boxplot() + labs(x = 'Country', y = 'Average Score', title = 'Average Rating for each country')

#Bar chart
Hotel_Data %>% ggplot(aes(x = Country)) + geom_bar(position = position_stack(reverse = TRUE), color = "black", fill = "white") + coord_flip() + stat_count(aes(label = ..count..),geom = "text")

# Identifying the country
x = subset(Hotel_Data, Country=="France")
Hotel_Data %>% ggplot(aes(x = Country , y = Average_Score, col = Country)) + geom_bar(stat = "identity") + geom_text(label = Hotel_Data$Average_Score);

# Identifying the country
#Hotel_Data %>% ggplot(aes(x = Country , y = Average_Score, col = Country)) + geom_bar(stat = "identity") + geom_text(label = Hotel_Data$Total);

# Identifying which nationality of people  visited UK the most 

Hotel_Data %>% filter(Country == 'U.K', Reviewer_Nationality != ' United Kingdom ' ) %>% select(Reviewer_Nationality) %>% group_by(Reviewer_Nationality) %>% summarise(n=n()) %>% arrange(desc(n))%>% head(5)

# Identifying which nationality of people visited France the most 

Hotel_Data %>% filter(Country == 'France', Reviewer_Nationality != ' France ') %>% select(Reviewer_Nationality) %>% group_by(Reviewer_Nationality) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(5)

# Identifying the top hotels in each country
Hotel_Data %>% filter(Average_Score >9) %>% select(Hotel_Name, Country, Hotel_Address,Total_Number_of_Reviews,Average_Score, lng, lat) %>% group_by(Hotel_Name,Country) %>% distinct() %>% arrange(desc(Total_Number_of_Reviews),desc(Average_Score)) %>% head(10) -> Top_Hotels

leaflet() %>% 
  addProviderTiles('OpenStreetMap.Mapnik',
                   options = providerTileOptions(noWrap = TRUE)) %>%
  addMarkers(data = cbind(as.numeric(Top_Hotels$lng),as.numeric(Top_Hotels$lat)) ,
             popup = paste0("<strong>Hotel: </strong>",
                            Top_Hotels$Hotel_Name,                 
                            "<br><strong>Address: </strong>", 
                            Top_Hotels$Hotel_Address, 
                            "<br><strong>Average Score: </strong>", 
                            Top_Hotels$Average_Score, 
                            "<br><strong>Number of Reviews: </strong>", 
                            Top_Hotels$Total_Number_of_Reviews),
             clusterOptions = markerClusterOptions())

str(Hotel_Data)

########################### Sentiment Analysis #############################
# Building corpus
corpus = HotelData$Review
corpus = Corpus(VectorSource(corpus))
inspect(corpus[1:5])  # View top 5 comments

# Cleaning the text
corpus = tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus = tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

corpus = tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

corpus = tm_map(corpus, removeWords, stopwords('english'))
inspect(corpus[1:5])

corpus = tm_map(corpus, stripWhitespace)
inspect(corpus[1:5])

# Term Document Matrix - To convert unstructured data into structured data as rows and columns
tdm = TermDocumentMatrix(corpus)
tdm
tdm = removeSparseTerms(tdm, 0.999)

# Removing words which are irrelevant for eg. Room and Hotel,because it is obvious we are talking about rooms.

corpus = tm_map(corpus, removeWords, c('hotel','room'))
corpus = tm_map(corpus, stripWhitespace)

corpus = tm_map(corpus, gsub, pattern = "No Negative", replacement = "Positive")
corpus = tm_map(corpus, gsub, pattern = "no negative", replacement = "Positive")
corpus = tm_map(corpus, gsub, pattern = "No negative", replacement = "Positive")
corpus = tm_map(corpus, gsub, pattern = "No negatives", replacement = "Positive")

corpus = tm_map(corpus,gsub, pattern = "No Positive", replacement = "Negative")
tdm = as.matrix(tdm)

# To view first 10 rows and first 20 columns

tdm[1:10, 1:20]

# Creating boxplot

bargraph = rowSums(tdm)

bargraph = subset(bargraph, bargraph>3000)

barplot(bargraph,las = 2, col = rainbow(75))

### From the barplot we can see that "Staff","Breakfast" and "location" are the three words that people are talking the most

# Creating Bag of Words


wordCl = sort(rowSums(tdm), decreasing = FALSE)
set.seed(100)
wordcloud(words = names(wordCl), 
          freq = wordCl,
          max.words = 250,
          scale = c(5,0.3),
          random.order = F,
          colors = brewer.pal(8, 'Dark2'),
          rot.per = 0.7)



# Sentiment Analysis for tweets:

tweets <- as.character(corpus)
class(tweets)
## [1] "character"
# Obtain Sentiment scores 


sentiment <- get_nrc_sentiment(as.character(corpus))
head(sentiment)

# Bar Plot for Sentiment Analysis
barplot(colSums(sentiment), 
                las = 2, 
                col = rainbow(10),
                ylab = 'Count',
                main = 'Sentiment Scores for Hotel Reviews')





