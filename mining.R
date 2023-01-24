install.packages("tuber")
library(tuber)

id<- ""
pass<- ""

yt_oauth(id,pass)

amazon <- get_all_comments(video_id = "0wP5d050fl0")
write.table(amazon, "amazon.csv")

stadia <- get_all_comments(video_id = "gg9CbWmhxNw")
write.table(stadia, "stadia.csv")

xcloud <- get_all_comments(video_id = "ok3EYsvLODQ")
write.table(xcloud, "xcloud.csv")

geforce <- get_all_comments(video_id = "4GRJNbYQm00")
write.table(geforce, "geforce.csv")

geforce_s <- get_all_comments(video_id = "MsMwkz75oJU")
write.table(geforce_s, "geforce_s.csv")

techopadplus <- get_all_comments(video_id = "AUsg9QSwUps")
write.table(techopadplus, "geforceplus.csv")

geforceD <- get_all_comments(video_id = "Ken8arVoQgo")
write.table(geforceD, "geforceD.csv")

stadia2 <- get_all_comments(video_id = "-oPcBmITLhU")
write.table(stadia2, "stadia2.csv")




install.packages("tm")
install.packages("RCurl")
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("funModeling")
install.packages("lubridate")
install.packages("stringr")
install.packages("tidytext")
install.packages("wordcloud2")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tibble")
install.packages("tidyr")
install.packages("readr")
install.packages("readxl")
install.packages("ggpubr")
install.packages("formattable")
install.packages("ggstance")
install.packages("psych")
install.packages("GGaly")
install.packages("rstatix")
install.packages("sentimentr")
install.packages("webshot")
install.packages("htmlwidgets")
install.packages("syuzhet")
install.packages("nabor")
install.packages("data.table")
install.packages("gutenbergr")
install.packages("stopwords")
install.packages("NLP")


library(NLP)
library(tm)
library(RCurl)
library(magrittr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(funModeling)
library(lubridate)
library(stringr)
library(tidytext)
library(wordcloud2)
library(wordcloud)
library(RColorBrewer)
library(tibble)
library(tidyr)
library(readr)
library(readxl)
library(ggpubr)
library(formattable)
library(ggstance)
library(psych)
library(GGaly)
library(rstatix)
library(sentimentr)
library(webshot)
library(htmlwidgets)
library(syuzhet)
library(nabor)
library(data.table)
library(gutenbergr)
library(stopwords)


#işlem yapacağımız csv dosyasını çağırır.
bulut <- read.csv(file.choose())
View(bulut)


# linkleri siler
bulut$text<-str_replace_all(bulut$text,"http[^[:space:]]*","")


# "@" ve "#" işaretlerini kaldırır.
bulut$text<-str_replace_all(bulut$text,"#\\S+","")
bulut$text<-str_replace_all(bulut$text,"@\\S+","")
view(bulut)


# Noktalama isaretlerini kaldırır.
bulut$text<-str_replace_all(bulut$text, "[[:punct:][:blank:]]+", " ")

# tüm harfleri küçüğe dönüştürür.
bulut$text<-str_to_lower(bulut$text,"tr")
view(bulut)

# Veri içindeki sayılar temizlenir
bulut$text<-removeNumbers(bulut$text)


#ASCII formatina uymayankarakterleri tezmizleme islemi yapiliyor
bulut$text<-str_replace_all(bulut$text,"[<].*[<]"," ")
bulut$text<-gsub("\ uFFFD","",bulut$text,fixed = TRUE)


#Alfabetik olamyan harfleri temizleme islemi.
bulut$text<-str_replace_all(bulut$text,"[^[:alnum:]]"," ")


# işlem yapacağımız kelime havuzundan çıkarmak istediğimiz gereksiz kullanmıyacağımız kelimeler.
liste=c("href","mu","abi","var","ama","ya","ben","yok","mi","gibi","mı","benim",
        "bi","olur","bile","zaten","ekran","sonra","sadece","istiyorum","mailim","br","iyi","bana","mail","bana","güzel","lt",
        "oyunları","çıkar","kodu","bende","by","creed","valhalla","go","assassin","teşekkürker","recep","video","kötü","lütfen",
        "isterim","ac","oyunu","inşallah","diye","k","ilk","hiç","powered","ne","umarım","d","the","kadar","değil","dandik",
        "of","ki","internetim","legends","bence","artık","nasıl","adresim","kod","oyunlar","teşekkürler","denemek","para","cs","p",
        "aylık","zaman","olmuş","olacak","şu","çünkü","şey","tr","aynı","kendi","olsa","sen","bakalım","fazla","hem","hem","bunu",
        "lazım","yorum","az","six","ile","bir","ve","bu","çok","için","da","de","daha","o","a","en","her","yani","olarak","olan",
        "olsun","e","ye","böyle","falan","düşük","şeklinde","oluyor","rust","siege","dead","strike","offensive","death","wild","cry",
        "cry","desert")

iconv(liste,"UTF-8","UTF-8")
liste



# Bağlaçlar ve gereksiz tekrarlamalar kaldırılır.
bulut$text<-removeWords(bulut$text,liste)

pluss$word<- removeWords(pluss$word,liste)

# kelimeleri alt alta sıralama işlemi.
sirali <- bulut %>% select(text) %>% mutate(linenumber = row_number()) %>% unnest_tokens(word,text)



#en çok tekrarlanan kelimelerin listeni bize tablo şeklinde verir.
sirali %>%
  count(word, sort = TRUE) %>%
  filter(n > 150) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  theme_bw()+
  xlab(NULL) +
  coord_flip() + 
  ggtitle("Yorumlarda en çok kullanılan kelimeler")



# kelime bulutu yapımı
sirali %>% 
  count(word) %>% 
  with(wordcloud(word,n, max.words=40,colors = brewer.pal(11,"Spectral")))




pluss<-sirali %>% group_by(word) %>% count() %>% arrange(desc(n))
pluss<-pluss %>% add_column(id=1:NROW(pluss),.before = "word")
pluss %>% head(50) %>% data.table( colnames = c('Sıra' = "id", 'Kelime' = "word"))

pluss %>%
  DT::datatable(colnames = c('Sıra' = "id", 'Kelime' = "word", "Frekansı"="n"),
                extensions = 'Buttons', 
                options = list(dom = 'Bfrtip', 
                               buttons = c('excel', "csv")))

install.packages("hwordcloud")
library(hwordcloud)

hwordcloud(text  = head(pluss$word,100), size = head(pluss$n,100),
           width = "100%", height = "500px",
           theme = "darkblue")


install.packages("sentimentr")
library(sentimentr)

polarite<-sentiment(pluss$word)

stat.desc(polarite$sentiment, basic=T) %>% pander()



polarite<-sentiment(pluss$word)


tablo<-cbind(pluss$word, polarite[,c(3,4)])
view(pluss)

ggplot(tablo, aes(word_count, sentiment))+
  geom_point(color="blue")+
  geom_hline(yintercept = mean(tablo$sentiment), color="red", size=1)+
  labs(y = "Skor", x = "Kelimelerin Frekansı") +
  labs(caption = "Oyunlarda Bulut Bilişim")+
  theme(plot.caption = element_text(hjust = 0, face = "italic"))



duygu<-pluss[,-1] %>% inner_join(get_sentiments("bing"),by="word")

duygu[,c(1,3,2)]%>% group_by(sentiment) %>% summarise(toplam=sum(n)) %>%
  mutate(oran=round(toplam/sum(toplam)*100,2)) %>% arrange(desc(oran)) %>% 
  rename("duygu"="sentiment")

duygu[,c(1,3,2)]%>% group_by(sentiment) %>% arrange(desc(n)) %>%
  top_n(10) %>%
  ggplot(aes(x=reorder(word,n), y=n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Kelime",x = "Sıklık") +
  coord_flip() +
  labs(caption = "Oyunlarda Bulut Bilişim")+
  theme(plot.caption = element_text(hjust = 0, face = "italic"))


install.packages("textdata")
library(textdata)
duygu<-pluss %>% inner_join(get_sentiments("afinn"),by="word")
stat.desc(duygu$value, basic=T) %>% pander()

library(syuzhet)
set.seed(1985) 
duygu4<-pluss %>%inner_join(get_sentiments("loughran"),by="word") 

duygu4 %>% group_by(sentiment) %>% summarise(toplam=n()) %>%
  mutate(oran=round(toplam/sum(toplam)*100,2)) %>%
  arrange(desc(oran)) %>%  rename("duygu"="sentiment")

duygu4 %>% 
  group_by(sentiment) %>% summarise(n=n())%>%
  ggplot(aes(reorder(sentiment, n), n, fill=sentiment)) +
  geom_bar(stat="identity", show.legend = FALSE)+
  labs(y = "Frekans", x = "Duygu")+
  
  labs(caption = "Oyunlarda Bulut Bilişim")+
  theme(plot.caption = element_text(hjust = 0, face = "italic"))  
  
  






