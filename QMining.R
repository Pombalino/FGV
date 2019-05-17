library(tidytext)
library(tidyr)
library(magrittr)
library(dplyr)
library(stringr)
library(ggplot2)
library(wordcloud)

ans <- read.delim(file="C:/Users/Pohlmann/Documents/FGV/6º Semestre/MktMix/PorFim.txt",header = FALSE,stringsAsFactors = FALSE)
anst<-as_tibble(ans)

anl <- mutate(anst, linenumber = row_number())

ung<-ungroup(anl)

print(ung)

tidyans<-unnest_tokens(ung,word,V1)
tidyans$word<-str_replace_all(tidyans$word,"jovens","jovem")
tidyans$word<-str_replace_all(tidyans$word,"sports","esporte")
tidyans$word<-str_replace_all(tidyans$word,"futebol","esporte")
tidyans$word<-str_replace_all(tidyans$word,"homem","homens")
tidyans$word<-str_replace_all(tidyans$word,"nívea","nivea")
tidyans$word<-str_replace_all(tidyans$word,"youtube","digital")
tidyans$word<-str_replace_all(tidyans$word,"etc","digital")
tidyans$word<-str_replace_all(tidyans$word,"propagandas","propaganda")
tidyans$word<-str_replace_all(tidyans$word,"produtos","produto")
tidyans$word<-str_replace_all(tidyans$word,"academias","esporte")
tidyans$word<-str_replace_all(tidyans$word,"marketing","propaganda")
tidyans$word<-str_replace_all(tidyans$word,"atingir","jovem")
tidyans$word<-str_replace_all(tidyans$word,"atinge","masculina")
tidyans$word<-str_remove_all(tidyans$word,'deixar')

anssort<-count(tidyans,word,sort=TRUE)

cleaner<-anti_join(anssort,get_stopwords(language="pt"))

wordcloud(cleaner$word,cleaner$n,
          max.words = 15,
          rot.per = FALSE,colors=c('#00ffaa','#bf00ff','#2040ff','#40A0ff','#8900ff'),
          random.color=FALSE)

# bigrams
tidyans2<-unnest_tokens(ung,word,V1,token = 'ngrams',n=2)
tidyans2$word<-str_replace_all(tidyans2$word,"jovens","jovem")
tidyans2$word<-str_replace_all(tidyans2$word,"sports","esporte")
tidyans2$word<-str_replace_all(tidyans2$word,"futebol","esporte")
tidyans2$word<-str_replace_all(tidyans2$word,"homem","homens")
tidyans2$word<-str_replace_all(tidyans2$word,"nívea","nivea")
tidyans2$word<-str_replace_all(tidyans2$word,"youtube","digital")
tidyans2$word<-str_replace_all(tidyans2$word,"propagandas","propaganda")
tidyans2$word<-str_replace_all(tidyans2$word,"produtos","produto")

anssort2<-count(tidyans2,word,sort=TRUE)

cleaner2<-anti_join(anssort2,get_stopwords(language="pt"))

wordcloud(cleaner2$word,cleaner2$n,
          max.words = 30,
          rot.per = FALSE,colors=c('#00ffaa','#bf00ff','#2040ff','#40A0ff','#8900ff'),
          random.color=FALSE)