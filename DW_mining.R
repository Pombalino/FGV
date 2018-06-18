library(textreadr)
library(tidyverse)
library(tidytext)
library(tidyr)
library(magrittr)
library(dplyr)
library(reprex)
library(wordcloud)
library(scales)
library(ggplot2)

doctor_list <- list.dirs(path="C:/Users/Usuario/Documents/Text Mining Exercise")
doctor_list <- doctor_list[2:13]
l <- list(doctor="", document = "", content = "")
all_doctors <- as.tibble(l)

# cria dfs com os conteúdos de cada doctor alinhados por arquivo (ep)
for (doutor in doctor_list){
  doc <- basename(doutor) # Pega o nome cada Doctor pra poder usar como string
  doc_tibble <- as.tibble(read_dir(doutor)) #lê todos os arquivos em cada diretório
  doc_tibble <- add_column(doc_tibble, doctor = doc, .before = 1) #adiciona o nome em uma nova col
  all_doctors <- rbind(all_doctors,doc_tibble) #all_doctors nunca foi inicializado mas 
                                               #vai ser preenchido
}

# agrupando os textos
by_doctor <- all_doctors %>% group_by(doctor)
by_episode <- by_doctor %>% group_by(document)
doctor_lines <- mutate(by_episode, linenumber = row_number())

# remove html clutter
doctor_cleaner <- doctor_lines
doctor_cleaner$content <- str_remove_all(doctor_cleaner$content,"<[a-z]>")
doctor_cleaner$content <- str_remove_all(doctor_cleaner$content,"</[a-z]>")
doctor_cleaner$content <- str_remove_all(doctor_cleaner$content,"<[a-z][a-z]>")
doctor_cleaner$content <- str_remove_all(doctor_cleaner$content,"<[a-z][a-z]/>")
doctor_cleaner$content <- str_remove_all(doctor_cleaner$content,"\\[")
doctor_cleaner$content <- str_remove_all(doctor_cleaner$content,"\\]")
doctor_cleaner$content <- str_remove_all(doctor_cleaner$content,"\\\\n")
doctor_cleaner$content <- str_remove_all(doctor_cleaner$content,"\\\\r")
doctor_cleaner$content <- str_remove_all(doctor_cleaner$content,"[A-Z]*:")
#linha acima tira o personagem de cada fala (padrão: "NEOM:")

# ungrouping
doctor_cleaner2 <- as.tibble(doctor_cleaner[2:79327,])
doctor_cleaner2
doctor_ungrouped <- ungroup(doctor_cleaner2)

# tidy, count and sort
tidy_doctor <- unnest_tokens(doctor_ungrouped,word,content)
data(stop_words)
tidy_doctor <- anti_join(tidy_doctor,stop_words)
tidy_sort <- count(tidy_doctor,word,sort=TRUE)

# ggplot
filter(tidy_sort,n>2070) -> tidy_filter
mutate(tidy_filter,word=reorder(word,n)) -> tidy_mutate

ggplot(tidy_mutate,aes(word,n))+geom_col()+xlab(NULL)+coord_flip()->tidy_ggplot
print(tidy_ggplot)

# wordcloud
wordcloud(tidy_sort$word,tidy_sort$n,
          max.words = 25,
          rot.per = FALSE,colors= c("#5740F2", "#165FA8", "#D5ADDF", "#28B9D8"))

# Análise de sentimentos
doctor_join <- inner_join(tidy_doctor,get_sentiments("bing")) 
doc_count <- count(doctor_join,doctor, index=linenumber %/% 10,sentiment)
spread(doc_count,sentiment,n,fill=0) -> doc_spread
doctor_sentiment <- mutate(doc_spread,sentiment=positive-negative)

doc_plot <- ggplot(doctor_sentiment,aes(index,sentiment,fill=doctor))
doc_plot + geom_col(show.legend = FALSE)->doc_col
doc_col+facet_wrap(~doctor,ncol=2,scales="free_x")->doc_wrap
print(doc_plot)
print(doc_col)
print(doc_wrap)

# palavras que mais contribuiram para os sentimentos
bing_word_counts <- doctor_join %>% count(word,sentiment,sort=TRUE)%>%ungroup()
bing_word_counts%>%
  group_by(sentiment)%>%
  top_n(10)%>%ungroup()%>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n,fill=sentiment))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment,scales="free_y")+
  labs(y="Contribution to sentiment",x=NULL)+
  coord_flip()->bing_ggplot
print(bing_ggplot)

# outras escalas no 4º Doctor
# AFINN
fourth <- filter(tidy_doctor,doctor=="Fourth Doctor")
afinn <- fourth %>%inner_join(get_sentiments("afinn"))%>%
  group_by(index=linenumber %/%25)%>%summarise(sentiment=sum(score))%>%
  mutate(method="AFINN")
# Bing and NRC
bing_and_nrc <- bind_rows(fourth %>% inner_join(get_sentiments("bing"))%>% mutate(method="Bing et al."),fourth %>%inner_join(get_sentiments("nrc")%>%filter(sentiment %in% c("positive","negative")))%>%mutate(method="NRC")) %>%
  count(method,index=linenumber %/%25,sentiment)%>%
  spread(sentiment,n,fill=0)%>%
  mutate(sentiment=positive-negative)
bind_rows(afinn,bing_and_nrc)%>%ggplot(aes(index,sentiment,fill=method))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~method,ncol=1,scales="free_y")->lexicons_doctor
print(lexicons_doctor)

# Sentiment Analysis per episode for a specific Doctor
nth_doctor <- filter(tidy_doctor,doctor=="Tenth Doctor")
fourth_join <- inner_join(nth_doctor,get_sentiments("bing")) 
fourth_count <- count(fourth_join,document, index=linenumber %/% 1,sentiment)
spread(fourth_count,sentiment,n,fill=0) -> fourth_spread
fourth_sentiment <- mutate(fourth_spread,sentiment=positive-negative)
fourth_plot <- ggplot(fourth_sentiment,aes(index,sentiment,fill=document))
fourth_plot + geom_col(show.legend = FALSE)->fourth_col
fourth_col+facet_wrap(~document,ncol=2,scales="free_x")->fourth_wrap
print(fourth_wrap)

# Comparing nth Doctor to mth Doctor
nth <- "First Doctor"
mth <- "Twelfth Doctor"
nth_doc <- filter(tidy_doctor,doctor==nth) %>% count(word,sort=TRUE)
mth_doc <- filter(tidy_doctor,doctor==mth)%>% count(word,sort=TRUE)
mutate(nth_doc,doctor=nth)->nth_doc2
mutate(mth_doc,doctor=mth)->mth_doc2
bind_rows(nth_doc2,mth_doc2) -> tidy_bind
combined_count <- count(tidy_bind,doctor,word)
combined_group <- group_by(combined_count,doctor)
mutate(combined_group,proportion=nn/sum(nn)) -> combined_mutate
combined_select <- select(combined_mutate,-nn)
spread(combined_select,doctor,proportion) -> combined_spread
combined_gather <- gather(combined_spread,doctor,proportion,mth)
combined_ggplot <- ggplot(combined_gather,aes(x=proportion,y=nth,
                                              color=abs(nth-proportion)))
combined_ggplot + geom_abline(color="gray40",lty=2) -> combined_line
combined_line <- combined_line + geom_jitter(alpha=0.1,size=2.5,
                                            width = 0.3,height=0.3)
print(combined_line)
# Error in nth - proportion : non-numeric argument to binary operator
combined_text <- combined_line + geom_text(aes(label=word),
                                          check_overlap = TRUE,vjust=1.5)
combined_scale <- combined_text+scale_x_log10(labels=percent_format())+
  scale_y_log10(labels=percent_format())

