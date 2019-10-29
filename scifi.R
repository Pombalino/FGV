library(tidytext)
library(tidyr)
library(magrittr)
library(dplyr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(gutenbergr) # API do Projeto Gutenberg (https://www.gutenberg.org/)
library(scales)

head(gutenberg_metadata) # só pra conhecer a carinha do gutenbergR
data(stop_words)

lovecraft <- gutenberg_works(author=="Lovecraft, H. P. (Howard Phillips)")%>%gutenberg_download(meta_fields = "title")
tidy_hpl <- lovecraft %>%unnest_tokens(word,text)%>%anti_join(stop_words)

eapoe <- gutenberg_works(author=="Poe, Edgar Allan")%>%gutenberg_download(meta_fields = "title")
tidy_poe <- eapoe %>%unnest_tokens(word,text)%>%anti_join(stop_words) # pega os livros do Edgar Allan Poe, remove as stopwords e separa as palavras
poe_sort <- count(tidy_poe,word,sort=TRUE) # e prepara igual os da Jane Austen
poe_filter <- filter(poe_sort,n>320) # Feliz Halloween
mutate(poe_filter,word=reorder(word,n)) -> poe_mutate

ggplot(poe_mutate,aes(word,n))+geom_col()+xlab(NULL)+coord_flip()->tidy_ggplot
print(tidy_ggplot)

wordcloud(poe_sort$word,poe_sort$n,
          max.words = 50,
          rot.per = FALSE,colors= c("#AB329C", "#195B5B", "#6D8D2F", "#287928"))

asimov <- gutenberg_works(author=="Asimov, Isaac")%>%gutenberg_download(meta_fields = "title")
tidy_asi <- asimov %>%unnest_tokens(word,text)%>%anti_join(stop_words)
asi_sort <- count(tidy_asi,word,sort=TRUE)
asi_filter <- filter(asi_sort,n>50) # Feliz Halloween
mutate(asi_filter,word=reorder(word,n)) -> asi_mutate

ggplot(asi_mutate,aes(word,n))+geom_col()+xlab(NULL)+coord_flip()->tidy_ggplot
print(tidy_ggplot)


# aqui junta os tibbles e remove metadados do projeto Gutenberg de acordo com o padrão deles
bind_rows(mutate(tidy_asi, author="Isaac Asimov"), mutate(tidy_poe,author='Edgar Allan Poe'),mutate(tidy_hpl,author='H.P. Lovecraft')) -> tidy_bind
tidy_bind <- mutate(tidy_bind, word=str_extract(word,"[a-z']+"))

tidy_count <- count(tidy_bind,author,word) # conta as palavras usadas por cada autor
group_by(tidy_count,author)->tidy_group # reagrupa tudo

mutate(tidy_group,proportion = n/sum(n))->tidy_mutate # cria as proporções de uso (palavra/total de palavras do autor)
select(tidy_mutate,-n)->tidy_selected # e remove a frequência absoluta

tidy_spread <- spread(tidy_selected,author,proportion) # essas linhas preparam o tibble para irem sem problemas para o gráfico
tidy_gather<-gather(tidy_spread,author,proportion,'Isaac Asimov':'H.P. Lovecraft')

ggplot(tidy_gather, aes(x = proportion, y = `Edgar Allan Poe`, color = abs(`Edgar Allan Poe` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "lightslategrey", high = "slateblue3") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Edgar Allan Poe", x = NULL)

# Vamos ver as correlações!
cor.test(data=tidy_gather[tidy_gather$author=="H.P. Lovecraft",], ~ proportion + `Edgar Allan Poe`)
cor.test(data=tidy_gather[tidy_gather$author=="Isaac Asimov",],~proportion+`Edgar Allan Poe`)
