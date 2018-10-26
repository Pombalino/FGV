library(tidytext)
library(tidyr)
library(magrittr)
library(dplyr)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(wordcloud)

austen_bybook <- austen_books() %>% group_by(book)
head(austen_bybook)
austen_lines <- mutate(austen_bybook, linenumber = row_number())
austen_chapters <- mutate(austen_lines, chapter = cumsum(str_detect(text,
                                                                    regex("^chapter [\\divxlc]", 
                                                                          ignore_case = TRUE))))

austen_ungrouped <- ungroup(austen_chapters)
tidy_books <- unnest_tokens(austen_ungrouped,word,text)

data(stop_words)
tidy_books <- anti_join(tidy_books,stop_words)

tidy_sort <- count(tidy_books, word, sort=TRUE)

filter(tidy_sort,n>540) -> tidy_filter
mutate(tidy_filter,word=reorder(word,n)) -> tidy_mutate

ggplot(tidy_mutate,aes(word,n))+geom_col()+xlab(NULL)+coord_flip()->tidy_ggplot
print(tidy_ggplot)

wordcloud(tidy_sort$word,tidy_sort$n,
          max.words = 75,
          rot.per = FALSE,colors= c("#973232", "#1E5B58", "#6A8D2F", "#287928"))

library(gutenbergr)

hgwells <- gutenberg_download(c(35,36,5230,159))
tidy_wells <- hgwells %>% unnest_tokens(word,text) %>% anti_join(stop_words)
wells_sort <- count(tidy_wells, word, sort=TRUE)
wells_filter<- filter(wells_sort,n>150)
mutate(wells_filter, word=reorder(word,n)) -> wells_mutate

ggplot(wells_mutate,aes(word,n))+geom_col()+xlab(NULL)+coord_flip()->tidy_ggplot
print(tidy_ggplot)

wordcloud(wells_mutate$word,wells_mutate$n,
          max.words = 75,
          rot.per = FALSE,colors= c("#AB329C", "#195B5B", "#6D8D2F", "#287928"))

bronte <- gutenberg_download(c(1260,768,969,9182,767))
tidy_bronte <- bronte %>%unnest_tokens(word,text)%>%
  anti_join(stop_words)

bind_rows(mutate(tidy_bronte, author="Brontë Sisters"), mutate(tidy_wells,author='H.G.Wells'),mutate(tidy_books,author='Jane Austen')) -> tidy_bind
tidy_bind <- mutate(tidy_bind, word=str_extract(word,"[a-z']+"))

tidy_count <- count(tidy_bind,author,word)
group_by(tidy_count,author)->tidy_group

mutate(tidy_group,proportion = n/sum(n))->tidy_mutate
select(tidy_mutate,-n)->tidy_selected

tidy_spread <- spread(tidy_selected,author,proportion)
tidy_gather<-gather(tidy_spread,author,proportion,'Brontë Sisters':'H.G.Wells')

library(scales)

ggplot(tidy_gather, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "lightslategrey", high = "slateblue3") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)


cor.test(data=tidy_gather[tidy_gather$author=="Brontë Sisters",], ~ proportion + `Jane Austen`)
cor.test(data=tidy_gather[tidy_gather$author=="H.G.Wells",],~proportion+`Jane Austen`)

# Aula adaptada de: https://www.tidytextmining.com/tidytext.html