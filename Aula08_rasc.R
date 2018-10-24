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

wordcloud(tidy_mutate$word,tidy_mutate$n,
          max.words = 100,
          rot.per = FALSE,colors= c("#973232", "#1E5B58", "#6A8D2F", "#287928"))

library(gutenbergr)

hgwells <- gutenberg_download(c(35,36,5230,159))
tidy_wells <- hgwells %>% unnest_tokens(word,text) %>% anti_join(stop_words)
wells_sort <- count(tidy_wells, word, sort=TRUE)
mutate(wells_sort, author="H.G.Wells") -> wells_mutate

ggplot(wells_mutate,aes(word,n))+geom_col()+xlab(NULL)+coord_flip()->tidy_ggplot
print(tidy_ggplot)

wordcloud(wells_mutate$word,wells_mutate$n,
          max.words = 100,
          rot.per = FALSE,colors= c("#AB329C", "#195B5B", "#6D8D2F", "#287928"))

bronte <- gutenberg_download(c(1260,768,969,9182,767))
bronte_mutate <- bronte %>%unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%count(word,sort=TRUE)%>%
  mutate(word=reorder(word,n),author="Brontë Sisters")

jane_mutate <- mutate(tidy_books,author="Jane Austen")

bind_rows(bronte_mutate,wells_mutate,jane_mutate) -> tidy_bind
tidy_bind <- mutate(tidy_bind, word=str_extract(word,"[a-z']+"))

tidy_count <- count(tidy_bind,author,word)
group_by(tidy_count,author)->tidy_group

mutate(tidy_group,proportion = nn/sum(nn))->tidy_mutate
select(tidy_mutate,-nn)->tidy_selected

tidy_spread <- spread(tidy_selected,author,proportion)
tidy_gather<-gather(tidy_spread,author,proportion,'Brontë Sisters':'H.G.Wells')

library(scales)
# aqui que ta cagado
ggplot(tidy_gather,aes(x=proportion,y='Jane Austen',color=abs('Jane Austen'-proportion)))->tidy_ggplot
tidy_ggplot+geom_abline(color="gray40",lty=2)->tidy_line
print(tidy_line)
print(tidy_scale) #que reflete aqui
tidy_line+geom_jitter(alpha=0.1,size=2.5,width=0.3,height=0.3)->tidy_jitter
tidy_jitter+geom_text(aes(label=word),check_overlap = TRUE, vjust=1.5)-> tidy_leg
tidy_leg+scale_x_log10(labels=percent_format())+scale_y_log10(labels=percent_format())->tidy_scale

print(tidy_scale)

tidy_scale+scale_color_gradient(limits=c(0,0.001),low="darkslategray4",high="gray75")->gradient
gradient + facet_wrap(~author,ncol=2)->wrap
wrap + theme(legend.position="none")+labs(y="Jane Austen",x=NULL)->tidy_legends

print(tidy_legends)

cor.test(data=tidy_gather[tidy_gather$author=="Brontë Sisters",]~proportion+'Jane Austen')$estimate
cor.test(data=tidy_gather[tidy_gather$author=="H.G.Wells",]~proportion+'Jane Austen')$estimate
