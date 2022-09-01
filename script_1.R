library(tidyverse)

macaco <- read_csv("dados_macacos.csv") 

macaco %>% glimpse

macaco$estratégia
### Ponto dos informantes

a <- ggplot(macaco, aes(x = Ponto))+
  geom_bar(show.legend = F)+
  scale_x_discrete(breaks = c(2,5,6,7,8,15,16) , limits=c(1:16))+
  ggtitle("Ponto de moradia dos informantes")+
  geom_text(aes(x = 2, y = 1.5,
                label = dim(macaco %>%
                              select(Ponto) %>% 
                              filter(Ponto==2))[1] %>% paste0(), color = "blue", size = 30), show.legend = F)+
  geom_text(aes(x = 5, y = 7,
                label = dim(macaco %>%
                              select(Ponto) %>% 
                              filter(Ponto==5))[1] %>% paste0(), color = "blue", size = 30), show.legend = F)+
  geom_text(aes(x = 6, y = 10,
                label = dim(macaco %>%
                              select(Ponto) %>% 
                              filter(Ponto==6))[1] %>% paste0(), color = "blue", size = 30), show.legend = F)+
  geom_text(aes(x = 7, y = 13.4,
                label = dim(macaco %>%
                              select(Ponto) %>% 
                              filter(Ponto==7))[1] %>% paste0(), color = "blue", size = 30), show.legend = F)+
  geom_text(aes(x = 8, y = 7,
                label = dim(macaco %>%
                              select(Ponto) %>% 
                              filter(Ponto==8))[1] %>% paste0(), color = "blue", size = 30), show.legend = F)+
  geom_text(aes(x = 15, y = 2.5,
                label = dim(macaco %>%
                              select(Ponto) %>% 
                              filter(Ponto==15))[1] %>% paste0(), color = "blue", size = 30), show.legend = F)+
  geom_text(aes(x = 16, y = 2.5,
                label = dim(macaco %>%
                              select(Ponto) %>% 
                              filter(Ponto==16))[1] %>% paste0(), color = "blue", size = 30), show.legend = F)+
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 15))+
  xlab("Ponto de moradia")+
  ylab("Informantes")


## Tempo de moradia na comunidade

macaco %>%
  count(tempo_residencia) %>%
  filter(!is.na(tempo_residencia)) %>% 
  slice_max(order_by = n, n = 10) %>% 
  mutate(tempo_residencia = forcats::fct_reorder(tempo_residencia, n)) %>% 
  ggplot() +
  geom_col(aes(x = tempo_residencia, y = n, fill = tempo_residencia), show.legend = F) +
  geom_label(aes(x = tempo_residencia, y = n/2, label = n)) +
  coord_flip()+
  ggtitle("Tempo de residência dos informantes")+
  xlab("Tempo de residência (anos)")+
  theme(axis.text = element_text(size = 11),
        title = element_text(size = 15))

png("/home/marcio/extra/Fig_1.png", width = 1600, height = 950, res=300)
a
dev.off()

png("/home/marcio/extra/Fig_2.png", width = 1600, height = 950, res=300)
b
dev.off()

## 100% dos informantes já viram o macaco-prego

## Avistamento

c <- macaco %>%
  count(avistamento) %>%
  filter(!is.na(avistamento)) %>% 
  slice_max(order_by = n, n = 16) %>% 
  mutate(avistamento = forcats::fct_reorder(avistamento, n)) %>% 
  ggplot() +
  geom_col(aes(x = avistamento, y = n, fill = avistamento), show.legend = F) +
  geom_label(aes(x = avistamento, y = n/2, label = n)) +
  coord_flip()+
  ggtitle("Em qual estação eles são mais avistados?")+
  xlab("Estação")+
  theme(axis.text = element_text(size = 11),
        title = element_text(size = 15))

png("/home/marcio/extra/Fig_3.png", width = 1900, height = 1200, res=300)
c
dev.off()


d <- ggplot(macaco) +
  geom_bar(
    aes(y = avistamento, fill = tempo_residencia),
    show.legend = T
  )+
  ggtitle("Em qual estação eles são mais avistados?", subtitle = "Por tempo de residência dos informantes")+
  ylab("Estação")+
  theme(axis.text = element_text(size = 11),
        title = element_text(size = 15))

png("/home/marcio/extra/Fig_4.png", width = 1900, height = 1200, res=300)
d
dev.off()


e <- ggplot(macaco %>% filter(tempo_residencia != "menos_1" & tempo_residencia != "1_5") ) +
  geom_bar(
    aes(y = avistamento, fill = tempo_residencia),
    show.legend = T
  )+
  ggtitle("Em qual estação eles são mais avistados?", subtitle = "Só quem mora há mais tempo")+
  ylab("Estação")+
  theme(axis.text = element_text(size = 11),
        title = element_text(size = 15))

png("/home/marcio/extra/Fig_5.png", width = 1900, height = 1200, res=300)
e
dev.off()

## Frequência de aparecimento

f <- macaco %>%
  count(frequencia_aparecimento) %>%
  filter(!is.na(frequencia_aparecimento)) %>% 
  slice_max(order_by = n, n = 16) %>% 
  mutate(frequencia_aparecimento = forcats::fct_reorder(frequencia_aparecimento, n)) %>% 
  ggplot() +
  geom_col(aes(x = frequencia_aparecimento, y = n, fill = frequencia_aparecimento), show.legend = F) +
  geom_label(aes(x = frequencia_aparecimento, y = n/2, label = n)) +
  coord_flip()+
  ggtitle("Frequência de visita dos macacos-prego às residências")+
  ylab("n")+
  xlab("Frequência de aparecimento")+
  theme(axis.text = element_text(size = 11),
        title = element_text(size = 15))

png("/home/marcio/extra/Fig_6.png", width = 1900, height = 1200, res=300)
f
dev.off()

## Tamanho dos grupos

g <- macaco %>%
  count(grupos) %>%
  filter(!is.na(grupos)) %>% 
  slice_max(order_by = n, n = 16) %>% 
  mutate(grupos = forcats::fct_reorder(grupos, n)) %>% 
  ggplot() +
  geom_col(aes(x = grupos, y = n, fill =grupos), show.legend = F) +
  geom_label(aes(x = grupos, y = n/2, label = n)) +
  coord_flip()+
  ggtitle("Tamanho dos grupos", subtitle = "De acordo com a estimativa dos informantes")+
  xlab("Grupos (número de indivíduos)")+
  theme(axis.text = element_text(size = 11),
        title = element_text(size = 15))

png("/home/marcio/extra/Fig_7.png", width = 1900, height = 1200, res=300)
g
dev.off()


## Relação das pessoas

h <- macaco %>%
  count(relação) %>%
  filter(!is.na(relação)) %>% 
  slice_max(order_by = n, n = 16) %>% 
  mutate(relação = forcats::fct_reorder(relação, n)) %>% 
  ggplot() +
  geom_col(aes(x = relação, y = n, fill = relação), show.legend = F) +
  geom_label(aes(x = relação, y = n/2, label = n)) +
  coord_flip()+
  ggtitle("Relação das pessoas com os macacos-prego", subtitle = "Comportamentos")+
  xlab("Comportamentos")+
  theme(axis.text = element_text(size = 11),
        title = element_text(size = 15))

png("/home/marcio/extra/Fig_8.png", width = 2100, height = 1350, res=300)
h
dev.off()

i <- ggplot(macaco %>% remove_missing()) +
  geom_bar(
    aes(y = relação, fill = prejuizo),
    show.legend = T
  )+
  ggtitle("Relação das pessoas com os macacos-prego", subtitle = "Por consequência das invasões")+
  ylab("Comportamentos")+
  theme(axis.text = element_text(size = 11),
        title = element_text(size = 15))

png("/home/marcio/extra/Fig_9.png", width = 2100, height = 1350, res=300)
i
dev.off() 

## Tendência de mudança da população

j <- macaco %>%
  count(tendencia) %>%
  filter(!is.na(tendencia)) %>% 
  slice_max(order_by = n, n = 16) %>% 
  mutate(tendencia = forcats::fct_reorder(tendencia, n)) %>%
  ggplot() +
  geom_col(aes(x = tendencia, y = n, fill=tendencia), show.legend = F) +
  geom_label(aes(x = tendencia, y = n/2, label = n)) +
  coord_flip()+
  ggtitle("Tendência de mudança na população de macacos-prego", subtitle = "De acordo com a percepção dos moradores locais")+
  ylab("n")+
  theme(axis.text = element_text(size = 11),
        title = element_text(size = 15))

png("/home/marcio/extra/Fig_10.png", width = 2300, height = 1400, res=300)
j
dev.off() 


k <- macaco %>% ggplot() +
  geom_bar(
    aes(y = tendencia, fill = tempo_residencia),
    show.legend = T
  )+
  ggtitle("Tendência de mudança na população de macacos-prego", subtitle = "Po tempo de moradia dos informantes")+
  ylab("Tendência")+
  xlab("n")+
  theme(axis.text = element_text(size = 11),
        title = element_text(size = 15))



l <- macaco %>% filter(tempo_residencia != "menos_1" & tempo_residencia != "1_5") %>% ggplot() +
  geom_bar(
    aes(y = tendencia, fill = tempo_residencia),
    show.legend = T
  )+
  ggtitle(" ", subtitle = "Só os moradores mais antigos")+
  ylab("Tendência")+
  xlab("n")+
  theme(axis.text = element_text(size = 11),
        title = element_text(size = 15))

png("/home/marcio/extra/Fig_11.png", width = 2300, height = 1700, res=300)
k/l
dev.off() 


k/l



m <-macaco %>%
  count(entraram_casa) %>%
  filter(!is.na(entraram_casa)) %>% 
  slice_max(order_by = n, n = 16) %>% 
  mutate(entraram_casa = forcats::fct_reorder(entraram_casa, n)) %>%
  ggplot() +
  geom_col(aes(x = entraram_casa, y = n, fill=entraram_casa), show.legend = T) +
  geom_label(aes(x = entraram_casa, y = n/2, label = n)) +
  coord_flip()+
  ggtitle("Os macacos-prego já entraram na sua casa?")+
  ylab("n")+
  xlab("Entraram?")+
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 15))

png("/home/marcio/extra/Fig_12.png", width = 2300, height = 1700, res=300)
m
dev.off() 



library(vegan)
library(tidyverse)
library(tm)
library(wordcloud)
library(patchwork)
library(SnowballC)
library(RColorBrewer)

alim_text <- macaco %>% select(alimentação)
com_text <- macaco %>% select(Comentários)
est_text <- macaco %>% select(estratégia)

i=1

docs1 <- Corpus(VectorSource(alim_text))

class(docs1) # note que o objeto pode ter duas classes!

docs1 %>% str

inspect(docs1)

docs1 <- tm_map(docs1, content_transformer(tolower))

## Remove os números do texto
docs1 <- tm_map(docs1, removeNumbers)

## Remove algumas palavras (veja o help) comuns em diversas línguas, como "dessa" "isso" "e"
docs1 <- tm_map(docs1, removeWords, stopwords("portuguese"))
docs1 <- tm_map(docs1, removeWords, c("prego", "relação", "pode", "raramente", "porém", "macaco", "ali", "vários", "bem", "primeiro", "dia", "pra", "toa", "dali", "ali",
                                      "vem", "meio", "todos", "cada", "contra", "tio",
                                      "pois", "onde", "porque", "tudo", "talvez", "sabe",
                                      "junto", "algum", "dentro", "desde", "sempre",
                                      "faz", "grande", "diz", "tão", "recem", "luiz", "todo", "dum", "sim", "aqui", "casa", "casas", "pregos", "macacos", "que", "quando", "com", "não", "dos", "eles", "para", "estão", "deixar", "coloco", "por", "nas", "das", "estou", "sei", "comeu", "tipo")) 


docs1 <- tm_map(docs1, removePunctuation)


docs1 <- tm_map(docs1, stripWhitespace)

## Explorando mais a fundo:
## Chamam isso de text mining, por isso a abreviatura 'tm', como o nome do pacote.

# Tabela de frequência dos termos
dtm1 <- TermDocumentMatrix(docs1)

m1 <- as.matrix(dtm1)
v1 <- sort(rowSums(m1),decreasing=TRUE)
d1 <- data.frame(palavra = names(v1),freq=v1)
head(d1, 18)

findFreqTerms(dtm1, lowfreq = 3)


png("/home/marcio/extra/Fig_14.png", width = 2300, height = 1700, res=300)

wordcloud(words = d1$palavra, freq = d1$freq, min.freq = 1,
          max.words=250, random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))+
  title(main = "Do que os macacos-prego se alimentam", outer = F, mgp = c(0,0,0))
dev.off()

png("/home/marcio/extra/Fig_15.png", width = 2300, height = 1700, res=300)

barplot(d1[1:25,]$freq, las = 2, names.arg = d1[1:25,]$palavra,
        col ="lightblue", main ="Palavras mais frequentes", ylim = c(0,10),
        ylab = "Frequências das palavras")

dev.off()
