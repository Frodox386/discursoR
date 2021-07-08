install.packages("tidyr")
install.packages("dplyr")
install.packages("tidytext")
install.packages("ggplot2") 
install.packages("wordcloud")
install.packages("RColorBrewer") 
library(tm)
library(NLP)
library(tidyr)
library(dplyr)
library(tidytext)
library(tibble)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

install.packages("lexiconPT")
library(lexiconPT)
install.packages(widyr)



getwd()
setwd("/Users/pabloalmada/Desktop/GovernoBolsonaro/Discursos")
arquivotxt <- c("/Users/pabloalmada/Desktop/GovernoBolsonaro/Discursos")
textos <- VCorpus(DirSource(arquivotxt, encoding = "UTF-8"),readerControl = list(language = "lat"))

##### Ajustes e transformações ######
#Transformando de Corpus para Tibble
discurso_presidente <-tidy(textos)

#Retirando as palavras stopwords
stopwords_pt <- c(stopwords("pt"), "é", "sr", "sra","todos", "presidente", "porta", "voz", "pais")
stopwords_pt_df <- data.frame(word = stopwords_pt)
discurso_presidente <- discurso_presidente %>%
  unnest_tokens(word, text) %>%
  anti_join(stopwords_pt_df, by = "word")

# Contando as palavras mais comuns nos discursos de todos os presidentes
discurso_presidente %>%
  count(word, sort = TRUE)

# Todas as palavras mais ditas, gravadas em um dataframe

todas_as_palavras <- discurso_presidente %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) 

# Ordenando todas as palavras mais ditas, gravadas em um dataframe
todas_as_palavras <- todas_as_palavras %>%
  arrange(desc(todas_as_palavras))

#Wordcloud
set.seed(1234)
wordcloud(words = todas_as_palavras$word, freq = todas_as_palavras$n, min.freq = 2,max.words=50, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))


#Ordenando as palavras pelos discursos
todas_as_palavras <- discurso_presidente %>%
  count(id, word, sort = TRUE) %>%
  ungroup()


#Correlacao entre os textos
library(widyr)
grupo_correlacao <- todas_as_palavras %>%
  pairwise_cor(id, word, n, sort = TRUE)


#Gráfico com as correlação. correlação >=0.8 ou <=-0.8
library(ggraph)
library(igraph)
grupo_correlacao %>%
  filter(correlation > .004) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 9, color = "Red") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme(legend.position = 'bottom') +
  theme_void()



#####################################################################
#Avaliando as palavras mais especificas do discurso 25.03.19
#####################################################################
textostrans <- tm_map(textostrans, content_transformer(tolower))
textostrans <- tm_map(textostrans, removeWords, stopwords("pt"))
textostrans <- tm_map(textostrans, removePunctuation)
textostrans <- tm_map(textostrans, stripWhitespace)

# utilizando o arquivo de stopwords
stopwords_pt <- c(stopwords("pt"), "para", "que", "é", "isso","senhor")
textostrans <- tm_map(textostrans, removeWords, stopwords_pt)
inspect(textostrans[[3]])

#Conversão de objeto Corpus em Tibble
matriztf <- DocumentTermMatrix(textostrans)
reportagem_palavras <- tidy(matriztf)
reportagem_palavras

########################################################################
#TOTAL DISCURSOS

discursos <- reportagem_palavras %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

discursos<- filter(discursos, tf > 0.02)

ggplot(discursos , aes(x = term, y = tf)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "Palavras mencionadas nos Discursos",
       x = "Palavras",
       y = "Mais faladas")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

########################################################################
#1.discurso 25.03.19 

palavras_25.03.19 <- reportagem_palavras %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

palavras_25.03.19<- filter(palavras_25.03.19,document == "25.03.19_Declaração à imprensa Porta-Voz.txt"& tf > 0.009)
palavras_25.03.19

ggplot(palavras_25.03.19, aes(x = term, y = tf)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "Declaração Porta-Voz Gen. Rego Barros(25.03.19)",
       subtitle = "comemorações devidas",
       x = "Palavras",
       y = "Mais faladas")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

########################################################################
#2. discurso 24.09.19 

palavras_24.09.19 <- reportagem_palavras %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

palavras_24.09.19<- filter(palavras_24.09.19,document == "24.09.19_Jair Bolsonaro abertura 74AGNU.txt" & tf > 0.00598)
palavras_24.09.19

ggplot(palavras_24.09.19, aes(x = term, y = tf)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "Jair Bolsonaro - Abertura 74AGNU(24.09.19)",
       subtitle = "um novo Brasil",
       x = "Palavras",
       y = "Mais faladas")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

########################################################################
#3.discurso 31.10.19EB

palavras_31.10.19EB <- reportagem_palavras %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

palavras_31.10.19EB<- filter(palavras_31.10.19EB,document == "31.10.19_Eduardo Bolsonaro Leda Nagle.txt" & tf > 0.009)
palavras_31.10.19EB

ggplot(palavras_31.10.19EB, aes(x = term, y = tf)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "Eduardo Bolsonaro canal Leila Nagle (31.10.19)",
       subtitle = "novo AI-5",
       x = "Palavras",
       y = "Mais faladas")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
########################################################################
#4.discurso 14.04.20

palavras_14.04.20 <- reportagem_palavras %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

palavras_14.04.20<- filter(palavras_14.04.20,document == "19.04.20_Jair Bolsonaro manifestação 19.abril20.txt" & tf > 0.012)
palavras_14.04.20

ggplot(palavras_14.04.20, aes(x = term, y = tf)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "Jair Bolsonaro Manifestação contra STF(14.04.20)",
       subtitle = "não queremos negociar nada",
       x = "Palavras",
       y = "Mais faladas")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#XXXXXXXXXXXXXXXXXXXX
#ANALISE DE SENTIMENTO
install.packages(textdata)
library(textdata)
library(tidytext)

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


####################################
#Leitura dos arquivos de opiniao

library(tm)
getwd()
setwd("~/Desktop/GovernoBolsonaro/en")
arquivotxt <- c("~/Desktop/GovernoBolsonaro/en")
textos <- VCorpus(DirSource(arquivotxt, encoding = "UTF-8"),readerControl = list(language = "lat"))

#instalando pacotes
#instalando pacotes
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("tidytext")
#install.packages("ggplot2") 
#install.packages("wordcloud") 
#install.packages("RColorBrewer") 
library(tidyr)
library(dplyr)
library(tidytext)
library(tibble)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

##### Ajustes e transformações ######

#Transformando de Corpus para Tibble
opiniao <-tidy(textos)

#Coloca no formato apropriado de token
conversao_opiniao <- opiniao %>%
  unnest_tokens(word, text)

#agrupando palavras
word_grupo <- conversao_opiniao %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

#Analisando a frequência das palavras por texto
tf_idf <- word_grupo %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))

#Gráfico da frequência de palavras
tf_idf %>%
  group_by(id) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ id, scales = "free") +
  ylab("tf-idf") +
  coord_flip()

#Preparacao da análise de sentimento, calculando valores.
#Estamos usando o léxico Afinn, pois oferece os valores por sentimento
#Outros Léxico "bing", "afinn", "loughran", "nrc"

grupo_sentimento <- word_grupo %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(value * n) / sum(n))


#gráfico de sentimento, opiniao da Folha tem menos negatividade que o do correio Braziliense
grupo_sentimento %>%
  mutate(recodifica = reorder(id, value)) %>%
  ggplot(aes(recodifica, value, fill = value > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("Valor da Análise sentimento")+
  xlab("Textos")


