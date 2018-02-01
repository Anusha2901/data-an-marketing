


rm(list=ls())

try(require(dplyr) || install.packages("dplyr"))
library(dplyr)

require(tidytext) || install.packages("tidytext")
library(tidytext)

try(require(tidyr) || install.packages("tidyr"))
library(tidyr)

try(require(stringr) || install.packages("stringr"))
library(stringr)

try(require(ggplot2) || install.packages("ggplot2"))
library(ggplot2)

try(require(tm) || install.packages("tm"))
library(tm)

try(require(wordcloud) || install.packages("wordcloud"))
library(wordcloud)

require(tibble)


corpus_cleaner = function(corpus,user_stopwords){

text = corpus
text  =  gsub("<.*?>", " ", text)              # regex for removing HTML tags
text = tolower(text)
text = stringr::str_replace_all(text,"[^a-zA-Z\\s]", " ")
text = stringr::str_replace_all(text,"[\\s]+", " ")

textdf = data_frame(seq(1,length(text),by=1),text)
colnames(textdf) = c("docind","text")
textdf$text = as.character(textdf$text);head(textdf)

data(stop_words)
user_stop_words = data.frame(word=user_stopwords,lexicon="USER")
total_stop_words = unique(rbind(stop_words,user_stop_words))

x = textdf %>% 
  group_by(docind) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(total_stop_words) %>% 
  count(word, sort = TRUE) ;x

return(x)
  
}

dtm_corpus = function(corpus,weighting_type){
  
  if(weighting_type=="TF"){
    x = cast_dtm(data=corpus,document=docind,term=word,value=n,weighting = tm::weightTf)
  } 
  if(weighting_type=="TFIDF"){
  x=cast_dtm(data=corpus,document=docind,term=word,value=n,weighting = tm::weightTfIdf)
  }
  
  return(x)
  
}

dtm_plots = function(dtm_output){

  dtm = dtm_output
  
build_wordcloud = function(dtm, 
                            max.words1, # max no. of words to accommodate
                            min.freq,   # min.freq of words to consider
                            title1){        # write within double quotes
  require(wordcloud)
  if (ncol(dtm) > 20000){   # if dtm is overly large, break into chunks and solve
    
    tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
    a = rep(tst,99)
    b = cumsum(a);rm(a)
    b = c(0,b,ncol(dtm))
    
    ss.col = c(NULL)
    for (i in 1:(length(b)-1)) {
      tempdtm = dtm[,(b[i]+1):(b[i+1])]
      s = colSums(as.matrix(tempdtm))
      ss.col = c(ss.col,s)
      print(i)      } # i loop ends
    
    tsum = ss.col
    
  } else { tsum = apply(dtm, 2, sum) }
  
  tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
  # head(tsum)
  # tail(tsum)
  
  # windows()  # New plot window
  wordcloud(names(tsum), tsum,     # words, their freqs 
            scale = c(3.5, 0.5),     # range of word sizes
            min.freq,                     # min.freq of words to consider
            max.words = max.words1,       # max #words
            colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
  title(sub = title1)     # title for the wordcloud display
  
}

build_wordcloud(dtm,100,2,"WordClooud")


dtm1 = as.matrix(dtm)   # need it as a regular matrix for matrix ops like %*% to apply
adj.mat = t(dtm1) %*% dtm1    # making a square symmatric term-term matrix 
diag(adj.mat) = 0     # no self-references. So diag is 0.
a1 = order(apply(adj.mat, 2, sum), decreasing = T)   # order cols by descending colSum
adj.mat = as.matrix(adj.mat[a1[1:50], a1[1:50]])   # taking the top 50 rows and cols only

distill.cog = function(mat1, # input TCM ADJ MAT
                       title, # title for the graph
                       s,    # no. of central nodes
                       k1){  # max no. of connections  
  library(igraph)
  a = colSums(mat1) # collect colsums into a vector obj a
  b = order(-a)     # nice syntax for ordering vector in decr order  
  
  mat2 = mat1[b, b]     # order both rows and columns along vector b
  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##
  
  wc = NULL
  
  for (i1 in 1:s){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
    mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc,word)
  } # i1 loop ends
  
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  graph = simplify(graph) 
  V(graph)$color[1:s] = "green"
  V(graph)$color[(s+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singletons?
  
  plot(graph, 
       layout = layout.kamada.kawai, 
       main = title)
  
}

distill.cog(adj.mat, 'Distilled COG',  5,  5)

bar_charts = function(dtm){
  
  x=(tidy(dtm) %>% ungroup(document) %>% count(term,sort=TRUE))[1:20,] %>%
    mutate(term=reorder(term, n)) %>%
  ggplot(aes(term,n)) +
    geom_bar(stat = "identity") +
    xlab(NULL) +
    coord_flip()
  
  return(x)
}

bar_charts(dtm)

}


senti_an_bing = function(x){
  
  bing = get_sentiments("bing") 
  senti.bing = x %>%
    mutate(linenumber = row_number()) %>%   # build line num variable
    ungroup() %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("bing")) %>%
    count(sentiment, index = linenumber %/% 1, sort = FALSE) %>%
    mutate(method = "bing")
  
  bing_df = data.frame(senti.bing %>% spread(sentiment, n, fill = 0))
  
  bing_pol = bing_df %>% 
    mutate(polarity = (positive - negative)) %>%   #create variable polarity = pos - neg
    arrange(desc(polarity), index)
  
  return(bing_pol)
  
}

senti_an_afinn = function(x){
  AFINN = get_sentiments("afinn")
  
  senti.afinn = x %>%
    mutate(linenumber = row_number()) %>%
    ungroup() %>%
    unnest_tokens(word, text) %>%
    inner_join(AFINN) %>%    # returns only intersection of wordlists and all columns
    group_by(index = linenumber %/% 1) %>% 
    summarise(sentiment = sum(score)) %>% 
    mutate(method = "afinn")
  
  return(senti.afinn)
}

senti_an_nrc = function(x){
  
nrc = get_sentiments("nrc")
  
senti.nrc = x %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment, index = linenumber %/% 1, sort = FALSE) %>%  # %/% gives quotient
  mutate(method = "nrc")

a = data.frame(senti.nrc %>% spread(sentiment, n, fill = 0))

return(a)

}

top_emotion_words = function(x,emotion){
  
nrc = get_sentiments("nrc")

output = x %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc) %>%
  filter(sentiment == emotion) %>%
  count(word, sort = TRUE)

return(output)

}

