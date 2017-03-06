# SEA - Estratificação e Desigualdade
# Final Work
# Neylson Crepalde

#library(readr)
#dados <- read_csv("/home/neylson/Documentos/Neylson Crepalde/Doutorado/SEA Desigualdade/Trabalho Final/dados_rede_teste.csv")

rede6 <- dados[,c(3,27)]
names(rede6) <- c("nome","indicacao")
#View(rede6)

####################################################################
# Limpando os dados
rede6$indicacao[rede6$indicacao=="nenhum"] = "none"
rede6$indicacao[rede6$indicacao=="Não se aplica"] = "none"
rede6$indicacao[rede6$indicacao=="Não houve"] = "none"
rede6$indicacao[rede6$indicacao=="Ninguém"] = "none"
rede6$indicacao[rede6$indicacao=="nenhum destes"] = "none"
rede6$indicacao[rede6$indicacao=="Nenhum"] = "none"
rede6$indicacao[rede6$indicacao=="Nennhum"] = "none"
rede6$indicacao[rede6$indicacao=="nunca escrevi ou publiquei artigo com nenhum dos colegas acima"] = "none"
rede6$indicacao[rede6$indicacao=="Nenhum desses"] = "none"
rede6$indicacao[rede6$indicacao=="Nenhum destes"] = "none"
rede6$indicacao[rede6$indicacao=="NENHUM"] = "none"
rede6$indicacao[rede6$indicacao=="sozinha"] = "none"
rede6$indicacao[rede6$indicacao=="x"] = "none"
rede6$indicacao[rede6$indicacao=="Nao publiquei com os colegas"] = "none"
rede6$indicacao[rede6$indicacao=="."] = "none"
rede6$indicacao[rede6$indicacao=="0"] = "none"
rede6$indicacao[rede6$indicacao=="Nunca pedi revisão"] = "none"
rede6$indicacao[rede6$indicacao=="Não sei..."] = "none"
rede6$indicacao[rede6$indicacao=="não tenho como opinar nesse aspecto"] = "none"
rede6$indicacao[rede6$indicacao=="Depende da área. Mas não tenho certeza."] = "none"
rede6$indicacao[rede6$indicacao=="Todxs"] = "none"
############################################################################

#####################################################################
# Limpando acentos e colocando letras minúsculas
rede6 <- sapply(rede6[,c(1,2)], tolower)
rede6 <- gsub("ã","a", rede6[,c(1,2)])
rede6 <- gsub("á","a", rede6[,c(1,2)])
rede6 <- gsub("é","e", rede6[,c(1,2)])
rede6 <- gsub("í","i", rede6[,c(1,2)])
rede6 <- gsub("ó","o", rede6[,c(1,2)])
rede6 <- gsub("ú","u", rede6[,c(1,2)])
###################################################################

###################################################################
# Separando os nomes e montando edgelist com o comando unnest()
library(tidyr)
rede6 <- as.data.frame(rede6, stringsAsFactors = F)

trabalho <- rede6 %>% unnest(indicacao=strsplit(indicacao,", "))
#View(trabalho)
##################################################################

###################################################################
# Preparando a rede
library(igraph)
trabalho <- as.matrix(trabalho)
trabalho <- graph_from_edgelist(trabalho, directed = T)
trabalho <- delete_vertices(trabalho, "none")
trabalho <- simplify(trabalho)

#plot(trabalho, vertex.label.cex=0.7, vertex.size=4, edge.arrow.size=0.3)
#plot(trabalho, vertex.label=NA, vertex.size=8, edge.arrow.size=0.3,
#     edge.color="black",main="Professional Indication", xlab="Density = 0.064")
