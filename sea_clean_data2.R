# SEA - Estratificação e Desigualdade
# Final Work
# Neylson Crepalde

#library(readr)
#dados <- read_csv("/home/neylson/Documentos/Neylson Crepalde/Doutorado/SEA Desigualdade/Trabalho Final/dados_rede_teste.csv")

rede2 <- dados[,c(3,23)]
names(rede2) <- c("nome","indicacao")
#View(rede2)


####################################################################
# Limpando os dados
rede2$indicacao[rede2$indicacao=="nenhum"] = "none"
rede2$indicacao[rede2$indicacao=="Não se aplica"] = "none"
rede2$indicacao[rede2$indicacao=="Não houve"] = "none"
rede2$indicacao[rede2$indicacao=="Ninguém"] = "none"
rede2$indicacao[rede2$indicacao=="nenhum destes"] = "none"
rede2$indicacao[rede2$indicacao=="Nenhum"] = "none"
rede2$indicacao[rede2$indicacao=="Nennhum"] = "none"
rede2$indicacao[rede2$indicacao=="nunca escrevi ou publiquei artigo com nenhum dos colegas acima"] = "none"
rede2$indicacao[rede2$indicacao=="Nenhum desses"] = "none"
rede2$indicacao[rede2$indicacao=="Nenhum destes"] = "none"
rede2$indicacao[rede2$indicacao=="NENHUM"] = "none"
rede2$indicacao[rede2$indicacao=="sozinha"] = "none"
rede2$indicacao[rede2$indicacao=="x"] = "none"
rede2$indicacao[rede2$indicacao=="Nao publiquei com os colegas"] = "none"
rede2$indicacao[rede2$indicacao=="."] = "none"
rede2$indicacao[rede2$indicacao=="0"] = "none"
rede2$indicacao[rede2$indicacao=="Nunca pedi revisão"] = "none"
############################################################################

#####################################################################
# Limpando acentos e colocando letras minúsculas
rede2 <- sapply(rede2[,c(1,2)], tolower)
rede2 <- gsub("ã","a", rede2[,c(1,2)])
rede2 <- gsub("á","a", rede2[,c(1,2)])
rede2 <- gsub("é","e", rede2[,c(1,2)])
rede2 <- gsub("í","i", rede2[,c(1,2)])
rede2 <- gsub("ó","o", rede2[,c(1,2)])
rede2 <- gsub("ú","u", rede2[,c(1,2)])
###################################################################

###################################################################
# Separando os nomes e montando edgelist com o comando unnest()
library(tidyr)
rede2 <- as.data.frame(rede2, stringsAsFactors = F)

revisao <- rede2 %>% unnest(indicacao=strsplit(indicacao,", "))
#View(revisao)
##################################################################

###################################################################
# Preparando a rede
library(igraph)
revisao <- as.matrix(revisao)
revisao <- graph_from_edgelist(revisao, directed = T)
revisao <- delete_vertices(revisao, "none")
revisao <- simplify(revisao)

#plot(revisao, vertex.label.cex=0.7, vertex.size=4, edge.arrow.size=0.3)
#plot(revisao, vertex.label=NA, vertex.size=8, edge.arrow.size=0.3, 
#     edge.color="black",main="Review", xlab="Density = 0.019")
