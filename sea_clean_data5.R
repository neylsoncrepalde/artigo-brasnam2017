# SEA - Estratificação e Desigualdade
# Final Work
# Neylson Crepalde

#library(readr)
#dados <- read_csv("/home/neylson/Documentos/Neylson Crepalde/Doutorado/SEA Desigualdade/Trabalho Final/dados_rede_teste.csv")

rede5 <- dados[,c(3,26)]
names(rede5) <- c("nome","indicacao")
#View(rede5)

####################################################################
# Limpando os dados
rede5$indicacao[rede5$indicacao=="nenhum"] = "none"
rede5$indicacao[rede5$indicacao=="Não se aplica"] = "none"
rede5$indicacao[rede5$indicacao=="Não houve"] = "none"
rede5$indicacao[rede5$indicacao=="Ninguém"] = "none"
rede5$indicacao[rede5$indicacao=="nenhum destes"] = "none"
rede5$indicacao[rede5$indicacao=="Nenhum"] = "none"
rede5$indicacao[rede5$indicacao=="Nennhum"] = "none"
rede5$indicacao[rede5$indicacao=="nunca escrevi ou publiquei artigo com nenhum dos colegas acima"] = "none"
rede5$indicacao[rede5$indicacao=="Nenhum desses"] = "none"
rede5$indicacao[rede5$indicacao=="Nenhum destes"] = "none"
rede5$indicacao[rede5$indicacao=="NENHUM"] = "none"
rede5$indicacao[rede5$indicacao=="sozinha"] = "none"
rede5$indicacao[rede5$indicacao=="x"] = "none"
rede5$indicacao[rede5$indicacao=="Nao publiquei com os colegas"] = "none"
rede5$indicacao[rede5$indicacao=="."] = "none"
rede5$indicacao[rede5$indicacao=="0"] = "none"
rede5$indicacao[rede5$indicacao=="Nunca pedi revisão"] = "none"
rede5$indicacao[rede5$indicacao=="Não sei..."] = "none"
rede5$indicacao[rede5$indicacao=="não tenho como opinar nesse aspecto"] = "none"
rede5$indicacao[rede5$indicacao=="Depende da área. Mas não tenho certeza."] = "none"
############################################################################

#####################################################################
# Limpando acentos e colocando letras minúsculas
rede5 <- sapply(rede5[,c(1,2)], tolower)
rede5 <- gsub("ã","a", rede5[,c(1,2)])
rede5 <- gsub("á","a", rede5[,c(1,2)])
rede5 <- gsub("é","e", rede5[,c(1,2)])
rede5 <- gsub("í","i", rede5[,c(1,2)])
rede5 <- gsub("ó","o", rede5[,c(1,2)])
rede5 <- gsub("ú","u", rede5[,c(1,2)])
###################################################################

###################################################################
# Separando os nomes e montando edgelist com o comando unnest()
library(tidyr)
rede5 <- as.data.frame(rede5, stringsAsFactors = F)

amizade <- rede5 %>% unnest(indicacao=strsplit(indicacao,", "))
#View(amizade)
##################################################################

###################################################################
# Preparando a rede
library(igraph)
amizade <- as.matrix(amizade)
amizade <- graph_from_edgelist(amizade, directed = F)
amizade <- delete_vertices(amizade, "none")
amizade <- simplify(amizade)

#plot(amizade, vertex.label.cex=0.7, vertex.size=4, edge.arrow.size=0.3)
#plot(amizade, vertex.label=NA, vertex.size=8, edge.arrow.size=0.3,
#     edge.color="black",main="Friendship", xlab="Density = 0.102")
