# SEA - Estratificação e Desigualdade
# Final Work
# Neylson Crepalde

#library(readr)
#dados <- read_csv("/home/neylson/Documentos/Neylson Crepalde/Doutorado/SEA Desigualdade/Trabalho Final/dados_rede_teste.csv")

rede4 <- dados[,c(3,25)]
names(rede4) <- c("nome","indicacao")
#View(rede4)

####################################################################
# Limpando os dados
rede4$indicacao[rede4$indicacao=="nenhum"] = "none"
rede4$indicacao[rede4$indicacao=="Não se aplica"] = "none"
rede4$indicacao[rede4$indicacao=="Não houve"] = "none"
rede4$indicacao[rede4$indicacao=="Ninguém"] = "none"
rede4$indicacao[rede4$indicacao=="nenhum destes"] = "none"
rede4$indicacao[rede4$indicacao=="Nenhum"] = "none"
rede4$indicacao[rede4$indicacao=="Nennhum"] = "none"
rede4$indicacao[rede4$indicacao=="nunca escrevi ou publiquei artigo com nenhum dos colegas acima"] = "none"
rede4$indicacao[rede4$indicacao=="Nenhum desses"] = "none"
rede4$indicacao[rede4$indicacao=="Nenhum destes"] = "none"
rede4$indicacao[rede4$indicacao=="NENHUM"] = "none"
rede4$indicacao[rede4$indicacao=="sozinha"] = "none"
rede4$indicacao[rede4$indicacao=="x"] = "none"
rede4$indicacao[rede4$indicacao=="Nao publiquei com os colegas"] = "none"
rede4$indicacao[rede4$indicacao=="."] = "none"
rede4$indicacao[rede4$indicacao=="0"] = "none"
rede4$indicacao[rede4$indicacao=="Nunca pedi revisão"] = "none"
rede4$indicacao[rede4$indicacao=="Não sei..."] = "none"
rede4$indicacao[rede4$indicacao=="não tenho como opinar nesse aspecto"] = "none"
rede4$indicacao[rede4$indicacao=="Depende da área. Mas não tenho certeza."] = "none"
############################################################################

#####################################################################
# Limpando acentos e colocando letras minúsculas
rede4 <- sapply(rede4[,c(1,2)], tolower)
rede4 <- gsub("ã","a", rede4[,c(1,2)])
rede4 <- gsub("á","a", rede4[,c(1,2)])
rede4 <- gsub("é","e", rede4[,c(1,2)])
rede4 <- gsub("í","i", rede4[,c(1,2)])
rede4 <- gsub("ó","o", rede4[,c(1,2)])
rede4 <- gsub("ú","u", rede4[,c(1,2)])
###################################################################

###################################################################
# Separando os nomes e montando edgelist com o comando unnest()
library(tidyr)
rede4 <- as.data.frame(rede4, stringsAsFactors = F)

metodologica <- rede4 %>% unnest(indicacao=strsplit(indicacao,", "))
#View(metodologica)
##################################################################

###################################################################
# Preparando a rede
library(igraph)
metodologica <- as.matrix(metodologica)
metodologica <- graph_from_edgelist(metodologica, directed = T)
metodologica <- delete_vertices(metodologica, "none")
metodologica <- simplify(metodologica)

#plot(metodologica, vertex.label.cex=0.7, vertex.size=4, edge.arrow.size=0.3)
#plot(metodologica, vertex.label=NA, vertex.size=8, edge.arrow.size=0.3,
#     edge.color="black",main="Methodological Prestige", xlab="Density = 0.041")
