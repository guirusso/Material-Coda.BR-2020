### [CEPESP 2020] - Coda 2020
# Como baixar dados dos candidatos 

# O diretório aqui, por exemplo:
# setwd("/Users/guilhermerusso/Downloads")

# Dados- TSE 2020
download.file("http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2020.zip",
              destfile = "consulta_cand_2020.zip")
unzip("consulta_cand_2020.zip", 
      exdir = "consulta_cand_2020")

list.files(path="./consulta_cand_2020/")
df20<-read.csv("consulta_cand_2020/consulta_cand_2020_BRASIL.csv", sep=";",
               encoding="latin1")


