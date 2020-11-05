#### O que colocar no começo de um script? ----
### Seu nome
## Data
# Propósito desse script

#### O que eu necessito para acessar (baixar) os dados eleitorais? ----

# Ferramenta de acesso aos dados conhecido. 
# Rotina (passos) para realizar uma tarefa específica.
# Grupos de rotinas ou funções prontos são chamados de PACOTES

# Vamos instalar alguns pacotes como exercício

install.packages("sidrar") # Sistema IBGE de Recuperação Automática 
# https://sidra.ibge.gov.br/home/pnadcm
# https://analisemacro.com.br/economia/dados-macroeconomicos/baixando-dados-do-sidra-com-o-r-o-pacote-sidrar/

install.packages("BETS") # FGV IBRE: Brazilian Economic Time Series 
# https://bibliotecadigital.fgv.br/dspace/bitstream/handle/10438/18172/Brazilian_Economic_Time_Series_BETS_R_package.pdf?sequence=1&isAllowed=y
# https://cran.r-project.org/web/packages/BETS/vignettes/BETS_basic_usage.html

# Instalar pacote cepespR

if (!require("devtools")) install.packages("devtools")
devtools::install_github("Cepesp-Fgv/cepesp-r")

#### Que outras ferramentas eu vou precisar para fazer essa análise? ----
install.packages("dplyr") # Pacote de manipulação de dados
# http://material.curso-r.com/manip/

install.packages("ggplot2") # Pacote de manipulação de dados
# https://rpubs.com/mnunes/ggplot2

library(cepespR)
library(dplyr)

## Estrutura básica de uma função no R

## banco_de_dados <- função(banco_de_dados, parâmetro1 = x, parâmetro2 = y)

## ATENÇÃO: dependendo da função e de sua finalidade, o banco de dados pode
## não ser um parâmetro. No pacote do CEPESP, por exemplo, as funções 
## possuem como objetivo fazer o download de dados eleitorais e não 
## manipular bancos de dados já existentes.

## Para ver todas as funções existentes em um pacote
## NOME_DO_PACOTE::

#### Acessando os dados: Quais dados eu preciso? ----

cand_depfed_18 <- get_candidates(year = 2018, 
                                 position = "Deputado Federal")

#### Visualização dos dados ----

View(cand_depfed_18) # Visualizando os dados
names(cand_depfed_18) # Nome das variáveis
head(cand_depfed_18) # Visualizando as primeiras seis observações

## Para referenciar ou visualizar uma variável específica de um banco
## de dados, usamos a seguinte estrutura:
## NOME_DO_BANCO_DE_DADOS$NOME_DA_VARIÁVEL

table(cand_depfed_18$CODIGO_SEXO) # Frequência por sexo (código)
table(cand_depfed_18$DESCRICAO_SEXO) # Frequência por sexo (descrição)

prop.table(table(cand_depfed_18$DESCRICAO_SEXO)) # O percentual
nrow(cand_depfed_18) # O total de linhas no banco

# Tabela Cruzada

table(cand_depfed_18$DESCRICAO_SEXO,
      cand_depfed_18$SIGLA_PARTIDO) # Frequência por sexo (descrição) e partido

# Vale notar que nem todas as candidaturas foram deferidas pela Justiça 

table(cand_depfed_18$DES_SITUACAO_CANDIDATURA) 

#### Manipulação dos dados ----

## O dplyr é um pacote do R que possui uma estrutura gramatical única. 
## Através do operador PIPE (%>%), podemos concatenar inúmeras funções
## de maneira simplificada e organizada.
## VALE LEMBRAR: o operador PIPE pode ser escrito através do atalho
## CTRL + SHIFT + M

## Agora vamos usar os comandos de dplyr para manipular os dados

cand_depfed_18 # os dados

cand_depfed_18 %>% # os dados
  mutate(feminino = DESCRICAO_SEXO == "FEMININO") # criando uma nova variável


cand_depfed_18 %>% # os dados
  mutate(feminino = DESCRICAO_SEXO == "FEMININO") %>% # criando uma nova variável
  summarize(mean(feminino)) # calculando o percentual de feminino

# Outra forma de fazer isso:

cand_depfed_18 %>%  # os dados
  summarize(percent_fem = sum(DESCRICAO_SEXO=="FEMININO"), # total de cand FEMININO
            total_cand = n()) %>% # total de cand
  mutate(percent_fem/total_cand) # número de cand FEM por total de cand

# E se analisarmos só cands que foram deferidas?

cand_depfed_18 %>% 
  filter(DES_SITUACAO_CANDIDATURA == "APTO") %>% 
  mutate(feminino = DESCRICAO_SEXO == "FEMININO") %>% 
  summarize(percent_fem=mean(feminino))

# E se quiser saber isso por partido?

cand_depfed_18 %>% 
  filter(DES_SITUACAO_CANDIDATURA == "APTO") %>% 
  mutate(feminino = DESCRICAO_SEXO == "FEMININO") %>% 
  group_by(SIGLA_PARTIDO) %>% 
  summarize(percent_fem = mean(feminino))

# Vamos salvar esses dados

tabela_partido <- cand_depfed_18 %>% 
  filter(DES_SITUACAO_CANDIDATURA == "APTO") %>% 
  mutate(feminino= DESCRICAO_SEXO == "FEMININO") %>% 
  group_by(SIGLA_PARTIDO) %>% 
  summarize(percent_fem = mean(feminino))

View(tabela_partido)

# E se quiser saber isso por estado?

tabela_estado <- cand_depfed_18 %>% 
  filter(DES_SITUACAO_CANDIDATURA == "APTO") %>% 
  mutate(feminino = DESCRICAO_SEXO == "FEMININO") %>% 
  group_by(SIGLA_UF) %>% 
  summarize(percent_fem = mean(feminino))

View(tabela_estado)


# E se quisermos saber as duas coisas ao mesmo tempo?

tabela_estado_partido<-cand_depfed_18 %>% 
  filter(DES_SITUACAO_CANDIDATURA == "APTO") %>% 
  mutate(feminino= DESCRICAO_SEXO == "FEMININO") %>% 
  group_by(SIGLA_UF, SIGLA_PARTIDO) %>% 
  summarize(percent_fem = mean(feminino))

View(tabela_estado_partido)

# Vamos colocar o número de candidaturas do partido no estado X também:

tabela_estado_partido<-cand_depfed_18 %>% 
  filter(DES_SITUACAO_CANDIDATURA == "APTO") %>% 
  mutate(feminino= DESCRICAO_SEXO == "FEMININO") %>% 
  group_by(SIGLA_UF, SIGLA_PARTIDO) %>% 
  summarize(n = n(),
            percent_fem = mean(feminino))

View(tabela_estado_partido)

# Já sabemos o número de candidaturas por sexo, mas e de eleitas?

names(cand_depfed_18)

table(cand_depfed_18$DESC_SIT_TOT_TURNO)

## PARA LEMBRAR: existem operadores lógicos que permitem
## modificar e analisar mais de uma variável ou categoria
## em uma mesma função. São eles:
## | = OU
## & = E
## == IGUAL A
## != DIFERENTE DE

cand_depfed_18 %>% 
  filter(DES_SITUACAO_CANDIDATURA == "APTO") %>% 
  mutate(eleita = DESC_SIT_TOT_TURNO == "ELEITO POR QP" |
         DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA")

# Qual é o percentual de candidatas mulheres (v. homes) que são eleitas?

cand_depfed_18 %>% 
  filter(DES_SITUACAO_CANDIDATURA == "APTO") %>% 
  mutate(eleita= DESC_SIT_TOT_TURNO == "ELEITO POR QP" |
         DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA") %>% 
  group_by(DESCRICAO_SEXO) %>% 
  summarize(pc_eleita = mean(eleita))

# Será que isso difere por estado?

tabela_eleita_estado <- cand_depfed_18 %>% 
  filter(DES_SITUACAO_CANDIDATURA == "APTO") %>% 
  mutate(eleita = DESC_SIT_TOT_TURNO == "ELEITO POR QP" |
                  DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA") %>% 
  group_by(DESCRICAO_SEXO, SIGLA_UF) %>% 
  summarize(pc_eleita = mean(eleita))

View(tabela_eleita_estado)

# E por partido?

tabela_eleita_partido <- cand_depfed_18 %>% 
  filter(DES_SITUACAO_CANDIDATURA == "APTO") %>% 
  mutate(eleita = DESC_SIT_TOT_TURNO == "ELEITO POR QP" |
                  DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA") %>% 
  group_by(DESCRICAO_SEXO, SIGLA_PARTIDO) %>% 
  summarize(n = n(),
            n_eleita = sum(eleita),
            pc_eleita = mean(eleita))

View(tabela_eleita_partido)

#### Arte (Visualização) ----

tabela_pc_eleita <- cand_depfed_18 %>% 
  filter(DES_SITUACAO_CANDIDATURA == "APTO") %>% 
  mutate(eleita = DESC_SIT_TOT_TURNO == "ELEITO POR QP" |
                  DESC_SIT_TOT_TURNO == "ELEITO POR MEDIA") %>% 
  group_by(DESCRICAO_SEXO) %>% 
  summarize(pc_eleita = mean(eleita))

library(ggplot2)

ggplot(tabela_pc_eleita, 
       aes(x = DESCRICAO_SEXO, y = pc_eleita)) +
  geom_bar(stat = "identity")

# Vamos melhorar um pouco esse gráfico:

ggplot(tabela_pc_eleita, 
       aes(x = DESCRICAO_SEXO, y = 100 * pc_eleita)) +
  geom_bar(stat ="identity") +
  labs(x = "Sexo do Candidato", 
       y = "Percentual de eleitos",
       title = "Diferença em Sucesso entre Candidatos a Deputado Federal")

# E lembre-se que há muita diferença entre estados:

tabela_eleita_estado

plot <- tabela_eleita_estado %>% 
  filter(SIGLA_UF == "SP" |
         SIGLA_UF == "AC") %>% 
ggplot(aes(x = DESCRICAO_SEXO, y = 100 * pc_eleita)) +
  facet_wrap(~SIGLA_UF) +
  geom_bar(stat="identity") +
  labs(x = "Sexo do Candidato", y = "Percentual de eleitos",
       title = "Diferença em Sucesso entre Candidatos a Deputado Federal")


#### Salvar os dados pro pessoal do design

install.packages("writexl")
library(writexl)

write_xlsx(tabela_eleita_estado,
           "data/output/Tabela_Eleita_Por_Estado.xlsx")


### BÔNUS

## Acima fizemos gráficos simples com o intuito de mostrar o potencial
## da função ggplot. Entretanto, mesmo perguntas simples e bancos de dados
## pequenos podem ser transformados em gráficos complexos e elaborados.
## Um exemplo: poderíamos plotar os resultados do banco de dados tabela_eleita_estado
## em um mapa, de acordo com as coordenadas geográficas de cada estado.
## Quais são os passos para fazermos este gráfico?

## Primeiro, precisamos das coordenadas geográficas de cada um dos estados brasileiros.
## O IBGE fornece estas informações, no pacote GEOBR.
## Vamos instalar o pacote.

install.packages("geobr")
library(geobr)

## Neste pacote, a função que fornece os 'shapes' dos estados é
## a read_state

shapeuf <- geobr::read_state(year = 2018,
                             simplified = TRUE)

## Verifica quais são as variáveis do banco

colnames(shapeuf)

## A coluna que representa as siglas dos estados é a abbrev_state. 
## Para que possamos juntar este banco com o banco tabela_eleita_estado,
## as colunas precisam ter o mesmo nome. Também vamos aproveitar e remover
## as colunas que não serão utilizadas.

shapeuf <- shapeuf %>% 
  rename("SIGLA_UF" = "abbrev_state") %>% 
  select(SIGLA_UF, geom)

## No R, usamos a função left_join para juntar bancos de dados
## que possuem colunas em comum. Neste caso, a coluna é SIGLA_UF.

tabela_eleita_estado <- left_join(tabela_eleita_estado, 
                                  shapeuf,
                                  by = "SIGLA_UF")

## Pronto! Agora o banco tabela_eleita_estado tem uma coluna contendo
## as coordenadas geográficas de cada um dos estados.
## E agora? Como fazemos um gráfico com estes dados?
## Vamos olhar primeiro para a distribuição das candidatas eleitas nos estados.


tabela_eleita_estado %>%
  filter(DESCRICAO_SEXO == "FEMININO") %>% 
  arrange(desc(pc_eleita)) %>% 
  ggplot() +
  geom_sf(mapping = aes(geometry = geom,
                        fill = 100 * pc_eleita),
          show.legend = "polygon",
          colour = "white")+
  geom_sf_text(aes(label = SIGLA_UF, geometry = geom), 
               size = 2)+
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")+
  labs(fill = "Percentual de candidatas eleitas",
       title = paste0("Percentual de Sucesso das Candidatas a Deputada Federal",
       "\nnos estados brasileiros"))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.subtitle = element_text(hjust="0.07", size = 18),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid  = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_text(face = "bold", size = 10),
    legend.justification = "right",
    legend.position = "bottom")


## E os candidatos?

tabela_eleita_estado %>%
  filter(DESCRICAO_SEXO == "MASCULINO") %>% 
  arrange(desc(pc_eleita)) %>% 
  ggplot() +
  geom_sf(mapping = aes(geometry = geom,
                        fill = 100 * pc_eleita),
          show.legend = "polygon",
          colour = "white")+
  geom_sf_text(aes(label = SIGLA_UF, geometry = geom), 
               size = 2)+
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")+
  labs(fill = "Percentual de candidatos eleitos",
       title = paste0("Percentual de Sucesso dos Candidatos a Deputado Federal",
                      "\nnos estados brasileiros"))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.subtitle = element_text(hjust="0.07", size = 18),
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid  = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_text(face = "bold", size = 10),
    legend.justification = "right",
    legend.position = "bottom")
