###########################################

# Instalando pacotes
install.packages('dplyr')
install.packages('stringr')
install.packages('ggplot2')

###########################################

# Chamando os pacotes instalados
library(ggplot2)
library(dplyr)
library(stringr)

###########################################

#Chamando a base

setwd("C:/Users/luiz1/OneDrive/Área de Trabalho/Meu 1º Projeto")
DOEXT <- read.csv('DOEXT.csv')

#Arrumando a variavel DataObito

adicionar_zero <- function(valor) {
  if (nchar(as.character(valor)) == 7) {
    paste0("0", valor)
  } else {
    as.character(valor)
  }
}

DOEXT$DTOBITO <- as.Date(sapply(DOEXT$DTOBITO, function(x) {
  ifelse(nchar(as.character(x)) == 7, adicionar_zero(x), as.character(x))
}), format = "%d%m%Y")

#Selecionando as variáveis 
dados = DOEXT %>% select(IDADE, SEXO, CAUSABAS, ACIDTRAB, RACACOR, OCUP, DTOBITO)

#Filtrando a Ocupação
tabela_Ocup = str_sub(dados$OCUP, end = 3)

table(tabela_Ocup)

dados_Ocup = dados %>% filter(str_detect(tabela_Ocup, "512"))

dados_Ocup$OCUP = dados_Ocup$OCUP %>% as.numeric()

dados_Ocup$OCUP = recode(dados_Ocup$OCUP,"512105" = "Serviços Gerais", "512110" ="Arrumador", "512115"= "Faxineiro ", "512120" = "Diarista"  )

table(dados_Ocup$OCUP)

#Filtrando idade

dados_Ocup$IDADE = dados_Ocup$IDADE %>% as.numeric()

dados_Ocup_Idad = dados_Ocup %>% filter(IDADE > 413 & IDADE < 476)

table(dados_Ocup_Idad$ACIDTRAB, dados_Ocup_Idad$SEXO)

#Filtando Acidente de trabalho 

table(dados_Ocup_Idad$ACIDTRAB)

dados_Ocup_Idad$ACIDTRAB = dados_Ocup_Idad$ACIDTRAB %>% as.numeric()

dados_Ocup_Idad_Acid = dados_Ocup_Idad %>% filter(ACIDTRAB > 0 & ACIDTRAB < 2)

table(dados_Ocup_Idad_Acid$SEXO, dados_Ocup_Idad_Acid$ACIDTRAB)

table(dados_Ocup_Idad_Acid$RACACOR, dados_Ocup_Idad_Acid$SEXO)
table(dados_Ocup_Idad_Acid$OCUP, dados_Ocup_Idad_Acid$SEXO)


#Filtrando Maculino e Feminino com Acidente de Trabalho 

dados_Ocup_Idad_Acid$SEXO = dados_Ocup_Idad_Acid$SEXO %>% as.numeric()

dados_Ocup_Idad_Acid_M = dados_Ocup_Idad_Acid %>% filter(SEXO > 0 & SEXO < 2)
dados_Ocup_Idad_Acid_F = dados_Ocup_Idad_Acid %>% filter(SEXO > 1 & SEXO < 3)

dados_Ocup_Idad_Acid_F$IDADE = str_sub(dados_Ocup_Idad_Acid_F$IDADE, start = 2)
dados_Ocup_Idad_Acid_M$IDADE = str_sub(dados_Ocup_Idad_Acid_M$IDADE, start = 2)

dados_Ocup_Idad_Acid_M$IDADE = as.numeric(dados_Ocup_Idad_Acid_M$IDADE)
dados_Ocup_Idad_Acid_F$IDADE = as.numeric(dados_Ocup_Idad_Acid_F$IDADE)

par(mfrow = c(1, 2))
hist(dados_Ocup_Idad_Acid_M$IDADE, main = "Gênero Maculino por idade",  xlab = "Idade", ylab = "Frequência", col = "purple")

hist(dados_Ocup_Idad_Acid_F$IDADE, main = "Gênero Feminino por idade", xlab = "Idade", ylab = "Frequência", col = "gold")


#Filtrando Masculino e Feminino sem Acidente de Trabalho

dados_Ocup_Idad$SEXO = dados_Ocup_Idad$SEXO %>% as.numeric()

dados_Ocup_Idad_M = dados_Ocup_Idad %>% filter(SEXO > 0 & SEXO < 2)
dados_Ocup_Idad_F = dados_Ocup_Idad %>% filter(SEXO > 1 & SEXO <3)



#Fitrando CAUSABAS masculino e feminino Acid

causabasMA = table(dados_Ocup_Idad_Acid_M$CAUSABAS)

ordemMA = sort(causabasMA , decreasing = TRUE)

CAUSABAS10MA = head (ordemMA, 10)
CAUSABAS10MA

causabasFA = table(dados_Ocup_Idad_Acid_F$CAUSABAS)

ordemFA = sort(causabasFA , decreasing = TRUE)

CAUSABAS10FA = head (ordemFA, 10)
CAUSABAS10FA

#Filtrando CAUSABAS masc e fem sem Acid

causabasM = table(dados_Ocup_Idad_M$CAUSABAS)

ordemM = sort(causabasM , decreasing = TRUE)

CAUSABAS10M = head (ordemM, 10)
CAUSABAS10M

causabasF = table(dados_Ocup_Idad_F$CAUSABAS)

ordemF = sort(causabasF , decreasing = TRUE)

CAUSABAS10F = head (ordemF, 10)
CAUSABAS10F

#Séries temporais CAUSABAS10M E CAUSABAS10F

CAUSABAS10M

colunas = names(CAUSABAS10M)
resultado <- filter(dados_Ocup_Idad_M, CAUSABAS %in% colunas) %>%
  select(DTOBITO, CAUSABAS)

p1 <- filter(resultado, CAUSABAS == "X934")

# Crie uma nova coluna com o mês correspondente
coluna_mes <- format(p1$DTOBITO, "%Y-%m")

# Faça a contagem por mês
contagem <- coluna_mes %>%
  as.data.frame() %>%
  mutate(MES = coluna_mes) %>%
  count(MES)
contagem
contagem$meses = 1:nrow(contagem)  

contagem$MES <- as.Date(paste0(contagem$MES, "-01"))

contagem

# Crie o gráfico de série temporal
X934 <- ggplot(contagem, aes(x = MES, y = n)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "8 month") +
  xlab("Mês") +
  ylab("Número de óbitos") +
  ggtitle("Série Temporal de Óbitos")

# Exiba o gráfico
X934

#Aqui, para fazer no caso feminino, é só repetir o processo, substituindo
#CAUSABAS10M por CAUSABAS10F


#Gráfico de porcentagem da CAUSABAS10FA e CAUSABAS10MA

dados_feminino <- data.frame(
  Categoria = c("V234", "V031", "V093", "V892", "W130", "V235", "V134", "V245", "V299", "W199"),
  Proporcao = c(14.29, 12.50, 12.50, 12.50, 10.71, 8.93, 7.14, 7.14, 7.14, 7.14),
  Genero = "Feminino"
)

dados_masculino <- data.frame(
  Categoria = c("W879", "V892", "V244", "W209", "X599", "V041", "V093", "W139", "V499", "V234"),
  Proporcao = c(14.05, 12.97, 11.89, 10.27, 10.27, 9.73, 8.11, 8.11, 7.57, 7.03),
  Genero = "Masculino"
)

dadosFM <- rbind(dados_feminino, dados_masculino)
p <- ggplot(dadosFM, aes(x = Categoria, y = Proporcao, fill = Genero)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Proporcao), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Porcentagens e Mortes por Categoria de Morte",
       y = "Porcentagem",
       x = "Categoria de Morte") +
  scale_fill_manual(values = c("Feminino" = "gold", "Masculino" = "purple")) +
  theme_minimal()
p
