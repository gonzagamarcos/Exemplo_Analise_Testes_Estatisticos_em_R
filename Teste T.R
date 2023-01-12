# Exemplo basiado nos exercícios "Estatistica na Prática 2". Curso Big Data Analytics com R e 
# Microsoft Azure Machine Learning da Data Science Academy (DSA).

# Testes Estatísticos a serem usdos.

# Teste T
# Teste F
# Shapiro-Wilk
# Teste ANOVA

# Objetivo:
# O Intuito é praticar os conceitos sobre os Testes acima aprendidos no curso, usando exemplos 
# didatico. Aplicar os conceitos dos teste estatístico e interpretar os resultados.


# Pergunta de Negócio:
# Existe diferença na pontuação do nível de Memória influenciado pelo tipo de Medicamento? 


# Conjunto de Dados: Memory Test on Drugged Islanders Data
# Keggle
# Link: https://www.kaggle.com/datasets/steveahn/memory-test-on-drugged-islanders-data


# Carregando Pacotes
library(tidyverse)


# Carregando o Dataset
df <- read.csv("Islander_data.csv")


# Visualizado df
View(df)


# Selecionando as Colunas que farão parte do Exemplo.
df <- df[c(5,6,8)]
View(df)


# Para este exemplo será usado apenas dois grupos do Dataset.
# Removendo o grupo "T"
unique(df$Drug)

grupo_t <- df$Drug == "T"
grupo_t

df <- df%>%
  filter(Drug != "T")

View(df)


# Verificando se existem valores NA
colSums(is.na(df)) # Não há Valores NA no conjunto de dados.


# Verificando as variáveis
str(df)
summary(df)


# Convertendo a Variável Drug para o tipo factor
df$Drug <- as.factor(df$Drug)


# Renomeando as colunas
new_name <- colnames(df)

new_name[1] <- "Dose"
new_name[2] <- "Medicamento"
new_name[3] <- "Pontos_Memoria"

colnames(df) <- new_name
head(df)


# Estatísticas e Plots

# Histograma da variável "Pontos_Memoria"
hist(df$Mem_Score_After)


# GGPairs
ggpairs(df)


# Boxplot com a distribuição de medicamento
qplot(Medicamento,
      Pontos_Memoria,
      data = df, 
      main = "Nível de Memória por Tipo de Medicamento",
      xlab = "Tipo de Medicamento", 
      ylab = "Pontuação do Nível de Memória") + 
  geom_boxplot(aes(fill = Medicamento))


# Conclusão da análise acima:
# É possível notar que há uma diferença nos Nível de Memória, mas isso está relacionado ao tipo de 
# Medicamento? 

# Para responder essa quetão vou usar o Teste T, mas antes é necessário validar as suposições do
# Próprio.


#### Teste T (Paramétrico) para uma amostra

# Para aplicar o Teste t, antes é necessário validar as 5 suposições do Teste.

# 1 - Os dados são aleatórios e representativos da população.
# 2 - A variável dependente é contínua.
# 3 - Ambos os grupos são independentes (ou seja, grupos exaustivos e excludentes).
# 4 - Os resíduos do modelo são normalmente distribuídos.
# 5 - A variância residual é homogênea (princípio da homocedasticidade).


#### Suposição 1,2 e 3:

# Observação: Vou considerar validadas a suposições 1, 2 e 3 para este exemplo.
# Como não tenho informações da coletas dos dados, e como os dados foram organizados, vou consideram
# essas três suposições válidas.


#### Suposição 4: Os resíduos do modelo são normalmente distribuídos.

# Validando a Suposição 4 com teste de normalidade de Shapiro Wilk.
# valor-p > 0,05, Segue uma distribuição normal.

# Hipóteses:

# H0: Os dados são normalmente distribuídos.

# H1: Os dados não são normalmente distribuídos.


?shapiro.test # Shapiro-Wilk Normality Test

shapiro.test(df$Pontos_Memoria[df$Medicamento == 'A']) # p-value = 0.009348
shapiro.test(df$Pontos_Memoria[df$Medicamento == 'S']) # p-value = 0.08148 

# Conclusão da análise acima:

# p-value < 0.05 para o grupo de Medicamentos "A", ou seja, rejeita-se H0, logo os dados não são
# normalmente distribuídos.

# p-value > 0.05 para o grupo de Medicamentos "S", ou seja, falha ao rejeitar H0, logo os dados são
# normalmente distribuídos. 


# Conclui que existe sim diferença significativa no uso de diferentes tipos de medicamento,
# como é possível confirma com o resultado apresentado pelo Boxplot.

# Poderia finalizar o estudo de caso aqui, pois a pergunta de negocio foi respondida, mas para fins 
# didádicos vou continuar com a última validação do Teste T.


#### Suposição 5: A variância residual é homogênea (princípio da homocedasticidade).

# Aplicando o Teste F para comparar se a média dos dois grupos tem a mesma variância.
# p-value > 0.05, rejeita a hipótese nula (H0) de que as médias dos dois grupos são iguais.

# Hipóteses:

# H0 (Hipótese Nula) = As médias de dados extraídos de uma população normalmente distribuída tem a 
# mesma variância.

# H1 (Hipótese Alternativa) = As médias de dados extraídos de uma população normalmente distribuída 
# não tem a mesma variância.

?var.test # F Test to Compare Two Variances
var.test(Pontos_Memoria ~ Medicamento, data = df) # p-value = 0.07698

# Conclusão da análise acima:
# p-value > 0.05 rejeita H0, há diferença entre as variâncias das médias dos Medicamentos.
# Ao não validar a Suposição do Teste F, confirma-se que há de fato diferença entre o uso dos 
# Medicamentos, cada medicamento influencia de forma diferente no nível de Mémoria.


#### Aplicando o Teste T:

# Mesmo com o Teste T sendo inválido para responder a pergunta inicial, vou aplicar o teste e
# interpretar o resultado comparando com as análises realizadas acima.

# Hipóteses:

# H0 (Hipótese Nula) = Não há diferença significativa entre as médias dos 2 grupos.
# Ha (Hipótese Alternativa) = Há diferença significativa entre as médias dos 2 grupos.

grupo_a = df$Pontos_Memoria[df$Medicamento == 'A']
grupo_a

grupo_s = df$Pontos_Memoria[df$Medicamento == 'S']
grupo_s

t.test(grupo_a, grupo_s, paired = FALSE, var.equal = FALSE, conf.level = 0.95) # 0.003772

# Conclusão da análise acima:
# p-value < 0.05, rejeita-se H0, logo, há diferença no uso dos Medicamentos para o aumento do nível 
# de Mémoria. Como foi possível constar no Boxplot e nas suposições 4 e 5.


# Solução Alternativa: Usando o Teste ANOVA

# Vou abordar o problema em questão de uma outra perspectiva, será que a dosagem também influencia
# no nível de Mémoria?

# Para responder, vou usar o Teste ANOVA que compara a média de dois ou mais grupos são 
# significativamente diferentes entre si.


# Suposições a serem validadas para aplicar o Teste ANOVA:

# 1 - Dentro de cada amostra, as observações são amostradas aleatoriamente e independentemente umas 
# das outras.

# 2 - Cada amostra de grupo é extraída de uma população normalmente distribuída.


#### Suposição 1:

# Observação:
# Vou considerar que é verdadeira, pois nçao é possível confirmar essa inflrmação para este Dataset.


#### Suposição 2:

# Validando a Suposição 2 com teste de normalidade de Shapiro Wilk.
# valor-p > 0,05, Segue uma distribuição normal.

# Separando as Doses em grupos:
unique(df$Dose)

dose_1 = df$Pontos_Memoria[df$Dose == 1]
dose_2 = df$Pontos_Memoria[df$Dose == 2]
dose_3 = df$Pontos_Memoria[df$Dose == 3]

# Hipóteses:

# H0: Os dados são normalmente distribuídos.

# H1: Os dados não são normalmente distribuídos

shapiro.test(dose_1) # p-value = 0.01569
shapiro.test(dose_2) # p-value = 0.03847
shapiro.test(dose_3) # p-value = 0.5648

# Conclusão da análise acima:
# valor-p < 0.05 para os grupos com Dosagem 1, 2, logo rejeita-se H0, os dados não segue uma
# distribuição normal.

# valor-p > 0.05 para o grupo com Dosagem 3, logo falha ao rejeitar H0, os dados seguem uma
# distribuição nomal.


#### Aplicando o Teste ANOVA:
teste_anova = aov(Pontos_Memoria ~ Dose, df)
summary(teste_anova)

# Conclusão da análise acima:
# Valor-p < 0.05, ou seja, 0.0265 < 0.05, logo, rejeitamos a H0. 
# As médias dos grupos não são as mesmas e consequentemente os medicamentos não tem o mesmo efeito.



#### Conclusão Final:

# Há uma significância estatística no tipo de medicamento e na dosagem, no aumento do Nível de
# Memória.






















