train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# add variavel survived dentro de test em um novo conjunto de dados test.survived
test.survived <- data.frame(survived = rep("None", nrow(test)), test[, ])

data.combined <- rbind(train, test.survived)

str(data.combined)

# transforma as colunas suvived e pclass em Factors
data.combined$survived <- as.factor(data.combined$survived)
data.combined$pclass <- as.factor(data.combined$pclass)

table(data.combined$survived)
table(data.combined$pclass)

# carrega bilioteca
library(ggplot2)

# --- relacao sobreviventes com relacao a PCLASS
# Hipotese - Pessoas ricas sobrevivem com uma maior taxa
train$pclass <- as.factor(train$pclass)
ggplot(train, aes(x = pclass, fill = factor(survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Examinar os primeiros nomes nos dados de treino
head(as.character(train$name))

# Quantos nomes sao unicos?
length(unique(as.character(data.combined$name)))

# Existem nomes duplicados no conjunto de dados combinados (treino e teste)
# Obter os nomes duplicados e guardar como vetor
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])

# Ver os dados repetidos em data.combined
data.combined[which(data.combined$name %in% dup.names), ]

# Identificar os nomes com Miss. Mr. ...
library(stringr)



misses <- data.combined[which(str_detect(data.combined$name, "Miss.")), ]
misses[1:5, ]


mrses <- data.combined[which(str_detect(data.combined$name, "Mrs.")), ]
mrses[1:5, ]

master <- data.combined[which(str_detect(data.combined$name, "Master.")), ]
master[1:5, ]

male <- data.combined[which(train$sex == "male"), ]
male[1:5, ]

# --- relacao sobreviventes com relacao ao titulo Mr., Miss...
# Criar uma nova coluna 'Title' para Miss., Mrs., Mr., Master.
extractTitle <- function(name) {
  name <- as.character(name)

  if (length(grep("Miss.", name)) > 0) {
    return("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "name"]))
}

data.combined$title <- as.factor(titles)

# Usar somente as primeiras 891 primeiras linhas  (pois sao as que correspondem
# aos dados de treino - train)
ggplot(data.combined[1:891,], aes(x = title, fill = survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")
  
# --- relacao sobreviventes male/female
# Distribuicao male/female em test e train
table(data.combined$sex)

# Ver a distribuicao de sobreviventes separados por male/female em cada Pclass
ggplot(data.combined[1:891, ], aes(x = sex, fill = survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# --- relacao sobreviventes e idade
# Distribuicao
summary(data.combined$age)
summary(data.combined[1:891, "age"])

# Ver a distribuicao de sobreviventes, com base na idade, sexo e pclass
ggplot(data.combined[1:891, ], aes(x = age, fill = survived)) +
  facet_wrap(~ sex + pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

# Ver qual as idades das pessoas com titulo Master.
boys <- data.combined[which(data.combined$title == "Master."), ]
summary(boys$age)

# Analisar pessoas com titulos Miss.
summary(misses$age)

ggplot(misses[misses$survived != "None",], aes(x = age, fill = survived)) +
  facet_wrap(~pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for Miss. by Pclass") +
  xlab("Age") +
  ylab("Total Count")


# criacao de um conjunto de dados com todas as Misses que viajam sozinhas
# talvez util pra usar mias tarde
misses.alone <- misses[which(misses$sibsp == 0 & misses$parch == 0),]
summary(misses.alone$age)
length(which(misses.alone$age <= 14.5))


# --- encontrar informacao relevante na coluna subsp
summary(data.combined$sibsp)

length(unique(data.combined$sibsp))
data.combined$sibsp <- as.factor(data.combined$sibsp)

ggplot(data.combined[1:891,], aes(x = sibsp, fill = survived)) +
  geom_bar(width = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = " Survived")

# Pais//filhos
ggplot(data.combined[1:891,], aes(x = parch, fill = survived)) +
  geom_bar(width = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = " Survived")

# Alguns dados com relacao ao tamanho das familias
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)

ggplot(data.combined[1:891, ], aes(x = family.size, fill = survived)) +
  geom_bar(width = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") + 
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")













