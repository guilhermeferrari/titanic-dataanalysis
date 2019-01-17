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


# Converter de Factor para Inteiro
# usar as.numeric diretamente so converte os indices do Factor
temp.convTest <- as.numeric(levels(data.combined$family.size))[data.combined$family.size]
summary(temp.convTest)

# Analisar a variavel ticket
str(data.combined$ticket)

# Ticket eh um Factor com 929 niveis, considerando que existem 1309 linhas de dados
# Sao muitos niveis para ser um Factor, entao sera tratado como string
data.combined$ticket <- as.character(data.combined$ticket)
data.combined$ticket[1:20]

# Aparentemente os dados nao estao estruturados, tentar encontrar um padrao entao
# Analisar o primeiro char de cada ticket
ticket.first.char <- ifelse(data.combined$ticket == "", " ", substr(data.combined$ticket, 1, 1))
unique(ticket.first.char)

data.combined$ticket.first.char <- as.factor(ticket.first.char)

ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) + 
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total count") +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total count") +
  labs(fill = "Survived")

# Aparentemente nao ha uma relacao direta entre os sobreviventes e o numero do ticket
# -----------------------------------------------------------------------------------


# Analisar as taxas pagas (Fares)
summary(data.combined$fare)
length(unique(data.combined$fare))

ggplot(data.combined[1:891,], aes(x = fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total count")

ggplot(data.combined[1:891,], aes(x = fare, fill = survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("fare") +
  ylab("Total count") +
  labs(fill = "Survived")

# Nada muito relevante
# --------------------

# Analise da variavel Cabin
str(data.combined$cabin)

# Por possuir varios Factor provavelmente nao deveria ser um. Converter para char
data.combined$cabin <- as.character(data.combined$cabin)
data.combined$cabin[1:100]

# Trocar cabines vazias por "U"
data.combined[which(data.combined$cabin == ""), "cabin"] <- "U"

# Pegar primeiro char
cabin.first.char <- as.factor(substr(data.combined$cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)

data.combined$cabin.first.char <- cabin.first.char

ggplot(data.combined[1:891, ], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.chat") +
  xlab("cabin.first.char") +
  ylab("Total count") +
  labs(fill = "Survived")

ggplot(data.combined[1:891, ], aes(x = cabin.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total count") +
  labs(fill = "Survived")

# Pessoas com mais de uma cabine
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Aparentemente nada relevante
# ----------------------------


# Avaliar onde a pessoa embarcou
str(data.combined$embarked)
levels(data.combined$embarked)

ggplot(data.combined[1:891, ], aes(x = embarked, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("embarked") +
  ylab("Total Count") +
  labs(fill = "Survived")
  

# ---------------------
#
# Inicio Video 4
#
#----------------------

library(randomForest)

# Treinar o algoritmo com os parametros pclass e title
rf.train.1 <- data.combined[1:891, c("pclass", "title")]
rf.label <- as.factor(train$survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

# Treinar o algoritmo usando pclass, title e sibsp
rf.train.2 <- data.combined[1:891, c("pclass", "title", "sibsp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

# Treinar usando pclass, title e parch
rf.train.3 <- data.combined[1:891, c("pclass", "title", "parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

# Combinar 2 e 3
rf.train.4 <- data.combined[1:891, c("pclass", "title", "sibsp", "parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)


# Treinar usando pclass, title e family.size
rf.train.5 <- data.combined[1:891, c("pclass", "title", "family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)


# Treinar usando pclass, title, sibsp family.size
rf.train.6 <- data.combined[1:891, c("pclass", "title", "sibsp", "family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)

# Treinar usando pclass, title, parch, family.size
rf.train.7 <- data.combined[1:891, c("pclass", "title", "parch", "family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)

# ---------------------
#
# Inicio Video 5
#
#----------------------

# 
test.submit.df <- data.combined[892:1309, c("pclass", "title", "family.size")]

# Realizar a analise dos dados de teste
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

# Arquivo CSV para kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df, file = "RF_SUB_20190114_1.csv", row.names = FALSE)

# O resultado do kaggle foi diferente do que sugeria  OOB do rf.5
# 79.42% kaggle vs 81.82% OOB
rf.5

library(caret)
help(package = "caret")

library(doSNOW)

set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

table(rf.label)
342/549

table(rf.label[cv.10.folds[[33]]])
308/494

ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)

#Configurar doSNOW para treinamento multi-core
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 1000,
                  trControl = ctrl.1)
stopCluster(cl)

rf.5.cv.1

# Tentar com 5-fold repeated 10 times
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10, index = cv.5.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)
stopCluster(cl)

rf.5.cv.2


# 3-fold

set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.3.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 64, trControl = ctrl.3)
stopCluster(cl)

rf.5.cv.3


















