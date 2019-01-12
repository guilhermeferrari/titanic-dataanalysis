# Titanic Análise de Dados

Primeiro teste com data science usando R.

Desenvolvido com base na serie de videos: [Intro to Data Science with R](https://www.youtube.com/watch?v=32o0DnuRjfg&list=PLTJTBoU5HOCRrTs3cJK-PbHM39cwCU0PF)

# Problemas encontrados

## Utilizando os dados
Por ser uma série de video antiga, algumas coisas devem ser levadas em consideração. Apesar do conjunto de dados estar disponível no [kaggle](kaggle.com), os dados foram atualizados com o passar do tempo, causando algumas inconsistencias na hora de acompanhar o video. 
Para evitar problemas, o autor dos videos disponibiliza os dados usados, que são os mesmos encontrados nesse repositório.

## Erros na hora de plotar os gráficos

### Erro na hora de plotar gráficos
Logo no primeiro plot já aparece um erro relacionado ao uso de `geom_histogram()`.
Para resolver, basta usar `geom_bar(width = 0.5)`. No decorrer dos videos é possível que apareçam algumas inconsistências. Se for o caso basta observar o meu código e comparar com o usado nos videos, pois já fiz as mudanças necessárias.

### Inconsistência do gráfico do vídeo e do gráfico que aparece para mim
Pelo que observei isso pode acontecer por dois motivos. 

O primeiro motivo é que nos videos, a barra dos gráficos começar no final do indicador no eixo x, enquanto na versão atual começa no meio. Isso pode causar algumas mudanças mas o comportamento dos gráficos no geral é o mesmo. 

O outro motivo é que nos videos o autor usou um limite para o eixo Y. Então, por exemplo se você limitar o plot até 100 no eixo Y, e possuir dados que ultrapassem esse valor, O valor simplesmente não vai aparecer no gráfico. Recomendo não usar `ylim()`