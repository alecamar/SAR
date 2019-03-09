---
title: "SAR exercicio pratico"
author: "Alexandre Camargo Martensen"
date: '2019-03-09 00:57:18'
output: 
  html_document:
     keep_md: yes
     toc: TRUE
     toc_depth: 3
  keep_md: TRUE
---

# Exercício prático sobre a **Relação Espécie-Área**

## Apresentação

Esse exercício elabora em cima do que vimos na aula teórica sobre a **relação espécie-área**. 

Nesse momento iremos *por a mão na massa* e rodar alguns modelos para entendermos melhor como se dá a relação espécie *vs.* área (SAR - do inglês, *"Species Area Relationship"*), que é tida por muitos como uma lei da Ecologia (*A única?!*).

A relação espécie-área é quase "universal", e isso é algo que intriga muito os pesquisadores, uma vez que as espécies e os indivíduos não são homogêneamente distribuídos no espaço. Então, como essa relação pode ser universal?! Iremos discutir isso mais pra frente no curso, por enquanto, vá pensando sobre o assunto!

Esse exercício que iremos fazer é adaptado do curso <a href="http://datacarpentry.org/semester-biology/">"Data Carpentry for Biologists"</a> que é muito bom e eu aconselho vocês a darem uma olhada. Esse exercício em especial vocês podem encontrar neste link:

http://datacarpentry.org/semester-biology/exercises/Higher-order-functions-species-area-relationship-R/

Nós iremos trabalhar com 5 dos modelos mais comuns que tentam modelar a relação espécie-área, contudo, muitos autores apontam que existem pelo menos 20 modelos com relativo suporte para explicar a relação espécie *vs.* área. 

<a href="https://onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2699.2008.02038.x">Dengler 2009</a> fez uma revisão desses modelos, e cita muitos artigos que discutem uma série de outros modelos, e inclusivem que discutem as similaridades, diferenças e confusões entre os modelos **espécie-área (SAR - Species Area Relatioship)** e os de **amostragem de espécies (SSR - Species Sampling Relationships)**, conforme discutimos na aula teórica.

## Os modelos

Os 5 modelos que iremos trabalhar são definidos por <a href="https://onlinelibrary.wiley.com/doi/10.1111/j.1365-2699.2010.02322.x">Dengler e Oldeland (2010) - o artigo é full open access - </a>como:

Potência: S = b0 * A^b1

Potência-quadrada: S = 10^(b0 + b1 * log(A) + b2 * log(A)^2)

Logarítmico: S = b0 + b1 * log(A)

Michaelis-Menten: S = b0 * A / (b1 + A)

Lomolino: S = b0 / (1 + b1^log(b2/A))

Contudo, estes modelos, bem como outros tantos são mais bem explorados no artigo de <a href="https://onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2699.2008.02038.x">Dengler de 2009</a> também full open access.

Ambos artigos estão disponíveis na pasta "./pdfs" do projeto (para quem baixou a pasta integralmente, o que eu recomendo... sempre!).

_______

## O código 

### Pacotes

Pacotes que necessitaremos

```r
library(knitr) #Apenas para visualização das tabelas em um formato mais amigável
```

### Entrada de dados

Inicialmente iremos entrar com os dados do tamanho das ilhas, criando o objeto *area*, que irá armazenar estes valores.

Caso você tenha baixado a pasta do projeto, pule este bloco de códigos e baixe diretamente a planilha "sar-areas.csv", conforme o código na sequência abaixo. 

```r
areas<-data.frame(c(1, 5.2, 10.95, 152.3, 597.6, 820, 989.8, 1232.5, 15061))
colnames(areas)<-"V1"
```

Através desse código, você consegue baixar diretamente a planilha com os dados das áreas amostradas.

```r
areas<-read.csv("sar-areas.csv", header=FALSE)
```

Visualizando as áreas das ilhas que iremos trabalhar.

```r
areas
```

```
##         V1
## 1     1.00
## 2     5.20
## 3    10.95
## 4   152.30
## 5   597.60
## 6   820.00
## 7   989.80
## 8  1232.50
## 9 15061.00
```

Posteriormente iremos entrar com os parâmetros das equações que são dados (<a href="https://onlinelibrary.wiley.com/doi/10.1111/j.1365-2699.2010.02322.x">artigo Dengler and Oldeland (2010)</a>)

```r
parametros<-read.csv("sar-model-data.csv", header=FALSE)
colnames(parametros)<-c("modelos", "b0", "b1", "b2")
```

Para quem não baixou as planilhas, entre com os dados manualmente

```r
parametros <- data.frame(rbind(c(20.81, 0.1896, NA), c(1.35, 0.1524, 0.0081),
c(14.36, 21.16, NA), c(85.91, 42.57, NA), c(1082.45, 1.59, 390000000)))
parametros<-cbind(c("Power", "PowerQuadratic", "Logarithmic", "MichaelisMenten", "Lomolino"), parametros)
colnames(parametros)<-c("modelos", "b0", "b1", "b2")
```

Visualizando os parâmetros

```r
kable(parametros)
```



modelos                 b0        b1        b2
----------------  --------  --------  --------
Power                20.81    0.1896        NA
PowerQuadratic        1.35    0.1524   8.1e-03
Logarithmic          14.36   21.1600        NA
MichaelisMenten      85.91   42.5700        NA
Lomolino           1082.45    1.5900   3.9e+08

### Funções

Nosso objetivo agora é calcularmos a riqueza predita por cada um dos modelos para cada uma das áreas, com base nos parâmetros dados acima. 

Para isso iremos criar funções para cada um dos modelos (ou seja, 5 ao todo), que calculam a riqueza para cada uma das áreas que temos (ou outras tantas que podemos entrar!). 

```r
potencia<-function(b0, Areas, b1){
  b0 * Areas^b1
}

pot_quadrada<-function(b0, b1, Areas, b2){
  10^(b0 + b1 * log10(Areas) + b2 * (log(Areas))^2)
}

loga<-function(b0, b1, Areas){
  b0 + b1 * log10(Areas)
}

MM<-function(b0, Areas, b1){
  b0 * (Areas / (b1 + Areas))
}

Lomo<-function(b0, b1, b2, Areas){
  b0/(1 + b1^(log10(b2/Areas)))
}
```

### Obtenção dos resultados

Utilizando as funções que criamos acima, iremos agora calcular as riquezas com base nos parâmetros e nas áreas dadas.

```r
mod1<-potencia(parametros[1,"b0"], areas, parametros[1,"b1"])
mod2<-pot_quadrada(parametros[2,"b0"], parametros[2,"b1"], areas, parametros[2,"b2"])
mod3<-loga(parametros[3,"b0"], parametros[3,"b1"], areas)
mod4<-MM(parametros[4,"b0"], areas, parametros[4,"b1"])
mod5<-Lomo(parametros[5,"b0"], parametros[5,"b1"], parametros[5,"b2"], areas)
```

Organizando os resultados

```r
mods<-cbind(mod1, mod2, mod3, mod4, mod5)
colnames(mods)<-c("Power", "PowerQuadratic", "Logarithmic", "Michaelis-Menten", "Lomolino")
```

Visualizando os resultados

```r
kable(mods)
```

     Power   PowerQuadratic   Logarithmic   Michaelis-Menten    Lomolino
----------  ---------------  ------------  -----------------  ----------
  20.81000         22.38721      14.36000           1.971770    19.77805
  28.44633         30.27857      29.51063           9.351727    27.36975
  32.76008         35.87601      36.35400          17.576878    31.66880
  53.96498         77.13348      60.54593          67.142675    52.73406
  69.93258        127.10642      73.10885          80.197160    68.39243
  74.25594        144.10692      76.01630          81.670125    72.59037
  76.95341        155.55478      77.74578          82.367483    75.19974
  80.22048        170.33015      79.76105          83.041774    78.34964
 128.94163        544.87057     102.76339          85.667859   123.83319

Obtendo a média dos resultados para comparar com o resultado dado no <a href="http://datacarpentry.org/semester-biology/solutions/Higher-order-functions-species-area-relationship-R.txt">site aqui</a>

```r
download.file("http://datacarpentry.org/semester-biology/solutions/Higher-order-functions-species-area-relationship-R.txt", "res_compara.txt")

res.comp<-read.table("res_compara.txt", header=TRUE, sep=",")

resultados<-cbind(areas, rowMeans(mods), res.comp)
```

Visualizando as médias e comparando com o apresentado pelo site como correto.

```r
kable(resultados)
```

       V1   rowMeans(mods)    X       area   avg_pred_S
---------  ---------------  ---  ---------  -----------
     1.00         15.86141    1       1.00       11.928
     5.20         24.99140    2       5.20       19.322
    10.95         30.84715    3      10.95       23.986
   152.30         62.30422    4     152.30       47.087
   597.60         83.74749    5     597.60       58.778
   820.00         89.72793    6     820.00       61.475
   989.80         93.56424    7     989.80       63.106
  1232.50         98.34062    8    1232.50       65.041
 15061.00        197.21533    9   15061.00       92.263

#RESULTADOS DIFEREM, VERIFICAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

### Plotando os resultados

Terminamos agora com a visualização gráfica das curvas, para que possamos analisar as diferenças entre as predições dos diferentes modelos.

Vamos ver graficamente o resultado dos nossos modelos

```r
plot(areas$V1, mod1$V1, xlab="Áreas", ylab="Espécies")
points(areas$V1, mod2$V1, col="red")
points(areas$V1, mod3$V1, col="blue")
points(areas$V1, mod4$V1, col="purple")
points(areas$V1, mod5$V1, col="brown")
```

![](exercicio_alunos_SAR_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

Vamos ver também na escala log10, que como vimos na aula teórica, "lineariza" as relações.

```r
plot(log10(areas$V1), log10(mod1$V1), xlab="log10 Áreas", ylab="log10 Espécies")
points(log10(areas$V1), log10(mod2$V1), col="red")
points(log10(areas$V1), log10(mod3$V1), col="blue")
points(log10(areas$V1), log10(mod4$V1), col="purple")
points(log10(areas$V1), log10(mod5$V1), col="brown")
```

![](exercicio_alunos_SAR_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


Note que a relação aqui ficou log10 x log10

### Perguntas

Agora que avaliamos os modelos para as ilhas do exercício, vamos assumir que estamos pensando em amostrar outras 3 ilhas, cujos tamanhos são  10, 100 e 1000 ha. Como os modelos se comportam para ilhas com estes tamanhos? Onde vai haver a maior diferença entre os modelos? Como isso pode afetar a utilização destes modelos para a tomada de decisão, caso estejamos por exemplo, interessados em entender um determinado impacto nestas ilhas?



**Coisas ainda pra fazer:** 

##Ainda necessita preparar....

Estas equações servem (com outros parâmetros) para analisarmos por exemplo, o número de espécies de Microcrustáceos zooplanctônicos das lagos do campus. Será que o número de espécies varia em função do tamanho da lagoa? 

Quais seriam os processos ecológicos que definiriam essa forma da relação espécie-área?
https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0038232

Aplicações para a conservação:
https://www.ecologyandsociety.org/vol9/iss2/art11/


**EXTRAS**

VEr se estes sites tem mais algum material interessante para incorporar na aula.
http://math.hws.edu/~mitchell/SpeciesArea/speciesAreaText.html

http://www.tiem.utk.edu/~gross/bioed/bealsmodules/spec_area.html



##Referências utilizadas

Dengler and Oldeland (2010) https://onlinelibrary.wiley.com/doi/10.1111/j.1365-2699.2010.02322.x

DEngler (2009) https://onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2699.2008.02038.x


