---
title: "SAR exercicio pratico"
author: "Alexandre Camargo Martensen"
date: '2019-03-15 16:36:35'
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

Nós iremos trabalhar com 5 dos modelos mais comuns que tentam modelar a relação espécie-área em ilhas (ver <a href="https://onlinelibrary.wiley.com/doi/10.1111/j.1365-2699.2010.02322.x">Dengler e Oldeland (2010) - o artigo é full open access - </a>), contudo, muitos autores apontam que existem pelo menos 20 modelos com relativo suporte para explicar a relação espécie *vs.* área. 

<a href="https://onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2699.2008.02038.x">Dengler 2009</a> fez uma revisão desses modelos, e cita muitos artigos que discutem uma série de outros modelos, e inclusivem que discutem as similaridades, diferenças e confusões entre os modelos **espécie-área (SAR - Species Area Relatioship)** e os de **amostragem de espécies (SSR - Species Sampling Relationships)**, conforme discutimos na aula teórica.

Ambos artigos estão disponíveis na pasta "./pdfs" do projeto.

_______

## O código 

### Pacotes

Pacotes que necessitaremos

```r
library(knitr) #Apenas para visualização das tabelas em um formato mais amigável
library(tidyr) #Para fazer as manipulacoes nas tabelas no fim para analise
```

```
## Warning: package 'tidyr' was built under R version 3.5.3
```

### Entrada de dados

Inicialmente iremos entrar com os dados do tamanho das ilhas, criando o objeto *areas*, que irá armazenar estes valores.

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

Posteriormente iremos entrar com os parâmetros das equações que são dados no (<a href="https://onlinelibrary.wiley.com/doi/10.1111/j.1365-2699.2010.02322.x">artigo de Dengler and Oldeland (2010)</a>)

```r
parametros<-read.csv("sar-model-data.csv", header=FALSE)
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
  10^(b0 + b1 * log10(Areas) + b2 * log10(Areas)^2)
}

loga<-function(b0, b1, Areas){
  b0 + b1 * log10(Areas)
}

MM<-function(b0, Areas, b1){
  b0 * (Areas / (b1 + Areas))
}

Lomo<-function(b0, b1, b2, Areas){
  b0/(1 + b1^log10(b2/Areas))
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
stdev<-apply(mods,1,sd)
media<-rowSums(mods)/5
mods<-cbind(areas, mods, media, stdev, stdev/media)
colnames(mods)<-c("Areas", "Power", "PowerQuadratic", "Logarithmic", "Michaelis-Menten", "Lomolino", "Medias", "Desvio Padrao", "CV")
```

Visualizando os resultados

```r
kable(mods)
```

    Areas       Power   PowerQuadratic   Logarithmic   Michaelis-Menten    Lomolino      Medias   Desvio Padrao          CV
---------  ----------  ---------------  ------------  -----------------  ----------  ----------  --------------  ----------
     1.00    20.81000         22.38721      14.36000           1.971770    19.77805    15.86141        8.330532   0.5252076
     5.20    28.44633         29.05838      29.51063           9.351727    27.36975    24.74736        8.643763   0.3492801
    10.95    32.76008         32.89711      36.35400          17.576878    31.66880    30.25138        7.300472   0.2413270
   152.30    53.96498         52.62999      60.54593          67.142675    52.73406    57.40353        6.347044   0.1105689
   597.60    69.93258         68.47995      73.10885          80.197160    68.39243    72.02219        4.952274   0.0687604
   820.00    74.25594         72.91834      76.01630          81.670125    72.59037    75.49021        3.708610   0.0491270
   989.80    76.95341         75.71906      77.74578          82.367483    75.19974    77.59709        2.849008   0.0367154
  1232.50    80.22048         79.14344      79.76105          83.041774    78.34964    80.10328        1.786207   0.0222988
 15061.00   128.94163        134.30311     102.76339          85.667859   123.83319   115.10184       20.341225   0.1767237

### Plotando os resultados

Terminamos agora com a visualização gráfica das curvas, para que possamos analisar as diferenças entre as predições dos diferentes modelos.

Vamos ver graficamente o resultado dos nossos modelos

```r
plot(mods$Areas, mods$PowerQuadratic, xlab="Áreas", ylab="Espécies" )
points(mods$Areas, mods$Power, col="red")
points(mods$Areas, mods$Logarithmic, col="blue")
points(mods$Areas, mods$'Michaelis-Menten', col="purple")
points(mods$Areas, mods$Lomolino, col="brown")
```

<img src="exercicio_alunos_SAR_files/figure-html/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

Vamos ver também na escala log10, que como vimos na aula teórica, "lineariza" as relações.

```r
plot(log10(mods$Areas), log10(mods$PowerQuadratic), xlab="Áreas", ylab="Espécies" )
points(log10(mods$Areas), log10(mods$Power), col="red")
points(log10(mods$Areas), log10(mods$Logarithmic), col="blue")
points(log10(mods$Areas), log10(mods$'Michaelis-Menten'), col="purple")
points(log10(mods$Areas), log10(mods$Lomolino), col="brown")
```

<img src="exercicio_alunos_SAR_files/figure-html/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />


Note que a relação aqui ficou log10 x log10

Agora iremos fazer um boxplot para vermos um pouco da variação que os modelos geram na previsão de riqueza das ilhas

```r
long<-gather(mods[,2:6])
l<-cbind(long, areas)
```


```r
kable(l)
```



key                      value         V1
-----------------  -----------  ---------
Power                20.810000       1.00
Power                28.446335       5.20
Power                32.760083      10.95
Power                53.964975     152.30
Power                69.932576     597.60
Power                74.255938     820.00
Power                76.953411     989.80
Power                80.220485    1232.50
Power               128.941632   15061.00
PowerQuadratic       22.387211       1.00
PowerQuadratic       29.058384       5.20
PowerQuadratic       32.897114      10.95
PowerQuadratic       52.629993     152.30
PowerQuadratic       68.479955     597.60
PowerQuadratic       72.918336     820.00
PowerQuadratic       75.719057     989.80
PowerQuadratic       79.143442    1232.50
PowerQuadratic      134.303109   15061.00
Logarithmic          14.360000       1.00
Logarithmic          29.510631       5.20
Logarithmic          36.354003      10.95
Logarithmic          60.545930     152.30
Logarithmic          73.108848     597.60
Logarithmic          76.016301     820.00
Logarithmic          77.745784     989.80
Logarithmic          79.761051    1232.50
Logarithmic         102.763387   15061.00
Michaelis-Menten      1.971770       1.00
Michaelis-Menten      9.351727       5.20
Michaelis-Menten     17.576878      10.95
Michaelis-Menten     67.142675     152.30
Michaelis-Menten     80.197160     597.60
Michaelis-Menten     81.670125     820.00
Michaelis-Menten     82.367483     989.80
Michaelis-Menten     83.041774    1232.50
Michaelis-Menten     85.667859   15061.00
Lomolino             19.778052       1.00
Lomolino             27.369748       5.20
Lomolino             31.668800      10.95
Lomolino             52.734060     152.30
Lomolino             68.392427     597.60
Lomolino             72.590374     820.00
Lomolino             75.199737     989.80
Lomolino             78.349642    1232.50
Lomolino            123.833194   15061.00



```r
boxplot(value ~ V1, data=l, xlab="Tamanho das ilhas", ylab="Número de espécies")
```

<img src="exercicio_alunos_SAR_files/figure-html/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />


### Perguntas

P1: Agora que avaliamos os modelos para as ilhas do exercício, vamos assumir que estamos pensando em amostrar outras 4 ilhas, cujos tamanhos são  20, 100, 1000 e 10000 ha. Como os modelos se comportam para ilhas com estes tamanhos? Onde vai haver a maior diferença entre os modelos? Como isso pode afetar a utilização destes modelos para a tomada de decisão, caso estejamos por exemplo, interessados em entender um determinado impacto nestas ilhas?

Gere figuras com estas novas ilhas inclusas e discuta os resultados

P2: Discuta alguns fatores ecológicos que podem afetar o número de espécies em uma ilha.

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

Dengler (2009) https://onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2699.2008.02038.x


