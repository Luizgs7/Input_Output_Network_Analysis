## Network analysis with Input-Output Matrix

Este projeto demonstra como executar uma análise de Networks das matrizes Insumo-Produto disponibilizadas pelo projeto internacional do União Européia chamado de *World Input-Output Database*. ([WIOD](http://www.wiod.org/project))
Esta página é uma contribuição a divulgação do método de análise de *graphos* na Ciência Econômica.

### Pacotes necessários

```
library(tidyverse)  
library(readxl)  
library(igraph)  
library(sna)  
library(network)  
library(intergraph)  
library(xtable)  
library(reshape)  
library(ggraph)  
library(data.table)  
library(RColorBrewer)  
library(gridExtra)    
library(rlang)  
library(plotly)  
library(sqldf)  
library(GGally)  
library(grid)  
```

### O código
O código que será apresentado abaixo foi estruturado para gerar automaticamente, mas ainda contando com certo grau de personalização,  bases de resumo estatístico das análises de graphos e os próprios graphos em si, podendo serem utilizados como subsidio técnico para análises econômicas variádas.

O código segue uma ordem lógia de processamento:  
- **Setup de variáveis**:
- **Mapeamento dos países a serem análisados**:
- **Importação bases**:
- **Aplicação do filtro de corte**:
- **Resumo Estatístico**:
- **Geração dos *Graphos* **:


![Brasil 2007](/Input-Output_Network/BRA_Net_2007_005.png "Brasil 2007")  

![México 2007](/Input-Output_Network/MEX_Net_2007_005.png "México 2007")


![Test](/Input-Output_Network/net_gif_000.gif "Test")


**Luiz Gabriel de Souza**  
*luiz.gabriel.souza@hotmail.com*  
[Linkedin](https://www.linkedin.com/in/luizgsouzacrm/)  
