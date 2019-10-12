## Network analysis with Input-Output Matrix

Este projeto demonstra como executar uma análise de Networks das matrizes Insumo-Produto disponibilizadas pelo projeto internacional do União Européia chamado de *World Input-Output Database*. ([WIOD](http://www.wiod.org/project))
Esta página é uma contribuição a divulgação do método de análise de *graphos* na Ciência Econômica.

O código abaixo foi utilizado como base de projeto de monografia, para conclusão do curso de Ciências Economicas pela Universidade Federal do Paraná, com apoio do grupo de pesquisa [Nex](http://www.nex.ufpr.br/portal/), dirigido pelo professor e orientador [Dr. João Basilio](http://lattes.cnpq.br/5809035566596498)

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

O código segue uma ordem lógia de processamento, em um loop que processa um país de cada vez e gera os graphos e as tabelas de reseumo estatísticos automaticamente:  
- **Setup de variáveis**:
- **Mapeamento dos países a serem análisados**:
- **Importação bases**:
- **Aplicação do filtro de corte**:
- **Resumo Estatístico**:
- **Geração dos Graphos**:

```
###################################################################################################
###################################################################################################
  #Variables to be set

  ### Directories
  #DirData     <- "C:/Users/NeX/Google Drive/Nex IC 2017 - Luiz Gabriel/03.Monografia 2019/01.Scrip_R"
  DirData      <- "C:/Users/Avell/Google Drive/Nex IC 2017 - Luiz Gabriel/03.Monografia 2019/01.Scrip_R" # Where yout scritp is 
  #local_graph  <- paste0(DirData,"/02.graphos_tidyverse/") # Where the graphs are going to be saved
  local_graph  <- "C:/Users/Avell/Google Drive/Nex IC 2017 - Luiz Gabriel/03.Monografia 2019/02.Artigo/graphos/" # Where the graphs are going to be saved
  local_matrix <- paste0(DirData,"/00.Matrizes/") # Where the matrix that are going to be used are
  
  
  # Filters of the networks
  vlr_filtro <- 0.05  # Filters every just what is grater then the max value of matrix * the filter value
  mode       <- "plus"
###################################################################################################
###################################################################################################
  

#set notation that is gonna be used by R
options(scipen=999,digits=1)
options(warn=-1)

# List of dada set's used in the analysis
Countries <- c(list.files(paste0(DirData, "/00.Matrizes"),pattern = ".xlsx"))


# Rewriting the countries names to be used in the graphs
CountryName <- case_when(
  # "SOUTH"
  Countries %like% "BRA" ~ "Brasil",
  Countries %like% "RUS" ~ "Rússia",
  Countries %like% "IND" ~ "Índia",
  Countries %like% "CHN" ~ "China",
  Countries %like% "MEX" ~ "México",
  # "NORTH"
  Countries %like% "KOR" ~ "Corea",
  Countries %like% "DEU" ~ "Alemanha",
  Countries %like% "GBR" ~ "Reino Unido",
  Countries %like% "NOR" ~ "Noruega",
  Countries %like% "USA" ~ "Estados Unidos",
  TRUE ~ substr(Countries,1,3) # Yout can add a countrie's name as you want
)


#Create the layout of the data frames that is going to collect graph statistics
{
  country          <- 1
  ano              <- 1
  numedges         <- 1
  avdegree         <- 1
  diameter         <- 1
  density          <- 1
  assortdegree     <- 1
  meandist         <- 1
  closeness        <- 1
  centrality_value <- 1
  betweenness      <- 1
  Avgcluster_coef  <- 1
  mean_hub         <- 1
  pagerank         <- 1
  data_0           <- data.frame(country, ano, numedges, avdegree, diameter, density, 
                                 assortdegree, #meandist,
                                 betweenness #, Avgcluster_coef
                                 )
}

#
names <-"a"
value <- 1
ano   <- 1
country <- 1

#set
Betwenness_best0     <- data.frame(names, value, ano, country)
eig_Centrality_best0 <- data.frame(names, value, ano, country)
hub_best0            <- data.frame(names, value, ano, country)
page_rank_best0      <- data.frame(names, value, ano, country)
degree_distribution0 <- data.frame(value, ano, country)

#Degree
setor         <- c("a")
weigth_dg     <- 1
all_dg        <- 1
in_dg         <- 1
out_dg        <- 1
ano           <- 1
degree_table0 <- data.frame(setor, weigth_dg, all_dg, in_dg, out_dg, ano, country)

###############################
### NERWORK ANALISYS - LOOP ###
###############################
for (c in 1:10){
  # Import the database and save each of the IO's (2000 a 2014)
  file  <- Countries[c]
  for (j in 0:14){
    print(paste("Importing matrix ",Countries[c],"Year ", 2000+j))
    
    ini <- paste0("E", 3 + j*120)
    fim <- paste0("BI", 58 + j*120)
    ran <- paste0(ini, ":", fim)
    mIO <- read_excel(paste0(local_matrix,file), na="na", sheet = "National IO-tables", range = ran, col_names = F)
    NameObj <- paste0("m", 2000 + j)
    assign(NameObj, mIO)
    
  }

  # Remove variables that are not gonna be used any more
  rm(ini,fim,ran,i,NameObj,mIO)
  
  # Sector names
  setor_name <- c( "Agropecuária" , "Ext Madeira" , "Pesca" , "Mineração" , "I Alimenticia" , "I Textil" ,
                   "I Madeira" , "I Papel" , "Midia" , "I Petro Refinado" , "I Quimica" , "I Farmaceutica" ,
                   "I Plastico" , "I Min N Met" , "I Metais" , "I Prod Metalicos" , "I Eletronica" , "I Elêtrica" ,
                   "I Maq e Equip" , "I Veiculos" , "I Tranporte" , "I Móveis" , "Rep e Inst MaqEquip" , "Fornec Serv" ,
                   "Água" , "Saneamento" , "Construção" , "Atac Varej Veic" , "Atacado" , "Varejo" , "Transp Terrestre" ,
                   "Transp Água" , "Transp Aéreo" , "Apoio Transp" , "Correios" , "Hotel e Rest" , "Publicação" , "Rádio e TV" ,
                   "Tecomunicações" , "Informática" , "Serv Financeiros" , "Seguros" , "Outros Serv Fin" , "Imoveis" ,
                   "Ativi Legais" , "Arquit e Eng" , "Ciência e Pesquisa" , "Marketing" , "Ativ Prof" , "Serv Adm ",
                   "Adm Pública" , "Educação" , "Saúde" , "Outros Serviços" , "Servi Doméstico" , "Organizações",
                   "Final_Consumption")
  
 

    # Beginning of the statement of process ----
  for(i in seq(2000,2014,by=7)) { 
    print(paste("Networking on", CountryName[c],i))
    
    m <- get(paste0("m",i))
    colnames(m) <- setor_name
    MaxVal      <- max(m)
    nFiltro     <- round(vlr_filtro*MaxVal,0)
    m[m < nFiltro] <- 0
    
    #m_adj_dir   <- graph.adjacency(as.matrix(m), weighted = TRUE, mode = mode)
    m_adj  <- as.undirected(graph.adjacency(as.matrix(m[1:56]), weighted=TRUE, mode = mode, diag = F))
    
    #       edge_density(m_adj)
    numedges          <- round(gsize(m_adj), digits = 3)              # Number of links of the Network
    avdegree          <- round(mean(igraph::degree(m_adj)), digits=3) # Average Dregree
    diameter          <- round(diameter(m_adj, unconnected = TRUE, weight = NA), digits = 3) # VER
    density           <- round(edge_density(m_adj,loops = TRUE), digits = 3) # Density: The ratio of the number of edges and the number of possible edges.
    assortdegree      <- round(assortativity(m_adj, igraph::degree(m_adj, mode="all"),directed = T), digits = 3) #
    meandist          <- round(mean_distance(m_adj, directed = T, unconnected = TRUE), digits = 3) # Considering only nodes with at least 1 link
    closeness         <- round(mean(igraph::closeness(m_adj, mode = 'all')), digits = 5)
    centrality_value  <- data.frame(igraph::eigen_centrality(m_adj, directed = T, weights = NA)$vector) # How connected a node is with important nodes in network
    betweenness       <- mean(igraph::betweenness(m_adj, directed = T, weights = E(m_adj)$weight))
    #Avgcluster_coef  <- transitivity(m_adj,type="barrat")  
    mean_hub          <- mean(hub_score(m_adj, weights = E(m_adj)$weight)$vector) 
    pagerank          <- round(page_rank(m_adj)$vector, digits = 3)  
    
    # Graphs Legend's  
    LegTxt = list(CountryName[c],
                  paste0("Ano = ", i),
                  paste0("Nº edges = ", numedges),
                  paste0("Av. Degree = ", avdegree),
                  paste0("Diameter = ", diameter) ,
                  paste0("Density = ", density) ,
                  paste0("Dist. Media =",meandist))
    
    #if (i==2000) {Max <- max(m[,57])}
    #V(m_adj)$tot_output <- 2+(m[,57]/Max)*10

#ScaledWeight = ((1 + (m[1:56] / max(m[1:56])))^2) -1
    m_net = network( as.matrix(m[1:56]/3000)
                     ,directed = FALSE
                     #,matrix.type = "bipartite"
                     ,ignore.eval = FALSE
                     ,names.eval = "weights"
    )
    network.vertex.names(m_net) = setor_name
    
    # Creates de Graphs and saves it at the directory
    set.seed(1)
    ggnet2(m_net
                       # ,mode = "" #"https://www.rdocumentation.org/packages/sna/versions/2.4/topics/gplot.layout
                       #Node    
                       ,mode = "fruchtermanreingold"
                       #,mode = "circle"  
                       ,label = TRUE
                       ,label.trim = TRUE
                       #,label.color = "gray"
                       ,label.alpha = 0.75
                       ,size = "degree" #,node.size = 6
                       ,size.legend = "Degree"      
                       ,size.cut = 5
                       ,size.min = 1
                       ,node.color = "steelblue"
                       #Edge
                       # ,edge.color = "black"
                       ,edge.size = "weights" #,edge.size = 1
                       # ,weight.cut = 2
                       ,edge.color = "grey60"
                       #Legend
                       ,legend.size = 12
                       ,legend.position = "bottom") +
      guides(color = FALSE, size = FALSE) +
      annotate(geom = "text",
               label = LegTxt,
               x = -Inf,
               y = seq(0.39, 0.18, length = 7),
               hjust = 0, 
               vjust = 9,
               size = 5)
    # Save the plot
    sfiltro <- gsub("\\D" ,"", toString(vlr_filtro)) # Set the filter to be set in the name of the graph
    gName   <- paste0(local_graph,"/",substr(file,1,3), "_Net_", i, "_", sfiltro, ".png" ) # Set the name of the graph and for what file it is gonna be sent to
    #ggsave(gName, scale = 2.1, dpi=720)
    ggsave(gName,  width = 25, height = 20, units = "cm")
   
    #dev.off() 

    ##Insert into summary data base
    ano    <- i
    data_1 <- data.frame(country=CountryName[c],ano = round(ano, digits = 0), 
                         numedges, 
                         avdegree, 
                         diameter, 
                         density,
                         assortdegree,
                         #meandist, 
                         betweenness
                         #Avgcluster_coef
                         )
    data_0 <- rbind(data_0, data_1)
    
    #Betenness
    betwenness       <- igraph::betweenness(m_adj, directed = T, weights = E(m_adj)$weight)  
    melt_betweness   <- melt(betwenness)
    Names_betwenness <- as.data.frame(attributes(betwenness), colnames = names( "Cod"))
    btwnss_fim       <- as.data.frame(c(Names_betwenness, melt_betweness,ano = i, country=CountryName[c]))
    Betwenness_best1 <- data.frame(btwnss_fim)
    Betwenness_best0 <- rbind(Betwenness_best1, Betwenness_best0)
    Betwenness_best0 <- Betwenness_best0[Betwenness_best0$ano > 1, ]
    
    #Eigen Centrality
    
    centrality_value$names     <- rownames(centrality_value)
    rownames(centrality_value) <- NULL
    colnames(centrality_value) <- c("centrality","names")
    eig_Centrality             <- round(centrality_value$centrality, digits = 3)
    melt_eig_Centrality        <- melt(centrality_value$centrality)
    names_centrality           <- centrality_value$names
    eig_Centrality_fim         <- as.data.frame(cbind(names_centrality, melt_eig_Centrality, ano = i, country=CountryName[c]))
    eig_Centrality_best01      <- data.frame(eig_Centrality_fim)
    colnames(eig_Centrality_best01) <- c("names", "value","ano", "country")
    eig_Centrality_best0       <- rbind(eig_Centrality_best0, eig_Centrality_best01)
    eig_Centrality_best0       <- eig_Centrality_best0[eig_Centrality_best0$ano > 1, ]
    
    #HUB
    hub        <- round(hub_score(m_adj, weights = NA)$vector, digits = 3)
    melt_hub   <- melt(hub)
    Names_hub  <- as.data.frame(attributes(hub), colnames = names( "Cod"))
    hub_fim    <- as.data.frame(c(Names_hub, melt_hub,ano = i, country=CountryName[c]))
    hub_best1  <- data.frame(hub_fim)
    hub_best0  <- rbind(hub_best1, hub_best0)
    hub_best0  <- hub_best0[hub_best0$ano > 1, ]
    
    #page_rank
    page_rank        <- round(page_rank(m_adj)$vector, digits = 3)
    melt_page_rank   <- melt(page_rank)
    Names_page_rank  <- as.data.frame(attributes(page_rank), colnames = names( "Cod"))
    page_rank_fim    <- as.data.frame(c(Names_page_rank, melt_page_rank,ano = i, country=CountryName[c]))
    page_rank_best1  <- data.frame(page_rank_fim)
    page_rank_best0 <- rbind(page_rank_best1, page_rank_best0)
    page_rank_best0  <- page_rank_best0[page_rank_best0$ano > 1, ]
    
    #Degre distribution
    degree_distribution1  <- data.frame(value = igraph::degree_distribution(m_adj, mode = "total"), ano = i, country=CountryName[c])
    degree_distribution0  <- rbind(  degree_distribution1, degree_distribution0)
    degree_distribution0  <- degree_distribution0[degree_distribution0$ano > 1, ]
    
    #DEGREE
    md_adj        <- as.directed(graph.adjacency(as.matrix(m[1:56]), weighted = TRUE, mode = "directed", diag = T))
    weigth_dg     <- data.frame(strength(md_adj, vids = V(md_adj), mode = "all", loops = TRUE))
    in_dg_setor   <- data.frame(igraph::degree(md_adj, v = V(md_adj), mode = "in", loops = TRUE, normalized = F))
    out_dg_setor  <- data.frame(igraph::degree(md_adj, v = V(md_adj), mode = "out", loops = TRUE, normalized = F))
    all_dg_setor  <- data.frame(igraph::degree(md_adj, v = V(md_adj), mode = "all", loops = TRUE, normalized = F))  
    
    weigth_dg              <- setDT(weigth_dg, keep.rownames = TRUE)[]
    colnames(weigth_dg)    <- c("setor","weigth_dg")
    colnames(in_dg_setor)  <- c("in_dg") 
    colnames(out_dg_setor) <- c("out_dg") 
    colnames(all_dg_setor) <- c("all_dg") 
    
    degree_table  <- data.frame(weigth_dg, all_dg_setor ,in_dg_setor, out_dg_setor, ano = i, country=CountryName[c])
    degree_table0 <- rbind(degree_table0, degree_table)
    degree_table0 <- degree_table0[degree_table0$ano > 1, ]
    
     
  }
  
}
```
### Exemplos dos Graphos gerados:

![Brasil 2007](/Input-Output_Network/BRA_Net_2007_005.png "Brasil 2007")  

![México 2007](/Input-Output_Network/MEX_Net_2007_005.png "México 2007")


### Transformando os graphos em GIFs:
![Test](/Input-Output_Network/net_gif_000.gif "Test")



**Luiz Gabriel de Souza**  
*luiz.gabriel.souza@hotmail.com*  
[Linkedin](https://www.linkedin.com/in/luizgsouzacrm/)  
