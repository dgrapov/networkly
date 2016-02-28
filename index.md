
# Description


# Install


```r
devtools::install_github("dgrapov/networkly.git")
```

# Examples
## Create network `Edge List` and `Node Attributes`

```r
library(networkly)
library(plotly)
#set up network structure
conn<-1 # average number of conenctions per variable
nodes<-10 # number of variables
net_size<-conn*nodes
edge_type<-2 # number of diffrent connections


#color/size
set.seed(555)
id<-factor(sample(1:edge_type,net_size,replace = TRUE))
id2<-factor(sample(1:10,nodes,replace = TRUE))

edge.list<-data.frame(source=sample(1:nodes,net_size,replace=TRUE),
                      target=sample(1:nodes,net_size,replace=TRUE),
                      color=rainbow(edge_type)[id],
                      size=sample(seq(1,10,length.out=10),edge_type,replace=TRUE)[id],
                      names=letters[id],stringsAsFactors = FALSE)
node.data<-data.frame(color=sample(rainbow(10),nodes,replace=TRUE)[id2],
                      size=sample(seq(5,15,length.out=10),nodes,replace=TRUE)[id2],
                      names=sample(LETTERS[1:5],nodes,replace=TRUE)[id2],stringsAsFactors = FALSE)
```
## `Edge List` describing network connections

```r
head(edge.list)
```

```
##   source target     color size names
## 1      4      5 #FF0000FF    8     a
## 2      1      3 #00FFFFFF    5     b
## 3      3      7 #00FFFFFF    5     b
## 4      3      3 #00FFFFFF    5     b
## 5      2      3 #00FFFFFF    5     b
## 6      6      3 #FF0000FF    8     a
```

## `Node Attributes` describing variables

```r
head(node.data)
```

```
##       color      size names
## 1 #FF0000FF  8.333333     C
## 2 #0066FFFF  6.111111     A
## 3 #FF9900FF  9.444444     D
## 4 #FF9900FF  9.444444     D
## 5 #FF9900FF 15.000000     B
## 6 #CC00FFFF  9.444444     C
```

## Create 2D network

```r
layout<-"fruchtermanreingold" #see networkly::get_network for 2D and 3D options

#net params
type<-"2d"
color<-'color'
size<-'size'
name<-'names'

#create network objects
obj<-get_network(edge.list,type=type,layout=layout)
net<-c(get_edges(obj,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=FALSE),get_nodes(obj,node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=FALSE))

#add legend
legend<-format_legend(obj,node.data=node.data)

net<-c(net,c(get_edges(legend,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=TRUE),get_nodes(legend,node.data=legend$node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=TRUE)))


net<-shiny_ly(net) # works in or out of shiny

#add layout options
layout(net,
       xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, hoverformat = '.2f'),
       yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, hoverformat = '.2f'))
```
<iframe id="example1" src="./inst/www/html/2dnetwork.html" style="border: none; width: 100%; height: 500px" frameborder="0"></iframe>


## Create 3D network

```r
#net params
type<-"3d"

#create network objects
obj<-get_network(edge.list,type=type,layout=layout)
net<-c(get_edges(obj,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=FALSE),get_nodes(obj,node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=FALSE))

#add legend
legend<-format_legend(obj,node.data=node.data)

net<-c(net,c(get_edges(legend,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=TRUE),get_nodes(legend,node.data=legend$node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=TRUE)))


net<-shiny_ly(net) 

#add layout options
layout(net,
         scene = list(showlegend=TRUE,
                      yaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title=""),
                      xaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title=""),
                      zaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title="")))
```
<iframe id="example2" src="./inst/www/html/3dnetwork.html"" style="border: none; width: 100%; height: 500px" frameborder="0"></iframe>

## Shiny
## `ui.R`

```r
shinyUI(bootstrapPage(
    plotlyOutput("network")
))
```
## `server.R` 

```r
shinyServer(function(session,input, output) {
  output$network<-renderPlotly({
    #network creation script goes here
    #see above for how to create 2d or 3d networks
    shiny_ly(net)
  })
})
```
## Run Demo App or try [HERE](http://ec2-52-22-43-130.compute-1.amazonaws.com:3838/demos/networkly/)

```r
networklyShiny()
```

## Limitations
#### To allow edge widths in 2D and 3D networks and to properly render edge paths in 3D networks each edge element is an indepnedent list item. The shown edge and node legends are place holders which do not correctly show or hide components. This can be changed during the network creation procces by using `showlegend=TRUE` and not creating a place holder legend as shown below. For 2D networks edge segments can be renderd from a vector with intermittent NAs e.g. `x/y/z = source1 target1 NA source2 target2` this currently does not work for 3D networks.

```r
net<-c(get_edges(obj,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=TRUE),get_nodes(obj,node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=TRUE))
shiny_ly(net)
```

## About
### updated 2016-02-28


```r
sessionInfo()
```

```
## R version 3.2.3 (2015-12-10)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 8.1 x64 (build 9600)
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] plotly_2.4.4  ggplot2_2.0.0 networkly_0.1
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.3      knitr_1.12       magrittr_1.5     network_1.12.0  
##  [5] munsell_0.4.3    colorspace_1.2-6 R6_2.1.2         stringr_1.0.0   
##  [9] httr_1.1.0       plyr_1.8.3       dplyr_0.4.1      tools_3.2.3     
## [13] parallel_3.2.3   grid_3.2.3       gtable_0.1.2     sna_2.3-2       
## [17] DBI_0.3.1        htmltools_0.3    yaml_2.1.13      assertthat_0.1  
## [21] digest_0.6.9     gridExtra_2.0.0  formatR_1.2.1    viridis_0.3.2   
## [25] htmlwidgets_0.5  base64enc_0.1-3  evaluate_0.8     rmarkdown_0.9   
## [29] stringi_1.0-1    scales_0.3.0     jsonlite_0.9.19
```
