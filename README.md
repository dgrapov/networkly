
# Description


# Install


```r
devtools::install_github("dgrapov/networkly.git")
```

# Examples
## Create network `Edge List` and `Node Attributes`

```r
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
library(networkly)
library(plotly)
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

<!--html_preserve--><div id="htmlwidget-2706" style="width:672px;height:480px;" class="plotly"></div>
<script type="application/json" data-for="htmlwidget-2706">{"x":{"data":[{"x":[-32.2043206004319,-31.8497107964598],"y":[5.67782639338277,7.18550954493934],"z":null,"name":"a","line":{"color":"rgb(255,0,0)","width":8},"mode":"lines","type":"scatter","hoverinfo":"none","showlegend":false},{"x":[-39.9770523848925,-38.0440824082017],"y":[8.87740390381926,8.25478958116065],"z":null,"name":"a","line":{"color":"rgb(255,0,0)","width":8},"mode":"lines","type":"scatter","hoverinfo":"none","showlegend":false},{"x":[-38.2471427840979,-38.0440824082017],"y":[10.5641353489692,8.25478958116065],"z":null,"name":"a","line":{"color":"rgb(255,0,0)","width":8},"mode":"lines","type":"scatter","hoverinfo":"none","showlegend":false},{"x":[-38.0440824082017,-36.4135007245832],"y":[8.25478958116065,10.2068610427864],"z":null,"name":"a","line":{"color":"rgb(255,0,0)","width":8},"mode":"lines","type":"scatter","hoverinfo":"none","showlegend":false},{"x":[-39.4786198096367,-38.0440824082017],"y":[6.46269076881508,8.25478958116065],"z":null,"name":"b","line":{"color":"rgb(0,255,255)","width":5},"mode":"lines","type":"scatter","hoverinfo":"none","showlegend":false},{"x":[-38.1463323465659,-38.0440824082017],"y":[5.72077799560505,8.25478958116065],"z":null,"name":"b","line":{"color":"rgb(0,255,255)","width":5},"mode":"lines","type":"scatter","hoverinfo":"none","showlegend":false},{"x":[-38.0440824082017,-36.4135007245832],"y":[8.25478958116065,10.2068610427864],"z":null,"name":"b","line":{"color":"rgb(0,255,255)","width":5},"mode":"lines","type":"scatter","hoverinfo":"none","showlegend":false},{"x":[-38.0440824082017,-38.0440824082017],"y":[8.25478958116065,8.25478958116065],"z":null,"name":"b","line":{"color":"rgb(0,255,255)","width":5},"mode":"lines","type":"scatter","hoverinfo":"none","showlegend":false},{"x":[-39.4786198096367,-38.0440824082017],"y":[6.46269076881508,8.25478958116065],"z":null,"name":"b","line":{"color":"rgb(0,255,255)","width":5},"mode":"lines","type":"scatter","hoverinfo":"none","showlegend":false},{"x":[-39.4786198096367,-38.1463323465659],"y":[6.46269076881508,5.72077799560505],"z":null,"name":"b","line":{"color":"rgb(0,255,255)","width":5},"mode":"lines","type":"scatter","hoverinfo":"none","showlegend":false},{"x":[-32.0463014537039],"y":[10.5701822711879],"z":null,"name":"B","marker":{"color":"rgb(0,102,255)","size":12.7777777777778},"mode":"markers","type":"scatter","hoverinfo":"name","showlegend":false},{"x":[-39.4786198096367],"y":[6.46269076881508],"z":null,"name":"A","marker":{"color":"rgb(0,102,255)","size":6.11111111111111},"mode":"markers","type":"scatter","hoverinfo":"name","showlegend":false},{"x":[-39.9770523848925,-34.5765292013333],"y":[8.87740390381926,2.87269735243468],"z":null,"name":"C","marker":{"color":"rgb(204,0,255)","size":9.44444444444444},"mode":"markers","type":"scatter","hoverinfo":"name","showlegend":false},{"x":[-36.4135007245832],"y":[10.2068610427864],"z":null,"name":"E","marker":{"color":"rgb(255,0,0)","size":7.22222222222222},"mode":"markers","type":"scatter","hoverinfo":"name","showlegend":false},{"x":[-38.1463323465659],"y":[5.72077799560505],"z":null,"name":"C","marker":{"color":"rgb(255,0,0)","size":8.33333333333333},"mode":"markers","type":"scatter","hoverinfo":"name","showlegend":false},{"x":[-31.8497107964598,-38.2471427840979],"y":[7.18550954493934,10.5641353489692],"z":null,"name":"B","marker":{"color":"rgb(255,153,0)","size":15},"mode":"markers","type":"scatter","hoverinfo":"name","showlegend":false},{"x":[-38.0440824082017,-32.2043206004319],"y":[8.25478958116065,5.67782639338277],"z":null,"name":"D","marker":{"color":"rgb(255,153,0)","size":9.44444444444444},"mode":"markers","type":"scatter","hoverinfo":"name","showlegend":false},{"x":[null],"y":[null],"z":null,"name":"a","line":{"color":"rgb(255,0,0)","width":8},"mode":"lines","type":"scatter","hoverinfo":"none","showlegend":true},{"x":[null],"y":[null],"z":null,"name":"b","line":{"color":"rgb(0,255,255)","width":5},"mode":"lines","type":"scatter","hoverinfo":"none","showlegend":true},{"x":[null],"y":[null],"z":null,"name":"B","marker":{"color":"rgb(0,102,255)","size":12.7777777777778},"mode":"markers","type":"scatter","hoverinfo":"name","showlegend":true},{"x":[null],"y":[null],"z":null,"name":"A","marker":{"color":"rgb(0,102,255)","size":6.11111111111111},"mode":"markers","type":"scatter","hoverinfo":"name","showlegend":true},{"x":[null],"y":[null],"z":null,"name":"C","marker":{"color":"rgb(204,0,255)","size":9.44444444444444},"mode":"markers","type":"scatter","hoverinfo":"name","showlegend":true},{"x":[null],"y":[null],"z":null,"name":"E","marker":{"color":"rgb(255,0,0)","size":7.22222222222222},"mode":"markers","type":"scatter","hoverinfo":"name","showlegend":true},{"x":[null],"y":[null],"z":null,"name":"C","marker":{"color":"rgb(255,0,0)","size":8.33333333333333},"mode":"markers","type":"scatter","hoverinfo":"name","showlegend":true},{"x":[null],"y":[null],"z":null,"name":"B","marker":{"color":"rgb(255,153,0)","size":15},"mode":"markers","type":"scatter","hoverinfo":"name","showlegend":true},{"x":[null],"y":[null],"z":null,"name":"D","marker":{"color":"rgb(255,153,0)","size":9.44444444444444},"mode":"markers","type":"scatter","hoverinfo":"name","showlegend":true}],"layout":{"xaxis":{"title":"","showgrid":false,"showticklabels":false,"zeroline":false,"hoverformat":".2f"},"yaxis":{"title":"","showgrid":false,"showticklabels":false,"zeroline":false,"hoverformat":".2f"},"margin":{"b":40,"l":60,"t":25,"r":10}},"base_url":"https://plot.ly"},"evals":[]}</script><!--/html_preserve-->

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

<!--html_preserve--><div id="htmlwidget-5625" style="width:672px;height:480px;" class="plotly"></div>
<script type="application/json" data-for="htmlwidget-5625">{"x":{"data":[{"x":[10.0141912824201,9.17614479664901],"y":[-1.33210913040514,-3.77931711996428],"z":[-9.84510344688357,-6.08785647847599],"name":"a","line":{"color":"rgb(255,0,0)","width":8},"mode":"lines","type":"scatter3d","hoverinfo":"none","showlegend":false},{"x":[-14.0640117632696,-8.342691532528],"y":[9.76829081087717,7.90679382175238],"z":[-7.20520213065542,-6.25420797616152],"name":"a","line":{"color":"rgb(255,0,0)","width":8},"mode":"lines","type":"scatter3d","hoverinfo":"none","showlegend":false},{"x":[-10.4867463317157,-8.342691532528],"y":[2.71241424226487,7.90679382175238],"z":[-9.22660384030341,-6.25420797616152],"name":"a","line":{"color":"rgb(255,0,0)","width":8},"mode":"lines","type":"scatter3d","hoverinfo":"none","showlegend":false},{"x":[-8.342691532528,-5.3754090194412],"y":[7.90679382175238,10.8329656563531],"z":[-6.25420797616152,-11.2017561042341],"name":"a","line":{"color":"rgb(255,0,0)","width":8},"mode":"lines","type":"scatter3d","hoverinfo":"none","showlegend":false},{"x":[-4.59726165886251,-8.342691532528],"y":[9.62058302372894,7.90679382175238],"z":[-1.82122512009542,-6.25420797616152],"name":"b","line":{"color":"rgb(0,255,255)","width":5},"mode":"lines","type":"scatter3d","hoverinfo":"none","showlegend":false},{"x":[-7.19616356815082,-8.342691532528],"y":[6.53797451328678,7.90679382175238],"z":[-0.420853260456697,-6.25420797616152],"name":"b","line":{"color":"rgb(0,255,255)","width":5},"mode":"lines","type":"scatter3d","hoverinfo":"none","showlegend":false},{"x":[-8.342691532528,-5.3754090194412],"y":[7.90679382175238,10.8329656563531],"z":[-6.25420797616152,-11.2017561042341],"name":"b","line":{"color":"rgb(0,255,255)","width":5},"mode":"lines","type":"scatter3d","hoverinfo":"none","showlegend":false},{"x":[-8.342691532528,-8.342691532528],"y":[7.90679382175238,7.90679382175238],"z":[-6.25420797616152,-6.25420797616152],"name":"b","line":{"color":"rgb(0,255,255)","width":5},"mode":"lines","type":"scatter3d","hoverinfo":"none","showlegend":false},{"x":[-4.59726165886251,-8.342691532528],"y":[9.62058302372894,7.90679382175238],"z":[-1.82122512009542,-6.25420797616152],"name":"b","line":{"color":"rgb(0,255,255)","width":5},"mode":"lines","type":"scatter3d","hoverinfo":"none","showlegend":false},{"x":[-4.59726165886251,-7.19616356815082],"y":[9.62058302372894,6.53797451328678],"z":[-1.82122512009542,-0.420853260456697],"name":"b","line":{"color":"rgb(0,255,255)","width":5},"mode":"lines","type":"scatter3d","hoverinfo":"none","showlegend":false},{"x":[-8.10860034343176],"y":[-12.7186832677738],"z":[-2.0313473566579],"name":"B","marker":{"color":"rgb(0,102,255)","size":12.7777777777778},"mode":"markers","type":"scatter3d","hoverinfo":"name","showlegend":false},{"x":[-4.59726165886251],"y":[9.62058302372894],"z":[-1.82122512009542],"name":"A","marker":{"color":"rgb(0,102,255)","size":6.11111111111111},"mode":"markers","type":"scatter3d","hoverinfo":"name","showlegend":false},{"x":[-14.0640117632696,-5.2830015647214],"y":[9.76829081087717,-5.72575525002453],"z":[-7.20520213065542,-21.9549245742133],"name":"C","marker":{"color":"rgb(204,0,255)","size":9.44444444444444},"mode":"markers","type":"scatter3d","hoverinfo":"name","showlegend":false},{"x":[-5.3754090194412],"y":[10.8329656563531],"z":[-11.2017561042341],"name":"E","marker":{"color":"rgb(255,0,0)","size":7.22222222222222},"mode":"markers","type":"scatter3d","hoverinfo":"name","showlegend":false},{"x":[-7.19616356815082],"y":[6.53797451328678],"z":[-0.420853260456697],"name":"C","marker":{"color":"rgb(255,0,0)","size":8.33333333333333},"mode":"markers","type":"scatter3d","hoverinfo":"name","showlegend":false},{"x":[9.17614479664901,-10.4867463317157],"y":[-3.77931711996428,2.71241424226487],"z":[-6.08785647847599,-9.22660384030341],"name":"B","marker":{"color":"rgb(255,153,0)","size":15},"mode":"markers","type":"scatter3d","hoverinfo":"name","showlegend":false},{"x":[-8.342691532528,10.0141912824201],"y":[7.90679382175238,-1.33210913040514],"z":[-6.25420797616152,-9.84510344688357],"name":"D","marker":{"color":"rgb(255,153,0)","size":9.44444444444444},"mode":"markers","type":"scatter3d","hoverinfo":"name","showlegend":false},{"x":[null],"y":[null],"z":[null],"name":"a","line":{"color":"rgb(255,0,0)","width":8},"mode":"lines","type":"scatter3d","hoverinfo":"none","showlegend":true},{"x":[null],"y":[null],"z":[null],"name":"b","line":{"color":"rgb(0,255,255)","width":5},"mode":"lines","type":"scatter3d","hoverinfo":"none","showlegend":true},{"x":[null],"y":[null],"z":[null],"name":"B","marker":{"color":"rgb(0,102,255)","size":12.7777777777778},"mode":"markers","type":"scatter3d","hoverinfo":"name","showlegend":true},{"x":[null],"y":[null],"z":[null],"name":"A","marker":{"color":"rgb(0,102,255)","size":6.11111111111111},"mode":"markers","type":"scatter3d","hoverinfo":"name","showlegend":true},{"x":[null],"y":[null],"z":[null],"name":"C","marker":{"color":"rgb(204,0,255)","size":9.44444444444444},"mode":"markers","type":"scatter3d","hoverinfo":"name","showlegend":true},{"x":[null],"y":[null],"z":[null],"name":"E","marker":{"color":"rgb(255,0,0)","size":7.22222222222222},"mode":"markers","type":"scatter3d","hoverinfo":"name","showlegend":true},{"x":[null],"y":[null],"z":[null],"name":"C","marker":{"color":"rgb(255,0,0)","size":8.33333333333333},"mode":"markers","type":"scatter3d","hoverinfo":"name","showlegend":true},{"x":[null],"y":[null],"z":[null],"name":"B","marker":{"color":"rgb(255,153,0)","size":15},"mode":"markers","type":"scatter3d","hoverinfo":"name","showlegend":true},{"x":[null],"y":[null],"z":[null],"name":"D","marker":{"color":"rgb(255,153,0)","size":9.44444444444444},"mode":"markers","type":"scatter3d","hoverinfo":"name","showlegend":true}],"layout":{"scene":{"showlegend":true,"yaxis":{"showgrid":false,"showticklabels":false,"zeroline":false,"title":""},"xaxis":{"showgrid":false,"showticklabels":false,"zeroline":false,"title":""},"zaxis":{"showgrid":false,"showticklabels":false,"zeroline":false,"title":""}},"margin":{"b":40,"l":60,"t":25,"r":10}},"base_url":"https://plot.ly"},"evals":[]}</script><!--/html_preserve-->


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
### updated 2016-02-27


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
## [17] DBI_0.3.1        htmltools_0.3    lazyeval_0.1.10  yaml_2.1.13     
## [21] assertthat_0.1   digest_0.6.9     gridExtra_2.0.0  formatR_1.2.1   
## [25] viridis_0.3.2    htmlwidgets_0.5  base64enc_0.1-3  evaluate_0.8    
## [29] rmarkdown_0.9    stringi_1.0-1    scales_0.3.0     jsonlite_0.9.19
```
