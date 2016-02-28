#save widgets

library(plotly)
library(networkly)

#setup
conn<-1 # average number of conenctions per variable
nodes<-10 # number of variables
net_size<-conn*nodes
edge_type<-2 # number of diffrent connections
layout<-"fruchtermanreingold"

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


#net params
type<-"2d"
color<-'color'
size<-'size'
name<-'names'
obj<-get_network(edge.list,type=type,layout=layout)
net<-c(get_edges(obj,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=FALSE),get_nodes(obj,node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=FALSE))


legend<-format_legend(obj,node.data=node.data)

net2<-c(net,c(get_edges(legend,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=TRUE),get_nodes(legend,node.data=legend$node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=TRUE)))


net<-shiny_ly(net) # works in or out of shiny


#2D
p<-layout(net,
         xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, hoverformat = '.2f'),
         yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, hoverformat = '.2f'))
#save
library(htmlwidgets)
setwd("inst/www/html/")
saveWidget(as.widget(p), file='2dnetwork.html')

#3D
#net params
type<-"3d"
color<-'color'
size<-'size'
name<-'names'
obj<-get_network(edge.list,type=type,layout=layout)
net<-c(get_edges(obj,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=FALSE),get_nodes(obj,node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=FALSE))

p<-layout(net,
         scene = list(showlegend=TRUE,
                      yaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title=""),
                      xaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title=""),
                      zaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title="")))
#save
saveWidget(as.widget(p), file='3dnetwork.html')
