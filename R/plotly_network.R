


#' @title network_ly
#' @import dplyr plotly
#' @export
network_ly<-function(edge.list,node.data,color="color",size="size",name="names",type="2d"){

  obj<-get_network(edge.list,type=type)
  net<-c(get_edges(obj,color=color,size=size,name=name,type=type),get_nodes(obj,node.data,color=color,size=size,name=name,type=type)) %>%
    plotly_build(.) %>% list_ggplotly(.)
  if(type=="2d"){
    layout(net,
           xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, hoverformat = '.2f'),
           yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, hoverformat = '.2f'))
  } else{
    layout(net,
           scene = list(showlegend=TRUE,
                        yaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title=""),
                        xaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title=""),
                        zaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title="")))
  }
}


#' @title get_edges
#' @export
get_edges<-function(obj,color="color",width="size",name="names",type="2d",...){

  #set net opts
  if(type == "2d"){
    opts<-list(mode="lines",type="scatter",...)
  } else {
    opts<-list(mode="lines",type="scatter3d",...)
  }
  #split list for element mapping (could be done together?)
  # mappings<-obj$edges[,c(color,width),drop=FALSE]
  segs<-rep(1:(nrow(obj$edges)/2),each=2)
  # if(!is.null(mappings)){
  #   id<-lapply(1:nrow(mappings),function(i){
  #     paste(paste(mappings[i,],collapse="_"),segs[i])
  #   })
  #   id<-unlist(id)
  #   el<-split(obj$edges,id)
  #
  # } else {
    el<-split(obj$edges,segs) # split all
  # }

  #need to split into induvidual segments, for 3d and size maping to work


  #generate elements
  res<-list()
  for (i in 1:length(el)){
    tmp<-el[[i]]
    res[[i]]<-c(list(x=tmp$x,
                     y=tmp$y,
                     z=tmp$z,
                     name = tryCatch(tmp[,name][1],error=function(e){NULL}), # TODO: fix hack
                     line=list(
                       color = ifelse(is.null(rgb_col(tmp[,color])[1]),rgb_col("gray"),rgb_col(tmp[,color])[1]),
                       width = tryCatch(as.numeric(as.character(tmp[,width][1])),error=function(e){5})) # why is this a factor?
    ),opts)
  }
  return(res)
}

#' @title get_nodes
#' @export
#' @import dplyr
get_nodes<-function(obj,node.data=NULL,color="color",size="size",name="names",type="2d",merge=TRUE,...){
  if(!is.null(node.data) & merge){
    #merge with nodes based on rowname
    node.data<-cbind(obj$nodes,node.data[rownames(obj$nodes),])

  }
  #set net opts
  if(type == "2d"){
    opts<-list(mode="markers",type="scatter",...)
  } else {
    opts<-list(mode="markers",type="scatter3d",...)
  }
  #split list for element mapping (could be done together?)
  mappings<-node.data[,c(color,size,name),drop=FALSE]

  if(!is.null(mappings)){
    id<-lapply(1:nrow(mappings),function(i){
      paste(mappings[i,],collapse="_")
    })
    id<-unlist(id)
    node.data<-split(node.data,id)
  } else {
    node.data<-list(node.data)
  }

  #generate elements
  res<-list()
  for (i in 1:length(node.data)){
    tmp<-node.data[[i]]
    res[[i]]<-c(list(x=tmp$x,
                     y=tmp$y,
                     z=tmp$z,
                     name = unique(tmp[,name]),
                     marker=list(
                       color = unique(rgb_col(tmp[,color])),
                       size = unique(tmp[,size]))

    ),opts)
  }
  return(res)
}



#' @title get_text
#' @export
get_text<-function(obj,node.data,text='names',extra=NULL,type="2d",xoff=0,yoff=0,zoff=0,...){
  if(!is.null(node.data)){
    #merge with nodes based on rowname
    node.data<-cbind(obj$nodes,node.data[rownames(obj$nodes),])
  }
  #set net opts
  if(type == "2d"){
    opts<-list(mode="text",type="scatter",...)
  } else {
    opts<-list(mode="text",type="scatter3d",...)
  }

  #split list for element mapping (could be done together?)
  res<-c(list(x=node.data$x + get_unit(node.data$x, xoff),
              y=node.data$y + get_unit(node.data$y, yoff),
              z=node.data$z + get_unit(node.data$z, zoff),
              text = node.data[,text])
         ,opts,extra)

  return(list(res))
}


#' @title make_edges
#' @export
make_edges<-function(whichRow,layout,adjacency,type="2d"){
  fromC <- layout[adjacency[whichRow, 1], ]
  toC <- layout[adjacency[whichRow, 2], ]
  if(type == "2d") {
    edge <- data.frame(c(fromC[1], toC[1]), c(fromC[2] ,toC[2]))
    colnames(edge)<-c("x","y")
  } else {
    edge <- data.frame(c(fromC[1], toC[1]), c(fromC[2] ,toC[2]), c(fromC[3] ,toC[3]))
    colnames(edge)<-c("x","y","z")
  }

  if(ncol(adjacency)>2){
    tmp<-data.frame(matrix(as.matrix(adjacency[whichRow, -c(1,2),drop=FALSE]),nrow = nrow(edge), ncol=ncol(adjacency)-2, byrow=TRUE))
    colnames(tmp)<-colnames(adjacency)[-c(1:2)]
    edge$extra<-tmp
    edge<-do.call("cbind",edge)
    colnames(edge)<-gsub("extra.","",colnames(edge))
  }
  edge$fromID<-adjacency[whichRow, 1]
  edge$toID<-adjacency[whichRow, 2]
  return(edge)
}

#' @title get_network
#' @export
#' @import network sna
get_network<-function(edge.list,type="2d",layout="fruchtermanreingold", layout.par=NULL){

  #source target assumed first
  rev.edge.list<-edge.list[,1:2,drop=FALSE]

  #extra info (separate now, later recombine)
  info<-edge.list[,-c(1:2),drop=FALSE]

  #getting layout and making sure edge list ids are in the same order
  g<-as.network(rev.edge.list[,1:2],matrix.type = "edgelist") #
  if (type== "2d") {
    layout<-switch(layout,
                   "adj" = gplot.layout.adj(g[,], layout.par),
                   "circle" =  gplot.layout.circle(g[,], layout.par),
                   "circrand" =  gplot.layout.circrand(g[,], layout.par),
                   "eigen" =  gplot.layout.eigen(g[,], layout.par),
                   "fruchtermanreingold" =  gplot.layout.fruchtermanreingold(g[,], layout.par),
                   "geodist" =  gplot.layout.geodist(g[,], layout.par),
                   "hall" =  gplot.layout.hall(g[,], layout.par),
                   "kamadakawai" =  gplot.layout.kamadakawai(g[,], layout.par),
                   "mds" =  gplot.layout.mds(g[,], layout.par),
                   "princoord" =  gplot.layout.princoord(g[,], layout.par),
                   "random" =  gplot.layout.random(g[,], layout.par),
                   "rmds" =  gplot.layout.rmds(g[,], layout.par),
                   "segeo" =  gplot.layout.segeo(g[,], layout.par),
                   "seham" =  gplot.layout.seham(g[,], layout.par),
                   "spring" =  gplot.layout.spring(g[,], layout.par),
                   "springrepulse" =  gplot.layout.springrepulse(g[,], layout.par),
                   "target" =  gplot.layout.target(g[,], layout.par)
    )

    #back to edgelist
    n.edge.list<-as.matrix.network.edgelist(g)
    #match layout with nodes
    dimnames(layout)<-list(rownames(g[,]),c("x","y"))

  } else {
    layout<-switch(layout,
                   "adj" =  gplot3d.layout.adj(g[,], layout.par),
                   "eigen" =  gplot3d.layout.eigen(g[,], layout.par),
                   "fruchtermanreingold" = gplot3d.layout.fruchtermanreingold(g[,], layout.par),
                   "geodist" = gplot3d.layout.geodist(g[,], layout.par),
                   "hall" = gplot3d.layout.hall(g[,], layout.par),
                   "kamadakawai" = gplot3d.layout.kamadakawai(g[,], layout.par),
                   "mds" = gplot3d.layout.mds(g[,], layout.par),
                   "princoord" = gplot3d.layout.princoord(g[,], layout.par),
                   "random" = gplot3d.layout.random(g[,], layout.par),
                   "rmds" = gplot3d.layout.rmds(g[,], layout.par),
                   "segeo" = gplot3d.layout.segeo(g[,], layout.par),
                   "seham" = gplot3d.layout.seham(g[,], layout.par)
    )

    #back to edgelist
    n.edge.list<-as.matrix.network.edgelist(g)
    #match layout with nodes
    dimnames(layout)<-list(rownames(g[,]),c("x","y","z"))
  }


  #preparing for edge path
  adjacency<-data.frame(n.edge.list,info)

  #create edge positions
  #TODO make parallel
  allEdges <- lapply(1:nrow(adjacency),function(i){
    make_edges(i,layout,adjacency,type=type)
  })
  list(nodes = data.frame(layout), edges=data.frame(do.call(rbind, allEdges)))
}

#' @title format_legend
#' @export
#' @import dplyr
format_legend<-function(obj,edges=TRUE,nodes=TRUE,width='size',color='color',size='size',name='names',node.data){

  res<-list()

  #edges
  if(edges){
    el<-obj$edges
    vars<-c(width,color,name)
    id<-el %>% select(one_of(vars)) %>% apply(.,1,paste,collapse="|")
    tmp<-el[!duplicated(id),]
    tmp$x<-tmp$y<-tmp$z<-NA
    res$edges<-tmp
  }
  #nodes
  if(nodes){
    el<-cbind(obj$nodes,node.data)
    vars<-c(size,color,name)
    id<-el %>% select(one_of(vars)) %>% apply(.,1,paste,collapse="|")
    tmp<-el[!duplicated(id),]
    tmp$x<-tmp$y<-tmp$z<-NA
    res$nodes<-tmp[,c("x","y","z")]
    res$node.data<-tmp
  }
  return(res)
}



#' @title list_ggplotly
#' @import plotly
list_ggplotly<-function (l, filename, fileopt, world_readable = TRUE)
{
  if (!missing(filename))
    l$filename <- filename
  if (!missing(fileopt))
    l$fileopt <- fileopt
  l$world_readable <- world_readable
  plotly:::hash_plot(df=NULL, l)
}

#' @title add_struct
#' @import plotly
add_struct<-function(obj){
  l <- list(data = obj)
  structure(plotly:::add_boxed(plotly:::rm_asis(l)), class = "plotly")
}



#' @title shiny_ly
#' @import plotly
#' @export
shiny_ly<-function(obj){
  net <- structure(obj, class = unique(c("plotly_hash", class(obj))))
  net <- plotly:::plotly_build(net)
  plotly:::hash_plot(df = NULL, net)
}

#accesory fxns

#' @title rgb_col
#' @import grDevices
rgb_col<-function(col){
  lapply(col,function(x){
    rgb<-col2rgb(x, alpha = FALSE)[,1]
    paste0('rgb(',paste(rgb,collapse=","),")",collapse="")
  }) %>% unlist()
}

#' @title line_brk
line_brk<-function(obj){
  paste0(lapply(obj,function(x) {paste(x,collapse="")}),collapse="<br>")
}


#' @title get_unit
get_unit<-function(x,prct=10){
  rng<-diff(range(x))*(prct/100)
}


#' @title test
test<-function(){
library(plotly)
library(networkly)

conn<-1
nodes<-10
net_size<-conn*nodes
edge_type<-2
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
                      size=sample(seq(10,50,length.out=10),nodes,replace=TRUE)[id2],
                      names=sample(LETTERS[1:5],nodes,replace=TRUE)[id2],stringsAsFactors = FALSE)



#net params
type<-"2d"
color<-'color'
size<-'size'
name<-'names'
obj<-get_network(edge.list,type=type,layout=layout)

net<-c(get_edges(obj,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=FALSE),
       get_nodes(obj,node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=FALSE),
       get_text(obj,node.data,text=name,extra=list(textfont=list(size=40)),type=type,yoff=-10,hoverinfo="none",showlegend=FALSE))



legend<-format_legend(obj,node.data=node.data)

net2<-c(net,c(get_edges(legend,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=TRUE),get_nodes(legend,node.data=legend$node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=TRUE)))


net<-shiny_ly(net) # works in or out of shiny
net<-shiny_ly(net2) # with legend?

if(type=="2d"){
  layout(net,
         xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, hoverformat = '.2f'),
         yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, hoverformat = '.2f'))

} else{
  layout(net,
         scene = list(showlegend=TRUE,
                      yaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title=""),
                      xaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title=""),
                      zaxis=list(showgrid=FALSE,showticklabels=FALSE,zeroline=FALSE,title="")))
}

}

