shinyServer(function(input, output, session) {

  values<-reactiveValues()

  #layout options
  get_layouts<-reactive({
    shiny::validate(need(!is.null(input$type)==TRUE,"Loading..."))
    if(input$type=="2d"){
      c("adj", "circle","circrand","eigen",
        "fruchtermanreingold","geodist","hall",
        "kamadakawai","mds","princoord","random",
        "rmds","segeo","seham","spring",
        "springrepulse","target")
    } else {
      c("adj",
        "eigen",
        "fruchtermanreingold",
        "geodist",
        "hall",
        "kamadakawai",
        "mds",
        "princoord",
        "random",
        "rmds",
        "segeo",
        "seham")
    }

  })

  #create controls
  output$controls<-renderUI({
    fluidRow(column(width=11,offset=1,
                    sliderInput("nodes","Nodes",2,100,value = 10, step=1),
                    sliderInput("edge_type","Types of edges",1,20,value = 2, step=1),
                    sliderInput("conn","Connections",1,10,value = 2, step=1),
                    radioButtons("type","Network",choices=c('2D'="2d",'3D'="3d"),inline=TRUE),
                    selectInput("layout","Layout", choices="fruchtermanreingold",selected="fruchtermanreingold",multiple = FALSE)
    ))
  })

  observeEvent(input$type,{
    if(is.null(input$type)||is.null(input$layout)) return()
    updateSelectInput(session,"layout",choices=get_layouts(),selected="fruchtermanreingold")
  })

  create_network<-reactive({
    shiny::validate( need(!is.null(input$edge_type) == TRUE,"Loading..."))
    shiny::validate( need(!is.null(input$type) == TRUE,"Loading..."))

    conn<-input$conn
    nodes<-input$nodes
    net_size<-conn*nodes
    edge_type<-input$edge_type
    layout<-input$layout

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

    #plotly_empty()
    #network_ly(edge.list,node.data,type=input$type)
    type<-input$type
    color<-'color'
    size<-'size'
    name<-'names'
    obj<-get_network(edge.list,type=type,layout=layout)
    net<-c(get_edges(obj,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=FALSE),get_nodes(obj,node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=FALSE))


    legend<-format_legend(obj,node.data=node.data)

    net<-c(net,c(get_edges(legend,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=TRUE),get_nodes(legend,node.data=legend$node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=TRUE)))

    #to view
    values$net_object<-net

    net<-shiny_ly(net)

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
  })

  output$network<-renderPlotly({
    shiny::validate( need(!is.null(input$edge_type) == TRUE,"Loading..."))
    shiny::validate( need(!is.null(input$type) == TRUE,"Loading..."))

    create_network()

  })

  output$network_UI<-renderUI({
    box(title = "network", status = "info", solidHeader = TRUE, width=12,height='500px',
        plotlyOutput("network")
    )
  })

#   #get network data
  get_network_data<-reactive({
    return(values$net_object)
  })

  #object sent to plotly
  output$obj<-renderPrint({
      toJSON(get_network_data(),auto_unbox = TRUE,pretty = TRUE)
  })

  output$obj_UI<-renderUI({
    box(verbatimTextOutput("obj"),title = "JSON", status = "info", solidHeader = TRUE,width=12)
  })

})
