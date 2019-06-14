#' Generate shiny interface for data visualization and exploration
#'
#' This function generate an interactive shiny application for visualizing and exploring treatment effect modeled by GRF.
#' @param object t-Sne clustering object that has been created.
#' @export
#' @examples
#' create.shiny(tsne_obj$result)

create.shiny <- function(tsne_dt){
  library(shiny)
  library(data.table)
  #library(plotly)

  numVar <-ncol(tsne_dt) - 5 #extract feature num
  nameVar <- colnames(tsne_dt)[6:ncol(tsne_dt)] #extract feature name
  numeric_list <-  unlist(lapply(tsne_dt[,6:ncol(tsne_dt)],is.numeric)) #extract numeric feature

  ui <- fluidPage(

    headerPanel("t-SNE Clustering"),

    sidebarPanel(
      selectInput("colselect", "Feature to Display",
                  colnames(tsne_dt)[6:length(colnames(tsne_dt))],
                  multiple = TRUE,
                  selected = colnames(tsne_dt)[6:8]),
      sliderInput('tau', "Treatment Effect", min = round(min(tsne_dt$tau),3), max = round(max(tsne_dt$tau),3), value = c(round(min(tsne_dt$tau),3),round(max(tsne_dt$tau),3))),

      uiOutput("selection")

    ),
    mainPanel(
      h3(textOutput("numObs")),
      plotlyOutput('tsnePlot')

    )

  )


  server <- function(input, output,session) {
    l <- list()
    for(i in 1:numVar){
      if(numeric_list[[i]]){
        #create slider input for numeric
        l[[names(numeric_list[i])]] <- sliderInput(names(numeric_list[i]), names(numeric_list[i]), min = min(tsne_dt[,5+i]), max = max(tsne_dt[,5+i]),value = c(min(tsne_dt[,5+i]),max(tsne_dt[,5+i])));
      }else{
        #create check box for catagorical
        l[[names(numeric_list[i])]] <- checkboxGroupInput(names(numeric_list[i]), names(numeric_list[i]), choices = levels(tsne_dt[,5+i]),selected = levels(tsne_dt[,5+i]));
      }
    }

    output$selection <- renderUI({
      boo <- (nameVar %in% input$colselect)
      l <- lapply(1:numVar,function(i){
        if(boo[i]) l[[names(numeric_list[i])]] else NULL
      })
      l
    }
    )

    output$numObs <- renderText({
      n <- dim(dataset())[1]
      sprintf("Found %i observations under this criteria", n)
    })


    dataset <- reactive({

      x <- lapply(1:numVar,function(i) {
        if(is.null(input[[names(numeric_list[i])]])){
          rep(TRUE,length(tsne_dt[[names(numeric_list[i])]]))
        }else{
          if(numeric_list[[i]]){
            tsne_dt[[names(numeric_list[i])]] %between% input[[names(numeric_list[i])]]
          }else{
            tsne_dt[[names(numeric_list[i])]] %in% input[[names(numeric_list[i])]]
          }
        }
      })
      boo <- apply(matrix(unlist(x), nrow = numVar, byrow=T),2,all)
      tsne_dt %>% dplyr::filter(tau %between% input$tau,boo)
    })


    output$tsnePlot <- renderPlotly({
      if(dim(dataset())[1] == 0) {
        g <- ggplot2::ggplot(data.frame()) + geom_point() +
          theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank()) +
          xlim(range(tsne_dt$X)[1]-1,range(tsne_dt$X)[2]+1) +
          ylim(range(tsne_dt$Y)[1]-1,range(tsne_dt$Y)[2]+1)
      }
      else{
        dt <- dataset()
        #custom text output
        txt <- sapply(1:nrow(dt),function(x){
          str <- ""
          for(i in 1:numVar){
            str <- paste(str,'</br>',nameVar[i],":",dt[[nameVar[i]]][x])
          }
          str})
        g <- ggplot2::ggplot(dt,aes(x = X, y = Y,color= Level,alpha = abs(z),
                           text = txt)) +
          scale_colour_manual(values = cols,breaks = c("positive p < 0.05" , "positive 0.05 < p < 0.1" ,"p > 0.1", "negative 0.05 < p < 0.1","negative p < 0.05"))+guides(alpha = F) +
          geom_point() +
          theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank()) +
          xlim(range(tsne_dt$X)[1]-1,range(tsne_dt$X)[2]+1) +
          ylim(range(tsne_dt$Y)[1]-1,range(tsne_dt$Y)[2]+1)
      }
      plotly::ggplotly(g,tooltip = "text")
    })
  }

  shinyApp(ui, server)
}
