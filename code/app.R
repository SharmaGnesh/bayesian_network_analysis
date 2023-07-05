suppressMessages(library(readxl))
suppressMessages(library(shiny))
suppressMessages(library(plotly))
suppressMessages(library(here))
suppressMessages(library(lme4))
suppressMessages(library(readxl))
suppressMessages(library(reshape2))
suppressMessages(library(ggthemes))
suppressMessages(library(dplyr))
suppressMessages(library(randomForest))
suppressMessages(library(dplyr))
suppressMessages(library(shiny))
suppressMessages(library(plotly))
suppressMessages(library(rsconnect))
suppressMessages(library(here))

data <- read_excel(here("retail_data.xlsx"))
data <- data[,-1] #removing the ID column
data <- data |>
  mutate(across(where(is.character),as.factor)) # converting character vectors to factor


data <- data |>
  mutate(Find= recode_factor(Find,
                             `1`="Unlikely extremely", `2`="Unlikely quite", `3`="Unlikley slightly", 
                             `4`="neither", `5`= "Likely slightly", `6`= "Likely quite", 
                             `7` = "Likely extremely"),
         Conven= recode_factor(Conven, 
                               `1`="neither", `2`="Convenient slightly", `3`="Convenient quite", 
                               `4`="Convenient extremely"),
         Suited= recode_factor(Suited, 
                               `1`="neither", `2`="Suited slightly", `3`="Suited quite", 
                               `4`="Suited extremely"),
         Browse= recode_factor(Browse,
                               `1`="Once a year", `2`="Once every few month", `3`="Once a month", 
                               `4`="More than once a month", `5`= "Once a week", `6`= "More than once a week", 
                               `7` = "Daily")
         
  )

data$Find <- as.numeric(data$Find)
data$Conven <- as.numeric(data$Conven)
data$Suited <- as.numeric(data$Suited)
data$Browse <- as.numeric(data$Browse)

data_numeric <- data |> 
  select(where(is.numeric))
correlation <- round(cor(data_numeric), 3)


# function for categorical and mixed variables
show_assoc <- function(d, x, y, by = NULL){
  
  
  
  if ( is.numeric(d[[x]]) & is.numeric(d[[y]]) ) {
    p <- ggplot2::ggplot(data=d, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
      ggplot2::geom_point() +
      ggplot2::xlab(x) +
      ggplot2::ylab(y) +
      {if(!is.null(by)) ggplot2::facet_wrap(~.data[[by]])}
    
  } else if ( is.factor(d[[x]]) & is.factor(d[[y]]) ) {
    d["x"] <- d[x] ; d["y"] <- d[y]
    
    p <- ggplot2::ggplot(data=d) +
      #ggmosaic::geom_mosaic(ggplot2::aes(x = ggmosaic::product(y, x) )) +
      ggplot2::geom_bar(ggplot2::aes(x=.data[[x]], fill = .data[[y]]), position = "dodge") +
      ggplot2::xlab(x) +
      #ggplot2::ylab(y) +
      {if(!is.null(by)) ggplot2::facet_wrap(~.data[[by]])}
    
  } else {
    
    fact <- names(dplyr::select_if(d[,c(x,y)], is.factor))
    num <- names(dplyr::select_if(d[,c(x,y)], is.numeric))
    
    
    p <- ggplot2::ggplot(data=d) +
      ggplot2::geom_boxplot(ggplot2::aes(x =.data[[fact]], y =.data[[num]],fill=.data[[fact]]) ) + ggplot2::xlab(fact) +
      ggplot2::ylab(num) + {if(!is.null(by)) ggplot2::facet_wrap(~.data[[by]])}
  }
  
  p
  
}


# Predictive model (Random forest) to find variables affecting profit percentage

set.seed(2019)
test_size = floor(0.3 * nrow(data))
samp = sample(nrow(data), test_size,replace = FALSE)
y_train = data[-samp,19]
x_train = data[-samp,-19]
y_test= data[samp,19]
x_test = data[samp,-19] #since the first column is just ID

train = cbind(y_train,x_train)
test = cbind(y_test,x_test)

colnames(train)[1] = "response"
colnames(test)[1] = "response"

mtry = sqrt(18)
model_1 = randomForest(response~., data = train, importance = TRUE)

pred_1 = predict(model_1, x_test)

accuracy_m1 = mean(y_test == pred_1)

importance = importance(model_1)
varImportance = data.frame(Variables = row.names(importance),
                           Importance =round(importance[, "%IncMSE"],2))
rankImportance=varImportance|>
  mutate(Rank=paste("#",dense_rank(desc(Importance))))


# shiny app
ui <- fluidPage(
  fluidRow(
    column(6,plotlyOutput("heat")),
    column(6,plotlyOutput("scatterplot"))),
  br(),br(),
  fluidRow(
    column(3,
           selectInput("var1","Factor variable",choices = colnames(data |> 
                                                                     select(where(is.factor)))),
           selectInput("var2","Numeric/Factor variable",choices = colnames(data)),
           selectInput("var3","Factor variable for conditioning",
                       choices = c(colnames(data |>
                                              select(where(is.factor))),"none"),
                       selected = "none"
           ),
           
           actionButton("action_1","Apply changes!")),
    column(9,plotOutput("non_numeric_plot"))
  ),
  br(),
  fluidRow(
    column(9,"Variable importance using Random forest model",plotOutput("imp_plot"))
  )
)

server <- function(input, output, session) {
  
  output$heat <- renderPlotly({
    
    plot_ly(source = "heat_plot") |>
      add_heatmap(
        x = names(data_numeric), 
        y = names(data_numeric), 
        z = correlation,
        zmin=-1,zmax=1, colorscale = "RdBu"
      )
  })
  
  output$scatterplot <- renderPlotly({
    
    clickData <- event_data("plotly_click", source = "heat_plot")
    if (is.null(clickData)) return(NULL)
    
    vars <- c(clickData[["x"]], clickData[["y"]])
    d <- setNames(data_numeric[vars], c("x", "y"))
    yhat <- fitted(lm(y ~ x, data = d))
    
    plot_ly(d, x = ~x) %>%
      add_markers(y = ~y) %>%
      add_lines(y = ~yhat) %>%
      layout(
        xaxis = list(title = clickData[["x"]]), 
        yaxis = list(title = clickData[["y"]]), 
        showlegend = FALSE
      )
  })
  
  a<-eventReactive(input$action_1,ignoreInit = TRUE,{
    x <- input$var1; y <- input$var2 
    by <- if(input$var3=="none"){NULL}else{input$var3}
    show_assoc(data,x,y,by)
  })
  
  output$non_numeric_plot<-renderPlot({a()})
  
  output$imp_plot <- renderPlot(
    ggplot(rankImportance,aes(x=reorder(Variables,Importance),
                              y=Importance,fill=Importance)) + 
      geom_bar(stat="identity") + 
      geom_text(aes(x = Variables, y = 0.5, label = Rank),
                hjust=0, vjust=0.55, size = 4, colour = "white") +
      labs(x = "Variables") +
      coord_flip() + 
      theme_classic()
  )
  
}


shinyApp(ui, server)