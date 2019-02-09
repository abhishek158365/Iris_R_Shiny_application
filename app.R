
library(shiny)
library(ggplot2)
library(shinydashboard)
library(caret)
library(randomForest)
library(kernlab)

d<-iris
set.seed(7)
testid<-createDataPartition(d$Species,times=1,p=0.75,list=TRUE)[[1]]
traindata<-d[testid,]
testdata<-d[-testid,]

ui <-dashboardPage(
  dashboardHeader(title = "Iris"),

  dashboardSidebar
  (
    selectInput("Xaxis","X axis",names(d),selected=names(d)[[1]]),
    selectInput("Yaxis","Y axis",names(d),selected=names(d)[[2]]),
    selectInput("Color","Color",names(d),selected=names(d)[[5]]),
    br(),br(),
    checkboxGroupInput("Model_params","Model Parameters",names(d)[1:(ncol(d)-1)],selected=names(d)[1:(ncol(d)-1)]),
    selectInput("HP_method","Hyperparameter Search method",c("grid","random"),selected="random"),
    selectInput("model","Model Family",c("rf","svmRadial"),selected="rf")
  ),
  
  dashboardBody
  ( 
    tabsetPanel(type = "tabs",
                tabPanel("Basic Plot", 
                         br(),
                         plotOutput("plotbasic")),
                
                tabPanel("Classification Model",br(),br(),
                         fluidRow(valueBoxOutput("TSMAPE"),
                                  valueBoxOutput("prv"),
                                  valueBoxOutput("rev"),
                                  valueBoxOutput("f1v"))
                                  ,br(),br(),
                         
                         fluidRow(box(title=("Hyperparameter Tuning"),
                                      width=10,background = "blue",solidHeader = TRUE),
                                  box(downloadButton("hpData", "Download"),width=2)),
                         dataTableOutput("model"),br(),br(),
                         
                         fluidRow(box(title=("All Class Precision Recall"),
                                      width=10,background = "blue",solidHeader = TRUE),
                                  box(downloadButton("prData", "Download"),width=2)),
                         dataTableOutput("mTable")
                        
                         ),
                tabPanel("Plots",uiOutput("plots")),
                
                tags$style("#TSMAPE {width:25%;}"),
                tags$style("#prv {width:25%;}"),
                tags$style("#rev {width:25%;}"),
                tags$style("#f1v {width:25%;}")
                
                
    
  )
)
)


server <- function(input, output) {
   observe
  (
    {
     output$plotbasic<-renderPlot(ggplot(data=d,aes(x=d[,input$Xaxis],y=d[,input$Yaxis],color=d[,input$Color]))+geom_point()+labs(x=input$Xaxis,y=input$Yaxis))
     tdata<-reactive({traindata[,colnames(traindata)%in%c(input$Model_params,"Species")]})
     model_ctrl<-reactive({trainControl(method="repeatedcv", number=10, repeats=3, search=input$HP_method)})
     modelA<-reactive({train(Species~., data=tdata(), method=input$model, metric="Accuracy", tuneLength=15, trControl=model_ctrl())})
     output$model<-renderDataTable(modelA()$results)
     
     res<-reactive({as.matrix(table(testdata$Species,predict(modelA(),testdata)))})
     Acc<-reactive({sum(diag(res()))/sum(res())})
     csum<-reactive({apply(res(),2,sum)})
     rsum<-reactive({apply(res(),1,sum)})
     pr<-reactive({diag(res())/csum()})
     re<-reactive({diag(res())/rsum()})
     f1 = reactive({2 * pr() * re() / (pr() + re())}) 
     
     output$mTable<-renderDataTable(data.frame(Class=row.names(res()),precision=pr(),recall=re(),F1_score=f1()))
     
     output$TSMAPE<-renderValueBox(valueBox(value =tags$p(round(Acc()*100,2),style = "font-size: 60%;"),subtitle="Test Accuracy",color="aqua",width=3))
     output$prv<-renderValueBox(valueBox(value =tags$p(round(mean(pr())*100,2),style = "font-size: 60%;"),subtitle="Average Precision",color="aqua",width=3))
     output$rev<-renderValueBox(valueBox(value =tags$p(round(mean(re())*100,2),style = "font-size: 60%;"),subtitle="Average Recall",color="aqua",width=3))
     output$f1v<-renderValueBox(valueBox(value =tags$p(round(mean(f1())*100,2),style = "font-size: 60%;"),subtitle="Average F1 Score",color="aqua",width=3))
     
     output$hpData <- downloadHandler(filename = function() {"hpData.csv"},content = function(file) {write.csv(modelA()$results, file, row.names = FALSE)})
     
     output$prData <- downloadHandler(filename = function() {"Precision_recall.csv"},content = function(file) {write.csv(data.frame(Class=row.names(res()),precision=pr(),recall=re(),F1_score=f1()), file, row.names = FALSE)})
     
     output$plots <- renderUI({
       plot_output_list <- lapply(1:length(input$Model_params), function(i) {
         plotname<-paste0("plot",i)
         fluidRow(box(plotOutput(plotname),width=12))
       })
       do.call(tagList, plot_output_list)
     })
     
     observe({
       for(i in 1:length(input$Model_params))
       {
         local({
           l_i<-i;
           Metric=input$Model_params[l_i]
           output[[paste0("plot",l_i)]]<-renderPlot(ggplot(data = d, aes(x = Species, y = d[,Metric]))+geom_line()+geom_boxplot()+labs(y=Metric,title=paste0(Metric," vs Species")))
         
         })
       }})

    }
  )
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)

