
# reference link:
# See this site for many more things that can be done with the data
# and for describing the metadata in the analyses I do
# https://www.kaggle.com/miroslavsabo/young-people-survey

#library(kohonen)
#library(fmsb)


responses <- read_csv("analyses/cluster_analysis/data/responses.csv")
legend <- read_csv("analyses/cluster_analysis/data/columns.csv")



# FUNCTION TO PLOT CONVEX HULL:
Plot_ConvexHull<-function(xcoord, ycoord, lcolor, grouplab){
  hpts <- chull(x = xcoord, y = ycoord)
  hpts <- c(hpts, hpts[1])
  lines(xcoord[hpts], ycoord[hpts], col = lcolor, lwd=2)
  text(xcoord[1],ycoord[1],grouplab, cex=2.2, col=lcolor, font=2)
  text(xcoord[1],ycoord[1],grouplab, cex=2.2, col="black")
}

myNorm <- function(x){
  (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))
}

###### Notes: ######
# perhaps better than the PCA plot which can be missleading, create a radial spiderweb plot
# for each grouping which suggests the inclinations of each group on multiple major axes of
# variation based on the PCA axes. For example, one axis might represent artistic inclinations
# while anothers might represent vanity or academic interests, etc. To do this, give each 
# variable a category and weight and then replot results. Or just plot the importance of variation
# on the PCA for distinguishing that category from the others?

ui <- fluidPage(
  
  tags$head(tags$style("#seedvalue{color: red;font-size: 20px; font-style: bold;}")),
  #tags$head(tags$style(".titles{color: red;font-size: 20px; font-style: bold;}")),
  
  headerPanel("k-means clustering of Slovakia's Youth"),

  wellPanel(
      fluidRow(
        fluidRow(
          column(width=4,
                 textInput(inputId="seed", label="Set Seed", value="20") #input the seed value
          ),
          column(width=6,
                 selectInput('comp', 'Aggregate Comparison', names(young.comp), selected = names(young.comp)[47])
          )
        ),
        fluidRow(
          column(width=2,actionButton("setseedButton", h4("Run Analysis"))), #button to update seed value
          column(width=2,textOutput(outputId="seedvalue")), #output seed value text
          column(width=6, h4("Directions:"),"Select a variable with which to compare groups. 
                 Most variables range from 1-5, with 5 being greatest support 
                 for the given characteristic or interest. Also, click on the tabs below to 
                 view key traits or top variables that describe each group.")
        ),
        fluidRow(
          column(width=4,"In 2013, students of the Statistics class at FSEV UK were asked to 
                 invite their friends to participate in this survey. The 
                 survey consisted of questions related to everything from 
                 fears, to hobbies, spending habits, and personality traits.
                 After removing missing values, a k-means cluster analysis 
                 was conducted to identify major groupings of youth."),
          column(width=4, offset=1,tableOutput("tableagg"))
        )
      )
    #numericInput('clusters', 'Cluster count', 3,min = 1, max = 6),
    #actionButton("testButton","Test"),
  ),
  
  tabsetPanel(
    tabPanel(
      h4("Key Traits"),
      fluidRow(style='padding:20px;',
               column(width=3,plotOutput('plot3',width=200, height=200)),
               column(width=3,plotOutput('plot4',width=200, height=200)),
               column(width=3,plotOutput('plot5',width=200, height=200)),
               column(width=3,plotOutput('plot6',width=200, height=200)),
               column(width=3,plotOutput('plot7',width=200, height=200))
      )
    ),
    tabPanel(
      h4("Top Characteristics"),
      fluidRow(style='padding:20px;',
        column(width=6,tableOutput('table1')),
        column(width=6,tableOutput('table2'))
      ),
      fluidRow(
        column(width=6,tableOutput('table3')),
        column(width=6,tableOutput('table4'))
      ),
      fluidRow(
        column(width=6,offset=3,tableOutput('table5'))
      )
    )
  )
      #plotOutput('plot1'),
      #plotOutput('plot2'),
)


server <- function(input, output) {
  
  clustnum <- reactiveValues()
  clustnum <- 5
  #young.norm.comp <- reactive({as.data.frame(apply(young.comp,2,myNorm))})
  #young.norm.comp <- reactive({as.data.frame(apply(young.comp,2,scale))})
  
  #young.norm.comp2 <- as.data.frame(lapply(young.comp,myNorm))
  
  # Create a value for setseed
  val <- eventReactive(input$setseedButton, {input$seed})
  # Output the seed value
  output$seedvalue <- renderText({ #output the seed value when update button clicked
    input$setseedButton
    val <- isolate(input$seed)
    paste("Seed =", val)
  })

  # Create a random subset of 200 individuals from the dataset for easier plotting
  subsamp <- reactive({set.seed(val()); sample(1:674,500)})

  # Perfom all necessary analyses and save results
  clusters <- reactive({set.seed(val()); kmeans(young.norm.comp, clustnum)})
  characters <- reactive({row.names(as.data.frame(t(clusters()$centers)))})
  PCAord <- reactive({princomp(young.norm.comp)})
  PCAsub <- reactive({PCAord()$scores[subsamp(),]})
  clustGroups <- reactive({as.numeric(clusters()$cluster[subsamp()])})
  PCAloadings <- reactive({as.data.frame(unclass(PCAord$loadings))})
  ### Normalize the loadings within each feature:
  
  # Plot a PCA with first two axes and label the groups
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(PCAsub(),
         col = clustGroups(), pch=16,
         cex = 2)
    #points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    
    for(i in 1:clustnum){
      Plot_ConvexHull(xcoord = PCAsub()[clustGroups()==i,1], ycoord = PCAsub()[clustGroups()==i,2], lcolor = i, grouplab=paste("Group",i))
    }
  })
  
  output$plot2 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(PCAsub()[,c(1,3)],
         col = clustGroups(), pch=16,
         cex = 2)
    #points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    
    for(i in 1:clustnum){
      Plot_ConvexHull(xcoord = PCAsub()[clustGroups()==i,1], ycoord = PCAsub()[clustGroups()==i,3], lcolor = i, grouplab=paste("Group",i))
    }
  })
  
  output$plot3 <- renderPlot({
    par(mar = c(0, 0, 2, 0), oma=c(0,0,2,0))
    radarchart(Groups.all()[[1]], axistype=0, pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2, pty=32, cglcol="grey", cglty=2, cglwd=1, vlcex=1)  
    title(main="Group 1", cex.main=1.5, adj=0.5, line=-1,outer=T)
  })
  output$plot4 <- renderPlot({
    par(mar = c(0, 0, 2, 0), oma=c(0,0,2,0))
    radarchart(Groups.all()[[2]], axistype=0, pcol=rgb(0.2,0,0.5,0.9) , pfcol=rgb(0.2,0,0.5,0.5) , plwd=2, pty=32, cglcol="grey", cglty=2, cglwd=1, vlcex=1)  
    title(main="Group 2", cex.main=1.5, adj=0.5, line=-1,outer=T)
  })
  output$plot5 <- renderPlot({
    par(mar = c(0, 0, 2, 0), oma=c(0,0,2,0))
    radarchart(Groups.all()[[3]], axistype=0, pcol=rgb(0.0,0.5,0.5,0.9) , pfcol=rgb(0.0,0.5,0.5,0.5) , plwd=2, pty=32, cglcol="grey", cglty=2, cglwd=1, vlcex=1)  
    title(main="Group 3", cex.main=1.5, adj=0.5, line=-1,outer=T)
  })
  output$plot6 <- renderPlot({
    par(mar = c(0, 0, 2, 0), oma=c(0,0,2,0))
    radarchart(Groups.all()[[4]], axistype=0, pcol=rgb(0.5,0.8,0.5,0.9) , pfcol=rgb(0.5,0.8,0.5,0.5) , plwd=2, pty=32, cglcol="grey", cglty=2, cglwd=1, vlcex=1)  
    title(main="Group 4", cex.main=1.5, adj=0.5, line=-1,outer=T)
  })
  output$plot7 <- renderPlot({
    par(mar = c(0, 0, 2, 0), oma=c(0,0,2,0))
    radarchart(Groups.all()[[5]], axistype=0, pcol=rgb(0.2,0.5,0.8,0.9) , pfcol=rgb(0,0.5,0.8,0.5) , plwd=2, pty=32, cglcol="grey", cglty=2, cglwd=1, vlcex=1)  
    title(main="Group 5", cex.main=1.5, adj=0.5, line=-1,outer=T)
  })
  
  #Create a table with the top 5 defining characteristics of each group
  topchar1 <- reactive({sort(clusters()$centers[1,], decreasing = T)})
  topchar2 <- reactive({sort(clusters()$centers[2,], decreasing = T)})
  topchar3 <- reactive({sort(clusters()$centers[3,], decreasing = T)})
  topchar4 <- reactive({sort(clusters()$centers[4,], decreasing = T)})
  topchar5 <- reactive({sort(clusters()$centers[5,], decreasing = T)})
  
  output$table1 <- renderTable(bordered=T,{data.frame("Group1"=names(topchar1())[1:5])})
  output$table2 <- renderTable(bordered=T,width=4,{data.frame("Group2"=names(topchar2())[1:5])})
  output$table3 <- renderTable(bordered=T,width=40,{data.frame("Group3"=names(topchar3())[1:5])})
  output$table4 <- renderTable(bordered=T,width=40,{data.frame("Group4"=names(topchar4())[1:5])})
  output$table5 <- renderTable(bordered=T,width=40,{data.frame("Group5"=names(topchar5())[1:5])})
  
  
  #### Create radar plots for each group
  # 1) Get a vector of categories and scores and order it based on the order of features for each group
  cats.group1 <- reactive({cats[order(clusters()$centers[1,], decreasing = T),]})
  cats.group2 <- reactive({cats[order(clusters()$centers[2,], decreasing = T),]})
  cats.group3 <- reactive({cats[order(clusters()$centers[3,], decreasing = T),]})
  cats.group4 <- reactive({cats[order(clusters()$centers[4,], decreasing = T),]})
  cats.group5 <- reactive({cats[order(clusters()$centers[5,], decreasing = T),]})
  
  # 2) Then multiply the cat scores with the group list loadings
  Scores.group1 <- reactive({cats.group1()$score * topchar1()})
  Scores.group2 <- reactive({cats.group1()$score * topchar2()})
  Scores.group3 <- reactive({cats.group1()$score * topchar3()})
  Scores.group4 <- reactive({cats.group1()$score * topchar4()})
  Scores.group5 <- reactive({cats.group1()$score * topchar5()})
 
  # 3) Then aggregate the sum value of each category for the group
  Sum.group1 <- reactive({
    dat <- as.data.frame(tapply(Scores.group1()[complete.cases(Scores.group1())], list(cats.group1()$cat[complete.cases(Scores.group1())]),sum))
    dat1 <- dat[-c(2,5,6,10,9,13,16,14),]
    dat2 <- as.data.frame(rbind(rep(6,length(dat1)), rep(-8,length(dat1)) , (dat1)))
    dat2
  })
  Sum.group2 <- reactive({
    dat <- as.data.frame(tapply(Scores.group2()[complete.cases(Scores.group2())], list(cats.group2()$cat[complete.cases(Scores.group2())]),sum))
    dat1 <- dat[-c(2,5,6,10,13,9,16,14),]
    dat2 <- as.data.frame(rbind(rep(6,length(dat1)), rep(-8,length(dat1)) , (dat1)))
    dat2
  })
  Sum.group3 <- reactive({
    dat <- as.data.frame(tapply(Scores.group3()[complete.cases(Scores.group3())], list(cats.group3()$cat[complete.cases(Scores.group3())]),sum))
    dat1 <- dat[-c(2,5,6,10,13,9,16,14),]
    dat2 <- as.data.frame(rbind(rep(6,length(dat1)), rep(-8,length(dat1)) , (dat1)))
    dat2
  })
  Sum.group4 <- reactive({
    dat <- as.data.frame(tapply(Scores.group4()[complete.cases(Scores.group4())], list(cats.group4()$cat[complete.cases(Scores.group4())]),sum))
    dat1 <- dat[-c(2,5,6,10,13,9,16,14),]
    dat2 <- as.data.frame(rbind(rep(6,length(dat1)), rep(-8,length(dat1)) , (dat1)))
    dat2
  })
  Sum.group5 <- reactive({
    dat <- as.data.frame(tapply(Scores.group5()[complete.cases(Scores.group5())], list(cats.group5()$cat[complete.cases(Scores.group5())]),sum))
    dat1 <- dat[-c(2,5,6,10,13,9,16,14),]
    dat2 <- as.data.frame(rbind(rep(6,length(dat1)), rep(-8,length(dat1)) , (dat1)))
    dat2
  })

  Groups.all <- reactive({
    temp <- rbind(Sum.group1()[3,],Sum.group2()[3,],Sum.group3()[3,],Sum.group4()[3,],Sum.group5()[3,])
    #scaled <- as.data.frame(lapply(temp, scale))
    #scaled <- as.data.frame(apply(temp, 2, scale))
    scaled <- as.data.frame(t(apply(temp, 1, scale)))
    names(scaled) <- names(as.data.frame(temp))
    #scaled <- as.data.frame(apply(temp,2,myNorm))
    #scaled <- temp
    mini <- min(scaled)
    maxi <- max(scaled)
    sum1 <- as.data.frame(rbind(rep(maxi,length(scaled)), rep(mini,length(scaled)) , (scaled[1,])))
    sum2 <- as.data.frame(rbind(rep(maxi,length(scaled)), rep(mini,length(scaled)) , (scaled[2,])))
    sum3 <- as.data.frame(rbind(rep(maxi,length(scaled)), rep(mini,length(scaled)) , (scaled[3,])))
    sum4 <- as.data.frame(rbind(rep(maxi,length(scaled)), rep(mini,length(scaled)) , (scaled[4,])))
    sum5 <- as.data.frame(rbind(rep(maxi,length(scaled)), rep(mini,length(scaled)) , (scaled[5,])))
    list(sum1,sum2,sum3,sum4,sum5)
  })
  
  output$tableagg <- renderTable({
    tab1 <- aggregate(data=young.comp, young.comp[,input$comp]~clusters()$cluster,mean)
    names(tab1) <- c("Group",input$comp)
    tab1
  })
  
  ###### TEST BUTTON ######
  observeEvent(input$testButton,{
    #print(clustGroups())
     print(sort(clusters()$centers[1,], decreasing = T))
    # print(topchar1())
    #print(clusters()$cluster)
    #print(young.comp[,input$comp])
    # print(Groups.all())
    #print()
  })
  #########################
}




shinyApp(ui = ui, server = server)