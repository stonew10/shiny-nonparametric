#library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Multiple Comparison App"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose Data File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("alpha", "Alpha",
                   choices = c("90%" = as.numeric(.1),
                               "95%" = as.numeric(.05),
                               "99%" = as.numeric(.01)),
                   selected = as.numeric(.05)),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("test", "Test",
                   choices = c("HSD" = 1,
                               "HSD Ranks" = 3,
                               "LSD" = 5,
                               "LSD Ranks" = 7,
                               "Bonferonni" = 9,
                               "Bonferonni Ranks" = 11),
                   selected = 1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents"),
      
      # Output: Boxplots ----
      plotOutput("boxplots"),
      
      #Output: Histogram -----
        plotOutput("histograms"),
      
      # Output: Test chosen ----
      tableOutput("test_out")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.
  
  data <- reactive({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)#[,-1]
    
  }
  
  )
  
  output$contents <- renderTable({
    
    data()
    
  })
  
  output$boxplots <- renderPlot({
    
    boxplot(data(), col=c("#c00000", rgb(0.36,0.57,0.28),rgb(58,87,149,max =250)), lwd=1)
    
  })
  
  output$histograms <- renderPlot({
    
    TestData = data()
    
    H1 = as.numeric(unlist(TestData[ ,1]))
    H2 = as.numeric(unlist(TestData[ ,2])) 
    H3 = as.numeric(unlist(TestData[ ,3]))
    
    #Example random normally distributed data centered around (6, 10, 14)
    #H1 = rnorm(100, 6)
    #H2 = rnorm(100, 10)
    #H3 = rnorm(100, 14)
    
    TestData = data()
    xmin = as.numeric(min(TestData)-5)
    xmax = as.numeric(max(TestData)+5)
    opar = par(lwd=1.5)
    #xlim and ylim will need to be changed to accomodate our final data set
    hist(H1, col=rgb(.9,0,0, alpha =.8),xlim=c(xmin,xmax), ylim=c(0,10), main="Overlapping Histogram" , xlab="Variable")
    hist(H2, col=rgb(0.36,0.57,0.28,alpha =.8), add=T)
    hist(H3, col=rgb(.23,.35,.62,alpha =.8), add=T)
    legend("topright", c(colnames(TestData[1]), colnames(TestData[2]), colnames(TestData[3])), 
           col=c("#c00000", rgb(0.36,0.57,0.28),rgb(58,87,149,max =250)), lwd=10)
    par(opar)
    
  })
  
  
  output$test_out <- renderTable({
    
    # Variables
    TestData = data()
    #TestData = Table3
    G1 = TestData[,1]
    G2 = TestData[,2]
    G3 = TestData[,3]
    
    n1 = length(G1)
    n2 = length(G2)
    n3 = length(G3)
    
    all_data = matrix(cbind(TestData[,1], TestData[,2], TestData[,3]), ncol = 1)
    ranks = rank(all_data)
    
    x = as.numeric(n1+1)
    y = as.numeric(n1+n2)
    z = as.numeric(n1+n2+1)
    aa = as.numeric(n1+n2+n3)
    
    r1 = ranks[1:n1]
    r2 = ranks[x:y]
    r3 = ranks[z:aa]
      
    mean1 = mean(G1)
    mean2 = mean(G2)
    mean3 = mean(G3)
    
    N = sum(n1, n2, n3)
    
    var1 = var(G1)
    var2 = var(G2)
    var3 = var(G3)
    
    k=ncol(input$file1)
    SSE = (n1-1)*(var1)+(n2-1)*(var2)+(n3-1)*(var3)
    MSE = (SSE/(N-k))
    m=(k*(k-1))/2
    
    alpha = as.numeric(input$alpha)
    
    if (input$test == 1) {
      HSD_12 = qtukey(alpha, k, N-k)*sqrt(MSE/(n1))
      HSD_13 = qtukey(alpha, k, N-k)*sqrt(MSE/(n1))
      HSD_23 = qtukey(alpha, k, N-k)*sqrt(MSE/(n1))
      
      a = isTRUE(HSD_12 <= abs(mean1-mean2))
      b = isTRUE(HSD_13 <= abs(mean1-mean3))
      c = isTRUE(HSD_23 <= abs(mean2-mean3))
      
      Out_A = c("Groups 1 and 2 are significantly different:", a)
      Out_B = c("Groups 1 and 3 are significantly different:", b)
      Out_C = c("Groups 2 and 3 are significantly different:", c)
      
      Out_F = rbind(Out_A, Out_B, Out_C)
      Out_F
      
      
    } else if (input$test == 3) {
      HSD_rank_12 = qtukey(1-alpha, k ,Inf)*sqrt((N*(N+1)/(12*n1))*((1/n1)+(1/n2)))
      HSD_rank_13 = qtukey(1-alpha, k ,Inf)*sqrt((N*(N+1)/(12*n1))*((1/n1)+(1/n3)))
      HSD_rank_23 = qtukey(1-alpha, k ,Inf)*sqrt((N*(N+1)/(12*n1))*((1/n2)+(1/n3)))
      
      a = isTRUE(HSD_rank_12) <= abs(mean(r1)-mean(r2))
      b = isTRUE(HSD_rank_13) <= abs(mean(r1)-mean(r3))
      c = isTRUE(HSD_rank_23) <= abs(mean(r2)-mean(r3))
      Out_A = c("Groups 1 and 2 are significantly different:", a)
      Out_B = c("Groups 1 and 3 are significantly different:", b)
      Out_C = c("Groups 2 and 3 are significantly different:", c)
      
      Out_F = rbind(Out_A, Out_B, Out_C)
      Out_F
      
    } else if (input$test == 5) {
      LSD_12 = qt(1-alpha/2, N-k)*sqrt(MSE*((1/n1)+(1/n2)))
      LSD_13 = qt(1-alpha/2, N-k)*sqrt(MSE*((1/n1)+(1/n3)))
      LSD_23 = qt(1-alpha/2, N-k)*sqrt(MSE*((1/n2)+(1/n3)))
      
      a = isTRUE(LSD_12 <= abs(mean1-mean2))
      b = isTRUE(LSD_13 <= abs(mean1-mean3))
      c = isTRUE(LSD_23 <= abs(mean2-mean3))
      Out_A = c("Groups 1 and 2 are significantly different:", a)
      Out_B = c("Groups 1 and 3 are significantly different:", b)
      Out_C = c("Groups 2 and 3 are significantly different:", c)
      
      Out_F = rbind(Out_A, Out_B, Out_C)
      Out_F
      
    } else if (input$test == 7) {
      LSD_rank_12 = qnorm(1-alpha/2)*sqrt((N*(N+1)/12)*(1/n1)+(1/n2))
      LSD_rank_13 = qnorm(1-alpha/2)*sqrt((N*(N+1)/12)*(1/n1)+(1/n3))
      LSD_rank_23 = qnorm(1-alpha/2)*sqrt((N*(N+1)/12)*(1/n2)+(1/n3))
      
      a = isTRUE(LSD_rank_12) <= abs(mean(r1)-mean(r2))
      b = isTRUE(LSD_rank_13) <= abs(mean(r1)-mean(r3))
      c = isTRUE(LSD_rank_23) <= abs(mean(r2)-mean(r3))
      Out_A = c("Groups 1 and 2 are significantly different:", a)
      Out_B = c("Groups 1 and 3 are significantly different:", b)
      Out_C = c("Groups 2 and 3 are significantly different:", c)
      
      Out_F = rbind(Out_A, Out_B, Out_C)
      Out_F
      
    } else if (input$test == 9) {
      Bonferonni_12 = qt(1-(alpha/(2*m)), N-k)*sqrt((MSE)*((1/n1)+(1/n2)))
      Bonferonni_13 = qt(1-(alpha/(2*m)), N-k)*sqrt((MSE)*((1/n1)+(1/n3)))
      Bonferonni_23 = qt(1-(alpha/(2*m)), N-k)*sqrt((MSE)*((1/n2)+(1/n3)))
      
      a = isTRUE(Bonferonni_12 <= abs(mean1-mean2))
      b = isTRUE(Bonferonni_13 <= abs(mean1-mean3))
      c = isTRUE(Bonferonni_23 <= abs(mean2-mean3))
      
      
      Out_A = c("Groups 1 and 2 are significantly different:", a)
      Out_B = c("Groups 1 and 3 are significantly different:", b)
      Out_C = c("Groups 2 and 3 are significantly different:", c)
      
      Out_F = rbind(Out_A, Out_B, Out_C)
      Out_F
      
      
    } else if (input$test == 11) {
      Bonferonni_rank_12 = qnorm(1-(alpha/(2*m)))*sqrt((((N*(N+1))/12))*((1/n1)+(1/n2)))
      Bonferonni_rank_13 = qnorm(1-(alpha/(2*m)))*sqrt((((N*(N+1))/12))*((1/n1)+(1/n3)))
      Bonferonni_rank_23 = qnorm(1-(alpha/(2*m)))*sqrt((((N*(N+1))/12))*((1/n2)+(1/n3)))
      
      BonfA = isTRUE(Bonferonni_rank_12) <= abs(mean(r1)-mean(r2))
      BonfB = isTRUE(Bonferonni_rank_13) <= abs(mean(r1)-mean(r3))
      BonfC = isTRUE(Bonferonni_rank_23) <= abs(mean(r2)-mean(r3))
      
      
      Out_A = c("Groups 1 and 2 are significantly different:", BonfA)
      Out_B = c("Groups 1 and 3 are significantly different:", BonfB)
      Out_C = c("Groups 2 and 3 are significantly different:", BonfC)
      
      Out_F = rbind(Out_A, Out_B, Out_C)
      Out_F
      
    }
    else {paste("Error")}
    
 
  })
  
  
  
}
# Run the app ----
shinyApp(ui, server)



