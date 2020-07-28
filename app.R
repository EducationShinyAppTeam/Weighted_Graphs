# Load Packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(ggplot2)
library(shinyMatrix)
library(igraph)
library(dplyr)
library(DT)
library(shinyWidgets)

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "Weighted Graphs"
APP_DESCP  <<- "This app explores the applications of Markov Chains to weighted graphs"

# End App Meta Data------------------------------------------------------------

# Function for showing the check or X based on a condition
# Input: boolean for whether the condition is true or false
# Output: appropriate image
correctnessPic <- function(condition){
  if(condition){
    img(src = "check.PNG", alt = "Correct Answer", width = 30)
  }
  else{
    img(src = "cross.PNG", alt = "InCorrect Answer", width = 30)
  }
}

# Function for showing positive or negative text output based on a condition
# Input: boolean for whether the condition is true or false
# Output: appropriate text
correctnessText <- function(condition){
  if(condition){
    "Good Job! You are Correct!"
  }
  else{
    "Check your work for errors"
  }
}

# Define UI for App
ui <- list(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
  ),
  ## Create the app page
  dashboardPage(
    skin = "blue",
    ### Create the app header
    dashboardHeader(
      titleWidth=250,
      title = "Weighted Graphs", # You may use a shortened form of the title here
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(class = "dropdown",
              tags$a(href='https://shinyapps.science.psu.edu/',
                     icon("home")))
    ),
    ### Create the sidebar/left navigation menu
    dashboardSidebar(
      width=250,
      sidebarMenu(
        id = "tabs",
        menuItem("Overview", tabName = "Overview", icon = icon("tachometer-alt")),
        menuItem("Explore", tabName = "Explore", icon = icon("wpexplorer")),
        menuItem("Challenge", tabName = "Challenge", icon = icon("cogs")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ### Create the content
    dashboardBody(
      tabItems(
        #### Set up the Overview Page
        tabItem(
          tabName = "Overview",
          withMathJax(),
          h1("Exploring Weighted Graphs and Markov Chains"), 
          p("This app show the relationship between connected graphs and Markov 
            Chains. The content of this app is loosely based on Sheldon M. Ross's 
            book Introduction to Probability Models, section 4.8. Prior understanding
            of discrete time markov chains is expected in this app."),
          h2("Instructions"),
          tags$ol(
            tags$li("Use the explore tab to explore the relationship between 
                    weighted graphs and Markov Chains by creating a graph then 
                    calculating its transition matrix and long run probabilities."),
            tags$li("In the challenge tab, challenge yourself to solve a more
                    complex problem using weighted graphs.")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt")
            )
          )
        ),
        
        #### Set up an Explore Page
        tabItem(
          tabName = "Explore",
          withMathJax(),
          h2("Explore the Weighted Graphs"),
          p("First use the sidebar panel to enter weights for your weighted graph.
            Edge weights must be non-negative and edges of weight 0 will not
            appear in the graph. Then, calculate the transition matrix for the 
            corresponding Markov chain as well as the long run probabilities
            for the chain. "),
          
          fluidRow(
            # Title
            titlePanel("Weighted Graphs and Markov Chains"),
            
            #"Step 1: Create a weighted graph.",
            sidebarLayout(
              sidebarPanel(
                "Enter non-negative weights for each edge. Edges with weight 0 
                will not appear in the graph.",
                matrixInput("weights", 
                            matrix(rep(c(0,1),3), 
                                   nrow=6, 
                                   dimnames = list(c("A-B", 
                                                     "A-C", 
                                                     "A-D", 
                                                     "B-C", 
                                                     "B-D", 
                                                     "C-D"),
                                                   c("Weight"))), 
                            rows = list(names = TRUE), 
                            cols = list(names = TRUE),
                            class="numeric"),
                width = 6
              ),
              
              # Outputs: plot of states visited and the matrix to the n-steps power
              mainPanel(
                "Your current graph:",
                plotOutput("graph"),
                tags$script(HTML(
                  "$(document).ready(function() {
                    document.getElementById('graph').setAttribute('aria-label',
                    `This image shows the weighted graph created by the inputted 
                    edge weights.`)
                  })"
                )),
                width=6
              )
            ),
            fluidRow(
              column(width=6,
                     "What are the probabilities for the transition matrix 
                     corresponding to your weighted graph? Write your 
                     probabilities as decimals to at least 2 decimal places.",
                     uiOutput("uMat"),
                     checkboxInput("showMatHint", "Show Hint"),
                     conditionalPanel(
                       condition = "input.showMatHint",
                       "From any node, the state will move to j from i with
                       probability \\(\\frac{\\omega _{ij}}{\\sum_k \\omega_{ik}}\\),
                       where \\(\\omega_{ij}\\) is the weight on the edge connecting 
                       nodes \\(i\\) and \\(j\\)."
                     ),
                     fluidRow(
                       column(width = 4, 
                              actionButton("check", "Check Answer")),
                       column(width = 8, uiOutput("correctness"))),
                     actionButton("displayMat", "Show Answer"),
                     tableOutput("chainMat")
              ),
              column(width=6,
                     "What are the long run probabilities for each state? Write 
                     your probabilities as decimals to at least 2 decimal places.",
                     uiOutput("LRuMat"),
                     checkboxInput("showLRHint", "Show Hint"),
                     conditionalPanel(
                       condition= "input.showLRHint",
                       "Using the time reversibility equations we get that the long
                       run probability for each state i is
                       \n \\(\\pi_i = \\frac{\\sum_j \\omega _{ij}}{\\sum_i\\sum_j \\omega_{ij}}\\).\n 
                       where \\(\\omega_{ij}\\) is the weight on the edge connecting 
                       nodes \\(i\\) and \\(j\\).
                       You can also find the long run probabilities using the 
                       transition matrix."
                     ),
                     fluidRow(
                       column(width = 4, actionButton("checkLR", "Check Answer")),
                       column(width = 8, uiOutput("correctnessLR"))),
                     actionButton("displayLR", "Show Answer"),
                     tableOutput("longRun")
              )))),
        
        #### Set up a Challenge Page
        tabItem(
          tabName = "Challenge",
          withMathJax(),
          h2("Challenge Yourself"),
          p("In this section, you will challenge yourself by solving a problem 
            using a Markov Chain to represent a weighted graph."),
          h3("Problem"),
          p("A knight is a chesspiece whose moves are each two spaces either 
          forward, backward, or sideways followed by one space perpendicular to 
          the direction of the first step, creating an L-shape. Suppose we have 
          a knight that starts at some given location on an 8x8 chessboard. What 
          is the expected number of moves for the knight to return to the 
          starting location for any location on the board, assuming that each 
            move takes the knight to each of its possible moves with equal 
            probability?"),
          
          tabsetPanel(
            id="challengeTabs",
            tabPanel("Home", value="home",
                     p("This section will allow you to work through the steps of 
                     the problem. The problem itself proceeds through the first 
                     three tabs while the final tab shows a simulated version of 
                     this problem."),
                     # Fill in the blank section; based on Ethan's fill in the 
                     # blank code
                     p("In considering this problem, we must first figure out how 
                       to approach it."),
                     div(style="display:inline-block", 
                         p("This problem can be considered as a weighted graph 
                           where,")),
                     div(style="display:inline-block", 
                         selectInput("fillinAns1", 
                                     label=NULL, 
                                     choices=c("",
                                               "knights", 
                                               "squares on the board", 
                                               "potential moves from the knight's 
                                               position"), 
                                     width="225px")),
                     div(style ="display:inline-block", 
                         htmlOutput("fillinFeedback1")),
                     div(style = "display:inline-block", 
                         p(' are the vertices and ')),
                     div(style="display:inline-block", 
                         selectInput("fillinAns2", 
                                     label=NULL, 
                                     choices=c("",
                                               "knights", 
                                               "squares on the board", 
                                               "potential moves from the knight's 
                                               position"),
                                     width="225px")),
                     div(style ="display:inline-block", 
                         htmlOutput("fillinFeedback2")),
                     div(style = "display:inline-block", 
                         p(' are edges.  The chain will have ')),
                     div(style="display:inline-block", 
                         numericInput("fillinAns3", 
                                      label=NULL, 
                                      value = NULL, 
                                      min=0, 
                                      step=1, 
                                      width = "75px")),
                     div(style ="display:inline-block", 
                         htmlOutput("fillinFeedback3")),
                     div(style = "display:inline-block", 
                         p(' states, one for each ')),
                     div(style="display:inline-block", 
                         selectInput("fillinAns4", 
                                     label=NULL, 
                                     choices = c("", "vertex", "edge"), 
                                     width="100px")),
                     div(style ="display:inline-block", 
                         htmlOutput("fillinFeedback4")),
                     div(style = "display:inline-block", 
                         p(' with the ')),
                     div(style="display:inline-block", 
                         selectInput("fillinAns5", 
                                     label=NULL, 
                                     choices = c("", "vertex", "edge"), 
                                     width="100px")),
                     div(style ="display:inline-block", 
                         htmlOutput("fillinFeedback5")),
                     div(style = "display:inline-block", 
                         p(' weights all equal because the knight is equally likely 
                       to go to any of its possible moves.')),
                     br(),
                     actionButton("submitFillIn","Submit Answer"),
                     bsButton("next0", "Next"))
            ,
            tabPanel("Pt 1", value="pt1",
                     sidebarLayout(
                       sidebarPanel(width=6,"In modelling this problem, drawing 
                       out the entire matrix for the Markov Chain would be tedious
                       as a chain with 64 states has 4096 transition probabilities, 
                       most of which will be 0.", "Instead, let's start by figuring
                       out how many branches will lead from each node (i.e. square
                       on the board). Fill in these values below.",
                                    fluidRow(
                                      column(width = 6, 
                                             fluidRow(column(width=8, 
                                                             numericInput("branchesA", 
                                                                          label="Edges From A", 
                                                                          value=0, 
                                                                          min=0, 
                                                                          max=10)),
                                                      column(width = 4, 
                                                             br(), 
                                                             uiOutput("correctBranchA"))),
                                             fluidRow(column(width=8, 
                                                             numericInput("branchesB", 
                                                                          label="Edges From B",
                                                                          value=0, 
                                                                          min=0,
                                                                          max=10)),
                                                      column(width = 4, 
                                                             br(), 
                                                             uiOutput("correctBranchB"))),
                                             fluidRow(column(width=8, 
                                                             numericInput("branchesC", 
                                                                          label="Edges From C", 
                                                                          value=0, 
                                                                          min=0, 
                                                                          max=10)),
                                                      column(width = 4, 
                                                             br(), 
                                                             uiOutput("correctBranchC")))),
                                      column(width = 6, 
                                             fluidRow(column(width=8, 
                                                             numericInput("branchesD", 
                                                                          label="Edges From D", 
                                                                          value=0,
                                                                          min=0, 
                                                                          max=10)),
                                                      column(width = 4, 
                                                             br(), 
                                                             uiOutput("correctBranchD"))),
                                             fluidRow(column(width=8, 
                                                             numericInput("branchesE", 
                                                                          label="Edges From E", 
                                                                          value=0, 
                                                                          min=0, 
                                                                          max=10)),
                                                      column(width = 4, 
                                                             br(), 
                                                             uiOutput("correctBranchE"))))
                                    ),
                                    actionButton("checkNumBranches", "Check Answers")
                       ),
                       
                       mainPanel(width=6,
                                 "Below illustrates the chess board. Each label 
                        corresponds to a number of potential moves for the knight.",           
                                 dataTableOutput("chessBoard", width=320)
                       )
                     ),
                     bsButton("next1", "Next")
                     
                     
            ),
            tabPanel("Pt 2", value="pt2",
                     tableOutput("table"),
                     "Assume the weight of each edge is 1 since they all must be 
                     the same. We will now calculate the long run proportions 
                     using the equation: 
                     \\(\\pi_i = \\frac{\\sum_j \\omega _{ij}}{\\sum_i\\sum_j \\omega_{ij}}\\)",
                     p("Now calculate the sum of all edge weights (the denominator)."),
                     fluidRow(
                       column(width=2, 
                              numericInput("sumWeights", 
                                           "Sum", 
                                           width='100px', 
                                           value=0)),
                       column(width=1,  br(),
                              uiOutput("correctSum"))
                     ),
                     actionButton( "checkSum", "Check Answer"),
                     p("Recall that the expected value of the number of jumps it 
                     would take the knight to return to a given spot is the inverse
                     of the long run probabilitity of the knight being in said 
                     state. What is the expected number of jumps to return to
                     a node of each type?"),
                     
                     fluidRow(
                       column(width=2,
                              numericInput("chessAnsA", "A", value=0),
                              uiOutput("ansA")),
                       column(width=2,
                              numericInput("chessAnsB", "B", value=0),
                              uiOutput("ansB")),
                       column(width=2,
                              numericInput("chessAnsC", "C", value=0),
                              uiOutput("ansC")),
                       column(width=2,
                              numericInput("chessAnsD", "D", value=0),
                              uiOutput("ansD")),
                       column(width=2,
                              numericInput("chessAnsE", "E", value=0),
                              uiOutput("ansE"))
                     ),
                     actionButton("checkFinAns", "Check Answers")
                     
                     
            ),
            tabPanel("Simulation", value="sim",
                     sidebarLayout(
                       sidebarPanel( width=6,
                                     p("This tab allows you to simulate the knight's moves 
                       around the chessboard. Click a square on the chessboard 
                       to move the knight there. You can then either use the 
                       Step button to allow the knight to move around the board 
                       one step at a time or use the Simulate button to see how 
                       many jumps it will take the knight to return to its
                       target position, the colored square, in one simulation"),
                                     actionButton("step", "Step"),
                                     actionButton("resetStep", "Reset Step Count"),
                                     textOutput("stepCount"),
                                     actionButton("push", "Simulate"),
                                     textOutput("text")),
                       mainPanel(width=6,
                                 plotOutput("ggstyle", click="start"), # Clickable plot to 
                                 # define chess board
                                 htmlOutput("chessBoardAlt")
                       )),
            )
          )
        ),
        
        #### Set up the References Page
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny, 
            R package. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. (2019), boastUtils: BOAST Utilities. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny', R package. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019), shiny: Web application framework for R, R package. Available 
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent", 
            "Csardi, G. and Nepusz, T. (2006), igraph: The igraph software package 
            for complex network research, InterJournal, Complex Systems 1695. 
            http://igraph.org"
          ), 
          p(
            class = "hangingindent", 
            "Neudecker, A. (2019), shinyMatrix: Shiny Matrix Input Field, R 
            package, R package. Available from 
            https://CRAN.R-project.org/package=shinyMatrix"
          ), 
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2020), shinyWidgets: Custom 
            Inputs Widgets for Shiny, R package. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent", 
            HTML(paste("Ross S. M. (2014),", tags$em("Introduction to Probability Models"), 
                       "(11th ed.), Amsterdam: Elsevier/Academic Press"
            ))), 
          p(
            class = "hangingindent", 
            "Wickham, H., François, R., Henry L., and Müller, K. (2020), dplyr: 
            A Grammar of Data Manipulation, R package. Available from 
            https://CRAN.R-project.org/package=dplyr"
          ),
          p(
            class = "hangingindent",
            "Wickham, W. (2016), ggplot2: Elegant graphics for data analysis,
            R Package. Springer-Verlag New York. Available from
            https://ggplot2.tidyverse.org"
          ),
          
          p(
            class = "hangingindent",
            "Xie, Y., Cheng, J. and Tan X. (2020), DT: A Wrapper of the 
            JavaScript Library 'DataTables', R package. Available from
            https://CRAN.R-project.org/package=DT"
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "In this app, you will explore the relationship between weighted 
      graphs and Markov chains. Explore the topic by creating weighted graphs 
      and their corresponding transition matricies. Then go to the challenge 
      section to solve a more applied problem related to weighted graphs.",
      type = "info"
    )
  })
  
  # Go button on Overview page
  observeEvent(
    eventExpr = input$go1, 
    handlerExpr = { 
      updateTabItems(session, 
                     inputId = "tabs", 
                     selected = "Explore" 
      ) 
    })
  
  #Makes sure weights are numeric
  weights<-reactive({as.numeric(input$weights)})
  
  # Creates a dataframe with only the relevant nodes from input
  reduceWeights<-reactive({
    # Make sure input is valid before preceding (weights must be in [0,1])
    validate(
      need(max(weights())>0 && min(weights()>=0), 
           "All edges must have non-negative weight and at least one edge must 
           have positive weight.")
    )
    # From and to values to create the 6 possible edges plus weights
    from<-c("A","A", "A", "B", "B", "C")
    to<-c("B","C", "D", "C", "D", "D")
    weight<-weights()
    df<-data.frame(from=from, to=to, weight=weight)
    dplyr::filter(df, weight>0) # Remove edges with weight 0
  })
  
  # Creates correct transition matrix
  createChainMatrix<-reactive({
    validEdges<-c()
    # Keep only edges used in the graph
    for(x in c("A", "B", "C", "D")){
      if(x %in% reduceWeights()$to || x %in% reduceWeights()$from){
        validEdges<-c(validEdges, x)}
    }
    cMat<-dplyr::filter(reduceWeights(), to %in% validEdges && from %in% validEdges)
    
    # Create a second matrix and bind it to the first with to and from switched 
    # (now all edges featured twice)
    cMat2<-cMat
    colnames(cMat2)<-c("to", "from", "weight")
    cMat<-rbind(cMat, cMat2)
    
    # Pivot the combined matrix so rows are the from nodes and columns are to 
    # (with weight as value)
    cMat<-tidyr::pivot_wider(cMat, 
                             id_cols="from", 
                             names_from="to", 
                             values_from = "weight")
    cMat[is.na(cMat)]<-0 # Turns NAs into 0s
    names<-cMat$from
    cMat<-cMat[,-1] # Gets rid of the row labeling the from values
    cMat<-as.data.frame(cMat) 
    rownames(cMat)<-(names) # Assign names to rows
    
    # Turn matrix into probabilities by dividing by row sums 
    # (because rows represent a node)
    for(x in 1:length(validEdges)){
      rowSum<-sum(cMat[x,])
      for(y in 1:length(validEdges)){
        cMat[x,y] <- cMat[x,y]/rowSum
      }
      
    }
    cMat[order(names(cMat))]
  })
  
  # Function to display correct transition matrix only when button is pushed
  displayCorrectMatrix<-eventReactive(input$displayMat, {createChainMatrix()})
  
  # Determines whether the transition matrix is correct
  isCorrect<-eventReactive(input$check,{
    if(typeof(all.equal(as.matrix(round(input$userMatrix,2)),
                        as.matrix(round(createChainMatrix(),2))))=="character"){
      FALSE}
    else{
      TRUE}
  })
  
  # Determines whether the long run probabilities are correct
  isCorrectLR<-eventReactive(input$checkLR,{
    if(typeof(all.equal(round(as.numeric(input$LRuserMatrix),2),
                        round(as.numeric(longRuns()[,1]),2)))=="character"){
      FALSE}
    else{
      TRUE}
  })
  
  # Determines long run proportions of the matrix
  longRuns<-reactive({
    validNodes<-rownames(createChainMatrix())
    # Set up structure for matrix
    LR<-as.data.frame(matrix(rep(0, length(validNodes))))
    rownames(LR)<-validNodes
    colnames(LR)<-c("Probability")
    # Split up the from and to cases
    justFrom<-reduceWeights()[,c("from", "weight")]
    justTo<-reduceWeights()[,c("to", "weight")]
    colnames(justTo)<-colnames(justFrom)
    # Bind the to and from nodes into one long data frame and sum the weights
    longWeights<-rbind(justTo, justFrom)
    totalSum<-sum(longWeights$weight)
    # Get long run probabilities by adding weights for all edges around a node 
    # then dividing by the sum of weights
    for(x in validNodes){
      LR[x, 1]<-sum(filter(longWeights, from==x)$weight)/totalSum
    }
    LR
  })
  
  # Conditionally shows long runs on button push
  showLR <- eventReactive(input$displayLR, {longRuns()})
  
  #Displays weighted graph
  output$graph<-renderPlot({plot(graph_from_data_frame(reduceWeights(), 
                                                       directed=F), 
                                 label=TRUE, 
                                 edge.label=reduceWeights()$weight)})
  
  #Displays correct transition matrix
  output$chainMat<-renderTable({displayCorrectMatrix()}, 
                               rownames=TRUE, 
                               striped=FALSE)
  
  #Sets up matrix input for transition matrix
  output$uMat<-renderUI({
    matrixInput("userMatrix", 
                matrix(rep(0, nrow(createChainMatrix())^2), 
                       nrow = nrow(createChainMatrix()), 
                       dimnames=list(rownames(createChainMatrix()), 
                                     colnames(createChainMatrix()))),  
                rows = list(names = TRUE), 
                cols = list( names = TRUE), 
                class="numeric")
  })
  
  #Sets up matrix input for long run probabilities
  output$LRuMat<-renderUI({
    matrixInput("LRuserMatrix", 
                matrix(rep(0, nrow(longRuns())), 
                       nrow = nrow(longRuns()), 
                       dimnames=list(rownames(longRuns()), c("Probability"))), 
                rows = list(names = TRUE), 
                cols = list( names = TRUE), 
                class="numeric")
  })
  
  # Long Run probabilites
  output$longRun<-renderTable({showLR()}, rownames=TRUE, striped=FALSE)
  
  #Display for whether or not transition matrix was correct
  output$correctness<-renderUI({correctnessPic(isCorrect())})
  
  #Display for whether or not transition matrix was correct
  output$correctnessLR<-renderUI({correctnessPic(isCorrectLR())})
  
  #-------------------------
  # Challenge
  #-------------------------
  # Fill in the Blank
  checkFillins <- eventReactive(input$submitFillIn, {
    output$fillinFeedback1 <- renderUI({correctnessPic(input$fillinAns1 == 
                                                         "squares on the board")})
    output$fillinFeedback2 <- renderUI({correctnessPic(input$fillinAns2 == 
                                                         "potential moves from the knight's 
                                               position")})
    output$fillinFeedback3 <- renderUI({correctnessPic(!(is.null(input$fillinAns3)) 
                                                       && input$fillinAns3 == 64)})
    output$fillinFeedback4 <- renderUI({correctnessPic(input$fillinAns4 == "vertex")})
    output$fillinFeedback5 <- renderUI({correctnessPic(input$fillinAns5 == "edge")})})
  
  correctFillInAns <- eventReactive(input$submitFillIn, {
    c(input$fillinAns1 == "squares on the board", 
      input$fillinAns2 == "potential moves from the knight's 
                                               position", 
      !(is.na(input$fillinAns3)) && input$fillinAns3 == 64,
      input$fillinAns4 == "vertex",
      input$fillinAns5 == "edge")
    
  })
  
  observeEvent(input$submitFillIn, {
    # checkFillins()
    output$fillinFeedback1 <- renderUI({correctnessPic(correctFillInAns()[1])})
    output$fillinFeedback2 <- renderUI({correctnessPic(correctFillInAns()[2])})
    output$fillinFeedback3 <- renderUI({correctnessPic(correctFillInAns()[3])})
    output$fillinFeedback4 <- renderUI({correctnessPic(correctFillInAns()[4])})
    output$fillinFeedback5 <- renderUI({correctnessPic(correctFillInAns()[5])})
    
  })
  
  # Color coded chess board
  output$chessBoard<-renderDataTable({
    board<-(matrix(c("A", "B", "C", "C", "C", "C", "B", "A",
                     "B", "C", "D", "D", "D", "D", "C", "B",
                     "C", "D", "E", "E", "E", "E", "D", "C",
                     "C", "D", "E", "E", "E", "E", "D", "C",
                     "C", "D", "E", "E", "E", "E", "D", "C",
                     "C", "D", "E", "E", "E", "E", "D", "C",
                     "B", "C", "D", "D", "D", "D", "C", "B",
                     "A", "B", "C", "C", "C", "C", "B", "A"),
                   nrow = 8
    ))
    # Assign appropriate color to each letter
    js <- "(/A/).test(value) ? '#0072B2' : (/B/).test(value) ? '#D55E00' : 
    (/C/).test(value) ? '#009E73' :(/D/).test(value) ? '#ce77a8' :
    (/E/).test(value) ? '#E69F00' :''"
    board<-datatable(board, 
                     rownames = NULL, 
                     colnames=c("","","","","","","",""),  
                     options = list(dom = 't', 
                                    bSort=FALSE, 
                                    columnDefs=list(list(className='dt-center', 
                                                         targets=0:7)))) %>% 
      formatStyle(1:ncol(board), backgroundColor = JS(js)) %>% 
      formatStyle(1:8,`border` = '.5px solid black') 
  })
  
  # Feedback for whether the number of branches for node A is correct
  outBranchA<-eventReactive(input$checkNumBranches,
                            {correctnessPic(!is.na(input$branchesA) && 
                                              input$branchesA==2)})
  output$correctBranchA<-renderUI({outBranchA()})
  
  # Feedback for whether the number of branches for node B is correct
  outBranchB<-eventReactive(input$checkNumBranches,
                            {correctnessPic(!is.na(input$branchesB) && 
                                              input$branchesB==3)})
  output$correctBranchB<-renderUI({outBranchB()})
  
  # Feedback for whether the number of branches for node C is correct
  outBranchC<-eventReactive(input$checkNumBranches,
                            {correctnessPic(!is.na(input$branchesC) && 
                                              input$branchesC==4)})
  output$correctBranchC<-renderUI({outBranchC()})
  
  # Feedback for whether the number of branches for node D is correct
  outBranchD<-eventReactive(input$checkNumBranches,
                            {correctnessPic(!is.na(input$branchesD) && 
                                              input$branchesD==6)})
  output$correctBranchD<-renderUI({outBranchD()})
  
  # Feedback for whether the number of branches for node E is correct
  outBranchE<-eventReactive(input$checkNumBranches,
                            {correctnessPic(!is.na(input$branchesE) && 
                                              input$branchesE==8)})
  output$correctBranchE<-renderUI({outBranchE()})
  
  # Feedback for whether the sum of all weights is correct
  sumCheck<-eventReactive(input$checkSum,{
    correctnessPic(!is.na(input$sumWeights) && input$sumWeights==336)})
  output$correctSum<-renderUI({sumCheck()})
  
  # Feedback for whether the sum of all weights is correct
  sumCheckText<-eventReactive(input$checkSum,{
    correctnessText(!is.na(input$sumWeights) &&input$sumWeights==336)})
  output$correctSumText<-renderText({sumCheckText()})
  
  # Check final answer for node A
  aCheck <-eventReactive(input$checkFinAns,{
    correctnessPic(!is.na(input$chessAnsA) && input$chessAnsA==168)})
  output$ansA <-renderUI({aCheck()})
  
  # Check final answer for node B
  bCheck <-eventReactive(input$checkFinAns,{
    correctnessPic(!is.na(input$chessAnsB) && input$chessAnsB==112)})
  output$ansB <-renderUI({bCheck()})
  
  # Check final answer for node C
  cCheck <-eventReactive(input$checkFinAns,{
    correctnessPic(!is.na(input$chessAnsC) && input$chessAnsC==84)})
  output$ansC <-renderUI({cCheck()})
  
  # Check final answer for node D
  dCheck <-eventReactive(input$checkFinAns,{
    correctnessPic(!is.na(input$chessAnsD) && input$chessAnsD==56)})
  output$ansD <-renderUI({dCheck()})
  
  # Check final answer for node E
  eCheck <-eventReactive(input$checkFinAns,{
    correctnessPic(!is.na(input$chessAnsE) && input$chessAnsE==42)})
  output$ansE <-renderUI({eCheck()})
  
  # Move between tabs using the Next button
  observeEvent(input$next0, {updateTabsetPanel(session, 
                                               "challengeTabs", 
                                               selected="pt1")})
  observeEvent(input$next1, {updateTabsetPanel(session, 
                                               "challengeTabs", 
                                               selected="pt2")})
  
  # Table of numbers of branches and number of occurences 
  output$table<-renderTable({matrix(c(4,2,8,3,20,4,16,6,16,8), 
                                    nrow = 2, 
                                    dimnames = list(c("Number of Nodes", 
                                                      "Number of Branches per Node"), 
                                                    c("A","B","C","D","E")))}, 
                            rownames=T, 
                            striped=F)
  
  # FOR CHESSBOARD SIMULATION
  
  # Defines a move for the knight
  # Input: current location of the knight
  # Output: new location for the knight
  move<-function(point){
    newPoint<-c(0,0) # Initialization to allow loop to start
    rand<-sample(1:8, size=8, replace=FALSE) # Random ordering of numbers 1-8 
    # to define moves
    index<-1 # Allow to walk through rand in case first n points are invalid
    while(newPoint[1]>8||newPoint[1]<1||newPoint[2]>8||newPoint[2]<1){
      # Define a new location for each number (starts at up 2 left 1 then works 
      # around clockwise)
      # Loop breaks when the new point lies on the chessboard
      if(rand[index]==1){
        newPoint[1]<-point[1]-1
        newPoint[2]<-point[2]+2
      }
      else if(rand[index]==2){
        newPoint[1]<-point[1]+1
        newPoint[2]<-point[2]+2
      }
      else if(rand[index]==3){
        newPoint[1]<-point[1]+2
        newPoint[2]<-point[2]+1
      }
      else if(rand[index]==4){
        newPoint[1]<-point[1]+2
        newPoint[2]<-point[2]-1
      }
      else if(rand[index]==5){
        newPoint[1]<-point[1]+1
        newPoint[2]<-point[2]-2
      }
      else if(rand[index]==6){
        newPoint[1]<-point[1]-1
        newPoint[2]<-point[2]-2
      }
      else if(rand[index]==7){
        newPoint[1]<-point[1]-2
        newPoint[2]<-point[2]-1
      }
      else{
        newPoint[1]<-point[1]-2
        newPoint[2]<-point[2]+1
      }
      
      index<-index+1
    }
    newPoint
    
  }
  
  # Defines current location of the knight and the knight's target
  point<-reactiveValues(x=1, y=1)
  target<-reactiveValues(x=1, y=1)
  steps<-reactiveValues(count=0)
  simulate<-reactiveValues(showText=F)
  
  # Action from clicking step button
  observeEvent(input$step, {
    newLoc<-move(c(point$x, point$y))
    point$x<-newLoc[1]
    point$y<-newLoc[2]
    steps$count<-steps$count+1
    simulate$showText<-F
  })
  
  # Reset for step counter
  observeEvent(input$resetStep,{
    steps$count<-0
  })
  
  # Prints number of steps
  output$stepCount<-renderText(paste("Number of Steps: ", steps$count))
  
  # Simulate and print number of jumps to return to target
  myText<-eventReactive(input$push,{
    n<-1
    loc<-move(c(point$x, point$y))
    while(!(loc[1]==target$x && loc[2]==target$y)){
      loc<-move(loc)
      n<-n+1
    }
    point$x<-loc[1]
    point$y<-loc[2]
    paste("Number of jumps to return to target: ",n)
  })
  
  # Outputs number of times it took the knight to return to its target
  output$text<-renderText({    
    if(simulate$showText){
      myText()}
    else{""}})
  
  # If simulate button is pushed, make sure text giving result is shown
  observeEvent(input$push,{simulate$showText<-TRUE})
  
  # Plot the chessboard
  output$ggstyle<-renderPlot({
    df<-data.frame(x=point$x, y=point$y, targetX=target$x, targetY=target$y)
    plot<-ggplot(aes(x=x,y=y), data=df) +
      geom_point(aes(x=targetX, y=targetY), 
                 color="#009E73", 
                 shape="square", 
                 size=21, 
                 alpha=1/2) +
      geom_point(shape="\u265E", size=15) + #\u265E is the unicode character 
      # for the knight
      # creates correct axes (without extra room after bounds like xlim and ylim)
      scale_x_continuous(limits=c(.5, 8.5), expand = c(0, 0), breaks=1:8) +
      scale_y_continuous(limits=c(.5, 8.5), expand = c(0, 0), breaks=1:8) +
      xlab("") +
      ylab("") +
      # Creates gridlines for chessboard by plotting lines
      geom_hline(aes(yintercept=1.5)) +
      geom_hline(aes(yintercept=2.5)) +
      geom_hline(aes(yintercept=3.5)) +
      geom_hline(aes(yintercept=4.5)) +
      geom_hline(aes(yintercept=5.5)) +
      geom_hline(aes(yintercept=6.5)) +
      geom_hline(aes(yintercept=7.5)) +
      geom_vline(aes(xintercept=1.5)) +
      geom_vline(aes(xintercept=2.5)) +
      geom_vline(aes(xintercept=3.5)) +
      geom_vline(aes(xintercept=4.5)) +
      geom_vline(aes(xintercept=5.5)) +
      geom_vline(aes(xintercept=6.5)) +
      geom_vline(aes(xintercept=7.5)) +
      theme_bw() + 
      # Keep real grid lines from being visible
      theme(panel.grid.minor = element_line(colour="white", size=0.5),
            panel.grid.major = element_line(colour="white", size=0.5),
            axis.text = element_text(size=18)
      ) 
    plot
  }, height = 400, width = 400)
  
  # Defines click event for the graph; point and target become that point 
  # (rounded to be in a square)
  observeEvent(input$start, {
    point$x<-round(input$start$x)
    point$y<-round(input$start$y)
    target$x<-round(input$start$x)
    target$y<-round(input$start$y)
    steps$count<-0
    simulate$showText<-F
  })
  
  # Alt Text
  output$chessBoardAlt <- renderUI({
    tags$script(HTML(
      paste0("$(document).ready(function() {
                        document.getElementById('ggstyle').setAttribute('aria-label',
                        `This image represents a chessboard. With numeric labels 
                        starting at the bottom lefthand corner, the knight's current
                        position is (", point$x, ",", point$y ,")`)
                      })")
    ))
  })
}

# Create Shiny App using BOAST App template
boastUtils::boastApp(ui = ui, server = server)

