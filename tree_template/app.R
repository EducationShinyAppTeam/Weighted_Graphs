library(shiny)
library(shinyMatrix)
library(matrixcalc)
library(igraph)
library(tidyr)
library(dplyr)
library(shinyjs)

# List of all node names (for generalizability)
# Order: root, 3 potential direct children of root, 9 leaf nodes (all left to right)
# In cases of having only two children, the right node is ignored
nodeNames<-c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")

# Define UI 
ui <- fluidPage(
    # Title
    titlePanel("Probability Tree Sample Code"),
    tags$style(type="text/css", "#view tr:last-child {font-weight:bold;}"),
    
    
    sidebarLayout(
        
        sidebarPanel(
            # Number of children for each of the nodes eligible to have children
            selectInput("nchildA", paste("Number of children for node ", nodeNames[1]), choices=c(2,3)),
            selectInput("nchildB", paste("Number of children for node ", nodeNames[2]), choices=c(2,3)),
            selectInput("nchildC", paste("Number of children for node ", nodeNames[3]), choices=c(2,3), width='60px'),
            #tags$head(tags$style(HTML(".selectize-input {height: 15px; width: 60px; font-size: 16px;}"))),
            conditionalPanel(
                condition = "input.nchildA==3",
                selectInput("nchildD", paste("Number of children for node ", nodeNames[4]), choices=c(2,3))),
            "Choose a probability for each given location. All probabilities will be rounded to 2 decimal places and are read from
            left to right in the tree.",
            uiOutput("uMat"), # Probabilities for each transition
            textOutput("warning"), # Alerts user if they enter a probability in an unused location
            uiOutput("uGMat"), # Takes in users probabilities for reaching each leaf node
            actionButton("check", "Check Answer"), # Checks answer
            
            # Output based on correctness of answer
            fluidRow(
                column(width=2,uiOutput("correctnessPic")),
                column(width=10,textOutput("correctnessText"))
            ),
            width = 4
        ),
        
        # Outputs: plot of states visited and the matrix to the n-steps power
        mainPanel(
            plotOutput("graph"),
            checkboxInput("displayProbs", "Display Correct Probabilities"),
            
            # Ability to combine multiple leaves
            uiOutput("uComb"), 
            numericInput("comboProb", "Combined Probability", value=0, min=0, max=1, step=.01),
            actionButton("comboCheck", "Check Your Answer"), # Checks answer
            fluidRow(
              column(width=1,uiOutput("correctnessPicCombo")),
              column(width=11,textOutput("correctnessTextCombo"))
            ),
            actionButton("showAns", "Show Answer"),
            textOutput("answer"),
            
            width=8
        )
    )
    
)

server<-function(input, output, session) {
    
    #Matrix Structure for the initial input
    baseMat<-reactive({
        if(input$nchildA==2 && input$nchildB ==2 && input$nchildC==2){
            matrix(c(rep(1,3),rep(0,3)), nrow=3, dimnames = list(nodeNames[1:3],c("Child 1", "Child 2")))
        }
        else if(input$nchildA==2){
            matrix(c(rep(1,3),rep(0,6)), nrow=3, dimnames = list(nodeNames[1:3],c("Child 1", "Child 2", "Child 3")))
        }
        else{
            matrix(c(rep(1,4),rep(0,8)), nrow=4, dimnames = list(nodeNames[1:4],c("Child 1", "Child 2", "Child 3")))
        }
    })
    
    # Gets all nodes that are currently not in the tree
    irrelevantNodes<-reactive({
        irrelevantNumbers<-c()
        if(input$nchildA==2){
            irrelevantNumbers<-c(irrelevantNumbers, 4, 11:13)
        }
        else if(input$nchildD==2){
            irrelevantNumbers<-c(irrelevantNumbers, 13)
        }
        if(input$nchildB==2){
            irrelevantNumbers<-c(irrelevantNumbers, 7)
        }
        if(input$nchildC==2){
            irrelevantNumbers<-c(irrelevantNumbers, 10)
        }
        irrelevantNumbers
    })
    
    # Gets numbers of leaf nodes:
    leafNodes <- reactive({
        leaves<-c(5,6,8,9)
        if(input$nchildB==3){
            leaves<-c(leaves, 7)
        }
        if(input$nchildC==3){
            leaves<-c(leaves, 10)
        }
        if(input$nchildA==3){
            leaves<-c(leaves, 11,12)
            if(input$nchildD==3){
                leaves<-c(leaves, 13)
            }
        }
        leaves
        
    })
    
    # Gives all weights as a single vector
    weights<- reactive({
        weight<-c(input$probabilities[1,], input$probabilities[2,], input$probabilities[3,])
        if(ncol(input$probabilities)==3){
            if(nrow(input$probabilities)==4){
                weight<-c(weight, input$probabilities[4,])
            }
            else{
                weight<-c(weight, rep(0,3))
            }}
            round(weight,2)
    })
    
    # Defines the matrix for user to define edge weights
    output$uMat<-renderUI({
        matrixInput("probabilities", baseMat(), rows = list(names = TRUE), cols = list( names = TRUE), class = "numeric")
    })
    
    # Defines the matrix for user to input leaf node probabilities
    output$uGMat<-renderUI({
            matrixInput("userGuesses", matrix(rep(0,length(leafNodes())), nrow=length(leafNodes()), dimnames = list(nodeNames[sort(leafNodes())],c("Probability of Reaching the State"))), rows = list(names = TRUE), cols = list( names = TRUE), class = "numeric")
    })
    
    # Defines warning message for when the user enters values into unused locations
    output$warning<-renderText({
        if(ncol(input$probabilities)==3)
            {lengths<-c(as.numeric(input$nchildA), as.numeric(input$nchildB), as.numeric(input$nchildC))
        if(lengths[1]==3){
            lengths<-c(lengths, as.numeric(input$nchildD))
        }
        
        message<-""
        for(i in 1:nrow(input$probabilities)){
            if(lengths[i]==2 && input$probabilities[i, 3]!=0){
                message<-c(message, paste("Note: Node ", nodeNames[i], "has only 2 children. Any positive probability placed on the third child will be ignored.\n"))
            }
        }
        message
        }
        
    })
    
    # Checks whether the current inputted probabilities are valid. 
    isValidInput<-reactive({
        if(min(input$probabilities)>=0 && max(input$probabilities)<=1){
        lengths<-c(as.numeric(input$nchildA), as.numeric(input$nchildB), as.numeric(input$nchildC))
        if(lengths[1]==3){
            lengths<-c(lengths, as.numeric(input$nchildD))
        }
        isValidInput<-TRUE
        for(i in 1:length(lengths)){
            if(sum(input$probabilities[i,1:lengths[i]])!=1){
                isValidInput<-FALSE
            }
        }
        }
        else{isValidInput=FALSE}
        isValidInput
    })
    
    # Creates data frame for the graph
    makeGraphDataFrame<-reactive({
        validate(
            need(isValidInput(), "Probabilities at each node (each matrix row) must add to 1 and be individually between 0 and 1.")
        )
        # Starting case (edges that always exist)
        from<-c(nodeNames[1],nodeNames[1],nodeNames[2],nodeNames[2],nodeNames[3],nodeNames[3]) 
        to<-c(nodeNames[2],nodeNames[3],nodeNames[5],nodeNames[6],nodeNames[8],nodeNames[9])
        
        # Add edges that exist for specific cases
        if(input$nchildB==3){
            from<-c(from, nodeNames[2])
            to<-c(to, nodeNames[7])
        }
        if(input$nchildC==3){
            from<-c(from, nodeNames[3])
            to<-c(to, nodeNames[10])
        }
        if(input$nchildA==3){
            from<-c(from, nodeNames[1], nodeNames[4], nodeNames[4])
            to<-c(to, nodeNames[4], nodeNames[11], nodeNames[12])
            if(input$nchildD==3){
                from<-c(from, nodeNames[4])
                to<-c(to, nodeNames[13])
            }
        }

        # Create weights to go with each edge
        weight <- weights()
            if(length(irrelevantNodes())>0 && ncol(input$probabilities)==3){
                badNodes<-irrelevantNodes()-1
                weight<-weight[-(irrelevantNodes()-1)]
            }
        # Adjusts the vertex labels for the leaf nodes if weights are being shown    
        if(input$displayProbs){
            weightIndex<-1
            for(i in 1:length(to)){
                if(to[i] %in% nodeNames[leafNodes()]){
                    to[i]<-paste("\n", to[i], "\n", correctProbabilities()[weightIndex,1])
                    weightIndex<-weightIndex+1
                }
            }
            
        }
        
        # Make actual data frame
        weight<-round(weight, 2)
        df<-data.frame(from=from, to=to)
        df<-df[order(df$from),]
        df$weight<-weight
        df
    })
    
    
    # Create layout for output graph
    layout<-reactive({layout_as_tree(graph_from_data_frame(makeGraphDataFrame()),root=1)})
    
    # Creates output graph
    output$graph<-renderPlot({plot(graph_from_data_frame(makeGraphDataFrame(), directed=F), label=TRUE, edge.label=makeGraphDataFrame()$weight, layout=layout(), asp=.2, vertex.size=8)})
    
    # Determines whether the transition matrix is correct
    isCorrect<-eventReactive(input$check,{
        user<-round(input$userGuesses[,1],2)
        correct<-round(correctProbabilities(), 2)
        correct<-correct[order(rownames(correct)),]
        
        isSame=TRUE
        for(i in 1:length(correctProbabilities())){
            if(user[i]!=correct[i]){
                isSame=FALSE
            }
        }
        isSame
        
    })
    
    # Puts checkmark or X depending on if answer was correct
    output$correctnessPic<-renderUI({
        if(isCorrect()){
            img(src = "check.PNG",width = 30) 
            #"Good Job! You are Correct!"
        }
        else{
            img(src = "cross.PNG",width = 30)
            #"Check Your Work for errors."
        }
    })
    
    # Writes either good job or check work depending on if answer was correct
    output$correctnessText<-renderText({
        if(isCorrect()){
            "Good Job! You are Correct!"
        }
        else{
            "Check Your Work for Errors."
        }
    })
    
    # Calculates the correct probabilities
    correctProbabilities<- reactive({
        probs<-c()
        # Case where all nodes have only two children
        if(ncol(input$probabilities)==2){
            probs<-c(weights()[1]*weights()[3],weights()[1]*weights()[4], weights()[2]*weights()[5],weights()[2]*weights()[6])
        }
        
        # Case where at least one node has three children
        else{
        for(leaf in leafNodes()){
            probs<-c(probs, weights()[leaf-1]*weights()[as.integer((leaf-2)/3)])
        }
        }
        matrix(probs, nrow=length(leafNodes()), ncol=1, dimnames=list(nodeNames[leafNodes()], NULL))
        
    })
    
    # Calculates correct probability of combination of states
    calcCombo<-reactive({
      sum(correctProbabilities()[as.numeric(input$leafChoices),1])
    })
    
    # Creates list for the choices for leaf nodes in uComb
    choices<-reactive({
      choices<-1:length(leafNodes())
      names(choices)<-nodeNames[leafNodes()]
      choices
    })
    
    # Allows users to select nodes to get probability
    output$uComb<-renderUI({
      selectInput("leafChoices", "Choose Good Nodes:", choices=choices(), multiple=TRUE)
    })
    
    isCorrectCombo<-eventReactive(input$comboCheck,{
      round(input$comboProb,2)==round(calcCombo(),2)
    })
    
    # Puts checkmark or X depending on if answer was correct
    output$correctnessPicCombo<-renderUI({
      if(isCorrectCombo()){
        img(src = "check.PNG",width = 30) 
        #"Good Job! You are Correct!"
      }
      else{
        img(src = "cross.PNG",width = 30)
        #"Check Your Work for errors."
      }
    })
    
    # Writes either good job or check work depending on if answer was correct
    output$correctnessTextCombo<-renderText({
      if(isCorrectCombo()){
        "Good Job! You are Correct!"
      }
      else{
        "Check Your Work for Errors."
      }
    })
    
    # Show answer
    ans<-eventReactive(input$showAns,{calcCombo()})
    output$answer<-renderText({ans()})
}
# Run the application 
shinyApp(ui = ui, server = server)
