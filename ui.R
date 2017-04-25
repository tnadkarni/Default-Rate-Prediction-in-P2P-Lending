library(shiny)
shinyUI(fluidPage(
  titlePanel("Loan Rangers Project"),
            sidebarLayout(sidebarPanel(
        
                numericInput("loan_amnt", "Loan Amount Desired", value = 11000),
                selectInput("term", "Loan Term",
                            c("36months", "60months"), selected = "36 months"),
                numericInput("years_w_credit_hist", "Years with credit history", value = 20),
                sliderInput("int_rate", "Interest rate assigned by LC", 5, 25, 12, ticks = FALSE, round = 2),
                selectInput("emp_length", "How many years employed",
                            c("< 1 year", "1 year", "2 years", "3 years", "4 years", 
                              "5 years", "6 years", "7 years", "8 years",
                              "9 years", "10+ years", "n/a"), selected = "< 1 year"),
                selectInput("home_ownership", "Home Ownership",
                            c("MORTGAGE", "RENT", "OWN"), selected = "RENT"),
                radioButtons("initial_list_status" , "Initial List Status of Loan",
                                   choices = c("Whole" = "w", "Fractional" = "f"),  selected = "w"),
                numericInput("annual_inc", "Annual Income", value = 75000),
                radioButtons("verification_status", "Verified by LC?", 
                             choices = c("Source Verified", "Verified", "Not Verified"), selected = "Verified"),
                sliderInput("dti", "Debt to Income Ratio", 0, 30, 13, round = 2),
                sliderInput("fico_avg", "Average Fico Score", 300, 850, 500, round = 0),
                numericInput("delinq_2yrs", "Number of Delinquencies in last 2 years", value = 0),
                numericInput("inq_last_6mths", "Number of Inquiries in last 6 months", value = 0),
                numericInput("open_acc", "Number of open credit lines", value = 10),
                numericInput("total_acc", "Total number of credit lines", value = 25),
                numericInput("pub_rec", "Number of derogatory public records", value = 0),
                numericInput("revol_bal", "Total revolving credit balance", value = 16000),
                sliderInput("revol_util", "Revolving line utilization rate", 0, 100, 54, ticks = FALSE, round = 2),
                numericInput("total_rec_int", "Total received interest", value = 2000),
                numericInput("total_rec_late_fee", "Total received late fees", value = 0),
                numericInput("collections_12_mths_ex_med", "Total collections last 12 months excluding medical", value = 0),
                numericInput("tax_liens", "Number of tax liens", value = 0),
                numericInput("acc_now_delinq", "Number of delinquent accounts", value = 0),
                numericInput("delinq_amnt", "Amount owed on delinquent accounts", value = 0),
                numericInput("chargeoff_within_12_mths", "Number of charge-offs in the last 12 months", value = 0),
                numericInput("pub_rec_bankruptcies", "Number of public record bankruptcies", value = 0),
               radioButtons("purpose", "Purpose of Loan", choices = c("Credit Card" = "credit_card", "Car" = "car", "Small Business" = "small_business",
                                                                       "Wedding" = "wedding", "Debt Consolidation" = "debt_consolidation",
                                                                       "Home Improvement" = "home_improvement", "Major Purchase" = "major_purchase",    
                                                                       "Medical" = "medical", "Moving" = "moving","Vacation" = "vacation",
                                                                       "House" = "house"), selected = "credit_card"),
                textInput("desc", "Loan Description", value="", placeholder = "What do you need the money for?"),
               actionButton("submit", "Submit")
    
              ),
            mainPanel(
              tabsetPanel(
              tabPanel("Model Info", div(plotOutput("auc"), align = "center"), column(4, htmlOutput("accuracy")), 
                       column(4, htmlOutput("precision")),column(4, htmlOutput("recall"))), 
              tabPanel("Default Risk",h3(textOutput("res")), plotlyOutput("rel_numv"),
                       plotlyOutput("rel_purpose"), plotlyOutput("rel_empl"), span(textOutput("textb"), style="color:blue"))
              )
            ))))
      
  
  
  
  
  
  
  
  
  
  
  
  
  
  