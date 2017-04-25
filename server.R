library(shiny)
library(broom)
library(pROC)
library(ROCR)
library(googleVis)
library(plotly)
suppressPackageStartupMessages(library(googleVis))

fdata1 <- read.csv("~/Text Analysis Results.csv", sep = ",")
df <- read.csv("~/data6242_SMOTE.csv", stringsAsFactors = FALSE)
df_test <- read.csv("~/data6242_test.csv", stringsAsFactors = FALSE)

la <- df$loan_amnt
t <- df$term
ir <- df$int_rate
el <- df$emp_length
ho <- df$home_ownership
ai <- df$annual_inc
vs <- df$verification_status
p <- df$purpose
d <- df$dti
d2y <- df$delinq_2yrs
i6m <- df$inq_last_6mths
pr <- df$pub_rec
rb <- df$revol_bal
ru <- df$revol_util
ta <- df$total_acc
ils <- df$initial_list_status
tri <- df$total_rec_int
trlf <- df$total_rec_late_fee
cem <- df$collections_12_mths_ex_med
and <- df$acc_now_delinq
cwm <- df$chargeoff_within_12_mths
da <- df$delinq_amnt
prb <- df$pub_rec_bankruptcies
tl <- df$tax_liens
fa <- df$fico_avg
ywch <- df$years_w_credit_hist

model <- glm(df$is_bad ~ la+t+ir+el+ho+ai+vs+p+d+d2y+i6m+pr+rb+ru+ta+ils+tri+trlf+
               cem+and+cwm+da+prb+tl+fa+ywch, family=binomial)




shinyServer(function(input, output) {
  
  test_pred <- reactive({
    
    la <- df_test$loan_amnt
    t <- df_test$term
    ir <- df_test$int_rate
    el <- df_test$emp_length
    ho <- df_test$home_ownership
    ai <- df_test$annual_inc
    vs <- df_test$verification_status
    p <- df_test$purpose
    d <- df_test$dti
    d2y <- df_test$delinq_2yrs
    i6m <- df_test$inq_last_6mths
    pr <- df_test$pub_rec
    rb <- df_test$revol_bal
    ru <- df_test$revol_util
    ta <- df_test$total_acc
    ils <- df_test$initial_list_status
    tri <- df_test$total_rec_int
    trlf <- df_test$total_rec_late_fee
    cem <- df_test$collections_12_mths_ex_med
    and <- df_test$acc_now_delinq
    cwm <- df_test$chargeoff_within_12_mths
    da <- df_test$delinq_amnt
    prb <- df_test$pub_rec_bankruptcies
    tl <- df_test$tax_liens
    fa <- df_test$fico_avg
    ywch <- df_test$years_w_credit_hist
    
    df_pred <- predict(model, newdata = data.frame(la, t, ir, el, ho, ai, vs, p, d, d2y, i6m, pr,
                                                   rb, ru, ta, ils, tri, trlf, cem, and, cwm, da,
                                                  prb, tl, fa, ywch), type = "response")
  
  })
  
  plot_auc <- reactive({
    df_pred <- test_pred()
    auc <- roc(df_test$is_bad, df_pred)
    plot(auc, main = "ROC CURVE", print.auc = TRUE)
    
  })
  
  rel_numv<- reactive({
    
    mod <- tidy(model)
    
    cols <- c("la", "ir", "ai", "d", "d2y", "i6m", "pr", "rb", "ru", "ta", "tri", "trlf",
              "cem", "and", "cwm", "da", "prb", "tl", "fa", "ywch")
    
    
    input_values <- c(input$loan_amnt, input$int_rate, input$annual_inc, input$dti,
                      input$delinq_2yrs, input$inq_last_6mths, input$pub_rec, input$revol_bal,
                      input$revol_util, input$total_acc, input$total_rec_int, input$total_rec_late_fee,
                      input$collections_12_mths_ex_med, input$acc_now_delinq, input$chargeoff_within_12_mths,
                      input$delinq_amnt, input$pub_rec, input$tax_liens, input$fico_avg,
                      input$years_w_credit_hist)
    
    x_values <- exp(mod$estimate[mod$term %in% cols]*input_values)
    
    terms <- c("Loan Amount", "Interest Rate", "Annual Income", "Debt-Income ratio", 
               "Delinquencies", "Inquiries", "Derogatory records",
               "Revolving balance", "Revolving utilization","Credit lines", "Received Interest",
               "Received late fees", "Collections excl Medical", "Delinquent accounts",
               "Charge-offs", "Delinquent Amount", "Public rec. bankruptcies",
               "Tax Liens", "Fico Score", "Years w/ credit history")
    
    
    p <- plot_ly(x = x_values, y = terms, type = 'bar', orientation = 'h', color = x_values < 1) %>%
      layout(title = "Contribution of Numeric attributes", showlegend = FALSE, margin = list(l=150))
    p
  })
  
  rel_purpose <- reactive({
    
    mod <- tidy(model)
    input_val <- input$purpose
    y_values <- c(0, mod$estimate[grepl("year", mod$term)])
    terms <- c("credit_card", "car", "small_business", "wedding", "debt_consolidation",
               "home_improvement", "major_purchase", "medical", "moving", "vacation",
               "house")
    p <- plot_ly(x = terms, y = y_values, type = "scatter", mode =  "markers", 
                 color = grepl(input_val, terms), marker=list( size=30 , opacity=0.5), 
                 colors = c("orange", "red"))%>%
      layout(title = "Contribution of Loan Purpose to Default Risk", showlegend = FALSE, margin = list(b=200))
    p
  })
  
  rel_empl <- reactive({
    
    mod <- tidy(model)
    input_val <- input$emp_length
    y_values <- c(0, mod$estimate[grepl("el", mod$term)])
    terms <- c("< 1 year", "1 year", "2 years", "3 years", "4 years", 
               "5 years", "6 years", "7 years", "8 years",
               "9 years", "10+ years", "n/a")
    p <- plot_ly(x = terms, y = y_values, type = "scatter", mode =  "markers", 
                 color = input_val == terms, marker=list( size=30 , opacity=0.5), 
                 colors = c("orange", "red"))%>%
      layout(title = "Contribution of Employment Length to Default Risk", showlegend = FALSE, margin = list(b=200))
    p
  })
  
  measurePrecision <- reactive({
    actual_labels <- df_test$is_bad
    df_pred <- test_pred()
    precision <- sum(as.numeric(df_pred>0.5) & actual_labels)/sum(as.numeric(df_pred>0.5))
    precision
    
  })
  
  measureRecall <- reactive({
    actual_labels <- df_test$is_bad
    df_pred <- test_pred()
    recall <- sum(as.numeric(df_pred>0.5) & actual_labels)/sum(actual_labels)
    recall
    
  })
  
  measureAccuracy <- reactive({
    actual_labels <- df_test$is_bad
    df_pred <- test_pred()
    cm = as.matrix(table(actual_labels, as.numeric(df_pred>0.5)))
    accuracy <- sum(diag(cm))/sum(cm)
    accuracy
    
  })
  
  pred_result <- eventReactive( input$submit, {
    
    la <- input$loan_amnt
    t <- input$term
    ir <- input$int_rate
    el <- input$emp_length
    ho <- input$home_ownership
    ai <- input$annual_inc
    vs <- input$verification_status
    p <- input$purpose
    d <- input$dti
    d2y <- input$delinq_2yrs
    i6m <- input$inq_last_6mths
    pr <- input$pub_rec
    rb <- input$revol_bal
    ru <- input$revol_util
    ta <- input$total_acc
    ils <- input$initial_list_status
    tri <- input$total_rec_int
    trlf <- input$total_rec_late_fee
    cem <- input$collections_12_mths_ex_med
    and <- input$acc_now_delinq
    cwm <- input$chargeoff_within_12_mths
    da <- input$delinq_amnt
    prb <- input$pub_rec_bankruptcies
    tl <- input$tax_liens
    fa <- input$fico_avg
    ywch <- input$years_w_credit_hist
    
    result <- predict(model, newdata = data.frame(la, t, ir, el, ho, ai, vs, p, d, d2y, i6m, pr,
                                                  rb, ru, ta, ils, tri, trlf, cem, and, cwm, da,
                                                  prb, tl, fa, ywch), type = "response")
    result
  })
  
  
  
  output$res <- renderText({ 
    paste("The borrower has a ", round(pred_result()*100, 2), " % probability of defaulting!")
  })
  
  output$accuracy <- renderGvis({
    df1 <- data.frame(Label = "Model Accuracy", Value = measureAccuracy()*100)
    gvisGauge(df1,
              options=list(min=0, max=100, greenFrom=70,
                           greenTo=100, yellowFrom=30, yellowTo=70,
                           redFrom=0, redTo=30, width=170, height=170));  
    
  })
  
  output$precision <- renderGvis({
    df1 <- data.frame(Label = "Model Precision", Value = measurePrecision()*100)
    gvisGauge(df1,
              options=list(min=0, max=100, greenFrom=70,
                           greenTo=100, yellowFrom=30, yellowTo=70,
                           redFrom=0, redTo=30, width=170, height=170));  
    
  })
  
  output$recall <- renderGvis({
    df1 <- data.frame(Label = "Model Recall", Value = measureRecall()*100)
    gvisGauge(df1,
              options=list(min=0, max=100, greenFrom=70,
                           greenTo=100, yellowFrom=30, yellowTo=70,
                           redFrom=0, redTo=30, width=170, height=170));  
    
  })
  
  output$auc <- renderPlot({
    plot_auc()
  })
  
  output$rel_numv <- renderPlotly({
    rel_numv()
  })
  
  output$rel_purpose <- renderPlotly({
    rel_purpose()
  })
  
  output$rel_empl <- renderPlotly({
    rel_empl()
  })
  
  text_result <- eventReactive(input$submit,  {
    desc <- input$desc
    is_desc <- !(desc == "")
    desc_len <- nchar(desc)
    words <- unlist(strsplit(desc, "[,. ]+"))
    desc_count <- length(words)
    desc_avg <- desc_len/desc_count
    credit <- is.element("credit", words)
    loan <- is.element("loan", words)
    debt <- is.element("debt", words)
    interest <- is.element("interest", words)
    card <- is.element("card", words)
    if (card == 0) {
      card <- is.element("cards", words)
    }
    consolidate <- is.element("consolidate", words)
    payment <- is.element("payment", words)
    rate <- is.element("rate", words)
    if (rate == 0) {
    rate <- is.element("rates", words)
    }
    high <- is.element("high", words)
    pay <- is.element("pay", words)
    time <- is.element("time", words)
    years <- is.element("years", words)
    monthly <- is.element("monthly", words)
    bills <- is.element("bills", words)
    home  <- is.element("home", words)
    job  <- is.element("job", words)
    money  <- is.element("money", words)
    lower <- is.element("lower", words)
    consolidation  <- is.element("consolidation", words)
    plan <- is.element("plan", words)
    good  <- is.element("good", words)
    free  <- is.element("free", words)
    car  <- is.element("car", words)
    never  <- is.element("never", words)
  
    varis <- unlist(list("presence of a description", "description length", "word count", "average word length", "presence of the word credit", "presence of the word debt", "presence of the word card(s)", "presence of the word payment", "presence of the word high", "presence of the word time", "presence of the word monthly", "presence of the word home", "presence of the word money", "presence of the word consolidation", "presence of the word good", "presence of the word car", "presence of the word loan", "presence of the word interest", "presence of the word consolidate", "presence of the word rate(s)", "presence of the word pay", "presence of the word years", "presence of the word bills", "presence of the word job", "presence of the word lower", "presence of the word plan", "presence of the word free", "presence of the word never"))
    contributors <- unlist(list(fdata1$is_desc[1]*is_desc , fdata1$desc_len[1]*desc_len , fdata1$desc_count[1]*desc_count , fdata1$desc_avg[1]*desc_avg , fdata1$word_credit[1]*credit , fdata1$word_debt[1]*debt , fdata1$word_card.s.[1]*card , fdata1$word_payment[1]*payment , fdata1$word_high[1]*high , fdata1$word_time[1]*time , fdata1$word_monthly[1]*monthly , fdata1$word_home[1]*home , fdata1$word_money[1]*money , fdata1$word_consolidation[1]*consolidation , fdata1$word_good[1]*good , fdata1$word_car[1]*car , fdata1$word_loan[1]*loan , fdata1$word_interest[1]*interest , fdata1$word_consolidate[1]*consolidate , fdata1$word_rate.s.[1]*rate , fdata1$word_pay[1]*pay , fdata1$word_years[1]*years , fdata1$word_bills[1]*bills , fdata1$word_job[1]*job , fdata1$word_lower[1]*lower , fdata1$word_plan[1]*plan , fdata1$word_free[1]*free , fdata1$word_never[1]*never))
    change <- format(round(sum(contributors)*100, 3), nsmall = 3)
    selected <- sort(contributors)
    topindex <- order(contributors, decreasing = T)[1]
    botindex <- order(contributors) [1]
    topv <- varis[topindex]
    botv <- varis[botindex]
    topn <- contributors[topindex]
    botn <- contributors[botindex]
    topn1 <- format(round(topn*100, 3), nsmall = 3)
    botn1 <- format(round(-botn*100, 3), nsmall = 3)
    result <- paste("The total change to your expected rate of default is ", change, "%. The top positive contributor is the ", toString(topv), ", increasing it by a value of ", toString(topn1), "%. ", "The top negative contributor is the ", toString(botv), ", decreasing it by a value of ", toString(botn1), "%.", sep = "")
    result
  })
  output$textb <- renderText({text_result()})
    })

