require(dplyr)
if (!require('shiny')){install.packages("shiny");require(shiny)}
if (!require('ggplot2')) install.packages("ggplot2")


#load("roc_elastic.Rda")
#load("roc_elastic_smote.Rda")
#load("roc_logistic.Rda")
#load("roc_logistic_smote.Rda")
#roc_elastic_data_smote = cbind(rocOutSmote$thresholds,rocOutSmote$sensitivities,rocOutSmote$specificities)

roc_elastic_data = cbind(rocOut$thresholds,rocOut$sensitivities,rocOut$specificities)

#elastic no smote
colnames(roc_elastic_data) = c("thresholds","sensitivities","specificities")
roc_elastic_data = as.data.frame(roc_elastic_data)

# #elastic smote
# colnames(roc_elastic_smote_data) = c("thresholds","sensitivities","specificities")
# roc_elastic_smote_data = as.data.frame(roc_elastic_smote_data)

# #logistic no smote
# colnames(roc_logistic_data) = c("thresholds","sensitivities","specificities")
# roc_logistic_data = as.data.frame(roc_logistic_data)
# 
# #logistic smote
# colnames(roc_logistic_smote_data) = c("thresholds","sensitivities","specificities")
# roc_logistic_smote_data = as.data.frame(roc_logistic_smote_data)



##test only
roc_elastic_smote_data = roc_elastic_data
roc_elastic_smote_data$sensitivities = 0.86*roc_elastic_data$sensitivities
roc_elastic_smote_data$specificities = 0.97*roc_elastic_data$specificities
roc_elastic_smote_data$thresholds = roc_elastic_data$thresholds

#test only
roc_logistic_data = roc_elastic_data
roc_logistic_smote_data = roc_elastic_smote_data



#elastic no smote
newdata_e = cbind(c(roc_elastic_data$sensitivities,roc_elastic_data$specificities),c(rep("sens",nrow(roc_elastic_data)),rep("spec",nrow(roc_elastic_data))),rep(roc_elastic_data$thresholds,2))
colnames(newdata_e) = c("value","type","threshold")
newdata_e = as.data.frame(newdata_e)
newdata_e$threshold = as.numeric(newdata_e$threshold)
newdata_e$value = as.numeric(newdata_e$value)

#elastic smote
newdata_es = cbind(c(roc_elastic_smote_data$sensitivities,roc_elastic_smote_data$specificities),c(rep("sens",nrow(roc_elastic_smote_data)),rep("spec",nrow(roc_elastic_smote_data))),rep(roc_elastic_smote_data$thresholds,2))
colnames(newdata_es) = c("value","type","threshold")
newdata_es = as.data.frame(newdata_es)
newdata_es$threshold = as.numeric(newdata_es$threshold)
newdata_es$value = as.numeric(newdata_es$value)

#logistic no smote
newdata_l = cbind(c(roc_logistic_data$sensitivities,roc_logistic_data$specificities),c(rep("sens",nrow(roc_logistic_data)),rep("spec",nrow(roc_logistic_data))),rep(roc_logistic_data$thresholds,2))
colnames(newdata_l) = c("value","type","threshold")
newdata_l = as.data.frame(newdata_l)
newdata_l$threshold = as.numeric(newdata_l$threshold)
newdata_l$value = as.numeric(newdata_l$value)

# #logistic smote
newdata_ls = cbind(c(roc_logistic_smote_data$sensitivities,roc_logistic_smote_data$specificities),c(rep("sens",nrow(roc_logistic_smote_data)),rep("spec",nrow(roc_logistic_smote_data))),rep(roc_logistic_smote_data$thresholds,2))
colnames(newdata_ls) = c("value","type","threshold")
newdata_ls = as.data.frame(newdata_ls)
newdata_ls$threshold = as.numeric(newdata_ls$threshold)
newdata_ls$value = as.numeric(newdata_ls$value)



ui = fluidPage(
  titlePanel("Sensitivities and Specificities"),
  # User input: number of bins for histogram
  sidebarLayout(
    sidebarPanel(
      #This is a "widget"
      #   The first two arguments are always 
      #      - inputId: A unique character 
      #      - label:  what does the user see as a description?
      radioButtons(inputId     = "modelChoices",
                   label        = "Select Model",
                   choiceNames  = c("Logistic Regression","Elastic Net"),
                   choiceValues = c('logistic','elastic')),
      sliderInput("thresholdSelected", label = "Input Desired Threshold", min=0,max=1,value = .5),
    ),
    # this directs where to send the user input to
    #  -> Send user input to "main panel" on site
    mainPanel(
      #   outputId: a character used to connect UI input to server output
      plotOutput(outputId = "namesPlot"),
      plotOutput(outputId = "newPlot")
    )
  )
  
)


server = function(input, output) {
  # renderPlot refreshed the display whenever the user input changes
  #   output:  The UI directs the user input to be displayed at "namesPlot"
  output$namesPlot = renderPlot({
    if(input$modelChoices=="elastic"){
     # barplot(roc_elastic_data$sensitivities[which.min(abs(roc_elastic_data$thresholds-input$thresholdSelected))],ylim=c(0,1)) + barplot(5)
      ggplot(data = rbind(newdata_e[newdata_e$type=="sens",][which.min(abs(newdata_e$threshold-input$thresholdSelected)),],newdata_e[newdata_e$type=="spec",][which.min(abs(newdata_e$threshold-input$thresholdSelected)),]))+
        geom_col(aes(x=type,y=value,fill=type),show.legend = FALSE)+
        labs(title="Elastic Net: Without SMOTE Correction")+xlab("")+
        scale_fill_manual(values = c("#800000","lightblue"))
      }
  else {
    ggplot(data = rbind(newdata_l[newdata_l$type=="sens",][which.min(abs(newdata_l$threshold-input$thresholdSelected)),],newdata_l[newdata_l$type=="spec",][which.min(abs(newdata_l$threshold-input$thresholdSelected)),]))+
      geom_col(aes(x=type,y=value,fill=type),show.legend = FALSE)+
      labs(title="Logistic Regression: Without SMOTE Correction")+xlab("")+
      scale_fill_manual(values = c("#800000","lightblue"))
  }
  })
  output$newPlot = renderPlot({
    if(input$modelChoices=="elastic"){
      ggplot(data = rbind(newdata_es[newdata_es$type=="sens",][which.min(abs(newdata_es$threshold-input$thresholdSelected)),],newdata_es[newdata_es$type=="spec",][which.min(abs(newdata_es$threshold-input$thresholdSelected)),]))+
        geom_col(aes(x=type,y=value,fill=type),show.legend = FALSE)+labs(title="Elastic Net: With SMOTE Correction")+
        xlab("")+scale_fill_manual(values = c("#800000","lightblue"))
    }
    else {
      ggplot(data = rbind(newdata_ls[newdata_ls$type=="sens",][which.min(abs(newdata_ls$threshold-input$thresholdSelected)),],newdata_ls[newdata_ls$type=="spec",][which.min(abs(newdata_ls$threshold-input$thresholdSelected)),]))+
        geom_col(aes(x=type,y=value,fill=type),show.legend = FALSE)+labs(title="Logistic Regression: With SMOTE Correction")+
        xlab("")+scale_fill_manual(values = c("#800000","lightblue"))
    }
  })
}



shinyApp(ui = ui, server = server)





if(input$plotType == 'total'){
  ggplot(data = filter(nameYear, name == input$text, gender == input$genderType)) + 
    geom_line(mapping = aes(x = year, y = total)) + 
    ylim(0,input$maxPlot)+ geom_vline(xintercept = input$vertPlot)
}else if(input$plotType == 'rank'){
  rankYear = nameYear %>% 
    group_by(year,gender) %>% 
    mutate(rank = rank(total,ties.method='min')) %>%
    mutate(rank = abs(rank-max(rank))+1)
  ggplot(data = filter(rankYear,name == input$text, gender == input$genderType)) + 
    geom_line(mapping = aes(x = year, y = rank))+ 
    ylim(0,input$maxPlot) + geom_vline(xintercept = input$vertPlot)
}else if(input$plotType == 'perc'){
  percYear = nameYear %>% 
    group_by(year,gender) %>% 
    mutate(perc = total/sum(total)*100)
  ggplot(data = filter(percYear,name == input$text, gender == input$genderType)) + 
    geom_line(mapping = aes(x = year, y = perc))+ 
    ylim(0,input$maxPlot) + geom_vline(xintercept = input$vertPlot)
}

