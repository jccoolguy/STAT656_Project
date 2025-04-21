require(dplyr)
if (!require('shiny')){install.packages("shiny");require(shiny)}
if (!require('ggplot2')) install.packages("ggplot2")

load("roc_elastic_data.Rda")
load("roc_elastic_data_smote.Rda")
roc_elastic_smote_data = roc_elastic_data_smote
rm(roc_elastic_data_smote)
load("roc_logistic_data.Rda")
load("roc_logistic_smote_data.Rda")

#elastic no smote
colnames(roc_elastic_data) = c("thresholds","sensitivities","specificities")
roc_elastic_data = as.data.frame(roc_elastic_data)

# #elastic smote
colnames(roc_elastic_smote_data) = c("thresholds","sensitivities","specificities")
roc_elastic_smote_data = as.data.frame(roc_elastic_smote_data)

# #logistic no smote
 colnames(roc_logistic_data) = c("thresholds","sensitivities","specificities")
 roc_logistic_data = as.data.frame(roc_logistic_data)
# 
# #logistic smote
 colnames(roc_logistic_smote_data) = c("thresholds","sensitivities","specificities")
 roc_logistic_smote_data = as.data.frame(roc_logistic_smote_data)



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
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId     = "modelChoices",
                   label        = "Select Model",
                   choiceNames  = c("Logistic Regression","Elastic Net"),
                   choiceValues = c('logistic','elastic')),
      sliderInput("thresholdSelected", label = "Select Desired Threshold", min=0,max=1,value = .5),
    ),
    mainPanel(
      plotOutput(outputId = "namesPlot"),
      plotOutput(outputId = "newPlot")
    )
  )
  
)


server = function(input, output) {
  output$namesPlot = renderPlot({
    if(input$modelChoices=="elastic"){
     # barplot(roc_elastic_data$sensitivities[which.min(abs(roc_elastic_data$thresholds-input$thresholdSelected))],ylim=c(0,1)) + barplot(5)
      ggplot(data = rbind(newdata_e[newdata_e$type=="sens",][which.min(abs(newdata_e$threshold-input$thresholdSelected)),],newdata_e[newdata_e$type=="spec",][which.min(abs(newdata_e$threshold-input$thresholdSelected)),]))+
        geom_col(aes(x=type,y=value,fill=type),show.legend = FALSE)+
        labs(title="Elastic Net: Without SMOTE Correction")+xlab("")+ylab("")+
        scale_fill_manual(values = c("#800000","lightblue"))+ylim(0,1)
      }
  else {
    ggplot(data = rbind(newdata_l[newdata_l$type=="sens",][which.min(abs(newdata_l$threshold-input$thresholdSelected)),],newdata_l[newdata_l$type=="spec",][which.min(abs(newdata_l$threshold-input$thresholdSelected)),]))+
      geom_col(aes(x=type,y=value,fill=type),show.legend = FALSE)+
      labs(title="Logistic Regression: Without SMOTE Correction")+xlab("")+ylab("")+
      scale_fill_manual(values = c("#800000","lightblue"))+ylim(0,1)
  }
  })
  output$newPlot = renderPlot({
    if(input$modelChoices=="elastic"){
      ggplot(data = rbind(newdata_es[newdata_es$type=="sens",][which.min(abs(newdata_es$threshold-input$thresholdSelected)),],newdata_es[newdata_es$type=="spec",][which.min(abs(newdata_es$threshold-input$thresholdSelected)),]))+
        geom_col(aes(x=type,y=value,fill=type),show.legend = FALSE)+labs(title="Elastic Net: With SMOTE Correction")+
        xlab("")+ylab("")+scale_fill_manual(values = c("#800000","lightblue"))+ylim(0,1)
    }
    else {
      ggplot(data = rbind(newdata_ls[newdata_ls$type=="sens",][which.min(abs(newdata_ls$threshold-input$thresholdSelected)),],newdata_ls[newdata_ls$type=="spec",][which.min(abs(newdata_ls$threshold-input$thresholdSelected)),]))+
        geom_col(aes(x=type,y=value,fill=type),show.legend = FALSE)+labs(title="Logistic Regression: With SMOTE Correction")+
        xlab("")+ylab("")+scale_fill_manual(values = c("#800000","lightblue"))+ylim(0,1)
    }
  })
}

shinyApp(ui = ui, server = server)

