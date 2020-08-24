library(shiny)
library(shinythemes)

ui =fluidPage(
  navbarPage(
    theme = shinythemes::shinytheme("united"),  # <--- To use a theme, uncomment this
    "Crisis Web Detection",
    pageWithSidebar(  
         headerPanel("Sistem Pendeteksian Dini Krisis Mata Uang di Indonesia"),
         sidebarPanel(
           wellPanel(
             selectInput("indikator",label="Pilih Indikator",choices = c("Impor"="Impor","Ekspor"="Ekspor","Cadangan Devisa"="Cadangan_Devisa",
                                                                         "Indeks Harga Saham Gabungan"="IHSG","Rasio Suku Bunga Pinjaman dan Simpanan"="SPinSim",
                                                                         "Suku Bunga Simpanan Riil"="SBSR","Selisih BI Rate Riil dan Fed Rate Riil"="SBIrFer",
                                                                         "Simpanan Bank"="Simpanan_Bank","Nilai Tukar Riil"="NTRiil","Nilai Tukar Perdagangan"="NTPdag",
                                                                         "M1"="M1","M2 per Cadangan Devisa"="M2/CD","M2 Multiplier"="M2M"),selected = NULL),
             br(),
           ),
           wellPanel(
             h3("Prediksi Krisis Mata Uang"),
             br(),
             selectInput("metode",label="Pilih Metode Analisis",choices = c("Markov Switching+Volatilitas"="MS_ARCH"),selected = NULL),
             actionButton("hasil","Prediksi"),
             br(),
         )),
         mainPanel(
           tabsetPanel(
             tabPanel("Prediksi",DT::dataTableOutput(outputId="prediksi")),
             tabPanel("Plot",plotOutput("plot",click="plot_click"))
           )
         )
)
)
)
server = function(input, output) {
  library(tseries)
  library(forecast)
  library(MSGARCH)
  library(RMySQL)
  library(ggfortify)
  library(ggplot2)
  mydb=dbConnect(MySQL(),user="root",password="rezpector12",dbname="Indikator_Krisis",host="127.0.0.1")
  data=dbSendQuery(mydb,"SELECT * FROM 15_indikator")
  data=fetch(data,n=-1)
  data_plot=ts(data[,2:14],start = c(1990,1),frequency = 12)
  ide=eventReactive(input$hasil,{
    data2=data[,input$indikator]
    data2=as.data.frame(data2)
    data2=remove_missing(data2)
    data3=ts(data2,start=c(1990,1),frequency = 12)
    if (input$indikator=="IHSG"|input$indikator=="M2M"){
      data4=data3
    }else{
      data4=diff(log(data3))}
    model=auto.arima(data4,stationary = FALSE,seasonal=FALSE,trace=FALSE)
    residu=resid(model)
    fit11=garch(residu,order=c(1,1),trace=FALSE) #GARCH(1,1)
    fit01=garch(residu,order=c(0,1),trace=FALSE) #ARCH(1)
    aic1=AIC(fit11)
    aic2=AIC(fit01)
    if (aic1>aic2){
      spec=CreateSpec(variance.spec = list(model = c("sARCH")),
                      distribution.spec = list(distribution = c("norm")),
                      switch.spec = list(do.mix = FALSE,K=2))
      out.mle = FitML(spec = spec, data= residu)
      trans.mat <- TransMat(out.mle, nahead = 12)
      state<-State(object=out.mle)
      p11<-state$SmoothProb[,,1]
      p12<-state$SmoothProb[,,2]
      pred11=(trans.mat[1,1]*p11[length(p11)])+(trans.mat[2,1]*p12[length(p12)])
      pred12=(trans.mat[1,2]*p11[length(p11)])+(trans.mat[2,2]*p12[length(p12)])
      pred1=c()
      pred2=c()
      pred1[1]=pred11
      pred2[1]=pred12
      for (i in 2:12){
        pred1[i]=(trans.mat[1,1]*pred1[i-1])+(trans.mat[2,1]*pred2[i-1])
        pred2[i]=(trans.mat[1,2]*pred1[i-1])+(trans.mat[2,2]*pred2[i-1])
      }
      pred=cbind.data.frame(pred1,pred2)
      if (input$indikator=="Simpanan_Bank"){
        pred2=ifelse(pred2>0.7,"Krisis","Tidak Krisis")
      }else if (input$indikator=="SPinSim"){
        pred2=ifelse(pred2>0.65,"Krisis","Tidak Krisis")
      }else if (input$indikator=="NTRiil"){
        pred2=ifelse(pred2>0.97,"Krisis","Tidak Krisis")
      }else if (input$indikator=="M2/CD"){
        pred2=ifelse(pred2>0.97,"Krisis","Tidak Krisis")
      }else if (input$indikator=="Ekspor"){
        pred2=ifelse(pred2>0.98,"Krisis","Tidak Krisis")
      }else if (input$indikator=="Impor"){
        pred2=ifelse(pred2>0.82,"Krisis","Tidak Krisis")
      }else if (input$indikator=="IHSG"){
        pred2=ifelse(pred2>0.5,"Krisis","Tidak Krisis")
      }else if (input$indikator=="Cadangan_Devisa"){
        pred2=ifelse(pred2>0.92,"Krisis","Tidak Krisis")
      }else if (input$indikator=="SBSR"){
        pred2=ifelse(pred2>0.92,"Krisis","Tidak Krisis")
      }else if (input$indikator=="SBIrFer"){
        pred2=ifelse(pred2>0.8,"Krisis","Tidak Krisis")
      }else if (input$indikator=="NTPdag"){
        pred2=ifelse(pred2>0.8,"Krisis","Tidak Krisis")
      }else if (input$indikator=="M2M"){
        pred2=ifelse(pred2>0.8,"Krisis","Tidak Krisis")
      }else{
        pred2=ifelse(pred2>0.8,"Krisis","Tidak Krisis")
      }
      Prediksi=pred2
      a=length(data2)+1
      b=length(data2)+12
      Time=data[a:b,1]
      keluaran=cbind.data.frame(Time,Prediksi)
      keluaran
    }else{
      spec=CreateSpec(variance.spec = list(model = c("sGARCH")),
                      distribution.spec = list(distribution = c("norm")),
                      switch.spec = list(do.mix = FALSE,K=2))
      out.mle = FitML(spec = spec, data= residu)
      trans.mat <- TransMat(out.mle, nahead = 12)
      state<-State(object=out.mle)
      p11<-state$SmoothProb[,,1]
      p12<-state$SmoothProb[,,2]
      pred11=(trans.mat[1,1]*p11[length(p11)])+(trans.mat[2,1]*p12[length(p12)])
      pred12=(trans.mat[1,2]*p11[length(p11)])+(trans.mat[2,2]*p12[length(p12)])
      pred1=c()
      pred2=c()
      pred1[1]=pred11
      pred2[1]=pred12
      for (i in 2:12){
        pred1[i]=(trans.mat[1,1]*pred1[i-1])+(trans.mat[2,1]*pred2[i-1])
        pred2[i]=(trans.mat[1,2]*pred1[i-1])+(trans.mat[2,2]*pred2[i-1])
      }
      pred=cbind.data.frame(pred1,pred2)
      if (input$indikator=="Simpanan_Bank"){
        pred2=ifelse(pred2>0.7,"Krisis","Tidak Krisis")
      }else if (input$indikator=="SPinSim"){
        pred2=ifelse(pred2>0.65,"Krisis","Tidak Krisis")
      }else if (input$indikator=="NTRiil"){
        pred2=ifelse(pred2>0.97,"Krisis","Tidak Krisis")
      }else if (input$indikator=="M2/CD"){
        pred2=ifelse(pred2>0.97,"Krisis","Tidak Krisis")
      }else if (input$indikator=="Ekspor"){
        pred2=ifelse(pred2>0.98,"Krisis","Tidak Krisis")
      }else if (input$indikator=="Impor"){
        pred2=ifelse(pred2>0.82,"Krisis","Tidak Krisis")
      }else if (input$indikator=="IHSG"){
        pred2=ifelse(pred2>0.5,"Krisis","Tidak Krisis")
      }else if (input$indikator=="Cadangan_Devisa"){
        pred2=ifelse(pred2>0.92,"Krisis","Tidak Krisis")
      }else if (input$indikator=="SBSR"){
        pred2=ifelse(pred2>0.92,"Krisis","Tidak Krisis")
      }else if (input$indikator=="SBIrFer"){
        pred2=ifelse(pred2>0.8,"Krisis","Tidak Krisis")
      }else if (input$indikator=="NTPdag"){
        pred2=ifelse(pred2>0.8,"Krisis","Tidak Krisis")
      }else if (input$indikator=="M2M"){
        pred2=ifelse(pred2>0.8,"Krisis","Tidak Krisis")
      }else{
        pred2=ifelse(pred2>0.8,"Krisis","Tidak Krisis")
      }
      a=length(data2)+1
      b=length(data2)+12
      Time=data[a:b,1]
      Prediksi=pred2
      keluaran=cbind.data.frame(Time,Prediksi)
      keluaran
    }
  })  
  output$prediksi=DT::renderDataTable({
    DT::datatable(ide())
  })
  output$plot <- renderPlot({
    autoplot(data_plot[,input$indikator],xlab="Time",ylab="Value")
  })
}
shinyApp(ui=ui,server=server)