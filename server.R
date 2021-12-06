#This is the server for methane conversion factor calculator

#First data frame for monthly temperature
df1<-data.frame(Temp.input = rep(0,12)
                ,Removal=rep("N",12))
df2<-data.frame(value=c(1200,100,1.0,3.0,0.24,95)
                ,row.names = c("VS","VS(%)","Tmin","Tdamping","B0","emptying(%)"))


#A function to calculate monthly monthly temperature from monthly air temperature
temp.cal<-function(rm,Temp.input,Tmin,Tdamp) {
  T.m<-c()
  rm<-ifelse(toupper(rm)=="Y",1,0)
  if (sum(rm[1:7])==0&sum(rm[8:12])==1) {
    for (i in 1:12){
      T.m[i]<-max((Temp.input[i]-Tdamp),Tmin)
    } 
  } else {
    for (i in 1:12){
      T.m[i]<-max(Temp.input[i],Tmin)
    }
  }
  T.m<-c(T.m[12],T.m[1:11])
  T.m
}

#function for van hoff equation
v.hoff<- function(x){
  round(exp((19347*(x-308.16))/(1.987*x*308.16)),7)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
 
  #initialize a dataframe for the parameters
  parameter<- reactiveValues(data = df2) 
  
  #output the dataframe based on parameters
  output$parameter_datatable <-renderRHandsontable({
    rhandsontable(parameter$data,rowHeaderWidth = 100) %>%
      hot_col("value", format = "0.00")
  })
  
  #Obtain the Tmin and Tdamping
  para<- reactive({
    para<-hot_to_r(input$"parameter_datatable")
  }) #[3,1] and [4,1] are Tmin and Tdamping
  
  # Set the monthly temperature table
  df1.value<-reactiveValues(data = df1)
  
  #output the datatable based on the monthly temperature 
  output$temp_datatable1 <-renderRHandsontable({
    rhandsontable(df1.value$data)
  })
  
  #Obtain and calculate the manure temperature data by Tmin and Tdamp
  temp.month <- reactive({
    temp.month<-hot_to_r(input$"temp_datatable1")
    if(input$check_air){
      temp.month[,1]
    }
    else {
      temp.month[,1]<-temp.cal(temp.month[,2],temp.month[,1],para()[3,1],para()[4,1])
    }
  })
  
  #Obtain the air temperature input
  temp.air <- reactive({
    temp.air<-hot_to_r(input$"temp_datatable1")[,1]
  })
  
  #Obtain the removal month
  Month.rm <- reactive({
    Month.rm<-hot_to_r(input$"temp_datatable1")[,2]
    ifelse(toupper(Month.rm)=="Y",1,0)
  })
  
  #Calculate MCF
  #Output a plot for air temperature and manure temperature
   VS_ava  <-reactiveValues(value=0)
   VS_con  <-reactiveValues(value=0)
   f.m     <-reactiveValues(value=0)
   CH4.p   <-reactiveValues(value=0)
   CH4_sel <-reactiveValues(value=0)
   MCF     <-reactiveValues(value=0)
   temp    <-reactiveValues(value=0)
 
  observeEvent(input$plot1, {
    #assign the parameters for calculators
    T.m<-temp.month()[1:12]
    Manure.rm<-rep(Month.rm()[1:12],3)
    VS_Yr<-para()[1,1]
    VS_LQD<-para()[2,1]
    E_eff<-para()[6,1]
    B0<-para()[5,1]
    ######################
    #convert C to K
    T.m.K<-celsius.to.kelvin(T.m)
    f.m$value<-rep(v.hoff(T.m.K),3)

    #Vs excreted an loaded
    VS_month<-rep(VS_Yr/12,each=36)
    VS_loaded<-VS_month*(VS_LQD/100)
  
    #Calculate CH4 produced and MCF
    for (i in 1:36) {
       if (i == 1){
        VS_ava$value[i]<-VS_loaded[i]
        VS_con$value[i]<-VS_ava$value[i]*f.m$value[i]
       } else if (Manure.rm[i]==0){
           VS_ava$value[i]<-VS_loaded[i]+VS_ava$value[i-1]-VS_con$value[i-1]
           VS_con$value[i]<-VS_ava$value[i]*f.m$value[i]
        }
        else {
          VS_emp<-(VS_ava$value[i-1]-VS_con$value[i-1])*(E_eff/100)
          VS_ava$value[i]<-VS_loaded[i]+((VS_ava$value[i-1]-VS_con$value[i-1])*(1-(E_eff/100)))
          VS_con$value[i]<-VS_ava$value[i]*f.m$value[i]
        }
    temp$value[i]<-VS_con$value[i]*B0
    CH4.p$value<-VS_Yr*B0
    CH4_sel$value<-round(sum(temp$value[25:36]),3)
    MCF$value<-round(CH4_sel$value/CH4.p$value,3)
    }
   
    })

  
   
  output$plot.temp <- renderPlot({
    req(input$plot1) #require the input button to be non-0 (ie: don't load the plot when the app first loads)
   #####
    potentialCH4<-round(CH4.p$value)
    producedCH4<-round(CH4_sel$value)
        if(input$check_air) {
      par(mar=c(5,5,2,1))
      plot(1:12,temp.month(),type="b",cex.lab=1.5,las=1,cex.axis=1.5
           ,xlab="Month",ylab=expression(paste("Temperature (",degree,"C)")),lty=2)
      text(1,max(temp.month())*0.70,"Air temperature not provided",pos=4)
      legend(1,max(temp.month())*0.95,"Manure temperature",bty="n",lty=1)
      text(10,max(temp.month())*0.9,paste("MCF=",round(MCF$value,2)),pos=4)
      text(10,max(temp.month())*0.8,paste0("Potential CH4=",potentialCH4),pos=4)
      text(10,max(temp.month())*0.7,paste0("Produced CH4=",producedCH4),pos=4)    }
    else{
      par(mar=c(5,5,2,1))
      plot(1:12,temp.air(),type="b",cex.lab=1.5,las=1,cex.axis=1.5
           ,xlab="Month",ylab=expression(paste("Temperature (",degree,"C)")),lty=2)
      lines(1:12,temp.month(),lty=1)
      legend(1,max(temp.air())*0.95,c("Manure temperature","Air temperature")
             ,bty="n",lty=c(1,2))
      text(10,max(temp.air())*0.9,paste("MCF=",round(MCF$value,2)),pos=4)
      text(10,max(temp.air())*0.8,paste0("Potential CH4=",potentialCH4),pos=4)
      text(10,max(temp.air())*0.7,paste0("Produced CH4=",producedCH4),pos=4)
    }
  })
  
  output$VSCH4<-renderTable({
     req(input$plot1)
     Vstable<-data.frame(rep(1:3,each=12),rep(1:12,3),VS_ava$value,VS_con$value,temp$value)
     colnames(Vstable)<-c("Year","Month","VS available (kg)","VS consumed (kg)","CH4 (m3)")
     Vstable
  })
  
})
