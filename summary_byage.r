rel<-function(scorematrix){
  #### calculating alpha
  nitem=dim(scorematrix)[2]
  k=nitem
  top=0
  btm=var(rowSums(scorematrix),na.rm=T)
  for (j in c(1:k)) {
    t=var(scorematrix[,j],na.rm=T)
    top=top+t
  }
  out=k*(1-top/btm)/(k-1)
  return(out)
}

library(shiny)
library(gtools)
udata=NULL

ui=shinyUI(fluidPage(
  fluidRow(
    h4('Upload file'),
    fileInput('file1',' Choose CSV File', accept=c('csv','.csv')),
    tags$hr(),
    h4(' Identify the following variable(s) in your dataset:')
  ),
  fluidRow(
    
    column(2,
           selectInput("teid",label=h5("TEID"),"") 
    ),
    column(2,
           selectInput("age", label=h5("Age group"),"")
    ),
    column(2,
           selectInput("clin", label=h5("Clinical group?"),"")
    ),
    column(6,
           selectizeInput('vars',label=h5("Item scores"),"",multi=T),
           actionButton('submit','Submit specification',icon=icon('upload'))
    )
  ),
  fluidRow(
    tags$hr(),
    h4('Data summary'),
    tableOutput('summ')
  )
))

server<-shinyServer(function(input, output, session) {
  inFile<-reactive({
    if (is.null(input$file1)) {
      return(NULL)
    } else {
      input$file1
    }
  })
  udata<-reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {
      read.csv(inFile()$datapath,header=T,stringsAsFactors=F)
    }
  })
  observe({
    updateSelectInput(session, 'teid', choices=names(udata()))
    updateSelectInput(session, 'age', choices=names(udata()))
    updateSelectInput(session, 'clin', choices=names(udata()))
	updateSelectizeInput(session,'vars',choices=names(udata()))
  })
  
  age<-reactive({
    d<-udata()
    d[,input$age]
  })
  clingrp<-reactive({
	d<-udata()
	d[,input$clin]
  })
  isclin<-reactive({
	!is.na(clingrp())
  })
  scores<-reactive({
	d<-udata()
	d[,input$vars]
  })
  output$summ<-renderTable({
	if (is.null(input$submit) | (input$submit==0)) return(NULL)
	isolate({
		aa<-age(); aa[isclin()]=clingrp()[isclin()]
		ss<-scores(); ss.k<-split(ss,aa)
		rawt<-rowSums(ss,na.rm=T); rawt.k<-split(rawt,aa)
 		N<-table(aa)
		alpha<-sapply(ss.k,FUN=rel)
		means<-sapply(rawt.k,mean)
		ranges<-sapply(rawt.k,range)
		tt<-rbind(N,means,ranges[1,],ranges[2,],alpha)
    row.names(tt)<-c("N","Ave raw score","Min raw score","Max raw score","alpha")
    tt<-tt[,mixedorder(colnames(tt))]
    tt
	})		
  })
})
runApp(list(ui=ui,server=server))
