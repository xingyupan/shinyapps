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
outliercheck=function(alphachange,teid,scores){
  ###################
  ## remove cases one at a time
  ## check effects on reliability
  ## report cases whose removal resulted in higher relability
  ####################
  n=dim(scores)[1]
  outs=NULL
  old.alpha=rel(scores)
  for (i in c(1:n)) {
    new.alpha=rel(scores[-i,])
    if ((new.alpha-old.alpha)>alphachange) outs<-c(outs,i)
  }
  outs=teid[outs]
  return(outs)
}

aftereffect=function(nremoval,optionalitem,delete,age,scores) {
  tn=choose(length(optionalitem),nremoval)/390
  x=combn(optionalitem,nremoval); n=1; 
  eff=matrix(nrow=dim(x)[2],ncol=(length(unique(age))+1))
  drop=rep(NA,dim(x)[2])
  for (n in c(1:dim(x)[2])) {
    aaa=x[,n]
    drop[n]=str_c(c(as.character(delete),as.character(aaa)),collapse=" ")
    ### total alpha ####
    eff[n,1]=alpha(scores[,(!names(scores) %in% c(delete,aaa))])$total$std.alpha
    ### alpha per age group ####
    ss.k<-split(scores[,(!names(scores) %in% c(delete,aaa))],age)
    eff[n,2:(length(unique(age))+1)]=sapply(ss.k, FUN=rel)
  }
  eff=data.frame(eff);eff=data.frame(drop,eff); 
  eff=eff[order(drop),]
  colnames(eff)[2]="overall"
  colnames(eff)[3:ncol(eff)]=names(table(age))
  eff[,2:ncol(eff)]=round(eff[,2:ncol(eff)],3)
  eff<-eff[,c(1,2,mixedorder(names(table(age)))+2)]
  ####may need to adjust item order here
  return(eff)
}

library(shiny)
library(psych)
library(stringr)
library(gtools)
udata=NULL

ui=shinyUI(navbarPage('Toolbox',
                      tabPanel('File',
                               ####### File specification ######
                               sidebarLayout(
                                 sidebarPanel(
                                   h5('Upload file'),
                                   fileInput('file1',label=h5('Choose CSV File'), accept=c('csv','.csv')),
                                   h5(' Identify the following variable(s) in your dataset:'),
                                   selectInput("teid",label=h5("TEID"),""),
                                   selectInput("age", label=h5("Age group"),""),
                                   selectInput("clin", label=h5("Clinical group"),""),
                                   selectizeInput('vars',label=h5("Item scores"),"",multi=T),
                                   actionButton('submit','Submit specification',icon=icon('upload'))
                                 ),
                                 mainPanel(
                                   h4('Data summary'),
                                   tableOutput('summ')
                                 )
                               )
                      ),
                      tabPanel('Check case',
                               ####### Check odd cases for scoring errors #####
                               sidebarLayout(
                                 sidebarPanel(
                                   numericInput('age_c','Which age group you want to check case for?',value='1'),
                                   numericInput('change','Threshold value',value='0.005'),
                                   actionButton("check","Check Case",icon=icon("search"))  
                                 ),
                                 mainPanel(
                                   htmlOutput('outl'),
                                   tableOutput('outl_s')
                                 )
                               )
                               
                      ),
                      tabPanel('Item selection',
                               ####### Select Item ######
                               div(style='display:inline-block;width:200px; vertical-align:top',
                                   numericInput('ndrop','Total # of items to drop',value=0)),
                               div(style='display:inline-block',
                                   selectizeInput('keyclin','Specify key clinical groups',choices='',multi=T,width='200px')),
                               tags$br(),actionButton('log','Show sensitive items',icon=icon('star')),
                               htmlOutput('logreg'),
                               tags$hr(),h4('Items to keep'),
                               div(style='display:inline-block',
                                   selectizeInput('k1','Starting items',choices='',multi=T, width='200px')),
                               div(style='display:inline-block',
                                   selectizeInput('k2','Floor items',choices='',multi=T,width='200px')),                         
                               div(style='display:inline-block',
                                   selectizeInput('k3','Ceiling items',choices='',multi=T,width='200px')),
                               div(style='display:inline-block',
                                   selectizeInput('k4','Critical items',choices='',multi=T,width='200px')),
                               h4('Items to drop'),
                               div(style='display:inline-block',
                                   selectizeInput('d1','Redundant items',choices='',multi=T,width='200px')),
                               div(style='display:inline-block',
                                   selectizeInput('d2','Problematic items',choices='',multi=T,width='200px')),
                               div(style='display:inline-block',
                                   selectizeInput('d3','Outdated items',choices='',multi=T,width='200px')),
                               div(style='display:inline-block',
                                   selectizeInput('d4','Other drops',choices='',multi=T,width='200px')),
                               tags$br(),
                               div(style="display:inline-block",actionButton("est","Estimate time",icon=icon('clock-o'))),
                               div(style="display:inline-block",actionButton("cal","Calculate",icon=icon('calculator'))),
                               ####### display results #####
                               textOutput('estim'),
                               textOutput('finish')
                      ),
                      tabPanel('Display solutions',
                               downloadButton('d.sl','Download results'),
                               div(style="padding:5px 0px",dataTableOutput('top.sl'))
                      ),
                      navbarMenu('Score change',
                                 tabPanel('code-based'),
                                 tabPanel('teid-based',
                                          fluidRow(
                                            div(style='padding:0px 5px', h4('Conduct Change')),
                                            div(style="display:inline-block; padding:0px 10px",actionButton("tch","add",icon=icon("plus"))),
                                            div(style="display:inline-block",actionButton("tch_rm","remove",icon=icon('minus'))),
                                            htmlOutput("multiInputs") 
                                          ),
                                          fluidRow(
                                            div(style='padding:0px 5px',h4('Effect')),
                                            div(style='padding:0px 5px',actionButton('id_c',"Check effect",icon=icon('calculator'))),
                                            tableOutput('id_eff'),
                                            div(style='padding:5px 5px',downloadButton('d.id_c','Download change log'))
                                          )
                                 )
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
  #### update dropdown menu choices ####
  observe({
    updateSelectInput(session, 'teid', choices=names(udata()))
    updateSelectInput(session, 'age', choices=names(udata()))
    updateSelectInput(session, 'clin', choices=names(udata()))
    updateSelectizeInput(session,'vars',choices=names(udata()))
  })
  
  id<-reactive({
    d<-udata()
    d[,input$teid]
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
    dx<-d[,input$vars]
    dx[,mixedorder(colnames(dx))]
  })
  
  observeEvent(input$submit, {
    updateSelectizeInput(session,'keyclin',choices=unique(as.character(clingrp())))
    updateSelectizeInput(session,'k1',choices=names(scores()))
    updateSelectizeInput(session,'k2',choices=names(scores()))
    updateSelectizeInput(session,'k3',choices=names(scores()))
    updateSelectizeInput(session,'k4',choices=names(scores()))
    updateSelectizeInput(session,'d1',choices=names(scores()))
    updateSelectizeInput(session,'d1',choices=names(scores()))
    updateSelectizeInput(session,'d2',choices=names(scores()))
    updateSelectizeInput(session,'d3',choices=names(scores()))
    updateSelectizeInput(session,'d4',choices=names(scores()))    
  })
  
  ##### Main tab: producing a summary of user data ######
  output$summ<-renderTable({
    if (is.null(input$submit) | (input$submit==0)) return(NULL)
    isolate({
      aa<-age(); aa[isclin()]=clingrp()[isclin()]
      ss<-scores(); ss.k<-split(as.data.frame(ss),aa)
      rawt<-rowSums(ss,na.rm=T); rawt.k<-split(rawt,aa)
      N<-table(aa)
      alpha<-sapply(ss.k,FUN=rel)
      means<-sapply(rawt.k,mean)
      ranges<-sapply(rawt.k,range)
      neffect<-sapply(ss.k,function(x) sum(apply(x,2,var,na.rm=T)>0))
      tt<-rbind(N,means,neffect,ranges[1,],ranges[2,],alpha)
      row.names(tt)<-c("N","Ave raw score","Effective items","Min raw score","Max raw score","alpha")
      tt<-tt[,mixedorder(colnames(tt))]
      tt
    })    
  })
  ##### Check case tab: check outlier ####
  #### check outlier ####
  check.o<-eventReactive(input$check,{
    agex<-input$age_c
    dx<-scores()[age()==agex,]
    idx<-id()[age()==agex]
    outliercheck(input$change,idx,dx)
  })
  output$outl=renderText({
    outliers<-str_c(check.o(),collapse=",")
    if (length(check.o())>0) {
      HTML(  
        paste0("Please check following cases for possible scoring issues. <br/>",
               "to see more/fewer TEIDs, please decrease/increase the threshold.<br/>",
               outliers,"<br/>")
      )
    } else {
      HTML(paste("No potential problem cases detected in age group with this threshold"))
    }
  })
  output$outl_s=renderTable({
    if (length(check.o())==0) return(NULL)
    dx<-data.frame(id(),age(),clingrp(),scores())
    colnames(dx)[1:3]=c("TEID","AgeGroup","ClinGroup")
    dx[id() %in% check.o(),]
  })
  #### Item select tab: calculate critical item ####
  
  k1<-reactive({ input$k1 })
  k2<-reactive({ input$k2 })
  k3<-reactive({ input$k3 })
  k4<-reactive({ input$k4 })
  d1 <-reactive({ input$d1 })
  d2<- reactive({ input$d2 })
  d3<-reactive({input$d3})
  d4<-reactive({input$d4})
  key.clin<-reactive({as.character(input$keyclin)})
  ndrop<-reactive({input$ndrop})
  ntot<-reactive({ dim(scores())[2] })
  keeps<-reactive({ unique(c(k1(),k2(),k3(),k4())) })
  drops<-reactive({unique(c(d1(),d2(),d3(),d4())) })
  fvars<-eventReactive(input$log,{
    nclin<-length(key.clin())
    x<-NULL
    for (i in 1:nclin) {
      clinx<-key.clin()[i]
      y<-as.character(clingrp())
      y[is.na(y)]="Non"
      y[!y %in% c(clinx,"Non")]=NA
      logi.non<-suppressWarnings(glm(as.factor(y)~1, family=binomial, data=scores()))
      logi.full<-suppressWarnings(glm(as.factor(y)~., family=binomial, data=scores()))
      both<-suppressWarnings(step(logi.non,scope=list(lower=formula(logi.non),upper=formula(logi.full)),
                                  direction='both',trace=0))
      flog<-formula(both)
      flog<-as.character(flog)[3]
      ff<-strsplit(flog," [+] ")[[1]]
      x[[i]]<-ff
    }
    x
  }) ## producing a list
  output$logreg<-renderText({
    if (input$log==0 | is.null(input$log)) return() 
    vv=""
    nl<-length(fvars())
    for (i in c(1:nl)){
      kk<-str_c(fvars()[[i]],collapse=" ")
      vv=paste(vv, "Following variables are highly sensitive differentiating ",key.clin()[i]," from non-clinicals.<br/>",kk,'<br/>',sep="")
    }
    x<-cor(scores(),use="complete.obs")
    diags<-which(x==1); all<-which(x>0.6)
    all<-all[! all %in% diags]; n<-dim(scores())[2]
    r<-all %/% n +1
    c<- all %% n; c[c==0]=n; r[c==n]=r[c==n]-1
    ## figure out item pairs whose correlation is higher than .6
    r<-names(scores())[r]; c<-names(scores())[c]
    ps<-paste(r,c); ps<-str_c(ps,collapse="),(")
    vv<-paste(vv,"The following item pairs has a correlation higher than 0.6: <br/>(", ps,
              ") <br/> Check for possible redundancy.",sep="")
    HTML(vv)
  })
  #### Item select tab: estimate calc time ####
  etime <- eventReactive(input$est, {
    nt<-ntot()-length(keeps())
    nd<-ndrop()-length(drops())
    choose(nt,nd)
  })
  mins<- reactive({etime()/400})
  output$estim<-renderText({
    if (mins()>30) {
      paste("Dropping",ndrop()-length(drops()),
            "from a total of",ntot()-length(keeps()) ,
            "items. It take approximately",mins(),"min to calculate all solutions.",
            "Please add more constraints to reduce your calculation time")
    } else if (mins()>5) {
      paste("Dropping",ndrop()-length(drops()),
            "from a total of",ntot()-length(keeps()) ,
            "items. It take approximately",mins(),"min to calculate all solutions.",
            "Go grab a coffee after you hit that Calculate button!")
      
    } else {
      paste("Dropping",ndrop()-length(drops()),
            "from a total of",ntot()-length(keeps()) ,
            "items. It will take approximately",mins(),"min to calculate all solutions")
    }
  })
  
  #### Item select Tab: calculate solutions ####
  kindrops<-reactive({ sum(keeps() %in% drops()) })
  newkeeps<-reactive({ keeps()[!keeps() %in% drops()]})
  eff<-eventReactive(input$cal, {
    d<-scores(); nitem<-dim(d)[2]
    optionalitem<-names(d)[!(names(d) %in% c(keeps(),drops()))]
    nt<-ntot()-length(newkeeps())
    nd<-ndrop()-length(drops())
    aa<-age(); aa[isclin()]=clingrp()[isclin()]
    print(nd)
    print(optionalitem)
    print(names(scores()))
    print(drops())
    aftereffect(nd,optionalitem,drops(),aa,scores())
  })
  output$top.sl<-renderDataTable({
    eff()
  })
  output$finish<-renderText({
    if (is.null(eff())) return()
    HTML("Calculation finished. Check Display solution tab for results.")
  })
  #### download item selection solution ####
  output$d.sl<-downloadHandler(
    filename<-function() {
      paste("ItemSelection_",format(Sys.Date(),"%b_%d_%Y"),".csv",sep="")},
    content=function(file) {
      write.csv(eff(),file, row.names=F) }
  )
  ##### Score change tab: dynamic UI ######
  output$multiInputs<-renderUI({
    if (is.null(input$tch) | input$tch==0) return()
    w=''
    if (is.null(input$tch_rm)| input$tch_rm==0) {
      nch=input$tch
    } else {
      nch=input$tch-input$tch_rm
    }
    if (nch<1) return()
    for (i in c(1:nch)) {
      w<-paste(w,
               div(style="display:inline-block; padding: 0px 10px; width:150px; vertical-align:top",
                   selectInput(paste('tch_item',i,sep=""),label='Item',choices=names(scores()),selected=input[[sprintf('tch_item%d',i)]])),
               div(style="display:inline-block;width:100px; vertical-align:top",
                   numericInput(paste('tch_score',i,sep=''),label='Score',value=input[[sprintf('tch_score%d',i)]])),
               div(style="display:inline-block",
                   selectizeInput(paste('tch_id',i,sep=''),label='TEID',choices=id(),multi=T,selected=input[[sprintf('tch_id%d',i)]])))
      if (i %%2 ==0) w<-paste(w,"<br/>")
    }
    HTML(w)
  })
  #### score change tab: calculate effect of change ####
  output$id_eff<-renderTable({
    if (is.null(input$id_c) | (input$id_c==0)) return(NULL)
    isolate({
      nscores<-scores()
      if (is.null(input$tch_rm)| input$tch_rm==0) {
        nch=input$tch
      } else {
        nch=input$tch-input$tch_rm
      }
      if (nch>0) {
        for (i in c(1:nch)) {
          item<-input[[sprintf("tch_item%d",i)]]
          s<-input[[sprintf("tch_score%d",i)]]
          ids<-input[[sprintf("tch_id%d",i)]]
          nscores[id() %in% ids,item]=s
        }
        aa<-age(); aa[isclin()]=clingrp()[isclin()]
        n.k<-split(nscores,aa)
        s.k<-split(scores(),aa)
        r0<-sapply(s.k,FUN=rel) ## calc reliability of orginal
        r1<-sapply(n.k,FUN=rel) ## calc reliability of new
        rch<-r1-r0  
        rtable<-rbind(r0,r1,rch)
        rtable<-as.data.frame(rtable)
        colnames(rtable)<-names(table(aa))
        rtable<-rtable[,mixedorder(names(table(aa)))]
        row.names(rtable)=c("original","current","change")
        rtable<-round(rtable,2)
        rtable
      }
    })
  })
  id_change_summary<-reactive({
    nch=input$tch-input$tch_rm
    items<-NULL;ss<-NULL;ids<-NULL;
    for (i in c(1:nch)) {
      items<-c(items,input[[sprintf("tch_item%d",i)]])
      ss<-c(ss,input[[sprintf("tch_score%d",i)]])
      idx<-str_c(input[[sprintf("tch_id%d",i)]], collapse=" ")
      ids<-c(ids,idx)
    }
    data.frame(items,score=ss,ids)
  })
  
  output$d.id_c<-downloadHandler(
    filename<-function() {
      paste("TEID_ScoreChange_",format(Sys.Date(),"%b_%d_%Y"),".csv",sep="")},
    content<-function(file) {
      write.csv(id_change_summary(),file,row.names=F)}
  )
  
  ##### Score change tab: display score change effect ####
})
runApp(list(ui=ui,server=server))
