library(shiny)
library(stringr)
library(psych)


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
codeineffect=function(codes,maxscore){
  c=codes[!is.na(codes)]
  l=length(c)
  s=rep(0,l)
  c.e=min(c)
  s[c %in% c(-7,-6)]=maxscore
  s[c %in% c(-8,-9,0,61:90,99)]=0
  s[c %in% c(2:30)]=2
  s[c %in% c(1,31:60)]=1
  s.e=max(s)
  return(list(code=c.e,score=s.e))
}
abyage=function(agegrp,scorematrix){
  ngroup=length(unique(agegrp))
  abyage=rep(NA,ngroup)
  nitem=dim(scorematrix)[2]
  k=nitem
  for (i in c(1:ngroup)) {
    top=0
    btm=var(rowSums(scorematrix[agegrp==i,]),na.rm=T)
    for (j in c(1:k)) {
      t=var(scorematrix[agegrp==i,j],na.rm=T)
      top=top+t
    }
    abyage[i]=k*(1-top/btm)/(k-1)
  }
  return(abyage)
}
aftereffect=function(nremoval,optionalitem,delete,age,scores) {
  start=Sys.time(); finished=F
  tn=choose(length(optionalitem),nremoval)/390
  print(paste("Approximate calculation time is",tn,"min.", sep=" "))
  if (tn>60) {print("Warning: it will take HOURS to get your result")
  }else if (tn>10) {print("Go grab a coffee while I work for you")}  
  
  x=combn(optionalitem,nremoval); n=1; 
  eff=matrix(nrow=dim(x)[2],ncol=(length(unique(age))+1))
  drop=rep(NA,dim(x)[2]);
  for (n in c(1:dim(x)[2])) {
    aaa=x[,n]
    drop[n]=str_c(as.character(aaa),collapse=" ")
    ### total alpha ####
    eff[n,1]=alpha(scores[,c(-c(aaa,delete))])$total$std.alpha
    ### alpha per age group ####
    eff[n,2:(length(unique(age))+1)]=abyage(age,scores[,c(-c(aaa,delete))])
  }
  
  eff=data.frame(eff);eff=data.frame(drop,eff); 
  eff=eff[order(drop),]
  colnames(eff)[2]="overall"
  colnames(eff)[3:ncol(eff)]=str_c("age",c(1:length(unique(age))))
  end=Sys.time()
  finished=T
  return(eff)
}

########################
#### Define Server #####
########################
shinyServer(function(input, output) {
  
  #### reading input values and transform them to proper format ####
  d <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE)
    data
  })
  
  keep<-reactive({
    as.numeric(str_extract_all(input$keep,"[[:digit:]]+")[[1]])
  })
  drop<-reactive({
    as.numeric(str_extract_all(input$drop,"[[:digit:]]+")[[1]])
  })
  opt<-reactive({
    as.numeric(str_extract_all(input$opt,"[[:digit:]]+")[[1]])
  })
  
  opt.l<-reactive({
    length(opt())
  })
  keep.l<-reactive({
    length(keep())
  })
  drop.l<-reactive({
    length(drop())
  })
  output$v2<-renderText({
    dd<-d()
    if (is.null(dd)) {return(NULL)}
    colnames(dd)
  })
  output$vars<-renderTable( {
    dd<-d()
    if (is.null(dd)) {return(NULL)}
    N<-table(dd[,2])
    alphas<-round(abyage(dd[,2],dd[3:(dim(dd)[2])]),3)
    tt<-rbind(N,alphas)
    tt
  })
  
  #### check outlier ####
  check.o<-eventReactive(input$check,{
    dd<-d(); nitem=dim(dd)[2]-2
    dx<-dd[dd[,2]==input$age,]
    outliercheck(input$change,dx[,1],dx[,3:(nitem+2)])
  })
  output$outl=renderText({
    outliers<-str_c(check.o(),collapse=",")
    if (length(check.o())>0) {
      HTML(  
        paste0("Here is a list of TEIDs whose removal lead to a higher alpha. <br/>",
               "to see more/fewer TEIDs, please decrease/increase the threshold.<br/>",
               outliers )
      )
    } else {
      HTML(paste("no potential outlier detected in age group with this threshold"))
    }
  })
  #### Estimating calculation time ####
  ntot<-reactive({
    if (opt.l()>0) {
      opt.l()
    } else {
      input$ntotal-keep.l()-drop.l()
    }
  })
  ndel<-reactive({
    if (drop.l()>0) {
      input$ndelete-drop.l()
    } else {
      input$ndelete
    }
  })
  
  etime <- eventReactive(input$est, {choose(ntot(),ndel())})
  mins<- reactive({etime()/400})
  
  output$estim<-renderText({
    if (mins()>30) {
      paste("Dropping",ndel(),"from a total of",ntot() ,"items. It take approximately",mins(),"min to calculate all solutions.",
            "Please add more constraints to reduce your calculation time")
    } else if (mins()>5) {
      paste("Dropping",ndel(),"from a total of",ntot() ,"items. It take approximately",mins(),"min to calculate all solutions.",
            "Go grab a coffee after you hit that Calculate button!")
      
    } else {
      paste("Dropping",ndel(),"from a total of",ntot() ,"items. It will take approximately",mins(),"min to calculate all solutions")
    }
  })
  
  ##### Calculate item selection solutions ####
#   output$regis<-renderText({
#     if(is.null(input$cal) || input$cal == 0) {return()}
#     "The system is calculating. Please wait."
#   })
  eff<-eventReactive(input$cal, {
    dd<-d(); nitem=dim(dd)[2]-2
    if (opt.l()>0) {
      optionalitem<-opt()
    } else {
      optionalitem<-c(1:nitem)[-c(drop(),keep())]
    }
    aftereffect(ndel(),optionalitem,drop(),dd[,2],dd[,3:(nitem+2)])
  })

  output$finish<-renderText({
    if (!is.null(eff())) {
     paste("Calculation finished, please see Solution tab for results.") 
    }
  })
  output$topsl<-renderDataTable({
    tops<-eff()
    top5<-tops[order(tops[,2]),]
    top5
  })
})


