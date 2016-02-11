## trying to add the options

#setwd("C:/Users/Sars/Desktop/Winter break app/Tab layout changed, and adding plotly/")
library(shiny)


shinyServer(function(input, output, session) {
  v <- reactiveValues(doPlot = FALSE)
  
  
  
  cap <- eventReactive(input$goButton, {
    "Select a defect:"
    # br()
    # hr()
    
  })
  
  
  output$caption <- renderText({
    cap()
  })
  
  output$Choices <- renderUI({
    if (is.null(Defects()))
      return(NULL)
    
    load("www/defects")
    codes <- unique(defects)
    
    code_list <- setNames(as.list(codes), codes)
    selectInput('selected_defect', label = NULL, choices = code_list)
    
    # bsTooltip('selected_defect', "Pick one or more defects.",
    #           "right", options = list(container = "body"))
    
  })
  
  
  
  ##########  ################
  
  output$EndDateWidget <- renderUI({
    if (is.null(input$datetype))
      return(NULL)
    
    switch(
      input$datetype,
      "Year" =
        mydateInput(
          'enddate',
          label = NULL,
          min = NULL,
          max = NULL,
          format = "yyyy",
          startview = "decade",
          weekstart = 0,
          language = "en",
          minviewmode = "years",
          width = NULL
        ),
      "Month" =
        mydateInput(
          'enddate',
          label = NULL,
          min = NULL,
          max = NULL,
          format = "yyyy-mm",
          startview = "month",
          weekstart = 0,
          language = "en",
          minviewmode = "months",
          width = NULL
        )
    )
  })
  
  output$BeginDateWidget <- renderUI({
    if (is.null(input$datetype))
      return(NULL)
    
    switch(
      input$datetype,
      "Year" =
        mydateInput(
          'begindate',
          label = NULL,
          min = NULL,
          max = NULL,
          format = "yyyy",
          startview = "decade",
          weekstart = 0,
          language = "en",
          minviewmode = "years",
          width = NULL
        ),
      "Month" =
        mydateInput(
          'begindate',
          label = NULL,
          min = NULL,
          max = NULL,
          format = "yyyy-mm",
          startview = "month",
          weekstart = 0,
          language = "en",
          minviewmode = "months",
          width = NULL
        )
    )
  })
  
  
  # CREATION OF TABS
  #     output$dynamicTabs <- renderUI({
  #       tabs <- lapply(input$in6,function(x){
  #         tabPanel(
  #           title = paste0(x)
  #           ,tags$iframe(src=('tmp2.html'),
  #                        width = '100%',
  #                        height = 600,
  #                        frameborder = 0,
  #                        marginheight = 0
  #           )
  #           ,value=x
  #         )
  #       })
  #       do.call(tabsetPanel,c(tabs,id='selectedTab'))
  #     })
  #
  
  # FILE CREATE
  
  observeEvent(input$goButton, {
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    if (input$MIME == 1) {
      case <- read.sas7bdat(inFile$datapath)
    }
    
    else if (input$MIME == 2) {
      case <- readWorksheetFromFile(inFile$datapath, sheet = 1)
    }
    
    else if (input$MIME == 3) {
      case <- read.table(inFile$datapath, header = T)
      
    }
    
    
    #       pop<-case[c("ID", "TIME", "POPULATION")]
    #       pop<-plyr::rename(pop, replace=c("POPULATION"="CASES"))
    #
    defects <- case$DEFECT_NAME
    #
    #       case<-case[c("ID", "CASES", "TIME")]
    #
    ##saving cases, pop, and defects###
    
    save(case, file = file.path(outputDir, "case"))
    #       save(pop, file=file.path(outputDir, "pop"))
    save(defects, file = file.path(outputDir, "defects"))
    
    
    
  })
  
  
  Defects <- eventReactive(input$goButton, {
    if (is.null("www/defects"))
      return(NULL)
    
    load("www/defects")
    defects
    
  })
  
  
  Case <- eventReactive(input$goButton, {
    if (is.null("www/case"))
      return(NULL)
    
    load("www/case")
    case
  })
  
  
  
  end <- reactive({
    if (input$datetype == "Month") {
      end <-
        gsub('.{2}$', '31', strftime(as.Date(input$enddate, origin = "1970-01-1"), "%Y/%m/%d"))
      end
    }
    else if (input$datetype == "Year") {
      end <-
        gsub('.{5}$', '12/31', strftime(as.Date(input$enddate, origin = "1970-01-1"), "%Y/%m/%d"))
      end
    }
    
    
    
  })
  
  begin <- reactive({
    if (input$datetype == "Month") {
      begin <-
        gsub('.{2}$', '01', strftime(
          as.Date(input$begindate, origin = "1970-01-1"),
          "%Y/%m/%d"
        ))
      begin
    }
    else if (input$datetype == "Year") {
      begin <-
        gsub('.{5}$', '1/1', strftime(
          as.Date(input$begindate, origin = "1970-01-1"),
          "%Y/%m/%d"
        ))
      begin
    }
    begin
    
  })
  
  
  
  date3 <- reactive({
    if (input$datetype == "Month") {
      date3 <- 2
    }
    else if (input$datetype == "Year") {
      date3 <- 1
    }
    
    date3
  })
  
  
  observeEvent(input$goButton, {
    v$doPlot <- TRUE
    
    load("www/case")
    
    #       load("www/pop")
    # load("www/defects")
    
    case_list <- list()
    defects <- unique(case["DEFECT_NAME"])
    V2_pop <- list()
    V2_case <- list()
    ss_case <- list()
    ss_pop <- list()
    thefile <- list()
    filepath = paste0(outputDir, "/thefile")
    library(rsatscan)
    ##
    
    #system('chmod 755 SaTScan/satscan_stdc++6_x86_64_64bit')
    
    for (i in 1:length(unique(case$DEFECT_NAME))){
      ### can explore this later? you can have dynamic time frames..
      # ----------> case<-case[!((case$TIME == 2001) | case$TIME == 2012),]
      
      code<-defects$DEFECT_NAME[i]
      
      
      case_list[[i]]<- case[which(case$DEFECT_NAME==code), ]
      
      V2_pop[[i]]<-case_list[[i]][c("ID", "TIME", "POPULATION")]
      V2_pop[[i]]<-plyr::rename(V2_pop[[i]], replace=c("POPULATION"="CASES"))
      
      
      V2_case[[i]]<-case_list[[i]][c("ID", "CASES", "TIME")]
      
      
      
      CaseFile<-paste(paste("Defect",code,sep="_"), ".cas", sep="")
      PopulationFile<-paste(paste("Defect",code,sep="_"), ".pop", sep="")
      
      
      ss_case[[i]]<-write.cas(V2_case[[i]], outputDir, paste("Defect",code,sep="_"))
      ss_pop[[i]]<-write.pop(V2_pop[[i]], outputDir, paste("Defect",code,sep="_"))
      
      
      invisible(ss.options(reset=TRUE))
      ss.options(list(CaseFile=CaseFile, PrecisionCaseTimes=date3(), StartDate=begin(), EndDate=end(), PopulationFile=PopulationFile, AnalysisType=2))
      ss.options(list(TimeAggregationUnits=date3(), OutputShapefiles="n", MostLikelyClusterEachCentroidDBase="n"))
      
      ss.options(list(MostLikelyClusterCaseInfoEachCentroidDBase="n", CensusAreasReportedClustersDBase="n", IncludeRelativeRisksCensusAreasDBase="n", SaveSimLLRsDBase="n"))
      
      
      ss.options(list(IncludePurelySpatial="n", IncludeClusters=0, IntervalStartRange="2000/1/1, 2000/12/31"))
      
      ss.options(list(IntervalEndRange="2000/1/1, 2000/12/31"))
      ss.options(list(ReportHierarchicalClusters="n"))
      
      ss.options(list(ReportGiniClusters="n", OutputTemporalGraphHTML="y"))
      
      write.ss.prm(outputDir, (paste("Parameter",code,sep="_")))
      
      #satscan(outputDir, (paste("Parameter",code,sep="_")), sslocation="C:/Program Files (x86)/SaTScan")
      
      thefile[[i]]<-satscan(outputDir, (paste("Parameter",code,sep="_")), sslocation="SaTScan", 
                            #ssbatchfilename = "satscan_stdc++6_x86_64_64bit", 
                            verbose=T )
      
      save(thefile, file=filepath)
      
    }
    
  })
  
  
  


  output$plots <- renderPlotly({

    if (is.null(Defects()))
      return(NULL)

       x<-input$selected_defect



        src_name=paste0("www/Parameter_",x,".temporal.html")
        writeLines(iconv(readLines(src_name), from = "ISO-8859-15", to = "UTF8"), paste0("www/",x,"ex2.html"))
       src_new<-paste0("www/",x,"ex2.html")


        ##############START DATA CLEAN##########

        doc.html=htmlParse(src_new)


        newdoc<-capture.output(doc.html)

        dates<-newdoc[35]

        graph_data<-newdoc[38]


        dates2<-ex_between(dates, "categories: [", "]")
        dates2<-gsub("'","",dates2)

        dates_final<-strsplit(dates2, ",")

        if (input$datetype == "Month") {
          cats<-lapply(dates_final, function(x) paste0(gsub("/","-",x),"-01"))
          cats
        }
        
        else if (input$datetype == "Year") {
          cats<-lapply(dates_final, function(x) paste0(gsub("/","-",x),"-01-01"))
          }
        #cats<-lapply(dates_final, function(x) paste0(gsub("/","-",x),"-01"))
        
        # cats<-lapply(dates_final, function(x) paste0(gsub("/","-",x),"-01"))
        cats2<-unlist(cats)
        cats3<-as.Date(cats2)

        cats4<-format(cats3, format="%Y-%m")


        minuteDataPlot2 <-  as.numeric(as.POSIXct(cats3))

        minutes<-minuteDataPlot2*1000

        date_final5<-minutes

        nseries2<-gsub("series: ","",graph_data)
        nseries2<-gsub("]\r","",nseries2)

        nser<-sub(".*'column',:]", "", strsplit(nseries2, "id:")[[1]])
        nser<-nser[-1]
        id<-ex_between(nser, "'" , "', z")
        data<- ex_between(nser, "data: [", "]")
        names(data)<-unlist(id)

        tmp <- sapply(data, as.character)

        #  unique(as.numeric(unlist(strsplit(gsub("[^0-9]", "", unlist(ll)), ""))))

        s <- strsplit(as.character(data), ',')
        names(s)<-unlist(id)

        output <- matrix(unlist(s), ncol = 4, byrow = TRUE)

        u<-t(output)
        colnames(u)  <- (unlist(id))

        data_final<-u
        data_final<-as.data.frame(data_final)

        data_final$clu_ind <- ifelse(data_final$cluster == "null","Observed", "Cluster")



        data_final$dates <- unlist(t(dates_final))


        data_final <- transform(data_final, obs_exp = as.numeric(levels(data_final$obs_exp))[data_final$obs_exp],
                                obs = as.numeric(levels(data_final$obs))[data_final$obs],
                                exp = as.numeric(levels(data_final$exp))[data_final$exp]
        )


        # data_final$dates_UTC2 <- date_final5

         data_final$dates_UTC3 <- cats3
        # data_final$dates_UTC4 <- cats4

        #########END DATA CLEAN ############

        ######### START PLOTLY GRAPHS ########

        p<-plot_ly(x = data_final$dates_UTC3,
                   y = data_final$obs,
                   name = "Observed",
                   type="bar",
                   color=data_final$clu_ind,
                   marker=list(
                       line=list(
                          color="#E2E2E2")),
                   colors = c("#4572A7","#AA4643"),
                   opacity=.8)

          add_trace(x = data_final$dates_UTC3,
                    y = data_final$exp,
                    name = "Expected Cases",
                    type="scatter",
                    marker = list(
                                   color="rgb(16, 32, 77)"))


          a <- list(
            title="",
            # autotick = FALSE,
            ticks = "outside",
            tick0 = min(data_final$dates_UTC3)
            ,
            tickangle=10,
            # dtick = 0.25,
            # ticklen = 5,
            # tickwidth = 2,
            tickcolor = toRGB("blue")
          )


        p <-layout(title=paste("Detected Clusters of",x),
          xaxis=a,
          yaxis=list(
                  title = "Number of Cases"
                  ),
           titlefont=(list(
             # family="Arial",
             size=16)),
          # autosize=T,
           # plot_bgcolor="#E2E2E2"
          #,
          #,
           width=800, height=500
          ,legend=(list(bgcolor="#E2E2E2"))
          )
        p


  })

  
  
  
  
  
  
  # output$dynamicTabs <- renderUI({
  #   if (is.null(Defects()))
  #     return(NULL)
  #
  #   # plots<-plots()
  #   # df<-plots()
  # #  plotname<-df$plotname
  #
  #   load("www/defects")
  #
  #   codes<-unique(defects)
  #
  #   code_list<-setNames(as.list(codes), codes)
  #
  #
  #
  #   tabs <- lapply(code_list,function(x) {
  #
  #   plotname <- paste("plot", x, sep="")
  #
  #     tabPanel(
  #       title = paste0(x),
  #       # tags$iframe(src =
  #    uiOutput(plots[plotname])
  #     )
  #   })
  #   do.call(tabsetPanel,c(tabs,id = 'selectedTab'))
  #
  # })
  
  
  output$inc <- renderUI({
    if (is.null(Defects()))
      return(NULL)
    
    x <- input$selected_defect
    
    src_name = paste0("www/Parameter_", x, ".temporal.html")
    writeLines(iconv(readLines(src_name), from = "ISO-8859-15", to = "UTF8"),
               paste0("www/", x, "ex2.html"))
    src_new <- paste0("www/", x, "ex2.html")
    
    return(tags$iframe(
      src = paste0(x, "ex2.html"),
      width = '100%',
      height = 800,
      scrolling = "auto"
    ))
    
  })
  
  
  
  
  
  
  output$contents23 <- renderUI({
    if (is.null(Defects()))
      return(NULL)
    load("www/thefile")
    
    str1 <- thefile[[1]]$main[25]
    str2 <- thefile[[1]]$main[26]
    str3 <- thefile[[1]]$main[27]
    str4 <- thefile[[1]]$main[28]
    str5 <- thefile[[1]]$main[29]
    str6 <- thefile[[1]]$main[30]
    str7 <- thefile[[1]]$main[31]
    str8 <- thefile[[1]]$main[32]
    
    h6(HTML(
      paste(str1, str2, str3, str4, str5, str6, str7, str8, sep = '<br/>')
    ))
    
  })
  
  
  observeEvent(input$filego, {
    updateNavbarPage(session, "mainNavbarPage", selected = "taboutput")
  })
  
})
