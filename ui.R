shinyUI(
  navbarPage(
    title = "NBDPN: Time Monitoring",
    id = "mainNavbarPage",
    theme = shinytheme("readable"),
    tabPanel(
      "Tutorial",
      fluidPage(
                 fluidRow(#class="myRow1",
                   column(8,#div(style = "height:200px"),
                          wellPanel(
                            h4(p(style = "color:dodgerblue","Step 1: Prepare data files")),

                            hr(),
                      # strong("Data should be structured as Table 1 (on the left)."),hr(),
                   #   span("To prepare data, follow the next steps:"),
                           h6(p(style = "color:dodgerblue","FORMAT")),
                   hr(),
                      span("Your aggregated data file can be accepted in any three of the following formats:"),
                      tags$ul(
                        tags$li(("SAS (.sas7bdat)")),         
                        tags$li(("Excel (.xls, .xlsx)")),  
                        tags$li(("Tab delimited (.txt)"))),  
             hr() ,
             h6(p(style = "color:dodgerblue","VARIABLES")),
             hr(),
             span("The file should have the following variables:"),
             tags$ul(
               tags$li(strong("ID:"),"Suggested — your two-character State (e.g. FL)."), 
               tags$li(strong("Defect_Name:"), "A string that identifies your defect. There should be no spaces. ","(e.g. 'Trisomy_21' instead of 'Trisomy 21'). There is no limit to how many defects to include in the analysis."), 
               tags$li(strong("Cases:"), "Case count for that time unit.**"),
               tags$li(strong("Time:"), "Aggregate all data by year only (YYYY), or by month and year (YYYY/MM)."),
               tags$li(strong("Population:"), "The number of live births (birth defect cases + all other infants born that year) in your whole registry area for each date combination."
               )),
                            br() ,
                            br() ,
                            br() ,
                            br() 
                            #,
                           
                            #,

                          ) ),
                   column(4,#div(style = "height:200px"),
                          wellPanel(h5("Table 1: Example data for formatting"),
                       includeHTML("www/tables.HTML"),hr(),
                         em( "Your aggregate data should look like the Table 1 (above). The highlighted rows in blue are not required: they are
                          visual representations of how each defect is included in a single file.")))),
                   fluidRow(
                     column(12,
                            wellPanel(
                              h4(p(style = "color:dodgerblue","Step 2: Notes on aggregating data:")),
                              # strong(),
                              hr(),
                              hr(),
                              
                              hr(),
                              p(style = "color:dodgerblue","Notes:"),
                              p("Your Excel file or text file must have a header row with the five variables (ID DEFECT_NAME CASES TIME POPULATION)."),
                              # p(style = "color:dodgerblue","Note on Cases**:"),
                              p("** Each defect must be within the same time frame. You cannot look at one defect from 2004-2006 and another from 2003-2006. To do this, create two different files.")
                              
                              ,
                              hr()
                            )
                     )),
                     
                     fluidRow(
                       column(4,offset=4,
                              wellPanel(
                                h4(p(style = "color:dodgerblue","Step 3")),
                                hr(),
                                p( "Once the data is aggregated, go to the File Input page to start the analysis."),
                                br(),  br(),  br(),  br(),  br(),  br(),
                                actionButton("filego", "Go to File Input page!"),
                                br(),  br(),  br(),  br(),  br(),  br(),
                                br()
                                
                              ) )))),
    
    
    
    
    tabPanel("File Input Page", value = "taboutput",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   tags$style(".well {background-color:gray92;}"),
                   wellPanel(
                     h5(p("File Input"), align = "right", style = "color:dodgerblue"),
                     img(src = 'Icons/one.png', align = "center"),
                     br(),
                     br(),
                     radioButtons(
                       "MIME",
                       label = (" Data type:"),
                       choices = list(
                         "SAS" = 1,
                         "Excel" = 2,
                         "Text File" = 3
                       ),
                       selected = 1
                     ),
                     br(),
                     img(src = 'Icons/two.png', align = "center"),
                     br(),
                     br(),
                     fileInput(
                       "file1",
                       label = "Place the data here:",
                       accept = c(
                         'text/csv',
                         'text/comma-separated-values',
                         'text/tab-separated-values',
                         'text/plain',
                         '.xls',
                         '.xlsx',
                         '.sas7bdat'
                       )
                     )
                   ),
                   
                   
                   #
                   # ),
                   wellPanel(
                     #Title
                     h5(p("Time Range"), align = "right", style = "color:dodgerblue"),
                     #Widget
                     img(src = 'Icons/three.png', align = "center"),
                     br(),
                     br(),
                     
                     selectInput(
                       "datetype",
                       label = "Choose a time frame:",
                       choices = list("Year", "Month")
                     ),
                     #Title
                     img(src = 'Icons/four.png', align = "center"),
                     br(),
                     br(),
                     strong(h6("Begin Date")),
                     #Widget
                     uiOutput("BeginDateWidget"),
                     
                     #Title
                     img(src = 'Icons/five.png', align = "center"),
                     br(),
                     br(),
                     strong(h6("End Date")),
                     #Widget
                     uiOutput("EndDateWidget"),
                     
                     ####START BUTTON####
                     img(src = 'Icons/six.png', align = "center"),
                     br(),
                     br(),
                     actionButton("goButton", "Start Analysis!")
                     #,
                     # h6(p("After pressing the 'Start Analysis' button, you will have to press
                     #      'show all content' and may have to refresh the screen.", style = "color:red"))
                     )
                   # ,
                   # wellPanel(
                   #   # h4(p("About"), align="right", style = "color:dodgerblue"),
                   #   # h5(("Today's Date")),
                   #   # dateInput("Today",
                   #   #           label = "Today's Date", value =
                   #   #             NULL),
                   #   p(
                   #     h5("Contact for help:"),
                   #     span("skhan81.900", style = "color:black"),
                   #     span("@", style = "color:dodgerblue"),
                   #     span("gmail.com", style = "color:black")
                   #   )
                   # )
                   )
                 ,
                 #            #,
                 #
                 #
                 #
                 #
                 #
                 #            ) ) ,
                 # ### SATSCAN OUTPUT ####
                 # tabPanel("Time Clusters",value="taboutput",
                 #          fluidPage(
                 mainPanel(# hr(),
                   h6(textOutput(
                     "caption", container = span
                   )),
                   uiOutput("Choices"),
                   
               plotlyOutput("plots")
               # ,
               # 
               # uiOutput("contents23")
               
             )
             ))),
    
    
    
    
    tabPanel("About",
             fluidPage(
               fluidRow(
                 column(12,
                        wellPanel(h4("About Time Monitoring"),
                                  hr(),
                                  
                                  h5(p("Purpose of this app:",style = "color:dodgerblue")),
                                  p("The National Birth Defects Prevention Network (NBDPN) identified a need to 
                                    conduct analysis of changes in the increases and decreases in birth defects over time on a regular basis.
                                    This tool addresses that need by providing simplified analysis through the SaTScan (spatial scan-statistic) software."),
                                  hr(),
                                  h5(p(style = "color:dodgerblue","Methods used:")),
                                  p("SaTScan "),
                                  tags$ul(
                                    tags$li("Purely temporal"),
                                    tags$li("Retrospective"),
                                    tags$li("High-rates only (Observed > Expected)"),
                                    tags$li("Poisson distribution"))
                                  ,
                                  br()
                                  #,
                                  # br(),
                                  # br(),
                                  # hr()
                                  #,
                                  # h5(p("Note: This app works best in Internet Explorer"),style="color:red")
                                  ))),
               column(12,
                      wellPanel(
                                # "About",
                                h4("Licenses"),
                                tags$ul(
                                  
                                  
                                  tags$li(
                                    "Shiny framework courtesy of: RStudio Team (2016). RStudio: Integrated Development for R. RStudio, Inc., Boston, MA URL",
                                    (
                                      tags$a(href = "http://www.rstudio.com/.", "http://www.rstudio.com/.")
                                    )
                                  ),
                                  tags$li(
                                    "SaTScan: Kulldorff M and Information Management Services I. SaTScanTM v9.4.2: Software for the spatial and space-time scan statistics. 2015.",
                                    (tags$a(href = "www.satscan.org", "www.satscan.org"))
                                  ),
                                  tags$li(
                                    "Icons made by",
                                    (
                                      tags$a(href = "http://www.flaticon.com/authors/alessio-atzeni", "Alessio Atzeni")
                                    ),
                                    "from",
                                    (tags$a(href = "http://www.flaticon.com", "www.flaticon.com")),
                                    "is licensed by",
                                    tags$a(href = "http://creativecommons.org/licenses/by/3.0/", "Creative Commons BY 3.0")
                                  ),
                                  
                                  tags$li("Various R Packages:", tags$ul(
                                    strong(tags$li('rsatscan')),
                                    tags$li('shinythemes'),
                                    tags$li('XML')
                                  )),
                                  hr()),
                                p( span("Contact skhan81.900", style = "color:black"),
                                   span("@", style = "color:dodgerblue"),
                                   span("gmail.com for any bugs or assistance.", style = "color:black"))
                      )
               )
             )
    )
    ))
  

