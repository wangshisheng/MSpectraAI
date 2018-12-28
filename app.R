library(shiny)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(ggjoy)
library(openxlsx)
library(gdata)
library(DT)
library(gtools)
library(ggsci)
library(mzR)
library(impute)
library(plyr)
library(tidyr)
library(abind)
library(data.table)
library(parallel)
library(ggrastr)
library(ggthemes)
library(viridis)
library(glue)
library(ComplexHeatmap)
library(circlize)
library(ROCR)
library(keras)
colpalettes<-unique(c(pal_npg("nrc")(10),pal_aaas("default")(10),pal_nejm("default")(8),pal_lancet("lanonc")(9),
                      pal_jama("default")(7),pal_jco("default")(10),pal_ucscgb("default")(26),pal_d3("category10")(10),
                      pal_locuszoom("default")(7),pal_igv("default")(51),
                      pal_uchicago("default")(9),pal_startrek("uniform")(7),
                      pal_tron("legacy")(7),pal_futurama("planetexpress")(12),pal_rickandmorty("schwifty")(12),
                      pal_simpsons("springfield")(16),pal_gsea("default")(12)))
#
ui<-renderUI(
  fluidPage(
    title="MSpectraAI",
    shinyjs::useShinyjs(),
    fluidRow(
      column(5,div(
        HTML(
          "<div style='text-align:right;margin-right:0px'>
          <a href='#' target=''><img src='MSpectraAI_logotizuo.jpg' width='100px'>
          </a>
          </div>"
        )
        )),
      column(7,div(
        HTML(
          "<div style='text-align:left;margin-top:30px;margin-left:-20px'>
          <a href='#' target=''><img src='MSpectraAI_logoti.jpg' height='80px'>
          </a>
          </div>"
        )
        ))
        ),
    tagList(
      tags$head(
        tags$link(rel="stylesheet", type="text/css",href="busystyle.css"),
        tags$script(type="text/javascript", src = "busy.js"),
        tags$style(type="text/css", "
                           #loadmessage {
                     position: fixed;
                     top: 0px;
                     left: 0px;
                     width: 100%;
                     height:100%;
                     padding: 250px 0px 5px 0px;
                     text-align: center;
                     font-weight: bold;
                     font-size: 100px;
                     color: #000000;
                     background-color: #D6D9E4;
                     opacity:0.6;
                     z-index: 105;
                     }
                     "),
        tags$script('
                            var dimension = [0, 0];
                    $(document).on("shiny:connected", function(e) {
                    dimension[0] = window.innerWidth;
                    dimension[1] = window.innerHeight;
                    Shiny.onInputChange("dimension", dimension);
                    });
                    $(window).resize(function(e) {
                    dimension[0] = window.innerWidth;
                    dimension[1] = window.innerHeight;
                    Shiny.onInputChange("dimension", dimension);
                    });
                    ')
      )
    ),

    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div(h2(strong("Calculating......")),img(src="rmd_loader.gif"),id="loadmessage")),
    tabsetPanel(
      tabPanel(
        "Welcome",
        uiOutput("welcomeui")
      ),
      tabPanel(
        "Import Data",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3("Import Raw Data"),
            h4("1. Peaks data:"),
            radioButtons(
              "fileType_Input",
              label = h4("File format:"),
              choices = list(".mzXML" = 1,".mzML"=2),
              selected = 1,
              inline = TRUE
            ),
            fileInput('file1', 'Import your data：',
                      accept=c('.mzXML','.mzML'),multiple=TRUE),
            tags$hr(style="border-color: grey;"),
            h4("2. Samples information data:"),
            radioButtons(
              "mchuanshaodxyibanfileType_Input_fenzu",
              label = h4("File format:"),
              choices = list(".xlsx" = 1,".xls"=2, ".csv/txt" = 3),
              selected = 1,
              inline = TRUE
            ),
            fileInput('mchuanshaodxyibanfile1_fenzu', 'Import your data：',
                      accept=c('text/csv','text/plain','.xlsx','.xls')),
            checkboxInput('mchuanshaodxyibanheader_fenzu', 'Header？', TRUE),
            checkboxInput('mchuanshaodxyibanfirstcol_fenzu', 'First column？', TRUE),
            conditionalPanel(condition = "input.mchuanshaodxyibanfileType_Input_fenzu==1",
                             numericInput("mchuanshaodxyibanxlsxindex_fenzu","Sheet index:",value = 1)),
            conditionalPanel(condition = "input.mchuanshaodxyibanfileType_Input_fenzu==2",
                             numericInput("mchuanshaodxyibanxlsxindex_fenzu","Sheet index:",value = 1)),
            conditionalPanel(condition = "input.mchuanshaodxyibanfileType_Input_fenzu==3",
                             radioButtons('mchuanshaodxyibansep_fenzu', 'Separator：',
                                          c(Comma=',',
                                            Semicolon=';',
                                            Tab='\t',
                                            BlankSpace=' '),
                                          ','))
          ),
          mainPanel(
            width = 9,
            hr(),
            actionButton("mcsbtn_yuanshidata","Calculate",icon("paper-plane"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            hidden(
              div(
                id="mcsbtn_yuanshidata_hid",
                h4("1. Raw files："),
                downloadButton("peaksdatadl","Download"),
                dataTableOutput("peaksdata"),
                h4("2. Samples information data："),
                dataTableOutput("samplesdata")
              )
            )
          )
        )
      ),
      tabPanel(
        "Mass Spectra Information",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            uiOutput("rawfilenamesui"),
            tags$hr(style="border-color: grey;"),
            h4("Histograms parameter"),
            numericInput('histbreaks', 'breaks：', value=100)
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              tabPanel(
                "MS",
                actionButton("mcsbtn_MS","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #00008B; border-color: #00008B"),
                hidden(
                  div(
                    id="mcsbtn_MSid",
                    div(id="MSsize_div",checkboxInput("MSsize","Change figure size？",FALSE)),
                    conditionalPanel(
                      condition = "input.MSsize==true",
                      sliderInput("MSsize_height","figure height：",min = 500,max = 5000,step = 100,value = 800)
                    ),
                    downloadButton("MSpicdl","Download"),
                    plotOutput("MSplot")
                  )
                )
              ),
              tabPanel(
                "MSMS",
                actionButton("mcsbtn_MSMS","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #00008B; border-color: #00008B"),
                hidden(
                  div(
                    id="mcsbtn_MSMSid",
                    div(id="MSMSsize_div",checkboxInput("MSMSsize","Change figure size？",FALSE)),
                    conditionalPanel(
                      condition = "input.MSMSsize==true",
                      sliderInput("MSMSsize_height","figure height：",min = 500,max = 5000,step = 100,value = 800)
                    ),
                    downloadButton("MSMSpicdl","Download"),
                    plotOutput("MSMSplot")
                  )
                )
              ),
              tabPanel(
                "Spectra Number",
                actionButton("mcsbtn_specnum","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #00008B; border-color: #00008B"),
                hidden(
                  div(
                    id="mcsbtn_specnumid",
                    downloadButton("spectradatadl","Download"),
                    dataTableOutput("spectradata")
                  )
                )
              )
            )
          )
        )
      ),
      tabPanel(
        "SWATH Extraction",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            uiOutput("rawfnswathui"),
            textInput('windowsize', 'Window Size：', value="20x20"),
            tags$hr(style="border-color: grey;"),
            textInput('msscope', 'MS Scope：', value="350;1800"),
            numericInput("msnumfilter","MS number filter:",value=300),
            textInput('msmsscope', 'MSMS Scope：', value="200;1500"),
            numericInput("msmsnumfilter","MSMS number filter:",value=100)
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              tabPanel(
                "MS 2D Map",
                actionButton("mcsbtn_MSswath","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #FF8888; border-color: #FF8888"),
                hidden(
                  div(
                    id="mcsbtn_MSswathid",
                    radioButtons(
                      "MS_2D_Map",
                      label = "",
                      choices = list("Map" = 1,"Data"=2),
                      selected = 1,
                      inline = TRUE
                    ),
                    conditionalPanel(
                      condition = "input.MS_2D_Map==1",
                      div(id="MSswathsize_div",checkboxInput("MSswathsize","Change figure size？",FALSE)),
                      conditionalPanel(
                        condition = "input.MSswathsize==true",
                        sliderInput("MSswathsize_height","figure height：",min = 500,max = 5000,step = 100,value = 800)
                      ),
                      downloadButton("MSswathpicdl","Download"),
                      plotOutput("MSswathplot")
                    ),
                    conditionalPanel(
                      condition = "input.MS_2D_Map==2",
                      downloadButton("MSswathdatadl","Download"),
                      dataTableOutput("MSswathdata")
                    )
                  )
                )
              ),
              tabPanel(
                "MS Intensity Density",
                actionButton("mcsbtn_msswathds","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #FF8888; border-color: #FF8888"),
                hidden(
                  div(
                    id="mcsbtn_msswathdsid",
                    fluidRow(
                      column(
                        width = 4,
                        textInput("rtindex","Retention Time Index:",value = "500:510")
                      ),
                      column(
                        width = 4,
                        textInput("mzindex","M/Z Window Index:",value = "200:400")
                      )
                    ),
                    div(id="msswathds_div",checkboxInput("msswathds","Change figure size？",FALSE)),
                    conditionalPanel(
                      condition = "input.msswathds==true",
                      sliderInput("msswathds_height","figure height：",min = 500,max = 5000,step = 100,value = 800)
                    ),
                    downloadButton("msswathdspicdl","Download"),
                    plotOutput("msswathdspic")
                  )
                )
              ),
              tabPanel(
                "MSMS 2D Map",
                actionButton("mcsbtn_MSMSswath","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #FF8888; border-color: #FF8888"),
                hidden(
                  div(
                    id="mcsbtn_MSMSswathid",
                    radioButtons(
                      "MSMS_2D_Map",
                      label = "",
                      choices = list("Map" = 1,"Data"=2),
                      selected = 1,
                      inline = TRUE
                    ),
                    conditionalPanel(
                      condition = "input.MSMS_2D_Map==1",
                      div(id="MSMSswathsize_div",checkboxInput("MSMSswathsize","Change figure size？",FALSE)),
                      conditionalPanel(
                        condition = "input.MSMSswathsize==true",
                        sliderInput("MSMSswathsize_height","figure height：",min = 500,max = 5000,step = 100,value = 800)
                      ),
                      downloadButton("MSMSswathpicdl","Download"),
                      plotOutput("MSMSswathplot")
                    ),
                    conditionalPanel(
                      condition = "input.MSMS_2D_Map==2",
                      downloadButton("MSMSswathdatadl","Download"),
                      dataTableOutput("MSMSswathdata")
                    )
                  )
                )
              ),
              tabPanel(
                "MSMS Intensity Density",
                actionButton("mcsbtn_msmsswathds","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #FF8888; border-color: #FF8888"),
                hidden(
                  div(
                    id="mcsbtn_msmsswathdsid",
                    fluidRow(
                      column(
                        width = 4,
                        textInput("msmsrtindex","Retention Time Index:",value = "500:510")
                      ),
                      column(
                        width = 4,
                        textInput("msmsmzindex","M/Z Window Index:",value = "200:400")
                      )
                    ),
                    div(id="msmsswathds_div",checkboxInput("msmsswathds","Change figure size？",FALSE)),
                    conditionalPanel(
                      condition = "input.msmsswathds==true",
                      sliderInput("msmsswathds_height","figure height：",min = 500,max = 5000,step = 100,value = 800)
                    ),
                    downloadButton("msmsswathdspicdl","Download"),
                    plotOutput("msmsswathdspic")
                  )
                )
              )
            )
          )
        )
      ),
      tabPanel(
        "All Data Integration",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3("Please Attention: This step is a little time-consuming, please be patient and take a cup of coffee or tea~~^_^")
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              tabPanel(
                "MS Data",
                actionButton("mcsbtn_MSalldata","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #AA7700; border-color: #AA7700"),
                hidden(
                  div(
                    id="mcsbtn_MSalldataid",
                    downloadButton("MSalldatadl","Download"),
                    h4("Data Preview:"),
                    dataTableOutput("MSalldata")
                  )
                )
              ),
              tabPanel(
                "MSMS Data",
                actionButton("mcsbtn_MSMSalldata","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #AA7700; border-color: #AA7700"),
                hidden(
                  div(
                    id="mcsbtn_MSMSalldataid",
                    downloadButton("MSMSalldatadl","Download"),
                    h4("Data Preview:"),
                    dataTableOutput("MSMSalldata")
                  )
                )
              )
            )
          )
        )
      ),
      tabPanel(
        "Deep Learning Model",
        sidebarLayout(
          sidebarPanel(
            width = 6,
            textAreaInput("deepmodelinput",h3("Type your Deep Learning Model:"),width = "100%",height="600px",
                          value = "keras_model_sequential() %>%
                          layer_dense(
                          units              = 128,
                          kernel_initializer = 'uniform',
                          activation         = 'relu',
                          input_shape        = 400) %>%
                          layer_dropout(rate = 0.2) %>%
                          layer_dense(
                          units              = 64,
                          kernel_initializer = 'uniform',
                          activation         = 'relu') %>%
                          layer_dropout(rate = 0.2) %>%
                          layer_dense(
                          units              = 3,
                          kernel_initializer = 'uniform',
                          activation         = 'softmax') %>%
                          compile(
                          optimizer = 'rmsprop',
                          loss      = 'categorical_crossentropy',
                          metrics   = c('accuracy')
                          )")
          ),
          mainPanel(
            width = 6,
            tabsetPanel(
              tabPanel(
                "Model Summary",
                actionButton("mcsbtn_modelsumm","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #7700FF; border-color: #7700FF"),
                hidden(
                  div(
                    id="mcsbtn_modelsummid",
                    verbatimTextOutput("modelsumm")
                  )
                )
              )
            )
          )
        )
      ),
      tabPanel(
        "Model Results",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectInput("modelresultype",h4("Results type:"),choices = c("MS"=1,"MSMS"=2)),
            numericInput("dnnepochs","epochs:",value = 10),
            numericInput("dnnbatch","batch size:",value = 32)
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              tabPanel(
                "Confusion Matrix",
                actionButton("mcsbtn_confmat","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #880000; border-color: #880000"),
                hidden(
                  div(
                    id="mcsbtn_confmatid",
                    h4("1. Matrix:"),
                    downloadButton("confmatdatadl","Download"),
                    dataTableOutput("confmatdata"),
                    h4("2. Accuracy Rate:"),
                    verbatimTextOutput("confmatsumm")
                  )
                )
              ),
              tabPanel(
                "Heatmap",
                actionButton("mcsbtn_cmheatmap","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #880000; border-color: #880000"),
                hidden(
                  div(
                    id="mcsbtn_cmheatmapid",
                    div(id="cmheatmap_div",checkboxInput("cmheatmap","Change figure size？",FALSE)),
                    conditionalPanel(
                      condition = "input.cmheatmap==true",
                      sliderInput("cmheatmap_height","figure height：",min = 500,max = 5000,step = 100,value = 800)
                    ),
                    downloadButton("cmheatmappicdl","Download"),
                    plotOutput("cmheatmappic")
                  )
                )
              ),
              tabPanel(
                "ROC Curve",
                actionButton("mcsbtn_roccurve","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #880000; border-color: #880000"),
                hidden(
                  div(
                    id="mcsbtn_roccurveid",
                    textInput("twoclassl","Two Class labels:",value="0;1"),
                    div(id="roccurve_div",checkboxInput("roccurve","Change figure size？",FALSE)),
                    conditionalPanel(
                      condition = "input.roccurve==true",
                      sliderInput("roccurve_height","figure height：",min = 500,max = 5000,step = 100,value = 800)
                    ),
                    #uiOutput("roccurveui"),
                    downloadButton("roccurvepicdl","Download"),
                    plotOutput("roccurveplot")
                  )
                )
              ),
              tabPanel(
                div(id="f1scorediv","F1 Score"),
                bsTooltip("f1scorediv","The class labels are same as those in ROC Curve！",
                          options = list(container = "body")),
                actionButton("mcsbtn_f1score","Calculate",icon("paper-plane"),
                             style="color: #fff; background-color: #880000; border-color: #880000"),
                hidden(
                  div(
                    id="mcsbtn_f1scoreid",
                    downloadButton("f1scoredfdl","Download"),
                    dataTableOutput("f1scoredf")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
#
server<-shinyServer(function(input, output, session){
  options(shiny.maxRequestSize=2048*1024^2)
  usertimenum<-as.numeric(Sys.time())
  #ui
  output$welcomeui<-renderUI({
    screenwidth<-input$dimension[1]
    #screenheight<-input$dimension[2]
    #tryCatch({},error=function(e) NULL)
    if(is.null(screenwidth)){
      return(NULL)
    }else{
      if(screenwidth<=1024){
        imgwidth<-350
      }
      else if(screenwidth>1024 & screenwidth<=1440){
        imgwidth<-450
      }
      else{
        imgwidth<-600
      }
    }

    fluidRow(
      div(style="text-align:center",h1("~~Welcome~~")),
      column(
        6,
        div(style="text-align:center",h3("Graphic Abstract")),
        div(style="text-align:center",
            a(href='https://github.com/wangshisheng/MSpectraAI',
              img(src='MSpectraAI_logo.jpg',height=imgwidth)))
      ),
      column(
        6,
        div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;",
            h3("I. This is an open source project, please visit our github to get the source code:"),
            a(href="https://github.com/wangshisheng/MSpectraAI",h4("https://github.com/wangshisheng/MSpectraAI")),
            h3("II. We recommend you to run this software locally."),
            h3("III. The detailed manual can be found here:"),
            a(href="https://github.com/wangshisheng/MSpectraAI/blob/master/SupportingNotes.pdf",h4("https://github.com/wangshisheng/MSpectraAI/blob/master/SupportingNotes.pdf")))
      )
    )
  })
  #show data
  peaksdataout<-reactive({
    files <- input$file1
    if (is.null(files)){
      dataread<-read.csv("phonscel_filenames.csv",stringsAsFactors = F,check.names = F)
    }else{
      dataread<-files[,1:2]
    }
    dataread
  })
  samplesdataout<-reactive({
    files <- input$mchuanshaodxyibanfile1_fenzu
    if (is.null(files)){
      dataread<-read.csv("phonscel_sampleinfo.csv",header = T,stringsAsFactors = F,check.names = F)
    }else{
      if (input$mchuanshaodxyibanfileType_Input_fenzu == "1"){
        dataread<-read.xlsx(files$datapath,rowNames=input$mchuanshaodxyibanfirstcol_fenzu,
                            colNames = input$mchuanshaodxyibanheader_fenzu,sheet = input$mchuanshaodxyibanxlsxindex_fenzu)
      }
      else if(input$mchuanshaodxyibanfileType_Input_fenzu == "2"){
        if(sum(input$mchuanshaodxyibanfirstcol_fenzu)==1){
          rownametfmchuanshaodxyiban_fenzu<-1
        }else{
          rownametfmchuanshaodxyiban_fenzu<-NULL
        }
        dataread<-read.xls(files$datapath,sheet = input$mchuanshaodxyibanxlsxindex_fenzu,header=input$mchuanshaodxyibanheader_fenzu,
                           row.names = rownametfmchuanshaodxyiban_fenzu, sep=input$mchuanshaodxyibansep_fenzu,stringsAsFactors = F)
      }
      else{
        if(sum(input$mchuanshaodxyibanfirstcol_fenzu)==1){
          rownametfmchuanshaodxyiban_fenzu<-1
        }else{
          rownametfmchuanshaodxyiban_fenzu<-NULL
        }
        dataread<-read.csv(files$datapath,header=input$mchuanshaodxyibanheader_fenzu,
                           row.names = rownametfmchuanshaodxyiban_fenzu, sep=input$mchuanshaodxyibansep_fenzu,stringsAsFactors = F)
      }
    }
    dataread
  })

  observeEvent(
    input$mcsbtn_yuanshidata,{
      shinyjs::show(id = "mcsbtn_yuanshidata_hid", anim = FALSE)
      output$peaksdata<-renderDataTable({
        datatable(peaksdataout(), options = list(pageLength = 10))
      })
      output$peaksdatadl<-downloadHandler(
        filename = function(){paste("rawdatainfo",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(peaksdataout(),file = file,row.names = F)
        }
      )
      output$samplesdata<-renderDataTable({
        samplesdf<-samplesdataout()
        datatable(samplesdf, options = list(pageLength = 10))
      })
    }
  )

  output$rawfilenamesui<-renderUI({
    files <- input$file1
    if (is.null(files)){
      dataread<-list.files(path="data/",pattern = input$fileType_Input)
    }else{
      dataread<-files$name
    }
    selectInput("fileselect",label = h4("Select a file:"),choices = dataread)
  })

  spectraout<-reactive({
    files <- input$file1
    if (is.null(files)){
      filepath<-paste0("data/",input$fileselect)
    }else{
      filepath<-files$datapath[grep(input$fileselect,files$name)]
    }
    datams<-openMSfile(filename = filepath)
    msdataheader <- header(datams)
    msspectra<-spectra(datams)
    list(msdataheader=msdataheader,msspectra=msspectra)
  })

  logboxplot_height <- reactive({
    heightx<-input$MSsize_height
    heightx
  })

  observeEvent(
    input$mcsbtn_MS,{
      shinyjs::show(id = "mcsbtn_MSid", anim = FALSE)
      output$MSplot<-renderPlot({
        dataspectrahead<-spectraout()$msdataheader
        dataspectra<-spectraout()$msspectra
        msnum<-unlist(lapply(dataspectra[which(dataspectrahead$msLevel==1)],nrow))
        hist(msnum,breaks = input$histbreaks,main = "Histogram of MS")
      },height = logboxplot_height)

      MSplotout<-reactive({
        dataspectrahead<-spectraout()$msdataheader
        dataspectra<-spectraout()$msspectra
        msnum<-unlist(lapply(dataspectra[which(dataspectrahead$msLevel==1)],nrow))
        hist(msnum,breaks = 100)
      })

      output$MSpicdl<-downloadHandler(
        filename = function(){paste("MShistogram_figure",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file,width =logboxplot_height()/100,height = logboxplot_height()/100)
          print(MSplotout())
          dev.off()
        }
      )
    }
  )

  logboxplot_afterheight <- reactive({
    heightx<-input$MSMSsize_height
    heightx
  })

  observeEvent(
    input$mcsbtn_MSMS,{
      shinyjs::show(id = "mcsbtn_MSMSid", anim = FALSE)
      output$MSMSplot<-renderPlot({
        dataspectrahead<-spectraout()$msdataheader
        dataspectra<-spectraout()$msspectra
        msnum<-unlist(lapply(dataspectra[which(dataspectrahead$msLevel==2)],nrow))
        hist(msnum,breaks = input$histbreaks,main = "Histogram of MSMS")
      },height = logboxplot_afterheight)

      MSMSplotout<-reactive({
        dataspectrahead<-spectraout()$msdataheader
        dataspectra<-spectraout()$msspectra
        msnum<-unlist(lapply(dataspectra[which(dataspectrahead$msLevel==2)],nrow))
        hist(msnum,breaks = input$histbreaks,main = "Histogram of MSMS")
      })

      output$MSMSpicdl<-downloadHandler(
        filename = function(){paste("MSMShistogram_figure",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file,width =logboxplot_afterheight()/100,height = logboxplot_afterheight()/100)
          print(MSMSplotout())
          dev.off()
        }
      )
    }
  )

  spectraallout<-reactive({
    files <- input$file1
    if (is.null(files)){
      filepath<-list.files(path="data/",pattern = input$fileType_Input,full.names=TRUE)
      filepathnames<-list.files(path="data/",pattern = input$fileType_Input)
    }else{
      filepath<-files$datapath
      filepathnames<-files$name
    }
    ms1spnum<-ms2spnum<-vector()
    withProgress(message = 'Calculating data', style = "notification", detail = "File 1", value = 0,{
      for(i in 1:length(filepath)){
        datamsi<-openMSfile(filename = filepath[i])
        msdataheaderi <- header(datamsi)
        ms1spnum[i]<-sum(msdataheaderi$msLevel==1)
        ms2spnum[i]<-sum(msdataheaderi$msLevel==2)
        incProgress(1/length(filepath), detail = paste("File", i))
      }
    })

    msspnumdf<-data.frame(filenames=filepathnames,ms1spnum=ms1spnum,ms2spnum=ms2spnum)
    msspnumdf
  })

  observeEvent(
    input$mcsbtn_specnum,{
      shinyjs::show(id = "mcsbtn_specnumid", anim = FALSE)
      output$spectradata<-renderDataTable({
        dataspectrahead<-spectraallout()
        datatable(dataspectrahead)
      })

      output$MSMSpicdl<-downloadHandler(
        filename = function(){paste("MSpectraNum_data",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(spectraallout(),file = file,row.names = F)
        }
      )
    }
  )
  ##
  output$rawfnswathui<-renderUI({
    files <- input$file1
    if (is.null(files)){
      dataread<-list.files(path="data/",pattern = input$fileType_Input)
    }else{
      dataread<-files$name
    }
    selectInput("fileselectswath",label = h4("Select a file:"),choices = dataread)
  })
  msspectra2dout<-reactive({
    files <- input$file1
    if (is.null(files)){
      filepath<-paste0("data/",input$fileselectswath)
    }else{
      filepath<-files$datapath[grep(input$fileselectswath,files$name)]
    }

    windowsize1<-as.numeric(strsplit(input$windowsize,"x")[[1]])
    if(length(windowsize1)==2){
      dimn<-windowsize1[1]*windowsize1[2]
    }else{
      dimn<-windowsize1
    }
    msscopestr<-as.numeric(strsplit(input$msscope,";")[[1]])
    msscopelow<-msscopestr[1]
    msscopebig<-msscopestr[2]
    intervals<-seq(msscopelow,msscopebig,length.out = dimn+1)
    intervalsdf<-data.frame(x1=intervals[1:dimn],x2=intervals[2:(dimn+1)])
    datams<-openMSfile(filename = filepath)
    msdataheader <- header(datams)
    msspectra<-spectra(datams)
    msdataheader_s<-msdataheader[msdataheader$msLevel==1,]
    msspectra_ms<-msspectra[which(msdataheader$msLevel==1)]
    rownum<-unlist(lapply(msspectra_ms,nrow))
    msspectra1<-msspectra_ms[rownum>=input$msnumfilter]
    intervalsdf3<-lapply(msspectra1,function(xxx,dim,intervalsdf){
      dimn<-dim
      intervalsdf<-intervalsdf
      xxx<-as.matrix(xxx)
      intervalsdf2<-lapply(xxx[,1],function(x){
        xx1<-as.numeric(x)
        xx2<-data.table::between(xx1,lower=intervalsdf$x1,upper=intervalsdf$x2)
        xx2
      })
      intervalsdf1<-do.call(rbind,intervalsdf2)
      intervalindex<-which(intervalsdf1==TRUE,arr.ind = TRUE)
      invalm<-matrix(0,nrow = nrow(intervalsdf1),ncol = ncol(intervalsdf1))
      for(ii in 1:nrow(intervalindex)){
        invalm[intervalindex[ii,1],intervalindex[ii,2]]<-xxx[intervalindex[ii,1],2]
      }
      intervals_val<-apply(invalm,2,sum)
      intervals_val
    },dim=dimn,intervalsdf=intervalsdf)
    intervalsdf4<-do.call(rbind,intervalsdf3)
    intervalsdf5<-intervalsdf4/apply(intervalsdf4, 1, max)
    rownames(intervalsdf5)<-round(msdataheader_s$retentionTime[rownum>=input$msnumfilter],3)
    colnames(intervalsdf5)<-round(intervalsdf$x1,3)
    intervalsdf5
  })
  MSswathsize_heightx<-reactive({
    MSswathsize_heightx<-input$MSswathsize_height
    MSswathsize_heightx
  })
  observeEvent(
    input$mcsbtn_MSswath,{
      shinyjs::show(id = "mcsbtn_MSswathid", anim = FALSE)
      output$MSswathplot<-renderPlot({
        dataspectra<-msspectra2dout()
        dataspectra_melt<-reshape::melt(dataspectra)
        ggplot(dataspectra_melt) +geom_point_rast(aes(x=X1, y=X2, color=value), shape=15, size=5)+
          scale_color_viridis(na.value="#FFFFFF00")+
          xlab("Retention Time") + ylab("M/Z")+
          theme(legend.position="right")+theme_base()
      },height = MSswathsize_heightx)

      MSswathplotout<-reactive({
        dataspectra<-msspectra2dout()
        dataspectra_melt<-reshape::melt(dataspectra)
        ggplot(dataspectra_melt) +geom_point_rast(aes(x=X1, y=X2, color=value), shape=15, size=5)+
          scale_color_viridis(na.value="#FFFFFF00")+
          xlab("Retention Time") + ylab("M/Z")+
          theme(legend.position="right")+theme_base()
      })

      output$MSswathpicdl<-downloadHandler(
        filename = function(){paste("MS2Dmap_figure",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file,width =MSswathsize_heightx()/100,height = MSswathsize_heightx()/100)
          print(MSswathplotout())
          dev.off()
        }
      )

      output$MSswathdata<-renderDataTable({
        dataspectrahead<-msspectra2dout()
        datatable(dataspectrahead[,1:20])
      })

      output$MSswathdatadl<-downloadHandler(
        filename = function(){paste("MSpectra2D_data",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(msspectra2dout(),file = file,row.names = F)
        }
      )
    }
  )
  ##MSMS2D
  msmsspectra2dout<-reactive({
    files <- input$file1
    if (is.null(files)){
      filepath<-paste0("data/",input$fileselectswath)
    }else{
      filepath<-files$datapath[grep(input$fileselectswath,files$name)]
    }

    windowsize1<-as.numeric(strsplit(input$windowsize,"x")[[1]])
    if(length(windowsize1)==2){
      dimn<-windowsize1[1]*windowsize1[2]
    }else{
      dimn<-windowsize1
    }
    msmsscopestr<-as.numeric(strsplit(input$msmsscope,";")[[1]])
    msmsscopelow<-msmsscopestr[1]
    msmsscopebig<-msmsscopestr[2]
    intervals<-seq(msmsscopelow,msmsscopebig,length.out = dimn+1)
    intervalsdf<-data.frame(x1=intervals[1:dimn],x2=intervals[2:(dimn+1)])
    datamsms<-openMSfile(filename = filepath)
    msmsdataheader <- header(datamsms)
    msmsspectra<-spectra(datamsms)
    msmsdataheader_s<-msmsdataheader[msmsdataheader$msLevel==2,]
    msmsspectra_ms<-msmsspectra[which(msmsdataheader$msLevel==2)]
    rownum<-unlist(lapply(msmsspectra_ms,nrow))
    msmsspectra1<-msmsspectra_ms[rownum>=input$msmsnumfilter]
    intervalsdf3<-lapply(msmsspectra1,function(xxx,dim,intervalsdf){
      dimn<-dim
      intervalsdf<-intervalsdf
      xxx<-as.matrix(xxx)
      intervalsdf2<-lapply(xxx[,1],function(x){
        xx1<-as.numeric(x)
        xx2<-data.table::between(xx1,lower=intervalsdf$x1,upper=intervalsdf$x2)
        xx2
      })
      intervalsdf1<-do.call(rbind,intervalsdf2)
      intervalindex<-which(intervalsdf1==TRUE,arr.ind = TRUE)
      invalm<-matrix(0,nrow = nrow(intervalsdf1),ncol = ncol(intervalsdf1))
      for(ii in 1:nrow(intervalindex)){
        invalm[intervalindex[ii,1],intervalindex[ii,2]]<-xxx[intervalindex[ii,1],2]
      }
      intervals_val<-apply(invalm,2,sum)
      intervals_val
    },dim=dimn,intervalsdf=intervalsdf)
    intervalsdf4<-do.call(rbind,intervalsdf3)
    intervalsdf5<-intervalsdf4/apply(intervalsdf4, 1, max)
    rownames(intervalsdf5)<-round(msmsdataheader_s$retentionTime[rownum>=input$msmsnumfilter],3)
    colnames(intervalsdf5)<-round(intervalsdf$x1,3)
    intervalsdf5
  })
  MSMSswathsize_heightx<-reactive({
    MSMSswathsize_heightx<-input$MSMSswathsize_height
    MSMSswathsize_heightx
  })
  observeEvent(
    input$mcsbtn_MSMSswath,{
      shinyjs::show(id = "mcsbtn_MSMSswathid", anim = FALSE)
      output$MSMSswathplot<-renderPlot({
        dataspectra<-msmsspectra2dout()
        dataspectra_melt<-reshape::melt(dataspectra)
        ggplot(dataspectra_melt) +geom_point_rast(aes(x=X1, y=X2, color=value), shape=15, size=5)+
          scale_color_viridis(na.value="#FFFFFF00")+
          xlab("Retention Time") + ylab("M/Z")+
          theme(legend.position="right")+theme_base()
      },height = MSMSswathsize_heightx)

      MSMSswathplotout<-reactive({
        dataspectra<-msmsspectra2dout()
        dataspectra_melt<-reshape::melt(dataspectra)
        ggplot(dataspectra_melt) +geom_point_rast(aes(x=X1, y=X2, color=value), shape=15, size=5)+
          scale_color_viridis(na.value="#FFFFFF00")+
          xlab("Retention Time") + ylab("M/Z")+
          theme(legend.position="right")+theme_base()
      })

      output$MSMSswathpicdl<-downloadHandler(
        filename = function(){paste("MSMS2Dmap_figure",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file,width =MSMSswathsize_heightx()/100,height = MSMSswathsize_heightx()/100)
          print(MSMSswathplotout())
          dev.off()
        }
      )

      output$MSMSswathdata<-renderDataTable({
        dataspectrahead<-msmsspectra2dout()
        datatable(dataspectrahead[,1:20])
      })

      output$MSMSswathdatadl<-downloadHandler(
        filename = function(){paste("MSMSpectra2D_data",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(msmsspectra2dout(),file = file,row.names = F)
        }
      )
    }
  )
  ##
  msswathds_heightx<-reactive({
    msswathds_heightx<-input$msswathds_height
    msswathds_heightx
  })
  observeEvent(
    input$mcsbtn_msswathds,{
      shinyjs::show(id = "mcsbtn_msswathdsid", anim = FALSE)
      output$msswathdspic<-renderPlot({
        dataspectra<-msspectra2dout()
        rtindexstr<-as.numeric(strsplit(input$rtindex,":")[[1]])
        mzindexstr<-as.numeric(strsplit(input$mzindex,":")[[1]])
        rtindexstrx<-rtindexstr[1]:rtindexstr[2]
        mzindexstrx<-mzindexstr[1]:mzindexstr[2]
        if(length(rtindexstrx)>length(mzindexstrx)){
          datareadx<-dataspectra[rtindexstrx,mzindexstrx]
        }else{
          datareadx<-t(dataspectra[rtindexstrx,mzindexstrx])
        }

        mtcars1x<-as.data.frame(datareadx)
        mtcars1x$carnamesx<-rownames(datareadx)
        mtcars1dfx<-mtcars1x %>% arrange(by=carnamesx) %>% gather(group, abundance, -carnamesx)
        ggplot(mtcars1dfx, aes(x = abundance, y = group,fill=group)) +
          geom_joy(scale = 2,alpha = 0.6) +
          scale_x_continuous(expand = c(0.01, 0)) +
          scale_y_discrete(expand = c(0.01, 0))+
          scale_fill_manual(values = colpalettes[1:ncol(datareadx)])+theme_base()+
          ylab("Density")+
          theme(legend.position="none")
      },height = msswathds_heightx)
      msswathdspicout<-reactive({
        dataspectra<-msspectra2dout()
        rtindexstr<-as.numeric(strsplit(input$rtindex,":")[[1]])
        mzindexstr<-as.numeric(strsplit(input$mzindex,":")[[1]])
        rtindexstrx<-rtindexstr[1]:rtindexstr[2]
        mzindexstrx<-mzindexstr[1]:mzindexstr[2]
        if(length(rtindexstrx)>length(mzindexstrx)){
          datareadx<-dataspectra[rtindexstrx,mzindexstrx]
        }else{
          datareadx<-t(dataspectra[rtindexstrx,mzindexstrx])
        }
        mtcars1x<-as.data.frame(datareadx)
        mtcars1x$carnamesx<-rownames(datareadx)
        mtcars1dfx<-mtcars1x %>% arrange(by=carnamesx) %>% gather(group, abundance, -carnamesx)
        ggplot(mtcars1dfx, aes(x = abundance, y = group,fill=group)) +
          geom_joy(scale = 2,alpha = 0.6) +
          scale_x_continuous(expand = c(0.01, 0)) +
          scale_y_discrete(expand = c(0.01, 0))+
          scale_fill_manual(values = colpalettes[1:ncol(datareadx)])+theme_base()+
          ylab("Density")+
          theme(legend.position="none")
      })
      output$msswathdspicdl<-downloadHandler(
        filename = function(){paste("MSDensity_figure",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file,width =msswathds_heightx()/100,height = msswathds_heightx()/100)
          print(msswathdspicout())
          dev.off()
        }
      )

    }
  )
##
  msmsswathds_heightx<-reactive({
    msmsswathds_heightx<-input$msmsswathds_height
    msmsswathds_heightx
  })
  observeEvent(
    input$mcsbtn_msmsswathds,{
      shinyjs::show(id = "mcsbtn_msmsswathdsid", anim = FALSE)
      output$msmsswathdspic<-renderPlot({
        dataspectra<-msmsspectra2dout()
        rtindexstr<-as.numeric(strsplit(input$rtindex,":")[[1]])
        mzindexstr<-as.numeric(strsplit(input$mzindex,":")[[1]])
        rtindexstrx<-rtindexstr[1]:rtindexstr[2]
        mzindexstrx<-mzindexstr[1]:mzindexstr[2]
        if(length(rtindexstrx)>length(mzindexstrx)){
          datareadx<-dataspectra[rtindexstrx,mzindexstrx]
        }else{
          datareadx<-t(dataspectra[rtindexstrx,mzindexstrx])
        }

        mtcars1x<-as.data.frame(datareadx)
        mtcars1x$carnamesx<-rownames(datareadx)
        mtcars1dfx<-mtcars1x %>% arrange(by=carnamesx) %>% gather(group, abundance, -carnamesx)
        ggplot(mtcars1dfx, aes(x = abundance, y = group,fill=group)) +
          geom_joy(scale = 2,alpha = 0.6) +
          scale_x_continuous(expand = c(0.01, 0)) +
          scale_y_discrete(expand = c(0.01, 0))+
          scale_fill_manual(values = colpalettes[1:ncol(datareadx)])+theme_base()+
          ylab("Density")+
          theme(legend.position="none")
      },height = msmsswathds_heightx)
      msmsswathdspicout<-reactive({
        dataspectra<-msmsspectra2dout()
        rtindexstr<-as.numeric(strsplit(input$msmsrtindex,":")[[1]])
        mzindexstr<-as.numeric(strsplit(input$msmsmzindex,":")[[1]])
        rtindexstrx<-rtindexstr[1]:rtindexstr[2]
        mzindexstrx<-mzindexstr[1]:mzindexstr[2]
        if(length(rtindexstrx)>length(mzindexstrx)){
          datareadx<-dataspectra[rtindexstrx,mzindexstrx]
        }else{
          datareadx<-t(dataspectra[rtindexstrx,mzindexstrx])
        }
        mtcars1x<-as.data.frame(datareadx)
        mtcars1x$carnamesx<-rownames(datareadx)
        mtcars1dfx<-mtcars1x %>% arrange(by=carnamesx) %>% gather(group, abundance, -carnamesx)
        ggplot(mtcars1dfx, aes(x = abundance, y = group,fill=group)) +
          geom_joy(scale = 2,alpha = 0.6) +
          scale_x_continuous(expand = c(0.01, 0)) +
          scale_y_discrete(expand = c(0.01, 0))+
          scale_fill_manual(values = colpalettes[1:ncol(datareadx)])+theme_base()+
          ylab("Density")+
          theme(legend.position="none")
      })
      output$msmsswathdspicdl<-downloadHandler(
        filename = function(){paste("MSMSDensity_figure",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file,width =msmsswathds_heightx()/100,height = msmsswathds_heightx()/100)
          print(msmsswathdspicout())
          dev.off()
        }
      )

    }
  )
  ##
  MSalldataout<-reactive({
    files <- input$file1
    if (is.null(files)){
      allmsfiles<-list.files(path="data/",pattern = input$fileType_Input,full.names = TRUE)
    }else{
      allmsfiles<-files$datapath
    }

    windowsize1<-as.numeric(strsplit(input$windowsize,"x")[[1]])
    if(length(windowsize1)==2){
      dimn<-windowsize1[1]*windowsize1[2]
    }else{
      dimn<-windowsize1
    }
    msscopestr<-as.numeric(strsplit(input$msscope,";")[[1]])
    msscopelow<-msscopestr[1]
    msscopebig<-msscopestr[2]
    intervals<-seq(msscopelow,msscopebig,length.out = dimn+1)
    intervalsdf<-data.frame(x1=intervals[1:dimn],x2=intervals[2:(dimn+1)])

    no_cores <- 4
    cl <- makeCluster(no_cores)
    allmsarraymatl<-list()
    ngroupnum<-vector()
    withProgress(message = 'Generating data', style = "notification", detail = "File 1", value = 0,{
      for(i in 1:length(allmsfiles)){
        datams<-openMSfile(filename = allmsfiles[i])
        msdataheader <- header(datams)
        msspectra<-spectra(datams)
        msspectra_ms<-msspectra[which(msdataheader$msLevel==1)]
        rownum<-unlist(lapply(msspectra_ms,nrow))
        msspectra1<-msspectra_ms[rownum>=input$msnumfilter]
        intervalsdf3<-parallel::parLapply(cl, msspectra1,function(xxx,dim,intervalsdf){
          dimn<-dim
          intervalsdf<-intervalsdf
          xxx<-as.matrix(xxx)
          intervalsdf2<-lapply(xxx[,1],function(x){
            xx1<-as.numeric(x)
            xx2<-data.table::between(xx1,lower=intervalsdf$x1,upper=intervalsdf$x2)
            xx2
          })
          intervalsdf1<-do.call(rbind,intervalsdf2)
          intervalindex<-which(intervalsdf1==TRUE,arr.ind = TRUE)
          invalm<-matrix(0,nrow = nrow(intervalsdf1),ncol = ncol(intervalsdf1))
          for(ii in 1:nrow(intervalindex)){
            invalm[intervalindex[ii,1],intervalindex[ii,2]]<-xxx[intervalindex[ii,1],2]
          }
          intervals_val<-apply(invalm,2,sum)
          intervals_val
        },dim=dimn,intervalsdf=intervalsdf)
        intervalsdf4<-do.call(rbind,intervalsdf3)
        allmsarraymatl[[i]]<-intervalsdf4
        ngroupnum[i]<-length(msspectra1)
        incProgress(1/length(allmsfiles), detail = paste("File", i))
      }
    })
    stopCluster(cl)
    allmsarray_cell<-do.call(rbind,allmsarraymatl)
    classdata<-samplesdataout()
    allmsarray_cell_label<-rep(classdata[[2]],ngroupnum)
    allmsarray_cellm<-cbind(Class=allmsarray_cell_label,allmsarray_cell)
    list(allmsarray_cellm=allmsarray_cellm,allmsarray_cell=allmsarray_cell,
         allmsarray_cell_label=allmsarray_cell_label,ngroupnum=ngroupnum)
  })
  observeEvent(
    input$mcsbtn_MSalldata,{
      shinyjs::show(id = "mcsbtn_MSalldataid", anim = FALSE)
      output$MSalldata<-renderDataTable({
        MSalldatax<-MSalldataout()$allmsarray_cellm
        datatable(MSalldatax[,1:30])
      })
      output$MSalldatadl<-downloadHandler(
        filename = function(){paste("MSall_data",usertimenum,".csv",sep="")},
        content = function(file){
          MSalldatax<-as.data.frame(MSalldataout()$allmsarray_cellm)
          fwrite(MSalldatax,file = file)
        }
      )
    }
  )
##
  MSMSalldataout<-reactive({
    files <- input$file1
    if (is.null(files)){
      allmsmsfiles<-list.files(path="data/",pattern = input$fileType_Input,full.names = TRUE)
    }else{
      allmsmsfiles<-files$datapath
    }

    windowsize1<-as.numeric(strsplit(input$windowsize,"x")[[1]])
    if(length(windowsize1)==2){
      dimn<-windowsize1[1]*windowsize1[2]
    }else{
      dimn<-windowsize1
    }
    msmsscopestr<-as.numeric(strsplit(input$msmsscope,";")[[1]])
    msmsscopelow<-msmsscopestr[1]
    msmsscopebig<-msmsscopestr[2]
    intervals<-seq(msmsscopelow,msmsscopebig,length.out = dimn+1)
    intervalsdf<-data.frame(x1=intervals[1:dimn],x2=intervals[2:(dimn+1)])

    no_cores <- 4
    cl <- makeCluster(no_cores)
    allmsmsarraymatl<-list()
    ngroupnum<-vector()
    withProgress(message = 'Generating data', style = "notification", detail = "File 1", value = 0,{
      for(i in 1:length(allmsmsfiles)){
        datamsms<-openMSfile(filename = allmsmsfiles[i])
        msmsdataheader <- header(datamsms)
        msmsspectra<-spectra(datamsms)
        msmsspectra_ms<-msmsspectra[which(msmsdataheader$msLevel==2)]
        rownum<-unlist(lapply(msmsspectra_ms,nrow))
        msmsspectra1<-msmsspectra_ms[rownum>=input$msmsnumfilter]
        intervalsdf3<-parallel::parLapply(cl, msmsspectra1,function(xxx,dim,intervalsdf){
          dimn<-dim
          intervalsdf<-intervalsdf
          xxx<-as.matrix(xxx)
          intervalsdf2<-lapply(xxx[,1],function(x){
            xx1<-as.numeric(x)
            xx2<-data.table::between(xx1,lower=intervalsdf$x1,upper=intervalsdf$x2)
            xx2
          })
          intervalsdf1<-do.call(rbind,intervalsdf2)
          intervalindex<-which(intervalsdf1==TRUE,arr.ind = TRUE)
          invalm<-matrix(0,nrow = nrow(intervalsdf1),ncol = ncol(intervalsdf1))
          for(ii in 1:nrow(intervalindex)){
            invalm[intervalindex[ii,1],intervalindex[ii,2]]<-xxx[intervalindex[ii,1],2]
          }
          intervals_val<-apply(invalm,2,sum)
          intervals_val
        },dim=dimn,intervalsdf=intervalsdf)
        intervalsdf4<-do.call(rbind,intervalsdf3)
        allmsmsarraymatl[[i]]<-intervalsdf4
        ngroupnum[i]<-length(msmsspectra1)
        incProgress(1/length(allmsmsfiles), detail = paste("File", i))
      }
    })
    stopCluster(cl)
    allmsmsarray_cell<-do.call(rbind,allmsmsarraymatl)
    classdata<-samplesdataout()
    allmsmsarray_cell_label<-rep(classdata[[2]],ngroupnum)
    allmsmsarray_cellm<-cbind(Class=allmsmsarray_cell_label,allmsmsarray_cell)
    list(allmsmsarray_cellm=allmsmsarray_cellm,allmsmsarray_cell=allmsmsarray_cell,
         allmsmsarray_cell_label=allmsmsarray_cell_label,ngroupnum=ngroupnum)
  })
  observeEvent(
    input$mcsbtn_MSMSalldata,{
      shinyjs::show(id = "mcsbtn_MSMSalldataid", anim = FALSE)
      output$MSMSalldata<-renderDataTable({
        MSMSalldatax<-MSMSalldataout()$allmsmsarray_cellm
        datatable(MSMSalldatax[,1:30])
      })
      output$MSMSalldatadl<-downloadHandler(
        filename = function(){paste("MSMSall_data",usertimenum,".csv",sep="")},
        content = function(file){
          MSMSalldatax<-as.data.frame(MSMSalldataout()$allmsmsarray_cellm)
          fwrite(MSMSalldatax,file = file)
        }
      )
    }
  )
##Deep Learning
  observeEvent(
    input$mcsbtn_modelsumm,{
      shinyjs::show(id = "mcsbtn_modelsummid", anim = FALSE)
      output$modelsumm<-renderPrint({
        typemodel<-input$deepmodelinput
        typemodelsumm<-eval(parse(text = glue(typemodel)))
        summary(typemodelsumm)
      })
    }
  )
##
  confmatoutms<-reactive({
    cmdata<-MSalldataout()
    allmsarray_cell<-cmdata$allmsarray_cell/apply(cmdata$allmsarray_cell, 1, max)
    allmsarray_cell_label<-cmdata$allmsarray_cell_label

    resloop_dnn<-NULL
    numcum1<-c(0,cumsum(cmdata$ngroupnum))
    withProgress(message = 'Training data', style = "notification", detail = "File 1", value = 0,{
      for(i in 1:length(cmdata$ngroupnum)){
        ABindex_testxx<-c((numcum1[i]+1):numcum1[i+1])
        test_imagesxx <- allmsarray_cell[ABindex_testxx,]
        train_imagesxx <- allmsarray_cell[-ABindex_testxx,]
        train_labelsxx <- to_categorical(allmsarray_cell_label[-ABindex_testxx])
        test_labelsxx <- to_categorical(allmsarray_cell_label[ABindex_testxx])

        typemodel<-input$deepmodelinput
        typemodelsumm<-eval(parse(text = glue(typemodel)))

        typemodelsumm %>% fit(train_imagesxx, train_labelsxx, epochs = input$dnnepochs, batch_size = input$dnnbatch,verbose = 0)
        test_labelsprexx1<-typemodelsumm %>% predict_classes(test_imagesxx)
        tab1x<-table(test_labelsprexx1)
        tab1<-matrix(tab1x, ncol=length(tab1x), byrow=TRUE, dimnames=NULL)
        colnames(tab1)<-names(tab1x)
        if(i==1){
          resloop_dnn<-tab1
        }else{
          resloop_dnn<-smartbind(resloop_dnn,tab1)
        }
        incProgress(1/length(cmdata$ngroupnum), detail = paste("File", i))
      }
    })
    resloop_dnnx<-resloop_dnn#[-1,]
    resloop_dnnx[is.na(resloop_dnnx)]<-0
    samplesdf<-samplesdataout()
    rownames(resloop_dnnx)<-samplesdf[[1]]
    resloop_dnnx
  })
  confmatoutmsms<-reactive({
    cmdata<-MSMSalldataout()
    allmsarray_cell<-cmdata$allmsmsarray_cell/apply(cmdata$allmsmsarray_cell, 1, max)
    allmsarray_cell_label<-cmdata$allmsmsarray_cell_label

    resloop_dnn<-NULL
    numcum1<-c(0,cumsum(cmdata$ngroupnum))
    withProgress(message = 'Training data', style = "notification", detail = "File 1", value = 0,{
      for(i in 1:length(cmdata$ngroupnum)){
        ABindex_testxx<-c((numcum1[i]+1):numcum1[i+1])
        test_imagesxx <- allmsarray_cell[ABindex_testxx,]
        train_imagesxx <- allmsarray_cell[-ABindex_testxx,]
        train_labelsxx <- to_categorical(allmsarray_cell_label[-ABindex_testxx])
        test_labelsxx <- to_categorical(allmsarray_cell_label[ABindex_testxx])

        typemodel<-input$deepmodelinput
        typemodelsumm<-eval(parse(text = glue(typemodel)))

        typemodelsumm %>% fit(train_imagesxx, train_labelsxx, epochs = input$dnnepochs, batch_size = input$dnnbatch,verbose = 0)
        test_labelsprexx1<-typemodelsumm %>% predict_classes(test_imagesxx)
        tab1x<-table(test_labelsprexx1)
        tab1<-matrix(tab1x, ncol=length(tab1x), byrow=TRUE, dimnames=NULL)
        colnames(tab1)<-names(tab1x)
        if(i==1){
          resloop_dnn<-tab1
        }else{
          resloop_dnn<-smartbind(resloop_dnn,tab1)
        }
        incProgress(1/length(cmdata$ngroupnum), detail = paste("File", i))
      }
    })
    resloop_dnnx<-resloop_dnn#[-1,]
    resloop_dnnx[is.na(resloop_dnnx)]<-0
    samplesdf<-samplesdataout()
    rownames(resloop_dnnx)<-samplesdf[[1]]
    resloop_dnnx
  })
  observeEvent(
    input$mcsbtn_confmat,{
      shinyjs::show(id = "mcsbtn_confmatid", anim = FALSE)
      output$confmatdata<-renderDataTable({
        if(input$modelresultype==1){
          resloop_dnnx<-confmatoutms()
        }else{
          resloop_dnnx<-confmatoutmsms()
        }
        datatable(resloop_dnnx)
      })
      output$confmatdatadl<-downloadHandler(
        filename = function(){paste("ConfMatrix_data",usertimenum,".csv",sep="")},
        content = function(file){
          if(input$modelresultype==1){
            resloop_dnnx<-confmatoutms()
          }else{
            resloop_dnnx<-confmatoutmsms()
          }
          write.csv(resloop_dnnx,file = file,row.names = F)
        }
      )
      output$confmatsumm<-renderPrint({
        if(input$modelresultype==1){
          resloop_dnnx<-confmatoutms()
        }else{
          resloop_dnnx<-confmatoutmsms()
        }
        samplesdf<-samplesdataout()
        classlabel<-unique(samplesdf[[2]])
        preclassi<-as.numeric(apply(resloop_dnnx,1,which.max))
        preclass<-classlabel[preclassi]
        originalclass<-samplesdf[[2]]
        rightrate<-sum(originalclass==preclass)/length(preclass)
        print(round(rightrate,3))
      })
    }
  )
  ##Heatmap
  cmheatmap_heightx<-reactive({
    cmheatmap_heightx<-input$cmheatmap_height
    cmheatmap_heightx
  })
  observeEvent(
    input$mcsbtn_cmheatmap,{
      shinyjs::show(id = "mcsbtn_cmheatmapid", anim = FALSE)
      output$cmheatmappic<-renderPlot({
        if(input$modelresultype==1){
          confmatoutdf<-confmatoutms()
        }else{
          confmatoutdf<-confmatoutmsms()
        }
        heatm<-confmatoutdf/apply(confmatoutdf,1,sum)
        typeclass<-paste0("Class", colnames(confmatoutdf))
        typeclasslist<-list(type1 = colpalettes[38:(38+ncol(confmatoutdf)-1)])
        names(typeclasslist$type1)<-typeclass
        ha_column <- HeatmapAnnotation(df = data.frame(type1 = typeclass),show_legend=FALSE,col = typeclasslist)
        samplesdf<-samplesdataout()
        rowleftclass<-as.character(samplesdf[[2]])
        rowleftclasslist<-list(type2=colpalettes[38:(38+ncol(confmatoutdf)-1)])
        names(rowleftclasslist$type2)<-unique(rowleftclass)
        ha_rowleft = rowAnnotation(df = data.frame(type2 = rowleftclass),show_legend=FALSE,
                                   col = rowleftclasslist, width = unit(1, "cm"))
        classlabel<-unique(samplesdf[[2]])
        preclassi<-as.numeric(apply(confmatoutdf,1,which.max))
        preclass<-as.character(classlabel[preclassi])
        preclasslist<-list(type2=colpalettes[38:(38+ncol(confmatoutdf)-1)])
        names(preclasslist$type2)<-unique(rowleftclass)#unique(preclass)
        ha_rowright = rowAnnotation(df = data.frame(type2 = preclass),show_legend=FALSE,
                                    col = preclasslist, width = unit(1, "cm"))
        f1 = colorRamp2(seq(min(heatm), max(heatm), length = 3), c("blue", "grey", "red"))
        ht1 = Heatmap(heatm, name = "Probability",col = f1, column_title = "", top_annotation = ha_column,
                      cluster_rows=FALSE,cluster_columns = FALSE,cell_fun = function(j, i, x, y, width, height, fill) {
                        grid.text(sprintf("%.3f", heatm[i, j]), x, y, gp = gpar(fontsize = 15,col="white"))
                      },heatmap_legend_param=list(legend_direction = "horizontal",legend_width = unit(5, "cm"),
                                                  title_position = "lefttop"),
                      column_names_side="top")
        ht_list = ha_rowleft+ht1+ha_rowright
        draw(ht_list, heatmap_legend_side = "bottom")
      },height = cmheatmap_heightx)
      cmheatmappicout<-reactive({
        if(input$modelresultype==1){
          confmatoutdf<-confmatoutms()
        }else{
          confmatoutdf<-confmatoutmsms()
        }
        heatm<-confmatoutdf/apply(confmatoutdf,1,sum)
        typeclass<-paste0("Class", colnames(confmatoutdf))
        typeclasslist<-list(type1 = colpalettes[38:(38+ncol(confmatoutdf)-1)])
        names(typeclasslist$type1)<-typeclass
        ha_column = HeatmapAnnotation(df = data.frame(type1 = typeclass),show_legend=FALSE,col = typeclasslist)
        samplesdf<-samplesdataout()
        rowleftclass<-as.character(samplesdf[[2]])
        rowleftclasslist<-list(type2=colpalettes[38:(38+ncol(confmatoutdf)-1)])
        names(rowleftclasslist$type2)<-unique(rowleftclass)
        ha_rowleft = rowAnnotation(df = data.frame(type2 = rowleftclass),show_legend=FALSE,
                                   col = rowleftclasslist, width = unit(1, "cm"))
        classlabel<-unique(samplesdf[[2]])
        preclassi<-as.numeric(apply(confmatoutdf,1,which.max))
        preclass<-as.character(classlabel[preclassi])
        preclasslist<-list(type2=colpalettes[38:(38+ncol(confmatoutdf)-1)])
        names(preclasslist$type2)<-unique(preclass)
        ha_rowright = rowAnnotation(df = data.frame(type2 = preclass),show_legend=FALSE,
                                    col = preclasslist, width = unit(1, "cm"))
        f1 = colorRamp2(seq(min(heatm), max(heatm), length = 3), c("blue", "grey", "red"))
        ht1 = Heatmap(heatm, name = "Probability",col = f1, column_title = "", top_annotation = ha_column,
                      cluster_rows=FALSE,cluster_columns = FALSE,cell_fun = function(j, i, x, y, width, height, fill) {
                        grid.text(sprintf("%.3f", heatm[i, j]), x, y, gp = gpar(fontsize = 15,col="white"))
                      },heatmap_legend_param=list(legend_direction = "horizontal",
                                                  legend_width = unit(5, "cm"),title_position = "lefttop"),
                      column_names_side="top")
        ht_list = ha_rowleft+ht1+ha_rowright
        draw(ht_list, heatmap_legend_side = "bottom")
      })
      output$cmheatmappicdl<-downloadHandler(
        filename = function(){paste("HeatMap_figure",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file,width =cmheatmap_heightx()/100,height = cmheatmap_heightx()/100)
          print(cmheatmappicout())
          dev.off()
        }
      )
    }
  )
  ##ROC
  #output$roccurveui<-renderUI({
  #  samplesdf<-samplesdataout()
  #  classnum<-length(unique(samplesdf[[2]]))
  #  if(classnum>2){
  #    combclass<-combn(classnum, 2, FUN = paste, collapse = 'vs')
  #    selectInput("rocclass",
  #                label = h4("Select Class VS:"),
  #                choices = combclass,selected = 1)
  #  }else{
  #    return(NULL)
  #  }
  #})
  roccurvepout<-reactive({
    samplesdf<-samplesdataout()
    classnum<-length(unique(samplesdf[[2]]))
    #roccinput<-input$rocclass
    if(classnum>2){
      rocclassstr<-as.numeric(strsplit(input$twoclassl,";")[[1]])
      if(input$modelresultype==1){
        cmdata<-MSalldataout()
        rocindex1<-c(cmdata$allmsarray_cell_label==rocclassstr[1] | cmdata$allmsarray_cell_label==rocclassstr[2])
        allmsarray_cell<-cmdata$allmsarray_cell[rocindex1,]/apply(cmdata$allmsarray_cell[rocindex1,], 1, max)
        allmsarray_cell_label1<-cmdata$allmsarray_cell_label[rocindex1]
      }else{
        cmdata<-MSMSalldataout()
        rocindex1<-c(cmdata$allmsmsarray_cell_label==rocclassstr[1] | cmdata$allmsmsarray_cell_label==rocclassstr[2])
        allmsarray_cell<-cmdata$allmsmsarray_cell[rocindex1,]/apply(cmdata$allmsmsarray_cell[rocindex1,], 1, max)
        allmsarray_cell_label1<-cmdata$allmsmsarray_cell_label[rocindex1]
      }

      rocindex2<-c(samplesdf[[2]]==rocclassstr[1] | samplesdf[[2]]==rocclassstr[2])
      allmsarray_cell_label<-rep(0:1,as.numeric(table(allmsarray_cell_label1)))
      cumnum<-cumsum(cmdata$ngroupnum[rocindex2])

      resloop_dnn<-NULL
      numcum1<-c(0,cumnum)
      typemodel1<-input$deepmodelinput
      typemodel<-gsub(" ","",typemodel1)
      aagrep<-gregexpr("units=\\d",typemodel)
      aagrepul<-unlist(aagrep)
      aagrepar<-attributes(aagrep[[1]])
      xx1<-aagrepul[length(aagrepul)]
      xx2<-aagrepar$match.length[length(aagrepar$match.length)]
      substr(typemodel,xx1,xx1+xx2-1)<-"units=2"
      #typemodel<<-typemodel
      withProgress(message = 'Training data', style = "notification", detail = "File 1", value = 0,{
        for(i in 1:length(cumnum)){
          ABindex_testxx<-c((numcum1[i]+1):numcum1[i+1])
          test_imagesxx <- allmsarray_cell[ABindex_testxx,]
          train_imagesxx <- allmsarray_cell[-ABindex_testxx,]
          train_labelsxx <- to_categorical(allmsarray_cell_label[-ABindex_testxx])
          test_labelsxx <- to_categorical(allmsarray_cell_label[ABindex_testxx])
          typemodelsumm<-eval(parse(text = glue(typemodel)))
          typemodelsumm %>% fit(train_imagesxx, train_labelsxx, epochs = input$dnnepochs, batch_size = input$dnnbatch,verbose = 0)
          test_labelsprexx1<-typemodelsumm %>% predict_classes(test_imagesxx)
          tab1x<-table(test_labelsprexx1)
          tab1<-matrix(tab1x, ncol=length(tab1x), byrow=TRUE, dimnames=NULL)
          colnames(tab1)<-names(tab1x)
          if(i==1){
            resloop_dnn<-tab1
          }else{
            resloop_dnn<-smartbind(resloop_dnn,tab1)
          }
          incProgress(1/length(cumnum), detail = paste("File", i))
        }
      })
      resloop_dnnx<-resloop_dnn#[-1,]
      resloop_dnnx[is.na(resloop_dnnx)]<-0
      rownames(resloop_dnnx)<-samplesdf[[1]][rocindex2]
      rocdf_oralcmsms<-data.frame(labels=rep(0:1,as.numeric(table(samplesdf[[2]][rocindex2]))),prevalue=resloop_dnnx[,2]/apply(resloop_dnnx,1,sum))
      wholedf_oralcmsms<-cbind(rocdf_oralcmsms,resloop_dnnx)
    }else{
      if(input$modelresultype==1){
        confmatoutdf<-confmatoutms()
      }else{
        confmatoutdf<-confmatoutmsms()
      }
      resloop_dnnx<-confmatoutdf
      rocdf_oralcmsms<-data.frame(labels=rep(0:1,as.numeric(table(samplesdf[[2]]))),prevalue=resloop_dnnx[,2]/apply(resloop_dnnx,1,sum))
      wholedf_oralcmsms<-cbind(rocdf_oralcmsms,resloop_dnnx)
    }
    list(resloop_dnnx=resloop_dnnx,wholedf_oralcmsms=wholedf_oralcmsms,rocdf_oralcmsms=rocdf_oralcmsms)
  })
  roccurve_heightx<-reactive({
    roccurve_heightx<-input$roccurve_height
    roccurve_heightx
  })
  observeEvent(
    input$mcsbtn_roccurve,{
      shinyjs::show(id = "mcsbtn_roccurveid", anim = FALSE)
      output$roccurveplot<-renderPlot({
        resloop_dnnx<-roccurvepout()$resloop_dnnx
        rocdf_oralcmsms<-roccurvepout()$rocdf_oralcmsms
        pred_oralcmsms <- prediction(rocdf_oralcmsms$prevalue, rocdf_oralcmsms$labels)
        perf_oralcmsms <- performance(pred_oralcmsms,"tpr","fpr")
        plot(perf_oralcmsms,col=colpalettes[6],type="o",pch=16,lwd=3,cex=2)
        abline(a=0, b=1, col="black")
        auc<- performance( pred_oralcmsms,  c("auc"))
        aucvalue_oralcmsms<-unlist(slot(auc , "y.values"))
        text(0.2,0.7,labels = paste0("AUC: ",round(aucvalue_oralcmsms,3)))
      },height = roccurve_heightx)
      roccurveplotout<-reactive({
        resloop_dnnx<-roccurvepout()$resloop_dnnx
        rocdf_oralcmsms<-roccurvepout()$rocdf_oralcmsms
        pred_oralcmsms <- prediction(rocdf_oralcmsms$prevalue, rocdf_oralcmsms$labels)
        perf_oralcmsms <- performance(pred_oralcmsms,"tpr","fpr")
        plot(perf_oralcmsms,col=colpalettes[6],type="o",pch=16,lwd=3,cex=2)
        abline(a=0, b=1, col="black")
        auc<- performance( pred_oralcmsms,  c("auc"))
        aucvalue_oralcmsms<-unlist(slot(auc , "y.values"))
        text(0.2,0.7,labels = paste0("AUC: ",round(aucvalue_oralcmsms,3)))
      })
      output$roccurvepicdl<-downloadHandler(
        filename = function(){paste("ROC_figure",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file,width =roccurve_heightx()/100,height = roccurve_heightx()/100)
          print(roccurveplotout())
          dev.off()
        }
      )
    }
  )
  ##F1 Score
  f1scoredfout<-reactive({
    rocdf_oralcmsms<-roccurvepout()$rocdf_oralcmsms
    F1scoredf1<-vector()
    for(i in 1:nrow(rocdf_oralcmsms)){
      if(rocdf_oralcmsms$labels[i]==0){
        F1scoredf1[i]<-2*(1-rocdf_oralcmsms$prevalue[i])/(1+(1-rocdf_oralcmsms$prevalue[i]))
      }else{
        F1scoredf1[i]<-2*rocdf_oralcmsms$prevalue[i]/(1+rocdf_oralcmsms$prevalue[i])
      }
    }
    F1scoredf<-data.frame(Labels=rocdf_oralcmsms$labels,F1Score=F1scoredf1)
    rownames(F1scoredf)<-rownames(rocdf_oralcmsms)
    F1scoredf
  })
  observeEvent(
    input$mcsbtn_f1score,{
      shinyjs::show(id = "mcsbtn_f1scoreid", anim = FALSE)
      output$f1scoredf<-renderDataTable({
        f1scoredfoutx<-f1scoredfout()
        f1scoredfoutx<-round(f1scoredfoutx,4)
        datatable(f1scoredfoutx, options = list(pageLength = 20)) %>%
          formatStyle("F1Score",background = styleColorBar(range(f1scoredfoutx), 'lightblue'),
                      backgroundSize = '98% 88%',backgroundRepeat = 'no-repeat',
                      backgroundPosition = 'center')
      })
      output$f1scoredfdl<-downloadHandler(
        filename = function(){paste("F1Score_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(f1scoredfout(),file = file)
        }
      )

      #output$f1scoredfdl<-downloadHandler(
      #  filename = function(){paste("ROC_figure",usertimenum,".pdf",sep="")},
      #  content = function(file){
      #    pdf(file,width =f1score_heightx()/100,height = f1score_heightx()/100)
      #    print(f1scoreplotout())
      #    dev.off()
      #  }
      #)
    }
  )
})

shinyApp(ui = ui, server = server)
