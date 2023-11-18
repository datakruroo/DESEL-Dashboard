#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(readxl)
library(flexdashboard)
library(shiny)
library(ggplot2)
library(ggalluvial)
library(plotly)
library(igraph)
library(networkD3)
library(dbscan)
library(rcartocolor)
library(treemapify)
library(DT)
library(wesanderson)
library(tidyr)
library(dplyr)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("simplex"),
    titlePanel("DESEL Heath & Help Dashboard"),
    p("Copyright 2023 KruRoo Studio พัฒนาโดย คณาจารย์ภาควิชาวิจัยและจิตวิทยาการศึกษา คณะครุศาสตร์ จุฬาลงกรณ์มหาวิทยาลัย"),
    sidebarLayout(
        sidebarPanel(
            uiOutput("template"),
            fileInput("file1", "ขั้นที่ 2 : Upload ไฟล์ MS Excel ตาม template",
                      accept = c(".xlsx")
            ),
            p("ขั้นที่ 3 : กดปุ่ม Analyze เพื่อวิเคราะห์ข้อมูล"),
            actionButton("goButton", "Analyze"),
            br(),
            br(),
            h5("คำแนะนำสำหรับการกรอกข้อมูลในไฟล์ template"),
            HTML("
            <ul>
            <li>การกรอกชื่อนักเรียนให้เขียนคำนำหน้าติดกับชื่อ เช่น เด็กชายกะนิด หรือ นางสาวเป้ เป็นต้น</li>
            <li>ฐานะทางบ้านเลือกกรอกได้ 3 ระดับ ได้แก่</li>
                <ul>
                <li> ยากจน </li>
                <li> ปานกลาง </li>
                <li> ร่ำรวย </li>
                </ul>
            <li> สุขภาพทางกายเลือกกรอกได้ 4 ประเภท ได้แก่ </li>
                <ul>
                <li> แข็งแรง </li>
                <li> พัฒนาการด้านร่างกายไม่สมวัย </li>
                <li> มีโรคประจำตัวเรื้อรัง </li>
                <li> มีความพิการทางกาย </li>
                </ul> 
            <li> การเข้าเรียนเลือกกรอกได้ 3 ประเภท ได้แก่ </li>
                <ul>
                <li> เข้าเป็นประจำ </li>
                <li> ขาด 1-2 ครั้งต่อวิชา </li>
                <li> ขาดตั้งแต่ 3 ครั้งต่อวิชา </li>
                </ul>   
            <li> การส่งงานเลือกกรอกได้ 3 ประเภท ได้แก่ </li>
                <ul>
                <li> ส่งประจำ </li>
                <li> ขาดส่ง 1-2 ครั้งต่อวิชา </li>
                <li> ขาดส่งตั้งแต่ 3 ครั้งต่อวิชา </li>
                </ul> 
            <li> sdq1 - sdq25 ใช้คะแนนจากแบบประเมินพฤติกรรมเด็ก (SDQ) ฉบับที่ประเมินโดยครู
            ที่มีจำนวน 25 ข้อ (ใช้ข้อมูลดิบไม่ต้องกลับสเกลคะแนน) จำแนกเป็น </li>
                <ul>
                <li> พฤติกรรมอารมณ์ 5 ข้อ</li>
                <li> พฤติกรรมอยู่ไม่นิ่ง/สมาธิสั้น 5 ข้อ</li>
                <li> พฤติกรรมเกเร 5 ข้อ </li>
                <li> ความสัมพันธ์กับเพื่อน 5 ข้อ </li>
                <li> สัมพันธภาพทางสังคม 5 ข้อ </li>
                </ul>
                
            </ul>
                 "),
            uiOutput("sdq"),
        width = 3),
        mainPanel(
            conditionalPanel(
                condition = "input.goButton > 0",
            # Output: ตาราง, กราฟ, หรืออื่นๆ
            fluidRow(
            style = "border: 1px dashed #ddd; padding: 10px; margin-bottom: 10px;",
            uiOutput("gaugetext"),
            column(3, gaugeOutput("gaugeOutput1",height = "100%")),
            column(3, gaugeOutput("gaugeOutput2",height = "100%")),
            column(3, gaugeOutput("gaugeOutput3",height = "100%")),
            column(3, gaugeOutput("gaugeOutput4",height = "100%"))
            ),
            
            fluidRow(
            style = "border: 1px dashed #ddd; padding: 10px; margin-bottom: 10px;",
            column(6,uiOutput("treemap_text")),
            column(6, 
                   plotOutput("treemap", height = "300px"))),
            fluidRow(
                column(3,selectInput("groupSelect", "เลือกกลุ่ม:", choices = NULL))),
            tabsetPanel(type = "tabs",
            br(),
            tabPanel("Risk Insight (1)",
            style = "border: 1px dashed #ddd; padding: 10px; margin-bottom: 10px;",

            DTOutput("table")
            #column(4, plotOutput("barchart_gender", height = "200px", width = "100px")),
            #column(4, plotOutput("barchart_econ", height = "200px", width = "200px")),
            #column(4, plotOutput("barchart_attend", height = "200px", width = "200px"))
            
            ), # tabpanel1
            tabPanel("Risk Insight (2): SDQ analysis",
            style = "border: 1px dashed #ddd; padding: 10px; margin-bottom: 20px;",
            fluidRow(
            column(3,selectInput("studentSelect", "เลือกนักเรียน:", choices = NULL)),
            ),
            fluidRow(
            column(3, 
                  h5(" ", "คะแนนจุดแข็งด้านสัมพันธภาพทางสังคม"),
                   gaugeOutput("gauge_strong", height = "100%")),
            column(8,
            fluidRow(
            h5(" ", "    ", "คะแนนความเสี่ยงด้านอารมณ์และพฤติกรรม"),
            column(4, gaugeOutput("gauge_emotion", width = "100%",height = "100%")),
            column(4, gaugeOutput("gauge_bully", width = "100%",height = "100%"))),
            fluidRow(
            column(4, gaugeOutput("gauge_hyper", width = "100%",height = "100%")),
            column(4, gaugeOutput("gauge_friend", width = "100%",height = "100%"))
            )),

            ),
            DTOutput("sdq_student")
            )

            ),
            hr()
            
            
            
            )# end of conditionalPanel
            
            ) #main    
        
    ) # sidelbar
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    data <- reactive({
        inFile <- input$file1
        if (is.null(inFile))
            return()
        read_excel(inFile$datapath)
    })
    
    url<-a("ดาวน์โหลด template สำหรับกรอกข้อมูล", href = "data_template.xlsx")
    output$template <- renderUI({
        tagList("ขั้นที่ 1 :", url)
    })
    
    url_sdq<-a("แบบประเมิน SDQ ฉบับครูประเมินผู้เรียน", href ="https://obeccare.thaieduforall.org/pdfdownload/%E0%B9%81%E0%B8%9A%E0%B8%9A%E0%B8%9B%E0%B8%A3%E0%B8%B0%E0%B9%80%E0%B8%A1%E0%B8%B4%E0%B8%99Sdq%E0%B8%89%E0%B8%9A%E0%B8%B1%E0%B8%9A%E0%B8%84%E0%B8%A3%E0%B8%B9.pdf")
    output$sdq <- renderUI({
      tagList("เอกสารอ้างอิง : ", url_sdq)
    })
    
    observeEvent(input$goButton, {
        # โค้ดสำหรับการวิเคราะห์ข้อมูล
        # ตัวอย่าง: result <- data() %>% ...
        # output$myOutput <- renderTable(result) หรือ renderPlot สำหรับกราฟ
        
        ### จัดการข้อมูลก่อน
        dat <- data()
        names(dat)[1]<-"name"
        dat <- dat%>%
            separate(col = "name", into = c("name","surname"), sep =" ") %>%
            select(-surname)
        names(dat)[1:7]<-c("name","gender","econ","health","gpax","attend","homework")
        
        
        names(dat)[c(1,4,9,17,20)+7] <- paste0("social_relation",1:5)
        names(dat)[c(3,8,13,16,24)+7] <- paste0("emotion",1:5) ## diff1
        names(dat)[c(5,7,12,18,22)+7] <- paste0("bully",1:5) ## diff2
        names(dat)[c(2,10,15,21,25)+7] <- paste0("hyperact", 1:5) ## diff3
        names(dat)[c(6,11,14,19,23)+7] <- paste0("friend", 1:5) ## diff4
        
        
        
        ### กลับสเกลข้อคำถามเชิงบวก
        dat[,c(7,11,14,21,25)+7] <- 2-dat[,c(7,11,14,21,25)+4]
        
        ## normalized function
        normalize <- function(x, na.rm=T) x/10
        
        
        temp <- dat %>%
            mutate(emotion = rowSums(select(., starts_with("emotion"))),
                   bully = rowSums(select(., starts_with("bully"))),
                   hyperact = rowSums(select(., starts_with("hyperact"))),
                   friend = rowSums(select(., starts_with("friend"))),
                   social = rowSums(select(., starts_with("social"))),
                   difficulty = emotion + bully + hyperact + friend) %>%
            ## ความเสี่ยงด้านฐานะ
            mutate(econ_risk = ifelse(econ=="ยากจน",1,0))  %>%
            ## ความเสี่ยงด้านการเรียน
            mutate(gpax_type = ifelse(gpax<1.5,1,0),
                   attend_type = ifelse(attend == "ขาดตั้งแต่ 3 ครั้งต่อวิชา",1,0),
                   homework_type = ifelse(homework == "ขาดส่งตั้งแต่ 3 ครั้งต่อวิชา",1,0)) %>%
            mutate(study_risk = ifelse((gpax_type+attend_type+homework_type)/3>0,1,0) ) %>%
            ## ความเสี่ยงด้านสุขภาพทางกาย
            mutate(health_risk = ifelse(health == "แข็งแรง",0,1)) %>%
            ## ความเสี่ยงด้านอารมณ์สังคม
            mutate_at(vars(c("emotion","bully","hyperact","friend")),normalize) %>%
            mutate(emotion_type = ifelse(emotion>=0.4,1,0),
                   bully_type = ifelse(bully>=0.4,1,0),
                   hyper_type = ifelse(hyperact>=0.6,1,0),
                   friend_type = ifelse(friend>=0.6,1,0),
                   sdq_risk = ifelse((emotion_type + bully_type + hyper_type + friend_type) >0,1,0)
            )
        
        test<- temp %>% select(name, econ_risk, study_risk, health_risk, sdq_risk) %>%
            mutate(risk_student = ifelse((econ_risk+ study_risk+health_risk+sdq_risk)>0,1,0),
                   risk_level = (econ_risk+ study_risk+health_risk+sdq_risk)/4) %>%
            filter(risk_student >0)
        
       group<- test %>%
            group_by(risk_student,econ_risk, study_risk, health_risk, sdq_risk) %>%
            count() %>% 
            ungroup() %>%
            arrange(-n) %>%
            mutate(group = LETTERS[1:nrow(.)])
       
 
       
       temp2<- temp %>%
        left_join(x=., y=group, by=c("econ_risk", "study_risk", "health_risk", "sdq_risk")) %>%
           filter(risk_student >0) %>%
          select(group, name, gender, econ, gpax, attend, homework, health, emotion, bully, hyperact, friend,
                 paste0("emotion",1:5), paste0("bully",1:5), paste0("hyperact",1:5), paste0("friend",1:5),
                 paste0("social_relation",1:5), ends_with("type")) %>%
           mutate(social_relation = rowSums(select(., starts_with("social_relation"))))

       item_name<-  c("ปวดศีรษะ ปวดท้อง หรือไม่สบายบ่อย",
                      "ขี้กังวล วิตกกังวล",
                      "ไม่มีความสุข ท้อแท้ ร้องไห้บ่อย",
                      "เครียด กังวลเวลาอยู่ในสถานการณ์ที่ไม่คุ้นเคย ขาดความมั่นใจในตนเอง",
                      "ขี้กลัว รู้สึกหวาดกลัวได้ง่าย",
                      
                      "มักอาละวาดหรือโมโหร้าย",
                      "ดื้อ ไม่เชื่อฟังหรือทำตามที่ผู้ใหญ่ต้องการ",
                      "มักมีเรื่องทะเลาะวิวาทกับเด็กอื่นหรือรังแกเด็กอื่น",
                      "ชอบโกหกหรือขี้โกง",
                      "ขโมยของที่บ้าน โรงเรียนหรือที่อื่น",
                      
                      "อยู่ไม่นิ่ง นั่งนิ่ง ๆ ไม่ได้",
                      "อยู่ไม่สุข วุ่นวายอย่างมาก",
                      "วอกแวกง่าย สมาธิสั้น",
                      "ขาดการคิดก่อนทำ",
                      "ไม่สามารถทำงานให้เสร็จ ขาดความตั้งใจในการทำงาน",
                      
                      "ค่อนข้างแยกตัว ชอบเล่นคนเดียว",
                      "ไม่มีเพื่อนสนิท",
                      "ไม่เป็นที่ชื่นชอบของเพื่อน",
                      "ถูกเด็กคนอื่นล้อเลียนหรือรังแก",
                      "เข้ากับผู้ใหญ่ได้ดีกว่าเด็กวัยเดียวกัน",
                      
                      "ห่วงใยความรู้สึกคนอื่น",
                      "เต็มใจแบ่งปันสิ่งของให้เพื่อน",
                      "เป็นที่พึ่งได้เวลาที่คนอื่นเสียใจ อารมณ์ไม่ดี หรือไม่สบายใจ",
                      "ใจดีกับเด็กที่เล็กกว่า",
                      "ชอบอาสาช่วยเหลือคนอื่น")
       
       sdq_name<-c(rep("ด้านอารมณ์",5),
                   rep("ด้านความประพฤติ/เกเร",5),
                   rep("ด้านพฤติกรรมอยู่ไม่นิ่ง/สมาธิสั้น",5),
                   rep("ด้านความสัมพันธ์กับเพื่อน",5),
                   rep("ด้านสัมพันธภาพทางสังคม",5))
       
       
       
       
        ### สร้าง gauge
        output$gaugetext <- renderUI({
            req(input$goButton > 0)  # ใช้ req() ที่นี่เช่นกัน
            h4("จำนวนนักเรียนที่เป็นกลุ่มเสี่ยงในชั้นเรียนจำแนกตามด้าน")
        })
        #### ด้านฐานะทางบ้าน
        output$gaugeOutput1 <- renderGauge({
        econ <- temp %>% 
            summarise(n=n(),
                      econ_risk = mean(econ_risk)*n) %>% t()
        
        
        gauge(econ[2,1]%>%as.numeric(), min = 0, max = econ[1,1]%>%as.numeric(), symbol = 'คน',
              label = "ฐานะทางบ้าน",
              sectors = gaugeSectors(
                  success = c(0,0),
                  warning = c(0,econ[1,1]%>%as.numeric()),
                  danger = c(100,100),
                  colors = c("success", "warning", "danger")
              ))
        })
        #### ด้านการเรียน
        output$gaugeOutput2 <- renderGauge({
        study <- temp %>%
            summarise(n=n(),
                      study_risk = mean(study_risk)*n) %>% t()
        
        gauge(study[2,1]%>%as.numeric(), min = 0, max = study[1,1]%>%as.numeric(), symbol = 'คน',
              label = "ด้านการเรียน",
              sectors = gaugeSectors(
                  success = c(0,0),
                  warning = c(0,study[1,1]%>%as.numeric()),
                  danger = c(100,100),
                  colors = c("success", "warning", "danger")
              ))
        })
        #### ด้านการสุขภาพกาย
        output$gaugeOutput3 <- renderGauge({
            health <- temp %>%
                summarise(n=n(),
                          health_risk = mean(health_risk)*n) %>% t()
            
            gauge(health[2,1]%>%as.numeric(), min = 0, max = health[1,1]%>%as.numeric(), symbol = 'คน',
                  label = "ด้านสุขภาพทางกาย",
                  sectors = gaugeSectors(
                      success = c(0,0),
                      warning = c(0,health[1,1]%>%as.numeric()),
                      danger = c(100,100),
                      colors = c("success", "warning", "danger")
                  ))
        })        
        #### ด้านอารมณ์/สังคม
        output$gaugeOutput4 <- renderGauge({
        sqd <- temp %>%
            summarise(n=n(),
                      sdq_risk = mean(sdq_risk)*n) %>% t()
        
        gauge(sqd[2,1]%>%as.numeric(), min = 0, max = sqd[1,1]%>%as.numeric(), symbol = 'คน',
              label = "ด้านอารมณ์/สังคม",
              sectors = gaugeSectors(
                  success = c(0,0),
                  warning = c(0,sqd[1,1]%>%as.numeric()),
                  danger = c(100,100),
                  colors = c("success", "warning", "danger")
              ))
        })   
        
        ### Treemap 
        output$treemap_text <- renderUI({
            req(input$goButton > 0)  # ใช้ req() ที่นี่เช่นกัน
            HTML("
            <h4>ผลการจัดกลุ่มนักเรียนที่เป็นกลุ่มเสี่ยงในชั้นเรียน</h4>
            <ul>
            <li>แผนภาพด้านขวาเรียกว่า Treemap</li>
            <li>ตัวอักษรภาษาอังกฤษ A, B, C, ... แทนชื่อกลุ่มลักษณะความเสี่ยงของนักเรียน</li>
            <li>ขนาดกล่องสี่เหลี่ยมแทนจำนวนนักเรียนในแต่ละกลุ่ม</li>
            <li>ภายในแต่ละกล่องสี่เหลี่ยมแสดง combination ของความเสี่ยงของนักเรียนภายในกลุ่ม</li>
            </ul>
            ")
            })
        
        output$treemap <- renderPlot({
 
           
            
            p<-group %>%
                pivot_longer(cols=2:5, names_to = "type", values_to = "risk") %>%
                mutate(type = factor(type, 
                                     levels=c("econ_risk","study_risk","health_risk","sdq_risk"),
                                     labels=c("มีฐานะยากจน","การเรียน","สุขภาพทางกาย","อารมณ์/สังคม"))) %>%
                filter(risk >0) %>%
                ggplot(aes(area = n, fill = type, subgroup = group, label = type))+
                geom_treemap(alpha = 0.9)+
                geom_treemap_subgroup_border(col = "white")+
                geom_treemap_text(family = "ChulaCharasNew", padding.y = grid::unit(4, "mm"))+
                geom_treemap_subgroup_text(place = "centre", grow = T,
                                           alpha = 0.4, colour ="white", min.size = 0)+
                scale_fill_carto_d(palette = 4)+
                theme(legend.position = "none")
            p
        })
        
        #### visualize ความเสี่ยงแบบละเอียด
        observe({
            updateSelectInput(session, "groupSelect", choices = c("All", unique(as.character(group$group))))
        })

      
        
        filtered_data <- reactive({
            if (input$groupSelect == "All" || is.null(input$groupSelect)) {
                temp2 %>% arrange(group) %>%
                    mutate_at(vars("gpax"), round,2)
            } else {
                temp2%>%filter(group %in% input$groupSelect)%>%
                    mutate_at(vars("gpax"), round,2)
            }
        })
        

        
        
        output$table <- renderDT({
            tab<-filtered_data() %>% 
                select(group,name, econ, gpax, attend, homework, health)
            #tab$econ <- as.numeric(factor(tab$econ,levels=c("ยากจน","ปานกลาง","ร่ำรวย")))
            names(tab)[1]<-"กลุ่มความเสี่ยง"
            names(tab)[2]<-"ชื่อนักเรียน"
            names(tab)[3]<-"ฐานะทางบ้าน"
            names(tab)[4]<-"เกรดเฉลี่ยสะสม"
            names(tab)[5]<-"การเข้าชั้นเรียน"
            names(tab)[6]<-"การส่งการบ้าน"
            names(tab)[7]<-"สุขภาพทางกาย"
            datatable(tab, extensions = c('ColReorder','Responsive'), 
                      options = list(colReorder = list(realtime = FALSE),
                                     columnDefs = list(
                                         list(className = 'dt-center', targets = '_all')
                                     ))) %>%
                ### จัด format econ
                formatStyle(
                    columns = 3,
                    backgroundColor = styleEqual(
                        levels = c("ยากจน","ปานกลาง","ร่ำรวย"), 
                        values = c('#CD5C08', '#C1D8C3', '#6A9C89')
                    ),
                    color = styleEqual(
                        levels = c("ยากจน","ปานกลาง","ร่ำรวย"), 
                        values = c("white","black","black")
                        )
                    
                    
                ) %>%
                ### จัด format GPAX
                formatStyle(
                    column = 4,
                    backgroundColor = styleInterval(
                        cuts = c(1.5,3), 
                        values = c('#CD5C08', '#C1D8C3', '#6A9C89')
                    )
                    
                ) %>%
                ### จัด format การเข้าชั้นเรียน
                formatStyle(
                    column = 5,
                    backgroundColor = styleEqual(
                        levels=c("ขาดตั้งแต่ 3 ครั้งต่อวิชา","ขาด 1 - 2 ครั้งต่อวิชา","เข้าเป็นประจำ"), 
                        values = c('#CD5C08', '#C1D8C3', '#6A9C89')
                    )
                    
                ) %>%
            ### จัด format การส่งการบ้าน
            formatStyle(
                column = 6,
                backgroundColor = styleEqual(
                    levels=c("ขาดส่งตั้งแต่ 3 ครั้งต่อวิชา","ขาดส่ง 1-2 ครั้งต่อวิชา","ส่งประจำ"),
                    values = c('#CD5C08', '#C1D8C3', '#6A9C89')
                )
                
            ) %>%
            ### จัด format การสุขภาพทางกาย
                
            formatStyle(
                    column = 7,
                    backgroundColor = styleEqual(
                        levels=c("มีความพิการทางกาย","มีโรคประจำตัวเรื้อรัง","พัฒนาการด้านร่างกายไม่สมวัย","แข็งแรง"),
                        values = c('#CD5C08','#F1B4BB', '#EAD7BB', '#6A9C89')
                    )
                    
                )    
            
        }) ### end of DT table 
        
        ##### ทำ SDQ
    
        observe({
            updateSelectInput(session, "studentSelect", choices = c(unique(as.character(filtered_data()$name))))
        })  
        
        output$gauge_strong <- renderGauge({
            social <- filtered_data() %>%
                filter(name %in% input$studentSelect) %>%
                select(social_relation) %>%
                mutate(social_relation = social_relation) %>% as.numeric()
            
            gauge(social, min = 0, max = 10, symbol = 'คะแนน',
                  label = "สัมพันธภาพทางสังคม",
                  sectors = gaugeSectors(
                      success = c(5,10),
                      warning = c(3,3),
                      danger = c(0,2),
                      colors = c("success", "warning", "danger")
                  ))
        })        
        
        
        output$gauge_emotion <- renderGauge({
            emotion <- filtered_data() %>%
                filter(name %in% input$studentSelect) %>%
                select(emotion) %>%
                mutate(emotion = emotion*10) %>% as.numeric()
            
            gauge(emotion, min = 0, max = 10, symbol = 'คะแนน',
                  label = "อารมณ์",
                  sectors = gaugeSectors(
                      success = c(0,3),
                      warning = c(4,4),
                      danger = c(5,10),
                      colors = c("success", "warning", "danger")
                  ))
        })        

        output$gauge_bully <- renderGauge({
            bully <- filtered_data() %>%
                filter(name %in% input$studentSelect) %>%
                select(bully) %>%
                mutate(bully = bully*10) %>% as.numeric()
            
            gauge(bully, min = 0, max = 10, symbol = 'คะแนน',
                  label = "พฤติกรรม/เกเร",
                  sectors = gaugeSectors(
                      success = c(0,3),
                      warning = c(4,4),
                      danger = c(5,10),
                      colors = c("success", "warning", "danger")
                  ))
        })    
        
        output$gauge_hyper <- renderGauge({
            hyperact <- filtered_data() %>%
                filter(name %in% input$studentSelect) %>%
                select(hyperact) %>%
                mutate(hyperact = hyperact*10) %>% as.numeric()
            
            gauge(hyperact, min = 0, max = 10, symbol = 'คะแนน',
                  label = "พฤติกรรมอยู่ไม่นิ่ง/สมาธิสั้น",
                  sectors = gaugeSectors(
                      success = c(0,5),
                      warning = c(6,6),
                      danger = c(7,10),
                      colors = c("success", "warning", "danger")
                  ))
        }) 
        
        output$gauge_friend <- renderGauge({
            friend <- filtered_data() %>%
                filter(name %in% input$studentSelect) %>%
                select(friend) %>%
                mutate(friend = friend*10) %>% as.numeric()
            
            gauge(friend, min = 0, max = 10, symbol = 'คะแนน',
                  label = "ความสัมพันธ์กับเพื่อน",
                  sectors = gaugeSectors(
                      success = c(0,5),
                      warning = c(6,6),
                      danger = c(7,10),
                      colors = c("success", "warning", "danger")
                  ))
        }) 
        
        output$sdq_student <- renderDT({
            sdq_dat <- filtered_data()%>%filter(name %in% input$studentSelect)
            sdq_dat %>%
                pivot_longer(cols=c(paste0("emotion",1:5),
                                    paste0("bully",1:5),
                                    paste0("hyperact",1:5),
                                    paste0("friend", 1:5),
                                    paste0("social_relation",1:5)
                ),
                names_to = "vars",
                values_to = "val")  %>%
                mutate(vars_name = item_name) %>%
                mutate(sdq = sdq_name) %>%
                filter(val>1) %>%
                select(sdq, vars_name)->tab2
            names(tab2)[2]<-"พฤติกรรมผู้เรียนที่เป็นลักษณะเด่น"
            datatable(
                tab2[order(tab2$sdq), ],
                extensions = 'RowGroup',
                options = list(rowGroup = list(dataSrc = 1),
                               columnDefs = list(
                                   list(visible = FALSE, targets = 1)  # ซ่อน Column2 (ซึ่งอยู่ในตำแหน่งที่ 1)
                               ),
                             searching = F,
                             paging = F),
                selection = 'none'
                )
            
        })
        
      
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
