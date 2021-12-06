
library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(shinythemes)
library(reshape2)
library(plotly)
library(Hmisc)
library(psych)


players <- read_csv("FullData.csv")

# tao data cho hien thi danh sach cac cau thu
data_table <- data.frame(
  Name = players$Name,
  Nationality = players$Nationality,
  `National Position` = players$National_Position,
  `National Kit` = players$National_Kit,
  Club = players$Club,
  `Club Position` = players$Club_Position,
  `Club Kit` = players$Club_Kit,
  Rating = players$Rating,
  Height = players$Height,
  Weight = players$Weight,
  `Preffered Foot` = players$Preffered_Foot,
  Age = players$Age,
  `Birth Date` = players$Birth_Date,
  `Contract Expiry` = players$Contract_Expiry
)


ui <- dashboardPage(
  dashboardHeader(title = "Filter Examples"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Trang chu", tabName = "all"),
      menuItem("So sanh 2 cau thu", tabName = "Common"),
      menuItem("So sanh giua cac clb", tabName = "so_sanh_clb"),
      menuItem("Bieu do", tabName = "Bieudo"),
      menuItem("Hoi quy tuyen tinh", tabName = "Hoi_quy_tuyen_tinh")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "all",

              mainPanel(
                tabsetPanel(
                  id = 'dataset',
                  tabPanel("data_table", DT::dataTableOutput("mytable2")),
                )
              )
      ),

      tabItem(tabName = "Common",
              fluidRow(
                column(2),

                column( 4,
                        align = 'center',
                        selectInput(inputId = "choose_player_1",
                                    label = "",
                                    choices = unique(players$Name),
                                    selected = 'Cristiano Ronaldo')),

                column( 4,
                        align = 'center',
                        selectInput(inputId = "choose_player_2",
                                    label = "",
                                    choices = unique(players$Name),
                                    selected = 'Lionel Messi')),

                column(2)
              ),

              fluidRow(

                box(
                  column(3,
                         align = 'center',
                         style = "margin-top:8em;",
                         tableOutput("profile1")),
                  column(6,
                         style = "font-size: 10px;",
                         plotlyOutput("bieu_do_the_hien_chi_so_cua_1_cau_thu")),
                  column(3,
                         align = 'center',
                         style = "margin-top:8em;",
                         tableOutput("profile2")),
                  column(3),
                  width = 12
                )
              )



      ),
      tabItem(tabName = "so_sanh_clb",

              fluidRow(
                box(plotOutput("so_sanh_chan_thuan_khong_thuan_clb"), width = 12)
              ),
              fluidRow(
                box(plotOutput("chi_so_trung_binh_clb"), width = 12)
              )
      ),
      tabItem(tabName = "Bieudo",

              fluidRow(
                box(selectInput("v_select_age_1", label = "Age", choices = unique(players$Age), selected = "28"), width = 4),
                box(selectInput("v_select_age_2", label = "Age", choices = unique(players$Age), selected = "32"), width = 4),
                box(selectInput("v_select_country", label = "Country", choices = unique(players$Nationality), selected = "Brazil"), width = 4)
              ),
              fluidRow(
                box(plotOutput("ti_so_cau_thu_theo_tuoi_1"), width = 6),
                box(plotOutput("ti_so_cau_thu_theo_tuoi_2"), width = 6)
              ),
              fluidRow(
                box(plotOutput("ratingPlayers"), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("clbPlayers"), width = 6),
                box(plotOutput("bieu_do_moi_tuong_quan_giua_pen_freekick"), width = 6)
              ),
              fluidRow(
                box(plotOutput("bieu_do_phan_bo_chi_so"), width = 12)
              ),
              fluidRow(
                box(plotOutput("moi_tuong_quan_chuyen_xa_va_gan"), width = 6),
                box(plotOutput("noi_tuong_quan_giua_pahn_ung_hieu_chien"), width = 6)
              )
      ),
      tabItem(tabName = "Hoi_quy_tuyen_tinh",

              fluidRow(
                box(plotOutput("Hoi_quy_tuyen_tinh_tien_dao"), width = 12)
              )
      )
    )
  )
)

server <- function(input, output) {

  output$select_example <- renderPlot({
    iris %>%
      filter(Species == input$v_select) %>%
      ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
      geom_point() +
      ggtitle("Select Filter")
  })

  output$slider_example <- renderPlot({
    iris %>%
      filter(Sepal.Length < input$v_slider) %>%
      ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
      geom_point() +
      ggtitle("Slider Example")
  })

  output$date_example <- renderPlot({
    economics %>%
      filter(date >= input$v_date) %>%
      ggplot(aes(x = date, y = unemploy)) +
      geom_line() +
      ggtitle("Date Example")
  })

  output$daterange_example <- renderPlot({
    economics %>%
      filter(date >= input$v_daterange[1] & date <= input$v_daterange[2]) %>%
      ggplot(aes(x = date, y = unemploy)) +
      geom_line() +
      ggtitle("Date Range Example")
  })

  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(data_table, options = list(orderClasses = TRUE))
  })


  # bieu do the hien ti so cau thu voi tuoi
  output$ti_so_cau_thu_theo_tuoi_1 <- renderPlot({

    data = filter(players, players$Age >= strtoi(input$v_select_age_1)
                  & players$Age <= strtoi(input$v_select_age_2)
                  & players$Nationality == input$v_select_country)

    avg_rating = aggregate(data$Rating, list(data$Age), FUN=mean)
    data_avg_rating <- data.frame(
      x = c(avg_rating$Group.1),
      y = c(avg_rating$x)
    )

    age <- ggplot(data_avg_rating, aes(x, y, group=1)) +
      geom_line(color="blue")+
      geom_point()

    # add title ...
    print(age + labs(title= "Bieu do the hien ti le tuoi voi trung binh chi so cua cau thu",
                     y="Chi so cau thu", x = "Tuoi (Nam)"))
  })

  # bieu do the hien ti so cau thu voi tuoi
  output$ti_so_cau_thu_theo_tuoi_2 <- renderPlot({

    data = filter(players, players$Age >= strtoi(input$v_select_age_1)
                  & players$Age <= strtoi(input$v_select_age_2)
                  & players$Nationality == input$v_select_country )
    avg_rating = aggregate(data$Age, list(data$Rating), FUN=mean)
    data_avg_rating <- data.frame(
      x = c(avg_rating$Group.1),
      y = c(avg_rating$x)
    )

    rating <- ggplot(data_avg_rating, aes(x, y, group=1)) +
      geom_line(color="red")+
      geom_point()

    # add title ...
    print(rating + labs(title= "Bieu do the hien ti le rating voi trung binh so tuoi",
                        y="Tuoi (Nam)", x = "Chi so cau thu"))
  })


  # bieu do the hien chi so cua cac cau thu
  output$ratingPlayers <- renderPlot({

    data_filter = filter(players, players$Age >= strtoi(input$v_select_age_1)
                         & players$Age <= strtoi(input$v_select_age_2)
                         & players$Nationality == input$v_select_country )

    library(plyr)

    data_filter = head(arrange(data_filter,desc(data_filter$Rating)), n = 20)

    data <- data.frame(
      x = c(data_filter$Name),
      y = c(data_filter$Rating),
      age = c(data_filter$Age)
    )

    # Plotting Multiple Charts
    library(ggplot2)

    ggplot(data, aes(x,y=age, , group=1))  +
      geom_bar(aes(x, y),stat="identity", fill="cyan",colour="#006000")+
      geom_line(color="red")+
      geom_point() +
      labs(title= "Bieu do the hien chi so va tuoi cua tung cau thu",
           x="Ten cua cau thu",y="Chi so va Tuoi cua cau thu")

  })

  #bieu do the hien cac chi so cua cac cau thu
  output$moi_tuong_quan_chuyen_xa_va_gan <- renderPlot({

    data_filter = filter(players, players$Age >= strtoi(input$v_select_age_1)
                         & players$Age <= strtoi(input$v_select_age_2)
                         & players$Nationality == input$v_select_country )

    tien_ve = c("CAM", "LM", "RM", "CM", "CDM", "RDM", "LDM")
    data_filter = filter(data_filter, Club_Position %in% tien_ve)

    data_tam <- data.frame(
      Short_Pass = c(data_filter$Short_Pass),
      Long_Pass = c(data_filter$Long_Pass)
    )


    plot(data_tam$Short_Pass ~ data_tam$Long_Pass, pch=16, main="Bieu do the hien moi lien he giua kha nang chuyen ngan va chuyen dai",
         xlab="Chi so chuyen ngan", ylab="Chi so chuyen dai", bty="l")

    reg <- lm(data_tam$Short_Pass ~ data_tam$Long_Pass)
    data_tam

    abline(reg)

  })

  #bieu do the hien cac chi so phan ung va hieu chien
  output$noi_tuong_quan_giua_pahn_ung_hieu_chien <- renderPlot({

    data_filter = filter(players, players$Age >= strtoi(input$v_select_age_1)
                         & players$Age <= strtoi(input$v_select_age_2)
                         & players$Nationality == input$v_select_country )

    data_tam <- data.frame(
      Aggression = c(data_filter$Aggression),
      Reactions = c(data_filter$Reactions)
    )


    plot(data_tam$Aggression ~ data_tam$Reactions, pch=16, main="Bieu do the hien moi lien he giua muc do hieu chien va phan ung",
         xlab="Chi so hieu chien", ylab="Chi so phan ung", bty="l")

    reg <- lm(data_tam$Aggression ~ data_tam$Reactions)
    data_tam

    abline(reg)

  })

  # bieu do tron, the hien ti le clb
  output$clbPlayers <- renderPlotly({

    data_filter = filter(players, players$Age >= strtoi(input$v_select_age_1)
                         & players$Age <= strtoi(input$v_select_age_2)
                         & players$Nationality == input$v_select_country )

    # dem so cau thu theo tung vi tri
    count_numbers = count(data_filter, "Club_Position")
    sum_players = sum(count_numbers$freq)


    # nhom cac vi tri cau thu lai
    tien_dao = c("ST", "CF", "LF", "RF", "RW", "LW")
    data_tiendao = filter(count_numbers, Club_Position %in% tien_dao)
    data_tiendao = round((sum(data_tiendao$freq) / sum_players) * 100)


    tien_ve = c("CAM", "LM", "RM", "CM", "CDM", "RDM", "LDM")
    data_tienve = filter(count_numbers, Club_Position %in% tien_ve)
    data_tienve = round((sum(data_tienve$freq) / sum_players) * 100)

    hau_ve = c("CB", "SW", "LB", "RB", "RWB")
    data_hauve = filter(count_numbers, Club_Position %in% hau_ve)
    data_hauve = round((sum(data_hauve$freq) / sum_players) * 100)

    data_danang = filter(count_numbers, count_numbers$Club_Position == "Sub")
    data_danang = round((sum(data_danang$freq) / sum_players) * 100)

    data_thumon = filter(count_numbers, count_numbers$Club_Position == "GK")
    data_thumon = round((sum(data_thumon$freq) / sum_players) * 100)

    data = data.frame(
      group = c("Tien dao", "Tien ve", "Hau ve", "Thu mon", "Da nang"),
      value = c(data_tiendao, data_tienve, data_hauve, data_thumon, data_danang)
    )


    fig <- plot_ly(data, labels = ~group, values = ~value, type = 'pie')
    fig <- fig %>% layout(title = 'United States Personal Expenditures by Categories sssss 1960',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

    fig

  })

  # bieu do phan tan chi so cau thu theo tung do tuoi
  output$bieu_do_phan_bo_chi_so <- renderPlot({


    # loc data theo select input
    data_filter = filter(players, players$Age >= strtoi(input$v_select_age_1)
                         & players$Age <= strtoi(input$v_select_age_2)
                         & players$Nationality == input$v_select_country )

    data = data.frame(
      rating = data_filter$Rating,
      age = data_filter$Age
    )

    data$age <- as.factor(data$age)

    # Change box plot colors by groups
    p<-ggplot(data, aes(x=age, y=rating, fill=age)) +
      geom_boxplot()
    p

  })

  # bieu do phan tan chi so tung cau lac bo, lay 10 cau lac bo co chi so trung binh cao nhat
  output$bieu_do_moi_tuong_quan_giua_pen_freekick <- renderPlot({

    # loc data theo seclect
    data_filter = filter(players, players$Age >= strtoi(input$v_select_age_1)
                         & players$Age <= strtoi(input$v_select_age_2)
                         & players$Nationality == input$v_select_country )

    # lay 20 cau thu co chi so rating cao nhat
    data_filter = head(arrange(data_filter,desc(data_filter$Rating)), n = 20)

    # tao data
    data = data.frame(
      Freekick_Accuracy = data_filter$Freekick_Accuracy,
      Penalties = data_filter$Penalties,
      Name = data_filter$Name
    )

    basic <- ggplot(data, aes(x = Freekick_Accuracy, y = Penalties, colour = factor(Name), shape = factor(Name), size = 1.5,)) +
      geom_point() + scale_shape_manual(values=seq(0,19))
    basic + theme(legend.text = element_text(size = 10, colour = "black"))
    basic + labs(title= "Bieu do the hien moi tuong quan giua chi so da phat va da pen",
                 x="Chi so da phat cua tung cau thu",y="Chi so da pen cua tung cau thu")

  })


  # bieu do the hien 10 cau thu giong nhau nhat
  output$bieu_do_the_hien_chi_so_cua_1_cau_thu <- renderPlotly({

    # filter data cau thu duoc chon thub 1
    data_filter = filter(players, players$Name == input$choose_player_1)

    # filter data cau thu thu 2
    data_filter_1 = filter(players, players$Name == input$choose_player_2)

    plot_ly(
      type = 'scatterpolar',
      mode = "closest",
      fill = 'toself'
    ) %>%
      add_trace(
        r = c(data_filter$Rating, data_filter$Weak_foot, data_filter$Skill_Moves, data_filter$Ball_Control, data_filter$Dribbling, data_filter$Marking,
              data_filter$Sliding_Tackle,data_filter$Standing_Tackle, data_filter$Aggression, data_filter$Reactions,
              data_filter$Attacking_Position, data_filter$Interceptions,data_filter$Vision,data_filter$Composure,data_filter$Crossing,data_filter$Short_Pass,
              data_filter$Long_Pass,data_filter$Acceleration,data_filter$Speed,data_filter$Stamina, data_filter$Strength,data_filter$Balance,
              data_filter$Agility,data_filter$Jumping,data_filter$Heading,data_filter$Shot_Power,data_filter$Finishing,data_filter$Long_Shots,
              data_filter$Curve,data_filter$Freekick_Accuracy,data_filter$Penalties,data_filter$Volleys),
        theta = c("Rating","Weak_foot","Skill_Moves","Ball_Control","Dribbling","Marking","Sliding_Tackle","Standing_Tackle","Aggression",
                  "Reactions","Attacking_Position","Interceptions","Vision","Composure","Crossing","Short_Pass","Long_Pass","Acceleration",
                  "Speed","Stamina","Strength","Balance","Agility", "Jumping", "Heading", "Shot_Power", "Finishing", "Long_Shots", "Curve",
                  "Freekick_Accuracy", "Penalties", "Volleys"),
        showlegend = TRUE,
        mode = "markers",
        name = data_filter$Name
      ) %>%
      add_trace(
        r = c(data_filter_1$Rating, data_filter_1$Weak_foot, data_filter_1$Skill_Moves, data_filter_1$Ball_Control, data_filter_1$Dribbling, data_filter$Marking,
              data_filter_1$Sliding_Tackle,data_filter_1$Standing_Tackle, data_filter_1$Aggression, data_filter_1$Reactions,
              data_filter_1$Attacking_Position, data_filter_1$Interceptions,data_filter_1$Vision,data_filter_1$Composure,data_filter_1$Crossing,data_filter_1$Short_Pass,
              data_filter_1$Long_Pass,data_filter_1$Acceleration,data_filter_1$Speed,data_filter_1$Stamina, data_filter_1$Strength,data_filter_1$Balance,
              data_filter_1$Agility,data_filter_1$Jumping,data_filter_1$Heading,data_filter_1$Shot_Power,data_filter_1$Finishing,data_filter_1$Long_Shots,
              data_filter_1$Curve,data_filter_1$Freekick_Accuracy,data_filter_1$Penalties,data_filter_1$Volleys),
        theta = c("Rating","Weak_foot","Skill_Moves","Ball_Control","Dribbling","Marking","Sliding_Tackle","Standing_Tackle","Aggression",
                  "Reactions","Attacking_Position","Interceptions","Vision","Composure","Crossing","Short_Pass","Long_Pass","Acceleration",
                  "Speed","Stamina","Strength","Balance","Agility", "Jumping", "Heading", "Shot_Power", "Finishing", "Long_Shots", "Curve",
                  "Freekick_Accuracy", "Penalties", "Volleys"),
        showlegend = TRUE,
        mode = "markers",
        name = data_filter_1$Name
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100)
          )
        ),

        showlegend=TRUE

      )

  })



  output$profile1 <- renderTable({

    p1_sum <- players %>%
      filter(Name == input$choose_player_1) %>%
      select(Name,
             Age,
             Nationality,
             Rating,
             Club,
             Club_Kit,
             Contract_Expiry)
    return(t(p1_sum))

  }, colnames = FALSE, rownames = TRUE, align = 'c')

  output$profile2 <- renderTable({

    p2_sum <- players %>%
      filter(Name == input$choose_player_2) %>%
      select(Name,
             Age,
             Nationality,
             Rating,
             Club,
             Club_Kit,
             Contract_Expiry)
    return( t(p2_sum) )

  }, colnames = FALSE, rownames = TRUE, align = 'c')


  # mo hinh hoi quy tuyen tinh cua tien dao
  output$Hoi_quy_tuyen_tinh_tien_dao <- renderPlot({

    tien_dao = c("ST", "CF", "LF", "RF", "RW", "LW")
    datas = filter(players, Club_Position %in% tien_dao)


    lm_datamovie <- select(datas, c(Skill_Moves,
                                    Ball_Control,
                                    Standing_Tackle,
                                    Attacking_Position,
                                    Crossing,
                                    Acceleration,
                                    Finishing,
                                    Freekick_Accuracy,
                                    Volleys,
                                    Rating))

    lm_soccer <- lm(Rating ~ ., data=lm_datamovie)
    summary(lm_soccer)

    lm_s <- step(lm_soccer, direction = "backward", trace = FALSE)
    summary(lm_s)
    anova(lm_s)
    ###s

    pairs.panels(lm_datamovie, col='red')

    mov_fh <- data.frame(Skill_Moves = 5,
                         Ball_Control = 93,
                         Standing_Tackle= 31,
                         Attacking_Position = 94,
                         Crossing = 84,
                         Acceleration = 91,
                         Finishing = 93,
                         Freekick_Accuracy = 76,
                         Volleys = 88)

    prediction <- predict(lm_s, newdata=mov_fh, interval="confidence")
    prediction

  })

  output$so_sanh_chan_thuan_khong_thuan_clb <- renderPlot({


    # loc data theo seclect
    avg_rating = aggregate(players$Rating, list(players$Club), FUN=mean)

    # lay 10 clb
    data_filter = avg_rating[order(-avg_rating$x),][1:20,]

    View(data_filter)

    data_frame =  c(data_filter$Group.1)

    data_filter = filter(players, Club %in% data_frame)

    data = data.frame(
      Club = c(data_filter$Club),
      Preffered_Foot = c(data_filter$Preffered_Foot)
    )

    # bar plot, fill to discrete column, color to static value
    ggplot(data, mapping = aes(x = Club))+
      geom_bar(mapping = aes(fill = Preffered_Foot), color = "yellow")+
      labs(title = "Fill mapped to discrete column, static color")


  })

  output$chi_so_trung_binh_clb <- renderPlot({

    # loc data theo seclect
    avg_rating = aggregate(players$Rating, list(players$Club), FUN=mean)

    # lay 10 clb
    data_filter = avg_rating[order(-avg_rating$x),][1:20,]

    data = data.frame(
      Club = data_filter$Group.1,
      Rating = data_filter$x
    )


  ggplot(data, aes(x = Club, y = Rating)) +
    geom_col()


  })

}

shinyApp(ui, server)
