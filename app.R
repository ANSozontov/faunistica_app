# initial -----------------------------------------------------------------
host <- "194.35.119.132"
Sys.setenv(TZ = "GMT-5")
library(tidyverse)
library(DBI)
library(shiny)
library(shinyalert)
library(tippy)
library(RPostgreSQL)

# source("app_additions.R")

# host <- dplyr::case_when(
#     intToUtf8(as.integer(curl::curl_fetch_memory("ident.me")$content, 16)) == "194.35.119.132" ~ "localhost", 
#     TRUE ~ "194.35.119.132"
# )

c_fau <- function(){
    dbConnect(RPostgreSQL::PostgreSQL(),
              dbname = "rnf_db",
              host = host,
              port = "5432",
              user = "rnf_app",
              password = readLines("/var/sec/rnf_app.pass")
    )
}
c_bib <- function(){
    dbConnect(RPostgreSQL::PostgreSQL(),
              dbname = "arabib_db",
              host = host,
              port = "5432",
              user = "arabib_read",
              password =  readLines("/var/sec/bib_app.pass")
    )
}
L <- readxl::read_xlsx("translation.xlsx") %>% 
    unite("key", block, key) %>% 
    transpose(.names = .$key)

# Server -----------------------------------------------------------------------
server <- function(input, output, session) {
# Server logic ------------------------------------------------------------
    # initial variables (for current session)
    # storage for reactive values
    values <- reactiveValues(
        sys = list(),
        current_user = NULL,
        status = "no",
        holds = 0,
        # FUTURE: divide holds from total to blocks separately
        # FUTUTRE: clear forms from unholded blocks
        clear_attempt = FALSE,
        current_language = "ru",
        current_df = NULL,
        data_loaded = NULL, # already in database
        geo.origin_ui = radioButtons("geo_origin", "Происхождение координат", 
            selected = character(0), 
            choices = c("Из публикации как есть" = "original", 
                        "Моя собственная привязка" = "volunteer")
        )
    ) 

    popup.info <- reactive({
        tippy(HTML('<p style="font-size:24px;text-align:right">🛈</p>'), 
              tooltip = r("srv_hold"), 
              trigger = "mouseover",
              theme = "material")
    })
    
    # Language Change
    r <- function(X){ # translates text into current language
        txt <- sapply(X,function(s) L[[s]][[values$current_language]], USE.NAMES=FALSE)
        # parse some code to html 
        if(substr(txt, 1, 4) == "ulli"){ 
            txt <- txt %>%
                stringr::str_replace_all("ulli_", "") %>% 
                stringr::str_replace_all("; ", "</li><li>") %>% 
                paste0("<ul><li>", ., "</li></ul>")
        }
        txt
    }
    observeEvent(input$change_language, {
        if(values$current_language == "ru") {
            updateActionButton(session, "change_language", label = "EN")
            showNotification("English language selected", type = "message")
            values$current_language <- "en"
            # } else if(current_language() == "en") { 
            #     updateActionButton(session, "change_language", label = "KZ")
            #     current_language("kz")
        } else{
            updateActionButton(session, "change_language", label = "RU")
            showNotification("Выбран русский язык", type = "message")
            values$current_language <- "ru"
        }
    })
    
# PAGE home ----------------------------------------------------------------
    output$p_home <- renderUI(tagList(
        actionButton("change_language", "RU", 
                     icon = icon("globe"),
                     style = "position: absolute; top: 8px; right: 5px; z-index:10000;"),
        h2(r("home_brief")),
        HTML(paste0("<p>Описание проекта для внешней аудитории. <br>", 
                    "4-5 предложений, где будет обоснование важности проекта с научной и социальной точки зрения. <br>", 
                    "Для каких задач нужны волонтеры и как они могут участвовать? <br> </p>")), 
        HTML(r("home_summary")),
        h2(r("home_details")),
        h3(r("home_aim")),
        HTML(" <br> <ul><li>1. Поиск сведений о находках живых организмов - обязательный этап каждого исследования окружающей среды. </li>"),
        HTML("<li>2. Традиционное решение этой задачи - тотальный просмотр всех научных публикаций - простое, но отнимает чрезвычайно много времени, сил и других ресурсов. </li>"),
        HTML("<li>3. Необходим переход к использованию средств быстрого и эффективного поиск этих данных, без необходимости смотреть каждую статью вручную. </li>"),
        HTML("<li>4. Такие средства есть в готовом виде, есть шаблонные решения с возможностью доработки под конкретные задачи, но…  Нет самих данных. </li>"),
        HTML("<li>5. Цель проекта - разработать подходы и технические средства оцифровки литературных данных по биоразнообразию.  </li>"),
        HTML("<li>6. Пауки Урала будут удобной модельной группой для разработки и оптимизации этих решений.</li> </ul>"),
        h3("Планируемый результат исследования"),
        HTML(" <br> <ul><li>1. Разработан инструмент оцифровки литературных источников, (веб-приложение, база данных, команда волонтеров).</li>"),
        HTML("<li>2. Создана база данных с извлеченными из литературы сведениями (вот этот этап требует привлечения волонтеров)</li>"),
        HTML("<li>3. Обеспечен доступ к полученным данным, к инструментам их анализа.</li> </ul>"),
        h3("Роль волонтеров в научном проекте"),
        HTML("Как результаты деятельности волонтёров помогут нашему исследованию?"),
        HTML(" <br> <ul><li>Нам нужна помощь волонтеров в распознавании и структурировании сведений о находках пауков из предложенных научных статей: кого, где, когда и кто нашел. </li>"),
        HTML("<li>Промежуточная задача - распознавание информации из научных статей и книг, наполнение ею базы данных. </li>"),
        HTML("<li>Глобальная задача - организация свободного доступа ко всем литературным сведениям о находках живых организмов, предоставление к ней средств поиска.</li>")
    ))
    
    
    
# PAGE team ---------------------------------------------------------------
    output$p_team <- renderUI(tagList(
        h3(r("team_team"), align = "center"), 
        fluidRow(
            column(width = 2, tags$img(src = "sozontov.jpg", width = "100%", `data-action`="zoom")), 
            column(width = 10, 
                   h4(r("team_sozontov1")), 
                   HTML(r("team_sozontov2")), 
                   HTML(paste0("<br><b>", r("team_sozontov3"), "</b><br>")),
                   HTML(r("team_sozontov4"))
            )
        ),
        fluidRow(HTML("<br>")),
        fluidRow(
            column(width = 2, HTML('<img src="ivanova.jpg" data-action="zoom" width="100%">')), 
            column(width = 10, h4("Иванова Наталья"), HTML("33 года, к.б.н., <br>
Институт математических проблем биологии – филиал Института прикладной математики им. М.В. Келдыша РАН<br>
<br> 
<b>Аудитор данных</b><br>
Описание роли описание роли описание роли описание роли описание роли описание роли"))),
        fluidRow(HTML("<br>")),
        fluidRow(
            column(width = 2, HTML('<img src="sokolova.jpg" data-action="zoom" width="100%">')), 
            column(width = 10, h4("Соколова Софья"), HTML("31 год, аспирант<br>
Южно-Уральский Федеральный научный центр Минералогии и геоэкологии УрО РАН, <br>
Институт экологии растений и животных УрО РАН<br>
<br> 
<b>Технический писатель</b><br>
Описание роли описание роли описание роли описание роли описание роли описание роли"))),
        fluidRow(
            column(width = 2, HTML('<img src="plakhina.jpg" data-action="zoom" width="100%">')),
            column(width = 10, h4("Плакхина Евгения"), HTML("37 лет, аспирант<br>
Пермский национальный исследовательский университет<br>
<br> 
<b>Библиограф</b><br>
Описание роли описание роли описание роли описание роли описание роли описание роли"))),
        fluidRow(
            column(width = 2, HTML('<img src="ustinova.jpg" data-action="zoom" width="100%">')),
            column(width = 10, h4("Устинова Анастасия"), HTML("23 года, аспирант<br>
Институт экологии растений и животных УрО РАН<br>
<br> 
<b>Коммуникатор</b><br>
Описание роли описание роли описание роли описание роли описание роли описание роли")))
    ))
    
# PAGE scientific profit ---------------------------------------------------
    output$p_sci.profit <- renderUI(tagList(
        HTML("<br>"),
        h3("Глобальная цель проекта, задачи"),
        HTML("Цель Цель Цель Цель Цель Цель Цель Цель Цель Цель Цель <br>"),
        HTML("Задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи задачи"),
        h3("Как результаты деятельности волонтёров помогают нашему исследованию"),
        HTML("(какую промежуточную задачу исследования решают волонтёры)")
    ))
    
# PAGE your personal profit ------------------------------------------------
    output$p_your.profit <- renderUI(tagList(
        HTML("<br>"),
        h3("В чем профит для сообщества волонтеров и исследователей"),
        HTML("<ul><li>Популяризация изучения биоразнообразия</li>"),
        HTML("<li>Показать коллегам и населению, что изучение биоразнобразия это не только натурализм, но высокотехнологичная обработка больших данных.</li>"),
        HTML("<li>Привлечение молодых исследователей в магистратуру и аспирантуру</li>"),
        HTML("<li>Перспектива масштабирования до других регионов и групп живых организмов</li></ul> <br> "),
        h3("В чем профит лично для вас"),
        HTML("Активные участники проекта по оцифровке могут рассчитывать на: <ul>"),
        HTML("<li>Возможность внести свой вклад в науку</li>"),
        HTML("<li>Рейтинг, почетные звания, официальные благодарственные письма</li>"),
        HTML("<li>Информацию о распространении и образе жизни тех, кого волонтер только что оцифровал </li>"),
        HTML("<li>Оффлайн активности: обсуждения, экскурсии, лекции, фестивали, квесты, митапы</li>"),
        HTML("<li>Онлайн чемпионаты, челенджи и марафоны: топ видов, топ авторов, топ точек и т.д.</li>"),
        HTML("<li>Мерч: футболки, значки, нашивки</li>"),
        HTML("<li>Возможность выполнить дипломную работу или диссертацию по этой теме и/или на этой базе данных </li>"),
        HTML("<li>Соавторство в научных публикациях для наиболее активных и продуктивных участников, упоминание в благодарностях для среднеактивных</li><ul>"),
        tags$img(src = "icons8-favicon-96.png", width = 500)
    ))
    
# PAGE how to help ---------------------------------------------------------
    output$p_howtohelp <- renderUI(tagList(
        HTML("<br>"),
        h4("Мы записали для вас несколько роликов, которые помогут вам лучше понять как устроен наш проект научного волонтерства и каким именно образом вы можете ему помочь."),
        HTML("<br>"),
        h2("Для чего и как изучают биоразнообразие"),
        HTML('<iframe width="560" height="315" 
                                 src="https://www.youtube.com/embed/vgaN2tHnVGM" 
                                 frameborder="0" allow="accelerometer; autoplay; encrypted-media; 
                                 gyroscope; picture-in-picture" allowfullscreen></iframe>'), 
        HTML("<br>"),
        h2("Бинарная номенклатура - научное название для живых существ"),
        HTML('<iframe width="560" height="315" 
                                 src="https://www.youtube.com/embed/HgTwE_U6m0U" 
                                 frameborder="0" allow="accelerometer; autoplay; encrypted-media; 
                                 gyroscope; picture-in-picture" allowfullscreen></iframe>'), 
        HTML("<br>"),
        h2("Научная этикетка и фаунистическая публикация"),
        HTML('<iframe width="560" height="315" 
                                 src="https://www.youtube.com/embed/wWQhxkcriXc" 
                                 frameborder="0" allow="accelerometer; autoplay; encrypted-media; 
                                 gyroscope; picture-in-picture" allowfullscreen></iframe>'), 
        HTML("<br>"),
        h2("Процедура ввода данных и интерфейс программы для этого"),
        HTML('<iframe width="560" height="315" 
                                 src="https://www.youtube.com/embed/Fn0NsQ3XOOg" 
                                 frameborder="0" allow="accelerometer; autoplay; encrypted-media; 
                                 gyroscope; picture-in-picture" allowfullscreen></iframe>'),
        h2("Полученные к сегодняшнему дню результаты"),
        HTML('<iframe width="560" height="315" 
                                 src="https://www.youtube.com/embed/-yDw2wr0z-w" 
                                 frameborder="0" allow="accelerometer; autoplay; encrypted-media; 
                                 gyroscope; picture-in-picture" allowfullscreen></iframe>'), 
        HTML("<br>")
    ))
    
# PAGE our project ---------------------------------------------------------
    output$p_our.project <- renderUI(tagList(
        HTML("<br>"),
        h3("Схема проекта", align = "center"),
        HTML('<center><img src="Схема_проекта.svg" width="68%"></center>'),
        h3("Этапы реализации", align = "center"),
        HTML("<b>Этап 1.</b> Разработка веб-приложения и  архитектуры базы данных<br> <br>"),
        icon("arrows-down-to-line"), HTML("<i>Вы находитесь здесь</i><br>"),
        HTML("<b>Этап 2.</b> Тестовый запуск проекта<br>"),
        icon("arrows-up-to-line"), HTML("<i>Вы находитесь здесь</i><br> <br>"),
        HTML("<b>Этап 3.</b> Внесение корректировок<br>"),
        HTML("<b>Этап 4.</b> Запуск проекта в полную силу<br>"),
        HTML("<b>Этап 5.</b> Внесение корректировок<br>")
    ))
    
    
# PAGE cooperation ---------------------------------------------------------
    output$p_cooperation <- renderUI(tagList(
        HTML("<br>"),
        tags$img(src = "icons8-favicon-96.png", width = 500)
    ))
    
# PAGE our web application -------------------------------------------------
    output$p_web.app <- renderUI(tagList(
        HTML("<br><p>Здесь будет описание проекта</p>")
    ))
    
# PAGE statistics general & personal ---------------------------------------
    output$p_stats.general <- renderUI(tagList(
        h3("Здесь данные по всему проекту"),
        br(),
        radioButtons("database_draft", "Вариант", choices = c(
            "Чистовик" = " WHERE type = 'record' ", "Черновик" = ""), 
            inline = TRUE),
        br(),
        actionButton("update_data_all", "Показать БД"),
        br(),br(),
        DT::DTOutput("data_all")
        # h5("А может и не будет...")
    ))
    
    output$p_stats.personal <- renderUI(tagList(
        h3("Здесь данные, введенные вами"),
        br(),
        actionButton("update_data_my", "Показать БД"),
        br(),
        DT::DTOutput("data_my")
    ))

# Actions on PAGE Statistic ---------------------------------------------------------
    observeEvent(input$update_data_all, {
        if(values$status == "yes"){
            s <- Sys.time()
            showNotification("Запрос отправлен, ожидайте...", type = "default")
            con <- c_fau()
            values$data_loaded_all <- dbGetQuery(
                con, paste0(
                "SELECT * FROM records ", input$database_draft, ";")) %>%
                select(-user_id, -ip, -type)
            dbDisconnect(con)
            showNotification(
                paste0("Ответ получен! Время обработки ", round(Sys.time()-s, 2), " сек."), type = "message")
        } else {
            shinyalert::shinyalert(
                "Невозможно", 
                text = "Cтатистика проекта видна только зарегистрированным пользователям",
                type = "warning")
        }
    })
    
    output$data_all <- DT::renderDT(
        values$data_loaded_all
    )
    
    observeEvent(input$update_data_my, { #
        if(values$status == "yes"){
            s <- Sys.time()
            showNotification("Запрос отправлен, ожидайте...", type = "default")
            con <- c_fau()
            values$data_loaded_my <- dbGetQuery(
                con, 
                paste0("select * from records WHERE user_id = ", 
                       values$current_user$tlg_user_id, ";")) %>%
                select( -user_id, -ip)
            dbDisconnect(con)
            showNotification(paste0("Ответ получен! Время обработки ", round(Sys.time()-s, 2), " сек."), type = "message")
        } else {
            shinyalert::shinyalert(
                "Невозможно", 
                text = "Персональная статистика видна только после авторизации",
                type = "warning")
        }
    })
    
    output$data_my <- DT::renderDT(
        values$data_loaded_my
    )
    
# PAGE statistics ----------------------------------------------------------
    output$p_statistics <- renderUI(tagList(
        h4("Сервисные отметки"),
        br(),
        renderPrint(str(values$current_df)),
        br(),
        renderPrint(str(values$sys)),
        br(),
        renderPrint(paste0("hold blocks: ", values$holds)),
        br(), 
        renderPrint(paste0("status = ", values$status)),
        br(),
        h4("Сервисные отметки закончились, далее идут заглушки под реальные статистические блоки", align = "center"), 
        tabsetPanel(
            tabPanel("Общая", uiOutput("p_stats.general")), 
            tabPanel("Персональная", uiOutput("p_stats.personal"))
        )
    ))
    
# PAGE input new data: auth ----------------------------------------------------
output$i_auth <- renderUI(switch(values$status,
    "yes" = {tagList(
        h3(r("i_auth.title"), align = "right"),
        br(),
        HTML(paste0(
            r("i_auth.yes"),
            "<br><tt>Вы зашли как: ",
            values$current_user$name,
            "</tt>"
        )),
        br(),
        actionButton("deauth", r("i_auth.out"), style="float:right")
    )}, 
    "no" = {tagList(
        h1(r("i_auth.title"), align = "left"),
        br(),
        fluidRow(
            column(width = 3, tags$img(src="tlg.bot_QR.svg", height = "135px", `data-action`="zoom")),
            column(width = 9, 
                fluidRow(
                    column(width = 7, 
                        HTML(paste0(
                            '<p align="left">', 
                            r("i_auth.text"),
                            ' <a href = "https://t.me/faunistica_2_bot" target="_blank">', 
                            r("i_link.text"), 
                            ":</a></p><b>←</b><br><br>"))
                    ), 
                    column(width = 5, 
                        HTML(r("i_auth.no"))
                    )
                ), 
                fluidRow(
                    column(width = 7,
                        passwordInput("pass", label = NULL, placeholder = r("i_passwd.fill"), width = '99%')
                    ), 
                    column(width = 5,
                           actionButton("auth", r("i_auth.in"), style="float:left", width = '98%')
                    )
                )
            ) 
        )
    )}
))

# ACTIONS: Log in ------------------------------------------------------------------
    observeEvent(input$auth, {
        if(nchar(input$pass) < 1) {
            shinyalert::shinyalert(title = "Ошибка", text = "Введите пароль", type = "error")
        } else if(nchar(input$pass) < 4) {
            shinyalert::shinyalert(title = "Ошибка", text = "Пароль слишком короткий", type = "error")
        } else if(nchar(input$pass) > 10) {
            shinyalert::shinyalert(title = "Ошибка", text = "Пароль слишком длинный", type = "error")
        } else {
            con <- c_fau()
            values$current_user <- DBI::dbGetQuery(con, 
                                                   paste0("SELECT * FROM users WHERE hash = '", 
                                                          # testpass
                                                          cli::hash_md5(toupper(input$pass)), "';"))
            dbDisconnect(con)
            if(nrow(values$current_user) != 1){
                shinyalert::shinyalert(title = "Ошибка", text = "Пароль неверный", type = "error")
            } else if(difftime(Sys.time(), values$current_user$hash_date, units = "mins") > 3000){
                shinyalert::shinyalert(title = "Пароль устарел", 
                                       text = "Сгенерируйте пароль заново в телеграм-боте", type = "warning")
            } else {
                values$status <- "yes"
                values$publ.id <- str_split(values$current_user$items, "\\|")[[1]]
                con <- c_bib()
                values$publ <- DBI::dbGetQuery(con, paste0("SELECT * FROM bib_tab where id = ", 
                                                           values$publ.id[1], ";"))   
                dbDisconnect(con)
                shinyalert::shinyalert(title = "Вход в систему", 
                                       text = paste0("Вы успешно залогинились! ",
                                                     "\nРад приветствовать вас, ",
                                                     values$current_user$name[[1]], 
                                                     "! \n \nВозможность записи: ", 
                                                     values$status),
                                       type = "success")
            }
        }
    })
    
# ACTIONS: Log out -------------------------------------------------------------
observeEvent(input$deauth, {
    values$status <- "no"
    output$currentstatus <- renderText(values$status)
    shinyalert::shinyalert(
        title = "Выход из системы", 
        text = paste0(
            "Вы успешно вышли из своей учетной записи!", 
            "\nДо новых встреч, ", 
            values$current_user$name[[1]],
            "! \n \nВозможность записи: ", 
            values$status),
        type = "info")
    shinyjs::enable("auth")
    shinyjs::disable("deauth")
    shinyjs::enable("pass")
})
    
# PAGE input new data: current publication --------------------------------
output$i_curr.publ <- renderUI(if(values$status == "yes"){tagList(
    h3("Текущая публикация", align = "left", style = "font-size: 2em"),
    HTML(paste0(
        "<a href = 'https://sozontov.site/arachnolibrary/files/", values$publ$file, "' target = 'blank'>",
        "<b>Автор", dplyr::case_when(str_detect(values$publ$author, ",") ~ "ы", TRUE ~ ""),
        ": </b>", values$publ$author,  "<br>", 
        "<b>Год: </b>", values$publ$year,      "<br>",
        "<b>Название: </b>", values$publ$name, "<br>",
        "<b>Выходные данные: </b>", values$publ$external, "</a><br><br>"
    )),
    actionButton("change_publ", "Публикация обработана. Получить следующую")
  )} else {NULL})

# ACTIONS: Publication change -----------------------------------------------------
observeEvent(input$change_publ, {
    showModal(modalDialog(
        title="Завершение работы с публикацией",
        easyClose = TRUE,
        size = "m",
        footer = tagList(
            h2("Вы уверены", align = "left"),
            h2("что полностью обработали файл?", align = "left"),
            br(), br(), br(),
            actionButton("confirm_full", "Да, полностью: внёс все виды и находки", 
                         style = "background-color: #FF6633; color: black;", width = "60%"),
            br(), br(),
            actionButton("confirm_part", "Не полностью, но все равно хочу поменять", 
                         style = "background-color: #FFCC00; color: black;", width = "60%"),
            br(), br(),
            modalButton("Отмена"#,                        style = "background-color: #66FFCC; color: red;"
                        )
        )
    ))
})

observeEvent(input$confirm_part, {
    removeModal()
    shinyalert::shinyalert("Невозможно", "Извините, такая возможность пока не предусмотрена. 
Внесите все виды и их находки из этой публикации. 

Возможность частичного внесения появится в будущем.")
})

observeEvent(input$confirm_full, {
    if(length(values$publ.id)>1){
        removeModal()
        shinyalert::shinyalert("Стадия 1","") 
        values$publ.id <- values$publ.id[-1]
        con <- c_bib()
        values$publ <- DBI::dbGetQuery(con, paste0("SELECT * FROM bib_tab where id = ", 
                                                   values$publ.id[1], ";"))   
        dbDisconnect(con)
        # showNotification("Публикация переключена на следующую")
        shinyalert::shinyalert("Стадия 2","") 
        con <- c_fau()
        dbSendQuery(con, paste0("UPDATE users SET items = '", 
                                paste0(values$publ.id, collapse = "|"),
                                "' WHERE tlg_user_id = ", 
                                values$current_user$tlg_user_id, ";")
                    )
        dbDisconnect(con)
        shinyalert::closeAlert()
        showNotification("Очередь публикаций обновлена", type = "message")
    } else {
        removeModal()
        showNotification("Невозможно: очередь публикаций подошла к концу", type = "error")
        showNotification("Ждите обновлений системы", type = "default")
    }
})
    
# PAGE input new data: adm -----------------------------------------------------
output$i_adm <- renderUI(tagList(
    h3(r("i_adm.title"), align = "center", style = "font-size: 2em"), 
    br(),
    fluidRow(
        column(width = 3, textInput("country", r("i_adm0"), value = "Россия")),
        # FUTURE:  list of regions, municipalities and localities by OTKMO
        column(width = 3, textInput("region",    r("i_adm1"))),
        column(width = 3, textInput("district",  r("i_adm2"))), 
        column(width = 3, textInput("loc", r("i_loc"), 
                                    placeholder = r("i_loc.fill"))) 
    ),
    fluidRow(
        column(width = 9), 
        column(width = 1, popup.info()),
        column(width = 1, actionButton("hold_adm", "",  
            icon = icon("lock"), style="float:right")), 
        column(width = 1, actionButton("unhold_adm", "", 
            icon = icon("lock-open"), disabled = TRUE, style="float:left"))
    )
))
    
    observeEvent(input$hold_adm, {
        values$holds <- values$holds+1
        shinyjs::enable("unhold_adm")
        shinyjs::disable("country")
        shinyjs::disable("region")
        shinyjs::disable("district")
        shinyjs::disable("loc")
        shinyjs::disable("hold_adm")
    })
    
    observeEvent(input$unhold_adm, {
        values$holds <- values$holds-1
        shinyjs::disable("unhold_adm")
        shinyjs::enable("country")
        shinyjs::enable("region")
        shinyjs::enable("district")
        shinyjs::enable("loc")
        shinyjs::enable("hold_adm")
    })
    
# PAGE input new data: geo ------------------------------------------------
output$i_geo <- renderUI({tagList(
    h3(r("i_geo.title"), align = "center", style = "font-size: 2em"), 
    br(),
    fluidRow(
        column(width = 2, 
            radioButtons("geo_type", "Формат координат", selected = 1, choices = c(
                'ГГ.гггг° (56.83777°)' = 1,
                "ГГ°ММ.мм' (56° 50.266')" = 2,
                "ГГ°ММ'СС'' (56° 50' 15.99'')" = 3
            ))),
        column(width = 3, uiOutput("coordinate_input")),
        column(width = 1, 
            HTML("<div style = 'font-size:25px'>N</div><div style = 'font-size:25px; padding-top: 10px'>E</div>")
        ),
        column(width = 3, renderUI(values$geo.origin_ui)),
        column(width = 3, textAreaInput("geo.rem",    r("i_geo2")), #, width = "100%"
            fluidRow(
                   column(width = 4, popup.info() ),
                   column(width = 4, actionButton("hold_geo", "",  icon = icon("lock"), style="float:right")), 
                   column(width = 4, actionButton("unhold_geo", "", icon = icon("lock-open"), disabled = TRUE, style="float:left"))
               )
        )
    )
)})

output$coordinate_input <- renderUI({
    switch(input$geo_type,
        `1` = tagList(
            fluidRow(
                column(10, numericInput("N1", NULL, NA, -89, 89)),
                column(2, HTML("<b>°</b>"))
            ), 
            fluidRow(
                column(10, numericInput("E1", NULL, NA, -180, 180)),
                column(2, HTML("<b>°</b>"))
            )
        ),
        `2` = tagList(
            fluidRow(
                column(5, numericInput("N2.1", NULL, NA, -89, 89), step = 1),
                column(1, HTML("<b>°</b>")),
                column(5, numericInput("N2.2", NULL, NA, -180, 180), step = 1),
                column(1, HTML("<b>'</b>"))
            ), 
            fluidRow(
                column(5, numericInput("E2.1", NULL, NA, 0, 60)),
                column(1, HTML("<b>°</b>")),
                column(5, numericInput("E2.2", NULL, NA, 0, 60)),
                column(1, HTML("<b>'</b>"))
            )
        ), 
        `3` = tagList(
            fluidRow(
                column(3, numericInput("N3.1", NULL, NA, -89, 89)),
                column(1, HTML("<b>°</b>")),
                column(3, numericInput("N3.2", NULL, NA, 0, 60)),
                column(1, HTML("<b>'</b>")),
                column(3, numericInput("N3.3", NULL, NA, 0, 60)),
                column(1, HTML('<b>"</b>'))
            ), 
            fluidRow(
                column(3, numericInput("E3.1", NULL, NA, -180, 180)),
                column(1, HTML("<b>°</b>")),
                column(3, numericInput("E3.2", NULL, NA, 0, 60)),
                column(1, HTML("<b>'</b>")),
                column(3, numericInput("E3.3", NULL, NA, 0, 60)),
                column(1, HTML('<b>"</b>'))
            )
        ) 
    )
})
     
observe({
    if (isTruthy(input$NN1) || isTruthy(input$EE1)) {
        shinyjs::disable("NN2")
        shinyjs::disable("NN3")
        shinyjs::disable("EE2")
        shinyjs::disable("EE3")
    } else {
        shinyjs::enable("NN2")
        shinyjs::enable("NN3")
        shinyjs::enable("EE2")
        shinyjs::enable("EE3")
    }
})

observe({
    if (isTruthy(input$NN2) || isTruthy(input$EE2)) {
        shinyjs::disable("NN1")
        shinyjs::disable("NN3")
        shinyjs::disable("EE1")
        shinyjs::disable("EE3")
    } else {
        shinyjs::enable("NN1")
        shinyjs::enable("NN3")
        shinyjs::enable("EE1")
        shinyjs::enable("EE3")
    }
})

observe({
    if (isTruthy(input$NN3) || isTruthy(input$EE3)) {
        shinyjs::disable("NN1")
        shinyjs::disable("NN2")
        shinyjs::disable("EE1")
        shinyjs::disable("EE2")
    } else {
        shinyjs::enable("NN1")
        shinyjs::enable("NN2")
        shinyjs::enable("EE1")
        shinyjs::enable("EE2")
    }
})
    
observeEvent(input$hold_geo, {
    values$holds <- values$holds+1
    shinyjs::disable("geo_type")
    shinyjs::disable("N1")
    shinyjs::disable("N2.1")
    shinyjs::disable("N2.2")
    shinyjs::disable("N3.1")
    shinyjs::disable("N3.2")
    shinyjs::disable("N3.3")
    shinyjs::disable("E1")
    shinyjs::disable("E2.1")
    shinyjs::disable("E2.2")
    shinyjs::disable("E3.1")
    shinyjs::disable("E3.2")
    shinyjs::disable("E3.3")
    shinyjs::disable("geo_origin")
    shinyjs::disable("geo.rem")
    shinyjs::disable("hold_geo")
    shinyjs::enable("unhold_geo")
})

observeEvent(input$unhold_geo, {
    values$holds <- values$holds-1
    shinyjs::enable("geo_type")
    shinyjs::enable("N1")
    shinyjs::enable("N2.1")
    shinyjs::enable("N2.2")
    shinyjs::enable("N3.1")
    shinyjs::enable("N3.2")
    shinyjs::enable("N3.3")
    shinyjs::enable("E1")
    shinyjs::enable("E2.1")
    shinyjs::enable("E2.2")
    shinyjs::enable("E3.1")
    shinyjs::enable("E3.2")
    shinyjs::enable("E3.3")
    shinyjs::enable("geo_origin")
    shinyjs::enable("geo.rem")
    shinyjs::enable("hold_geo")
    shinyjs::disable("unhold_geo")
})
    
# PAGE input new data: event ----------------------------------------------
    output$i_event <- renderUI(tagList(
        h3(r("i_ev.title"), align = "center", style = "font-size: 2em"), 
        br(),
        fluidRow(
            column(width = 3, 
                   column(width = 4, numericInput("yy", "Год", NA, 1770, 2024)),
                   column(width = 3, numericInput("mm", "Месяц",  NA, 1,    12  )),
                   column(width = 4, numericInput("dd", "День",   NA, 1,    31  ))
                ),
            column(width = 3, textInput("habitat", r("i_ev.hab"))),
            # FUTURE: connect date with publ: date not more than publ year
            column(width = 3, textInput(
                "effort", 
                r("i_ev.effort"), 
                placeholder = r("i_ev.effort_fill"))),
            column(width = 3, textAreaInput("event_rem", r("i_ev.rem")))
        ),
        fluidRow(
            column(width = 2, checkboxInput("day.def", "День определён", TRUE)),
            column(width = 7), 
            column(width = 1, popup.info() ),
            column(width = 1, actionButton("hold_ev", "",  icon = icon("lock"), style="float:right")), 
            column(width = 1, actionButton("unhold_ev", "", icon = icon("lock-open"), disabled = TRUE, style="float:left"))
        )
    ))

observeEvent(input$day.def, {
    if(input$day.def == FALSE){
        updateTextInput(session, "dd", value = NA)
        shinyjs::disable("dd")
    }
    if(input$day.def == TRUE){
        shinyjs::enable("dd")
    }
})
    
    observeEvent(input$hold_ev, {
        values$holds <- values$holds+1
        shinyjs::disable("yy")
        shinyjs::disable("mm")
        shinyjs::disable("dd")
        shinyjs::disable("day.def")
        shinyjs::disable("habitat")
        shinyjs::disable("effort")
        shinyjs::disable("event_rem")
        shinyjs::enable("unhold_ev")
        shinyjs::disable("hold_ev")
    })
    observeEvent(input$unhold_ev, {
        values$holds <- values$holds-1
        shinyjs::enable("yy")
        shinyjs::enable("mm")
        shinyjs::enable("dd")
        shinyjs::enable("day.def")
        shinyjs::enable("habitat")
        shinyjs::enable("effort")
        shinyjs::enable("event_rem")
        shinyjs::enable("hold_ev")
        shinyjs::disable("unhold_ev")
    })
    
# PAGE input new data: taxa -----------------------------------------------
    output$i_taxa <- renderUI(tagList(
        h3(r("i_taxa.title"), align = "center", style = "font-size: 2em"), 
        br(),
        fluidRow(
            # FUTURE: connect taxa list from world spider catalog
            column(width = 3, textInput("fam", r("i_taxa.fam"))),
            column(width = 3, textInput("gen", r("i_taxa.gen"))),
            column(width = 3, textInput("sp",  r("i_taxa.sp"))),
            column(width = 3, textAreaInput("tax.rem", "Таксономические примечания"))
        ),
        fluidRow(
            column(width = 3, 
                conditionalPanel(
                    condition = "input.taxa_nsp == true", # If checkboxA is TRUE, show selectInput
                    selectInput("type_status", label = NULL, choices = c(
                        "Выберите типовой статус экземпляра" = "", 
                        "Голотип", "Паратип", "Неотип", "Другое")
                    )
                )
            ),
            column(width = 3, checkboxInput("taxa_nsp", r("i_taxa.nsp"))), 
            column(width = 3, checkboxInput("sp.def", r("i_taxa.sp.def"), value = TRUE)),
            column(width = 1, br(), popup.info()),
            column(width = 1, br(), actionButton("hold_taxa", "",  icon = icon("lock"), style="float:right")), 
            column(width = 1, br(), actionButton("unhold_taxa", "", icon = icon("lock-open"), disabled = TRUE, style="float:left"))
        )
    ))
    
    observeEvent(input$sp.def, {
        if(input$sp.def == FALSE){
            updateTextInput(session, "sp", value = NA)
            shinyjs::disable("sp")
        }
        if(input$sp.def == TRUE){
            shinyjs::enable("sp")
        }
    })
    
    observeEvent(input$hold_taxa, {
        values$holds <- values$holds+1
        shinyjs::disable("fam")
        shinyjs::disable("gen")
        shinyjs::disable("sp")
        shinyjs::disable("tax.rem")
        shinyjs::disable("type_status")
        shinyjs::disable("taxa_nsp")
        shinyjs::disable("sp.def")
        shinyjs::enable("unhold_taxa")
        shinyjs::disable("hold_taxa")
    })
    
    observeEvent(input$unhold_taxa, {
        values$holds <- values$holds-1
        shinyjs::enable("fam")
        shinyjs::enable("gen")
        shinyjs::enable("sp")
        shinyjs::enable("tax.rem")
        shinyjs::enable("type_status")
        shinyjs::enable("taxa_nsp")
        shinyjs::enable("sp.def")
        updateRadioButtons(session, "sp.def", selected = TRUE)
        shinyjs::enable("hold_taxa")
        shinyjs::disable("unhold_taxa")
    })
    
# PAGE input new data: amount ---------------------------------------------
output$i_abu <- renderUI(tagList(
    h3(r("i_abu.title"), align = "center", style = "font-size: 2em"),
    HTML("<br>"), 
    fluidRow(
        column(width = 6, 
            fluidRow(
                column(2, numericInput("jjj", "Ювенильных", width = "80%",
                                       value = 0, min = 0, max = 299)),
                column(2, numericInput("sm", "juv самцов",  width = "85%",
                                       value = 0, min = 0, max = 299)),
                column(2, numericInput("sf", "juv самок", width = "75%",
                                       value = 0, min = 0, max = 299)),
                column(3, numericInput("mmm", r("i_abu.mmm"),  width = "85%",
                    value = 0, min = 0, max = 299)),
                column(3, numericInput("fff", r("i_abu.fff"), width = "75%",
                                       value = 0, min = 0, max = 299))
            )
        ),
        column(width = 3, textInput("coll", "Коллектор")),
        column(width = 3, textAreaInput("ind_rem", r("i_abu.rem"))),
    )
))
    
    
# PAGE input new data - combine! ------------------------------------------
output$p_input.data <- renderUI(tagList(
    br(),
    switch(values$status, 
        "no" = fluidRow(
            column(width = 6, uiOutput("i_curr.publ")), 
            column(width = 6, uiOutput("i_auth"))
        ),
        "yes"  = fluidRow(
            column(width = 10, uiOutput("i_curr.publ")), 
            column(width = 2, uiOutput("i_auth"))
        )
    ),
    br(),
    hr(),
    uiOutput("i_adm"),
    hr(),
    uiOutput("i_geo"),
    hr(),
    uiOutput("i_event"),
    hr(),
    uiOutput("i_taxa"),
    hr(),
    uiOutput("i_abu"),
    hr(),
    fluidRow(
        column(width = 4),
        column(width = 2, actionButton("check", r("i_check"), width = "80%", style="float:left")),
        column(width = 2, actionButton("record", r("i_record"), width = "80%", style="float:left")),
        column(width = 2),
        column(width = 2, actionButton("drop", r("i_drop"), width = "80%", style="float:left"))
    ),
    br(),
    hr(),
    br()
))

# ACTIONS: Clear / drop --------------------------------------------------------
clear_fields <- function(session){
    # adm
    updateTextInput(session, "country", value = "Россия")
    updateTextInput(session, "region", value = "")
    updateTextInput(session, "district", value = "")
    updateTextInput(session, "loc", value = "")
    
    # geo
    updateNumericInput(session, "N1", value = "")
    updateNumericInput(session, "E1", value = "")
    updateNumericInput(session, "N2.1", value = "")
    updateNumericInput(session, "N2.2", value = "")
    updateNumericInput(session, "E2.1", value = "")
    updateNumericInput(session, "E2.2", value = "")
    updateNumericInput(session, "N3.1", value = "")
    updateNumericInput(session, "N3.2", value = "")
    updateNumericInput(session, "N3.3", value = "")
    updateNumericInput(session, "E3.1", value = "")
    updateNumericInput(session, "E3.2", value = "")
    updateNumericInput(session, "E3.3", value = "")
    # updateRadioButtons(session, "geo_type", selected = 1)
    updateTextInput(session, "geo.rem", value = "")
    values$geo.origin_ui <- NULL
    values$geo.origin_ui <- radioButtons("geo_origin", "Происхождение координат", 
                                         selected = character(0), 
                                         choices = c("Из публикации как есть", "Моя собственная привязка")
    )
    
    #event
    updateTextInput(session, "yy", value = "")
    updateTextInput(session, "mm", value = "")
    updateTextInput(session, "dd", value = "")
    updateCheckboxInput(session, "day.def", value = TRUE)
    updateTextInput(session, "habitat", value = "")
    updateTextInput(session, "effort", value = "")
    updateTextInput(session, "event_rem", value = "")
    
    #taxa
    updateTextInput(session, "fam", value = "")
    updateTextInput(session, "gen", value = "")
    updateTextInput(session, "sp", value = "")
    updateTextInput(session, "tax.rem", value = "")
    updateCheckboxInput(session, "sp.def", value = TRUE)
    updateCheckboxInput(session, "taxa_nsp", value = FALSE)
    updateSelectInput(session, "type_status", selected = "")
    
    # abu
    updateTextInput(session, "mmm", value = "")
    updateTextInput(session, "fff", value = "")
    updateTextInput(session, "sm", value = "")
    updateTextInput(session, "sf", value = "")
    updateTextInput(session, "jjj", value = "")
    updateTextInput(session, "coll", value = "")
    updateTextInput(session, "ind_rem", value = "")
    # FUTUTRE: clear forms from unholded blocks
}
    
observeEvent(input$drop, { 
    if(values$holds != 0) { 
        shinyalert::shinyalert(
            title = "Невозможно", 
            text = paste0(
                "Не могу сбросить введённые данные\nЧасть блоков зафиксирована (",
                values$holds, " шт.)"
            ), 
            type = "warning")
    } else {
        clear_fields(session)
    }
})

# ACTIONS: Check & Record ------------------------------------------------------
adm_check <- function(country, region, district){
    errors <- character()
    if(nchar(country) < 4) {errors <- c(errors, "Страна указана некорректно")}
    if(nchar(region) < 5)  {errors <- c(errors, "Регион указан некорректно")}
    if(nchar(district) < 5){errors <- c(errors, "Район указан некорректно")}
    errors
}

geo_check1 <- function(nn, ee, geo_type = 1){
    if(geo_type != 1){break}
    errors <- character()
    nn_out <- NA
    ee_out <- NA
    if(is.na(nn) || nn == 0){
        errors <- c(errors, "Широта не задана")
    } else if(nchar(nn)<4){
        errors <- c(errors, "Недостаточна точность широты")
    } else if(nchar(nn)>9){
        errors <- c(errors, "Невозможно большая точность широты")
    } else {nn_out <- nn}
    if(is.na(ee) || ee == 0){
        errors <- c(errors, "Долгота не задана")
    } else if(nchar(ee)<4){
        errors <- c(errors, "Недостаточна точность долготы")
    } else if(nchar(ee)>9){
        errors <- c(errors, "Невозможно большая точность долготы")
    } else {ee_out <- ee}
    if(!is.na(nn_out) && !is.na(ee_out) && abs(nchar(nn_out) - nchar(ee_out)) > 2){
        errors <- c(errors, "Разная точность широты и долготы")
        nn_out <- NA
        ee_out <- NA
    }
    list(nn = nn_out, ee = ee_out, errors = errors)
}

geo_check2 <- function(nn1, nn2, ee1, ee2, geo_type = 2){
    if(geo_type != 2){break}
    errors <- character()
    nn_out <- NA
    ee_out <- NA
    
    if(is.na(nn1) || is.na(nn2) || nn1 == 0 || nn2 == 0){
        errors <- c(errors, "Широта не задана")
    } else if(nn2 >= 60){
        errors <- c(errors, "Минуты широты выходят за пределы допустимого")
    } else if(nn1 != round(nn1)) {
        errors <- c(errors, "Дробные градусы широты")
    } else if(nchar(nn2)<2){
        errors <- c(errors, "Недостаточна точность широты")
    } else if(nchar(nn2)>6){
        errors <- c(errors, "Невозможно большая точность широты")
    } else {nn_out <- nn1 + nn2/60}
    
    if(is.na(ee1) || is.na(ee2) || ee1 == 0 || ee2 == 0){
        errors <- c(errors, "Долгота не задана")
    } else if(ee2 >= 60){
        errors <- c(errors, "Минуты долготы выходят за пределы допустимого")
    } else if(ee1 != round(ee1)) {
        errors <- c(errors, "Дробные градусы долготы")
    } else if(nchar(ee2)<2){
        errors <- c(errors, "Недостаточна точность долготы")
    } else if(nchar(ee2)>6){
        errors <- c(errors, "Невозможно большая точность долготы")
    } else {ee_out <- ee1 + ee2/60}

    if(!is.na(nn_out) && !is.na(ee_out) && abs(nchar(nn2) - nchar(ee2)) > 2){
        errors <- c(errors, "Разная точность широты и долготы")
        nn_out <- NA
        ee_out <- NA
    }
    list(nn = nn_out, ee = ee_out, errors = errors)
}

geo_check3 <- function(nn1, nn2, nn3, ee1, ee2, ee3, geo_type = 3){
    if(geo_type != 3){break}
    errors <- character()
    nn_out <- NA
    ee_out <- NA
    
    if(is.na(nn1) || is.na(nn2) || is.na(nn3) || nn1 == 0 || nn2 == 0 || nn3 == 0){
        errors <- c(errors, "Широта не задана")
    } else if(nn2 >= 60){
        errors <- c(errors, "Минуты широты выходят за пределы допустимого")
    } else if(nn3 >= 60){
        errors <- c(errors, "Секунды широты выходят за пределы допустимого")
    } else if(nn1 != round(nn1)) {
        errors <- c(errors, "Дробные градусы широты")
    } else if(nn2 != round(nn2)) {
        errors <- c(errors, "Дробные минуты широты")
    } else if(nchar(nn3)<2){
        errors <- c(errors, "Недостаточна точность широты")
    } else if(nchar(nn3)>4){
        errors <- c(errors, "Невозможно большая точность широты")
    } else {nn_out <- nn1 + nn2/60 + nn3/3600}
    
    if(is.na(ee1) || is.na(ee2) || is.na(ee3) || ee1 == 0 || ee2 == 0 || ee3 == 0){
        errors <- c(errors, "Долгота не задана")
    } else if(ee2 >= 60){
        errors <- c(errors, "Минуты долготы выходят за пределы допустимого")
    } else if(ee3 >= 60){
        errors <- c(errors, "Секунды долготы выходят за пределы допустимого")
    } else if(ee1 != round(ee1)) {
        errors <- c(errors, "Дробные градусы долготы")
    } else if(ee2 != round(ee2)) {
        errors <- c(errors, "Дробные минуты долготы")
    } else if(nchar(ee3)<2){
        errors <- c(errors, "Недостаточна точность долготы")
    } else if(nchar(ee3)>4){
        errors <- c(errors, "Невозможно большая точность долготы")
    } else {ee_out <- ee1 + ee2/60 + ee3/3600}
    
    if(!is.na(nn3) && !is.na(ee3) && abs(nchar(nn3) - nchar(ee3)) > 2){
        errors <- c(errors, "Разная точность широты и долготы")
        nn_out <- NA
        ee_out <- NA
    }
    list(nn = nn_out, ee = ee_out, errors = errors)
}

my_check <- function(){
    # check starts from geo coordinates
    errors <- character()
    if(input$geo_type == 1){
        nn_raw <<- input$N1
        ee_raw <<- input$E1
        check <- geo_check1(input$N1, input$E1, 1)
    } else if(input$geo_type == 2){
        nn_raw <<- paste0(input$N2.1, "°", input$N2.2, "'")
        ee_raw <<- paste0(input$E2.1, "°", input$E2.2, "'")
        check <- geo_check2(input$N2.1, input$N2.2, input$E2.1, input$E2.2, geo_type = 2)
    } else if(input$geo_type == 3){
        nn_raw <<- paste0(input$N3.1, "°", input$N3.2, "'", input$N3.3, '"')
        ee_raw <<- paste0(input$E3.1, "°", input$E3.2, "'", input$E3.3, '"')
        check <- geo_check2(input$N3.1, input$N3.2, input$N3.3,
            input$E3.1, input$E3.2, input$E3.3, geo_type = 3)
    }

    nn <<- check$nn
    ee <<- check$ee
    errors <- c(errors, check$errors)
    if(!is.na(nn) && (nn < 50.5 | nn > 73)) {
        errors <- c(errors, "Широта выходит за границы исследуемого региона")
    }
    if(!is.na(ee) && (ee < 55 | nn > 74)) {
        errors <- c(errors, "Точка выходит за границы исследуемого региона по широте")
    }
    if(is.null(input$geo_origin)){ #!= "original" | input$geo_origin != "volunteer"){
        errors <- c(errors, "Происхождение координат не указано")
    }
    
    # adm check
    errors <- c(errors, adm_check(input$country, input$region, input$district))
    
    # date check
    if(is.na(input$yy)){
        errors <- c(errors, "Год не указан")
    }
    if(is.na(input$mm)){
        errors <- c(errors, "Месяц не указан")
    }
    if(is.na(input$dd) & input$day.def){
        errors <- c(errors, "День не указан")
    }
    
    # taxa check
    if(nchar(input$fam) < 1){
        errors <- c(errors, "Семейство не указано")
    }
    if(nchar(input$gen) < 1){
        errors <- c(errors, "Род не указан")
    }
    if(nchar(input$sp) < 1 & input$sp.def){
        errors <- c(errors, "Вид не указан")
    }
    
    # amount check
    if(is.na(input$mmm) && is.na(input$fff) && is.na(input$jjj) &&
       is.na(input$sm) && is.na(input$sf)){
        errors <- c(errors, "Слишком мало особей")
    } else if (input$mmm + input$fff + input$jjj +
               input$sm + input$sf < 1){
        errors <- c(errors, "Слишком мало особей")
    }
    if(is.na(input$coll)){
        errors <- c(errors, "Коллектор? (если не указано в пубикации, поставьте её автора")
    } else if(nchar(input$coll)<3){
        errors <- c(errors, "Коллектор не распознан")
    }
    
    # final check
    errors <- paste0(errors, collapse = "; ")
    if(nchar(errors) > 0) { 
        return(list(check = "errors", errors = errors))
    } else { 
        return(list(check = "fine",   errors = errors))
    }
}
    
observeEvent(input$check, {
    current_check <- my_check()
    
    if(current_check$check == "errors") {
        current_check$errors %>%
            # paste0(collapse = "; ") %>% 
            stringr::str_replace_all("; ", "</li><li>") %>% 
            paste0('<p align="left"><ul><li>', 
                   ., 
                   "</li></ul></p>") %>% 
            shinyalert::shinyalert("Ошибки:", 
                               text = .,
                               type = "warning",
                               html = TRUE)
        showNotification("Есть ошибки", type = "error")
    } else {
        shinyalert::shinyalert("Успех!", "Ошибок не найдено", type = "success")
        showNotification("Все проверки пройдены!", type = "message")
    }
    values$current_df <- {tibble(
        user_id  = if(is.null(values$current_user$tlg_user_id)){NA
                    } else {values$current_user$tlg_user_id},
        datetime = Sys.time(),
        ip       = NA, ### put here code for tracking
        publ_id  = if(is.null(values$publ$id)){NA
                    } else {values$publ$id},
        type     = "check",
        errors   = paste0(current_check$errors, collapse = " | "),
        adm_country     = input$country,
        adm_region      = input$region,
        adm_district    = input$district,
        adm_loc  = input$loc,
        geo_nn   = nn,
        geo_ee   = ee,
        geo_nn_raw = nn_raw,
        geo_ee_raw = ee_raw,
        geo_origin = if(is.null(input$geo_origin)){NA} else {input$geo_origin},
        geo_REM  = input$geo.rem,
        eve_yy   = input$yy,
        eve_mm   = input$mm,
        eve_dd   = input$dd,
        eve_day.def     = input$day.def,
        eve_habitat     = input$habitat,
        eve_effort      = input$effort,
        eve_REM  = input$event_rem,
        tax_fam  = input$fam,
        tax_gen  = input$gen,
        tax_sp   = input$sp,
        tax_sp.def      = input$sp.def,
        tax_nsp         = input$taxa_nsp,
        tax_type_status = input$type_status,
        tax_REM  = input$tax.rem,
        abu_mmm  = input$mmm,
        abu_fff  = input$fff,
        abu_sm   = input$sm,
        abu_sf   = input$sf,
        abu_jjj  = input$jjj,
        abu_coll = input$coll,
        abu_ind_rem     = input$ind_rem
    )}
    con <- c_fau()
    DBI::dbWriteTable(con, "records", values$current_df, append = TRUE, row.names = FALSE)
    DBI::dbDisconnect(con)
})
    
observeEvent(input$record, {
    current_check <- my_check()
    if(values$status != "yes"){ 
        shinyalert::shinyalert(
            title = "Не выполнен вход", 
            text = paste0('Вносить записи можно только после авторизации. ',
                          'Получите пароль у <a href="https://t.me/faunistica_2_bot" ', 
                          ' target="_blank">телеграм-бота</a> и введите его.'),
            type = "error",
            html = TRUE)
        showNotification("Не выполнен вход", type = "error")
    } else if(current_check$check == "errors") {
        shinyalert::shinyalert("Ошибки:", 
                               text = paste0('<p align="left"><ul><li>', 
                                             current_check$errors, 
                                             "</li></ul></p>"),
                               type = "warning",
                               html = TRUE)
        showNotification("Есть ошибки", type = "error")
    } else {
        showNotification("Все проверки пройдены, записываем в базу данных...", type = "default")
        values$current_df <- {tibble(
            user_id  = if(is.null(values$current_user$tlg_user_id)){NA
            } else {values$current_user$tlg_user_id},
            datetime = Sys.time(),
            ip       = NA, ### put here code for tracking
            publ_id  = if(is.null(values$publ$id)){NA
            } else {values$publ$id},
            type     = "record",
            errors   = paste0(current_check$errors, collapse = " | "),
            adm_country     = input$country,
            adm_region      = input$region,
            adm_district    = input$district,
            adm_loc  = input$loc,
            geo_nn   = nn,
            geo_ee   = ee,
            geo_nn_raw = nn_raw,
            geo_ee_raw = ee_raw,
            geo_origin = if(is.null(input$geo_origin)){NA} else {input$geo_origin},
            geo_REM  = input$geo.rem,
            eve_yy   = input$yy,
            eve_mm   = input$mm,
            eve_dd   = input$dd,
            eve_day.def     = input$day.def,
            eve_habitat     = input$habitat,
            eve_effort      = input$effort,
            eve_REM  = input$event_rem,
            tax_fam  = input$fam,
            tax_gen  = input$gen,
            tax_sp   = input$sp,
            tax_sp.def      = input$sp.def,
            tax_nsp         = input$taxa_nsp,
            tax_type_status = input$type_status,
            tax_REM  = input$tax.rem,
            abu_mmm  = input$mmm,
            abu_fff  = input$fff,
            abu_sm   = input$sm,
            abu_sf   = input$sf,
            abu_jjj  = input$jjj,
            abu_coll = input$coll,
            abu_ind_rem     = input$ind_rem
        )}
        con <- c_fau()
        DBI::dbWriteTable(con, "records", values$current_df, append = TRUE, row.names = FALSE)
        DBI::dbDisconnect(con)
        showNotification("Записано успешно!", type = "message")
    }
})

# NAVBAR ------------------------------------------------------------------
    output$NAVBAR <- renderUI(tagList(
        # shinyjs::useShinyjs(),
        navbarPage(
            title = tags$div(style="position: relative; margin-right: 90px", 
                             tags$img(src="logo_placeholder.svg", height = "70px"),
                             tags$p(style="position: relative; top: -70px; left: 90px; ", 
                                    "Faunistica 2.0")
            ),
            windowTitle = "Faunistica 2.0",
            position = "fixed-top",
            tabPanel(title = r("nv_home"), uiOutput("p_home")), 
            navbarMenu(r("nv_about"), 
                       tabPanel(r("nv_team"), uiOutput("p_team")),
                       "----",
                       r("nv_for_volunteers"),
                       tabPanel(r("nv_profit_science"), uiOutput("p_sci.profit")), 
                       tabPanel(r("nv_profit_personal"),   uiOutput("p_your.profit")), 
                       tabPanel(r("nv_howtohelp"),   uiOutput("p_howtohelp")), 
                       tabPanel(r("nv_voluntary_project"), uiOutput("p_our.project")), 
                       "----",
                       r("nv_for_scientists"),
                       tabPanel(r("nv_cooperation"), uiOutput("p_cooperation")), 
                       tabPanel(r("nv_web_app"), uiOutput("p_web.app")), 
                       tabPanel(r("nv_scientific_project"), h4("to be filled soon...")),
                       "----"
            ),
            tabPanel(r("nv_statistics"), uiOutput("p_statistics") 
                     
            ),
            tabPanel(r("nv_participate"), uiOutput("p_input.data")) 
        )
    ))
    
}

# UI ----------------------------------------------------------------------
ui <- fluidPage(
    shinyjs::useShinyjs(),
    HTML('<link href="zoom.css" rel="stylesheet">'),
    HTML('<script src="zoom.js"></script>'),
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    tags$style(type = "text/css", ".right-align {text-align: right;}"),
    tags$head(tags$link(rel="shortcut icon", href="icons8-favicon-96.png")),
    uiOutput("NAVBAR")
)

# Run the application 
shinyApp(ui = ui, server = server,  
         options = list(launch.browser = TRUE, host = "0.0.0.0"),
         onStart = function() {
             
             onStop(function() {
                 rm(list = ls())
                 # dbDisconnect(con)
             })
         }
)