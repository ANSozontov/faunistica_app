# initial -----------------------------------------------------------------
# bot:        /srv/bot/bot.R
# running sh: /srv/bot/bot.sh
# service:    rnf_authbot.service
# psql  conf: /etc/postgresql/14/main/pg_hba.conf

library(telegram.bot)
library(RPostgreSQL)
library(tidyverse)

con <- function(){
                 dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname = "rnf_db",
                 host = "sozontov.site",
                 # host = "localhost",
                 port = "5432",
                 user = "rnf_bot",
                 password = readr::read_lines("/var/sec/rnf_bot.pass"))
}

# bot3 --------------------------------------------------------------------
# user_name = FALSE
bot <- Bot(token = scan("/var/sec/rnf_bot.tok", what = "", quiet = TRUE))
bot$clean_updates()

fun_start <- function(bot, update){
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = paste0('Здравствуйте, ', 
                                  update$message$from$first_name, '
Я - телеграм-бот проекта <a 
href="https://ansozontov.github.io/faunistica/">Faunistica 2.0</a>, очень рад, что Вы  им заинтересовались. 
С удовольствием зарегистрирую вас как нового участника и дам пароль для входа на <a
href = "https://sozontov.site/faunistica_2.0">наш сайт</a>.
Чем могу вам помочь?'),
                    parse_mode = 'HTML',
                    disable_web_page_preview = TRUE)
}

MessageFilters$flt_auth <- BaseFilter(function(message) {
    grepl(x = message$text, ignore.case = TRUE,
          pattern = 'auth|Вход в веб-приложение')
})

fun_auth <- function(bot, update){
    usr_id <- update$message$from$id
    # FUTURE: добавить строку состояния с id текущей публикации
    cc <- con()
    usr_status <- dbGetQuery(cc, paste0("select reg_stat from users where tlg_user_id = '", usr_id, "';"))[[1]]
    
    if(length(usr_status) == 0) {
        bot$sendMessage(chat_id = update$message$chat_id, 
            text = "Увы, вас пока нет среди зарегистрированных пользователей. \nЖелаете зарегистрироваться?\n /register ← Нажмите сюда"
            )
        dbDisconnect(cc)
    } else if (usr_status != 1) {
        bot$sendMessage(chat_id = update$message$chat_id, 
                        text = "Увы, ваша регистрация начата, но не закончена. \nНастоятельно рекомендую продолжить:\n /register ← Нажмите сюда"
                        )
        dbDisconnect(cc)
    } else {
        tmp <- paste0(sample(c(0:9, LETTERS), 6, replace = TRUE), collapse = "")
        tm <- Sys.time()
        # cc <- con()
        DBI::dbSendQuery(cc, paste0(
            "UPDATE users SET hash = '", 
            cli::hash_md5(tmp), 
            "', hash_date = '", 
            Sys.time(),
            "' WHERE tlg_user_id = '", 
            usr_id, 
            "';"))
        dbDisconnect(cc)
        bot$sendMessage(chat_id = update$message$chat_id, parse_mode = 'Markdown',
            text = paste0(
                "Ваш код доcтупа: \n```", 
                tmp, "```",
                "\n \nСгенерирован", 
                format(tm, " %d %b %Y"), 
                " \nСрок действия до ",
                format(tm + 600, "%H:%M:%S"), 
                "\nуральского часового пояса (GMT + 5)"
            )
        )
        rm(tmp)
    }
}

MessageFilters$flt_menu <- BaseFilter(function(message) {
    grepl(x = message$text, ignore.case = TRUE,
          pattern = 'menu|меню')
})

fun_menu <- function(bot, update){
    text <- "Вы вызвали меню"
    RKM <- ReplyKeyboardMarkup(
        keyboard = list(
            list(KeyboardButton("Вход в веб-приложение")), 
            list(KeyboardButton("Регистрация в проекте")),
            list(KeyboardButton("Обратиться в поддержку")),
            list(KeyboardButton("Статистика проекта"))
        ),
        resize_keyboard = TRUE, 
        one_time_keyboard = TRUE
    )
    bot$sendMessage(update$message$chat_id, text, reply_markup = RKM)
}

MessageFilters$flt_supp <- BaseFilter(function(message) {
    grepl(x = message$text, ignore.case = TRUE,
          pattern = 'supp|support|помощь|поддержка|техподдержка|Обратиться в поддержку')
})

fun_supp <- function(bot, update){
    bot$sendMessage(chat_id = update$message$chat_id, 
        reply_markup = ReplyKeyboardRemove(),
        text ="Извините, модуль автоматической техподдержки ещё находится в стадии разработки. 
Можете оставить обратную связь через <a href = 'https://sozontov.site/feedback'>эту форму</a>.", 
        parse_mode = "HTML")
}

MessageFilters$flt_stat <- BaseFilter(function(message) {
    grepl(x = message$text, ignore.case = TRUE,
          pattern = 'статистика|stats|stat|statistics|обзор|summary')
})

fun_stat <- function(bot, update){
    bot$sendMessage(chat_id = update$message$chat_id, 
                    reply_markup = ReplyKeyboardRemove(),
                    text = "Раздела со статистикой пока ещё нет... 
Но вы можете помочь с этим!
Связаться с разработчиками можно через <a href = 'https://sozontov.site/feedback'>форму обратной связи</a>.",
                    parse_mode = "HTML")
}

MessageFilters$flt_regi <- BaseFilter(function(message) {
    grepl(x = message$text, ignore.case = TRUE,
          pattern = 'Регистрация в проекте|регистрация|register')
})

fun_regi <- function(bot, update){ 
    usr_id <- update$message$from$id
    cc <- con()
    usr_status <- dbGetQuery(cc, paste0("select reg_stat from users where tlg_user_id = '", usr_id, "';"))[[1]]
    # dbDisconnect(cc)
    
    if(length(usr_status) == 0){
        # cc <- con()
        DBI::dbWriteTable(cc, "users", 
                          data.frame(
                              tlg_user_id = update$message$from$id,
                              tlg_name = update$message$from$first_name,
                              tlg_username = update$message$from[[4]],
                              name = NA,
                              reg_stat = 2,
                              hash = "",
                              hash_date = Sys.Date()
                              ),
                          append = TRUE, row.names = FALSE
        )
        dbDisconnect(cc)
        bot$sendMessage(chat_id = update$message$chat_id, 
                        reply_markup = ReplyKeyboardRemove(),
                        text = "Давайте начнем регистрацию!\n<a href = 'https://yandex.ru'>Пользовательское соглашение</a> принимаете?\n (да/нет)", 
                        parse_mode = 'HTML',
                        disable_web_page_preview = TRUE
        )
    } else if (usr_status == 1) {
        # cc <- con()
        dbGetQuery(cc, paste0("select name from users where tlg_user_id = '", usr_id, "';")) %>% 
            `[[`(1) %>% 
            paste0("Вы уже зарегистрированы под именем ", .) %>% 
            bot$sendMessage(chat_id = update$message$chat_id, text = ., reply_markup = ReplyKeyboardRemove())
        dbDisconnect(cc)
    } else { 
        bot$sendMessage(chat_id = update$message$chat_id, 
            text = "Извините, но вы не завершили начатую ранее регистрацию. \nМожет вернемся к этому?")
        bot$sendMessage(chat_id = update$message$chat_id, 
                        text = "В прошлый раз вы остановились на стадии...")
        if(usr_status == 2){
            bot$sendMessage(chat_id = update$message$chat_id, 
                            text = "<a href = 'https://www.google.com/search?q=%D1%81%D0%B8%D0%BC%D0%BF%D1%82%D0%BE%D0%BC%D1%8B+%D1%88%D0%B8%D0%B7%D0%BE%D1%84%D1%80%D0%B5%D0%BD%D0%B8%D0%B8&oq=%D1%81%D0%B8%D0%BC%D0%BF%D1%82%D0%BE%D0%BC%D1%8B+%D1%88%D0%B8%D0%B7%D0%BE%D1%84%D1%80%D0%B5%D0%BD%D0%B8%D0%B8&gs_lcrp=EgZjaHJvbWUyDAgAEEUYORixAxiABDIHCAEQABiABDIHCAIQABiABDIHCAMQABiABDIHCAQQABiABDIHCAUQABiABDIHCAYQABiABDIHCAcQABiABDIHCAgQABiABDIHCAkQABiABNIBCDU0ODdqMGo3qAIAsAIA&sourceid=chrome&ie=UTF-8'>Пользовательское соглашение</a>. Вы его принимаете? \n(да/нет)", 
                            parse_mode = 'HTML',
                            disable_web_page_preview = TRUE)
        } else if (usr_status == 3) { 
            bot$sendMessage(chat_id = update$message$chat_id, "Ввод имени. Напишите его, пожалуйста.", reply_markup = ReplyKeyboardRemove())
        } else {
            bot$sendMessage(chat_id = update$message$chat_id, text = "Непредвиденная ошибка. Сообщите разработчику: sozontov.site/feedback", reply_markup = ReplyKeyboardRemove())
        }
        dbDisconnect(cc)
    }
}

fun_text <- function(bot, update){
    usr_id <- update$message$from$id
    cc <- con()
    usr_status <- dbGetQuery(cc, paste0("select reg_stat from users where tlg_user_id = '", usr_id, "';"))[[1]]
    if(length(usr_status) == 0) {
        bot$sendMessage(chat_id = update$message$chat_id, 
                        reply_markup = ReplyKeyboardRemove(),
                        text = "Извините, но вы пока ещё не пользователь. Нажмите /register и поговорим как человек с машиной 😉")
        dbDisconnect(cc)
    } else if(usr_status == 1){
        bot$sendMessage(chat_id = update$message$chat_id, 
                        reply_markup = ReplyKeyboardRemove(),
                        text = "Извините, обрабатывать контент такого типа мне пока сложно🫣  \n попробуйте вызывать меню: /menu")
        dbDisconnect(cc)
    } else if(usr_status == 2){
        # ждем ответа по принятию соглашения
        if(update$message$text %in% c("yes", "Yes", "YES", "да", "Да", "ДА", "принимаю", "Принимаю", "ПРИНИМАЮ", "ага", "Ага", "АГА")){ ##update$callback_query$data == "yes" ||
            # Принял! Работаем, братья!)
            DBI::dbSendQuery(cc, paste0("UPDATE users SET reg_stat = 3 WHERE tlg_user_id = '", usr_id, "';"))
            bot$sendMessage(chat_id = update$from_chat_id(),
                            text = "Ваше согласие учтено👌")
            bot$sendMessage(chat_id = update$from_chat_id(),
                            text = "Пожалуйста, напишите как вас зовут🙃")
            dbDisconnect(cc)
            # FUTURE: вставить и другие опросы: пол, возраст, образование, место работы
        } else {
            # not accepted
            bot$sendMessage(chat_id = update$from_chat_id(),
                            text = "Ничего, может быть позже.. 🙄")
            dbDisconnect(cc)
        }
        
    } else if(usr_status == 3){
        # ждем введенного имени
        name_msg <- update$message$text
        if(nchar(name_msg)<3) {
            bot$sendMessage(chat_id = update$from_chat_id(), text = "Ответ слишком короткий, не могу такое принять \n🙄\nПопробуйте добавить фамилию")
            dbDisconnect(cc)
        } else if(nchar(name_msg)>20) {
            bot$sendMessage(chat_id = update$from_chat_id(), text = "Ответ слишком длинный, не могу такое принять \n🙄🙄🙄")
            dbDisconnect(cc)
        } else if(str_detect(name_msg, "[:punct:]")){
            bot$sendMessage(chat_id = update$from_chat_id(), text = "В ответе содержатся знаки препинания, не могу такое принять \n🚫")
            dbDisconnect(cc)
        } else if(str_detect(name_msg, "[:digit:]")){
            bot$sendMessage(chat_id = update$from_chat_id(), text = "В ответе содержатся цифры, не могу такое принять \n♨️️")
            dbDisconnect(cc)
        } else {
            #fine!
            DBI::dbSendQuery(cc, paste0("UPDATE users SET name = '", name_msg, "', reg_stat = 1 WHERE tlg_user_id = '", usr_id, "';"))
            dbDisconnect(cc)
            bot$sendMessage(chat_id = update$message$chat_id, 
                            text = paste0("Приятно познакомиться, ", name_msg, "!\n 🤗\nПоздравляю с благополучной регистрацией!")
            )
        }
    }
}

fun_other <- function(bot, update){
    bot$sendMessage(chat_id = update$message$chat_id, 
                        reply_markup = ReplyKeyboardRemove(),
                        text = "Извините, обрабатывать контент такого типа мне пока сложно🫣 \n попробуйте вызывать меню: /menu")
    }

updater <- Updater(token = scan("/var/sec/rnf_bot.tok", what = "", quiet = TRUE)) + 
    CommandHandler("start", fun_start)+ # start_handler
    # Authentication C&M hundlers
    CommandHandler("auth", fun_auth) +
    MessageHandler(fun_auth, 
        filters = MessageFilters$flt_auth) +
    # Menu C&M handlers
    CommandHandler("menu", fun_menu) +
    MessageHandler(fun_menu, 
        filters = MessageFilters$flt_menu) +
    # Stats С&M handlers
    CommandHandler("stats", fun_stat) +
    MessageHandler(fun_stat, filters = MessageFilters$flt_stat) +
    # Support С&M handlers
    CommandHandler("support", fun_supp) +
    MessageHandler(fun_supp, filters = MessageFilters$flt_supp) +
    # Registration
    CommandHandler("register", fun_regi) + 
    MessageHandler(fun_regi, filters = MessageFilters$flt_regi) +
    # media & other content 
    MessageHandler(fun_other, MessageFilters$sticker |
                   MessageFilters$audio | MessageFilters$document | 
                   MessageFilters$photo | MessageFilters$video | 
                   MessageFilters$contact | MessageFilters$location) + 
    # (almost) all text messages
    MessageHandler(fun_text, MessageFilters$text) 

updater$start_polling()
# Отладка -----------------------------------------------------------------
# bot$clean_updates()
# update <- bot$get_updates()[[1]]

