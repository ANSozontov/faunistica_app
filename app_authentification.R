# initial -----------------------------------------------------------------
# library(shiny)
# library(shinyalert)
library(telegram.bot)
# library(RPostgreSQL)

con_config <- list(database = "rdatabase",
                   hostname = "localhost",
                   dsn_port = "5432",
                   dsn_uid  = "fau_bot",
                   mypass   = scan("/var/mypass",
                                   what = "", nlines = 1, quiet = TRUE))
con <- dbConnect(dbDriver("PostgreSQL"),
                 dbname = con_config$database,
                 host = con_config$hostname,
                 port = con_config$dsn_port,
                 user = con_config$dsn_uid,
                 password = con_config$mypass)

# tlg date to regular format
# as.POSIXct(update[[i]]$message$date, origin="1970-01-01") 


users <- rbind(users, data.frame(tlg_user_id = 276388547, 
                          name = "Ans",
                          hash = "",
                          hash_date = NA))

# bot3 --------------------------------------------------------------------
user_name = FALSE
users <- data.frame(tlg_user_id = NA, 
                    name = "t_b2b9eb4",
                    hash = "",
                    hash_date = Sys.time())
bot <- Bot(token = scan("/var/mytoken", what = "", quiet = TRUE))
# hash <- character()


fun_start <- function(bot, update){
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = paste0('Здравствуйте, ', 
                                  update$message$from$first_name, '
Я - телеграм-бот проекта <a href="https://ru.wikipedia.org/wiki/Рикроллинг">Faunistica 2.0</a>. 
Очень рад, что Вы заинтересовались <i><b>нашим проектом</b></i>. 
Чем могу вам помочь?'),
                    parse_mode = 'HTML',
                    disable_web_page_preview = TRUE)
}

fun_other <- function(bot, update){
    bot$sendMessage(chat_id = update$message$chat_id, 
        text = "Извините, обрабатывать контент такого типа мне пока сложно🫣")
}

MessageFilters$flt_auth <- BaseFilter(function(message) {
    grepl(x = message$text, ignore.case = TRUE,
          pattern = 'auth|Вход в веб-приложение')
})

fun_auth <- function(bot, update){
    tlg_usr_id <- update$message$from$id
    if(!(tlg_usr_id %in% users$tlg_user_id)) {
        bot$sendMessage(chat_id = update$message$chat_id, 
            text = "Вас нет среди зарегистрированных пользователей")
    } else{
        tmp <- paste0(sample(c(0:9, LETTERS), 6, replace = TRUE), collapse = "")
        users$hash[users$tlg_user_id == tlg_usr_id] <<- cli::hash_md5(tmp)
        users$hash_date[users$tlg_user_id == tlg_usr_id] <<- Sys.time()
        bot$sendMessage(chat_id = update$message$chat_id,
                    text = paste0("Ваш код доcтупа: ```", tmp, "```"), 
                    parse_mode = 'Markdown')
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
        text ="Да батюшки! Помощь потребовалась? Так её ж нет пока ещё...")
}

MessageFilters$flt_regi <- BaseFilter(function(message) {
    grepl(x = message$text, ignore.case = TRUE,
          pattern = 'Регистрация в проекте|регистрация|register')
})

fun_regi <- function(bot, update){ 
    tlg_usr_id <- update$message$from$id
    # bot$sendMessage(chat_id = update$message$chat_id, 
    #                 text = paste0("your id is ", tlg_usr_id))
    if(tlg_usr_id %in% users$tlg_user_id){
        bot$sendMessage(chat_id = update$message$chat_id, 
            text = paste0("Вы уже зарегистрированы под именем ", 
                users$name[users$tlg_user_id == tlg_usr_id])
        )
    } else {
        IKM <- InlineKeyboardMarkup(
            inline_keyboard = list(
                list(
                    InlineKeyboardButton("✅ Принимаю", callback_data = 'yes'),
                    InlineKeyboardButton("❌ Отмена", callback_data = 'no')
                )
            )
        )
        bot$sendMessage(chat_id = update$message$chat_id, 
                        text = "Добро пожаловать!
<a href = 'https://yandex.ru'>Пользовательское соглашение</a> принимаете?", 
                        parse_mode = 'HTML',
                        disable_web_page_preview = TRUE,
                        reply_markup = IKM)
        
    }
}

answer_cb <- function(bot, update) {
    # полученные данные с кнопки
    data <- update$callback_query$data
    
    # обработка результата
    if ( data == 'no' ) {
        msg <- "Ничего, может быть позже.. 🙄"
    } else {
        user_name <<- TRUE
        msg <- "Спасибо. Введите своё имя, пожалуйста"
    }
    
    # Отправка сообщения
    bot$sendMessage(chat_id = update$from_chat_id(),
                    text = msg)
    
    # сообщаем боту, что запрос с кнопки принят
    bot$answerCallbackQuery(callback_query_id = update$callback_query$id)
}

fun_echo <- function(bot, update){
    if(user_name){ 
        users <<- rbind(users, data.frame(
            tlg_user_id = update$message$from$id, 
            name = update$message$text, 
            hash = "", 
            hash_date = NA))
        user_name <<- FALSE
        bot$sendMessage(chat_id = update$message$chat_id, 
            text = "Приятно познакомиться 🤗
Поздравляю с благополучной регистрацией!")
    } else {
        bot$sendMessage(chat_id = update$message$chat_id, 
                        text = paste0("Что вы сказали?\n", update$message$text, "? 
Такие команды я пока не научился понимать😒")
        )
    }
}

updater <- Updater(token = scan("/var/mytoken", what = "", quiet = TRUE)) + 
    CommandHandler("start", fun_start)+ # start_handler
    # other content
    MessageHandler(fun_other, MessageFilters$sticker |
                       MessageFilters$audio | MessageFilters$document | 
                       MessageFilters$photo | MessageFilters$video | 
                       MessageFilters$contact | MessageFilters$location) +
    # Authentication C&M hundlers
    CommandHandler("auth", fun_auth) +
    MessageHandler(fun_auth, 
        filters = MessageFilters$flt_auth) +
    # Menu C&M handlers
    CommandHandler("menu", fun_menu) +
    MessageHandler(fun_menu, 
        filters = MessageFilters$flt_menu) +
    # Support С&M handlers
    CommandHandler("support", fun_supp) +
    MessageHandler(fun_supp, filters = MessageFilters$flt_supp) +
    CommandHandler("register", fun_regi) + 
    MessageHandler(fun_regi, filters = MessageFilters$flt_regi) +
    CallbackQueryHandler(answer_cb) + 
    MessageHandler(fun_echo, filters = MessageFilters$text)

updater$start_polling()
bot$clean_updates()
update <- bot$get_updates()
update <- update[[1]]
