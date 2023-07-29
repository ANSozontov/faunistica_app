library(shiny)
library(shinyalert)
library(telegram.bot)

users <- data.frame(tlg_user_id = numeric(), name = character())
hash <- character()
bot <- Bot(token = scan("/var/mytoken", what = "", quiet = TRUE))
# manual bot --------------------------------------------------------------

echo <- function(bot, update){
    bot$sendMessage(chat_id = update$message$chat_id, text = update$message$text)
}

start_handler <- CommandHandler("start", function(bot, update){
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = sprintf("Здравствуйте, %s! 
Я - телеграм-бот проекта Faunistica 2.0. 
Очень рад, что Вы заинтересовались нашим проектом. 
Чем могу вам помочь?", update$message$from$first_name))
})

about_handler <- CommandHandler("about", function(bot, update){
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = "Это проект Faunistica 2.0. 
Очень рад, что Вы заинтересовались нашим проектом.", 
    )
})

# inline_handler <- CommandHandler('inline', function(bot, update) {
#     
#     text <- "Yes or no?"
#     IKM <- InlineKeyboardMarkup(
#         inline_keyboard = list(
#             list(
#                 InlineKeyboardButton("Yes", callback_data = 'yes'),
#                 InlineKeyboardButton("No", callback_data = 'no'),
#                 InlineKeyboardButton("Войти", callback_data = 'in'),
#                 InlineKeyboardButton("Зарегистрироваться_a", callback_data = '/about'),
#                 InlineKeyboardButton("Техподдержка_s", callback_data = '/start'),
#                 InlineKeyboardButton("Статистика", callback_data = 'omg')
#             )
#         )
#     )
#     
#     # Send Inline Keyboard
#     bot$sendMessage(update$message$chat_id, text, reply_markup = IKM)
# })

callback_handler <- CallbackQueryHandler(function(bot, update) {
    
    data <- update$callback_query$data
    if(data == "yes") {
        bot$sendMessage(chat_id = update$callback_query$message$chat$id, 
                        text = paste0("Yes!"))
        bot$answerCallbackQuery(callback_query_id = update$callback_query$id,
                                text = "you selected yes", data)
    } else {
    
    # Send Custom Keyboard
    bot$sendMessage(chat_id = update$callback_query$message$chat$id, 
                    text = paste0("Hello"))
    
    
    bot$answerCallbackQuery(callback_query_id = update$callback_query$id,
                            text = paste("Answer recorded:", data))
    }
}
)

inline_handler <- CommandHandler('menu', function(bot, update) {
    
    text <- "Yes or no?"
    RKM <- ReplyKeyboardMarkup(
        keyboard = list(
            list(
                KeyboardButton("/auth"), #, callback_data = 'yes'),
                KeyboardButton("No"), # callback_data = 'no'),
                KeyboardButton("Войти") #callback_data = 'in')
            ), list(
                KeyboardButton("Зарег"), #callback_data = '/about'),
                KeyboardButton("Техпо"), #callback_data = '/start'),
                KeyboardButton("Стати") # callback_data = 'omg')
            )
        ), 
        resize_keyboard = TRUE, 
        one_time_keyboard = TRUE
    )
    
    # Send Inline Keyboard
    bot$sendMessage(update$message$chat_id, text, reply_markup = RKM)
})

updater <- Updater(token = scan("/var/mytoken", what = "", quiet = TRUE))

updater <- updater + 
    start_handler + 
    about_handler + 
    callback_handler+
    # CallbackQueryHandler(answer_cb) +
    inline_handler +
    MessageHandler(echo, MessageFilters$text)

updater$start_polling()
updater

# new simple bot ----------------------------------------------------------
start_handler <- CommandHandler("start", function(bot, update){
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = paste0('Здравствуйте, ', 
                                  update$message$from$first_name, '
Я - телеграм-бот проекта <a href="https://ru.wikipedia.org/wiki/Рикроллинг">Faunistica 2.0</a>. 
Очень рад, что Вы заинтересовались <b>нашим проектом</b>. 
Чем могу вам помочь?'),
                    parse_mode = 'HTML',
                    disable_web_page_preview = TRUE)
})

auth_handler <- CommandHandler("auth", function(bot, update){
    tmp <- paste0(sample(c(0:9, LETTERS), 6, replace = TRUE), collapse = "")
    hash <<- c(hash, cli::hash_md5(tmp))
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = paste0("Ваш код доcтупа: ```", tmp, "```"), 
                    parse_mode = 'Markdown')
})

menu_handler <- CommandHandler('menu', function(bot, update) {
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
    # Send Inline Keyboard
    bot$sendMessage(update$message$chat_id, text, reply_markup = RKM)
})

echo <- function(bot, update){
    if(update$message$text == "Вход в веб-приложение"){ 
        tmp <- paste0(sample(c(0:9, LETTERS), 6, replace = TRUE), collapse = "")
        hash <<- c(hash, cli::hash_md5(tmp))
        bot$sendMessage(chat_id = update$message$chat_id,
                        text = paste0("Ваш код доcтупа: ```", tmp, "```"), 
                        parse_mode = 'Markdown',
                        reply_markup = ReplyKeyboardRemove()
        )
    } else if(update$message$text == "Обратиться в поддержку"){
        bot$sendMessage(chat_id = update$message$chat_id, 
            reply_markup = ReplyKeyboardRemove(),
            text ="Да батюшки! В поддержку? Так её ж нет пока ещё...")
    } else if(update$message$text == "Регистрация в проекте") {
        
    } else {
        bot$sendMessage(chat_id = update$message$chat_id, 
            text = paste0("Что вы сказали?\n", update$message$text, "? 
Такие команды я пока не научился понимать😒")
        )
    }
}

updater <- Updater(token = scan("/var/mytoken", what = "", quiet = TRUE))

updater <- updater + 
    menu_handler +
    start_handler +
    auth_handler +
    # callback_handler+
    # # CallbackQueryHandler(answer_cb) +
    # inline_handler +
    MessageHandler(echo, MessageFilters$text)

updater$start_polling()
updater

