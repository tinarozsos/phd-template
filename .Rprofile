source("templates.R")

if(interactive()){
  shell("cls")
  cat("\014")
  instl <- utils::installed.packages()
  if("cli" %in% instl){
    
    thick_bar <- function(){
      colour <- grDevices::colorRampPalette(c("#E40303", "#FF8C00", "#FFED00", "#008026", "#24408E", "#732982"))(40)
      
      invisible(
        lapply(colour, cli::make_ansi_style) |>
          lapply(\(x){
            cat(x(paste0(rep("\u2583", 1), collapse = "")))
          })
      )
    }
    
    mood <- function() {
      x <- c("¯\\_(ツ)_/¯", "[¬º-°]¬", "(♥_♥)", "(っ◕‿◕)っ", "(ง •̀_•́)ง",
             "=^..^=", "ಠ_ಠ", "(✿◠‿◠)", "~(^._.^)",
             "(^_^)", "(^.^)", "<3", ":)", ":D", "◔ᴗ◔", "◔̯◔", "(◣_◢)",  
             "✈️", "✨", "✨✨", "❓", "⭐", "🌈", 
             "🌟", "🌤", "🌷", "🌸", "🌵", "🌻", "🍉", "🎉", "🎯", "🐋",
             "🐌", "🐔", "🐬", "🐮", "👻", "👽", "💕", "💙", "💥", "🤡",
             "😂", "😄", "😍", "😎", "😴", "😸", "😽", "😾", "🦆")
      sample(x, 1)
    }
    
    thick_bar()
    cat("\n\n")
    cli::cli_text("\n{R.version.string} -- {R.version$nickname}")
    cli::cli_text("Running under {utils::osVersion}")
    cli::cli_text("System time is {format(Sys.time(), '%F %T')}; system mood is {mood()}")
    thick_bar()
  }
  cat("\n")
  if("praise" %in% instl){
    cat(praise::praise("\n${Exclamation}, you are ${adjective}!"))
  }
  rm(instl, thick_bar, mood)
}
