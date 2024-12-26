
## 👦 🐵 👾 👽 🐥  👤 🐧 🚶 👻  💎


# cat("\U1F480")  ## 💀
# cat("\U1F47D")  ## 👽
# cat("\U1F680")  ## 🚀
# cat("\U1F47B")  ## 👻
# cat("\U1F477")  ## 👷 
# cat("\U1F546")  🕆
# cat("\U1F9D1")  ## 🧑
# cat("\U1F383")  ## 🎃
# cat("\U1F571")  🕱
# cat("\U1F989")  ## 🦉
# cat("\U1F333")  ## 🌳
# cat("\U1F332")  ## 🌲
# cat("\U1F7E4")  ##🟤
# cat("\UE0020")  ## space
# cat("\UE002E")  ## space

# cat(rep("\U1F383",10))
# cat(rep("\U1F480",10))
# cat(rep("\U1F333",10))
# cat(rep("xx",10))

## 🌳🌳🌳🌳🌳🌳🌳🌳🌳🌳🌳🌳🌳🌳🌳🌳🌳🌳🌳
## 💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀💀
## 👷👷👷👷👷👷👷👷👷👷👷👷👷👷👷👷👷👷👷👷
## 🟤🟤🟤🟤🟤🟤🟤🟤🟤🟤🟤🟤🟤🟤🟤🟤🟤🟤🟤🟤
## 🎃🎃🎃🎃🎃🎃🎃🎃🎃🎃🎃🎃🎃🎃🎃🎃🎃🎃🎃🎃󠀠󠀠󠀠󠀠󠀠
##                                         
## _+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
## ⊗⊗⊗⊗⊗⊗⊗⊗⊗⊗⊗⊗⊗⊗⊗⊗⊗⊗⊗⊗⊗⊗⊗⊗
## Creepy Computer Games
## Reynold, Colin and McCaig, Rob, Creepy Computer Games (Usborne, London).
## https://archive.org/details/Creepy_Computer_Games_1983_Usborne_Publishing/
## Gravedigger by Alan Ramsey

stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

start_screen <- function() { 
  
  str <- paste0(
    paste(rep("\U1F480", 20), collapse = ""), "\n",
    "           C R E E P Y \U1F47D A L I E N  ", "\n",
    paste(rep("\U1F480", 20), collapse = ""), "\n",
    "\n",
    "    Help the alien to find his rocket\n",
    "         Beware of the skeletons!\n",
    "\n",
    "  Inspired by 'Gravedigger', written 1983\n",
    "      by Alan Ramsay for ZXSpectrum\n",
    "                   \n"
  )
  
  cat("\014")  ## clear screene
  cat(str)     ## show intro
  inp <- readline("[S]tart | [H]elp | [Q]uit : ")
  inp <- toupper(inp)
  if (inp == "Q") { stop_quietly() }
  inp
}

help_screen <- function() { 
  
  str <- paste0(
    paste(rep("\U1F480", 20), collapse = ""), "\n",
    "You are an alien lost in a graveyard and have \n", 
    "until midnight to find your way to your rocket\n", 
    "Skeletons are waiting to scare you to death  \n",
    "should you come to close. You can dig up to 5\n", 
    "holes to help keep them away. And do not fall\n",
    "down the holes. Move [N]orth, [S]outh, [E]ast\n", 
    "or [W]est. See if you can escape!            \n",
    paste(rep("\U1F480", 20), collapse = ""), "\n"
  )
  
  cat("\014")  ## clear screene
  cat(str)     ## show intro
  inp <- readline("Press <ENTER> to start :")
  inp <- toupper(inp)
  if (inp == "Q") { stop_quietly() }
  inp
}


run <- function() {
  
  inp <- start_screen()
  if (inp == "H") { help_screen() }
  
  A <- matrix(ncol = 20, nrow = 10)
  A[, ] <- " "
  
  ## Starting variables
  W <- 0 # Move number
  X <- 5 # Remaining holes
  death <- 0 # Game over?
  
  ## Initiate pieces
  chr_Y <- "\U1F47D" # "\U1F477" ## "*"
  chr_B <- "_+" ## "+"
  chr_C <- "()" ## "O" 
  chr_D <- "::" ## ":"
  chr_E <- "\U1F480" ## "X"
  chr_Z <- "  " ## " "
  chr_R <- "\U1F680"
  
  Y <- "*"
  B <- "+"
  C <- "O" 
  D <- ":"
  E <- "X"
  Z <- " "
  R <- ">"
  
  
  ## Draw board
  ## Add borders
  A[c(1, 10), ] <- D
  A[, 1] <- D
  A[1:8, 20] <- D
  A[9, 20] <- R
  ## Add graves
  for (i in 1:20){
    A[floor(runif(1) * 7 + 2), floor(runif(1) * 15 + 3)] <- B
  }
  
  ## Starting positions
  ## Player
  M <- 2
  N <- 2
  A[N, M] <- Y
  ## Skeletons
  S <- c(4, 19, 3, 19, 2, 19)
  last_command <- ""
  
  ## Game play
  repeat{    
    ## Position skeletons
    for (J in seq(1, 5, by = 2)) {
      A[S[J], S[J + 1]] <- E
    }
    W <- W + 1 ## Move counter
    if (W > 60) {
      print("The clock's struck midnight")
      print("Aghhhhh!!!!")
      break
    }
    ## Print board
    cat("\014")  ## clear screene
    v <- paste(as.vector(t(A)), collapse = "")        
    for (i in 1:10) {
      str <- substr(v, (i - 1) * 20 + 1, (i - 1) * 20 + 20)
      str_chr <- vector()
      for (ii in 1:20) {
        if (substr(str, ii, ii) == Y) { str_chr <- c(str_chr, chr_Y) }
        if (substr(str, ii, ii) == B) { str_chr <- c(str_chr, chr_B) }
        if (substr(str, ii, ii) == C) { str_chr <- c(str_chr, chr_C) }
        if (substr(str, ii, ii) == D) { str_chr <- c(str_chr, chr_D) }
        if (substr(str, ii, ii) == E) { str_chr <- c(str_chr, chr_E) }
        if (substr(str, ii, ii) == Z) { str_chr <- c(str_chr, chr_Z) }
        if (substr(str, ii, ii) == R) { str_chr <- c(str_chr, chr_R) }
      }
      cat(paste0(str_chr, collapse = ""), "\n")
    }
    ## Enter move
    A1 <- toupper(readline(paste0("Move ", W, ": Go [N],[S],[E],[W] or [Q]uit: ")))
    if (A1 == "") { 
      A1 <- last_command
    } else {
      last_command <- A1
    }
    ## Move player
    T <- N
    U <- M
    
    if (A1 == "EXIT" | A1 == "QUIT" | A1 == "Q") {
      break
    }
    if (A1 == "N") {
      T <- N - 1
    } else if (A1 == "E") {
      U <- M + 1
    } else if (A1 == "S") {
      T <- N + 1
    } else if (A1 == "W") {
      U <- M - 1
    }
    
    ## Collision detection
    if (A[T, U] == D | A[T, U] == B) { # Edge or grave
      
      cat("That way's blocked\n")
      beepr::beep(sound=10)
      Sys.sleep(1)
    } else if (A[T, U] == C) { # Hole
      beepr::beep(sound=9)
      cat("You've fallen into the hole\n")
      break
    } else if (A[T, U] == E) { # Skeleton
      death <- 1
    } else if (T == 9 & U == 20) { # Escaped
      beepr::beep(sound=3)
      cat("You're free!\n")
      cat(paste0("Your performance rating is ",
                 floor((60 - W) / 60 * (96 + X)), "%"))
      break
    } else if (death == 1) {
      beepr::beep(sound=9)
      cat("You've been scared to death by a skeleton\n")
      break
    } else if (A[T, U] == Z) { # Player can move
      ## Move player and dig hole
      A [N, M] <- Z
      if (X != 0) {
        B1 <- toupper(readline("Dig a hole Y or [N] : "))
        if (B1 == "Y") {
          X <- X - 1
          A[N, M] <- C
        }
      }
      N <- T
      M <- U
      A[T, U] <- Y
      ## Move skeletons
      for (J in seq(1, 5, by = 2)) {
        ## Store skeleton position in temp variable
        P <- S[J]
        Q <- S[J + 1]
        if (any(c(A[P + 1, Q], A[P - 1 , Q], A[P, Q - 1], A[P, Q + 1]) == Y)) {
          death <- 1
        } else
        {
          ## Move skeletons
          if (A1 == "S" & A[P + 1, Q] == Z){
            S[J] <- S[J] + 1 # Follow player
            A[P, Q] <- Z
          } else if (A1 == "N" & A[P - 1, Q] == Z){
            S[J] <- S[J] - 1 # Follow player
            A[P, Q] <- Z
          } else if (A1 == "E" & A[P, Q - 1] == Z & M < Q){
            S[J + 1] <- S[J + 1] - 1 # Move towards player
            A[P, Q] <- Z
          } else if (A1 == "E" & A[P, Q + 1] == Z & M > Q) {
            S[J + 1] <- S[J + 1] + 1 # Reverse direction
            A[P, Q] <- Z
          }
        }
      }
    }
  }
  
} ## run