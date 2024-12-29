## Inspired by Gravedigger (written by Alan Ramsey)
## Listing published 1983 in 'Creepy Computer Games'
## https://archive.org/details/Creepy_Computer_Games_1983_Usborne_Publishing/

stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

clear_screen <- function(animate = TRUE) {  
  if (animate) {
    cat("\014")
  } else {
    cat("\n")
  }
}

play_sound <- function(nr, sound = TRUE)  {
  if (sound) {
     beepr::beep(sound = nr)
  }
}

def_character <- function() {
  
  ## define character (internal)
  chr <- list(
    alien = "*",  # Y
    grave = "+",  # B
    hole  = "O",  # C
    wall  = ":",  # D
    enemy = "X",  # E
    space = " ",  # Z
    ship  = ">",  # R
    owl   = "B"   
  )
  
  ## define screen symbol
  sym <- list(
    alien = "\U1F47D",  # chr_Y "*"  
    grave = "_+",       # chr_B "+"
    hole  = "()",       # chr_C "O" 
    wall  = "::",       # chr_D ":"
    enemy = "\U1F480",  # chr_E "X"
    space = "  ",       # chr_Z " "
    ship  = "\U1F6F8",  # chr_R ">"
    owl   = "\U1F989"
  )
  
  ## return as list
  def <- list(chr = chr, sym = sym)
  def
  
} 

start_screen <- function(def, animate = TRUE, sound = TRUE) { 
  
  ## define start screen
  str <- paste0(
    paste(rep(def$sym$enemy, 20), collapse = ""), "\n",
    "\n",
    "          C R E E P Y {alien} A L I E N   \n",
    "{ship}\n",
    "     Help the alien to find his ship      \n",
    "         Beware of the skeletons!         \n",
    "                                          \n",
    "  Inspired by 'Gravedigger', written 1983 \n",
    "\n",
    paste(rep(def$sym$enemy, 20), collapse = ""), "\n",
    "\n"
  )
  
  ## if animate, let ufo fly in from left side
  if (animate) {
    alien <- "  "
    for (i in 0:22) {
      clear_screen(animate)
      ship <- paste(rep(" ", i), collapse = "")
      ship <- paste0(ship, def$sym$ship)
      cat(glue::glue(str))     ## show intro
      Sys.sleep(0.05)
    }
    alien <- def$sym$alien
    clear_screen(animate)
    cat(glue::glue(str))     ## show intro
    Sys.sleep(1)
  }
  
  ## show start screen (including alien & ufo)
  play_sound(5, sound)
  clear_screen(animate)
  ship <- paste(rep(" ", 22), collapse = "")
  ship <- paste0(ship, def$sym$ship)
  alien <- def$sym$alien
  cat(cli::col_red(glue::glue(str)))     ## show intro

  ## user input
  inp <- readline("[S]tart | [H]elp | [Q]uit : ")
  inp <- toupper(inp)

  ## return user input (upper case)
  inp
}

help_screen <- function(def, animate = TRUE, sound = TRUE) { 
  
  ## define help screen
  str <- paste0(
    paste(rep(def$sym$enemy, 20), collapse = ""), "\n",
    "You are an alien lost in a graveyard and  \n", 
    "have until midnight to find your way to   \n", 
    "your ship. Skeletons are waiting to scare \n",
    "you to death should you come to close.    \n", 
    "You can dig up to 5 holes to help keep    \n",
    "them away. And do not fall down the holes.\n", 
    "Move [N]orth, [S]outh, [E]ast or [W]est. \n",
    "See if you can escape (in 60 moves)!\n",
    paste(rep(def$sym$enemy, 20), collapse = ""), "\n"
  )
  
  ## show help screen
  clear_screen(animate)
  cat(cli::col_red(str))     ## show intro
  
  ## get user input
  inp <- readline("Press <ENTER> to start :")
  inp <- toupper(inp)

  # return user input (upper case)
  inp
}

refresh_screen <- function(A, def = def, animate = animate) {
  
  ## define symbols
  # Y <- "*"  # alien
  # B <- "+"  # grave
  # C <- "O"  # hole
  # D <- ":"  # wall
  # E <- "X"  # enemy
  # Z <- " "  # space
  # R <- ">"  # rocket
  
  ## Print board
  clear_screen(animate)
  v <- paste(as.vector(t(A)), collapse = "")        
  for (i in 1:10) {
    str <- substr(v, (i - 1) * 20 + 1, (i - 1) * 20 + 20)
    str_chr <- vector()
    for (ii in 1:20) {
      if      (substr(str, ii, ii) == def$chr$wall)  { str_chr <- c(str_chr, def$sym$wall) }
      else if (substr(str, ii, ii) == def$chr$space) { str_chr <- c(str_chr, def$sym$space) }
      else if (substr(str, ii, ii) == def$chr$alien) { str_chr <- c(str_chr, def$sym$alien) }
      else if (substr(str, ii, ii) == def$chr$grave) { str_chr <- c(str_chr, def$sym$grave) }
      else if (substr(str, ii, ii) == def$chr$hole)  { str_chr <- c(str_chr, def$sym$hole) }
      else if (substr(str, ii, ii) == def$chr$enemy) { str_chr <- c(str_chr, def$sym$enemy) }
      else if (substr(str, ii, ii) == def$chr$ship)  { str_chr <- c(str_chr, def$sym$ship) }
      else if (substr(str, ii, ii) == def$chr$owl)   { str_chr <- c(str_chr, def$sym$owl) }
    }
    cat(paste0(str_chr, collapse = ""), "\n")
  }
  
} # refresh_screen

fly_ufo <- function(A, def, animate = TRUE)  {
  
  ## Alien enters ufo
  A[9,19] <- " "
  refresh_screen(A, def, animate)
  Sys.sleep(0.2)
  
  ## Alien flies away
  for(i in 1:9)  {
    
    A[10-i, 20] <- def$chr$wall  # ":"  
    A[10-i-1, 20] <- def$chr$ship # ">"
    refresh_screen(A, def, animate)
    Sys.sleep(0.2)
  }
  
} # fly_ufo

#' Play creepyalien in the R-console
#'
#' @param animate Animated CLI graphics? (TRUE|FALSE)
#' @param sound Play sound? (TRUE|FALSE)
#' @return Nothing  
#' @export
#' @examples
#' ## Start game (in interactive R sessions)
#' if (interactive())  {
#'    creepyalien()
#' }
creepyalien <- function(animate = TRUE, sound = TRUE) {
  
  ## define characters used in game
  def <- def_character()
  
  ## start & help screen
  inp <- start_screen(def, animate, sound)
  if (inp == "Q") { return("You Quit!") }
  
  if (inp == "H") { 
    inp <- help_screen(def, animate, sound) 
    if (inp == "Q") { return("You Quit!") }
  }
  
  ## define game screen (20 x 10)
  A <- matrix(ncol = 20, nrow = 10)
  A[, ] <- " "
  
  ## Starting variables
  W <- 0       # Move number
  X <- 5       # Remaining holes
  death <- 0   # Game over?
  
  # ## define symbols
  # Y <- "*"  # alien
  # B <- "+"  # grave
  # C <- "O"  # hole
  # D <- ":"  # wall
  # E <- "X"  # enemy
  # Z <- " "  # space
  # R <- ">"  # rocket
  
  ## Draw board
  ## Add borders
  A[c(1, 10), ] <- def$chr$wall  # top & bottom wall
  A[, 1] <- def$chr$wall         # left wall
  A[1:8, 20] <- def$chr$wall     # right wall
  A[9, 20] <- def$chr$ship       # rocket/ship
  
  ## Add graves
  for (i in 1:20){
    A[floor(runif(1) * 7 + 2), floor(runif(1) * 15 + 3)] <- def$chr$grave
  }
  
  ## Add owl
  A[5,10] <- def$chr$owl
  
  ## Starting positions
  ## Player
  M <- 2
  N <- 2
  A[N, M] <- def$chr$alien
  ## Skeletons
  S <- c(4, 19, 3, 19, 2, 19)
  last_command <- ""
  
  play_sound(6, sound)
  
  ## Game play
  repeat{    
    ## Position skeletons
    for (J in seq(1, 5, by = 2)) {
      A[S[J], S[J + 1]] <- def$chr$enemy
    }
    W <- W + 1 ## Move counter
    if (W > 60) {
      play_sound(9, sound)
      cat("The clock's struck midnight\n")
      break
    }
    
    ## Draw game screen
    refresh_screen(A, def, animate)
    
    ## Enter move
    A1 <- toupper(readline(paste0("Moves ", 60-W+1, ": Go [N],[S],[E],[W] or [Q]uit: ")))
    if (A1 == "Q") { return("You Quit!") }
    if (A1 == "") { 
      A1 <- last_command
    } else {
      last_command <- A1
    }
    ## Move player
    T <- N   ## N = old x position, T = new x position 
    U <- M   ## M = old y position, U = new y position
    
    if (A1 == "EXIT" | A1 == "QUIT" | A1 == "Q") {
      break
    }
    if (A1 == "N" | A1 == "NORD") {
      T <- N - 1
    } else if (A1 == "E"| A1 == "EAST") {
      U <- M + 1
    } else if (A1 == "S"| A1 == "SOUTH") {
      T <- N + 1
    } else if (A1 == "W"| A1 == "WEST") {
      U <- M - 1
    }
    
    ## Collision detection
    if (A[T, U] == def$chr$wall | A[T, U] == def$chr$grave) { # Edge or grave
      
      cat("That way's blocked\n")
      play_sound(10, sound)
      Sys.sleep(1)
    } else if (A[T, U] == def$chr$hole) { # Hole
      play_sound(9, sound)
      A[N, M] <- def$chr$space
      A[T, U] <- def$chr$hole
      if (animate) {
        Sys.sleep(0.5)
        A[T, U] <- def$chr$alien
        refresh_screen(A, def, animate)
        Sys.sleep(0.5)
        A[T, U] <- def$chr$hole
      }
      refresh_screen(A, def, animate)
      cat("You've fallen into the hole\n")
      break
    } else if (A[T, U] == def$chr$enemy) { # Skeleton/Enemy
      death <- 1
    } else if (T == 9 & U == 20) { # Escaped
      play_sound(3, sound)
      if (animate) {
        fly_ufo(A, def, animate)
      }
      #cat(paste0("Your performance rating is ",
      #           floor((60 - W) / 60 * (96 + X)), "%"), "\n")
      cat("Great, you made it!\n")
      break
    } else if (death == 1) {
      play_sound(9, sound)
      cat("You've been scared to death by a skeleton\n")
      break
    } else if (A[T, U] == def$chr$owl) { # Owl
      A [N, M] <- def$chr$space
      N <- T
      M <- U
      A[T, U] <- def$chr$alien
      play_sound(2, sound)
      readline("You get extra time: press <ENTER> ")
      W <- 0  # reset to 0 moves
    } else if (A[T, U] == def$chr$space) { # Player can move
      ## Move player and ask if dig hole
      A [N, M] <- def$chr$space
      if (X != 0) {
        B1 <- toupper(readline("Dig a hole [Y] or [N] : (Enter = N) "))
        if (B1 == "Q") { return("You Quit!") }
        if (B1 == "Y") {
          X <- X - 1
          A[N, M] <- def$chr$hole
        }
      }
      N <- T
      M <- U
      A[T, U] <- def$chr$alien
      ## Move skeletons
      for (J in seq(1, 5, by = 2)) {
        ## Store skeleton position in temp variable
        P <- S[J]
        Q <- S[J + 1]
        if (any(c(A[P + 1, Q], A[P - 1 , Q], A[P, Q - 1], A[P, Q + 1]) == def$chr$alien)) {
          death <- 1
        } else
        {
          ## Move skeletons
          if (A1 == "S" & A[P + 1, Q] == def$chr$space){
            S[J] <- S[J] + 1 # Follow player
            A[P, Q] <- def$chr$space
          } else if (A1 == "N" & A[P - 1, Q] == def$chr$space){
            S[J] <- S[J] - 1 # Follow player
            A[P, Q] <- def$chr$space
          } else if (A1 == "E" & A[P, Q - 1] == def$chr$space & M < Q){
            S[J + 1] <- S[J + 1] - 1 # Move towards player
            A[P, Q] <- def$chr$space
          } else if (A1 == "E" & A[P, Q + 1] == def$chr$space & M > Q) {
            S[J + 1] <- S[J + 1] + 1 # Reverse direction
            A[P, Q] <- def$chr$space
          } # move skeleton
        }   # move skeleton
      } # for
    } # player can move
  } # repeat
  
} #

# run

#' Play creepyalien in the R-console
#'
#' @param animate Animated CLI graphics? (TRUE|FALSE)
#' @param sound Play sound? (TRUE|FALSE)
#' @return Nothing  
#' @export
#' @examples
#' ## Start game (in interactive R sessions)
#' if (interactive())  {
#'    run()
#' }

run <- function(animate = TRUE, sound = TRUE) {
  creepyalien(animate = animate, sound = sound) 
}
