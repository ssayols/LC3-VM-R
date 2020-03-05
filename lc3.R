##
## Registers
##
R <- list(R0   =0L,
          R1   =1L,
          R2   =2L,
          R3   =3L,
          R4   =4L,
          R5   =5L,
          R6   =6L,
          R7   =7L,
          PC   =8L,  # program counter 
          COND =9L)  # condition flags

##
## Opcodes
##
OP <- list(BR  =0L,    # branch 
           ADD =1L,    # add  
           LD  =2L,    # load 
           ST  =3L,    # store 
           JSR =4L,    # jump register 
           AND =5L,    # bitwise and 
           LDR =6L,    # load register 
           STR =7L,    # store register 
           RTI =8L,    # unused 
           NOT =9L,    # bitwise not 
           LDI =10L,   # load indirect 
           STI =11L,   # store indirect 
           JMP =12L,   # jump 
           RES =13L,   # reserved (unused) 
           LEA =14L,   # load effective address 
           TRAP=15L)   # execute trap 

##
## Condition Flags 
##
FL <- list(POS=1L, # P 
           ZRO=2L, # Z 
           NEG=4L) # N 

##
## Memory Mapped Registers 
##
MR <- list(KBSR=as.integer(as.raw(0xfe00)), # keyboard status 
           KBDR=as.integer(as.raw(0xfe02))) # keyboard data 

##
## TRAP Codes
##
TRAP <- list(GETC =as.integer(as.raw(0x20)),  # get character from keyboard, not echoed onto the terminal 
             OUT  =as.integer(as.raw(0x21)),  # output a character 
             PUTS =as.integer(as.raw(0x22)),  # output a word string 
             IN   =as.integer(as.raw(0x23)),  # get character from keyboard, echoed onto the terminal 
             PUTSP=as.integer(as.raw(0x24)),  # output a byte string 
             HALT =as.integer(as.raw(0x25)))  # halt the program 

##
## Memory Storage
##
MAX_MEM <- 2^16   # 2^16 2-byte words via 16bit addressing
memory <- integer(MAX_MEM)

##
## Register Storage
##
reg <- integer(length(R))

##
## Functions
##
#' Extend any number shorter than 16 bits to 16 bits, taking the sign into account.
#' @param x the number to be extended
#' @param n number of significant bits in x
sign_extend <- function(x, n) {
  if ((x >> (n - 1)) & 1) {
    x |= (0xFFFF << n)
  }
  x
}

#' Swap each uint16 that is loaded.
#' LC-3 programs are big-endian, but most of the modern computers we
#' use are little endian. As a result, we need to swap each uint16 that
#' is loaded.
#' @param x uint16_t
#' @return 2-byte word with bytes swapped
swap16 <- function(x) {
  (x << 8) | (x >> 8);
}