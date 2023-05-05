install.packages("petrinetR")
library(petrinetR)
install.packages("readtext")
library(readtext)

# SPECIALIST NETWORK

# This program allows: 
# 1) MAX: 1 specialist on duty.
# 2) Only one patient being treated at a time.
# 2) The number of firing times will define the final display.

# PLEASE CHANGE THE PATH TO THE APPROPRIATE DATA DIRECTORY!
setwd("D:/Users NTN/Store/Documents/Semester 211/Mathematical Modeling/Assignment/item2")
DATA_DIR <- getwd() 

init_marking <- readLines(paste(DATA_DIR, "/initmarking_2.txt", sep = ""))
firing_times <- readLines(paste(DATA_DIR, "/firingtimes_2.txt", sep = ""))

token_f <- strtoi(init_marking[1])
token_b <- strtoi(init_marking[2])
token_d <- strtoi(init_marking[3])
firing_max <- strtoi(firing_times)

free <- "free"
busy <- "busy"
docu <- "docu"
start<- "start"
change<- "change"
end <- "end"
P<-c(free,busy,docu)
T<-c(start,change,end)
F_in<-c(free,start,busy,change,docu,end)
F_out<-c(start,busy,change,docu,end,free)
F<-data.frame(from=F_in,to=F_out)
M0<-c()
N_s <- create_PN(P,T,F,M0)

if (token_f > 1 || token_b > 1 || token_d > 1 || 
    token_f < 0 || token_b < 0 || token_d < 0 || 
    token_f == 1 && token_b == 1 && token_d == 1 || 
    token_f == 1 && token_b == 1 && token_d == 0 || 
    token_f == 1 && token_b == 0 && token_d == 1 || 
    token_f == 0 && token_b == 1 && token_d == 1){
  print("HALTED: Invalid input!")
  # halt
  quit(save = "ask")
}
print("=======================================")
print(paste0(token_f,".free ",token_b,".busy ",token_d,".docu"))
print("=======================================")

firing_temp <- firing_max
while (firing_temp > 0){
  if (token_f == 1) {
    M0<-c(free)
    N_s[["marking"]]<-M0
    print(render_PN(N_s))
    print("=======================================")
    print(paste0(token_f,".free ",token_b,".busy ",token_d,".docu"))
    print("=======================================")
    Sys.sleep(2)
    print(execute(N_s,start))
    token_f <- token_f - 1
    token_b <- token_b + 1
    Sys.sleep(3)
  }
  else if (token_b == 1){
    M0<-c(busy)
    N_s[["marking"]]<-M0
    print(render_PN(N_s))
    print("=======================================")
    print(paste0(token_f,".free ",token_b,".busy ",token_d,".docu"))
    print("=======================================")
    Sys.sleep(2)
    print(execute(N_s,change))
    token_b <- token_b - 1
    token_d <- token_d + 1
    Sys.sleep(3)
  }
  else if (token_d == 1){
    M0<-c(docu)
    N_s[["marking"]]<-M0
    print(render_PN(N_s))
    print("=======================================")
    print(paste0(token_f,".free ",token_b,".busy ",token_d,".docu"))
    print("=======================================")
    Sys.sleep(2)
    print(execute(N_s,end))
    token_d <- token_d - 1
    token_f <- token_f + 1
    Sys.sleep(3)
  }
  firing_temp <- firing_temp - 1
}


                 

