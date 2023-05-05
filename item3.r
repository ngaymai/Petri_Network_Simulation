install.packages("petrinetR")
library(petrinetR)
install.packages("readtext")
library(readtext)

# SUPERIMPOSED NETWORK

# This program allows: 
# 1) MAX: 10 patients in place wait & 1 specialist on duty.
# 2) Only one patient being treated at a time.
# 2) The number of firing times will define the final display.

# PLEASE CHANGE THE PATH TO THE APPROPRIATE DATA DIRECTORY!
setwd("D:/Users NTN/Store/Documents/Semester 211/Mathematical Modeling/Assignment/item3/")
DATA_DIR <- getwd() 

init_marking <- readLines(paste(DATA_DIR, "/initmarking_3.txt", sep = ""))
firing_times <- readLines(paste(DATA_DIR, "/firingtimes_3.txt", sep = ""))

token_w <- strtoi(init_marking[1])
token_f <- strtoi(init_marking[2])
token_ib <- strtoi(init_marking[3])
token_dc <- strtoi(init_marking[4])
token_dn <- strtoi(init_marking[5])
firing_max <- strtoi(firing_times)

if (token_w > 10 || token_i > 1 || token_w < 0 || token_i < 0 || token_done < 0 || 
    token_f > 1 || token_b > 1 || token_d > 1 || 
    token_f < 0 || token_b < 0 || token_d < 0 || 
    token_f == 1 && token_b == 1 && token_d == 1 || 
    token_f == 1 && token_b == 1 && token_d == 0 || 
    token_f == 1 && token_b == 0 && token_d == 1 || 
    token_f == 0 && token_b == 1 && token_d == 1){
  print("HALTED: Invalid input!")
  # halt
  quit(save = "ask")
}
wait <- "wait"
free <- "free"
inbu <- "inside|busy"
docu <- "docu"
done <- "done"
start<-"start"
change<-"change"
end<-"end"

P<-c(wait,free,inbu,docu,done)
T<-c(start,change,end)
F_in<-c(wait,free,start,inbu,change,change,docu,end)
F_out<-c(start,start,inbu,change,docu,done,end,free)
F<-data.frame(from=F_in,to=F_out)
M0<-c()
PN<-create_PN(P,T,F,M0)
while (token_w > 0){
  
  if (token_f==1){
    if(token_dn>0)
    {M0<-c(wait,free,done)}
    else
    {M0<-c(wait,free)}
    }
  else if (token_ib == 1){
    if(token_dn>0)
    {M0<-c(wait,inbu,done)}
    else
    {M0<-c(wait,inbu)}
  }
  else if (token_dc == 1){
    if(token_dn>0)
    {M0<-c(wait,docu,done)}
    else
    {M0<-c(wait,docu)}
  }
  PN[["marking"]]<-M0
  print(render_PN(PN))
  print("=======================================")
  print(paste0(token_w,".wait ",token_f,".free ",
               token_ib,".inside|busy ",token_dc,".docu ",token_dn,".done"))
  print("=======================================")
  Sys.sleep(2)
  if(token_f==1)
  {
    print(execute(PN,start))
    token_ib<-token_ib+1
    token_w<-token_w-1
    token_f<-token_f-1
    Sys.sleep(3)
  }
  
  else if(token_ib==1)
  {
    print(execute(PN,change))
    token_dc<-token_dc+1
    token_dn<-token_dn+1
    token_ib<-token_ib-1
    Sys.sleep(3)
  }
  
  else if(token_dc==1)
  {
    print(execute(PN,end))
    token_dc<-token_dc-1
    token_f<-token_f+1
    Sys.sleep(3)
  }
 
}
if(token_ib==1)
{
  if(token_dn>0)
  {M0<-c(inbu,done)}
  else
  {M0<-c(inbu)}
  PN[["marking"]]<-M0
  print(render_PN(PN))
  print("=======================================")
  print(paste0(token_w,".wait ",token_f,".free ",
               token_ib,".inside|busy ",token_dc,".docu ",token_dn,".done"))
  print("=======================================")
  Sys.sleep(2)
  print(execute(PN,change))
  token_dc<-token_dc+1
  token_dn<-token_dn+1
  token_ib<-token_ib-1
  Sys.sleep(3)
}
if(token_dc==1)
{
  M0<-c(docu)
  PN[["marking"]]<-M0
  print(render_PN(PN))
  print("=======================================")
  print(paste0(token_w,".wait ",token_f,".free ",
               token_ib,".inside|busy ",token_dc,".docu ",token_dn,".done"))
  print("=======================================")
  Sys.sleep(2)
  print(execute(PN,end))
  token_dc<-token_dc-1
  token_f<-token_f+1
  Sys.sleep(3)
}

cat(" =======================================\n",
    paste(token_w,".wait ",token_f,".free ",
          token_ib,".inside|busy ",token_dc,".docu ",token_dn,".done\n"),
    "=======================================\n")
M0<-c(done,free)
PN[["marking"]]<-M0
print(render_PN(PN))

