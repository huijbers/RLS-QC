# database setup
#libPaths("/groups/population-sciences/IT/R")
library(RPostgreSQL)
library(ggplot2)
#library(dplyr)
library(tcltk)



getPass <- function() {
    wnd<-tktoplevel()
    tclVar("")->passVar
    tclVar("")->serverVar
    tclVar("")->userVar
    tclVar("")->dbnameVar
    tclvalue(userVar) <- Sys.getenv("USERNAME")
    tclvalue(dbnameVar) <- "rsr"
    servers <- c("rsr-rs.dzne.de", "popstud-testdb.dzne.de", "bn-svr-027-vm08-cos.dzne.de")
    tl<-tklistbox(wnd,height=length(servers),selectmode="single",background="white")
    tkgrid(tklabel(wnd,text="Server"))
    tkgrid(tl)
    for (s in servers) {
        tkinsert(tl,"end",s)
    }
    tkselection.set(tl,1)
    tkgrid(tklabel(wnd,text="DB-Name:"));  
    tkgrid(tkentry(wnd,textvariable=dbnameVar));  
    tkgrid(tklabel(wnd,text="Username:"));  
    tkgrid(tkentry(wnd,textvariable=userVar));  
    tkgrid(tklabel(wnd,text="Password:")); 
    tkgrid(tkentry(wnd,textvariable=passVar,show="*"));  
    tkgrid(tkbutton(wnd,text="OK",command=function() tkdestroy(wnd)));  
    server <- servers[as.integer(tkcurselection(tl))]
    tkwait.window(wnd);
    list(pass=tclvalue(passVar),
         user=tclvalue(userVar), server="bn-svr-027-vm08-cos.dzne.de",
         dbname=tclvalue(dbnameVar)) # servers[as.numeric(tkcurselection(tl))+1])
}


makeConnection <- function() {
    p <- getPass()
    con <<- dbConnect(PostgreSQL(), user=p$user,
                 password=p$pass,dbname=p$dbname, host=p$server)
    dbSendQuery(con, "set schema 'research'")
}


dataset <- function(name) {
  dbReadTable(con, name)
}


export.dataset <- function(name) {
    x <- dataset(name)
    filename <- paste("export-", name, format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".csv", sep="")
    write.table(filename, x, row.names=F, sep="\t")
    print(paste("Wrote ", filename, " in directory " , getwd(), sep=""))
}


export.binary <- function(filenames, b64) {
                                        # relies on the external base64 program, because
                                        # the R function cannot handle NUL in strings
    print(paste("Writing in directory ", getwd()))
    for(i in 1:length(filenames)) {
        if (!is.na(b64[i])) {
            system2("base64", "-di", input=b64[i], stdout=filenames[i])
            print(paste("Wrote ", filenames[i], sep=""))
        } else {
            print(paste("Skipped ", filenames[i], ": no data", sep=""))
        }
    }
}

makeConnection()

View(dataset("avail"))

cat("Welcome, dear Scientist!\n")
cat("Type 'x <- dataset(\"NAME\")' to retrieve the dataset NAME from the database and assign it to variable x\n")
cat("Type 'export.dataset(\"NAME\")' to export the current version of the dataset as csv\n")
cat("Use 'export.binary(filenames, filecontents)' to export binary files (will only work on Mac/Linux)\n")
