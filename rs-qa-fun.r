## rs-qa-fun.r
## Standard functions for Rhineland Study quality assurance

## 


library(dplyr)
library(tidyr)
library(ggplot2)


## return a dataset with test cases and ignore removed, and age and date group columns added
RsPrepareQa <- function(x) {
    ageGroups <- read.csv(text='name,begin
<30,     0
30-39,  30
40-49,  40
50-59,  50
60-69,  60
70-79,  70
80-89,  80
90+,    90
')
                                        # status sorted by frequency (for factor conversion).
                                        # This is really handy, as the "frequent"
                                        # states will then always show up first automatically
    sortedStatus <-
        (x %>%
             group_by(metadata_exam_status) %>%
                 summarize(N = n()) %>%
                     ungroup() %>%
                         arrange(desc(N)))$metadata_exam_status
    
    x %>%
        filter(metadata_pt_test == 'REAL') %>%
        filter(metadata_exam_status != 'IGNORE') %>%
        mutate(metadata_pt_age_group = cut(metadata_pt_age,
                           breaks=c(ageGroups$begin, 150),right=FALSE,
                           labels=ageGroups$name)) %>%
        mutate(metadata_exam_date_group =
                           as.factor(format(metadata_exam_date, '%Y.%m'))) %>%
        mutate(metadata_exam_duration_min = metadata_exam_duration/60) %>%
        mutate(metadata_exam_status=factor(metadata_exam_status, levels=sortedStatus))
}

## return a summary: number of participants, and number of participants with OK examinations
## per date group
## please amend/filter your dataset with RsPrepareQa before!
RsStatusByDate <- function(x) {
    x %>% 
        group_by(metadata_exam_date_group) %>%
            summarize(N.all = length(unique(metadata_pt_sicn)),
                      N.ok  = length(unique(metadata_pt_sicn[metadata_exam_status == 'OK'])),
                      percent.ok = round(100*N.ok / N.all, digits=1))
}



## return a summary: Number of examinations by status (columns) and months (rows)

RsAllStatusByDate <- function(x) {
    x %>%
        group_by(metadata_exam_date_group, metadata_exam_status) %>%
            summarize(N = n()) %>%
                spread(metadata_exam_status, N, fill=0) 
}


## return a summary: duration of examinations per age group
## - please amend/filter your dataset with RsPrepareQa before!
## - please filter by SOP version (?) / date before!
RsDurationByAge <- function(x) {
    x %>%
        group_by(metadata_pt_age_group) %>%
            summarize(N = n(),
                      duration.p90 = round(quantile(metadata_exam_duration_min, probs=c(0.9), na.rm=TRUE), digits=1),
                      duration.median = round(median(metadata_exam_duration_min, na.rm=TRUE),digits=1))
}


## sum up time columns
## this function selects all columns starting with <prefix> and ending with "time"
## and append a column "sum_time_<prefix>" to the dataset, containing the
## rowwise sums of the columns. Also, the columns are converted to numeric

RsAddTimeSummary <- function(df, prefix) {
    df %>% select(metadata_exam_mrid, starts_with(prefix)) %>%
        select(metadata_exam_mrid, ends_with("time")) %>%
            gather(question, time, -metadata_exam_mrid) %>%
                mutate(time = as.numeric(time)) %>%
                    group_by(metadata_exam_mrid) %>%
                        summarize(sum_time = sum(time, na.rm=T)/1000/60) %>%
                            rename_(.dots=setNames(list("sum_time"), paste("sum_time_", prefix, sep=""))) %>%
                                right_join(df)
}


## print a floating point minute value nicely as min:sec
RsMinuteToMinSec <- function(m) {
    m <- round(m*60)/60
    sprintf("%2d:%02d", as.integer(floor(m)), as.integer((m - floor(m))*60))
}


################################################################################################
## from here on useful utility functions not necessarily meant as templates to use yourself

## interactively add new comments to a dataset based on a file
RsAddComments <- function(x, commentFile, scientist) {
    oldComments <- read.csv(commentFile, fileEncoding="UTF-8", as.is=TRUE,
                            colClasses=c("character", "numeric", "Date", rep("character",5), "Date"))
    newstates <- c("OK", "DOUBTFUL", "NOTOK")
    x %<>%
        filter(metadata_exam_status != 'OK' |
                   metadata_exam_comment != '') %>%
            anti_join(oldComments, by=c("metadata_exam_mrid"))
    if (nrow(x)==0) {
        cat("Nothing to do, enjoy your free time!\n")
        return
    }
    cat(paste("Hello,", scientist, "we'll go through the", nrow(x) ,
              "cases with unchecked comments and/or not 'OK' status\n"))
    for (i in 1:nrow(x)) {
        xx <- x[i,]
        View(t(xx))
        cat("Look at the data. New status? [0] Skip / [1] OK / [2] DOUBTFUL /  [3] NOTOK / [9]: End this session: ")
        status <- as.numeric(readline())
        if (status >= 1 && status <= 3) {
            cat("Enter an additional comment: ")
            comment <- readline()
            oldComments <- rbind(oldComments,
                                 xx %>%
                                     select(metadata_pt_sicn, metadata_exam_mrid, metadata_exam_date,
                                            metadata_exam_status, metadata_exam_comment) %>%
                                         mutate(metadata_exam_qastatus =newstates[status],
                                            metadata_exam_qacomment = comment,
                                                metadata_exam_qaresponsible = scientist,
                                                metadata_exam_qadate = format(Sys.Date(), "%Y-%m-%d")))
        }
        if (status == 9) {
            break
        }
    }
    write.csv(oldComments, file=commentFile, row.names=FALSE, fileEncoding="UTF-8")
}


## get the union of all sics in all the datasets
## result will be a dataset with a column
## metadata_pt_sicn | <dataset1.all> | <dataset1.ok> | ....
## test cases are not treated seperately and should be filtered out before
RsComplete <- function(...) {
    datasets <- list(...)
    names <- tail(as.character(as.list(match.call())), n=-1)
                                        # union of the metadata_pt_sicn
    result <- data.frame(metadata_pt_sicn =
                             Reduce(union,
                                    lapply(datasets,
                                           function(x) { x$metadata_pt_sicn })))
                                        # add the counts for individual datasets
    for (i in 1:length(names)) {
        result <- result %>%
            left_join(datasets[[i]] %>%
                          group_by(metadata_pt_sicn) %>%
                              summarize_(.dots = setNames(list(~length(metadata_exam_status),
                                             ~sum(metadata_exam_status == 'OK')),
                                             c(paste(names[i], "all",  sep="."),
                                               paste(names[i], "ok",  sep=".")))))
    }
    result
}    

