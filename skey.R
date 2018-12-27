library(data.table)
library(dplyr)
library(readtext)
library(quanteda)
library(reshape2)

ptime <- function(e) { as.numeric(system.time(e)[3]) }
psize <- function(x) { format(object.size(x), units="auto") }

NGRAM <- 4
NGRP <- 10
PRUNE <- 2
DEBUG <- FALSE
if (DEBUG == TRUE) { NM <- 2 } else { NM <- NGRP }

PFLIST <- readLines("profanity_filter_en.txt")

getTokens <- function(txt, nt) {
        tok <- function(x) {
                tokens(
                        char_tolower(x),
                        what = "word",
                        remove_separators = TRUE,
                        remove_numbers = TRUE,
                        remove_punct = TRUE,
                        remove_symbols = TRUE,
                        remove_twitter = TRUE,
                        ngrams = nt,
                        hash=FALSE,
                        verbose = FALSE
                )
        }
        unlist(lapply(txt, tok))
}

getNgram <- function(s, nt=1:NGRAM) {
        fx <- function(x) {
                a <- unlist(strsplit(x, "_"))
                v <- tail(a, 1)
                k <- paste0(head(a, length(a)-1), collapse="_")
                c(k, v)
        }
        ts <- ptime(
                df <- data.frame(t(sapply(getTokens(s, nt), fx)), 
                                 row.names=NULL, stringsAsFactors=FALSE)
                
        )
        if (ncol(df) > 0) {
                print(sprintf("Parsed %d tokens in %s sec (%s)", nrow(df), ts, psize(df)))
                names(df) <- c("prev", "word")
                df$count <- 1
        } else {
                df <- NULL
        }
        df
}

pruneNgram <- function(ng) {
        data.frame(ng[ng$count>=PRUNE,] %>% group_by(prev) %>% top_n(n=5, wt=count),
                   stringsAsFactors=FALSE)
}

predictWords <- function(ls, ng, nw=5, pf=FALSE) {
        predictWord <- function(s, ng, wc) {
                DISCOUNT <- 0.7
                t <- tail(getTokens(tolower(s), 1), NGRAM-1)
                ws <- data.frame(stringsAsFactors=FALSE)
                for (i in 1:length(t)) {
                        k <- paste0(t[i:length(t)], collapse="_")
                        w <- ng[ng$prev==k, c("word", "count")]
                        w$word <- as.character(w$word)
                        b <- (1-DISCOUNT)^max(i-1, 0)
                        d <- DISCOUNT^(as.numeric(i!=length(t)))
                        w <- mutate(w, prob=(count/sum(w$count)*b*d))
                        ws <- rbind(ws, w)
                }
                if (pf == TRUE) { # profanity filter here
                        ws <- ws[!(ws$word %in% PFLIST),]                        
                }
                if (nrow(ws) > 0) {
                        ws <- aggregate(prob ~ word, ws, sum)        
                        ws <- top_n(ws[order(-ws$prob),], wc, wt=prob)
                        ws <- ws[1:wc, 1]
                        ws <- ws[complete.cases(ws)]
                } else {
                        ws <- c()
                }
                if (length(ws) < wc) {
                        ws <- c(ws, predictWord("", ng, wc))[1:wc]
                }
                ws
        }
        lapply(ls, function(x){ predictWord(x, ng, nw) })
}

prepData <- function(ddir) {
        set.seed(1537773)
        TRGPATH <- file.path(ddir, "/trg/")
        TSTPATH <- file.path(ddir, "/tst/")
        dir.create(TRGPATH, showWarnings=FALSE)
        dir.create(TSTPATH, showWarnings=FALSE)
        unlink(file.path(TRGPATH, "*"))
        unlink(file.path(TSTPATH, "*"))
        
        print(sprintf("NGRP: %d", NGRP))
        fdat <- data.frame(
                trg=sapply(1:NGRP, function(x) 
                        file.path(TRGPATH, paste0("trg-", x, ".txt"))),
                tst=sapply(1:NGRP, function(x) 
                        file.path(TSTPATH, paste0("tst-", x, ".txt"))),
                stringsAsFactors=FALSE
        )
        for (fn in dir(ddir, pattern="*.txt")) {
                f <- file(file.path(ddir, fn), open="rt", raw=TRUE)
                s <- readLines(f, encoding="UTF-8", skipNul=FALSE)
                close(f)
                g <- rep(1:NGRP, length(s)/NGRP)
                t <- split(s[1:length(g)], g)
                for (i in 1:NGRP) {
                        u <- unlist(t[i])
                        if (DEBUG == TRUE) {
                                u <- sample(u, length(u)*0.01, replace=FALSE)
                        }
                        n <- length(u)
                        trg <- sample(1:n, n*0.7, replace=FALSE)
                        write(u[trg], fdat$trg[i], append=TRUE)
                        write(u[-trg], fdat$tst[i], append=TRUE)
                        print(sprintf("%d: %d lines from %s (%s)", i, n, fn, psize(u)))
                }
        }
        fdat
}

loadNgram <- function(datafile) {
        ng <- data.frame(prev=character(), word=character(), count=integer(),
                         stringsAsFactors=FALSE)
        f <- file(datafile, open="rt", raw=TRUE)
        while (length(x <- readLines(f, n=100000, encoding="UTF-8")) > 0 ) {
                print(sprintf("Read %d lines from %s (%s)", 
                              length(x), datafile, psize(x)))
                df <- getNgram(x)
                ts <- ptime(
                        ng <- rbind(ng, df)
                )
                print(sprintf("Appended in %s sec (%s)", ts, psize(ng)))
                ts <- ptime(
                        ng <- aggregate(count ~ prev+word, ng, sum)
                )
                print(sprintf("Merged in %s sec (%s)", ts, psize(ng)))
        }
        close(f)
        ng
}

trainModels <- function(da) {
        ng <- list()
        for (i in 1:NM) {
                print(date())
                ts <- ptime(
                        ng[[i]] <- loadNgram(da$trg[i])
                )
                ng[[i]] <- pruneNgram(ng[[i]])
                print(sprintf("Train Ngram model %d completed in %s sec (%s)", i, ts, psize(ng)))
        }
        ng
}

mergeModels <- function(ng) {
        ng1 <- data.frame(prev=character(), word=character(), count=integer(), stringsAsFactors=FALSE)
        for (i in 1:NM) {
                ng1 <- rbind(ng1, ng[[i]])
        }
        ts <- ptime(
                ng1 <- aggregate(count ~ prev+word, ng1, sum)
        )
        print(sprintf("Merged in %s sec (%s)", ts, psize(ng1)))
        ts <- ptime(
                ng1 <- pruneNgram(ng1)
        )
        print(sprintf("Pruned in %s sec (%s)", ts, psize(ng1)))
        ng1
}

testModel <- function(ng, fn) {
        tc <- 0
        tm <- 0
        f <- file(fn, open="rt", raw=TRUE)
        while (length(x <- readLines(f, n=1000, encoding="UTF-8")) > 0 ) {
                df <- getNgram(x, NGRAM)
                if (is.null(df)) next
                ts <- ptime(
                        pword <- unlist(predictWords(gsub("_", " ", df$prev), ng, 1))
                )
                print(sprintf("predictWords completed in %s sec", ts))
                tc <- tc + sum(df$count)
                tm <- tm + sum(as.numeric(df$word==pword)*df$count)
                print(sprintf("%s / %s (%s) %s", tm, tc, format(tm/tc, digits=5, ns=4), date()))
        }
        close(f)
        data.frame(count=tc, match=tm)
}

testModels <- function(ng, da) {
        re <- data.frame(count=integer(), match=integer())
        for (i in 1:NM) {
                print(date())
                ts <- ptime(
                        re <- rbind(re, testModel(ng, da$tst[i]))
                )
        }
        re
}