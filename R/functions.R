get.divisions <- function(URLs){
    if (length(URLs) > 1){  ## if argument is a *vector* of URLs
        return(unlist(lapply(URLs, get.divisions), recursive = FALSE))
    }
    URLstring <- URLs
    cat(URLstring, "\n")
    theURL <- url(URLstring)
    lines <- try(scan(theURL, what=character(0), sep="\n",
                      strip.white = TRUE, blank.lines.skip = TRUE))
    close(theURL)
    if (inherits(lines, "try-error")) {
        cat("****** URL unreachable ******\n")
        return(NULL)}
    startlines <- grep("<!-- division -->", lines)
    if (length(startlines)==0){
        startlines <- grep(">Division No", lines) - 1}
    if (length(startlines)==0){
        cat("****** no division startline ******\n")}
    endlines <- grep("<!-- end division -->", lines)
    if (length(endlines) == 0) endlines <-
        grep("^Question accordingly", lines)
    if (length(startlines) == 0) return(NULL)
    result <- list()
    for (i in 1:length(startlines)){
        this.division <- lines[startlines[i] : endlines[i]]
        id <- strsplit(this.division[2], split = "\"")[[1]][2]
        id <- standard.divname(id)
        AYESlinenumber <- grep("AYES", this.division)
        NOESlinenumber <- grep("NOES", this.division)
        ENDlinenumber <- length(this.division)
        ayes <- this.division[(AYESlinenumber + 1) : (NOESlinenumber - 1)]
        noes <- this.division[(NOESlinenumber + 1) : (ENDlinenumber - 3)]
        HTMLlines <- grep("^\ *<", ayes)
        if (length(HTMLlines) > 0) ayes <-
            ayes[-HTMLlines] # get rid of HTML markup lines
        HTMLlines <- grep("^\ *<", noes)
        if (length(HTMLlines) > 0) noes <-
            noes[-HTMLlines] # get rid of HTML markup lines
        oddlines <- c(
                      grep("Whereupon", noes),
                      grep("Question", noes)
                      )
        if (length(oddlines) > 0) noes <-
            noes[-oddlines] # remove odd statements
        ayes <- gsub("[\ ]*<[pP]>$", "", ayes) # to remove trailing <P>
        noes <- gsub("[\ ]*<[pP]>$", "", noes)
        ayes <- gsub("[\ ]*<BR>$", "", ayes) # to remove trailing <BR>
        noes <- gsub("[\ ]*<BR>$", "", noes)
        ayes <- gsub("[\ ]*<br>$", "", ayes) # to remove trailing <br>
        noes <- gsub("[\ ]*<br>$", "", noes)
        ayes <- sub("<[iI]>", "(",  ayes)    # next 8 lines sort out <i> etc
        ayes <- sub("\\(\\ *\\(", "(", ayes)
        noes <- sub("<[iI]>", "(",  noes)
        noes <- sub("\\(\\ *\\(", "(", noes)
        ayes <- sub("</[iI]>", ")",  ayes)
        ayes <- sub("\\)\\ *\\)", ")", ayes)
        noes <- sub("</[iI]>", ")",  noes)
        noes <- sub("\\)\\ *\\)", ")", noes)
        ayes.TellerLine <- grep("Tellers", ayes)
        noes.TellerLine <- grep("Tellers", noes)
        if (length(ayes.TellerLine) > 0){
            Nayes <- length(ayes) - 1
            if (ayes.TellerLine != (Nayes - 1)){
                if (ayes.TellerLine == Nayes) {
                    ayes[(Nayes + 1):(Nayes + 2)] <-
                        strsplit(ayes[Nayes + 1], " and ")[[1]]}
                else {
                    cat("****** Nonstandard format ******\n")
                    return(NULL)}
            }
        }
        if (length(noes.TellerLine) > 0){
            Nnoes <- length(noes)-1
            if (noes.TellerLine != (Nnoes - 1)){
                if (noes.TellerLine == Nnoes) {
                    noes[(Nnoes + 1):(Nnoes + 2)] <-
                        strsplit(noes[Nnoes + 1], " and ")[[1]]}
                else {
                cat("****** Nonstandard format ******\n")
                return(NULL)}
            }
        }
        result[[id]] <- list(url = URLstring, ayes = ayes, noes = noes)}
    result}

standard.divname <- function(div.id){
    date <- paste("0", substr(div.id, 1, 5), sep = "")
    divno <- strsplit(div.id, split = "_div")[[1]][2]
    divno <- substr(as.character(1000 + as.numeric(divno)), 2, 4)
    paste("div", divno, ".", date, sep = "")
}

collect.MPnames <- function(divs, verbose = TRUE){
    if (verbose) {
        cat("Processing", length(divs), "divisions \n")
    }
    names <- character(0)
    for (i in (1:length(divs))){
        if (verbose) cat(i, " ")
        temp <- uniquify.MPnames(divs[[i]]$ayes)
#        problems <- temp %in% problem.names
#        if (any(problems)) print(temp[problems])
        names <- rbind(names, temp)
        temp <- uniquify.MPnames(divs[[i]]$noes)
#        problems <- temp %in% problem.names
#        if (any(problems)) print(temp[problems])
        names <- unique(rbind(names, temp))
    }
    names <- names[order(names[,1]),]
    duplicates <- names[duplicated(names[,1]),1]
    selector <- apply(names, 1,
          function(row){
              !(row[1] %in% duplicates) || !("NA"==row[2])})
    if (verbose) cat("\n")
    as.data.frame(names[selector,])}

"uniquify.MPnames" <-
    function (ayes.or.noes, namerules = namerules.9203) 
{
    if (length(ayes.or.noes) == 0) return(character(0))
    MPtitles <- c("Mrs", "Mr", "Miss", "Ms", "Sir", "Dr", "Reverend", 
                  "Rev", "RH", "Rt", "Hon", "rh", "rah", "hon", "Professor", 
                  "Prof", "The", "Dame")
    ALPHABET <- paste(LETTERS, collapse = "")
    temp <- ayes.or.noes
    temp <- sub("&#214;", "O", temp)
    temp <- sub("&#244;", "o", temp)
    temp <- sub("&#150;", "-", temp)
    temp <- gsub("\\.", " ", temp)
    sort.out.tellers <- function(teller.lines) {
        teller.lines <- gsub("\\.", " ", teller.lines)
        teller.lines <- gsub(paste("[", ALPHABET, "] ", sep = ""), 
                             " ", teller.lines)
        teller.lines <- gsub(paste("[", ALPHABET, "]$", sep = ""), 
                             " ", teller.lines)
        for (t in MPtitles) {
            teller.lines <- sub(paste(" ", t, "$", sep = ""), 
                                " ", teller.lines)
            teller.lines <- sub(paste(t, " ", sep = ""), " ", 
                                teller.lines)
        }
        teller.lines <- sub(" and", " ", teller.lines)
        teller.lines <- sub("^ *", "", teller.lines)
        teller.lines <- sub(" *$", "", teller.lines)
        teller.lines <- gsub("  +", " ", teller.lines)
        unlist(lapply(strsplit(teller.lines, " "), function(line) {
            if (length(line) == 3) {
                if (length(grep("\\(", line[3])) > 0) {
                    return(paste(paste(rev(line[1:2]), collapse = ", "), 
                                 line[3]))
                }
                line <- c(paste(line[1:2], collapse = " "), line[3])
            }
            paste(rev(line), collapse = ", ")
        }))
    }
    tellerline <- grep("Tellers", temp)
    if (length(tellerline) > 1) 
        stop("More than one Tellers line")
    if (length(tellerline) == 1) {
        tellerlines <- temp[(tellerline + 1):(tellerline + 2)]
        tellerlines <- sort.out.tellers(tellerlines)
        temp <- c(temp[0:(tellerline - 1)], tellerlines)
    }
    for (t in MPtitles) {
        temp <- sub(paste(" ", t, "$", sep = ""), " ", temp)
        temp <- sub(paste(t, " ", sep = ""), " ", temp)
    }
    temp <- sub("<[Ii]>", " ", temp)
    temp <- sub("</[Ii]>", "", temp)
    temp <- sub("\\)", "", temp)
    temp <- strsplit(temp, split = "\\(")
    MPnames <- sapply(temp, function(x) x[1])
    seatnames <- sapply(temp, function(x) x[2])
    MPnames <- gsub(paste("[", ALPHABET, "] ", sep = ""), " ", 
                    MPnames)
    MPnames <- gsub(paste("[", ALPHABET, "]$", sep = ""), " ", 
                    MPnames)
    MPnames <- sub("<br>", " ", MPnames)
    MPnames <- sub("<BR>", " ", MPnames)
    MPnames <- sub("^ *", "", MPnames)
    MPnames <- sub(" *$", "", MPnames)
    MPnames <- gsub("  +", " ", MPnames)
    MPnames <- sub("Mc ", "Mc", MPnames)
    MPnames <- sub("Mac ", "Mac", MPnames)
    adjusted <- namerules(MPnames, seatnames)
    MPnames <- adjusted$MPnames
    seatnames <- adjusted$seatnames
    seatnames <- substr(seatnames, 1, 3)
    cbind(MPname = MPnames, seatname = seatnames)
}

namerules.9203 <- function(MPnames, seatnames){
    MPnames <- sub(",,", ",", MPnames)
    MPnames <- sub("Ashton, Joe", "Ashton, Joseph", MPnames)
    MPnames <- sub("Ainger, Nicholas", "Ainger, Nick", MPnames)
    MPnames <- sub("Beith.*", "Beith, A J", MPnames)
    MPnames <- sub("Barker, Gregory", "Barker, Greg", MPnames)
    MPnames <- sub("Blackman, Elizabeth", "Blackman, Liz", MPnames)
    MPnames <- sub("Blizzard, Robert", "Blizzard, Bob", MPnames)
    MPnames <- sub("Boswell, Timothy", "Boswell, Tim", MPnames)
    MPnames <- sub("Brake, Thomas", "Brake, Tom", MPnames)
    MPnames <- sub("Brandreth, Giles", "Brandreth, Gyles", MPnames)
    MPnames <- sub("Brown, Nicholas", "Brown, Nick", MPnames)
    MPnames <- sub("Campbell, Ronald", "Campbell, Ronnie", MPnames)
    MPnames <- sub("Campbell-Savours,*$", "Campbell-Savours, Dale", MPnames)
    MPnames <- sub("Cann, James", "Cann, Jamie", MPnames)
    MPnames <- sub("Carlile, Alexander", "Carlile, Alex", MPnames)
    MPnames <- sub("Chapman, Sidney", "Chapman, Sydney", MPnames)
    MPnames <- sub("Churchill,", "Churchill, Winston", MPnames)
    MPnames <- sub("Colman, Tony", "Colman, Anthony", MPnames)
    MPnames <- sub("Cunningham, John", "Cunningham, Jack", MPnames)
    MPnames <- sub("Curtis-Thomas, Clare", "Curtis-Thomas, Claire", MPnames)
    MPnames <- sub("Delvin, Tim", "Devlin, Tim", MPnames)
    MPnames <- sub("Deva, Niranjan", "Deva, Nirj Joseph", MPnames)
    MPnames <- sub("Douglas-Hamilton.*",
                   "Douglas-Hamilton, James", MPnames)
    MPnames <- sub("Duncan-Smith, Iain", "Duncan Smith, Iain", MPnames)
    MPnames <- sub("Dunn, Bob", "Dunn, Robert", MPnames)
    MPnames <- sub("Ennis, Jeffrey", "Ennis, Jeff", MPnames)
    MPnames <- sub("Etherington, William", "Etherington, Bill", MPnames)
    MPnames <- sub("Fishburn, John Dudley", "Fishburn, Dudley", MPnames)
    MPnames <- sub("Foster, Donald", "Foster, Don", MPnames)
    MPnames <- sub("Foster, Michael John", "Foster, Michael", MPnames)
    MPnames <- sub("Gallie, Philip", "Gallie, Phil", MPnames)
    MPnames <- sub("Gapes, Michael", "Gapes, Mike", MPnames)
    MPnames <- sub("Grant,$", "Grant, Anthony", MPnames)
    MPnames <- sub("Gummer, John Selwyn", "Gummer, John", MPnames)    
    MPnames <- sub("Hamilton, Archibald", "Hamilton, Archie", MPnames)
    MPnames <- sub("Hawkins, Nicholas", "Hawkins, Nick", MPnames)
    MPnames <- sub("Hepple, John", "Heppell, John", MPnames)
    MPnames <- sub("Hinchcliffe, David", "Hinchliffe, David", MPnames)
    MPnames <- sub("Hoon, Geoff$", "Hoon, Geoffrey", MPnames)
    MPnames <- sub("Hope, Philip", "Hope, Phil", MPnames)
    MPnames <- sub("Johnson Smith,$", "Johnson Smith, Geoffrey", MPnames)
    MPnames <- sub("Jones, Ieuan$", "Jones, Ieuan Wyn", MPnames)
    MPnames <- sub("Jones, John Owen", "Jones, Jon Owen", MPnames)
    MPnames <- sub("Kellet-Bowman, Elaine",
                   "Kellett-Bowman, Elaine", MPnames)
    MPnames <- sub("Kirkwood, Archie", "Kirkwood, Archy", MPnames)
    MPnames <- sub("Lester, Jim", "Lester, James", MPnames)
    MPnames <- sub("Lightbrown, David", "Lightbown, David", MPnames)
    MPnames <- sub("MacShane, Dennis", "MacShane, Denis", MPnames)
    MPnames <- sub("Macdonald, Calum", "MacDonald, Calum", MPnames)
    MPnames <- sub("MacKinlay, Andrew", "Mackinlay, Andrew", MPnames)
    MPnames <- sub("McKay, Andrew", "Mackay, Andrew", MPnames)
    MPnames <- sub("O'Brien, Michael", "O'Brien, Mike", MPnames)
    MPnames <- sub("Olner, William", "Olner, Bill", MPnames)
    MPnames <- sub("Powell, Ray$", "Powell, Raymond", MPnames)
    MPnames <- sub("Quinn, Joyce", "Quin, Joyce", MPnames)
    MPnames <- sub("Robertson, John Home", "Home Robertson, John", MPnames)
    MPnames <- sub("Sainsbury, Timothy", "Sainsbury, Tim", MPnames)
    MPnames <- sub("Skinner, Denis", "Skinner, Dennis", MPnames)
    MPnames <- sub("Smith, Llewellyn", "Smith, Llew", MPnames)
    MPnames <- sub("Spicer, Jim", "Spicer, James", MPnames)
    MPnames <- sub("Thompson, John", "Thompson, Jack", MPnames)
    MPnames <- sub("Turner, Denis", "Turner, Dennis", MPnames)
    MPnames <- sub("Whitney, Ray$", "Whitney, Raymond", MPnames)
    MPnames <- sub("Wood, Tim$", "Wood, Timothy", MPnames)
#    MPnames <- sub("Wright, Tony", "Wright, Anthony", MPnames) # 92-97 only
#    MPnames <- sub("Smith, Geoffrey", "Johnson Smith, Geoffrey", MPnames)
    to.delete <- which(MPnames == "Geoffrey")
    if (length(to.delete) > 0){
        MPnames <- MPnames[-to.delete]
        seatnames <- seatnames[-to.delete]
    }
    to.delete <- which(MPnames == "Lord James")
    if (length(to.delete) > 0){
        MPnames <- MPnames[-to.delete]
        seatnames <- seatnames[-to.delete]
    }
    MPnames <- sub("MacKay", "Mackay", MPnames)
    MPnames <- sub("McCabe, Steve", "McCabe, Stephen", MPnames)
    MPnames <- sub("McCafferty, Chris$", "McCafferty, Christine", MPnames)
    MPnames <- sub("Murphy, Dennis", "Murphy, Denis", MPnames)
    MPnames <- sub("Jones, Jenny", "Jones, Jennifer", MPnames)
    MPnames <- sub("Kingham, Tessa", "Kingham, Tess", MPnames)
    MPnames <- sub("Love, Andy", "Love, Andrew", MPnames)
    MPnames <- sub("Simon, Si.*", "Simon, Sion", MPnames)
    MPnames <- sub("Brinton, Helen", "Clark, Helen", MPnames)
    MPnames <- sub("Morrison, Andrew", "Murrison, Andrew", MPnames)
    MPnames <- sub("O'Brien, Bill", "O'Brien, William", MPnames)
    MPnames <- sub("O'Hara, Eddie", "O'Hara, Edward", MPnames)
    MPnames <- sub("Sawford, Phil$", "Sawford, Philip", MPnames)
    MPnames <- sub("Spink, Robert", "Spink, Bob", MPnames)
    MPnames <- sub("Aubyn, Nick St", "St Aubyn, Nick", MPnames)
    MPnames <- sub("Vis, Rudi", "Vis, Rudolf", MPnames)
    MPnames <- sub("Webb, Steven", "Webb, Steve", MPnames)
    seatnames <- sub("Shettleston", "Glasgow Shettleston", seatnames)
    seatnames <- sub("C'bridge", "Cambridge", seatnames)
    seatnames <- sub("Gt Grimsby", "Grimsby", seatnames)
    seatnames <- sub("Great Grimsby", "Grimsby", seatnames)
    seatnames <- sub("City of Chester", "Chester", seatnames)
    seatnames <- sub("Hillsborough", "Sheffield Hillsborough", seatnames)
    seatnames <- sub("Deeside", "Alyn and Deeside", seatnames)
    seatnames <- sub("M-Norfolk", "Mid-Norfolk", seatnames)
    seatnames <- sub("N Warks", "Warwickshire North", seatnames)
    seatnames <- sub("Kemptown", "Brighton Kemptown", seatnames)
    seatnames <- sub("Withington", "Manchester Withington", seatnames)
    seatnames <- sub("Ab'", "Abe", seatnames)
                                    # these two to cope with Robert Hughes
    seatnames <- sub("A'd", "Abe", seatnames)
    seatnames <- ifelse((MPnames == "Williams, Alan") &
                           (substr(seatnames,1,1) == "S"),
                       "Swa", seatnames)
    MPnames <- ifelse((MPnames == "Cunningham,") &
                         (substr(seatnames,1,3) == "Per"),
                      "Cunningham, Roseanna", MPnames)
    MPnames <- ifelse((MPnames == "Squire,") &
                         (substr(seatnames,1,3) == "Dun"),
                      "Squire, Rachel", MPnames)    
    MPnames <- ifelse((MPnames == "Prentice,") &
                         (substr(seatnames,1,3) == "Lew"),
                      "Prentice, Bridget", MPnames)
    MPnames <- ifelse((MPnames == "Jones,") &
                         (substr(seatnames,1,3) == "B'h"),
                      "Jones, Lynne", MPnames)
    MPnames <- ifelse((MPnames == "Taylor,") &
                         (substr(seatnames,1,3) == "Str"),
                      "Taylor, John", MPnames)
    MPnames <- ifelse((MPnames == "Brown,") &
                         (substr(seatnames,1,3) == "N'c"),
                      "Brown, Nick", MPnames)
    MPnames <- ifelse((MPnames == "Brown,") &
                         (substr(seatnames,1,3) == "Bri"),
                      "Brown, Michael", MPnames)
    MPnames <- ifelse((MPnames == "Morris,") &
                         (substr(seatnames,1,3) == "Wy'"),
                      "Morris, Alfred", MPnames)
    MPnames <- ifelse((MPnames == "Morris,") &
                         (substr(seatnames,1,3) == "Abe"),
                      "Morris, John", MPnames)
    MPnames <- ifelse((MPnames == "Smith,") &
                         (substr(seatnames,1,3) == "Isl"),
                      "Smith, Chris", MPnames)
    no.seatname <- c("Baker, Nicholas",
                     "Brown, Nick",
                     "Bruce, Ian",
                     "Clark, David",
                     "Clarke, Kenneth",
                     "Currie, Edwina",
                     "Davies, Chris",
                     "Forsythe, Clifford",
                     "Grant, Anthony",
                     "Hogg, Douglas",
                     "Howarth, Alan",
                     "Howell, David",
                     "Howell, Ralph",
                     "Jackson, Glenda",
                     "Jenkins, Brian",
                     "Kennedy, Jane",
                     "Knight, Jill",
                     "McCartney, Robert",
                     "Mitchell, Austin",
                     "Mitchell, David",
                     "Michie, Bill",
                     "Nicholson, Emma",
                     "O'Brien, Mike",
                     "Robertson, Raymond",
                     "Robinson, Geoffrey",
                     "Spicer, Michael",
                     "Squire, Rachel",
                     "Squire, Robin",
                     "Taylor, Ann",
                     "Thompson, Donald",
                     "Townsend, Cyril",
                     "Walker, Bill")
    seatnames <- ifelse(MPnames %in% no.seatname, NA, seatnames)
    TonyWright.indices <- grep("Wright,\ Tony",MPnames)
    if (length(TonyWright.indices) > 0){
        MPnames[TonyWright.indices[substr(
                 seatnames[TonyWright.indices],1,2) == "Gt"]
               ] <- "Wright, Anthony"
    }
    list(MPnames = MPnames, seatnames = seatnames)
}

"make.votesheet" <-
    function (divs, MPnames, verbose = TRUE) 
{
    sort.division.lists <- function(division, MPnames) {
        if (verbose) {
            assign("counter", counter + 1, inherits = TRUE)
            cat(counter, " ")
        }
        ayes <- uniquify.MPnames(division$ayes)
        noes <- uniquify.MPnames(division$noes)
        result <- rep("-", nrow(MPnames))
        if (!is.null(nrow(ayes))){
            for (i in 1:nrow(ayes)) {
                row <- ayes[i, ]
                MPname <- row[1]
                seatname <- row[2]
                index <- which(MPname == MPnames$MPname)
                if (length(index) == 1) 
                    result[index] <- "y"
                if (length(index) > 1) {
                    index <- index[which(MPnames$seatname[index] == 
                                     seatname)]
                    result[index] <- "y"
                }
                if (length(index) == 0) {
                    stop(paste("no match for", MPname, seatname))
                }
            }
        }
        if (!is.null(nrow(noes))){
            for (i in (1:nrow(noes))) {
                row <- noes[i, ]
                MPname <- row[1]
                seatname <- row[2]
                index <- which(MPname == MPnames$MPname)
                if (length(index) == 1) 
                    result[index] <- if (result[index] == "y") 
                        "b"
                    else "n"
                if (length(index) > 1) {
                    index <- index[which(MPnames$seatname[index] == 
                                     seatname)]
                    result[index] <- if (result[index] == "y") 
                        "b"
                    else "n"
                }
                if (length(index) == 0) {
                    stop(paste("no match for", MPname, seatname))
                }
            }
        }
        result
    }
    if (verbose) {
        cat("Processing", length(divs), "divisions \n")
        counter <- 0
    }
    result <- lapply(divs, function(div) {
        sort.division.lists(div, MPnames)
    })
    result <- as.data.frame(result)
    party <- MPnames$party
    result <- cbind(party, result)
    seatnames <- paste("(", as.character(MPnames$seatname), ")", 
                       sep = "")
    seatnames <- sub("\\(NA\\)", "", seatnames)
    row.names(result) <- paste(MPnames$MPname, seatnames)
    if (verbose) 
        cat("\n")
    result
}
    
write.votesheet <- function(x, file = NULL, keep.b = FALSE,
                             aye = "y", no = "n", novote = "-")
##  Keith Poole's OC codes are aye = "1", no = "6", novote = "0"
{
    party <- x$party
    votesheet <- as.matrix(x[,-1])
    ndivs <- ncol(votesheet)
    stored <- attributes(votesheet)
    if (!keep.b) votesheet <- sub("^b$", "-", votesheet)
    if (aye != "y") votesheet <- sub("^y$", aye, votesheet)
    if (no != "n") votesheet <- sub("^n$", no, votesheet)
    if (novote != "-") votesheet <- sub("^-$", novote, votesheet)
    attributes(votesheet) <- stored
    votestring <- apply(votesheet, 1, function(row)paste(row, collapse=""))
    if (!is.null(file)){
        oldwidth <- options()$width
        options(width = 50 + ndivs)
        sink(file)
        print(data.frame(party = party, votes = votestring))
        sink()
        options(width = oldwidth)
    } else print(data.frame(party = party, votes = votestring))
    invisible(NULL)
}

partyhues <-
        function(vec){
        result <- "black"
        result <- ifelse(vec=="Lab", "red", result)
        result <- ifelse(vec=="LD", "yellow", result)
        result <- ifelse(vec=="Con", "blue", result)
        result
    }

write.csv <- function(x, file = ""){
    write.table(x, file = file , sep = ",", col.names = NA)
}
