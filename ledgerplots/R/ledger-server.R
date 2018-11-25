
#' @title initialise pipes to read queries from ledger
#'
#' In linux it is possible to use named pipes to interact with the
#' REPL interface of ledger.
#'
#' The advatange of this is that ledger files are read only once.
#'
#' @param flags flags supplied to ledger
init.ledger.server <- function(flags = "", ledger.path = NULL) {

  # input file is a named pipe
  input <- tempfile()
  code <- system2(command="mkfifo",args=input,stderr=FALSE)

  # check exit code
  if (code != 0) {
    stop("cannot mkfifo")
  }
  
  # pid of the process that keeps cat fifo alive
  pid3 <- system(paste0("sleep 1d > ",input," & echo $!"),
                 intern = TRUE)

  # output file is a regular file
  output <- tempfile()
  
  # start listening for requests, store pid of the ledger process
  pid_file <- tempfile()
  pid2 <- system(paste0("(cat ",
                        input,
                        " & echo $! >&3 ) 3>",
                        pid_file,
                        " | ledger > ",
                        output,
                        " &  echo $!"),
                 intern = TRUE)

  # keep pid of the cat fifo process
  pid1 <- system(paste0("cat ",pid_file),intern=TRUE)

  return(list("input"=input,
              "output"=output,
              "pids"=c(pid1,pid2,pid3)))
}
