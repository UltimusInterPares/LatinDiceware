library(dplyr)
floor(runif(1, min = 11111, max = 66666))

tableOneTwo <- tibble(
  .rows = 6,
  c1 = c('A', 'G', 'M', 'S', 'Y', '4'),
  c2 = c('B', 'H', 'N', 'T', 'Z', '5'),
  c3 = c('C', 'I', 'O', 'U', '0', '6'),
  c4 = c('D', 'J', 'P', 'V', '1', '7'),
  c5 = c('E', 'K', 'Q', 'W', '2', '8'),
  c6 = c('F', 'L', 'R', 'X', '3', '9')
)


# Having two cells map to a space feels sketch
tableThreeFour <- tibble(
  .rows = 5,
  c1 = c('a', 'g', 'm', 's', 'y'),
  c2 = c('b', 'h', 'n', 't', 'z'),
  c3 = c('c', 'i', 'o', 'u', '~'),
  c4 = c('d', 'j', 'p', 'v', '_'),
  c5 = c('e', 'k', 'q', 'w', ' '), 
  r6 = c('f', 'l', 'r', 'x', ' ') 
)

tableFiveSix <- tibble(
  .rows = 5,
  c1 = c('!', '&', '+', '|', '<'),
  c2 = c('@', '*', '[', '`', '>'),
  c3 = c('#', '(', ']', ';', '/'),
  c4 = c('$', ')', '{', ':', '?'),
  c5 = c('%', '-', '}', "\'", '.'),
  r6 = c('^', '=', '\\', '\"', ',')
)

wordList <- read.csv('Latin.csv', header = FALSE)

# Finds position of random symbol within the passphrase
getPosition <- function(Length = 5) {
  symbolPosition <- floor(runif(1, min = 1, max = Length))
  return(symbolPosition)
}

# Generates the symbol
getSymbol <- function() {
  symbol = ''
  symbolLength <- floor(runif(1, min = 1, max = 6))
  
  # Runs once for each character in the phrase.
  # The phrase length is set to a random number
  # between 2 and 10
  for(i in c(0:symbolLength)) {
    # Rolls 2d6 to determine what table and column to use.
    # Third die is rolled later, to make sure it doesn't
    # return an out-of-boud value.
    firstRoll <- floor(runif(1, min = 1, max = 6))
    secondRoll <- floor(runif(1, min = 1, max = 6))
    if (firstRoll %in% c(1,2)) {
      thirdRoll  <- floor(runif(1, min = 1, max = 6))
      
      symbol <- paste(symbol,
                      tableOneTwo[[thirdRoll, secondRoll]],
                      sep = '')
    } else if (firstRoll %in% c(3,4)) {
      thirdRoll  <- floor(runif(1, min = 1, max = 5))
      
      symbol <- paste(symbol,
                      tableThreeFour[[thirdRoll, secondRoll]],
                      sep = '')
    
    } else if (firstRoll %in% c(5,6)) {
      thirdRoll  <- floor(runif(1, min = 1, max = 5))
      
      symbol <- paste(symbol,
                      tableFiveSix[[thirdRoll, secondRoll]],
                      sep = '')
    }
  }
  
  return(symbol)
}

# https://stackoverflow.com/a/12539805
# grepl() won't return logical(0) for false
checkSymbol <- function(symbol) {
  qual <- grepl("\\d+", symbol) & grepl("\\W+", symbol) & grepl("[:upper:]+", symbol)
  return(qual)
}

testCheck <- function(symbol){
  symbol <- getSymbol()
  print(symbol)
  checkSymbol(symbol)
}

# generates a portion of the passphrase
getPhrase <- function() {
  roll <- ''
  phrase <- ''
  for (i in c(0:4)) {
    roll <- paste(roll, floor(runif(1, min = 1, max = 6)), sep = '')
  }
  
  roll <- as.numeric(roll)
  
  #phrase <- as.character(wordList[which(wordList == roll),2])
  phrase <- wordList[[which(wordList == roll),2]]
  return(phrase) 
}

getPassPhrase <- function(Length = 5){
  symbolPosition <- getPosition(Length)
  compliant <- FALSE
  
  while (compliant == FALSE) {
    symbol <- getSymbol()
    compliant <- checkSymbol(symbol)
  }
  
  passPhrase <- ''
  for (i in c(1:Length)) {
    passPhrase[i] <- getPhrase()
  }
  
  # Inserts the symbol chunk
  passPhrase[symbolPosition] <- symbol
  
  # Converts the vector to a string
  passPhrase <- toString(passPhrase)
  
  # Removes columns from the string
  passPhrase <- gsub(',', '', passPhrase)
  
  # Makes the results available
  return(passPhrase)
}

getPhraseLength <- function(passPhrase) {
  phraseLength <- nchar(passPhrase)
  return(phraseLength)
}

analyzePhrase <- function(passPhrase, phraseLength) {
  charEntropy <- (log(95)/log(2))
  phraseEntropy <- charEntropy * phraseLength
  writeLines(paste('Your pass phrase has ', charEntropy, ' bits of entropy per character.'))
  writeLines(paste('Your pass phrase has a total of ', phraseEntropy, ' bits of entropy.'))
  friends <- 2^phraseEntropy
  #writeLines(paste(entropy, ' other possible combinations share this entropy.'))
}

Generate <- function(Length = 5){
  passPhrase <- getPassPhrase(Length)
  phraseLength <- getPhraseLength(passPhrase)
  
  analyzePhrase(passPhrase, phraseLength)
  
  return(passPhrase)
}
