library("gtools")
# make a deck of cars
suits <- c("Hearts", "Spades", "Diamonds", "Clubs")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number=numbers, suit=suits)
deck <- paste(deck$number, deck$suit)
deck

# probability of king
kings <- paste("King", suits)
mean(deck %in% kings)

# probability of a king if we already have a king
hands <- permutations(52, 2, v=deck)
first_cards <- hands[,1]
second_cards <- hands[,2]
mean(first_cards %in% kings & second_cards %in% kings) / mean(first_cards %in% kings)

# probability of ace and facecard blackjack
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number=facecard, suit=suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck)
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,1] %in% facecard & hands[,2] %in% aces))

# monte carlo probability of ace and facecard blackjack
results <- replicate(10000,
  {
    hand <- sample(deck, 2, replace=TRUE)
    (hand[1] %in% aces & hand[2] %in% facecard) | (hand[1] %in% facecard & hand[2] %in% aces)
  }
)
mean(results)

# monte carlo same birthday chance
same_bday_prob <- function(n, B=10000){
  results <- replicate(B, {
    bdays <- sample(1:365, n, replace=TRUE)
    any(duplicated(bdays))
  })
  mean(results)
}
n <- 1:60
prob <- sapply(n, same_bday_prob)

# exact same bday chance
exact_bday_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365
  1-prod(prob_unique)
}
eprob <- sapply(n, exact_bday_prob)
plot(n, prob)
lines(n, eprob, col="red")
