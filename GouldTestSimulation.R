###############################################################################
##
## Implementing Gould's (1989) metric of brokerage centrality. This score
##  identifies nodes' roles as 'brokers' - network members that directly
##  connect different communities within the network.
##
## The current implementation exactly copies Gould's original algorithm, which
##  has the feature/limitation(?) of only taking 2-step paths into account
##  when identifying nodal centrality. This means that nodes playing a more
##  indirect intermediary role are ignored. Future work may relax this
##  requirement.
##
###############################################################################

################################################################################
##
## Set up working directory and load libraries.
##
## Source GouldBrokerage script. This contains one function, GouldBrokerage(),
##  that intakes a binary matrix object and outputs a graph object with
##  nodes sized by Gould's community brokerage score. See GouldBrokerage.R
##  for further documentation.
##
################################################################################

rm(list = ls())
if(!'pacman' %in% installed.packages()){
  install.packages('pacman')
}
pacman::p_load(igraph, data.table, expm)

## Set working directory - CHANGE THIS WHEN RUNNING ON A NEW SYSTEM
os_detect <- Sys.info()['sysname']

if (os_detect == 'Darwin'){ ## (Darwin = Mac OS, for some reason)
  setwd('/Users/jesse/Dropbox/NetworkBrokerage')
  source('/Users/jesse/Dropbox/NetworkBrokerage/GouldBrokerage.R')
  
} else if (os_detect == 'Linux'){
  setwd('/media/jesse/Files/Dropbox/NetworkBrokerage')
  source('/media/jesse/Files/Dropbox/NetworkBrokerage/GouldBrokerage.R')
  
} else if (os_detect == 'Windows'){
  setwd('C:/Users/Jesse/Dropbox/NetworkBrokerage')
  source('C:/Users/Jesse/Dropbox/NetworkBrokerage/GouldBrokerage.R')
}


################################################################################
##
## Replicate Gould's brokerage example from his 1989 paper.
##
################################################################################

## Generate test matrix
testmat <- matrix(0, nrow = 8, ncol = 8)
testnames <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
rownames(testmat) <- testnames
colnames(testmat) <- testnames

## Generate ties
testmat[1, c(2:3, 6:8)] <- 1
testmat[2, c(3:5, 7)] <- 1
testmat[3, c(4:5)] <- 1
testmat[4, c(5)] <- 1
# testmat[5, c()]
testmat[6, c(7:8)] <- 1
testmat[7, c(8)] <- 1

## Symmetrize matrix
testmat[lower.tri(testmat)] <- t(testmat)[lower.tri(testmat)]

## Generate brokerage scores and plot
gould_net <- GouldBrokerage(testmat)

## Examine brokerage scores
print(V(gould_net)$brokerage)


################################################################################
##
## Simulate an Erdos-Renyi graph and analyze
##
################################################################################
testnet <- erdos.renyi.game(n = 50, p.or.m = .1, type = 'gnp')
testout <- GouldBrokerage(as.matrix(as_adj(testnet)))


################################################################################
##
## Simulate a symmetric/undirected, binary network with random data.
##  Note: setting the random seed ensures that repeated runs of the script
##  will produce the same 'random' network each time.
##
################################################################################

set.seed(777)

## Network size
n <- 25

## Generate network
in_matrix <- matrix(sample(c(0,1), n^2, prob = c(.95, .05), replace = T), nrow = n)

## Force symmetry
in_matrix[lower.tri(in_matrix)] <- t(in_matrix)[lower.tri(in_matrix)]

## Set diagonal to 0 (no self-ties)
diag(in_matrix) <- 0


################################################################################
##
## Calculate Gould's brokerage centrality scores for all nodes in the matrix.
##
################################################################################

broker_net <- GouldBrokerage(in_matrix)

## Some optional comparison plots: compare Gould's brokerage centrality to 
##  betweenness centrality scores (standard calculation).

# plot(broker_net
#      , vertex.size = V(broker_net)$size
#      , vertex.label = NA
# )
# 
# plot(broker_net
#      , vertex.size = V(broker_net)$between
#      , vertex.label = NA
#      )

## Correlate Gould's and standard betweenness scores.
## Usually very high correlation, >0.95
cor(V(broker_net)$size, V(broker_net)$between)

## Correlate *order/rank* of Gould's and standard betweenness scores.
## Generally these don't correlate highly - hovers +/- 0.3
cor(order(V(broker_net)$size), order(V(broker_net)$between))



################################################################################
##
## Test on some real data: Noordin's Indonesian terror network data set.
##
################################################################################

noordin <- fread('./noordin.csv')


keyterms <- c(
  'ORGAN', 'SCHOOL', 'CLASS', 'COMMUN', 'KIN', 'TRAIN', 'EMPLOY', 'OPERAT'
  , 'FRIEND', 'RELIG', 'SOUL', 'PLACE', 'MEET')

key_brokers <- data.table(
  network = rep(NA_character_, length(keyterms))
  , TopBroker = rep(0.0, length(keyterms))
  , BrokerScore = rep(0.0, length(keyterms))
  , NumBrokers = rep(0.0, length(keyterms))
)

for(term in 1:length(keyterms)){
  organ_bimat <- as.matrix(
    noordin[, grep(keyterms[term], names(noordin))
            , with = F]
    )
  organ_soc <- organ_bimat %*% t(organ_bimat)
  organ_soc <- ifelse(organ_soc > 0, 1, 0)
  diag(organ_soc) <- 0
  
  noordin_brokerage <- GouldBrokerage(organ_soc)
  
  key_brokers[term, ] <- list(
    keyterms[term]
    , which.max(V(noordin_brokerage)$brokerage)
    , max(V(noordin_brokerage)$brokerage)
    , length(which.max(V(noordin_brokerage)$brokerage))
    )
  
}

setkeyv(key_brokers, c('TopBroker', 'BrokerScore'))

key_brokers


