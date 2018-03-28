# Public code snippets

Some small, self-contained code snippets created for other projects.

## Snippets

### GouldBrokerage/Signed/TestSimulation
Calculates node-level centrality scores based on Gould's brokerage metric. This works in two steps: first divide the network into communities, and then find the subset of nodes that serve as 'bridges' spanning different communities. The more communities a node bridges, and the greater the number of ties, the higher its brokerage centrality.

### Phoenix2Net
Converts historic Phoenix event data files from the University of Illinois' Cline Center for Democracy (http://www.clinecenter.illinois.edu/data/event/phoenix/) into temporal network objects for analysis.

### TriadicBalance / network_balance
A systematic measure of triadic balance for network analysis in R. Intakes signed networks (networks with positive and negative weighted ties) and returns measures of balance at the node, dyad, and network level. This script can also estimate balance over different structures (triadic through _k_-adic) and implement some different ways of measuring balance.
