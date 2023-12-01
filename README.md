#####
#
# Copyright (c) Joseph L. Breeden, 2023
#
# This is the readme file for how to use the AI evolution scripts
#
# This code is provided for research purposes only. 
# It may not be used for any commercial purpose without written permission of the author.
# This code is provided without any warranty or promise of effectiveness.
#
#####

The code is written in a language of “ants” and “guards”. In my research paper, these are referred to as “robots” and “humans”.  The context is interesting, but does not change the code.

1. initi_brain_train.R is used to generate data based upon a hard-coded decision tree. Then an over-parameterized neural network is created to replicate the movements of the ants, essentially translating the decision tree to a neural network.

2. goal_evolv.R is used to run specific simulations for the evolution of the AI agents. Parameters are changed to experiment with the course of the simulations.

All other files are utility functions used in these two steps.

