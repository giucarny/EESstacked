The 2019 EES Stacked Data Matrix
================
2022-07-28

This repository contains the code for building the stacked data matrix
(SDM) of the 2019 European Election Studies
([EES](http://europeanelectionstudies.net/)) voter study. The creation
of this SDM is part of the research activities of the
[ProConEU](https://www.mzes.uni-mannheim.de/proconeu/) project, an
academic research effort[^1] analysing the enlarging gaps between
proponents and opponents of the European Union (EU) in terms of party
politics, citizen politics, and social media communication. The data
pipeline and the general workflow were completed mostly between July
2021 and January 2022 making use of [R](https://cran.r-project.org/)
version 4.1-4.2.

# What is an SDM

An SDM consists of a long format data matrix in which each row
represents the (dyadic) relationship between two sets of relevant
elements.

Among its applications, this data matrix has been extensively used for
the study of voting behaviour. In this setting, the SDM observations are
usually voter-party dyads, namely dyadic relationships between
individual voters and the relevant vote choices available to each
individual voter in a given election.

[^1]:  Funded by the German Federal Ministry of Education and Research
    ([BMBF]((https://www.bmbf.de/bmbf/en/home/home_node.html)))
