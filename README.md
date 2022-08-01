The 2019 EES Stacked Data Matrix
================
2022-07-28

This repository contains the code for building the stacked data matrix
(SDM) of the 2019 European Election Studies
([EES](http://europeanelectionstudies.net/)) voter study. The creation
of this SDM is part of the research activities of the
[ProConEU](https://www.mzes.uni-mannheim.de/proconeu/) project, an
academic research effort analysing the enlarging gaps between proponents
and opponents of the European Union (EU) in terms of party politics,
citizen politics, and social media communication. The data pipeline and
the general workflow were completed mostly between July 2021 and January
2022 making use of [R](https://cran.r-project.org/) version 4.1-4.2.

# What is an SDM

An SDM consists of a long format data matrix in which each row
represents the (dyadic) relationship between two sets of relevant
elements.

Among its applications, this data matrix has been extensively used for
the study of voting behaviour. In this setting, the SDM observations are
usually voter-party dyads, namely dyadic relationships between
individual voters and the relevant vote choices available to each
individual voter in a given election.

The reason behind the development of the SDM for voting behaviour
studies is that it allows to go beyond problems related to the
comparability of vote choice across different party systems, especially
multi-party ones. By relying on party-voted dyads the SDM allows to
address research questions concerning *entire* party systems, thus
enhancing the possibility to develop longitudinal and/or cross-national
comparative analyses without:

1.  Arbitrarily reducing the number of relevant vote choices (parties)
    of the system;
2.  Reducing the vote alternatives available in a given election to a
    single property of said alternatives (e.g., party positions on the
    Left-Right continuum).

Hence, the SDM allows to include in the analyses all the relevant
individual-, party-, and context-level factors that might affect the
vote choice.

# How to build the 2019 EES SDM

1.  Fork the repository;
2.  Download the [EES 2019 voter study
    dataset](http://europeanelectionstudies.net/european-election-studies/ees-2019-study/voter-study-2019),
    and the related [codebook](https://access.gesis.org/dbk/67448)
    (rename the latter ‘ZA7581\_codebook.csv’);
3.  Create a new folder in
    [`Data`](https://github.com/giucarny/EESstacked/tree/main/Data),
    rename it ‘EES2019’ and transfer/paste the EES data here;  
4.  Run the
    [`EES2019_stack`](https://github.com/giucarny/EESstacked/blob/NewREADME/Scripts/EES2019_stack.R)
    script.
