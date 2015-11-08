This folder contains all files for Study 1.

1. Summary of Experiments

Experiment 1 (, in which
only reduced variants were learnt in the training), Experiment 2 (
, in which only full variants were learnt), and Experiment 3 (, in
which both variants were learnt). For each experiment, there were two groups:
One group being taught the words with spelling as additional information, and
the other not.

2. Experimental data

/data contains all experimental data. For each of the three experiments, there is:

- A 'lexdec' file, containing the data from the lexical decision task
- A 'voctest' file, containing the data from the vocabulary test
- A 'lextale' file, containing the data from the Lextale test
- A 'questinf' file, containing the data from the information filled in
by participants in the questionnaires

In addition, there is a 'phonotactics.txt' file, containing information about
the phonotactic legality of each of the 24 words that were learnt.

3. Reproducing the Results

For each of the three experiments there is one 'analysis' R-script and
one 'stats' R-script. To reproduce the up-to-date results (cf. 4.), 
the scripts are run entirely withouth any changes in the code.

3.1. Analysis scripts

Each of the three analysis scripts takes as input the raw experimental data for the
corresponding experiment (see 2.) and outputs:
- The data subsets, on which the analyses are based (most importantly
one subset for the analyses of accuracy, and one subset for the analyses of RTs)
- A descriptive analysis (tables and plots) of accuracy and RT data.

Each script is divided into subsections (e.g., creating new columns, removing high error
sets, etc.).  At each step, comments explain what is done exactly.
After running one entire analysis script, the statistical analyses can be performed with the
corresponding 'stats' script.

3.2. 'Stats' scripts

The scripts perform the statistical analyses, and each contains three subsections.
The first subsection performs the proficiency comparisons
between the -spelling and the +spelling group. The second subsection performs linear
regression on accuracy data, and the third subsection on RT data. In each subsection, comments
give more details on what is done exactly.

4. Drafts

/writing contains the most up-to-date versions of the method and the result section.
The result section contains the descriptive results from the analysis scripts (see 3.1) and
the statistical results from the 'stats' scripts (see 3.2)
