Run scripts from plotscripts / tablescripts folders in the root directory. They
source the prep.R script to prepare the data for visualization.

After installing dependencies, run the `run.sh` file for a complete replication.
Install dependencies with `pip -r requirements.txt` and `./findDeps.sh`.
You could also run with docker: To do that, run `runDocker.sh`.

In the paper:

Tables:
   Table 1 - tables/all_eval.tex
   Table 2 - tables/regionwise.tex
   Table A-2 - tables/highest_predicted.tex
Figures:
   Figure 1: plots/rocs.pdf / plots/precrec.pdf
   Figure 2: maps/either_mean.png, maps/major_mean.png
   Figure 3: plots/timeline.pdf
   Figure A-1: plots/catmap.pdf
   Figure A-2: plots/Rplots*.pdf 
   Figure A-3: plots/mcp_either.png plots/mcp_2018.png 
