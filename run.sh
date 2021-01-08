set -e

for f in $(ls -d plotscripts/* ; ls -d tablescripts/*)
do
   Rscript $f
done

python3 discrep_maps/main.py 
python3 discrep_maps/main.py major

Rscript error_analysis_plot/pull.R
Rscript error_analysis_plot/errplt.R cache/p2010_2018.csv plots/mcp_either.jpg combined either_actual label
Rscript error_analysis_plot/errplt.R cache/p2016_2018.csv plots/mcp_2018.jpg combined either_actual label

rm Rplots*
Rscript sepplots/sep.R
mv Rplots* plots
