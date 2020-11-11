set -e 

Rscript -e "install.packages('devtools')"
Rscript -e "devtools::install_github('peder2911/evallib')"
Rscript -e "devtools::install_github('peder2911/timelib')"
Rscript -e "devtools::install_github('peder2911/imlib')"
Rscript -e "install.packages('pROC')"

Rscript -e "devtools::install_github('https://github.com/rich-iannone/UnidecodeR')"

find . -name "*.R" -exec perl -ne 'print "$1\n" if /(?<=library\()([^\)]+)/' {} + \
   |awk '!/dplyr/&&!/sf/&&!/parallel/&&!/splines/&&!/timelib/&&!/evallib/&&!/imlib/'\
   |awk '{$1=$1;print}'\
   |sort|uniq\
   |Rscript -e "install.packages(readLines(file('stdin')))"
