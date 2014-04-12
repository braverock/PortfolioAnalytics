# http://robjhyndman.com/hyndsight/makefiles/
# https://www.cs.umd.edu/class/fall2002/cmsc214/Tutorial/makefile.html

# List the R files used
RFILES := data_prep.R data_analysis.R optimize.R optimization_analysis.R

# Rout indicator files to show R file has run
# R CMD BATCH will generate .Rout files after running
OUT_FILES:= $(RFILES:.R=.Rout)


all: $(OUT_FILES) slidy_presentation.html presentation.Rmd

# Generate slidy presentation from markdown file
slidy_presentation.html: $(RFILES) $(OUT_FILES) presentation.md
	pandoc -t slidy -s presentation.md -o slidy_presentation.html

# Generate markdown file from R markdown file
presentation.md: presentation.Rmd
	Rscript -e "library(knitr); knit('presentation.Rmd')"

# Data analysis script
data_analysis.Rout: data_analysis.R
	R CMD BATCH --vanilla data_analysis.R

# Optimization analysis script to analyze results of optimization
optimization_analysis.Rout: optimization_analysis.R optimize.R
	R CMD BATCH --vanilla optimization_analysis.R

# Run optimizations
optimize.Rout: optimize.R
	R CMD BATCH --vanilla optimize.R

# Data prep
data_prep.Rout: data_prep.R
	R CMD BATCH --vanilla data_prep.R

lwShrink.Rout: R/lwShrink.R
	R CMD BATCH --vanilla R/lwShrink.R

# Use Rscript to run the necessary R files as an alternative to R CMD BATCH
runR:
	Rscript data_prep.R
	Rscript data_analysis.R
	Rscript optimize.R
	Rscript optimization_analysis.R

clean:
	rm -f *.Rout
	rm -f data_figures/*.png
	rm -f optimization_figures/*.png
	rm -f optimization_results/*.rda
	rm -f *.md
	rm -f *.html
	