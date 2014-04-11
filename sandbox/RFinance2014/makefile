# http://robjhyndman.com/hyndsight/makefiles/
# https://www.cs.umd.edu/class/fall2002/cmsc214/Tutorial/makefile.html

# List out the R files used
RFILES := data_analysis.R optimize.R optimization_analysis.R

# Rout indicator files to show R file has run
OUT_FILES:= $(RFILES:.R=.Rout)


all: slidy_presentation.html presentation.Rmd $(OUT_FILES)

# Generate slidy presentation from markdown file
slidy_presentation.html: presentation.md
	pandoc -t slidy -s presentation.md -o slidy_presentation.html

# Generate markdown file from R markdown file
presentation.md: presentation.Rmd
	Rscript -e "library(knitr); knit('presentation.Rmd')"

# Data analysis script
data_analysis.Rout: data_analysis.R
	R CMD BATCH data_analysis.R

# Optimization analysis script to analyze results of optimization
optimization_analysis.Rout: optimization_analysis.R optimize.R
	R CMD BATCH optimization_analysis.R

# Run optimizations
optimize.Rout: optimize.R
	R CMD BATCH optimize.R

lwShrink.Rout: R/lwShrink.R
	R CMD BATCH R/lwShrink.R

# Use Rscript to run the necessary R files as an alternative to R CMD BATCH
run:
	Rscript data_analysis.R
	Rscript optimize.R
	Rscript optimization_analysis.R

clean:
	rm *.Rout
	rm *.html