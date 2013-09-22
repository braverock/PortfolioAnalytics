# Project space for developing code for Factset 2013 conference.  

## Constructing Portfolios of Dynamic Strategies using Downside Risk Measures
Peter Carl, Hedge Fund Strategies, William Blair & Co.

In this session, we'll discuss portfolio construction within the context of portfolios of dynamic strategies. The speaker will use an approach that identifies several sets of objectives to establish benchmark, target, and nearby portfolios in a variety of ways and with complex constraints, including some that equalize or budget risks using downside measures of risk. We will then examine the ex-post results through time and identify the conditions under which certain objectives might be expected to do well or poorly. Any investor with complex, layered objectives and who is subject to a variety of real-world constraints may find useful elements in the methods presented.

For presentation at FactSet's 2013 US Investment Process Symposium
November 10 - 12 , 2013

# SETUP
The workspace is set up in the following way:
R script files reside in the project root directory
README: this description file covering the objectives of the project and any instructions
notebook.md: file to track progress
./R      contains files with un-packaged function definitions (only functions - 
         no script code to be run)
./data   contains data used in the analysis; treated as read only - do not write
         data into this directory.
./data/README should cover who downloaded what data from where and when
./cache  contains processed data files and intermediary results to be processed
./results  contains output, figures, or other generated files.  Should be able to delete the contents and regenerate them
./docs contains documents with text discussing results
./logs: contains logging output 
./src: contains non-R source code where needed
./bin: compiled binaries or scripts

May want to organize subdirectories in results and data chronologically at some point

# HOWTO
To create PDF of slides:
$ pandoc symposium-slides-2013.Rmd -t beamer -o symposium-slides-2013.pdf --template=beamer.template