What's contained within this archive:
ALL/ #study level data, example analyses, and wrapper script for ALL
CNS/ #same, for CNS
ST/  #same, for all solid tumors
functions/ #function library for the pipeline
resources/ #other things like output table templates, agent tracker, etc.


EXAMPLE ANALYSIS - ALL
Run the wrapper script here:
	ALL/1806/ALL.1806.wrapper.R
	just update the path for the variable "parent.dir" at the top
When complete, it should create these files:
ALL/1806/
	1806-<model>plots.pdf	 #quick and dirty ggplots for each model.
	1806-<model>/
		image.RData	#model-specific analysis R Data object; contains an entire snapshot of the analysis
		summary.xlsx	#primary analysis results which were delivered to the research sites
		summary2.txt	#more recent formatting of analysis results meant for compiling across all studies. These were never shared with the RSs
	PNGs/	#publication ready PNGs; survival, weight, cd45 for each model
	CSVs/	#tables formatted for graphPad and excel




EXAMPLE ANALYSIS - ST
Wrapper script here:
	ST/1816/ST.wrapper.R
	Similar to ALL wrapper, update the "parent.dir" variable before running
When complete, should create these files:
ST/1816/
	1816-<model>plots.pdf	 #quick and dirty ggplots for each model.
	1816-<model>/
		image.RData	#model-specific analysis R Data object; contains an entire snapshot of the analysis
		summary.xlsx	#primary analysis results which were delivered to the research sites
		summary2.txt	#more recent formatting of analysis results meant for compiling across all studies. These were never shared with the RSs
	PNGs/	#publication ready PNGs; survival, weight, volume for each model
	CSVs/	#tables formatted for graphPad and excel



EXAMPLE ANALYSIS - CNS
Wrapper script here:
	CNS/1817/1817.CNS.config.R
	update "parent.dir"
When complete, should create these files:
CNS/1817/
	1817-<model>plots.pdf	 #quick and dirty ggplots for each model.
	1817-<model>/
		image.RData	#model-specific analysis R Data object; contains an entire snapshot of the analysis
		summary.xlsx	#primary analysis results which were delivered to the research sites
		summary2.txt	#more recent formatting of analysis results meant for compiling across all studies. These were never shared with the RSs
	PNGs/	#publication ready PNGs; survival for each model
	CSVs/	#tables formatted for graphPad and excel









