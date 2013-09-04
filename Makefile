### Makefile to prepare panelAR package

# prepare the package for release
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

# Rules
all: $(PKGSRC).pdf clean

$(PKGSRC).tex: vignettes/$(PKGSRC).Rnw
	cd vignettes;\
	echo "Sweave(\"panelAR.Rnw\", debug=TRUE, eval=TRUE)" | R --slave

### Rd2tex here
Rd2tex: man/panelAR.Rd man/summary.panelAR.Rd man/plot.panelAR.Rnw man/runs.analysis.Rnw
 	echo 'library(tools); Rd2latex(parse_Rd("man/panelAR.Rd"), out="vignettes/man-panelAR.tex"); Rd2latex(parse_Rd("man/summary.panelAR.Rd"), out="vignettes/man-summary.panelAR.tex"); Rd2latex(parse_Rd("man/plot.panelAR.Rd"), out="vignettes/man-plot.panelAR.tex"); Rd2latex(parse_Rd("man/runs.analysis.Rd"), out="vignettes/man-runs.analysis.tex")' | R --slave

$(PKGSRC).pdf: $(PKGSRC).tex Rd2tex
	cd vignettes;\
	pdflatex $(PKGSRC);\
	bibtex $(PKGSRC);\
	pdflatex $(PKGSRC);\
	pdflatex $(PKGSRC);\
	pdflatex $(PKGSRC)

build:	
	cd ..;\
	R CMD build $(PKGSRC) --resave-data

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran                 

clean:
	cd vignettes;\
	rm -rf *.aux *.tex *.bbl *.blg *.bcf *.log *.out *.rel *.toc *.idx