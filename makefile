LFILE = README

all: knith #open 

knith: $(LFILE).Rmd
	echo "rmarkdown::render('$(LFILE).Rmd',output_file='$(LFILE).html')" | R --no-save -q

knitr: $(LFILE).Rmd
	echo "rmarkdown::render('$(LFILE).Rmd',output_file='$(LFILE).md')" | R --no-save -q

open: $(LFILE).html
	xdg-open $(LFILE).html &

check:
	Rscript -e 'library(pkgcheck); checks <- pkgcheck(); print(checks); summary (checks)'

clean:
	rm -rf *.html *.png README_cache 
