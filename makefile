LFILE = README
YFILE = data-raw/insert-yaml-in-use-action
VIGNETTE = environment

#all: insert knith #open 
all: init readme vignette

doc: ## Update package documentation with `roxygen2`
	Rscript -e "roxygen2::roxygenise()"; \

init: ## Initialize pkgdown site
	echo "pkgdown::init_site()" | R --no-save -q

pkgdown: ## Build entire pkgdown site
	echo "pkgdown::build_site()" | R --no-save -q

vignette: ## Build pkgdown article
	echo "pkgdown::build_article('$(VIGNETTE)',quiet=FALSE)" | R --no-save -q

knith: $(LFILE).Rmd ## Render README as HTML
	echo "rmarkdown::render('$(LFILE).Rmd',output_file='$(LFILE).html')" | R --no-save -q

knitr: $(LFILE).Rmd ## Render README as markdown
	echo "rmarkdown::render('$(LFILE).Rmd',output_file='$(LFILE).md')" | R --no-save -q

#open: $(LFILE).html
#	xdg-open $(LFILE).html &
open: ## Open main HTML vignette in browser
	xdg-open docs/articles/$(VIGNETTE).html &

check: ## Run `rcmdcheck`
	Rscript -e 'rcmdcheck::rcmdcheck()'

pkgcheck: ## Run `pkgcheck` and print results to screen.
	Rscript -e 'library(pkgcheck); checks <- pkgcheck(); print(checks); summary (checks)'

test: ## Run test suite
	Rscript -e 'testthat::test_local()'

clean:
	rm -rf *.html *.png README_cache 

help: ## Show this help
	@printf "Usage:\033[36m make [target]\033[0m\n"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'
