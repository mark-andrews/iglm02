slides = glm
slides2 = glm2

$(slides).pdf: $(slides).Rmd preamble.tex
	Rscript -e "rmarkdown::render(input = '$(slides).Rmd', output_file = '$(slides).pdf')"
	
$(slides2).pdf: $(slides2).Rmd preamble.tex
	Rscript -e "rmarkdown::render(input = '$(slides2).Rmd', output_file = '$(slides2).pdf')"
	
clean:
	rm -rf $(slides)_files
	rm -rf $(slides).tex
	rm -rf $(slides).log
	rm -rf $(slides2)_files
	rm -rf $(slides2).tex
	rm -rf $(slides2).log
		rm -f missfont.log
	
mrproper: clean
	rm -rf $(slides).pdf
	rm -rf $(slides2).pdf