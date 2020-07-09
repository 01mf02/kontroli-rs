all: structure.svg

clean: structure.pdf.clean
	rm -f structure.svg*

%.pdf.clean:
	rm -f $*.aux $*.log $*.pdf

%.pdf: %.tex
	pdflatex $<

# To get a nice, properly scalable SVG from the resulting file,
# delete the portion `width="..." height="..."` from the `<svg>` tag.
%.svg.fixed: %.pdf
	pdf2svg $< $@

%.svg: %.svg.fixed
	sed -e 's/width="[^"]*" //' -e 's/height="[^"]*" //' $< > $@
