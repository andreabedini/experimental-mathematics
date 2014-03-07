PDFDIR = pdfs
objects = $(addprefix $(PDFDIR)/,$(patsubst %.md,%.pdf,$(wildcard Week?.md)))

filters = pandoc-citeproc ./no-indent.py ./code-blocks ./headers

all: $(objects)

$(PDFDIR)/%.pdf: %.md tufte.latex 
	pandoc $(addprefix --filter ,$(filters)) --template=tufte --latex-engine=xelatex -f markdown -t latex $< -o $@
