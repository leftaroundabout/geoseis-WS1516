PDFs	=  prs.pdf

all: $(PDFs)

LaTeXC = bin/pdflatex_halt-on-error
# BIBTEX = biber

SILENT = >/dev/null 2>/dev/null


prs.pdf: prs.tex
	$(LaTeXC) prs.tex

