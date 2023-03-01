build: pdf

pdf: main.tex out
	pdflatex --jobname=Algo_Surkis_Anton_HW03 --shell-escape --output-directory=out $<

out:
	mkdir -p out

clean:
	rm -rf out
