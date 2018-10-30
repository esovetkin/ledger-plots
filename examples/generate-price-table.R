#!/usr/bin/env Rscript

require("ledgerplots")

generate.price.table(query="-H -X EUR",
                     ofile="price-table.tex",
                     ledger.options="-f food.ledger")

system("pdflatex price-table.tex && pdflatex price-table.tex")
