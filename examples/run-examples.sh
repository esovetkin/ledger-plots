#!/bin/bash

# make figures directory
mkdir -p figs

# generate dummy ledger entries
./dummy-example-generator.R

# indent ledger entries
emacs --batch -l ~/.emacs-minimal --eval='(progn (find-file "expenses.ledger") (ledger-mode) (mark-whole-buffer) (indent-region (point-min) (point-max) nil) (save-buffer))'
emacs --batch -l ~/.emacs-minimal --eval='(progn (find-file "food.ledger") (ledger-mode) (mark-whole-buffer) (indent-region (point-min) (point-max) nil) (save-buffer))'

# generate plots using ledger-plots
../ledger-plots -q "\^assets" \
                -f "cumsum :: function(x) {i<-1:length(x); predict(lm(cumsum(x)~i))}" \
                --ledger-options='-f expenses.ledger' \
                --output-pdf-ncol=1 \
                --output-pdf-nrow=1 \
                -n 1 \
                -o figs/assets.pdf

../ledger-plots --queries="-X EUR ;; -X USD" \
                -f "cumsum :: function(x) {i<-1:length(x); predict(lm(cumsum(x)~i))}" \
                --ledger-options='-f expenses.ledger \^assets' \
                --type "revalued" \
                --output-pdf-ncol=2 \
                --output-pdf-nrow=1 \
                --output-pdf-height="3.5" \
                -o figs/assets-revalued.pdf

../ledger-plots -q "\^expenses" \
                -f "monthly :: function(x) yearly(x)/12 :: function(x) { res <- 30*cumsum(x)/(1:length(x)); res[1:100]<-NA; res }" \
                --ledger-options='-f expenses.ledger -H -X EUR' \
                -n 4 \
                -o figs/expenses.pdf

../ledger-plots -q "\^expenses" \
                -f "monthly :: function(x) yearly(x)/12 :: function(x) { res <- 30*cumsum(x)/(1:length(x)); res[1:100]<-NA; res }" \
                --ledger-options='-f expenses.ledger -H -X EUR' \
                -C "tags" \
                -n 4 \
                -o figs/expenses-tags.pdf

../ledger-plots -q "\^expenses" \
                -C "alluvial" \
                -f "function(x) yearly(x)/12" \
                --output-pdf-ncol=1 \
                --output-pdf-nrow=1 \
                --ledger-options='-f expenses.ledger -H -X EUR' \
                -n 1 \
                -o figs/expenses-alluvial.pdf

../ledger-plots -q "\^food" \
                -f "monthly :: function(x) quarterly(x)/3 :: function(x) yearly(x)/12 " \
                --ledger-options="-f food.ledger -H -X EUR" \
                -n 4 \
                -o figs/food.pdf

../ledger-plots -q "\^food" \
                -f "monthly.price :: quarterly.price :: yearly.price" \
                --type "price" \
                --ledger-options="-f food.ledger -H -X EUR" \
                -n 4 \
                -o figs/food-price.pdf

../ledger-plots -q "\^food" \
                -f "monthly :: function(x) quarterly(x)/3 :: function(x) yearly(x)/12 " \
                --type "volume" \
                --ledger-options="-f food.ledger -H -X EUR" \
                -n 4 \
                -o figs/food-volume.pdf

../ledger-plots -q "\^food" \
                -f "function(x) yearly(x)/12" \
                --type "volume" \
                -C "alluvial" \
                --ledger-options="-f food.ledger -H -X EUR" \
                -n 4 \
                -o figs/food-volume-alluvial.pdf

../ledger-plots --generate-price-table \
                -q "food: -H -X EUR" \
                -f "min :: mean :: tail" \
                --ledger-options='-f food.ledger' \
                --conversion="1kg = 1l ;; 1kg = 1x ;; 1kg = 1qb" \
                -o "figs/price-table.tex"

# convert images to png
files=()
files+=("figs/assets.pdf")
files+=("figs/assets-revalued.pdf")
files+=("figs/expenses.pdf")
files+=("figs/expenses-tags.pdf")
files+=("figs/expenses-alluvial.pdf")
files+=("figs/food.pdf")
files+=("figs/food-price.pdf")
files+=("figs/food-volume.pdf")
files+=("figs/food-volume-alluvial.pdf")

for file in ${files[@]}
do
    convert -density 200 "${file}" "${file%.*}.png"
done
