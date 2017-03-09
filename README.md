# ledger-plots - generate plots for your ledger

Make plots for your ledger entries and save them to a pdf file.

Obligatory screenshot:
![plot of some expenses account](screenshot.jpg?raw=true)

## Dependencies

 * [ledger](http://ledger-cli.org/)

 * [R](https://www.r-project.org/) does all plots

## Installation

In order to use the ledger-plots it is sufficient to copy the
ledger-plots file. Run
```
ledger-plots --install-dependencies
```
to install the missing R-packages.

Alternatively, there is an AUR package for archlinux:
ledger-plots-git.

See full options list by saying
```
ledger-plots --help
```

## Queries and functions

A query is being passed to ledger for getting a list of transactions
to be used in plots.

For example, to make plots including only "assets" ledger accounts
call
```
ledger-plots -q "^assets: -X EUR"
```

Note that it is important to convert all transactions to a common
currency. This can be done by means of ledger itself with "-X"
argument (all examples given here convert to euro).

One may specify several queries, separated by two semi-colons (";;")
```
ledger-plots -q "^assets: -X EUR;;^liability: -X EUR"
```

To be more precise, the following ledger command is being called:
```
ledger csv <query>
```

The ledger calls simply output a list of transactions in a csv
format. In order to make useful plots one needs to compute some useful
function (statistic) on these transactions. But which function to
choose?  Well, that depends on the type of transaction you are
plotting.

For instance, for assets it makes sense to calculate accumulated
sum. The function can be specified in --functions argument
```
ledger-plots -f "cumsum" -q "^assets: -X EUR"
```

For plotting expenses it makes sense to calculate accumulated sum over
a period of 30 days (or 7 day, whatever)
```
ledger-plots -f "function(x) {filter(x,rep(1,30),sides=1)}" -q "^expenses: -X EUR"
```

The ready to use functions are coming. So that this can be used by
people who doesn't know R syntax.

## Food prices

In case you also have a separate ledger file where you keep you
categorised expenses on food, you can generate a table with a cheapest
price for a given category, plus the shop.

In order to do this, with a comment to transaction in ledger you
should also specify weight, number of pieces, etc. For example:
```
2016/05/21 edeka
    Food:Spirits:Campari                       10.99 EUR ; 0.75l
    Food:Meat:Sausage:Geflügel                  4.78 EUR ; 4x @ 200g
    Food:Beverages:Juice                        1.49 EUR ; 1l
    Food:Kondenzmilk                            2.58 EUR ; 2x @ 400g
    Food:Mushrooms:Champignons                  2.14 EUR ; 0.428kg
    Food:Mango                                  5.96 EUR ; 4x
    Bath:Toothpaste                             4.98 EUR ; 2x @ 75ml elmex
    Expenses
```
