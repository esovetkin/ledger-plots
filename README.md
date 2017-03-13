# ledger-plots - generate plots for your ledger

Make plots for your ledger entries and save them to a pdf file.

Obligatory screenshot:
![plot of some expenses account](screenshot.jpg?raw=true)

# Installation

The program consists of a single script (*ledger-plots*) and this is
the only file you need to manually copy on your machine.

Alternatively, you can install the script as an AUR
package
[ledger-plots-git](https://aur.archlinux.org/packages/ledger-plots-git/).

## Dependencies

It is assumed that you have [ledger](http://ledger-cli.org/) installed
and you know its query syntax.

The script is written in [R](https://www.r-project.org/) and this is
the only dependency. However, you need to install extra R-packages to
make it work.

In order to install missing R-package dependencies run
```
ledger-plots --install-dependencies
```
or, equivalently,
```
ledger-plots -u
```

## Getting help

To see full options list say
```
ledger-plots --help
```

# Examples and how it works

A query is being passed to ledger for getting a list of transactions
to be used in plots.

For example, to make plots for the ledger query "^assets:" call
```
ledger-plots -q "^assets: -X EUR"
```
After the execution a single pdf file is generated and can be viewed
by an external program.

Ledger supports multiple currency transactions. It is also able to
convert transaction to different currencies using the price derived
from transactions or using a separate price database. The conversion
maybe done using the `-X` option in the ledger query syntax.

Therefore, don't specify a query to convert all your transactions to a
common currency when you call ledger-plots. In case multiple
transactions are present ledger-plots selects only transactions with
the most frequent currency.

You can specify extra options which are passed to ledger with the
`--ledger-options` argument. For instance, this argument is useful to
avoid repetition in multiple queries and/or to specify the location of
the ledger files.

You can specify an output pdf filename with `-o` (`--output`)
option. All plots are placed on a landscape page layout with 4 plots
per page.

In case your ledger executable is located in a non-$PATH directory,
then you can specify its location by means `--ledger-bin-path`
option. The ledger is called by using an R-function `system`, which
invokes the OS command.

## Queries and statistics on transactions

The query is specified using `-q` (`--queries`) option. Ledger-plots
passes the specified query string to the ledger. You may also specify
several queries together separating them by two semi-colons (";;")
```
ledger-plots -q "^assets: -X EUR;; ^liability: -X EUR"
```
This will
make plots for each query one after another.

You may also pass a function to be calculated on the transaction
vector. This can be specified in `-f` (`--functions`) option. The
function can be any R function that takes a vector as an arguments and
returns a vector.

For different type of accounts it is reasonable to calculate different
functions. For instance, for assets you can calculate accumulated sum
for each of the "assets" account
```
ledger-plots -f "cumsum" -q "^assets: -X EUR"
```

If you plot expenses it is more reasonable to calculate a moving
average over some period of time. For example, you can calculate a
monthly average (over 30 days)
```
ledger-plots -f "monthly" -q "^expenses: -X EUR"
```

More generally, you can specify any R function. For instance, a 7-days
average can be specified as

```
ledger-plots -f "function(x) {filter(x,rep(1,30),sides=1)}" -q "^expenses: -X EUR"
```

Multiple function can be combined together for multiple queries by
separating them by two semi-colons (";;"). The first function in
the list of functions corresponds to the first query, second function
corresponds to the second query, etc. For example,
```
ledger-plots -f "cumsum  ;; monthly  ;; function(x) {-cumsum(x)}" \
             -q "^assets:;;^expenses:;;^income:" \
             --ledger-options="-X EUR"
```
for the query "^assets:" accumulated sum is calculated, for the query
"^expenses" a 30-days average and for the query "^income:" the
inverted accumulated sum.

## Plots ordering

By default plots of several accounts are ordered by the account depth
and a value of function specified in `--order-function`.

That is, firstly the accounts with depth 1 are plotted (e.g. "Assets"
has depth 1, and "Assets:Cash" has depth 2). All plots for accounts
with a given depth are ordered according to the value of the
`--order-function`. By default `--order-function=function(x)
sum(abs(x))`, which means that firstly appear plots with the biggest
sum of the absolute value of the transactions.

# Food prices

ledger-plots is also able to make plots and generate table for the
food prices. It assumes the following syntax:
```
2016/05/21 edeka
    Food:Spirits:Campari                       10.99 EUR ; 0.75l
    Food:Beverages:Juice                        1.49 EUR ; 1l
    Food:Kondenzmilk                            2.58 EUR ; 2x @ 400g
    Food:Mushrooms:Champignons                  2.14 EUR ; 0.428kg
    Food:Mango                                  5.96 EUR ; 4x
    Bath:Toothpaste                             4.98 EUR ; 2x @ 75ml elmex
    Expenses
```

The support for this feature is coming...
