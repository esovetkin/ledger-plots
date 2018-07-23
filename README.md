# ledger-plots - generate plots for your ledger

Make plots for your ledger entries and save them to a pdf file.

![plot of some expenses account](examples/assets.png?raw=true)

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

In case multiple transactions are present ledger-plot will generate
several plots together for each of the currency, ordered by the
currency name.

You can specify extra options which are passed to ledger with the
`--ledger-options` argument. For instance, this argument is useful to
avoid repetition in multiple queries and/or to specify the location of
the ledger files.

You can specify an output pdf filename with `-o` (`--output`)
option. By default, all plots are placed on a landscape page layout
(13x7 inches) with 2x2 plots per page. The plots per page matrix can
be changed via `--output-pdf-ncol` and `--output-pdf-nrow`
options. The size of the pdf page can be changed via
`--output-pdf-width` and `--output-pdf-height` options.

In case your ledger executable is located in a non-$PATH directory,
then you can specify its location by means `--ledger-bin-path`
option. The ledger is called by using an R-function `system`, which
invokes the OS command.

## Queries and statistics on transactions

The query is specified using `-q` (`--queries`) option. Ledger-plots
passes the specified query string to the ledger. You may also specify
several queries together separating them by two semi-colons (";;")
```
ledger-plots -q "^assets: -X EUR ;; ^liability: -X EUR"
```
This will make plots for each query one after another.

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
ledger-plots -f "monthly" -q "^expenses: -H -X EUR"
```

More generally, you can specify any R function. For instance, a 7-days
average can be specified as

```
ledger-plots -f "function(x) {filter(x,rep(1,7),sides=1)}" -q "^expenses: -X EUR"
```

There are several query statistic functions available: `weekly`,
`monthly`, `quarterly` and `yearly`.

Multiple function can be combined together for multiple queries by
separating them by two semi-colons (";;"). The first function in
the list of functions corresponds to the first query, second function
corresponds to the second query, etc. For example,
```
ledger-plots -f "cumsum  ;; monthly  ;; function(x) {-cumsum(x)}" \
             -q "^assets:;;^expenses: -H;;^income:" \
             --ledger-options="-X EUR"
```
for the query "^assets:" accumulated sum is calculated, for the query
"^expenses" a 30-days average and for the query "^income:" the
inverted accumulated sum.

One can also calculate and plot in one figure several function
statistics. These functions can be specified by separating them with
"::" symbol. For example,
```
ledger-plots -f "cumsum :: function(x) {i <- 1:length(x); predict(lm(x~i))}" \
             -q "^assets: -X EUR"
```
plots cumulative sums plus a linear regression of the accumulated
assets. When plotting several query statistic, you might want to use
`--plot-legend` argument, to create a legend for each plot.

## Plots ordering and description

By default plots of several accounts are ordered by the account depth
and a value of function specified in `--order-function`.

That is, firstly the accounts with depth 1 are plotted (e.g. "Assets"
has depth 1, and "Assets:Cash" has depth 2). All plots for accounts
with a given depth are ordered according to the value of the
`--order-function`. By default `--order-function=function(x)
sum(abs(x))`, which means that firstly appear plots with the biggest
sum of the absolute value of the transactions.

With option `--no-depth-order` plots are ordered only with respect to
the `--order-function`.

The thick vertical lines on the plots correspond to the first day of
the month. The thin vertical lines correspond to the first day of the
week (Monday).

## Revalued plot

With `-t "revalued"` one is able to plot the revalued valued of the
query. This allows to see possible gain losses in a hypothetical
situation "if I were buying some currency what would I have
gained/lost"
```
ledger-plots -t "revalued" -f "cumsum" -q "-X EUR ;; -X GBP ;; -X USD ;; -X XBT" \
             --ledger-options="^assets:"
```

Note that this feature relies on your ledger price database.

## Plot of tags

Using option `-C "tags"` (or equivalently `--categorise-by="tags"`)
one may make plots for different tags and combination of tags. By
default, only plots for account with depth 1 are made (this behaviour
is controlled with option `--min-tags-account-depth`). Plots are made
only for tags combination with maximal length `--max-tags-tuples` and
with a minimal number of transactions equal to `--min-tags-entries`.

For example,
```
ledger-plots -C "tags" -f "monthly" -q "^expenses: -H -X EUR"
```
makes plot for expenses and different combinations of tags being used
for those transactions.

# Food prices and volumes

ledger-plots is also able to parse a special syntax of transaction
notes. For example, this can be used to keep a track on the consumed
amounts of food and food prices.

![food price plot](examples/food-prices.png?raw=true)

![food volume plot](examples/food-volumes.png?raw=true)

In order to keep track on the food prices/volumes one has to put an
extra entry to the transaction notes. For example, like in the following transactions:
```
2017/01/1 Lidl
    Food:Cookies                                   3 GBP ; 500g
    Food:Milk                                      2 GBP ; 2x @ 1l
    Expenses

2017/01/2 Edeka
    Food:Cookies                                   3 EUR ; 500g
    Food:Milk                                      2 EUR ; 2x @ 1l
    Bath:Toothpaste                                1 EUR ; 75ml
    Bath:Toothbrush                                5 EUR ; 3x
    Expenses

2017/01/3 Sainsbury's
    Food:Cookies                                   3 GBP ; 500g
    Food:Milk                                      2 GBP ; 2x @ 1l
    Expenses
```

Note that for each transaction multiple entries in different units can be specified. In
that case the volume/price is accounted and plotted in different
units. For example,
```
2017/01/1 Lidl
    Food:Ice Cream                                   3 EUR ; 2kg 2.2l
    Expenses
```

Generally, the volume of the account is given in the following format
```
<value>[]?<unit>
```

In order to make `ledger-plots` to parse the notes and make
corresponding plots use `--type` argument.
```
ledger-plots -q "-H -X EUR" -f "monthly.price" \
             --ledger-options='-f examples/food.ledger' \
             --type "price"

ledger-plots -q "-H -X EUR" -f "monthly" \
             --ledger-options='-f examples/food.ledger' \
             --type "volume"
```

Note that statistic value `monthly` doesn't make much sense for price
plots, therefore, use `monthly.price` that calculates a moving average
of 30 days price.
