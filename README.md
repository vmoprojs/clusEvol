**clusEvol**: A Procedure for Cluster Evolution Analytics.

## About

The objective of CEA is to analyze the evolution of an object and its
neighbours over time. Neighbours are identified by clustering
algorithms, CEA leverages the temporal component of panel data and it is
based on combining two techniques that are usually not related:
leave-one-out and plug-in principle. This allows us to use exploratory
what if questions in the sense that the present information of an object
is plugged in a dataset in a previous timepoint so that we can explore
its evolution (and that of its neighbors) up to the present.

## CEA: Cluster Evolution Analytics

### Installation Instructions

**Option 1**: CRAN

Our cross-platform version can be found at CRAN: `clusEvol` package. You
can install it with:

`install.packages("GeoModels")`

**Option 2**: Developer

For `clusEvol` installation you will need to previously install devtools
package if you do not have it installed yet:

`install.packages("devtools")`

`library(devtools)`

`devtools` lets you install packages from github since they need to be
installed from source code.

### Description

`clusEvol` is a function that allows us to use exploratory what if
questions in the sense that the present information of an object is
plugged-in a dataset in a previous time frame so that we can explore its
evolution (and of its neighbours) to the present.

clusEvol can be synthetized has the following steps

-   Identify clusters to which `sel.obj` is similar in `time.base` .

-   The data of `sel.obj` in time.base is plugged-in in each time
    period.

-   Clusters are generated in each time period with data from `sel.obj`
    in `time.base`.

### Example

    library(clusEvol)
    data(actpas)

    solclusEvol <- clusEvol(x=actpas,objects="razon_social", 
    time = "fecha",target.vars = c("montoAct","operAct"),
                            time.base=max(actpas$fecha),
                            sel.obj="BANCO SOLIDARIO S.A.",init = min(actpas$fecha),
                            logscale = TRUE,ng = 5,clm = "pam")
    print(solclusEvol)

    ## 
    ## ##################################################################
    ## clusEvol: Cluster Evolution Analytics
    ## 
    ## 
    ## Number of neighbours  BANCO SOLIDARIO S.A. is a group member:  
    ## 2021-06 2021-07 2021-08 2021-09 2021-10 2021-11 2021-12 2022-01 2022-02 2022-03 
    ##      11      12      12      29      30      28      11      24      13      13 
    ## 2022-04 2022-05 2022-06 2022-07 2022-08 2022-09 2022-10 2022-11 2022-12 2023-01 
    ##      25      12      21      14      10      11      27      44      12      11 
    ## 2023-02 2023-03 2023-04 2023-05 
    ##      39      12      11      10 
    ## 
    ## 
    ## Cluster that  BANCO SOLIDARIO S.A. belongs to:  
    ## 2021-06 2021-07 2021-08 2021-09 2021-10 2021-11 2021-12 2022-01 2022-02 2022-03 
    ##       5       4       5       3       3       3       4       3       4       5 
    ## 2022-04 2022-05 2022-06 2022-07 2022-08 2022-09 2022-10 2022-11 2022-12 2023-01 
    ##       3       4       3       4       4       5       3       3       3       4 
    ## 2023-02 2023-03 2023-04 2023-05 
    ##       3       4       4       4 
    ## 
    ## Clusters in time:
    ##          
    ##              1    2    3    4    5
    ##   2021-06   64   81   41   90   12
    ##   2021-07   53   93   83   13   68
    ##   2021-08   53   88   75   92   13
    ##   2021-09   55   76   30   86   84
    ##   2021-10   53   87   31   82   78
    ##   2021-11   52   82   29   59  110
    ##   2021-12   47  106   79   12   87
    ##   2022-01   43   92   25   53  115
    ##   2022-02   46   96  105   14   73
    ##   2022-03   43   92   93   90   14
    ##   2022-04   42   81   26  103   80
    ##   2022-05   47   92   83   13   96
    ##   2022-06   51   93   22  100   66
    ##   2022-07   45   85   86   15  101
    ##   2022-08   43  102   86   11   93
    ##   2022-09   44  105   86   91   12
    ##   2022-10   48  102   28   87   76
    ##   2022-11   97   91   45   73   40
    ##   2022-12  101   41   13   77  110
    ##   2023-01   41   94  110   12   83
    ##   2023-02   79   87   40   99   44
    ##   2023-03   54  102  102   13   77
    ##   2023-04   47  114  106   12   68
    ##   2023-05   48  113  101   11   76
    ## attr(,"class")
    ## [1]  table

Note that the example uses `actpas` dataset cointained in the package.
`actpas` is Ecuador’s amount of Assets and Liabilities Operations of the
National Financial System, openly available at:
<https://contenido.bce.fin.ec/home1/economia/tasas/IndiceSFN.htm>

The print method displays principal aspects of CEA. In this case,
`BANCO SOLIDARIO S.A.` is selected to be analyzed in monthly data from
2021-06 to 2023-05:

-   Number of neighbours BANCO SOLIDARIO S.A. is a group member.
-   Cluster that BANCO SOLIDARIO S.A. belongs to.
-   Clusters in time.

`clusEvol` function outputs a `clusEvol` class object that has a plot
method associated (`?plot.clusEvol` for detailed parameter information):

    plot(solclusEvol,2)

Plot shows the neighbours of the selected object over time.

### Package Citation

Once you have installed clusEvol, you can have a BibTex citation with
`citation("clusEvol")` and get:

    To cite package ‘clusEvol’ in publications use:

      Morales-Oñate V, Morales-Oñate B (2024). _clusEvol: A Procedure for Cluster Evolution Analytics_.
      R package version 1.0.0, <https://CRAN.R-project.org/package=clusEvol>.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {clusEvol: A Procedure for Cluster Evolution Analytics},
        author = {Víctor Morales-Oñate and Bolívar Morales-Oñate},
        year = {2024},
        note = {R package version 1.0.0},
        url = {https://CRAN.R-project.org/package=clusEvol},
      }
