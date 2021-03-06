
#Meta-omic Networks with MetaNetR
Cullen, N., Casaburi, G., Foster, J.*

*corresponding author: jfoster@ufl.edu

#Introduction

The MetaNetR package was developed to face a long-standing problem in the meta-omics research: providing non-technical researchers a method for creating publication- and presentation-quality network plots from hierarchical meta-omic data. With MetaNetR, users have access to extensive functionality using only a few commands. Most importantly, the process for creating an advanced network plot is __completely__ interactive -- requiring zero manual parameter changes.

Overall, MetaNetR offers users the ability to do the following:

- Load (big) meta-omic datasets
- Map hierarchical networks from raw datasets
- Visualize and create network plots with a fully user-interactive process
- Explore a vast collection of descriptive statistical measures of networks

In this vignette, the basic, yet powerful, functionality of MetaNetR will be reviewed. Before starting, the package needs to be installed and loaded into the R environment.
## Package Installation and Loading

Installing MetaNetR is similar to installing any other R package. Since the package is not hosted on CRAN, users must have the zip file containing the package, but there are three general ways to install it:

- Users can call the `install.packages` function while passing in the user's local file path to the zip file -- e.g. `install.packages(file.path)`. 

- Alternatively, a more user-friendly way to install the package is to call the function `install.packages(file.choose(),repos=NULL)`, which allows the user to browse for the zip file. 

- Lastly, and most user-friendly, the package can be installed in RStudio by going to `Tools > Install Packages... > Browse` on the upper RStudio toolbar, then choosing the zip file.

If a user wishes to stay more up-to-date with package development, the package can be installed directly from github by importing the devtools package and calling the function `install_github("ncullen93/metanetr")`.
 
Loading the package is done as usual in R with the `library` or `require` functions.

```{r, results="hide", warning=FALSE,message=FALSE}
library(MetaNetR)
```
## Loading Data

Loading data is fundamental to the network creation process. With MetaNetR, users load data exactly as they would in the standard R environment.

The package includes three actual datasets (found in the folder "Data" within the package files) that can be used for practice -- called dataset0 (large dataset), and three partitions of dataset0 (dataset1, dataset2, dataset3). Data is taken from a previously published study on the metagenome of stromatolites. [^1]

[^1]: Mobberley, J.M., et al. (2015) Inner workings of thrombolites: spatial gradients of metabolic activity as revealed by metatranscriptome profiling. Scientific Re-ports 5: 12601

The command to load two of the datasets is as follows:

```{r}
data(dataset1)
data(dataset2)
```
The command is similar for loading any other dataset. The command below provides a preliminary understanding of the data structure.
```{r}
head(dataset1, n=5)
```

Other datasets would be loaded into the R environment using any standard data loading method, but they should be of type `data.frame`. Once the data sets are loaded they need to be combined into an R `list`. This step is necessary for mapping the data later.
```{r}
files <- list(dataset1,dataset2)
```
## Creating an `LPV Map`
Next, it is necessary to create a _label-path-value map_ for each dataset. This step tells MetaNetR what each column in the data represents. A __label__ is a non-essential identifier (e.g. read number), a __path__ represents part of the hierarchical taxonomical structure (e.g. protein) and a __value__ is the actual numerical, experimental result (e.g. abundance).

```{r}

# FOR DATASET 1
label.columns.1 <- c(1) # columns in dataset1 that are "labels"
path.columns.1 <- c(2,3,4) # columns in dataset1 that are "paths"
value.columns.1 <- c(5,6,7) # columns in dataset1 that are "values"
lpv.1 <- list(label.columns.1,
              path.columns.1,
              value.columns.1) # combine labels/paths/values into a list

# FOR DATASET 2
label.columns.2 <- c(1) # columns in dataset1 that are "labels"
path.columns.2 <- c(2,3,4) # columns in dataset1 that are "paths"
value.columns.2 <- c(5,6,7) # columns in dataset1 that are "values"
lpv.2 <- list(label.columns.2,
              path.columns.2,
              value.columns.2) # combine labels/paths/values into a list

```
A single list is then created from the two lpv maps, as with the data sets:
```{r}
lpv <- list(lpv.1,
            lpv.2) # combbine each dataset lpv into one main lpv
```
## Loading a MetaNetR variable
Once the data and lpv maps are ready, they can be loaded and combined into one variable by calling the `load.data` function.

```{r}
mn <- load.data(f=files,lpv=lpv)
```
Above, the `load.data` function was passed in the `files` list of datasets and the `lpv` list of lpv maps as arguments. The result of the function is assigned to a new variable called `mn`. Here, `mn` is now a MetaNetR object. To check that it belongs to the "MetaNet" class, use the following command:
```{r}
class(mn)
```
Additionally, the `mn` variable can be printed to the console, where information about it will be visible.
```{r}
mn
```
Users can then set metadata to keep track of important notes related to the data or the visualizations. For example, suppose that `mn` has data which was collected in Florida.

```{r}
set.metadata(mn, "Location", "Florida")
```

Remember that functions such as `set.metadata` can be found in the official MetaNetR documentation.

## Mapping Networks

Once the `mn` variable containing the data and original lpv maps is obtained, the next step is to actually map a network using the `map.network` function. Please refer to the official documentation or call `help(map.network)`, as this function serves to create the actual network structure using the underlying data and lpv map. The underlying network is created in the code from the igraph package; this is what is meant by saying MetaNetR is build "on top of" igraph.

With this function, the relevant information will again be passed in as an lpv map -- a different one though. This time, however, the lpv map offers an additional level of flexibility because it is not required to actually use the entire _original_ lpv map. 

To observe this aspect of the program, use the following command:

```{r}
map.network(mn,datasets=c(1,2),
            lpv=list(list(1,1:2,2:3),list(1,1:2,2:3)))
```
In the example above, both datasets were used by specifying their indices in the `datasets` argument. Since two datasets were loaded earlier, the possible values for the `datasets` argument are `c(1)`, `c(2)`, or `c(1,2)`. If five datasets were loaded, any combination of 1 through 5 is possible by specifying the datasets to include in an R vector. In practice, users may load in many datasets originally, but only use a few for a given network mapping.

Then, the `lpv` argument of the function was specified just as originally -- each dataset gets its own `list`, and each list has three elements separated by commas: the label columns, the path columns, and the value columns. Here, for both datasets `list(1,1:2,2:3)` was passed in, telling the function to use the __first__ label, the __first__ and __second__ paths, and the __second__ and __third__ values for each dataset. This means that the __third__ path and the __first__ value were left off from the network creation process. 

Thus, the resulting "combined network" from calling this function will have only two paths (hierarchies) and two values (abundances) with which to work.

The `map.network` function creates a "combined network" from all of the datasets, merging them into one hierarchical network structure. It is common to use all of the labels, paths, and values for each dataset. Please refer to the following example:
```{r}
map.network(mn, datasets=c(1,2),
            lpv=list(list(1,1:3,1:3),list(1,1:3,1:3)))
```
Additionally, it is possible to use different _paths_ or different _values_ from each dataset, which can lead to an interesting structural merging of the two:
```{r}
map.network(mn, datasets=c(1,2),
            lpv=list(list(1,1:2,1:3),list(1,2:3,1:2)))
```
This functionality gives the user flexibility in the network creation process, without requiring that data is loaded over and over again with different original lpv maps.

With all of these possibilities, the `map.network` function creates a combined network with each call. It is possible to view the different networks that have been created using the following command:
```{r}
mn$combined.networks
```
Additionally, note that the `mn` print summary has been updated to reflect the number of mapped networks now available.
```{r}
mn
```
## Visualizing Networks
The last stage involves visualizing the network, which can be done using the `run.vis` function. This function has a few options:

        network.index : the index of which "combined network" the user wants to view.
        new.window : whether to plot the networks in the native RStudio graphics window, or in a new window.
        single.plot : whether to put all intermediate plots on the same screen, or show them one-by-one.

These arguments all become quite clear very quickly with repeated use.

For our example, one possible scenario is the following:
`run.vis(mn, network.index=1, new.window=FALSE, single.plot=TRUE)`

In an actual R environment, this command would start the visualization process. This process is documented and explained further in the supplementary materials section.

## Descriptive Statistics

As this package is built on top of the popular __igraph__[^2]  package, the multiplicity of network statistics function available in that package is available in MetaNetR. With this, the functionality in MetaNetR grows as the functionality in the igraph package does. 

[^2]: igraph.org/r/doc/

Therefore, the user is recommended to look at the igraph documentation for inspiration on which functions/statistics to use. Here is a simple example for obtaining the degree distribution of the network and extracting the "hub nodes"
```{r}
net.1 <- mn$combined.networks[[1]]
degree.dist <- degree(net.1)

hub.nodes <- degree.dist[degree.dist>mean(degree.dist)]
print(hub.nodes)
```

For a further look at the package, see the supplementary materials document or the official MetaNetR documentation.
