# objectExplorerR

Interactive Shiny-based explorer for ontology objects defined by
[ontologySpecR](https://github.com/CathalByrneGit/ontologySpecR).

Search, filter, traverse links, and inspect ontology objects visually —
without writing code.

## Installation

```r
# install.packages("remotes")
remotes::install_github("CathalByrneGit/ontologySpecR")
remotes::install_github("CathalByrneGit/objectExplorerR")
```

## Quick Start

```r
library(ontologySpecR)
library(objectExplorerR)

b <- read_bundle("path/to/bundle.json")
con <- DBI::dbConnect(RSQLite::SQLite(), "database.db")

explore_ontology(b, con)
```

## Features

- **Auto-generated filters** from object type property definitions
- **Link traversal** — navigate relationships across object types
- **Detail view** with properties, linked objects, actions, and graph
- **Free-text search** across all string properties
- **Optional integrations** with actionTypesR (actions) and vertexR (graph)

## License

MIT
