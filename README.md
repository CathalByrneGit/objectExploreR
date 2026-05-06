# objectExplorerR

Interactive Shiny-based explorer for ontology objects defined by
[ontologySpecR](https://github.com/CathalByrneGit/ontologySpecR).

Search, filter, traverse links, and inspect ontology objects visually —
without writing code.

## Installation

```r
# install.packages("remotes")
remotes::install_github("CathalByrneGit/ontologySpecR")
remotes::install_github("CathalByrneGit/objectSetsR")
remotes::install_github("CathalByrneGit/actionTypesR")
remotes::install_github("CathalByrneGit/objectExplorerR")
```

## Quick Start

```r
library(ontologySpecR)
library(actionTypesR)
library(objectExplorerR)

b <- read_bundle("path/to/bundle.json")
con <- DBI::dbConnect(RSQLite::SQLite(), "database.db")

# Create ActionContext (recommended)
action_ctx <- action_context(b, con)
action_ctx <- register_handler(action_ctx, "MyAction", function(...) { ... })

explore_ontology(b, con, action_ctx = action_ctx)
```

## Features

- **Auto-generated filters** from object type property definitions
  - Multi-select dropdown for enum-like string properties
  - Range sliders for numeric properties
  - Tri-state radio for booleans (Any/True/False)
  - Date range pickers for date/datetime
- **Link traversal** — navigate relationships across object types
- **Detail view** with properties, linked objects, actions, and graph
- **Free-text search** across all string properties
- **Dark mode** toggle (Bootstrap 5)
- **Action execution** via actionTypesR with full audit logging
- **Concept columns** integration (optional, with conceptR)
- **Graph neighbourhood** visualization (optional, with vertexR)

## Architecture

All data access goes through `objectSetsR` for lazy query evaluation.
Actions are executed via `actionTypesR` for proper audit logging.

## License

MIT
