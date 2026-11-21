# objectExplorerR 0.3.0

## Reads bundles through the spec

* `explore_ontology()` normalises its input once with
  `ontologySpecR::as_bundle()`, so a bundle object, a legacy-keyed list or a
  path to a bundle JSON file are all accepted.
* No code reads `bundle$objects`, `$links` or `$actions` directly any more.
  Types come from `bundle_object_types()`, `bundle_link_types()` and
  `bundle_action_types()`, link ends from `link_endpoints()` and the primary
  key from `object_pk()`. These accessors return lists named by id; the type
  dropdown still shows display names and submits ids.
* `explore_ontology()` rejects a closed connection up front; the unused
  `dplyr` import is dropped.
* Removed the local `property_column()`; the spec provides it.
* ontologySpecR (>= 0.2.1) is required; `Remotes:` added for ontologySpecR,
  objectSetsR, actionTypesR, vertexR and conceptR.

## Bug fixes

* Running a deprecated `action_handlers` action called an undefined `bundle()`
  inside the action form. The handlers are now wrapped in an `ActionContext`
  once, in `explore_ontology()`, so every action goes through
  `actionTypesR::submit_action()` and is logged.
* Action forms were registered again each time the object type changed, which
  stacked observers and could submit one click several times; they were also
  keyed by position, so after switching type an id could point at a different
  action. Each action form is now keyed by action id and created once.
* Link traversal listed the same linked object once per source row (two routes
  from JFK gave JFK twice); results are distinct again.
* Text filters and free-text search are case-insensitive on every backend
  (LIKE is case-sensitive on DuckDB).
* The 1000-row cap is a `LIMIT` in the query (`os_slice()`), not a trim after
  collecting the whole table.

## Tests

* Fixtures come from `ontologySpecR::spec_example_bundle()` plus a matching
  database (DuckDB if installed, otherwise RSQLite).
* New `shiny::testServer()` tests for the type selector, the action form and
  the whole app server, and tests for `explore_ontology()` itself.
