# Launch the Object Explorer with the aviation demo dataset
#
# Prerequisites:
#   install.packages(c("shiny", "DT", "DBI", "dplyr", "rlang", "jsonlite", "bslib"))
#   install.packages("RSQLite")  # or install.packages("duckdb")
#   remotes::install_github("CathalByrneGit/ontologySpecR")
#   remotes::install_github("CathalByrneGit/objectSetsR")
#   remotes::install_github("CathalByrneGit/actionTypesR")
#   remotes::install_github("CathalByrneGit/objectExplorerR")

library(ontologySpecR)
library(objectSetsR)
library(actionTypesR)
library(objectExplorerR)
library(DBI)

# Read the aviation demo bundle
b <- read_bundle(system.file("examples", "aviation-demo.json",
                              package = "ontologySpecR"))

# Create an in-memory database and seed demo data
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# Create and populate airports table
DBI::dbExecute(con, "
  CREATE TABLE airports (
    airport_id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    city TEXT,
    country TEXT,
    latitude REAL,
    longitude REAL,
    elevation_ft INTEGER
  )
")

DBI::dbExecute(con, "
  INSERT INTO airports VALUES
    ('JFK', 'John F. Kennedy International', 'New York', 'United States', 40.6413, -73.7781, 13),
    ('LAX', 'Los Angeles International', 'Los Angeles', 'United States', 33.9425, -118.4081, 128),
    ('ORD', 'O''Hare International', 'Chicago', 'United States', 41.9742, -87.9073, 672),
    ('LHR', 'London Heathrow', 'London', 'United Kingdom', 51.4700, -0.4543, 83),
    ('CDG', 'Charles de Gaulle', 'Paris', 'France', 49.0097, 2.5479, 392),
    ('NRT', 'Narita International', 'Tokyo', 'Japan', 35.7647, 140.3864, 141),
    ('DXB', 'Dubai International', 'Dubai', 'United Arab Emirates', 25.2532, 55.3657, 62),
    ('SIN', 'Singapore Changi', 'Singapore', 'Singapore', 1.3644, 103.9915, 22),
    ('SYD', 'Sydney Kingsford Smith', 'Sydney', 'Australia', -33.9461, 151.1772, 21),
    ('FRA', 'Frankfurt Airport', 'Frankfurt', 'Germany', 50.0379, 8.5622, 364)
")

# Create and populate airlines table
DBI::dbExecute(con, "
  CREATE TABLE airlines (
    airline_id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    country TEXT,
    active INTEGER
  )
")

DBI::dbExecute(con, "
  INSERT INTO airlines VALUES
    ('AA', 'American Airlines', 'United States', 1),
    ('BA', 'British Airways', 'United Kingdom', 1),
    ('AF', 'Air France', 'France', 1),
    ('LH', 'Lufthansa', 'Germany', 1),
    ('EK', 'Emirates', 'United Arab Emirates', 1),
    ('SQ', 'Singapore Airlines', 'Singapore', 1),
    ('QF', 'Qantas', 'Australia', 1),
    ('NH', 'All Nippon Airways', 'Japan', 1)
")

# Create and populate routes table
DBI::dbExecute(con, "
  CREATE TABLE routes (
    route_id TEXT PRIMARY KEY,
    origin_id TEXT NOT NULL,
    destination_id TEXT NOT NULL,
    airline_id TEXT NOT NULL,
    stops INTEGER DEFAULT 0,
    equipment TEXT
  )
")

DBI::dbExecute(con, "
  INSERT INTO routes VALUES
    ('R001', 'JFK', 'LHR', 'AA', 0, '777'),
    ('R002', 'JFK', 'LHR', 'BA', 0, 'A380'),
    ('R003', 'JFK', 'CDG', 'AF', 0, '777'),
    ('R004', 'LAX', 'NRT', 'NH', 0, '787'),
    ('R005', 'LAX', 'SYD', 'QF', 0, 'A380'),
    ('R006', 'LHR', 'DXB', 'EK', 0, 'A380'),
    ('R007', 'FRA', 'JFK', 'LH', 0, 'A340'),
    ('R008', 'SIN', 'LHR', 'SQ', 0, 'A380'),
    ('R009', 'CDG', 'NRT', 'AF', 1, '777'),
    ('R010', 'ORD', 'LHR', 'AA', 0, '787'),
    ('R011', 'DXB', 'SIN', 'EK', 0, '777'),
    ('R012', 'SYD', 'SIN', 'QF', 0, 'A330')
")

# Create ActionContext and register handlers (preferred method)
action_ctx <- action_context(b, con)
action_ctx <- register_handler(action_ctx, "UpdateAirportStatus",
  function(conn, action, params, targets) {
    message("Updating airport status for: ", paste(targets, collapse = ", "))
    message("New status: ", params$new_status)
    # In a real application, this would update a status column:
    # DBI::dbExecute(conn,
    #   "UPDATE airports SET status = ? WHERE airport_id = ?",
    #   list(params$new_status, targets[[1]]))
    list(status = "success", message = "Status updated")
  }
)

# Launch the explorer with ActionContext!
explore_ontology(b, con, action_ctx = action_ctx)
