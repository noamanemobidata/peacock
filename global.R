library(shiny)
library(bslib)
library(DBI)
library(GWalkR)
library(shinyTree)
library(shinyAce)
library(shinyjs)
library(RSQLite)
library(httr)
library(glue)
library(dbplyr)
library(duckdb)
library(dplyr)


employees <- dbConnect(RSQLite::SQLite(), "data/employees.db", 
                       #flags = SQLITE_RO, vfs = "unix-none"
                       )
con <- dbConnect(duckdb::duckdb(), dbdir="data/nycflights13.duckdb", read_only=F)


OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")


generate_db_tree <- function(con, name) {
  # Get all tables in the database
  tables <- dbListTables(con)

  # Initialize the database structure
  db_structure <- list()

  # For each table, get its columns
  for (table in tables) {
    columns <- dbListFields(con, table)

    # Create the table structure
    table_structure <- structure(
      lapply(columns, function(col) structure("", sttype = "column")),
      sttype = "table",
      stopened = FALSE
    )

    # Set the names of the list to be the column names
    names(table_structure) <- columns

    # Add the table to the database structure
    db_structure[[table]] <- table_structure
  }

  # Wrap the entire structure
  final_structure <- list(
    structure(
      db_structure,
      sttype = "database",
      stopened = TRUE
    )
  )

  # Set the name of the outer list to be the database name
  names(final_structure) <- name

  return(final_structure)
}


openai_response <- function(prompt, db) {
  
  
  if(db=="EmployeeDB"){
    msg="You are a SQLite expert. Given an input question, first create a syntactically correct SQLite query to run, then look at the results of the query and return the answer to the input question.
Unless the user specifies in the question a specific number of examples to obtain, query for at most 100 results using the LIMIT clause as per SQLite. You can order the results to return the most informative data in the database.
Never query for all columns from a table. You must query only the columns that are needed to answer the question. Wrap each column name in double quotes (\") to denote them as delimited identifiers.
      Pay attention to use only the column names you can see in the tables below. Be careful to not query for columns that do not exist. Also, pay attention to which column is in which table.
      Pay attention to use date('now') function to get the current date, if the question involves \"today\".
      here is the description of the database: Table Explanations
# Employees Database Schema

## Table: departments

### Columns
| Name      | Data Type    | Description                                                   |
|-----------|--------------|---------------------------------------------------------------|
| dept_no   | char(4)      | Department identification number. Primary key.                |
| dept_name | varchar(40)  | Name of the department. (unique key)                          |

### Relationships
- Referenced by `dept_emp.dept_no`
- Referenced by `dept_manager.dept_no`

## Table: dept_emp

### Columns
| Name      | Data Type    | References  | Description                                                   |
|-----------|--------------|-------------|---------------------------------------------------------------|
| emp_no    | int(10, 0)   | employees   | Employee identification number.                               |
| dept_no   | char(4)      | departments | Department identification number.                             |
| from_date | date         |             | Date the employee started work in the department.             |
| to_date   | date         |             | Date the employee left the department. '9999-01-01' = Current |

### Relationships
- References `departments.dept_no`
- References `employees.emp_no`

## Table: dept_manager

### Columns
| Name      | Data Type    | References  | Description                                                   |
|-----------|--------------|-------------|---------------------------------------------------------------|
| emp_no    | int(10, 0)   | employees   | Employee identification number.                               |
| dept_no   | char(4)      | departments | Department identification number.                             |
| from_date | date         |             | Date the employee started as department manager.              |
| to_date   | date         |             | Date the employee ended as manager. '9999-01-01' = Current    |

### Relationships
- References `departments.dept_no`
- References `employees.emp_no`

## Table: employees

### Columns
| Name       | Data Type    | Description                                    |
|------------|--------------|------------------------------------------------|
| emp_no     | int(10, 0)   | Employee identification number. Primary key.   |
| birth_date | date         | Date of birth.                                 |
| first_name | varchar(14)  | The employee's first name.                     |
| last_name  | varchar(16)  | The employee's last name.                      |
| gender     | enum         | M = Male, F = Female                           |
| hire_date  | date         | Employee hired on this date.                   |

### Relationships
- Referenced by `dept_emp.emp_no`
- Referenced by `dept_manager.emp_no`
- Referenced by `salaries.emp_no`
- Referenced by `titles.emp_no`

## Table: salaries

### Columns
| Name      | Data Type    | References | Description                                                   |
|-----------|--------------|------------|---------------------------------------------------------------|
| emp_no    | int(10, 0)   | employees  | Employee identification number.                               |
| salary    | int(10, 0)   |            | The amount of salary.                                         |
| from_date | date         |            | Start date of this salary. '9999-01-01' = Current salary      |
| to_date   | date         |            | End date of this salary. '9999-01-01' = Current salary        |

### Relationships
- References `employees.emp_no`

## Table: titles

### Columns
| Name      | Data Type    | References | Description                                                   |
|-----------|--------------|------------|---------------------------------------------------------------|
| emp_no    | int(10, 0)   | employees  | Employee identification number.                               |
| title     | varchar(50)  |            | The job title.                                                |
| from_date | date         |            | Start date of this title.                                     |
| to_date   | date         |            | End date of this title. '9999-01-01' = Current title.         |

### Relationships
- References `employees.emp_no`


produces ONLY the text of the sql request without any other text, and without  ```sql   
if the question does not require an SQL answer, revise your answer as an sql comment 
        "
  }else{
    
    msg="You are a Duckdb expert. Given an input question, first create a syntactically correct Duckdb query to run, then look at the results of the query and return the answer to the input question.
Unless the user specifies in the question a specific number of examples to obtain, query for at most 100 results using the LIMIT clause as per Duckdb. You can order the results to return the most informative data in the database.
Never query for all columns from a table. You must query only the columns that are needed to answer the question. Wrap each column name in double quotes (\") to denote them as delimited identifiers.
      Pay attention to use only the column names you can see in the tables below. Be careful to not query for columns that do not exist. Also, pay attention to which column is in which table.
      here is the description of the database: Table Explanations :

table airports:
columns:
faa :FAA airport code.
name:Usual name of the aiport.
lat, lon :Location of airport.
alt:Altitude, in feet.
tz:Timezone offset from GMT.
dst:Daylight savings time zone. A = Standard US DST: starts on the second Sunday of March, ends on the first Sunday of November. U = unknown. N = no dst.
tzone:IANA time zone, as determined by GeoNames webservice. 

table : airlines:
columns:
carrier :Two letter abbreviation.
name:Full name.

table : flights: 
columns:
year, month, day :Date of departure.
dep_time, arr_time :Actual departure and arrival times (format HHMM or HMM), local tz.
sched_dep_time, sched_arr_time:Scheduled departure and arrival times (format HHMM or HMM), local tz.
dep_delay, arr_delay:Departure and arrival delays, in minutes. Negative times represent early departures/arrivals.
carrier:Two letter carrier abbreviation. See airlines to get name.
flight:Flight number.
tailnum:Plane tail number. See planes for additional metadata.
origin, dest:Origin and destination. See airports for additional metadata.
air_time:Amount of time spent in the air, in minutes.
distance:Distance between airports, in miles.
hour, minute:Time of scheduled departure broken into hour and minutes.
time_hour:Scheduled date and hour of the flight as a POSIXct date. Along with origin, can be used to join flights data to weather data.

table : planes:
columns:
tailnum:Tail number.
year:Year manufactured.
type:Type of plane.
manufacturer, model:Manufacturer and model.
engines, seats:Number of engines and seats.
speed:Average cruising speed in mph.
engine:Type of engine.
    
weather
Format
A data frame with columns:
origin:Weather station. Named origin to facilitate merging with flights data.
year, month, day, hour:Time of recording.
temp, dewp:Temperature and dewpoint in F.
humid:Relative humidity.
wind_dir, wind_speed, wind_gust:Wind direction (in degrees), speed and gust speed (in mph).
precip:Precipitation, in inches.
pressure:Sea level pressure in millibars.
visib:Visibility in miles.
time_hour:Date and hour of the recording as a POSIXct date.
  
produces ONLY the text of the sql request without any other text, and without  ```sql   
if the question does not require an SQL answer, revise your answer as an sql comment 

    "
  }
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", OPENAI_API_KEY)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-4o-mini",
      temperature = 1,
      messages = list(
        list(
          role = "system",
          content = msg
        ),
        list(
          role = "user",
          content = prompt
        )
      )
    )
  )
  
  return(response)
}
