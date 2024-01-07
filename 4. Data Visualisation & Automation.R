#
# ──────────────────────────────────────────────────────────────────────────────
#
# SECTION 4: DATA VISUALISATION & AUTOMATION
#
# ──────────────────────────────────────────────────────────────────────────────
#
# Libraries used:

library(ggplot2)
library(recipes)
library(shiny)
library(cronR)
library(httr2)
library(DBI)
library(RPostgres)

# SECTION TABLE OF CONTENTS
#
# 
# 4. Data Visualisation & Automation
#     -   ggplot2 Package
#     -   recipes Package
#     -   shiny Package
#     -   cronR Package
#     -   httr2 Package (API calls)
#     -   DBI and RPostgres Packages (Database Integration)
# 
# ──────────────────────────────────────────────────────────────────────────────
# GGPLOT2 PACKAGE
# ──────────────────────────────────────────────────────────────────────────────

# ggplot2 is a great alternative to functions like plot() and hist() if you
# want to get fancy. Here are all of its basic functions:

# declaring a plot - does nothing on its own

ggplot(pumpkin) + ...

# declaring aesthetic mappings. It's used to specify how variables in the data
# are mapped to visual properties (aesthetics) of the plot.

ggplot(pumpkin, aes(x = High.Price, y = Low.Price)) + ...

# These functions specify the type of plot you want to draw

# scatter plot

ggplot(pumpkin, aes(x = High.Price, y = Low.Price)) + 
  geom_point()

# line plot

ggplot(pumpkin, aes(x = High.Price, y = Low.Price)) + 
  geom_line()

# bar plot (requires a factor)

pumpkin$Repack <- factor(pumpkin$Repack)

ggplot(pumpkin, aes(x = Repack)) + 
  geom_bar()


# make multi panel plots depending on a factor

ggplot(pumpkin, aes(x = High.Price, y = Low.Price)) + 
  geom_point() +
  facet_wrap(~ Repack)

# Customize the non-data parts of your plot (like axes, text, colors, etc.)

ggplot(pumpkin, aes(x = High.Price, y = Low.Price)) + 
  geom_point() + 
  theme_minimal()

# Add titles and modify axis labels

ggplot(pumpkin, aes(x = High.Price, y = Low.Price)) + 
  geom_point() + 
  labs(title = "Scatterplot", x = "X Axis", y = "Y Axis")

# Save plots as a file

gg_plot <- ggplot(pumpkin, aes(x = High.Price, y = Low.Price)) + 
  geom_point()

ggsave("myplot.png", plot = gg_plot, device = "png")
# (will save to working directory unless otherwise specified)


# ──────────────────────────────────────────────────────────────────────────────
# RECIPES PACKAGE
# ──────────────────────────────────────────────────────────────────────────────

# recipes can take care of most of the preprocessing and modelling functions 
# that were discussed in the earlier section. It can handle everything 
# much faster and simpler

# Here are its key functions:

# initialize a recipe object. Specify a formula and a dataset
# does nothing on its own. You need to link additional steps with 
# the %>% operator (after every line, not just the first one)
rec <- recipe(High.Price ~ Low.Price + Mostly.Low, data = pumpkin) %>%
  ...

# add different preprocessing steps to the recipe:
# (can be used on all or only specified predictors)

# Remove columns
step_rm(X, X.1)

# Apply Manipulations
step_mutate(AvgPrice = (Low.Price + High.Price) / 2)

# Factorisation
step_factor()

# Outliers
step_range(High.Price, range = c(lower_limit, upper_limit))

# Transformations
step_log(Variable)
step_BoxCox()
step_YeoJohnson()

# Centering
step_center(all_predictors())

# Scaling
step_scale(all_predictors())
   
# For creating dummy variables from categorical predictors
step_dummy(all_nominal())
   
# Normalizing numeric predictors
step_normalize(all_numeric())

# Missing values (NAs)
step_naomit() # delete rows with Nas
step_impute_mean(all_predictors()) # impute with mean (or other options)
step_impute_knn(neighbors = 5) # impute with knn and set neighbors

# Lumping
step_other(Variable, threshold = 0.1)

# Dummy encoding
step_dummy()

# For principal component analysis (PCA)
step_pca(all_numeric(), threshold = 0.95)

# For other all_ functions you can use:
?has_role


# After creating the recipe, you have to first prepare it

prep <- prep(rec, training = train)

# Then bake it to obtain the preprocessed dataset

train <- bake(prep, new_data = train)
test <- bake(prep, new_data = test)

# ──────────────────────────────────────────────────────────────────────────────
# SHINY PACKAGE
# ──────────────────────────────────────────────────────────────────────────────

# Any Shiny declaration requires three basic ingredients:

ui <- fluidPage(
  # UI code goes here
)

server <- function(input, output, session) {
  # Backend code goes here
}

shinyApp(ui = ui, server = server) # used to launch the preview

# For the UI part, there are multiple UI elements to choose from. The first 
# variable passed to the function is always the internal input/output variable

# A box where users can enter text.
textInput("text_id", "Enter text:")

# For entering numeric values.
numericInput("num_id", "Enter a number:", value = 1)

# A slider for selecting a range of values.
sliderInput("slider_id", "Choose a number", min = 1, max = 100, value = 50)

# A checkbox that can be toggled.
checkboxInput("check_id", "Check for Yes", value = TRUE)

# For selecting one option from multiple choices.
radioButtons("radio_id", "Choose one:", choices = c("Option 1", "Option 2", "Option 3"))

# A dropdown list to select an option.
selectInput("select_id", "Select one:", choices = c("Option 1", "Option 2", "Option 3"))

# For selecting dates.
dateInput("date_id", "Select a date:")

# To select a range of dates.
dateRangeInput("daterange_id", "Date range:")

# Allows users to upload files.
fileInput("file_id", "Choose a file:")

# A clickable button, usually for submitting forms.
actionButton("submit_id", "Submit")

# To display data in a table format.
tableOutput("table_id")

# For displaying plots.
plotOutput("plot_id")

# To display text.
textOutput("text_out_id")

# To dynamically generate UI elements.
uiOutput("ui_id")

# For creating tabbed panels.
tabsetPanel(
    tabPanel("Tab 1", 
              textOutput("content1")),
    tabPanel("Tab 2", 
              plotOutput("content2"))
)

# To create a navigation bar layout.
navbarPage("App Title", 
    tabPanel("Tab 1", textOutput("content1")),
    tabPanel("Tab 2", plotOutput("content2"))
)

# A layout with a sidebar and main panel.
sidebarLayout(
    sidebarPanel(textInput("text_id", "Enter text:")),
    mainPanel(textOutput("output_id"))
)

# UI that appears based on conditions.
conditionalPanel(
  condition = "input.check_id == true",
  textOutput("conditional_text")
)
    
# For downloading files.
downloadButton("download_id", "Download")

# For a title
titlePanel("My Shiny App")
h1("header 1")
h2("header 1")
# (goes up to h6)

# For a sidebar layout
sidebarLayout(
  sidebarPanel(
    h3("Sidebar Title"),
    textInput("text_input", "Enter Text"),
  ),
  
  mainPanel(
    h3("Main Panel"),
    textOutput("text_output"),
  )
)


# As for server elements, you always have to link them to the input and output
# objects like:

output$variable <- ...

input$variable

# here are some that you can use:

# Creates a reactive expression that automatically updates its output when its input changes.
reactive_data <- reactive({ data.frame(x = rnorm(input$slider_input)) })
# call it using ()
reactive_data()


# Render outputs that change in response to user inputs
   
# Renders reactive text.
output$text_output <- renderText({ paste("You entered", input$text_input) })

# Renders reactive plots.
output$plot_output <- renderPlot({ plot(reactive_data()) })

# Renders reactive tables.
output$table_output <- renderTable({ reactive_data() })

# Dynamically generates UI elements.
output$ui_output <- renderUI({ selectInput("dynamic_input", "Choose one:", choices = 1:input$slider_input) })


# These are used for side effects and do not produce output to the UI.

# Executes code in response to changes in inputs or expressions.
observe({ print(input$slider_input) })
# print() prints to console for debugging purposes

# Similar to `observe`, but triggered by specific events.
observeEvent(input$action_button, { print("Button clicked") })
# Used to store and modify reactive state within the server logic.

# For single reactive values.
counter <- reactiveVal(0)
observeEvent(input$action_button, { counter(counter() + 1) })

# A list of reactive values.
values <- reactiveValues(count = 0)
observeEvent(input$action_button, { values$count <- values$count + 1 })

# To generate downloadable content.
output$download <- downloadHandler(
  filename = function() { "data.csv" },
  content = function(file) { write.csv(reactive_data(), file) }
)

# ──────────────────────────────────────────────────────────────────────────────
# CRONR PACKAGE
# ──────────────────────────────────────────────────────────────────────────────

# All cronR jobs require a pre-made R script file that will store the code that
# will be actually executed. CronR jobs are run in the background - they don't
# even require RStudio to be open, so remember to delete them after you're done
# with the exam

# schedule a cron job.
cmd <- cron_rscript('/path/to/your/file.R')
cron_add(command = "Rscript /path/to/your/script.R", frequency = '* * * * *')

# Remove a cron job.
cron_rm(id = 'job_id')  # 'job_id' is returned by `cron_add`

# Clear all cron jobs created by cronR.
cron_clear()

# List all cron jobs created by cronR.
cron_ls()

# Returns the number of cron jobs.
cron_njobs()

# Retrieves the last error log from the cron job.
cron_last_error(id = 'job_id')

# Scheduling in cronR is done using the five asterisk system '* * * * *'
# each one means something else, here they are from the first to the last one:

# Minute (0 - 59)
# Hour (0 - 23)
# Day of the Month (1 - 31)
# Month (1 - 12 or Jan - Dec)
# Day of the Week (0 - 6 or Sun - Sat, where 0 = Sunday)
  
# Each asterisk can be replaced with: 
# a specific value (eg. execute on a specific day or minute)
# a list/range of values (eg. from January to March, from the 1st to 5th day)
# or just leave it as '*' which stands for "every" possible value

# every minute of every hour of every day of every month and every day of the week
'* * * * *'

# at the zeroth minute of every hour (run once every round hour)
'0 * * * *'

# every minute from the 15th to the 30th minute
'15-30 * * * *'

# at midnight every day
'0 0 * * *'

# at midnight every first day of the month
'0 0 1 * *'

# every Sunday at midnight
'0 0 * * 0'

# every 5 minutes
'*/5 * * * *'

# every day except Thursday
'0 0 * * 0-2,4-6'

# ──────────────────────────────────────────────────────────────────────────────
# HTTR2 PACKAGE
# ──────────────────────────────────────────────────────────────────────────────

# The core of this package is the request object, you'll be passing elements to
# it using the %>% operator

# Base command: Create a new HTTP request, you have to pass the URL here
req <- request("https://api.example.com")

# You can also change the URL later
req <- request() %>% req_url("https://api.example.com")

# Set the HTTP method (GET, POST, etc.) of a request
req_method("GET")

# Add a url path
req_url_path("path")

# Add a url parameter
req_url_query(n = 7)

# Add a JSON body to a request
req_body <- list("John", "Arbuckle", "Male", "Single")
req_body_json(req_body)

# Add headers to a request
req_headers(authorization = "auth")

# Perform a dry run
req_dry_run()

# Perform the request and store the response
resp <- req %>% req_perform()

# then, depending on the type of the reponse, you can extract it as one of
# these types (it'll most likely be JSON):

resp %>% resp_body_json()
resp %>% resp_body_string()
resp %>% resp_body_html()
resp %>% resp_body_xml()

# ──────────────────────────────────────────────────────────────────────────────
# DBI AND RPOSTGRES PACKAGES (DATABASE INTEGRATION)
# ──────────────────────────────────────────────────────────────────────────────

# the first step to connecting to your database is creating a credentials.R
# file. It will contain the information the system needs to connect to either
# the relevant container (if run locally on the droplet) or the relevant machine
# (if connecting via an IP address)

# the file should contain something like this:

# for your docker if connecting externally
cred_psql_docker <- list(dbname = 'postgres',
                         host = 'Your.Droplet.IP.Address',
                         user = 'postgres',
                         pass = 'YourPassword',
                         port = 5432)

# for your docker if connecting internally
cred_psql_docker <- list(dbname = 'postgres',
                         host = 'postgres',
                         user = 'postgres',
                         pass = 'YourPassword',
                         port = 5432)

# If you need to connect to Benjamin's databases (either your personal db or
# dvd_rental) you can look up the details on Brightspace and replace the above


# TIP: If you forgot the password of your droplet container, run this in the
# droplet terminal:

# docker inspect -f '{{.Config.Env}}' container_name


# After you have the credentials file, create these functions in a 
# psql_queries.R file (Make sure to call on library(DBI) and 
# library(RPostgres) first):

# To get data from Postgres to R:
psql_select <- function(cred, query_string){
  
  con_postgre <- dbConnect(Postgres(),
                           dbname = cred$dbname,
                           host = cred$host,
                           user = cred$user,
                           pass = cred$pass,
                           port = cred$port)
  
  query_res <- dbSendQuery(con_postgre, query_string)
  query_res_out <- dbFetch(query_res, n = -1)
  
  dbClearResult(query_res)
  dbDisconnect(con_postgre)
  
  return(query_res_out)
}


# To manipulate data in Postgres from R. E.g., create schemas, tables, insert
# rows or update values
psql_manipulate <- function(cred, query_string){
  
  con_postgre <- DBI::dbConnect(RPostgres::Postgres(),
                                dbname = cred$dbname,
                                host = cred$host,
                                user = cred$user,
                                pass = cred$pass,
                                port = cred$port)
  
  query_res <- dbSendStatement(con_postgre, query_string)
  
  return(paste0("Satement completion: ", dbGetInfo(query_res)$has.completed))
  
  dbClearResult(query_res)
  dbDisconnect(con_postgre)
}


# To insert entire dataframes in Postgres tables
psql_append_df <- function(cred, schema_name, tab_name, df){
  
  con_postgre <- dbConnect(Postgres(),
                           dbname = cred$dbname,
                           host = cred$host,
                           user = cred$user,
                           pass = cred$pass,
                           port = cred$port)
  
  query_res <- dbAppendTable(con = con_postgre,
                             name = Id(schema = schema_name, table = tab_name),
                             value = df)
  
  print(paste0("Number of rows inserted: ", query_res))
  
  dbDisconnect(con_postgre)
}

# Then you can execute the code in another file while calling them with:
source("credentials.R")
source("psql_queries.R")
# (make sure they are in the same directory)

# and you can connect to the database as such:

# Execute any SQL code
psql_manipulate(cred = cred_psql_docker, 
                # pass the credentials
                query_string = "CREATE SCHEMA intg1;"
                # Insert any SQL code in the string
)

# Create a dataframe
df <- data.frame(department_name = c("Education", "Chemistry"),
                 department_location = c("Aarhus N", "Aarhus C"))

# Write the dataframe to a postgres table
department <- psql_append_df(cred = cred_psql_docker,
                             schema_name = "intg1", 
                             #specify schema
                             tab_name = "department", 
                             #specify table
                             df = df
                             #specify data source on R side
)

# Fetching data from the database
psql_select(cred = cred_psql_docker,
            query_string = "select * from intg1.department;"
            # insert any select statement
)
