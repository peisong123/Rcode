#Load sqldf package, which will load all others necessary
#By default, SQLite runs in background to do processing, could use others DB engines if you wanted

library("sqldf")

#Import employees data
employees <- read.csv("~/Desktop/employees.csv")

#Import orders data - Matt G, Jason T, Matt W, and La Shanda H go to sushi lunch
#taking a potential client with them

orders <- read.csv("~/Desktop/orders.csv")


##### Single Table operations ####

#1 - Select all employees that are male

male_employees <- sqldf("SELECT * FROM employees WHERE gender = 'm'")

#2 - Get a count by first name

name_counts <- sqldf("SELECT firstname, COUNT (firstname) as occurances FROM employees GROUP BY firstname")

#3 - Get a count by first name, excluding non-employees

name_counts_emponly <- sqldf("SELECT firstname, COUNT(firstname) as occurances
                             FROM employees 
                             WHERE firstname != 'rudi'
                             GROUP BY firstname")

#4 - Use a case statement to define a new data column of california employees, 
#    using "lower" to evaluate all names as lowercase to ensure case insensitivity

employees_cali <- sqldf("SELECT *, 
                        CASE
                          WHEN lower(firstname) = 'stewart' THEN 1
                          WHEN lower(firstname) = 'hila' THEN 1
                          WHEN lower(firstname) = 'jon' THEN 1
                          WHEN lower(firstname) = 'solon' THEN 1
                          ELSE 0
                        END as cali_emp
                        FROM employees
                        ")

#5 - Sort employees_cali by cali_emp descending, first name ascending (ascending is default)

employees_cali_sorted <- sqldf("SELECT *, 
                               CASE
                                  WHEN lower(firstname) = 'stewart' THEN 1
                                  WHEN lower(firstname) = 'hila' THEN 1
                                  WHEN lower(firstname) = 'jon' THEN 1
                                  WHEN lower(firstname) = 'solon' THEN 1
                                  ELSE 0
                                END as cali_emp
                                FROM employees
                                ORDER BY cali_emp DESC, firstname
                        ")

##### Multi-Table operations ####

#1.  Left join employees and orders table (keep all records from employees table, 
#    matching records from orders)

left_join <- sqldf("SELECT * 
                   FROM employees a 
                   LEFT JOIN orders b ON a.id=b.id
                   WHERE a.firstname != 'rudi'
                   ")

#2.  "Right join" isn't supported in sqldf package, but switching order of tables and left join
#    is functionally equivalent

right_join_equiv <- sqldf("SELECT * 
                            FROM orders b 
                            LEFT JOIN employees a ON a.id=b.id
                          ")

#3.  Inner join...select only records that match both tables

inner_join <- sqldf("SELECT * 
                   FROM employees a, orders b
                   WHERE a.id=b.id
                   ")

#4.  Matt G. sees bill, wonders how bill can be so low!
#    Join orders to employees, find who is ordering items less than 10 dollars, sorted by lowest cost

inexpensive_items <- sqldf("SELECT * 
                         FROM orders a 
                         LEFT JOIN employees b ON a.id= b.id
                         WHERE item_cost < 10
                         ORDER BY item_cost
                         ")

#4a.  Realizing some things are priced by piece, figure out who spent less than $20 on any one
#     type of food

inexpensive_line_items <- sqldf("SELECT *, 
                                (item_cost * quantity_ordered) as item_level_cost
                                 FROM orders a 
                                 LEFT JOIN employees b ON a.id= b.id
                                 WHERE item_level_cost < 20
                                 ORDER BY item_level_cost
                                ")

#5.  Realizing that even item level cost is wrong question, Matt G. wants to know whose total lunch < $30
#    Need to use GROUP BY to get totals by person, then use HAVING instead of WHERE because
#    of the use of the GROUP BY summary function (WHERE is a record level operator)

lunch_under_30 <- sqldf("SELECT lastname, firstname,
                         SUM(item_cost * quantity_ordered) as lunch_cost
                         FROM orders a 
                         LEFT JOIN employees b ON a.id= b.id
                         GROUP BY a.id
                         HAVING lunch_cost < 30
                         ")

#6.  Matt G. wants to keep track of food consumption per person, particularly who the "lightweights"
#    are in the group.  Who's eating less than average on a cost basis?
#    This requires a subquery to first determine the average cost of this meal, passing that value
#    to the HAVING clause

#Subquery: returns a single value for the average lunch cost for employees (those with valid ID num)
#"SELECT SUM(item_cost * quantity_ordered)/COUNT(DISTINCT id) as avg_lunch_cost
# FROM orders WHERE id != 'NA'")

lower_than_average_cost <- sqldf("SELECT lastname, firstname,
                                  SUM(item_cost * quantity_ordered) as lunch_cost
                                  FROM orders a 
                                  LEFT JOIN employees b ON a.id= b.id
                                  WHERE a.id != 'NA'
                                  GROUP BY a.id
                                  HAVING lunch_cost < (
                                                        SELECT SUM(item_cost * quantity_ordered)/COUNT(DISTINCT id) as avg_lunch_cost
                                                        FROM orders 
                                                        WHERE id != 'NA'
                                                      )
                                ")

