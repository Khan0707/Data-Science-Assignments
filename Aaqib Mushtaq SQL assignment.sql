SET GLOBAL sql_mode = 'ONLY_FULL_GROUP_BY';
select @@GLOBAL.sql_mode;

# The given database contains details regarding the sales transaction of a superstore. 
# It comprises of 5 tables, namely cust_dimen (containing details about 
# customer and their respective locations),prod_dimen (containing 
# product category and their subcategories),orders_dimen (with order number,
# date, and priority), shipping_dimen (with ship date, order and shipping 
# mode), and market_fact (order-wise,customer-wise and market-wise order quantity, 
# sales value, discount profit and shipping cost details).

# We can derive various insights from given database 
# e.g market_fact table will help us in making decisions 
# regarding segment wise sales and profitability of products,
# Shipping mode wise profit etc.Similarly combining details 
# of cust_dimen and market_fact we can find customers 
# who often shop with us etc.

# Keys of various Tables:

# 1. cust_dimen :- Cust_id as Primary Key, no foreign key.
# 2. prod_dimen :- Prod_id as Primary Key, no foreign key .
# 3. orders_dimen :- Ord_id as Primary Key , no foreign key.	
# 4. shipping_dimen:- Shipping id as primary key and Order_ID as foreign key.
# 5. market_fact:-  No Primary Key. Ord_id, Prod_id, Ship_id and Cust_id as foreign key.

#Task 2:-

#A
select sum(sales) as total_sales, avg(sales) as avg_sales
from market_fact;



#B
select Region, count(*) as No_of_customers
from cust_dimen
group by Region
order by No_of_customers desc;


#C
select Region, count(*) as No_of_customers
from cust_dimen
group by Region
having No_of_customers=
 (select max(No_of_customers)
 from (
       select Region, count(*) as No_of_customers
	   from cust_dimen
       group by Region
       ) temp
 );


##D 
select Prod_id, sum(Order_Quantity) as No_of_Products_Sold
from market_fact
group by Prod_id
order by No_of_Products_Sold desc;

##E

select Customer_Name, sum(Order_Quantity) as  No_of_tables_purchased

from cust_dimen c 
inner join market_fact m on c.Cust_id=m.Cust_id
inner join prod_dimen p on m.Prod_id = p.Prod_id

where Region="Atlantic" and Product_Sub_Category = "TABLES"

group by Customer_Name
order by no_of_tables_purchased desc;


##Task 3

##A
select product_category, round(sum(Profit),2) as profits
from prod_dimen p 
	inner join market_fact m  on p.Prod_id = m.Prod_id
group by Product_Category
order by Profits desc;

##B

select product_category, Product_Sub_Category,round(sum(Profit),2) as profits
from prod_dimen p 
     inner join market_fact m  on p.Prod_id = m.Prod_id
group by Product_Sub_Category,  product_category;


##C

## finding the least profitable product sub-category:

Select Product_Sub_Category, sum(Profit)
from market_fact m 
	inner join prod_dimen p on m.Prod_id = p.Prod_id
group by Product_Sub_Category
order by sum(Profit) ;

##tables is the  least profitable product sub-category;
                    
#main code:

select Region, count(Ship_id) as No_of_shipments,
     round(sum(Profit),2) as Profit_in_each_region
 
 from market_fact m 
		inner join cust_dimen c on m.Cust_id = c.Cust_id
        inner join prod_dimen p on m.Prod_id = p.Prod_id

Where Product_Sub_Category = "Tables"

group by Region

order by Profit_in_each_region;



        
                    

