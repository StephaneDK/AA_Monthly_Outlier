# -*- coding: utf-8 -*-

from __future__ import print_function
import os
import sys

path_python_outlier = os.environ["path_code_outlier"].replace("Code","csv")
path_python_roas = os.environ["path_code_ROAS"].replace("Code","csv")

os.chdir(path_python_outlier)

class LatestDataCheck(Exception):
    pass

#Getting last saturday date
from datetime import date
from datetime import timedelta
today = date.today()
last_saturday = today - timedelta(days= (today.weekday() - 5) % 7)
last_saturday = last_saturday.strftime('%Y-%m-%d')


#Connecting to DB --------------------------------------------------------------------------------------------------------------------
#!/usr/bin/python
import snowflake.connector as sfc
import psycopg2
from config import Config
from configparser import ConfigParser
import csv



def config(filename='database.ini', section='snowflake_db'):
    # create a parser
    parser = ConfigParser()
    # read config file
    parser.read(filename)

    # get section, default to snowflake
    db = {}
    if parser.has_section(section):
        params = parser.items(section)
        for param in params:
            db[param[0]] = param[1]
    else:
        raise Exception('Section {0} not found in the {1} file'.format(section, filename))

    return db


def connect():
    """ Connect to the GDH server """
    conn = None
    try:
        # read connection parameters
        params = config()

        # connect to the GDH server
        print('\nConnecting to the GDH database...')
        conn = sfc.connect(account='PRH',
                                region='us-east-1',
                                user= params['user'],
                                password= params['password'])
		
        # create a cursor
        cur = conn.cursor()
        
        #execute a statement
        print('Snowflake database version:')
        cur.execute('SELECT CURRENT_VERSION()')

        print('\nRetrieving data for forecasting')

        # display the Snowflake database server version
        db_version = cur.fetchone()
        print(db_version, ' \n ')
        

       




        #Getting AA Data for UK
        sql_context = """
        select 
            "Country", "Title", "Asin", isbn, "AA Status", "AA Reason Code", -- "Campaign_name",
            sum("AA Spend")as AA_Spend, sum("AA Sales") as AA_Sales,sum("AA profit") as AA_Profit, 
            sum("AA Clicks")as AA_Clicks, sum("AA Impressions") as AA_Impressions, 
            sum("Total Amazon Sales (AA Supported ASINs)") as Sales, 
            avg("Cost per click (CPC)") as CPC, avg("Conversion Rate (CVR)") as CVR

        from PRH_GLOBAL_DK_SANDBOX.PUBLIC.VW_AADASH_DBR_UK 
 
        where to_varchar( "Date", 'yyyy-MM') = to_varchar(dateadd('month', -1, current_date), 'yyyy-MM')

        group by 1,2,3,4,5,6
        order by 7 desc
        """
        
        cur.execute(sql_context)
    
        # Fetch all rows from database
        record = cur.fetchall()

        #Checking if latest data is available
        if not record :
            print('No Print Status')
            raise LatestDataCheck()

        #Writing csv file
        with open('UK_AA_Data.csv', 'w') as f:
            column_names = [i[0] for i in cur.description]
            file = csv.writer(f, lineterminator = '\n')
            file.writerow(column_names)
            file.writerows(record) 
            print('Latest UK AA Data Saved \n')
            







        #Getting AA Data for US
        sql_context = """
        select 
            "Country", "Title", "Asin", isbn, "AA Status", "AA Reason Code", -- "Campaign_name",
            sum("AA Spend")as AA_Spend, sum("AA Sales") as AA_Sales,sum("AA profit") as AA_Profit, 
            sum("AA Clicks")as AA_Clicks, sum("AA Impressions") as AA_Impressions, 
            sum("Total Amazon Sales (AA Supported ASINs)") as Sales, 
            avg("Cost per click (CPC)") as CPC, avg("Conversion Rate (CVR)") as CVR

        from PRH_GLOBAL_DK_SANDBOX.PUBLIC.VW_AADASH_DBR_US 
 
        where to_varchar( "Date", 'yyyy-MM') = to_varchar(dateadd('month', -1, current_date), 'yyyy-MM')

        group by 1,2,3,4,5,6
        order by 7 desc
        """
        
        cur.execute(sql_context)
    
        # Fetch all rows from database
        record = cur.fetchall()

        #Checking if latest data is available
        if not record :
            print('No Print Status')
            raise LatestDataCheck()

        #Writing csv file
        with open('US_AA_Data.csv', 'w') as f:
            column_names = [i[0] for i in cur.description]
            file = csv.writer(f, lineterminator = '\n')
            file.writerow(column_names)
            file.writerows(record) 
            print('Latest US AA Data Saved \n')

#Getting ROAS Data for Change Over Time
        sql_context = """
        with camp_uk as (
        select  'UK' as "REGION", *,  "sales"/nullifzero("spend") as ROAS 
        from PRH_GLOBAL_DK_SANDBOX.PUBLIC.VW_AADASH_MO_CAMP_UK
        where "Month Date" != to_varchar(current_date, 'yyyy-MM') and "Month Date" >= to_varchar(current_date-90, 'yyyy-MM')
        )

        , sales_uk as (

        select to_varchar(a.sale_date,'YYYY-MM') as date, b.asin, a.prod_key, sum(a.qty_ord) as amz_ord, sum(a.val_ord) as amz_val from  PRH_GLOBAL.PUBLIC.F_REGION_POS_SALES a
        left join PRH_GLOBAL.PUBLIC.D_REGION_PROD b ON a.prod_key = b.prod_key and a.region_code= b.region_code
        where b.company = 'DK' and lower(a.country) = 'uk' 
        and a.POS_ACCT in ('A4', 'AMZ') and format != 'EL'
        group by 1,2,3
        order by 1 desc, 4 desc nulls last

        )

        , uk_cte as (
        select a.*, b.amz_ord, b.amz_val, a."sales"/nullifzero(b.amz_val) as aa_per
        from camp_uk a
        left join sales_uk b on a."Month Date" = b.date and IFNULL( a.adgroup_name,'9999') = b.asin

        )

        , camp_us as (
        select  'US' as "REGION", *,  "sales"/nullifzero("spend") as ROAS 
        from PRH_GLOBAL_DK_SANDBOX.PUBLIC.VW_AADASH_MO_CAMP_US
        where "Month Date" != to_varchar(current_date, 'yyyy-MM') and "Month Date" >= to_varchar(current_date-90, 'yyyy-MM')
        )

        , sales_us as (

        select to_varchar(a.sale_date,'YYYY-MM') as date, b.asin, a.prod_key, sum(a.qty_ord) as amz_ord, sum(a.val_ord) as amz_val from  PRH_GLOBAL.PUBLIC.F_REGION_POS_SALES a
        left join PRH_GLOBAL.PUBLIC.D_REGION_PROD b ON a.prod_key = b.prod_key and a.region_code= b.region_code
        where b.company = 'DK' and lower(a.country) = 'us' 
        and a.POS_ACCT in ('A4', 'AMZ') and format != 'EL'
        group by 1,2,3
        order by 1 desc, 4 desc nulls last

        )

        , us_cte as (

        select a.*, b.amz_ord, b.amz_val, a."sales"/nullifzero(b.amz_val) as aa_per
        from camp_us a
        left join sales_us b on a."Month Date" = b.date and IFNULL( a.adgroup_name,'9999') = b.asin
        )

        select * from uk_cte
        union all
        select * from us_cte

        order by  "REGION" ,  "Month Date" desc, "spend" desc 
        """
        
        cur.execute(sql_context)
    
        # Fetch all rows from database
        record = cur.fetchall()

        #Checking if latest data is available
        if not record :
            print('No Print Status')
            raise LatestDataCheck()

        os.chdir(path_python_roas)

        #Writing csv file
        with open('ROAS_data.csv', 'w') as f:
            column_names = [i[0] for i in cur.description]
            file = csv.writer(f, lineterminator = '\n')
            file.writerow(column_names)
            file.writerows(record) 
            print('Latest ROAS Data Saved \n')


       
        #close the communication with the PostgreSQL
        cur.close()
        

        
    except (Exception, psycopg2.DatabaseError) as error:
        print(error)
    finally:
        if conn is not None:
            conn.close()
            print('Database connection closed\n')

if __name__ == '__main__':
    connect()









