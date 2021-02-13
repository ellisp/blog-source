drop table if exists f_prices;
GO
drop table if exists d_products;
GO

CREATE TABLE d_products (
  product_id INTEGER PRIMARY KEY,
  ticker_yahoo VARCHAR(16) NOT NULL UNIQUE,
  ticker_origin VARCHAR(16),
  exchange_origin VARCHAR(16),
  latest_full_description VARCHAR(255),
  latest_full_description_tc VARCHAR(255),
  industry_group VARCHAR(255),
  latest_observed DATE
  
)
GO

CREATE TABLE f_prices (
  product_id INTEGER,
  observation_date DATE NOT NULL,
  open FLOAT,
  high FLOAT,
  low FLOAT,
  close FLOAT,
  volume FLOAT,
  adjusted FLOAT,
  short_positions FLOAT,
  total_product_in_issue FLOAT,
  short_positions_prop FLOAT,
  
  FOREIGN KEY (product_id) REFERENCES d_products (product_id)
)