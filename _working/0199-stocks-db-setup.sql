drop table if exists f_prices_and_volumes;
GO
drop table if exists d_variables;
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

CREATE TABLE d_variables (
  variable_id INTEGER PRIMARY KEY,
  variable VARCHAR(60) NOT NULL UNIQUE
)
GO

CREATE TABLE f_prices_and_volumes (
  product_id INTEGER,
  observation_date DATE NOT NULL,
  variable_id INTEGER NOT NULL,
  value FLOAT NOT NULL,
  
  FOREIGN KEY (product_id) REFERENCES d_products (product_id),
  FOREIGN KEY (variable_id) REFERENCES d_variables (variable_id)
)
GO

CREATE UNIQUE INDEX one_obs_per_var ON f_prices_and_volumes(variable_id, product_id, observation_date);
GO

INSERT INTO d_variables (variable) VALUES
	('open'),
	('high'),
	('low'),
	('close'),
	('volume'),
	('adjusted'),
	('short_positions'),
	('total_product_in_issue'),
	('short_positions_prop')
