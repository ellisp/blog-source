
--CREATE SCHEMA wvs;

DROP TABLE IF EXISTS wvs.f_wvs
DROP TABLE IF EXISTS wvs.d_respondents
DROP TABLE IF EXISTS wvs.d_responses
DROP TABLE IF EXISTS wvs.d_questions


CREATE TABLE wvs.d_respondents(
	respondent_id						INT NOT NULL PRIMARY KEY,
	wave								NVARCHAR(63) NOT NULL,
	country_code						NVARCHAR(63) NOT NULL,
	weight								NUMERIC(20,17) NOT NULL,
	survey_year							SMALLINT
)


CREATE TABLE wvs.d_questions(
	question_id				INT NOT NULL PRIMARY KEY,
	question				NVARCHAR(255) NOT NULL
)

-- response (second column below) isn't unique because of upper / lower case issues for agree strongly
CREATE TABLE wvs.d_responses(
	response_id		INT NOT NULL PRIMARY KEY,	
	response					NVARCHAR(255) NOT NULL,
	response_no_commas			NVARCHAR(255) NOT NULL,
	response_no_spaces			NVARCHAR(255) NOT NULL,
	response_lower_case		NVARCHAR(255) NOT NULL,
	response_numeric			NUMERIC(25, 17),
	response_any_numerals		VARCHAR(63),
	response_class				VARCHAR(63),
	agrees						NUMERIC(2,1) NOT NULL,
	important						NUMERIC(2,1) NOT NULL,
	trust						NUMERIC(2,1) NOT NULL,
	often						NUMERIC(2,1) NOT NULL,
	like_me						NUMERIC(2,1) NOT NULL,
	interested				NUMERIC(2,1) NOT NULL,
	satisfied 				NUMERIC(2,1) NOT NULL,
	happy 				NUMERIC(2,1) NOT NULL,
	respect					NUMERIC(2,1) NOT NULL,
	justifiable					NUMERIC(2,1) NOT NULL,
	invalid				NUMERIC(2,1) NOT NULL
	)

CREATE TABLE wvs.f_wvs (
	respondent_id INT NOT NULL,
	question_id	INT NOT NULL,
	response_id INT NOT NULL,
	FOREIGN KEY (respondent_id) REFERENCES wvs.d_respondents(respondent_id),
	FOREIGN KEY (question_id) REFERENCES wvs.d_questions(question_id),
	FOREIGN KEY (response_id) REFERENCES wvs.d_responses(response_id)
)

ALTER TABLE wvs.f_wvs ADD PRIMARY KEY (respondent_id, question_id);