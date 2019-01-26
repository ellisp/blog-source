


WITH agree_questions AS
	(SELECT	DISTINCT question, c.question_id
	FROM wvs.d_responses AS a
	INNER JOIN wvs.f_wvs AS b
		ON a.response_id = b.response_id
	INNER JOIN wvs.d_questions AS c
		ON b.question_id = c.question_id
	WHERE agrees = 1)
SELECT
	sum(g.weight * f.agrees) / sum(g.weight) AS weighted_agree,
	question,
	country_code
FROM wvs.f_wvs AS d
INNER JOIN agree_questions AS e
	ON d.question_id = e.question_id
INNER JOIN wvs.d_responses AS f
	ON d.response_id = f.response_id
INNER JOIN wvs.d_respondents AS g
	ON d.respondent_id = g.respondent_id
WHERE f.invalid != 1
GROUP BY question, country_code
ORDER by weighted_agree DESC
