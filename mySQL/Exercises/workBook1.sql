/* 8d
SELECT o.name, AVG(r.rating) as average, COUNT(r.rating) as cnt FROM occupations o JOIN users u ON u.occupation_id = o.id JOIN ratings r ON r.user_id = u.id JOIN genres_movies g ON g.movie_id = r.movie_id WHERE g.id = 11 GROUP BY o.name HAVING cnt > 2 ORDER BY average DESC;*/
/*8e
SELECT o.name as occup, g.name as gen FROM occupations o JOIN users u ON o.id = u.occupation_id JOIN ratings r ON u.id = r.user_id JOIN genres_movies gm ON r.movie_id = gm.movie_id JOIN genres g ON gm.id = g.id GROUP BY o.name;*/
/*9
SELECT m.title, COUNT(r.rating) as cnt FROM ratings r JOIN movies m ON r.movie_id = m.id GROUP BY m.title ORDER BY cnt desc limit 1;*/
/*10
SELECT m.title, AVG(r.rating) as average, COUNT(r.rating) as cnt FROM ratings r JOIN movies m ON r.movie_id = m.id GROUP BY m.title HAVING cnt>10 ORDER BY average desc limit 1;*/
/*11
SELECT m.title, AVG(r.rating) as average, COUNT(r.rating) as cnt FROM ratings r JOIN movies m ON r.movie_id = m.id GROUP BY m.title HAVING cnt>10 ORDER BY average asc limit 1;*/