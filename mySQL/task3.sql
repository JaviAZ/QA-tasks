/*Q1*/
CREATE VIEW mostLucrativeAuthor AS SELECT a.fname AS 'Author', b.stock_sold AS 'Books Sold' FROM authors a JOIN books b ON a.idauthors = b.idauthors ORDER BY stock_sold desc LIMIT 1;
/*Q2 ONLY FOR AUTHOR 4 NOT DYNAMIC*/
CREATE VIEW bestGenreA4 AS SELECT  a.fname AS 'Author', g.name AS 'Genre', SUM(b.stock_sold) as sold FROM genres g JOIN genres_books gb ON g.idgenres = gb.idgenres JOIN books b ON gb.idbooks = b.idbooks JOIN authors a ON a.idauthors = b.idauthors WHERE b.idauthors = 4 GROUP BY g.idgenres ORDER BY sold desc LIMIT 1;
/*Q3*/
CREATE VIEW unfinished AS SELECT a.fname, b.title, b.start_date FROM authors a JOIN books b ON a.idauthors = b.idauthors WHERE b.published = 0 ORDER BY b.start_date;