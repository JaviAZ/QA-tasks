/*Q1*/
CREATE VIEW notNeutered AS SELECT name AS 'Name' FROM animals WHERE idanimals NOT IN (SELECT idanimals FROM vet_bookings) AND neutered = 0 AND exit_date IS NULL;
/*Q2*/
CREATE VIEW longestStay AS SELECT name As 'Name', entry_date AS 'Entered' FROM animals WHERE exit_date IS NULL ORDER BY entry_date;
/*Q3 ONLY FOR PENGUINS NOT DYNAMIC*/
CREATE VIEW smileAndWave AS  SELECT name AS 'Name', anim_type AS 'Species' FROM animals WHERE exit_date IS NULL AND anim_type = 'Penguin';