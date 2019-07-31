/*Q1 ONLY FOR 2019-07-08 NOT DYNAMIC*/
CREATE VIEW roomsFree AS SELECT name as 'Free rooms' FROM rooms WHERE idrooms != (SELECT rooms_id FROM room_booking WHERE startDate = "2019-07-08");
/*Q2 ONLY FOR CHRISTOPHER NOT DYNAMIC*/
CREATE VIEW consultantsInCourse AS SELECT cour.name as Course, cons.name as Consultant FROM courses cour JOIN courses_consultants cc ON cour.idcourses = cc.courses_id JOIN consultants cons ON cc.consultants_id = cons.idconsultants JOIN trainers t ON cour.trainers_id = t.idtrainers WHERE t.name = "Christopher" AND cc.status = 1;
/*Q3*/
CREATE VIEW courseTrainer AS SELECT c.name as Course, t.name as Trainer FROM trainers t JOIN courses c ON c.trainers_id = t.idtrainers ORDER BY c.idcourses asc;
/*Q4 ONLY FOR IMRAN NOT DYNAMIC*/
CREATE VIEW completedCourses AS SELECT cour.name as Course, cons.name as Consultant FROM courses cour JOIN courses_consultants cc ON cour.idcourses = cc.courses_id JOIN consultants cons ON cc.consultants_id = cons.idconsultants WHERE cons.name = "Imran" AND cc.status = 0;
/*Q5*/
CREATE VIEW clientConsultant AS SELECT cli.name as 'Client', cons.name as 'Consultant' FROM clients cli JOIN client_booking clibo ON cli.idclients = clibo.clients_id JOIN consultants cons ON clibo.consultants_id = cons.idconsultants ORDER BY cli.idclients;
