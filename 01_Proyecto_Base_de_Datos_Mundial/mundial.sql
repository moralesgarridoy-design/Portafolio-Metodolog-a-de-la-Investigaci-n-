-- Proyecto: Base de datos "Mundial de Fútbol"
-- Autor: Yahyr Morales Garrido
-- Nota: ejecutar en MariaDB/MySQL.

CREATE DATABASE IF NOT EXISTS mundial;
USE mundial;

CREATE TABLE teams (
    team_id INT PRIMARY KEY,
    team_name VARCHAR(100),
    country VARCHAR(100),
    confederation VARCHAR(50)
);

CREATE TABLE matches (
    match_id INT PRIMARY KEY,
    stage VARCHAR(50),
    match_date DATE,
    stadium VARCHAR(100),
    home_team_id INT,
    away_team_id INT,
    home_score INT,
    away_score INT,
    FOREIGN KEY (home_team_id) REFERENCES teams(team_id),
    FOREIGN KEY (away_team_id) REFERENCES teams(team_id)
);

CREATE TABLE players (
    player_id INT PRIMARY KEY,
    player_name VARCHAR(100),
    team_id INT,
    position VARCHAR(50),
    goals INT,
    FOREIGN KEY (team_id) REFERENCES teams(team_id)
);

CREATE TABLE stadiums (
    stadium_id INT PRIMARY KEY,
    stadium_name VARCHAR(100),
    city VARCHAR(100),
    capacity INT
);

CREATE TABLE referees (
    referee_id INT PRIMARY KEY,
    full_name VARCHAR(100),
    country VARCHAR(100),
    experience_years INT
);

-- Limpieza de referees
UPDATE referees
SET full_name = TRIM(full_name),
    country = TRIM(country);

UPDATE referees
SET full_name = 'Desconocido'
WHERE full_name IS NULL OR full_name = '';

UPDATE referees
SET country = CASE
    WHEN LOWER(country) IN ('mexico','méxico',' mexico ') THEN 'México'
    WHEN LOWER(country) = 'brazil' THEN 'Brasil'
    WHEN LOWER(country) = 'england' THEN 'Inglaterra'
    ELSE country
END;

UPDATE referees
SET experience_years = 0
WHERE experience_years IS NULL;

-- Procedimiento almacenado de limpieza
DELIMITER //
CREATE PROCEDURE clean_referees()
BEGIN
    UPDATE referees
    SET full_name = TRIM(full_name),
        country = TRIM(country);

    UPDATE referees
    SET full_name = 'Desconocido'
    WHERE full_name IS NULL OR full_name = '';

    UPDATE referees
    SET country = CASE
        WHEN LOWER(country) IN ('mexico','méxico',' mexico ') THEN 'México'
        WHEN LOWER(country) = 'brazil' THEN 'Brasil'
        WHEN LOWER(country) = 'england' THEN 'Inglaterra'
        ELSE country
    END;

    UPDATE referees
    SET experience_years = 0
    WHERE experience_years IS NULL;
END//
DELIMITER ;

-- Consultas de ejemplo
SELECT * FROM teams;
SELECT * FROM players;
SELECT * FROM matches;
SELECT * FROM stadiums;
SELECT * FROM referees;

SELECT team_name, confederation FROM teams;
SELECT player_name, goals FROM players;
SELECT stadium_name, city FROM stadiums;
SELECT * FROM players WHERE goals > 3;
SELECT * FROM matches WHERE home_score > away_score;
SELECT confederation, COUNT(*) FROM teams GROUP BY confederation;
SELECT team_id, COUNT(*) FROM players GROUP BY team_id;
SELECT AVG(home_score + away_score) FROM matches;
SELECT SUM(goals) FROM players;
SELECT MAX(capacity) FROM stadiums;
SELECT MIN(capacity) FROM stadiums;
SELECT team_id, AVG(goals) FROM players GROUP BY team_id;
SELECT country, COUNT(*) FROM referees GROUP BY country;
SELECT player_name, goals FROM players ORDER BY goals DESC;
SELECT team_name FROM teams ORDER BY team_name ASC;
SELECT stadium, COUNT(*) FROM matches GROUP BY stadium;
SELECT home_team_id, COUNT(*) FROM matches
WHERE home_score > away_score GROUP BY home_team_id;
SELECT player_name FROM players
WHERE goals = (SELECT MAX(goals) FROM players);
SELECT stage, COUNT(*) FROM matches GROUP BY stage;
