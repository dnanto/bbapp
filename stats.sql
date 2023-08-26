PRAGMA journal_mode = MEMORY;

PRAGMA synchronous = OFF;

PRAGMA foreign_keys = OFF;

PRAGMA ignore_check_constraints = OFF;

PRAGMA auto_vacuum = NONE;

PRAGMA secure_delete = OFF;

BEGIN TRANSACTION;

CREATE SCHEMA IF NOT EXISTS `stats` DEFAULT;

CREATE TABLE
    IF NOT EXISTS `stats`.`player` (
        `id` INT NOT NULL,
        `name` TEXT NOT NULL,
        `alias` TEXT NULL,
        PRIMARY KEY (`id`)
    );

CREATE TABLE
    IF NOT EXISTS `stats`.`team` (
        `id` INT NOT NULL,
        `name` TEXT NOT NULL,
        `color` TEXT NOT NULL,
        `captain1` INT NULL,
        `captain2` INT NULL,
        `year` INT NOT NULL,
        `season` INT NOT NULL,
        PRIMARY KEY (`id`),
        INDEX `fk_team_player1_idx` (`captain1` ASC) VISIBLE,
        INDEX `fk_team_player2_idx` (`captain2` ASC) VISIBLE,
        UNIQUE INDEX `uq_team` (`name` ASC, `year` ASC, `season` ASC) VISIBLE,
        CONSTRAINT `fk_team_player1` FOREIGN KEY (`captain1`) REFERENCES `stats`.`player` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
        CONSTRAINT `fk_team_player2` FOREIGN KEY (`captain2`) REFERENCES `stats`.`player` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
    );

CREATE TABLE
    IF NOT EXISTS `stats`.`roster` (
        `id` INT NOT NULL,
        `team` INT NOT NULL,
        `player` INT NOT NULL,
        PRIMARY KEY (`id`),
        INDEX `fk_roster_team1_idx` (`team` ASC) VISIBLE,
        INDEX `fk_roster_player1_idx` (`player` ASC) VISIBLE,
        CONSTRAINT `fk_roster_team1` FOREIGN KEY (`team`) REFERENCES `stats`.`team` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
        CONSTRAINT `fk_roster_player1` FOREIGN KEY (`player`) REFERENCES `stats`.`player` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
    );

CREATE TABLE
    IF NOT EXISTS `stats`.`rink` (
        `id` INT NOT NULL,
        `name` TEXT NOT NULL,
        `address` TEXT NOT NULL,
        PRIMARY KEY (`id`)
    );

CREATE TABLE
    IF NOT EXISTS `stats`.`match` (
        `id` INT NOT NULL,
        `team1` INT NOT NULL,
        `team2` INT NOT NULL,
        `week` INT NOT NULL,
        `game` INT NOT NULL,
        `rink` INT NOT NULL,
        PRIMARY KEY (`id`),
        INDEX `fk_match_team1_idx` (`team1` ASC) VISIBLE,
        INDEX `fk_match_team2_idx` (`team2` ASC) VISIBLE,
        INDEX `fk_match_rink1_idx` (`rink` ASC) VISIBLE,
        UNIQUE INDEX `uq_match` (`team1` ASC, `team2` ASC, `week` ASC, `game` ASC) VISIBLE,
        CONSTRAINT `fk_match_team1` FOREIGN KEY (`team1`) REFERENCES `stats`.`team` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
        CONSTRAINT `fk_match_team2` FOREIGN KEY (`team2`) REFERENCES `stats`.`team` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
        CONSTRAINT `fk_match_rink1` FOREIGN KEY (`rink`) REFERENCES `stats`.`rink` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
    );

CREATE TRIGGER match_team_check
BEFORE INSERT ON match
WHEN (SELECT year, season FROM team WHERE name == NEW.team1) != (SELECT year, season FROM team WHERE name == NEW.team2)
BEGIN
    SELECT RAISE(FAIL, "the teams for this match are not part of the same year/season");
END;

CREATE TABLE
    IF NOT EXISTS `stats`.`point` (
        `id` INT NOT NULL,
        `match` INT NOT NULL,
        `team` INT NOT NULL,
        `shooter` INT NOT NULL,
        `assist1` INT NOT NULL,
        `assist2` INT NOT NULL,
        `goalie` INT NOT NULL,
        `period` INT NULL,
        `time` TEXT NULL,
        `EV` INT NOT NULL,
        `PP` INT NOT NULL,
        `SH` INT NOT NULL,
        `EN` INT NOT NULL,
        PRIMARY KEY (`id`),
        INDEX `fk_point_match1_idx` (`match` ASC) VISIBLE,
        INDEX `fk_point_player1_idx` (`shooter` ASC) VISIBLE,
        INDEX `fk_point_player2_idx` (`assist1` ASC) VISIBLE,
        INDEX `fk_point_player3_idx` (`assist2` ASC) VISIBLE,
        INDEX `fk_point_player4_idx` (`goalie` ASC) VISIBLE,
        CONSTRAINT `fk_point_match1` FOREIGN KEY (`match`) REFERENCES `stats`.`match` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
        CONSTRAINT `fk_point_player1` FOREIGN KEY (`shooter`) REFERENCES `stats`.`player` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
        CONSTRAINT `fk_point_player2` FOREIGN KEY (`assist1`) REFERENCES `stats`.`player` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
        CONSTRAINT `fk_point_player3` FOREIGN KEY (`assist2`) REFERENCES `stats`.`player` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
        CONSTRAINT `fk_point_player4` FOREIGN KEY (`goalie`) REFERENCES `stats`.`player` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
    );

CREATE TABLE
    IF NOT EXISTS `stats`.`foul` (
        `id` INT NOT NULL,
        `call` TEXT NOT NULL,
        PRIMARY KEY (`id`)
    );

CREATE TABLE
    IF NOT EXISTS `stats`.`penalty` (
        `id` INT NOT NULL,
        `match` INT NOT NULL,
        `team` INT NOT NULL,
        `player` INT NULL,
        `server` INT NULL,
        `goalie` INT NOT NULL,
        `foul` INT NOT NULL,
        `duration` INT NOT NULL,
        `period` INT NULL,
        `time` TEXT NULL,
        `scored` INT NOT NULL,
        PRIMARY KEY (`id`),
        INDEX `fk_penalty_match1_idx` (`match` ASC) VISIBLE,
        INDEX `fk_penalty_player1_idx` (`player` ASC) VISIBLE,
        INDEX `fk_penalty_player2_idx` (`server` ASC) VISIBLE,
        INDEX `fk_penalty_player3_idx` (`goalie` ASC) VISIBLE,
        INDEX `fk_penalty_foul1_idx` (`foul` ASC) VISIBLE,
        CONSTRAINT `fk_penalty_match1` FOREIGN KEY (`match`) REFERENCES `stats`.`match` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
        CONSTRAINT `fk_penalty_player1` FOREIGN KEY (`player`) REFERENCES `stats`.`player` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
        CONSTRAINT `fk_penalty_player2` FOREIGN KEY (`server`) REFERENCES `stats`.`player` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
        CONSTRAINT `fk_penalty_player3` FOREIGN KEY (`goalie`) REFERENCES `stats`.`player` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
        CONSTRAINT `fk_penalty_foul1` FOREIGN KEY (`foul`) REFERENCES `stats`.`foul` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
    );

CREATE TABLE
    IF NOT EXISTS `stats`.`shot` (
        `id` INT NOT NULL,
        `match` INT NOT NULL,
        `team` INT NOT NULL,
        `goalie` INT NOT NULL,
        `SH` INT NOT NULL,
        PRIMARY KEY (`id`),
        INDEX `fk_shot_match1_idx` (`match` ASC) VISIBLE,
        INDEX `fk_shot_player1_idx` (`goalie` ASC) VISIBLE,
        CONSTRAINT `fk_shot_match1` FOREIGN KEY (`match`) REFERENCES `stats`.`match` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
        CONSTRAINT `fk_shot_player1` FOREIGN KEY (`goalie`) REFERENCES `stats`.`player` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
    );

CREATE TABLE
    IF NOT EXISTS `stats`.`meta` (
        `id` INT NOT NULL,
        `match` INT NOT NULL,
        `data` TEXT NOT NULL,
        PRIMARY KEY (`id`),
        INDEX `fk_meta_match1_idx` (`match` ASC) VISIBLE,
        CONSTRAINT `fk_meta_match1` FOREIGN KEY (`match`) REFERENCES `stats`.`match` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
    );

CREATE TABLE
    IF NOT EXISTS `stats`.`legacy` (
        `id` INT NOT NULL,
        `roster` INT NOT NULL,
        `G` INT NOT NULL DEFAULT 0,
        `A` INT NOT NULL DEFAULT 0,
        `PIM` INT NOT NULL DEFAULT 0,
        `PPG` INT NOT NULL DEFAULT 0,
        `PPA` INT NOT NULL DEFAULT 0,
        `SHG` INT NOT NULL DEFAULT 0,
        `SHA` INT NOT NULL DEFAULT 0,
        `GWG` INT NOT NULL DEFAULT 0,
        `W` INT NOT NULL DEFAULT 0,
        `L` INT NOT NULL DEFAULT 0,
        `OTL` INT NOT NULL DEFAULT 0,
        `SH` INT NOT NULL DEFAULT 0,
        `GA` INT NOT NULL DEFAULT 0,
        `SHO` INT NOT NULL DEFAULT 0,
        PRIMARY KEY (`id`),
        INDEX `fk_legacy_roster1_idx` (`roster` ASC) VISIBLE,
        CONSTRAINT `fk_legacy_roster1` FOREIGN KEY (`roster`) REFERENCES `stats`.`roster` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
    );

COMMIT;

PRAGMA ignore_check_constraints = ON;

PRAGMA foreign_keys = ON;

PRAGMA journal_mode = WAL;

PRAGMA synchronous = NORMAL;