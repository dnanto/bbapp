-- Creator:       MySQL Workbench 8.0.34/ExportSQLite Plugin 0.1.0
-- Author:        Daniel Antonio Negrón (@dnanto)
-- Caption:       This is a model for broomball league statistics.
-- Project:       bbapp

-- Schema: stats
ATTACH "stats.sdb" AS "stats";
BEGIN;
CREATE TABLE "stats"."player"(
  "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  "name" TEXT NOT NULL,
  "alias" TEXT,
  CONSTRAINT "uq_player_idx"
    UNIQUE("name","alias")
);
CREATE TABLE "stats"."team"(
  "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  "year" INTEGER NOT NULL,
  "season" INTEGER NOT NULL,
  "session" INTEGER NOT NULL,
  "name" TEXT NOT NULL,
  "color" TEXT NOT NULL,
  CONSTRAINT "uq_team_idx"
    UNIQUE("year","season","session","name")
);
CREATE TABLE "stats"."roster"(
  "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  "team" INTEGER NOT NULL,
  "player" INTEGER NOT NULL,
  "captain" TEXT,
  CONSTRAINT "uq_roster_idx"
    UNIQUE("team","player"),
  CONSTRAINT "fk_roster_team"
    FOREIGN KEY("team")
    REFERENCES "team"("id"),
  CONSTRAINT "fk_roster_player"
    FOREIGN KEY("player")
    REFERENCES "player"("id")
);
CREATE INDEX "stats"."roster.fk_roster_team_idx" ON "roster" ("team");
CREATE INDEX "stats"."roster.fk_roster_player_idx" ON "roster" ("player");
CREATE TABLE "stats"."foul"(
  "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  "call" TEXT NOT NULL,
  CONSTRAINT "uq_foul_idx"
    UNIQUE("call")
);
CREATE TABLE "stats"."rink"(
  "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  "name" TEXT NOT NULL,
  "address" TEXT NOT NULL,
  CONSTRAINT "uq_rink_idx"
    UNIQUE("name")
);
CREATE TABLE "stats"."legacy"(
  "roster" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  "G" INTEGER NOT NULL DEFAULT 0,
  "A" INTEGER NOT NULL DEFAULT 0,
  "PIM" INTEGER NOT NULL DEFAULT 0,
  "PPG" INTEGER NOT NULL DEFAULT 0,
  "PPA" INTEGER NOT NULL DEFAULT 0,
  "SHG" INTEGER NOT NULL DEFAULT 0,
  "SHA" INTEGER NOT NULL DEFAULT 0,
  "GWG" INTEGER NOT NULL DEFAULT 0,
  "W" INTEGER NOT NULL DEFAULT 0,
  "L" INTEGER NOT NULL DEFAULT 0,
  "OTL" INTEGER NOT NULL DEFAULT 0,
  "SH" INTEGER NOT NULL DEFAULT 0,
  "GA" INTEGER NOT NULL DEFAULT 0,
  "SHO" INTEGER NOT NULL DEFAULT 0,
  CONSTRAINT "fk_legacy_roster"
    FOREIGN KEY("roster")
    REFERENCES "roster"("id")
);
CREATE TABLE "stats"."match"(
  "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  "team1" INTEGER NOT NULL,
  "team2" INTEGER NOT NULL,
  "week" INTEGER NOT NULL,
  "game" INTEGER NOT NULL,
  "rink" INTEGER,
  "meta" TEXT,
  CONSTRAINT "uq_match_idx"
    UNIQUE("team1","team2","week","game"),
  CONSTRAINT "fk_match_team1"
    FOREIGN KEY("team1")
    REFERENCES "team"("id"),
  CONSTRAINT "fk_match_team2"
    FOREIGN KEY("team2")
    REFERENCES "team"("id"),
  CONSTRAINT "fk_match_rink"
    FOREIGN KEY("rink")
    REFERENCES "rink"("id")
);
CREATE INDEX "stats"."match.fk_match_team1_idx" ON "match" ("team1");
CREATE INDEX "stats"."match.fk_match_team2_idx" ON "match" ("team2");
CREATE INDEX "stats"."match.fk_match_rink_idx" ON "match" ("rink");
CREATE TABLE "stats"."point"(
  "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  "match" INTEGER NOT NULL,
  "team" INTEGER NOT NULL,
  "shooter" INTEGER NOT NULL,
  "assist1" INTEGER,
  "assist2" INTEGER,
  "goalie" INTEGER,
  "period" INTEGER,
  "time" TEXT,
  "EV" INTEGER NOT NULL,
  "PP" INTEGER NOT NULL,
  "SH" INTEGER NOT NULL,
  "EN" INTEGER NOT NULL,
  CONSTRAINT "fk_point_match"
    FOREIGN KEY("match")
    REFERENCES "match"("id"),
  CONSTRAINT "fk_point_shooter"
    FOREIGN KEY("shooter")
    REFERENCES "player"("id"),
  CONSTRAINT "fk_point_assist1"
    FOREIGN KEY("assist1")
    REFERENCES "player"("id"),
  CONSTRAINT "fk_point_assist2"
    FOREIGN KEY("assist2")
    REFERENCES "player"("id"),
  CONSTRAINT "fk_point_goalie"
    FOREIGN KEY("goalie")
    REFERENCES "player"("id")
);
CREATE INDEX "stats"."point.fk_point_match_idx" ON "point" ("match");
CREATE INDEX "stats"."point.fk_point_shooter_idx" ON "point" ("shooter");
CREATE INDEX "stats"."point.fk_point_assist1_idx" ON "point" ("assist1");
CREATE INDEX "stats"."point.fk_point_assist2_idx" ON "point" ("assist2");
CREATE INDEX "stats"."point.fk_point_goalie_idx" ON "point" ("goalie");
CREATE TABLE "stats"."penalty"(
  "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  "match" INTEGER NOT NULL,
  "team" INTEGER NOT NULL,
  "player" INTEGER,
  "server" INTEGER,
  "goalie" INTEGER,
  "foul" INTEGER,
  "duration" INTEGER NOT NULL,
  "period" INTEGER,
  "time" TEXT,
  "scored" INTEGER,
  CONSTRAINT "fk_penalty_match"
    FOREIGN KEY("match")
    REFERENCES "match"("id"),
  CONSTRAINT "fk_penalty_player"
    FOREIGN KEY("player")
    REFERENCES "player"("id"),
  CONSTRAINT "fk_penalty_server"
    FOREIGN KEY("server")
    REFERENCES "player"("id"),
  CONSTRAINT "fk_penalty_goalie"
    FOREIGN KEY("goalie")
    REFERENCES "player"("id"),
  CONSTRAINT "fk_penalty_foul1"
    FOREIGN KEY("foul")
    REFERENCES "foul"("id")
);
CREATE INDEX "stats"."penalty.fk_penalty_match_idx" ON "penalty" ("match");
CREATE INDEX "stats"."penalty.fk_penalty_player_idx" ON "penalty" ("player");
CREATE INDEX "stats"."penalty.fk_penalty_server_idx" ON "penalty" ("server");
CREATE INDEX "stats"."penalty.fk_penalty_goalie_idx" ON "penalty" ("goalie");
CREATE INDEX "stats"."penalty.fk_penalty_foul_idx" ON "penalty" ("foul");
CREATE TABLE "stats"."shot"(
  "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  "match" INTEGER NOT NULL,
  "team" INTEGER NOT NULL,
  "goalie" INTEGER,
  "SH" INTEGER NOT NULL,
  "period" INTEGER,
  CONSTRAINT "fk_shot_match"
    FOREIGN KEY("match")
    REFERENCES "match"("id"),
  CONSTRAINT "fk_shot_goalie"
    FOREIGN KEY("goalie")
    REFERENCES "player"("id")
);
CREATE INDEX "stats"."shot.fk_shot_match_idx" ON "shot" ("match");
CREATE INDEX "stats"."shot.fk_shot_player_idx" ON "shot" ("goalie");

-- triggers 

CREATE TRIGGER "stats"."match_team_check"
BEFORE INSERT ON match
WHEN (
  SELECT year, season FROM team WHERE name == NEW.team1) != (SELECT year, season FROM team WHERE name == NEW.team2
  )
BEGIN
    SELECT RAISE(FAIL, "the teams for this match are not part of the same year/season");
END;

-- views

CREATE VIEW "stats"."v_point" AS
SELECT
  point.id,
  team.year, team.season, match.game, match.week,
  team.name AS team, team.color,
  player1.name AS shooter, player2.name AS assist1, player3.name AS assist2, player4.name AS goalie,
  period, time, EV, PP, SH, EN
FROM point
LEFT JOIN match ON match == match.id
LEFT JOIN team ON IIF(team == 1, team1, team2) == team.id
LEFT JOIN player AS player1 ON shooter == player1.id
LEFT JOIN player AS player2 ON assist1 == player2.id
LEFT JOIN player AS player3 ON assist2 == player3.id
LEFT JOIN player AS player4 ON goalie == player4.id
;

CREATE VIEW "stats"."v_penalty" AS
SELECT
  penalty.id,
  team.year, team.season, match.game, match.week,
  team.name AS team, team.color,
  player1.name AS player, player2.name AS server, player3.name AS goalie,
  foul.call,
  duration, period, time, scored
FROM penalty
LEFT JOIN match ON match == match.id
LEFT JOIN team ON IIF(team == 1, team1, team2) == team.id
LEFT JOIN player AS player1 ON player == player1.id
LEFT JOIN player AS player2 ON server == player2.id
LEFT JOIN player AS player3 ON goalie == player3.id
LEFT JOIN foul ON foul == foul.id
;

CREATE VIEW "stats"."v_shot" AS
SELECT
  shot.id,
  team.year, team.season, match.game, match.week,
  team.name AS team, team.color,
  player.name AS goalie,
  SH, period
FROM shot
LEFT JOIN match ON match == match.id
LEFT JOIN team ON IIF(team == 1, team1, team2) == team.id
LEFT JOIN player ON goalie == player.id
;

CREATE VIEW "stats"."v_roster" AS
SELECT 
  roster.id, roster.team AS team_id, roster.player AS player_id,
  team.year, team.season, team.name AS team, team.color, 
  player.name AS player FROM roster
LEFT JOIN team ON team == team.id
LEFT JOIN player ON player == player.id
;

CREATE VIEW "stats"."v_assist" AS
SELECT 
  ROW_NUMBER() OVER() AS assist_id, 
  point.id AS point_id,
  team.year, team.season,
  match.week, match.game,
  team.name AS team, team.color,
  player_1.id AS source_id, player_2.id AS target_id,
  player_1.name AS source, player_2.name AS target, 
  "type"
FROM (
  SELECT id, match, team AS team_id, COALESCE(assist1, shooter) AS player_1_id, shooter AS player_2_id, IIF(assist1, "A1", "A0") AS "type" FROM point
  UNION
  SELECT id, match, team, assist2, assist1, "A2" FROM point WHERE assist2 IS NOT NULL
) AS point
LEFT JOIN player AS player_1 ON player_1_id == player_1.id
LEFT JOIN player AS player_2 ON player_2_id == player_2.id
LEFT JOIN match ON match == match.id
LEFT JOIN team ON IIF(team_id == 1, team1, team2) == team.id
;

CREATE VIEW "stats"."v_matchup" AS
SELECT 
  team.year, team.season, x.week, x.game, 
  x.match_id, x.team_id, team.name AS team, team.color, rink.name AS rink, 
  COALESCE(score, 0) AS score
FROM
(
  SELECT id AS match_id, week, game, team1 AS team_id, rink AS rink_id FROM match 
  UNION
  SELECT id, week, game, team2, rink FROM match
) AS x
LEFT JOIN
(
  SELECT match.id AS match_id, team.id AS team_id, COUNT(*) AS score FROM point
  LEFT JOIN match ON match = match.id
  LEFT JOIN team ON IIF(team == 1, team1, team2) == team.id
  GROUP BY match_id, team_id
) AS y
ON x.match_id == y.match_id AND x.team_id == y.team_id
LEFT JOIN team ON x.team_id = team.id
LEFT JOIN rink ON x.rink_id = rink.id
;

COMMIT;
