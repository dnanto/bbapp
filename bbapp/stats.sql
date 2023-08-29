-- Schema: stats
--   stats

ATTACH "stats.sdb" AS "stats";
BEGIN;

CREATE TABLE "stats"."player" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "name" TEXT NOT NULL,
  "alias" TEXT NULL
);
CREATE UNIQUE INDEX "stats"."uq_player_name" ON "player" ("name" ASC);

CREATE TABLE "stats"."team" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "name" TEXT NOT NULL,
  "color" TEXT NOT NULL,
  "captain1" INTEGER NULL,
  "captain2" INTEGER NULL,
  "year" INTEGER NOT NULL,
  "season" NUMERIC NOT NULL,
  CONSTRAINT "fk_team_player1" FOREIGN KEY ("captain1") REFERENCES "player" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT "fk_team_player2" FOREIGN KEY ("captain2") REFERENCES "player" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);
CREATE INDEX "stats"."fk_team_player1_idx" ON "team" ("captain1" ASC);
CREATE INDEX "stats"."fk_team_player2_idx" ON "team" ("captain2" ASC);
CREATE UNIQUE INDEX "stats"."uq_team" ON "team" ("name" ASC, "year" ASC, "season" ASC);

CREATE TABLE "stats"."roster" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "team" INTEGER NULL,
  "player" INTEGER NOT NULL,
  CONSTRAINT "fk_roster_team1" FOREIGN KEY ("team") REFERENCES "team" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT "fk_roster_player1" FOREIGN KEY ("player") REFERENCES "player" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);
CREATE INDEX "stats"."fk_roster_team1_idx" ON "roster" ("team" ASC);
CREATE INDEX "stats"."fk_roster_player1_idx" ON "player" ("player" ASC);

CREATE TABLE "stats"."rink" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "name" TEXT NOT NULL,
  "address" TEXT NOT NULL
);
CREATE UNIQUE INDEX "stats"."uq_rink" ON "rink" ("name" ASC);

CREATE TABLE "stats"."match" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "team1" INTEGER NOT NULL,
  "team2" INTEGER NOT NULL,
  "week" INTEGER NOT NULL,
  "game" INTEGER NOT NULL,
  "rink" INTEGER,
  "meta" TEXT,
  CONSTRAINT "fk_match_team1" FOREIGN KEY ("team1") REFERENCES "team" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT "fk_match_team2" FOREIGN KEY ("team2") REFERENCES "team" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT "fk_match_rink1" FOREIGN KEY ("rink") REFERENCES "rink" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);
CREATE INDEX "stats"."fk_match_team1_idx" ON "match" ("team1" ASC);
CREATE INDEX "stats"."fk_match_team2_idx" ON "match" ("team2" ASC);
CREATE INDEX "stats"."fk_match_rink1_idx" ON "match" ("rink" ASC);
CREATE UNIQUE INDEX "stats"."uq_match" ON "match" ("team1" ASC, "team2" ASC, "week" ASC, "game" ASC);
CREATE TRIGGER "stats"."tr_match_team"
BEFORE INSERT ON "stats"."match"
WHEN (SELECT year, season FROM team WHERE name == NEW.team1) != (SELECT year, season FROM team WHERE name == NEW.team2)
BEGIN
    SELECT RAISE(FAIL, "the teams for this match are not of the same year/season");
END;

CREATE TABLE "stats"."point" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "match" INTEGER NOT NULL,
  "team" INTEGER NOT NULL CHECK (team == 1 OR team == 2),
  "shooter" INTEGER NOT NULL,
  "assist1" INTEGER NULL,
  "assist2" INTEGER NULL,
  "goalie" INTEGER NULL,
  "period" INTEGER NULL,
  "time" TEXT NULL,
  "EV" INTEGER NOT NULL,
  "PP" INTEGER NOT NULL,
  "SH" INTEGER NOT NULL,
  "EN" INTEGER NOT NULL,
  CONSTRAINT "fk_point_match1" FOREIGN KEY ("match") REFERENCES "match" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT "fk_point_player1" FOREIGN KEY ("shooter") REFERENCES "player" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT "fk_point_player2" FOREIGN KEY ("assist1") REFERENCES "player" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT "fk_point_player3" FOREIGN KEY ("assist2") REFERENCES "player" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT "fk_point_player4" FOREIGN KEY ("goalie") REFERENCES "player" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);
CREATE INDEX "stats"."fk_point_match1_idx" ON "point" ("match" ASC);
CREATE INDEX "stats"."fk_point_player1_idx" ON "point" ("shooter" ASC);
CREATE INDEX "stats"."fk_point_player2_idx" ON "point"("assist1" ASC);
CREATE INDEX "stats"."fk_point_player3_idx" ON "point" ("assist2" ASC);
CREATE INDEX "stats"."fk_point_player4_idx" ON "point" ("goalie" ASC);

CREATE TABLE "stats"."foul" (
    "id" INTEGER PRIMARY KEY NOT NULL,
    "call" TEXT NOT NULL
);
CREATE UNIQUE INDEX "stats"."uq_foul" ON "foul" ("call" ASC);

CREATE TABLE "stats"."penalty" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "match" INTEGER NOT NULL,
  "team" INTEGER NOT NULL CHECK (team == 1 OR team == 2),
  "player" INTEGER NULL,
  "server" INTEGER NULL,
  "goalie" INTEGER NULL,
  "foul" INTEGER NULL,
  "duration" INTEGER NOT NULL,
  "period" INTEGER NULL,
  "time" TEXT NULL,
  "scored" INTEGER NULL,
  CONSTRAINT "fk_penalty_match1" FOREIGN KEY ("match") REFERENCES "match" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT "fk_penalty_player1" FOREIGN KEY ("player") REFERENCES "player" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT "fk_penalty_player2" FOREIGN KEY ("server") REFERENCES "player" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT "fk_penalty_player3" FOREIGN KEY ("goalie") REFERENCES "player" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT "fk_penalty_foul1" FOREIGN KEY ("foul") REFERENCES "foul" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);
CREATE INDEX "stats"."fk_penalty_match1_idx" ON "penalty" ("match" ASC);
CREATE INDEX "stats"."fk_penalty_player1_idx" ON "penalty" ("player" ASC);
CREATE INDEX "stats"."fk_penalty_player2_idx" ON "penalty" ("server" ASC);
CREATE INDEX "stats"."fk_penalty_player3_idx" ON "penalty" ("goalie" ASC);
CREATE INDEX "stats"."fk_penalty_foul1_idx" ON "penalty" ("foul" ASC);

CREATE TABLE "stats"."shot" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "match" INTEGER NOT NULL,
  "team" INTEGER NOT NULL CHECK (team == 1 OR team == 2),
  "goalie" INTEGER NULL,
  "period" INTEGER NULL,
  "SH" INTEGER NOT NULL,
  CONSTRAINT "fk_shot_match1" FOREIGN KEY ("match") REFERENCES "match" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT "fk_shot_player1" FOREIGN KEY ("goalie") REFERENCES "player" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);
CREATE INDEX "stats"."fk_shot_match1_idx" ON "shot" ("match" ASC);
CREATE INDEX "stats"."fk_shot_player1_idx" ON "shot" ("goalie" ASC);

CREATE TABLE "stats"."legacy" (
  "id" INTEGER PRIMARY KEY NOT NULL,
  "roster" INTEGER NOT NULL,
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
  CONSTRAINT "fk_legacy_roster1" FOREIGN KEY ("roster") REFERENCES "roster" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);
CREATE INDEX "stats"."fk_legacy_roster1_idx" ON "legacy" ("roster" ASC);

CREATE VIEW "stats"."v_point" AS
SELECT
  point.id,
  team.year, team.season, match.game, match.week,
  team.name AS team, team.color,
  player1.name AS shooter, player2.name AS assist1, player3.name AS assist2, player4.name AS goalie,
  period, time, EV, PP, SH, EN
FROM point
LEFT JOIN match ON match = match.id
LEFT JOIN team ON iif(team == 1, team1, team2) = team.id
LEFT JOIN player AS player1 ON shooter = player1.id
LEFT JOIN player AS player2 ON assist1 = player2.id
LEFT JOIN player AS player3 ON assist2 = player3.id
LEFT JOIN player AS player4 ON goalie = player4.id
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
LEFT JOIN match ON match = match.id
LEFT JOIN team ON iif(team == 1, team1, team2) = team.id
LEFT JOIN player AS player1 ON player = player1.id
LEFT JOIN player AS player2 ON server = player2.id
LEFT JOIN player AS player3 ON goalie = player3.id
LEFT JOIN foul ON foul = foul.id
;

CREATE VIEW "stats"."v_shot" AS
SELECT
  shot.id,
  team.year, team.season, match.game, match.week,
  team.name AS team, team.color,
  player.name AS goalie,
  SH, period
FROM shot
LEFT JOIN match ON match = match.id
LEFT JOIN team ON iif(team == 1, team1, team2) = team.id
LEFT JOIN player ON goalie = player.id
;

CREATE VIEW "stats"."v_roster" AS
SELECT 
  roster.id, roster.team AS team_id, roster.player AS player_id,
  team.year, team.season, team.name AS team, team.color, 
  player.name AS player FROM roster
LEFT JOIN team ON team = team.id
LEFT JOIN player ON player = player.id
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
  SELECT id, match, team AS team_id, coalesce(assist1, shooter) AS player_1_id, shooter AS player_2_id, iif(assist1, "A1", "A0") AS "type" FROM point
  UNION
  SELECT id, match, team, assist2, assist1, "A2" FROM point WHERE assist2 IS NOT NULL
) AS point
LEFT JOIN player AS player_1 ON player_1_id == player_1.id
LEFT JOIN player AS player_2 ON player_2_id == player_2.id
LEFT JOIN match ON match = match.id
LEFT JOIN team ON iif(team_id == 1, team1, team2) = team.id
;

COMMIT;