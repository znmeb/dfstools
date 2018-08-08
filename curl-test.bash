#! /bin/bash -v

curl --show-error --verbose -X "GET" \
   "https://api.stattleship.com/baseball/mlb/games?team_id=mlb-kc&interval_type=regularseason" \
  -H "Content-Type: application/json" \
  -H "Authorization: Token token=${STATTLESHIP_TOKEN}" \
  -H "Accept: application/vnd.stattleship.com; version=1"

curl --show-error --verbose -X "GET" \
  "https://api.stattleship.com/basketball/nba/seasons" \
  -H "Content-Type: application/json" \
  -H "Authorization: Token token=${STATTLESHIP_TOKEN}" \
  -H "Accept: application/vnd.stattleship.com; version=1"
