# Spelling Bee

Play the latest [NY Times "Spelling Bee"](https://www.nytimes.com/puzzles/spelling-bee)
puzzle with a group of friends.

This is the *real* puzzle, just like playing on the NY Times site/apps:

- only words from the official list are accepted
- the same scoring rules apply

In addition, you get these features:

- see the total score for each player
- for each word you've found, see which other player(s) also found it
- see how many points each of the other players has for words that you haven't found
- see the group score considering your words *and* your friends' words
- *no hints*, even if you open Web Inspector

You can also review every previous puzzle, including all the words and who found them.

For a demo, see https://spelling-bee-with-enemies.herokuapp.com, but please note that not all
of the features work. If you're interested in hosting a server for yourself and your friends,
get in touch and I can help you get started.


## Backend

This repo provides a UI for playing. It requires a backend that provides the letters each day,
checks words, and keeps track of which players have found each word. The required API is as follows:

### `GET /puzzle`

Ask the server for the latest puzzle and the state of solving. The request body is empty and can be
ignored. The response contains the day's letters, all the words found by the client, and the total
score for each user (including the client itself.) For each word already found, it indicates
the name of each user who has already found that word (including the current user.) When the puzzle
is still active, only words already found by the current user are included.

Example response body:

```json
{
    "user": "jeff",
    "id": 12021,
    "nextPuzzleId": null,
    "previousPuzzleId": 12020,
    "puzzle": {
        "expiration": 1614326400,
        "printDate": "2021-02-25",
        "displayDate": "February 25, 2021",
        "displayWeekday": "Thursday",
        "editor": "Sam Ezersky",
        "centerLetter": "o",
        "outerLetters": ["a", "g", "l", "m", "r", "u",
        ]
    },
    "found": {
        "glom": ["jeff", "steve"],
        "gloom": ["jeff"]
    },
    "hints": {
        "maxScore": 150
    },
    "friends": {
        "steve": { "score": 120 },
        "jeff": { "score": 6 },
        "dave": { "score": 0 }
    },
    "co-op": {
        "score": 121
    }
}
```

This request can be repeated as often as desired to get the latest scores for each player.


### `GET /puzzle/{id}`

Get a puzzle by id, which can be the latest, active puzzle or any previous day's (expired) puzzle.
The response is the same as for `/puzzle` *except*: if the puzzle is expired, then `found` will
also contain words that the user didn't find.

Note: no assumption is made about ids being in any particular sequence. The client always asks
for the latest puzzle and uses `previousPuzzleId` to walk backwards.


### `POST /word`

Submit a word to the server to be checked against the list of accepted words for the latest,
non-expired puzzle.

Example request body:

```json
"glum"
```

If the word is acceptable, it is recorded as found by the client's user (if authenticated), and
a `200 OK` response is returned with no body.

If the word is not correct, a `400 Bad Request` response is returned, possibly with some explanatory
information in the body (but the format of that information is not specified.) A correctly
implemented client will not submit any badly-formed words, so it can infer that the problem is that
the word does not appear on the list of accepted words.


### Authentication

The backend must somehow authenticate users and identify who's who so it can show the correct state
to each user. The client doesn't provide any UI for login.

If there's no authenticated user, the backend cannot save state, so the client keeps track of the list
of found words locally.


## Scoring

> Ranks are based on a percentage of possible points in a puzzle.

For example, when the total possible is 180:

- Beginner: 0 = 0%
- Good Start: 4 = 2% (rounded down to the nearest even integer)
- Moving Up: 9 = 5%
- Good: 14 = 8% (rounded up to the nearest even integer)
- Solid: 27 = 15%
- Nice: 45 = 25%
- Great: 72 = 40%
- Amazing: 90 = 50%
- Genius: 126 = 70%
- Queen Bee: 180 = 100%

4-letter words are worth one point each. Longer words are worth one point per letter.
A "pangram" (a word containing all seven letters) earns an additional seven points.
