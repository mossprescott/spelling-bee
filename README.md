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

For a demo, see https://spellingbee.theprescotts.com, but please note that not all
of the features work. If you're interested in hosting a server for yourself and your friends,
get in touch and I can help you get started.


## UI

Some things about the UI that might not be obvious:

A player's score "bubble" is filled with a solid color if they have found a pangram (TODO: *all*
the pangrams.)

The number in the main, colored score bubble for the player is the score for all of the words
found by the player so far. A number like "+7" in the grey bubble to the right of it is the number
of points needed to reach the next level.

The last bubble is colored when the player reaches the "Genius" score for the day. If the player
finds all possible words, the bubble turns yellow (and the secret "Queen Bee" level is shown.)

### Friend and Group Scores

The server keeps track of a set of friends for each player. When authenticated, the current
player's score and each friend's score is shown below the word list.

Score bubbles for each friend work just like the main score display, except that the total score
is shown to the right of the bubbles to make it easier to read. Note: the score shown is
each players' actual total score, including words they found which don't appear anywhere in the
UI (because the current player hasn't found them.)

The name and score of the current player (that's you!) are shown in **bold**. Otherwise it just
shows the same score as appears at the top of the page.

When a number appears in parens after a friend's score, it shows the points that friend
has earned for words which you do *not* have. For example, if a friend has found all the words
you have, but also has one additional 5-letter word that you don't, "(5)" will appear.

The last row of scores shows the score for the whole ***Group***, which is the total point value
of all words found by the player and any of the friends. The maximum score possible for the day
is also shown on the right.

The "Group" score bubble is filled if *every* pangram has been found by *someone* in the group.
That is, on days when more than one pangram is available, the Group score bubble remains
unfilled as long as there is a pangram that no one has found yet. Note: this is a clue; there
would otherwise be no way to tell whether another pangram is available.


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
    "found": [
        ["gloom", ["jeff"]],
        ["glom", ["jeff", "steve"]]
    ],
    "hints": {
        "maxScore": 150,
        "pangramCount": 1
    },
    "friends": {
        "steve": { "hasPangram": true, "score": 120, "hasAllPangrams": false },
        "jeff": { "hasPangram": false, "score": 6, "hasAllPangrams": false },
        "dave": { "hasPangram": false, "score": 0, "hasAllPangrams": false }
    },
    "co-op": {
        "hasAllPangrams": true,
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
