Yahtzee — Human vs Optimal AI (Web)

What this is
- Client-side web app that loads the precomputed optimal expected-score table (`gstbl/OptEScore-Official.gstbl`) from this repo and uses it to play optimal solitaire Yahtzee decisions for the computer.
- Human plays standard Yahtzee turns; computer plays optimally (for expected final score) using the same official rules as the Pascal app.

How to run
- From the repo root, serve files via any static server and open `/web/index.html` in a browser.
- Example with Python 3:
  - `python3 -m http.server 8000`
  - Navigate to `http://localhost:8000/web/`

Notes
- The app reads `../gstbl/OptEScore-Official.gstbl` relative to `/web/`. Keep the repo layout intact.
- Joker rule, upper-section bonus, and extra-Yahtzee bonus are implemented per the Pascal sources.
- The computer’s “optimal” play is optimal for solitaire expected score, not for head-to-head match dynamics.

Tests
- Open `/web/tests.html` (e.g., `http://localhost:8000/web/tests.html`).
- Tests run in-browser using a zeroed table for DP checks and verify:
  - Indexing into the table, scoring rules (Yahtzee, Joker rule, upper bonus).
  - DP best-category on final roll and basic keep advice.
  - AI turn simulation runs end-to-end.
