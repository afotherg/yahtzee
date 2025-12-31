Solitaire Yahtzee Advisor and Proficency Tester
========= ======= ======= === ========== ======

Our Optimal Solitaire Yahtzee Advisor and Yahtzee Proficiency Test
have been on-line since August 1999.  The current preferred address is
<http://www.win.tue.nl/~wstomv/misc/yahtzee/>.

This functionality is now also available for a stand-alone computer.
No Internet connection needed.

Package contents:

  ReadMe.txt
  YahtzeeTrainerLazarus/
    "source files"
    units/  (directory for binaries and executables)
      YahtzeeTrainerSettings.ini
  gstbl/
    TablesReadMe.txt

Separate download for the tables and executable (platform dependent,
currently only for Mac OS X, powerpc-darwin; but these can be constructed
from the preceding files):
  gstbl/
    OptEScore-Official.gstbl
    OptEScore-Official-V.gstbl
  YahtzeeTrainerLazarus/
    units/
      yahtzeetrainer

The current version (1.3 beta) provides the basic functionality.
It may be extended in the future (see below).  Feedback is welcome
on <mailto:yahtzee@win.tue.nl>.

Notes
-----
Loading of the two 6MB tables takes a little time (depending
on your computer configuration); there is no progress bar.  If the
tables cannot be found in the default location (in ../../gstbl/
from the directory of the executable), then you can set a new
location via the Settings menu item in the Tables menu.
The new location is remembered for the next run in
YahtzeeTrainerSettings.ini.

The tables can be created ("from scratch") via the Settings
menu item in the Tables menu.  Note however that this is a
time-consuming operation.  On my 1.25 GHz laptop with Mac OS X,
creating OptEScore-Official.gstbl takes about 2 hours,
and OptEScore-OfficialV.gstbl (the variances) takes
another 4 hours.  There is a progress bar (not tested) during these
operations, but they cannot be interrupted, nor can you
do anything else while the tables are computed.

The program creates a log file (possibly useful for bug reports)
in the units folder under the name YahtzeeTrainer.log.

The implementation is based on FreePascal (2.1.1) and Lazarus
(0.9.9), which are available for various platforms (including Linux,
Mac OS X, Windows).  For more information see <http://www.freepascal.org/>,

Changes in 1.3 beta (14 August 2005)
------- -- --- ----
Added a Windows menu.
Suppressed the option to "keep all dice" in the advisor.
Increased the font size of the dice values.
Added HELD labels above the dice, tracking status of the check boxes.
Added the GPL-ed source code.

Changes in 1.2.1 beta (30 December 2003)
------- -- ----- ----
When the tables are created from scratch, the program no
longer needs to be restarted to use the tables.

Only one warning message is shown when both tables cannot
be found.

The web address has been updated, to prepare for a move
of our entire Yahtzee site.

Changes in 1.2 beta (15 December 2003)
------- -- --- ----
The problem with the limited path length has been solved.
The tables are now also found when the length of their full
path name exceeds 80.

The directory for the tables can be set through Tables->Settings.
Click ... next to the directory path to get a select directory
dialog.  You can also create a new directory.

The tables can be created from scratch; see Tables->Settings.
Each table has 786432 entries.  The expected final score
is 254.5896... with a variance of (59.6117...)^2.  These
numbers can be confirmed through the info button.

The Update button in the Yahtzee Advisor window now also
triggers a new analysis (as if you clicked Analyze after it).

Future plans/ideas
------ ----- -----
Manual dice
Manual entry on Score Card (like the web site)
Configurable parameters (for rule variations; in particular:
  1 to 5 rolls per turn; 0 to 63 point threshold for
  Upper Section Bonus; size of Upper Section Bonus and
  Extra Yahtzee Bonus; yes/no Jokers)
Statistics over multiple games (also from stored game
  recordings; therefore saving games is useful)
Additional automated strategies (next to the
  OSYP = Optimal Solitaire Yahtzee Player)
Analysis and exploration of strategies
Multi-player games (including automated players)
More targeted training (rather than playing games with fair dice)
Offer an "explanation" for optimal choices

The program already contains most of these features,
but they lack a graphical user interface.

Enjoy,

Tom Verhoeff
