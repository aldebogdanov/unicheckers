## Simple checkers game

Console application to play checkers against the computer.
Written in `Haskell` using `UI.Ncurses library` for frontend.

Application can be configured with `unicheckers.conf` file.
You can use also `/etc/unicheckers.conf` file that will be overwritten by the local one.
Config example with default values (which will be used even without `.conf` file) is in repository. 

Known issues
- Troubles on quit. Mouse clicking after (F10) exit produces chars in terminal
- Troubles with randomizer initialization
- Some questions about rules when King is eating
- If checker become King after eating turn is switching to another player regardless can this checker eat again or not
- AI is making redundant calculations even if the single turn is possible at the moment
- Mismatch in coordinates

Features to improve:
- Need to more optimize and enhance AI


### ChangeLog

#### 0.0.9.6
2020-12-08
- Migrated to Relude
- Removed filtering AI variants (there were no advantages, just disadvantages)

#### 0.0.9.5
2020-12-07
- AI Caching
- Logging
- Config file

#### 0.0.9.4
2020-12-06
- Multithreading
- AI optimized a bit
- Higher level set to 7 (can be slow as hell)

#### 0.0.9.3
2020-12-06
- AI refactored and little enhanced
- Fixed bugs with zero sized variants list and negative list index

#### 0.0.9.2
2020-12-05
- Fixed win detection. Computer will play until end even if victory is already guaranteed

Aleksandr Bogdanov © 2020.
