## Simple checkers game

Console application to play checkers against the computer.
Written in Haskell using `UI.Ncurses library` for frontend.

Still bugly.

Known issues:
- Troubles on quit. Mouse clicking after (F10) exit produces chars in terminal;
- Troubles with randomizer initialization;
- Some questions about rules when King is eating;
- If checker become King after eating turn is switching to another player regardless can this checker eat again or  not;
- Need to more optimize and enhance AI;
- Have to provide more debug information in debug window;
- Mismatch in coordinates;

### ChangeLog

#### 0.0.9.3
2020-12-06
- AI refactored and little enhanced
- Fixed bugs with zero sized variants list and negative list index 

#### 0.0.9.2
2020-12-05
- Fixed win detection. Computer will finish game even if victory is already guaranteed

Aleksandr Bogdanov © 2020.
