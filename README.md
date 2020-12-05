## Simple checkers game

Console application to play checkers against the computer.
Written in Haskell using `UI.Ncurses library` for frontend.

Still bugly.

Known issues:
- Troubles on quit. It seems something with garbage collection;
- Troubles with randomizer initialization;
- Some questions about rules when King is eating;
- Need to optimize and enhance AI;
- Have to provide more debug information in debug window;
- Mishmash with coordinates;
- Code refactoring to become more Haskell-way is not bad idea.

### ChangeLog

#### 0.0.9.2
2020-12-05
- Fixed win detection. Computer will finish game even if victory is already guaranteed

Aleksandr Bogdanov © 2020.
