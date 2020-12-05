## Simple checkers game

Console application to play checkers against the computer.
Written in Haskell using `UI.Ncurses library` for frontend.

Still bugly.

Known issues:
- troubles on quit. It seems something with garbage collection;
- when computer guaranteed that you will lose in N turns (depending on level you chose), it stops game unfinished;
- troubles with randomizer initialization;
- need to optimize and enhance AI;
- have to provide more debug information in debug window;
- mishmash with coordinates;
- code refactoring to become more Haskell-way is not bad idea.

Aleksandr Bogdanov © 2020.
