# ekzperiment
A basic zombie roguelike that I plan to either develop more or scavenge for useful codebits in new projects.

https://github.com/whitehackrpg/ekzperiment/assets/130791778/1fc2169a-3669-400e-bfe1-03c7de2eed96

The project requires that you have Bearlibterminal installed, as well as Steve Losh's cl-blt (https://docs.stevelosh.com/cl-blt/). 

Clone ekzperiment in your local quicklisp directory, and then, in a REPL:
```
(ql:quickload :ekzperiment)
(in-package :ekzperiment)
(main)
```
To get fullscreen, use Alt-F11.
