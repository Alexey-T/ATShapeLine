
This was TLine component for Delphi (downloaded from Torry.net), now TShapeLine for Lazarus.
TLine id is already busy in Lazarus tachart.

I redone get/set of canvas.pen and canvas.brush: do it only inside Paint,
before it was all accross the code, in getters, setters, etc. This gives crashes of IDE
on changing props in Linux.

Tested on Linux gtk2.


Alexey Torgashin, 2015