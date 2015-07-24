
This was a TLine component for Delphi ( http://www.torry.net/authorsmore.php?id=3649 ), now TShapeLine for Lazarus. TLine id is busy in Lazarus tachart.

- I redone get/set of canvas.pen and canvas.brush: do it only inside Paint, before it was all accross the code, in getters, setters, etc. This gives crashes of IDE on changing props in Linux. 
- I added any linewidth for any direction with arrow1=true and arrow2=true.
- I converted demo to Laz using ide converter. 
- Icon added to component-pallette to 'Misc'.

Lazarus: 1.4.0.
Tested: Linux gtk2.


Ported by Alexey Torgashin