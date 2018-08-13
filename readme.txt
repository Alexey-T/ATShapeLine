ATShapeLine is a component which paints line (directions: left-right, up-down, diagonals), with or without arrows at both sides. Line width is option. Line color and arrow color are options. It is Lazarus port of Delphi component TLine (renamed since TLine id is busy with TAChart).

Original author: Gon Perez-Jimenez (Spain, 2002)
Ported to Lazarus by: Alexey Torgashin (Russia)

- I redone get/set of canvas.pen and canvas.brush: do it only inside Paint, before it was all accross the code, in getters, setters, etc. This gives crashes of IDE on changing props in Linux. 
- I added any linewidth for any direction with arrow1=true and arrow2=true.
- I converted demo to Laz using ide converter. 
- Icon added to component-pallette to 'Misc'.

Lazarus: 1.6+
