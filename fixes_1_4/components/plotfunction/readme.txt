This package contains 2 components:

TPlotFunctionPanel
  Plots a function y=F(x) on a grid.
  The function result is calculated with an event "OnCalcPlot". This
  function is called for each X for which an Y must be calculated.

TPlotExpressionPanel
  Plots a function y=F(x) on a grid.
  The function result is calculated from the expression "Expression"
  The fpexprpars unit is used to calculate the expression. The
  "Identifiers" property of the parsers is exposed so the variables
  and function definitions can be manipulated

Both components have the following properties:

Active : If set to false, only the grid is drawn.

PlotColor : Color of the plotted line

Caption : Caption of the graph


XAxis, YAxis: These control the appearance of the X and Y axis.
  They have both the following properties:
    Color     : Axis color
    TickColor : Color of the tick marks on the axis.
    Ticks     : Number of tick marks or distance between tick marks on the
                axis.
    TickSize  : Length of the tick mark.
    TickMode  : Ticks is number of ticks or distance (in pixels) between  ticks.
    TickFont  : Font for the tick labels
    Caption   : Caption of the axis.
    Drawzero  : Should the 0 line be drawn extra ?
    Origin    : Starting point (in X or Y) of the axis. 
    Interval  : Interval over which will be drawn. The axis covers the 
                X or Y interval [Origin,Origin+Interval]
       
    LegendInterval : The interval (in ticks) at which a legend for the tick
                     mark should be printed.
    LegendFormat : Controls the formatting of the legend at the tick marks.
                   This should be a valid float formatting specifier (used in FormatFloat);
    GridInterval : The number of ticks at which a grid line must be drawn.
    GridColor : Color of the grid line.

