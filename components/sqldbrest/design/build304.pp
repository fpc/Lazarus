unit build304;

{ ---------------------------------------------------------------------
  This unit is only needed to build with FPC 3.0.4:

  It makes sure that all units of the sqldbrestbridge in ../src are compiled.
  Otherwise, the ../src directory must be added to lazsqldbrest as well,
  which will lead to duplicate units warnings.

  As soon as FPC 3.2.0 is released, this unit can be removed.

  Michael.
  ---------------------------------------------------------------------}
{$mode objfpc}{$H+}

interface

uses
  sqldbrestcsv ,sqldbrestxml, sqldbrestcds, sqldbrestado,sqldbrestmodule;

implementation

end.

