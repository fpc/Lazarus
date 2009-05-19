unit fdockbook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls;

type
  TDockBook = class(TForm)
    Pages: TPageControl;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  DockBook: TDockBook;

implementation

initialization
  {$I fdockbook.lrs}

end.

