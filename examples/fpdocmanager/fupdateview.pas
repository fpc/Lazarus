unit fUpdateView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TUpdateView }

  TUpdateView = class(TForm)
    edUpdate: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  UpdateView: TUpdateView;

implementation

{$R *.lfm}

end.

