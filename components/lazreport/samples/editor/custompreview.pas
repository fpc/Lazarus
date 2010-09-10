unit custompreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LR_View;

type

  { TfrmCustomPreview }

  TfrmCustomPreview = class(TForm)
    frPreview1: TfrPreview;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmCustomPreview: TfrmCustomPreview;

implementation

{$R *.lfm}

end.

