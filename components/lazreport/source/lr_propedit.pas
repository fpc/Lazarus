unit lr_propedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LR_Class;

type

  { TPropEditor }

  TPropEditor = class(TForm)
  protected
    FView: TfrView;
  public
    function ShowEditor(AView: TfrView): TModalResult; virtual;
    property View: TfrView read FView;
  end;

implementation

{ TPropEditor }

function TPropEditor.ShowEditor(AView: TfrView): TModalResult;
begin
  FView:=AView;
  Result := ShowModal;
end;

end.

