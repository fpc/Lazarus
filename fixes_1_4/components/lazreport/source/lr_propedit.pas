unit lr_propedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LR_Class;

type

  { TPropEditor }

  TPropEditor = class(TForm)
  public
    View: TfrView;

    function ShowEditor: TModalResult; virtual;
  end;

implementation

{ TPropEditor }

function TPropEditor.ShowEditor: TModalResult;
begin
  Result := ShowModal;
end;

end.

