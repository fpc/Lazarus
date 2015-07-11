unit frmselectconnectiontype;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, lazdatadeskstr;

type

  { TSelectConnectionTypeForm }

  TSelectConnectionTypeForm = class(TForm)
    BPConnections: TButtonPanel;
    LBConnections: TListBox;
    procedure FormCreate(Sender: TObject);
  private
    function GetSelected: String;
    { private declarations }
  public
    { public declarations }
    Property SelectedConnection : String Read GetSelected;
  end; 

var
  SelectConnectionTypeForm: TSelectConnectionTypeForm;

implementation

{$R *.lfm}

uses fpdatadict;

{ TSelectConnectionTypeForm }

Type

  { TEngineObject }

  TEngineObject = Class(TComponent)
  private
    FEN: String;
  Public
    Property EngineName : String Read FEN Write FEN;
  end;

procedure TSelectConnectionTypeForm.FormCreate(Sender: TObject);

Var
  L : TstringList;
  I : Integer;
  E : TEngineObject;
  dd,dt : string;
  cap : TFPDDEngineCapabilities;

begin
  //
  Caption := sld_Selectaconnectiontype;
  //
  L:=TStringList.Create;
  try
    GetDictionaryEngineList(L);
    L.Sorted:=True;
    For i:=0 to L.Count-1 do
      begin
      GetDictionaryEngineInfo(L[i],dd,dt,cap);
      E:=TEngineObject.Create(Self);
      E.EngineName:=L[i];
      LBConnections.Items.AddObject(DD,E);
      end;
  finally
    L.Free;
  end;

end;

function TSelectConnectionTypeForm.GetSelected: String;
begin
  If LBConnections.ItemIndex=-1 then
    Result:=''
  else
    Result:=TengineObject(LBConnections.Items.Objects[LBConnections.ItemIndex]).EngineName;
end;

end.

