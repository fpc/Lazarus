unit androidstringlists;

{$mode objfpc}{$H+}

interface

uses
  // libs
  android_all, androidpipescomm,
  // wdgetset
  WSLCLClasses, LCLClasses,
  // LCL + RTL
  Types, Classes, SysUtils, Controls, LCLType, LCLProc, Graphics, Math, Contnrs,
  AVL_Tree, LMessages, LCLMessageGlue, stdctrls, Forms;

type
  { TAndroidComboBoxStrings }

  TAndroidComboBoxStrings = class(TStringList)
  private
    FWinControl: TWinControl;
    FOwner: TObject;//TAndroidComboBox;
  protected
    procedure Put(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    constructor Create(AWinControl: TWinControl; AOwner: TObject{TAndroidComboBox});
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Sort; override;
    procedure Exchange(AIndex1, AIndex2: Integer); override;
  end;

implementation

uses androidprivate;

{ TAndroidComboBoxStrings }

procedure TAndroidComboBoxStrings.Put(Index: Integer; const S: string);
begin
  inherited Put(Index, S);
  //FOwner.BeginUpdate;
  TAndroidComboBox(FOwner).Put(Index, S);
//  FOwner.setItemText(Index, S);
//  FOwner.EndUpdate;
end;

procedure TAndroidComboBoxStrings.InsertItem(Index: Integer; const S: string);
var
  FSavedIndex: Integer;
  FSavedText: WideString;
begin
  inherited InsertItem(Index, S);
{  //FOwner.BeginUpdate;
  FSavedText := FOwner.getText;
  FSavedIndex := FOwner.currentIndex;}
  TAndroidComboBox(FOwner).insertItem(Index, S);
{  if FOwner.getEditable then
  begin
    if (FSavedIndex <> FOwner.currentIndex) then
      FOwner.setCurrentIndex(FSavedIndex);
    FOwner.setText(FSavedText);
  end;
  FOwner.EndUpdate;}
end;

procedure TAndroidComboBoxStrings.InsertItem(Index: Integer; const S: string;
  O: TObject);
begin
  inherited InsertItem(Index, S, O);
end;

constructor TAndroidComboBoxStrings.Create(AWinControl: TWinControl;
  AOwner: TObject);
begin
  inherited Create;
  FWinControl := AWinControl;
  FOwner := AOwner;
  {$ifdef LCL_ANDROID_STDCTRLS_VERBOSE}
  vAndroidPipesComm.Log(Format('[TAndroidComboBox.Clear] AWinControl=%P AOwner=%P', [@AWinControl, @AOwner]));
  {$endif}
end;

procedure TAndroidComboBoxStrings.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TAndroidComboBoxStrings.Clear;
begin
  inherited Clear;
  TAndroidComboBox(FOwner).clear;
end;

procedure TAndroidComboBoxStrings.Delete(Index: Integer);
begin
  inherited Delete(Index);
  TAndroidComboBox(FOwner).delete(Index);
end;

procedure TAndroidComboBoxStrings.Sort;
begin
  inherited Sort;
end;

procedure TAndroidComboBoxStrings.Exchange(AIndex1, AIndex2: Integer);
begin
  inherited Exchange(AIndex1, AIndex2);
end;

end.

