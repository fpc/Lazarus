{ $Id$
                  -----------------------------------
                  carbonstrings.pp  -  Carbon strings
                  -----------------------------------

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonStrings;

{$mode objfpc}{$H+}

interface

// debugging defines
{$I carbondebug.inc}

uses
 // carbon bindings
{$ifdef ver2_2_0}
  FPCMacOSAll,
{$else}
  MacOSAll,
{$endif}
 // rtl+ftl
  Types, Classes, SysUtils, Math,
 // LCL
  LCLProc, LCLType, Graphics, Controls, StdCtrls,
 // LCL Carbon
  CarbonDef, CarbonEdits, CarbonListViews;

type
  { TCarbonComboBoxStrings }

  TCarbonComboBoxStrings = class(TStringList)
  private
    FOwner: TCarbonComboBox;  // Carbon combo box control owning strings
  protected
    procedure Put(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    constructor Create(AOwner: TCarbonComboBox);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Sort; override;
  public
    property Owner: TCarbonComboBox read FOwner;
  end;
  
  { TCarbonListBoxStrings }

  TCarbonListBoxStrings = class(TStringList)
  private
    FOwner: TCarbonListBox;  // Carbon list box control owning strings
  protected
    procedure Put(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    constructor Create(AOwner: TCarbonListBox);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Sort; override;
  public
    property Owner: TCarbonListBox read FOwner;
  end;

  { TCarbonMemoStrings }

  TCarbonMemoStrings = class(TStrings)
  private
    FStringList: TStringList; // internal string list
    FOwner: TCarbonMemo;      // Carbon memo control owning strings
    FExternalChanged: Boolean;// Carbon strings object has changed
    procedure InternalUpdate;
    procedure ExternalUpdate;
  protected
    function GetTextStr: string; override;
    function GetCount: Integer; override;
    function Get(Index: Integer): string; override;
  public
    constructor Create(AOwner: TCarbonMemo);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure SetText(TheText: PChar); override;

    procedure ExternalChanged; dynamic;
  public
    property Owner: TCarbonMemo read FOwner;
  end;


implementation

uses
  CarbonProc;

{ TCarbonComboBoxStrings }

{------------------------------------------------------------------------------
  Method:  TCarbonComboBoxStrings.Put
  Params:  Index - Index of string to change
           S     - New text

  Changes the text on line with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonComboBoxStrings.Put(Index: Integer; const S: string);
begin
  inherited Put(Index, S);

  FOwner.Remove(Index);
  FOwner.Insert(Index, S);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBoxStrings.InsertItem
  Params:  Index - Line index
           S     - Text to insert

  Inserts the text on line with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonComboBoxStrings.InsertItem(Index: Integer; const S: string);
begin
  inherited InsertItem(Index, S);

  FOwner.Insert(Index, S);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBoxStrings.InsertItem
  Params:  Index - Line index
           S     - Text to insert
           O     - Object to insert

  Inserts the text on line with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonComboBoxStrings.InsertItem(Index: Integer; const S: string;
  O: TObject);
begin
  inherited InsertItem(Index, S, O);

  FOwner.Insert(Index, S);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBoxStrings.Create
  Params:  AOwner - Carbon combo box owner of strings

  Creates new strings for Carbon combo box items
 ------------------------------------------------------------------------------}
constructor TCarbonComboBoxStrings.Create(AOwner: TCarbonComboBox);
begin
  inherited Create;
  FOwner := AOwner;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBoxStrings.Clear

  Clears strings
 ------------------------------------------------------------------------------}
procedure TCarbonComboBoxStrings.Clear;
var
  I: Integer;
  C: Integer;
begin
  C := Count;
  
  inherited Clear;
  
  for I := C - 1 downto 0 do FOwner.Remove(I);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBoxStrings.Delete
  Params:  Index - Line index

  Deletes line with the specified index from strings
 ------------------------------------------------------------------------------}
procedure TCarbonComboBoxStrings.Delete(Index: Integer);
begin
  inherited Delete(Index);
  
  FOwner.Remove(Index);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonComboBoxStrings.Sort

  Sorts the strings
 ------------------------------------------------------------------------------}
procedure TCarbonComboBoxStrings.Sort;
var
  I: Integer;
begin
  inherited Sort;
  
  for I := 0 to Count - 1 do
  begin
    FOwner.Remove(I);
    FOwner.Insert(I, Strings[I]);
  end;
end;

{ TCarbonListBoxStrings }

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.Put
  Params:  Index - Index of string to change
           S     - New text

  Changes the text on line with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.Put(Index: Integer; const S: string);
begin
  inherited Put(Index, S);

  FOwner.UpdateItem(Index);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.InsertItem
  Params:  Index - Line index
           S     - Text to insert

  Inserts the text on line with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.InsertItem(Index: Integer; const S: string);
begin
  inherited InsertItem(Index, S);

  FOwner.InsertItem(Index);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.InsertItem
  Params:  Index - Line index
           S     - Text to insert
           O     - Object to insert

  Inserts the text on line with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.InsertItem(Index: Integer; const S: string;
  O: TObject);
begin
  inherited InsertItem(Index, S, O);

  FOwner.InsertItem(Index);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.Create
  Params:  AOwner - Carbon list box owner of strings

  Creates new strings for Carbon list box items
 ------------------------------------------------------------------------------}
constructor TCarbonListBoxStrings.Create(AOwner: TCarbonListBox);
begin
  inherited Create;
  FOwner := AOwner;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.Clear

  Clears strings
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.Clear;
begin
  inherited Clear;

  FOwner.ClearItems;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.Delete
  Params:  Index - Line index

  Deletes line with the specified index from strings
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.Delete(Index: Integer);
begin
  inherited Delete(Index);

  FOwner.DeleteItem(Index);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBoxStrings.Sort

  Sorts the strings
 ------------------------------------------------------------------------------}
procedure TCarbonListBoxStrings.Sort;
begin
  inherited Sort;

  FOwner.UpdateItems;
end;

{ TCarbonMemoStrings }

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.InternalUpdate

  Updates the internal strings from Carbon interface
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.InternalUpdate;
var
  S: String;
begin
  S := '';
  //DebugLn('TCarbonMemoStrings.InternalUpdate');
  if FOwner.GetText(S) then
    FStringList.Text := S;

  FExternalChanged := False;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.ExternalUpdate

  Updates the strings in Carbon interface from internal
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.ExternalUpdate;
begin
  //DebugLn('TCarbonMemoStrings.ExternalUpdate Text: ' + FStringList.Text);
  FOwner.SetText(FStringList.Text);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.GetTextStr
  Returns: Text of Carbon strings
 ------------------------------------------------------------------------------}
function TCarbonMemoStrings.GetTextStr: string;
begin
  if FExternalChanged then InternalUpdate;
  Result := FStringList.Text;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.GetCount
  Returns: Number of lines
 ------------------------------------------------------------------------------}
function TCarbonMemoStrings.GetCount: Integer;
begin
  if FExternalChanged then InternalUpdate;
  Result := FStringList.Count;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Get
  Params:  Index - Line index
  Returns: Text on line with the specified index
 ------------------------------------------------------------------------------}
function TCarbonMemoStrings.Get(Index: Integer): string;
begin
  if FExternalChanged then InternalUpdate;
  Result := FStringList[Index];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Create
  Params:  AOwner - Carbon memo owner of strings

  Creates new strings for Carbon memo strings
 ------------------------------------------------------------------------------}
constructor TCarbonMemoStrings.Create(AOwner: TCarbonMemo);
begin
  FOwner := AOwner;
  FStringList := TStringList.Create;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Destroy

  Releases strings from Carbon memo strings
 ------------------------------------------------------------------------------}
destructor TCarbonMemoStrings.Destroy;
begin
  FStringList.Free;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Assign
  Params:  Source - Object to assing

  Assings strings object
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.Assign(Source: TPersistent);
begin
  if (Source = Self) or (Source = nil) then Exit;
  if Source is TStrings then
  begin
    FStringList.Clear;
    FStringList.Text := TStrings(Source).Text;
    ExternalUpdate;
  end
  else
    inherited Assign(Source);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Clear

  Clears strings
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.Clear;
begin
  FStringList.Clear;
  ExternalUpdate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Delete
  Params:  Index - Line index

  Deletes line with the specified index from strings
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.Delete(Index: Integer);
begin
  FStringList.Delete(Index);
  ExternalUpdate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.Insert
  Params:  Index - Line index
           S     - Text to insert

  Inserts the text on line with the specified index
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.Insert(Index: Integer; const S: string);
begin
  FStringList.Insert(Index, S);
  ExternalUpdate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.SetText
  Params:  TheText - Text to set

  Sets the text of strings
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.SetText(TheText: PChar);
begin
  FStringList.Text := TheText;
  ExternalUpdate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonMemoStrings.ExternalChanged

  Notifies that strings object in Carbon interface has changed
 ------------------------------------------------------------------------------}
procedure TCarbonMemoStrings.ExternalChanged;
begin
  FExternalChanged := True;
end;


end.
