unit ExampleGrid1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, RTTIGrids,
  RTTICtrls;

type

  { TMyCollectionItem }

  TMyCollectionItem = class(TCollectionItem)
  private
    FInt: integer;
    FStr: string;
    FColor: TColor;
  published
    property Color: TColor read FColor write FColor;
    property Str: string read FStr write FStr;
    property Int: integer read FInt write FInt;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    TICheckBox1: TTICheckBox;
    TIGrid1: TTIGrid;
    procedure Form1Create(Sender: TObject);
    procedure Form1Destroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    MyCollection: TCollection;
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Form1Create(Sender: TObject);
var
  NewItem: TMyCollectionItem;
begin
  // create a collection with 2 items
  MyCollection:=TCollection.Create(TMyCollectionItem);

  NewItem:=TMyCollectionItem(MyCollection.Add);
  NewItem.Color:=clRed;
  NewItem.Str:='Some';
  NewItem.Int:=123;

  NewItem:=TMyCollectionItem(MyCollection.Add);
  NewItem.Color:=clBlue;
  NewItem.Str:='Text';
  NewItem.Int:=789;

  // show the collection
  TIGrid1.ListObject:=MyCollection;
end;

procedure TForm1.Form1Destroy(Sender: TObject);
begin
  // disconnect collection from grid
  TIGrid1.ListObject:=nil;
  // free collection
  MyCollection.Free;
end;

initialization
  {$I examplegrid1.lrs}

end.

