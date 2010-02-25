{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Michael Van Canneyt

  Page editor. Edits 1 file. A page editor exists of a package editor
  and an element editor, joined on a panel with a splitter between them.
  
  It is built modular, in 3 pieces.
  - Package editor.  Edits structure of the documentation file.
  - Element editor.  Edits 1 element node in the documentation file.
  - EditorPage.      Combines the above two to one visual element.
  
  The package editor is mainly concerned with structure of the file in
  terms of packages/topic/module/element. All things which are concerned
  with structure are shunted to the package editor. In turn, the package
  editor has a series of event handlers in order to react on changes.
  
  The element editor edits 1 element. It can handle the following tags:
  short/descr/errors/seealso/example

  There is no direct interaction between the package editor and the element
  editor. This allows changing either of them without needing to change the other.
  The page editor handles all communication between the two.
  
  This allows to implement several editors. One could also implement the
  editor page so it is split horizontal, whatever.

}
unit pgEditor;

{$mode objfpc}
{$h+}

interface

uses SysUtils,Classes,dom,xmlread,xmlwrite,Forms,Controls,FileUtil,extctrls,
     comctrls,Dialogs,menus,pkeditor,eleditor,fpdeutil;

type
  TEditorPageNew = class(TFrame)

  end;

Type

  { TEditorPage }
  TEditorPage = Class(TTabSheet)
  Private
    FDocument : TXMLDocument;
    FPackages : TCustomPackageEditor;
    FElement : TCustomElementEditor;
    FSplitter : TSplitter;
    FFileNAme : String;
    Procedure ElementSelected(Node : TDomElement) ;
    Procedure TopicSelected(Node : TDomElement) ;
    Procedure ModuleSelected(Node : TDomElement) ;
    Procedure PackageSelected(Node : TDomElement) ;
//    Procedure SelectionChanged(Sender : TObject; Node : TDomElement) ;
//    Procedure ElementDeSelected(Sender : TObject; Node : TDomElement) ;
    Function GetCurrentSelection : String;
    Function GetCurrentPackage : TDomElement;
    Function GetCurrentModule : TDomElement;
    Function GetCurrentTopic : TDomElement;
    Function GetCurrentElement : TDomElement;
    Procedure SetCurrentModule(Value : TDomElement);
    Procedure SetCurrentTopic(Value : TDomElement);
    Procedure SetCurrentPackage(Value : TDomElement);
    Procedure SetCurrentElement(Value : TDomElement);
    Procedure SetModified(Value : Boolean);
    Function  GetModified : Boolean;
    Function MakeBackup(FN : String) : Boolean;
    Procedure DisplayDocument;
    Procedure ElementChanged(Sender: TObject);
  protected
    procedure SetParent(NewParent: TWinControl); override;
  Public
    constructor Create(AOwner : TComponent); override;
    Function  FirstPackage : TDomElement;
    Function  FirstModule(APackage : TDomElement) : TDomElement;
    Procedure LoadFromFile(FN : String);
    Procedure LoadFromStream(S : TStream);
    Procedure SaveToFile(FN : String);
    Procedure SetFileName(FN : String);
    Procedure InsertTag(TagType : TTagType);
    Procedure InsertLink(LinkTarget,LinkText : String);
    Procedure InsertTable(Cols,Rows : Integer; UseHeader : Boolean);
    Procedure InsertShortPrintLink(pLinkTarget: string);
    Procedure NewPackage(APackageName : String);
    Procedure NewModule(AModuleName : String);
    Procedure NewTopic(ATopicName : String);
    Procedure NewElement(AElementName : String);
    Procedure GetElementList(List : TStrings);
    function  GetInitialDir: String;
    Procedure ClearDocument;
    Function CanInsertTag(TagType : TTagType) : Boolean;
    Property FileName : String Read FFileName;
    Property CurrentSelection : String Read GetCurrentSelection;
    Property CurrentPackage : TDomElement Read GetCurrentPackage Write SetCurrentPackage;
    Property CurrentModule : TDomElement Read GetCurrentModule Write SetCurrentModule;
    Property CurrentTopic : TDomElement Read GetCurrentTopic Write SetCurrentTopic;
    Property CurrentElement : TDomElement Read GetCurrentElement Write SetCurrentElement;
    Property Modified : Boolean Read GetModified Write SetModified;
  end;


implementation

uses frmnewnode,lazdeopts,lazdemsg;

{$R *.lfm}

{ ---------------------------------------------------------------------
  TPageEditor
  ---------------------------------------------------------------------}

constructor TEditorPage.Create(AOwner : TComponent);
begin
  inherited;
  FPackages:=TPackageEditor.Create(Self);
  FPackages.Parent:=Self;
  FPackages.Align:=alLeft;
  FPackages.OnSelectElement:=@ElementSelected;
  FPackages.OnSelectModule:=@ModuleSelected;
  FPackages.OnSelectPackage:=@PackageSelected;
  FPackages.OnSelectTopic:=@TopicSelected;

  FSplitter:=TSplitter.Create(Self);
  FSPlitter.Parent:=Self;
  FSplitter.Align:=alLeft;
  FSplitter.Width:=5;

  FElement:=TElementEditor.Create(Self);
  FElement.Parent:=Self;
  FElement.Align:=AlClient;
  FElement.OnGetElementList:=@GetELementList;
  FElement.OnGetInitialDir:=@GetInitialDir;
  FElement.OnChange:=@ElementChanged;
end;



Procedure TEditorPage.ClearDocument;

begin
  if (FDocument<>nil) then
    begin
    FDocument.Free;
    FDocument:=Nil;
    end;
end;

function TEditorPage.CanInsertTag(TagType: TTagType): Boolean;
begin
  Result:=FElement.CanInsertTag(TagType);
end;

Procedure TEditorPage.LoadFromFile(FN : String);

Var
  F : TFileStream;

begin
  ClearDocument;
  F:=TFileStream.Create(UTF8ToSys(FN),fmOpenRead);
  Try
    SetFileName(FN);
    ReadXMLFile(FDocument,F);
    DisplayDocument;
  finally
    F.Free;
  end;
end;

Procedure TEditorPage.LoadFromStream(S : TStream);

begin
  ClearDocument;
  ReadXMLFile(FDocument,S);
  SetFileName(SNewDocument);
  DisplayDocument;
end;

Procedure TEditorPage.SetFileName(FN : String);

begin
  FFileName:=FN;
  Caption:=ChangeFileExt(ExtractFileName(FN),'');
end;

Function TEditorPage.MakeBackup(FN : String) : Boolean;

Var
  BN : String;

begin
  Result:=Not CreateBackup;
  If not Result then
    begin
    BN:=ChangeFileExt(FN,BackupExtension);
    Result:=RenameFileUTF8(FN,BN);
    end;
end;

Procedure TEditorPage.SaveToFile(FN : String);
begin
  MakeBackup(FN);
  if FN <> FFileName then SetFileName(FN);
  If FElement.Modified then FElement.Save;
  WriteXMLFile(FDocument, FN);
  Modified :=False;
end;

Procedure TEditorPage.DisplayDocument;

begin
  FPackages.DescriptionNode:=FDocument.DocumentElement;
end;

procedure TEditorPage.ElementChanged(Sender: TObject);
begin
  if Sender=nil then ;
  TPackageEditor(FPackages).UpdateSelectedNodeStatus;
end;

procedure TEditorPage.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if Assigned(NewParent) then
    FSplitter.Width:=5;
end;



Procedure TEditorPage.ElementSelected(Node : TDomElement) ;

Var
  OldNode : TDomElement;

begin
  OldNode:=FElement.Element;
  If OldNode<>Node then
    FElement.Element:=Node;
end;

Procedure TEditorPage.PackageSelected(Node : TDomElement) ;

begin
  ElementSelected(Node);
end;

Procedure TEditorPage.ModuleSelected(Node : TDomElement) ;

begin
  ElementSelected(Node);
end;

Procedure TEditorPage.TopicSelected(Node : TDomElement) ;

begin
  ElementSelected(Node);
end;


Procedure TEditorPage.InsertTag(TagType : TTagType);

begin
  FElement.InsertTag(TagType)
end;

Procedure TEditorPage.InsertLink(LinkTarget,LinkText : String);

begin
  FElement.InsertLink(LinkTarget,LinkText);
end;


Procedure TEditorPage.InsertTable(Cols,Rows : Integer; UseHeader : Boolean);

begin
  Felement.InsertTable(Cols,Rows,UseHeader);
end;


procedure TEditorPage.InsertShortPrintLink(pLinkTarget: string);
begin
  FElement.InsertPrintShortLink(pLinkTarget);
end;


Function TEditorPage.GetCurrentSelection : String;

begin
  Result:=FElement.CurrentSelection;
end;

Procedure TEditorPage.NewPackage(APackageName : String);

Var
  P : TDomElement;

begin
  P:=FDocument.CreateElement('package');
  P['name']:=APAckageName;
  FDocument.DocumentElement.AppendChild(P);
  FPackages.Refresh;
  FPackages.Modified:=True;
  CurrentPackage:=P;
end;

Function TEditorPage.FirstPackage : TDomElement;

Var
  N : TDomNode;

begin
  N:=FDocument.DocumentElement.FirstChild;
  While (N<>Nil) and Not IsPackageNode(N) do
    N:=N.NextSibling;
  Result:=TDomElement(N);
end;

Function TEditorPage.FirstModule(APackage : TDomElement) : TDomElement;

Var 
  N : TDomNode;

begin
  N:=APAckage.FirstChild;
  While (N<>Nil) and Not IsModuleNode(N) do
      N:=N.NextSibling;
  Result:=TDomElement(N);
end;

Procedure TEditorPage.NewModule(AModuleName : String);

Var 
  M,P : TDomElement;
  
begin
  If CurrentPackage<>Nil then
    P:=CurrentPackage
  else
    P:=FirstPackage;
  If (P=Nil) then  
    Raise Exception.CreateFmt(SErrNoPackageForModule,[AModuleName]);
  M:=FDocument.CreateElement('module');
  M['name']:=AModuleName;
  P.AppendChild(M);
  FPackages.Refresh;
  FPackages.Modified:=True;
  CurrentModule:=M;
end;

Procedure TEditorPage.NewTopic(ATopicName : String);

Var 
  T,M,P : TDomElement;
  
begin
  { 
    If currently a topic is selected, make a subtopic, or a sibling topic.
    If no topic is selected, then make a topic under the current module or 
    package. A menu to move topics up/down is needed...
  }
  if (CurrentTopic<>Nil) then
    begin
    M:=CurrentTopic.ParentNode as TDomElement;
    If M.NodeName='module' then
      P:=M
    else if M.NodeName='topic' then
      P:=M
    else   
      P:=CurrentTopic;
    end
  else if (CurrentModule<>Nil) then
    P:=CurrentModule
  else if (CurrentPackage<>Nil) then
    P:=CurrentPackage
  else
    P:=FirstPackage;
  If (P=Nil) then  
    Raise Exception.CreateFmt(SErrNoNodeForTopic,[ATopicName]);
  T:=FDocument.CreateElement('topic');
  T['name']:=ATopicName;
  P.AppendChild(T);
  FPackages.Refresh;
  FPackages.Modified:=True;
  CurrentTopic:=T;
end;

Procedure TEditorPage.NewElement(AElementName : String);

Var 
  P,E,M : TDomElement;

begin
  If CurrentModule<>Nil then
    M:=CurrentModule
  else
    begin
    P:=FirstPackage;
    If P<>Nil then
      M:=FirstModule(P)
    else
      M:=Nil;
    If M<>Nil then
      CurrentModule:=M;  
    end;  
  If (M=Nil) then  
    Raise Exception.CreateFmt(SErrNoModuleForElement,[AElementName]);
  E:=FDocument.CreateElement('element');
  E['name']:=AElementName;
  M.AppendChild(E);
  FPackages.AddElement(E);
end;

Function TEditorPage.GetCurrentPackage : TDomElement;

begin
  Result:=FPackages.CurrentPackage;
end;


Function TEditorPage.GetCurrentModule : TDomElement;

begin
  Result:=FPackages.CurrentModule;
end;


Function TEditorPage.GetCurrentTopic : TDomElement;

begin
  Result:=FPackages.CurrentTopic;
end;


Function TEditorPage.GetCurrentElement : TDomElement;
begin
  Result:=FElement.Element;
end;

Procedure TEditorPage.SetCurrentElement(Value : TDomElement);

begin
  FPackages.CurrentElement:=Value;
end;


Procedure TEditorPage.SetCurrentModule(Value : TDomElement);

begin
  FPackages.CurrentModule:=Value;
end;


Procedure TEditorPage.SetCurrentTopic(Value : TDomElement);

begin
  FPackages.CurrentTopic:=Value;
end;


Procedure TEditorPage.SetCurrentPackage(Value : TDomElement);

begin
  FPackages.CurrentPackage:=Value;
end;

Procedure TEditorPage.SetModified(Value : Boolean);

begin
  If Not Value then
    begin
    FPackages.Modified:=False;
    FElement.Modified:=False;
    FElement.SavedNode:=False;
    end;
end;

Function TEditorPage.GetModified : Boolean;

begin
  Result:=FPackages.Modified or
          FElement.Modified or
          FElement.SavedNode;
end;

Procedure TEditorPage.GetElementList(List : TStrings);

Var
  N : TDOmNode;

begin
 With List do
   begin
   Clear;
   If Assigned(CurrentModule) then
     begin
     N:=Currentmodule.FirstChild;
     While (N<>Nil) do
       begin
       If (N is TDomElement) and (N.NodeName='element') then
         Add(TDomElement(N)['name']);
       N:=N.NextSibling;  
       end;
     end;  
   end;
end;

function TEditorPage.GetInitialDir: String;
begin
  result := '';
  if FileExistsUTF8(FFileName) then
    Result := ExtractFilePath(FFileName);
end;

end.
