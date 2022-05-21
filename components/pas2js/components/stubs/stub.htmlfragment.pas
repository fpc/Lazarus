unit stub.htmlfragment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TAllowUnrenderEvent = Procedure (Sender : TObject; var allowClose : Boolean) of object;

  { TCustomHTMLFragment }

  TCustomHTMLFragment = class(TDatamodule)
  private
    FHTMLFile: String;
    FOnAllowUnrender: TAllowUnrenderEvent;
    FOnHTMLLoaded: TNotifyEvent;
    FOnRendered: TNotifyEvent;
    FOnRenderFail: TNotifyEvent;
    FOnUnRendered: TNotifyEvent;
    FParentID: String;
    FTemplateName: String;
    FIsRendered : Boolean;
    FUseProjectHTMLFile: Boolean;
    procedure SetUseProjectHTMLFile(AValue: Boolean);
  Protected
    Property ParentID : String Read FParentID Write FParentID;
    Property TemplateName : String Read FTemplateName Write FTemplateName;
    Property HTMLFileName : String Read FHTMLFile Write FHTMLFile;
    Property OnRendered : TNotifyEvent Read FOnRendered Write FOnrendered;
    Property OnUnRendered : TNotifyEvent Read FOnUnRendered Write FOnUnrendered;
    Property OnRenderFail : TNotifyEvent Read FOnRenderFail Write FOnrenderFail;
    Property OnHTMLLoaded : TNotifyEvent Read FOnHTMLLoaded Write FOnHTMLLoaded;
  //    Property AppendToParent : Boolean Read FAppendToParent Write FAppendToParent;
    Property OnAllowUnrender : TAllowUnrenderEvent Read FOnAllowUnrender Write FOnAllowUnrender;
  Public
    Procedure Render; virtual;
    Procedure UnRender;
    Procedure Show;
    Procedure Hide;
    Property UseProjectHTMLFile : Boolean Read FUseProjectHTMLFile Write SetUseProjectHTMLFile;
    Property IsRendered : Boolean Read FIsRendered;
  end;

  THTMLFragment = class(TCustomHTMLFragment)
  Published
    Property UseProjectHTMLFile;
    Property ParentID;
    Property TemplateName;
    Property HTMLFileName;
    Property OnRendered;
    Property OnHTMLLoaded;
  //    Property AppendToParent;
    Property OnAllowUnrender;
    Property OnUnrendered;
  end;


implementation

{ TCustomHTMLFragment }

procedure TCustomHTMLFragment.SetUseProjectHTMLFile(AValue: Boolean);
begin
  if FUseProjectHTMLFile=AValue then Exit;
  FUseProjectHTMLFile:=AValue;
  if FUseProjectHTMLFile then
    begin
    TemplateName:='';
    HTMLFileName:='';
    end;
end;

procedure TCustomHTMLFragment.Render;
begin

end;

procedure TCustomHTMLFragment.UnRender;
begin

end;

procedure TCustomHTMLFragment.Show;
begin

end;

procedure TCustomHTMLFragment.Hide;
begin

end;

end.

