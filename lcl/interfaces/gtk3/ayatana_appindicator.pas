unit ayatana_appindicator;

{$mode delphi}

interface

uses dynlibs,glib2,lazgtk3,lazgobject2,lazglib2;

const
  libappindicator_3 = 'libappindicator3.so.1';              // Unity or Canonical libappindicator3-1
  LibAyatanaAppIndicator = 'libayatana-appindicator3.so.1'; // Ayatana - typically called libayatana-appindicator3-1
  IconType = 'png';

type
  TAppIndicatorCategory = (
    APP_INDICATOR_CATEGORY_APPLICATION_STATUS,
    APP_INDICATOR_CATEGORY_COMMUNICATIONS,
    APP_INDICATOR_CATEGORY_SYSTEM_SERVICES,
    APP_INDICATOR_CATEGORY_HARDWARE,
    APP_INDICATOR_CATEGORY_OTHER
  );

  TAppIndicatorStatus = (
    APP_INDICATOR_STATUS_PASSIVE,
    APP_INDICATOR_STATUS_ACTIVE,
    APP_INDICATOR_STATUS_ATTENTION
  );

  PAppIndicator = ^TAppIndicator;

  { TAppIndicator }

  TAppIndicator = object(TGObject)
    function get_type:GType; inline;
    function new(id, icon_name: PGChar; category: TAppIndicatorCategory): PAppIndicator;inline;static;
    function new_with_path(id, icon_name: PGChar; category: TAppIndicatorCategory; icon_theme_path: PGChar): PAppIndicator;inline;static;
    { Set properties }
    procedure set_status(status: TAppIndicatorStatus); inline;
    procedure set_attention_icon(const icon_name: PGChar); inline;
    procedure set_attention_icon_full(const icon_name,icon_desc:PGChar);inline;
    procedure set_menu(menu: PGtkMenu); inline;
    procedure set_icon(const icon_name: PGChar); inline;
    procedure set_label(_label, guide: PGChar); inline;
    procedure set_icon_theme_path(icon_theme_path: PGChar); inline;
    procedure set_ordering_index(ordering_index: guint32); inline;
    procedure set_title(const title:PgChar); inline;
    { Get properties }
    function get_id(): PGChar; inline;
    function get_category(): TAppIndicatorCategory; inline;
    function get_status(): TAppIndicatorStatus; inline;
    function get_icon(): PGChar; inline;
    function get_icon_theme_path(): PGChar; inline;
    function get_attention_icon(): PGChar; inline;
    function get_menu(): PGtkMenu; inline;
    function get_label(): PGChar; inline;
    function get_label_guide(): PGChar; inline;
    function get_ordering_index(): guint32; inline;
  end;

var
  { GlobalAppIndicator creation routines }
  app_indicator_get_type: function: GType; cdecl;
  app_indicator_new: function(id, icon_name: PGChar; category: TAppIndicatorCategory): PAppIndicator; cdecl;
  app_indicator_new_with_path: function(id, icon_name: PGChar; category: TAppIndicatorCategory; icon_theme_path: PGChar): PAppIndicator; cdecl;
  { Set properties }
  app_indicator_set_status: procedure(self: PAppIndicator; status: TAppIndicatorStatus); cdecl;
  app_indicator_set_attention_icon: procedure(self: PAppIndicator; const icon_name: PGChar); cdecl;
  app_indicator_set_attention_icon_full: procedure(self: PAppIndicator; const icon_name,icon_desc:PGChar);cdecl;
  app_indicator_set_menu: procedure(self: PAppIndicator; menu: PGtkMenu); cdecl;
  app_indicator_set_icon: procedure(self: PAppIndicator; icon_name: PGChar); cdecl;
  app_indicator_set_label: procedure(self: PAppIndicator; _label, guide: PGChar); cdecl;
  app_indicator_set_icon_theme_path: procedure(self: PAppIndicator; icon_theme_path: PGChar); cdecl;
  app_indicator_set_ordering_index: procedure(self: PAppIndicator; ordering_index: guint32); cdecl;
  app_indicator_set_title: procedure(self: PAppIndicator;const title:Pgchar); cdecl;
  { Get properties }
  app_indicator_get_id: function(self: PAppIndicator): PGChar; cdecl;
  app_indicator_get_category: function(self: PAppIndicator): TAppIndicatorCategory; cdecl;
  app_indicator_get_status: function(self: PAppIndicator): TAppIndicatorStatus; cdecl;
  app_indicator_get_icon: function(self: PAppIndicator): PGChar; cdecl;
  app_indicator_get_icon_theme_path: function(self: PAppIndicator): PGChar; cdecl;
  app_indicator_get_attention_icon: function(self: PAppIndicator): PGChar; cdecl;
  app_indicator_get_menu: function(self: PAppIndicator): PGtkMenu; cdecl;
  app_indicator_get_label: function(self: PAppIndicator): PGChar; cdecl;
  app_indicator_get_label_guide: function(self: PAppIndicator): PGChar; cdecl;
  app_indicator_get_ordering_index: function(self: PAppIndicator): guint32; cdecl;


  function Gtk3AppIndicatorInit: Boolean;

implementation



{ TAppIndicator }

function TAppIndicator.get_type: GType;
begin
  Result := app_indicator_get_type();
end;

function TAppIndicator.new(id, icon_name: PGChar;
  category: TAppIndicatorCategory): PAppIndicator;
begin
  Result:=app_indicator_new(id, icon_name, category);
end;

function TAppIndicator.new_with_path(id, icon_name: PGChar;
  category: TAppIndicatorCategory; icon_theme_path: PGChar): PAppIndicator;
begin
  Result:=app_indicator_new_with_path(id, icon_name, category, icon_theme_path);
end;

procedure TAppIndicator.set_status(status: TAppIndicatorStatus);
begin
  app_indicator_set_status(@self, status);
end;

procedure TAppIndicator.set_attention_icon(const icon_name: PGChar);
begin
  app_indicator_set_attention_icon(@self, icon_name);
end;

procedure TAppIndicator.set_attention_icon_full(const icon_name,
  icon_desc: PGChar);
begin
  app_indicator_set_attention_icon_full(@self, icon_name, icon_desc);
end;

procedure TAppIndicator.set_menu(menu: PGtkMenu);
begin
  app_indicator_set_menu(@self, menu);
end;

procedure TAppIndicator.set_icon(const icon_name: PGChar);
begin
  app_indicator_set_icon(@self, icon_name);
end;

procedure TAppIndicator.set_label(_label, guide: PGChar);
begin
  app_indicator_set_label(@self, _label, guide);
end;

procedure TAppIndicator.set_icon_theme_path(icon_theme_path: PGChar);
begin
  app_indicator_set_icon_theme_path(@self, icon_theme_path);
end;

procedure TAppIndicator.set_ordering_index(ordering_index: guint32);
begin
  app_indicator_set_ordering_index(@self, ordering_index);
end;

procedure TAppIndicator.set_title(const title: PgChar);
begin
  app_indicator_set_title(@self, title);
end;

function TAppIndicator.get_id(): PGChar;
begin
  Result := app_indicator_get_id(@self);
end;

function TAppIndicator.get_category(): TAppIndicatorCategory;
begin
  Result := app_indicator_get_category(@self);
end;

function TAppIndicator.get_status(): TAppIndicatorStatus;
begin
  Result := app_indicator_get_status(@self);
end;

function TAppIndicator.get_icon(): PGChar;
begin
  Result := app_indicator_get_icon(@self);
end;

function TAppIndicator.get_icon_theme_path(): PGChar;
begin
  Result := app_indicator_get_icon_theme_path(@self);
end;

function TAppIndicator.get_attention_icon(): PGChar;
begin
  Result := app_indicator_get_attention_icon(@self);
end;

function TAppIndicator.get_menu(): PGtkMenu;
begin
  Result := app_indicator_get_menu(@self);
end;

function TAppIndicator.get_label(): PGChar;
begin
  Result := app_indicator_get_label(@self);
end;

function TAppIndicator.get_label_guide(): PGChar;
begin
  Result := app_indicator_get_label_guide(@self);
end;

function TAppIndicator.get_ordering_index(): guint32;
begin
  Result := app_indicator_get_ordering_index(@self);
end;


var
  Loaded: Boolean;
  Initialized: Boolean;

function Gtk3AppIndicatorInit: Boolean;
var
  Module: HModule;

  function TryLoad(const ProcName: string; var Proc: Pointer): Boolean;
  begin
    Proc := GetProcAddress(Module, ProcName);
    Result := Proc <> nil;
  end;

begin
  Result := False;
  if Loaded then
    Exit(Initialized);
  Loaded := True;
  if Initialized then
    Exit(True);
  Module := LoadLibrary(libappindicator_3);
  if Module = 0 then begin
    Module := LoadLibrary(LibAyatanaAppIndicator);
    if Module = 0 then begin                    // Sorry, no TrayIcon !
      {$ifdef DEBUGAPPIND}
      writeln('Failed to load an appindicator library');{$endif}
      Exit;
    end
    {$ifdef DEBUGAPPIND} else writeln('Loaded ' + LibAyatanaAppIndicator){$endif};
  end {$ifdef DEBUGAPPIND} else writeln('Loaded ' + libappindicator_3){$endif};
  Result :=
    TryLoad('app_indicator_get_type', @app_indicator_get_type) and
    TryLoad('app_indicator_new', @app_indicator_new) and
    TryLoad('app_indicator_new_with_path', @app_indicator_new_with_path) and
    TryLoad('app_indicator_set_status', @app_indicator_set_status) and
    TryLoad('app_indicator_set_attention_icon', @app_indicator_set_attention_icon) and
    TryLoad('app_indicator_set_attention_icon_full', @app_indicator_set_attention_icon_full) and
    TryLoad('app_indicator_set_menu', @app_indicator_set_menu) and
    TryLoad('app_indicator_set_icon', @app_indicator_set_icon) and
    TryLoad('app_indicator_set_label', @app_indicator_set_label) and
    TryLoad('app_indicator_set_icon_theme_path', @app_indicator_set_icon_theme_path) and
    TryLoad('app_indicator_set_ordering_index', @app_indicator_set_ordering_index) and
    TryLoad('app_indicator_set_title', @app_indicator_set_title) and
    TryLoad('app_indicator_get_id', @app_indicator_get_id) and
    TryLoad('app_indicator_get_category', @app_indicator_get_category) and
    TryLoad('app_indicator_get_status', @app_indicator_get_status) and
    TryLoad('app_indicator_get_icon', @app_indicator_get_icon) and
    TryLoad('app_indicator_get_icon_theme_path', @app_indicator_get_icon_theme_path) and
    TryLoad('app_indicator_get_attention_icon', @app_indicator_get_attention_icon) and
    TryLoad('app_indicator_get_menu', @app_indicator_get_menu) and
    TryLoad('app_indicator_get_label', @app_indicator_get_label) and
    TryLoad('app_indicator_get_label_guide', @app_indicator_get_label_guide) and
    TryLoad('app_indicator_get_ordering_index', @app_indicator_get_ordering_index);
  Initialized := Result;
end;

end.

