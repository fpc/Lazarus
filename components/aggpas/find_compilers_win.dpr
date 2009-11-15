{target:win}
//
// AggPas 2.4 RM3 Demo application
// Milan Marusinec alias Milano (c) 2006 - 2008
// Note: Press F1 key on run to see more info about this demo
//
program
 find_compilers_win ;

uses
 SysUtils ,Windows ,

 agg_basics ,
 agg_platform_support ,

 agg_color ,
 agg_pixfmt ,
 agg_pixfmt_rgb ,

 agg_ctrl ,
 agg_cbox_ctrl ,
 agg_rbox_ctrl ,

 agg_rendering_buffer ,
 agg_renderer_base ,
 agg_renderer_scanline ,
 agg_rasterizer_scanline_aa ,
 agg_scanline ,
 agg_scanline_u ,
 agg_render_scanlines ,

 agg_gsv_text ,
 agg_conv_stroke ,
 file_utils_ ;

{$I agg_mode.inc }
{$I- }
type
 src_key = record
   key ,
   val : string[99 ];

  end;

const
 flip_y = true;

 g_appl = 'AggPas';
 g_full = 'AggPas 2.4 RM3 vector graphics library';

 g_agg_paths = 'src;src\ctrl;src\platform\win;src\util;src\svg;gpc;expat-wrap';
 g_inc_paths = 'src';
 g_out_paths = '_debug';

 g_delphi_config = '-CG -B -H- -W-';
 g_fpc_config    = '-Mdelphi -Twin32 -WG -Sg -Se3 -CX -XX -Xs -B -Op3 -v0i';

 g_max       = 20;
 g_max_demos = 100;

 key_max  = 99;

var
 g_lock  ,g_image : boolean;

 g_found ,g_num_demos : unsigned;

 g_search_results : array[0..g_max - 1 ] of shortstring;

 g_demos : array[0..g_max_demos - 1 ] of string[99 ];

 key_array : array[0..key_max - 1 ] of src_key;
 key_count ,
 key_lastx : unsigned;
 key_scanx : shortstring;

type
 the_application_ptr = ^the_application;

 dialog_ptr = ^dialog;

 func_action = function(appl : the_application_ptr; sender : dialog_ptr ) : boolean;

 user_action_ptr = ^user_action;
 user_action = record
   func : func_action;
   ctrl : rbox_ctrl;

  end;

 user_choice = record
   ctrl : cbox_ctrl;
   attr : shortstring;

  end;

 dlg_status_e = (ds_none ,ds_define ,ds_ready ,ds_waiting_input ,ds_running );

 dialog = object
   m_appl : the_application_ptr;
   m_info : PChar;
   m_text : char_ptr;
   m_tx_x ,
   m_tx_y : double;
   m_aloc ,
   m_size : unsigned;
   m_clri ,
   m_clrt : aggclr;

   m_status : dlg_status_e;

   m_actions : array[0..4 ] of user_action;
   m_choices : array[0..25 ] of user_choice;

   m_num_actions ,
   m_num_choices : unsigned;

   m_cur_action : user_action_ptr;

   m_waiting : func_action;

   constructor Construct(appl : the_application_ptr; info : PChar; clr : aggclr_ptr = NIL );
   destructor  Destruct;

   procedure set_waiting(act : func_action );

   procedure add_action(name : PChar; act : func_action; x1 ,y1 ,x2 ,y2 : double );
   procedure add_choice(name ,attr : PChar; x ,y : double; status : boolean = false );

   procedure change_text(text : PChar; x ,y : double; clr : aggclr_ptr = NIL );
   procedure append_text(text : PChar );

   function  add_controls : boolean;
   procedure set_next_status(status : dlg_status_e = ds_none );

   function  find_cur_action : boolean;
   function  call_cur_action : boolean;
   procedure call_waiting;

  end;

 the_application = object(platform_support )
   m_dlg_welcome    ,
   m_dlg_set_drives ,
   m_dlg_searching  ,
   m_dlg_not_found  ,
   m_dlg_found_some : dialog;

   m_cur_dlg : dialog_ptr;

   m_ras : rasterizer_scanline_aa;
   m_sl  : scanline_u8;

   m_Thread : THandle;
   m_ApplID : LongWord;
   m_DoQuit : boolean;
   m_ShLast ,
   m_DoShow : shortstring;

   constructor Construct(format_ : pix_format_e; flip_y_ : boolean );
   destructor  Destruct;

   procedure draw_text(x ,y : double; msg : PChar; clr : aggclr_ptr = NIL );

   procedure on_init; virtual;
   procedure on_draw; virtual;

   procedure on_ctrl_change; virtual;
   procedure on_idle; virtual;

   procedure on_key(x ,y : int; key ,flags : unsigned ); virtual;

  end;

{ NEXTKEY }
function NextKey(var val : shortstring ) : boolean;
begin
 result:=false;

 while key_lastx < key_count do
  begin
   inc(key_lastx );

   if cmp_str(key_array[key_lastx - 1 ].key ) = key_scanx then
    begin
     val:=key_array[key_lastx - 1 ].val;

     result:=true;

     break;

    end;

  end;

end;

{ FIRSTKEY }
function FirstKey(key : shortstring; var val : shortstring ) : boolean;
begin
 key_lastx:=0;
 key_scanx:=cmp_str(key );

 result:=NextKey(val );

end;

{ LOADKEYS }
procedure LoadKeys(buff : char_ptr; size : int );
type
 e_scan = (expect_lp ,load_key ,load_val ,next_ln ,expect_crlf );

var
 scan : e_scan;
 key  ,
 val  : shortstring;

procedure add_key;
begin
 if key_count < key_max then
  begin
   key_array[key_count ].key:=key;
   key_array[key_count ].val:=val;

   inc(key_count );

  end;

 key:='';
 val:='';

end;

begin
 key_count:=0;

 scan:=expect_lp;
 key :='';
 val :='';

 while size > 0 do
  begin
   case scan of
    expect_lp :
     case buff^ of
      '{' :
	   scan:=load_key;

      else
       break;

     end;

    load_key :
     case buff^ of
      #13 ,#10 :
       break;

      ':' :
       scan:=load_val;

      '}' :
       begin
        add_key;

    	scan:=next_ln;

       end;

      else
       key:=key + buff^;

     end;

    load_val :
     case buff^ of
      #13 ,#10 :
       break;

      '}' :
       begin
        add_key;

     	scan:=next_ln;

       end;

      else
       val:=val + buff^;

     end;

    next_ln :
     case buff^ of
      #13 ,#10 :
       scan:=expect_crlf;

      ' ' :
      else
       break;

     end;

    expect_crlf :
     case buff^ of
      '{' :
       scan:=load_key;

      #13 ,#10 :
      else
       break;

     end;

   end;

   dec(size );
   inc(ptrcomp(buff ) );

  end;

end;

{ CONSTRUCT }
constructor dialog.Construct;
begin
 m_clri.ConstrDbl(0 ,0 ,0 );
 m_clrt.ConstrDbl(0 ,0 ,0 );

 m_appl:=appl;
 m_info:=info;
 m_text:=NIL;
 m_tx_x:=0;
 m_tx_y:=0;
 m_aloc:=0;
 m_size:=0;

 if clr <> NIL then
  m_clri:=clr^;

 m_status:=ds_define;

 m_num_actions:=0;
 m_num_choices:=0;

 m_cur_action:=NIL;
 m_waiting   :=NIL;

end;

{ DESTRUCT }
destructor dialog.Destruct;
var
 i : unsigned;

begin
 if m_text <> NIL then
  agg_freemem(pointer(m_text ) ,m_aloc );

 if m_num_actions > 0 then
  for i:=0 to m_num_actions - 1 do
   m_actions[i ].ctrl.Destruct;

 if m_num_choices > 0 then
  for i:=0 to m_num_choices - 1 do
   m_choices[i ].ctrl.Destruct;

end;

{ SET_WAITING }
procedure dialog.set_waiting;
begin
 m_waiting:=@act;

end;

{ ADD_ACTION }
procedure dialog.add_action;
begin
 case m_status of
  ds_define ,ds_ready :
   if m_num_actions < 5 then
    begin
     m_actions[m_num_actions ].ctrl.Construct(x1 ,y1 ,x2 ,y2 ,not flip_y );
     m_actions[m_num_actions ].ctrl.add_item (name );

     m_actions[m_num_actions ].func:=@act;

     inc(m_num_actions );

     set_next_status(ds_ready );

    end;

 end;

end;

{ ADD_CHOICE }
procedure dialog.add_choice;
begin
 case m_status of
  ds_define ,ds_ready :
   if m_num_choices < 26 then
    begin
     m_choices[m_num_choices ].ctrl.Construct(x ,y ,name ,not flip_y );
     m_choices[m_num_choices ].ctrl.status_  (status );

     m_choices[m_num_choices ].attr:=StrPas(attr ) + #0;

     inc(m_num_choices );

    end;

 end;

end;

{ CHANGE_TEXT }
procedure dialog.change_text;
begin
 if StrLen(text ) + 1 > m_aloc then
  begin
   agg_freemem(pointer(m_text ) ,m_aloc );

   m_aloc:=StrLen(text ) + 1;

   agg_getmem(pointer(m_text ) ,m_aloc );

  end;

 move(text[0 ] ,m_text^ ,StrLen(text ) + 1 );

 m_size:=StrLen(text );
 m_tx_x:=x;
 m_tx_y:=y;

 if clr <> NIL then
  m_clrt:=clr^;

end;

{ APPEND_TEXT }
procedure dialog.append_text;
var
 new_text : char_ptr;
 new_aloc : unsigned;

begin
 if StrLen(text ) + m_size + 1 > m_aloc then
  begin
   new_aloc:=StrLen(text ) + m_size + 1;

   agg_getmem(pointer(new_text ) ,new_aloc );

   move(m_text^ ,new_text^ ,m_size );

   agg_freemem(pointer(m_text ) ,m_aloc );

   m_aloc:=new_aloc;
   m_text:=new_text;

  end;

 move(text[0 ] ,char_ptr(ptrcomp(m_text ) + m_size )^ ,StrLen(text ) + 1 );

 inc(m_size ,StrLen(text ) );

end;

{ ADD_CONTROLS }
function dialog.add_controls;
var
 i : unsigned;

begin
 result:=false;

 case m_status of
  ds_ready :
   begin
    m_appl.m_ctrls.Destruct;
    m_appl.m_ctrls.Construct;

    if m_num_actions > 0 then
     for i:=0 to m_num_actions - 1 do
      m_appl.add_ctrl(@m_actions[i ].ctrl );

    if m_num_choices > 0 then
     for i:=0 to m_num_choices - 1 do
      m_appl.add_ctrl(@m_choices[i ] );

    set_next_status;

    result:=true;

   end;

 end;

end;

{ SET_NEXT_STATUS }
procedure dialog.set_next_status;
begin
 if status <> ds_none then
  m_status:=status
 else
  case m_status of
   ds_define :
    m_status:=ds_ready;

   ds_ready :
    m_status:=ds_waiting_input;

   ds_waiting_input :
    m_status:=ds_running;

  end;

end;

{ FIND_CUR_ACTION }
function dialog.find_cur_action;
var
 i : unsigned;

begin
 result:=false;

 case m_status of
  ds_waiting_input :
   if m_num_actions > 0 then
    for i:=0 to m_num_actions - 1 do
     if m_actions[i ].ctrl._cur_item = 0 then
      begin
       m_cur_action:=@m_actions[i ];

       result:=true;

       exit;

      end;

 end;

end;

{ CALL_CUR_ACTION }
// result of true means, that this was the last call
function dialog.call_cur_action;
begin
 result:=false;

 case m_status of
  ds_running :
   if m_cur_action <> NIL then
    result:=m_cur_action.func(m_appl ,@self );

 end;

end;

{ CALL_WAITING }
procedure dialog.call_waiting;
begin
 if @m_waiting <> NIL then
  m_waiting(m_appl ,@self );

end;

{ create_delphi }
procedure create_delphi(batch_file ,comp_path ,project : shortstring );
var
 command : AnsiString;

 suffix ,file_path ,file_name ,file_ext : shortstring;

 df : text;

begin
// Compose the units path string
 spread_name(comp_path ,file_path ,file_name ,file_ext );

 command:=dir_str(file_path );

 spread_name(command ,file_path ,suffix ,file_ext );

 suffix:=file_path + 'lib';

// Compose the command string
 command:='"' + comp_path + 'dcc32.exe" ';
 command:=command + '-U"' + suffix + '";';
 command:=command + g_agg_paths + ' ';
 command:=command + '-I' + g_inc_paths + ' ';
 command:=command + '-N' + g_out_paths + ' ';
 command:=command + g_delphi_config + ' ';
 command:=command + project;

// Create the file
 AssignFile(df ,batch_file );
 rewrite   (df );
 writeln   (df ,command );
 close     (df );

end;

{ create_fpc }
procedure create_fpc(batch_file ,comp_path ,project : shortstring );
var
 command : AnsiString;

 suffix ,file_path ,file_name ,file_ext : shortstring;

 df : text;

begin
// Compose the units path string
 spread_name(comp_path ,file_path ,file_name ,file_ext );

 command:=dir_str(file_path );

 spread_name(command ,file_path ,suffix ,file_ext );

 command:=dir_str(file_path );

 spread_name(command ,file_path ,file_name ,file_ext );

 suffix:=file_path + 'units\' + suffix;

// Compose the command string
 command:='"' + comp_path + 'ppc386.exe" ';
 command:=command + '-FD"' + suffix + '" ';
 command:=command + '-Fu'  + g_agg_paths + ' ';
 command:=command + '-Fi'  + g_inc_paths + ' ';
 command:=command +  '-FU'  + g_out_paths + ' ';
 command:=command + g_fpc_config + ' ';
 command:=command + project;

// Create the file
 AssignFile(df ,batch_file );
 rewrite   (df );
 writeln   (df ,command );
 close     (df );

end;

{ create_batch_files }
procedure create_batch_files(project : shortstring; var del ,fpc : unsigned );
var
 i ,del_cnt ,fpc_cnt : unsigned;

 batch ,batch_path ,comp_path ,file_path ,comp_name ,file_name ,file_ext : shortstring;

 df : text;

begin
 spread_name(ParamStr(0 ) ,batch_path ,file_name ,file_ext );

 del_cnt:=1;
 fpc_cnt:=1;

 for i:=0 to g_found - 1 do
  begin
   spread_name(g_search_results[i ] ,comp_path ,comp_name ,file_ext );
   spread_name(project ,file_path ,file_name ,file_ext );

   if cmp_str(comp_name ) = cmp_str('dcc32' ) then
    begin
    // Make batch for Delphi
     if del_cnt = 1 then
      batch:=''
     else
      str(del_cnt ,batch );

     batch:='delphi' + batch + '-' + file_name;
     batch:=fold_name(batch_path ,batch ,'*.bat' );

     create_delphi(batch ,comp_path ,project );

    // Make file
     if del_cnt = 1 then
      file_ext:=''
     else
      str(del_cnt ,file_ext );

     file_ext :='delphi' + file_ext + '_make_all';
     file_name:=fold_name(batch_path ,file_ext ,'*.bat' );

     AssignFile(df ,file_name );

     if del = 0 then
      rewrite(df )
     else
      append(df );

     file_ext:='call "' + batch + '"';

     writeln(df ,file_ext );
     close  (df );

     inc(del_cnt );

    end
   else
    begin
    // Make batch for FreePascal
     if fpc_cnt = 1 then
      batch:=''
     else
      str(fpc_cnt ,batch );

     batch:='fpc' + batch + '-' + file_name;
     batch:=fold_name(batch_path ,batch ,'*.bat' );

     create_fpc(batch ,comp_path ,project );

    // Make file
     if fpc_cnt = 1 then
      file_ext:=''
     else
      str(fpc_cnt ,file_ext );

     file_ext :='fpc' + file_ext + '_make_all';
     file_name:=fold_name(batch_path ,file_ext ,'*.bat' );

     AssignFile(df ,file_name );

     if fpc = 0 then
      rewrite(df )
     else
      append(df );

     file_ext:='call "' + batch + '"';

     writeln(df ,file_ext );
     close  (df );

     inc(fpc_cnt );

    end;

  end;

 inc(del ,del_cnt - 1 );
 inc(fpc ,fpc_cnt - 1 );

end;

{ action_configure }
function action_configure(appl : the_application_ptr; sender : dialog_ptr ) : boolean;
var
 i : unsigned;

 text : shortstring;
 rgba : aggclr;

 del ,fpc : unsigned;

begin
 rgba.ConstrDbl(0 ,0.5 ,0 );

 appl.m_dlg_searching.change_text('Creating appropriate batch files ...' ,10 ,320 ,@rgba );
 appl.force_redraw;

// Setup the final text
 rgba.ConstrDbl(0 ,0.5 ,0 );

 appl.m_dlg_found_some.change_text('' ,10 ,385 ,@rgba );

 for i:=0 to g_found - 1 do
  begin
   str(i + 1 ,text );

   text:='(' + text + ')  ' + g_search_results[i ] + #13#0;

   appl.m_dlg_found_some.append_text(@text[1 ] );

  end;

// Create the batch files
 if g_num_demos > 0 then
  begin
   appl.m_dlg_found_some.append_text(
    #13 +
    'Appropriate batch files for compiling the ' + g_appl + ' demos were created'#13 +
    'in the directory, from which this helper utility was run.' );

   del:=0;
   fpc:=0;

   for i:=0 to g_num_demos - 1 do
    create_batch_files(g_demos[i ] ,del ,fpc );

   if del > 0 then
    appl.m_dlg_found_some.append_text(
     #13#13 +
     'Note: For the Delphi compiler, which was found on your system,'#13 +
     'helper utility assumes, that the system libraries needed for'#13 +
     'successful compilation are located in the parallel directory'#13 +
     '"..\lib" of the particular Delphi compiler path.' );

   if fpc > 0 then
    appl.m_dlg_found_some.append_text(
     #13#13 +
     'Note: For the Free Pascal compiler, which was found on your system,'#13 +
     'helper utility assumes, that the system libraries needed for'#13 +
     'successful compilation are located in the parallel directory'#13 +
     '"..\units\i386-win32" of the particular Free Pascal compiler path.' );

  end
 else
  appl.m_dlg_found_some.append_text(
   #13 +
   'NO batch files for compiling the ' + g_appl + ' demos'#13 +
   'were created in the directory, from which this helper'#13 +
   'utility was run, because no *.dpr projects were found.' );

// Refresh
 appl.force_redraw;

end;

{ action_set_drives }
function action_set_drives(appl : the_application_ptr; sender : dialog_ptr ) : boolean;
var
 letter ,
 path   ,
 drive  : shortstring;

 drive_type ,i ,count : unsigned;

begin
// Scan for drives in the system
 letter:='C';
 count :=0;

 for i:=1 to 24 do
  begin
   path :=letter + ':\'#0;
   drive:='';

   drive_type:=GetDriveType(@path[1 ] );

   case drive_type of
    DRIVE_FIXED     : drive:='fixed harddrive';
    DRIVE_REMOVABLE : drive:='removable drive';
    DRIVE_REMOTE    : drive:='network or remote drive';
    DRIVE_CDROM     : drive:='CD-ROM drive';
    DRIVE_RAMDISK   : drive:='RAM disk';

   end;

   if drive <> '' then
    begin
     drive:='  ' + StrPas(@path[1 ] ) + ' (' + drive + ')' + #0;

     appl.m_dlg_set_drives.add_choice(@drive[1 ] ,@path[1 ] ,30 ,360 - count * 30 ,count = 0 );

     inc(count );

    end;

   inc(byte(letter[1 ] ) );

  end;

 appl.m_cur_dlg:=@appl.m_dlg_set_drives;

// OK Done
 result:=true;

end;

{ action_while_search }
function action_while_search(appl : the_application_ptr; sender : dialog_ptr ) : boolean;
var
 text : shortstring;
 rgba : aggclr;

begin
 while g_lock do;

 g_lock:=true;

 if appl.m_ShLast <> appl.m_DoShow then
  begin
   str(g_found ,text );

   text:=
    '  ' + appl.m_DoShow + #13#13 +
    'Compilers found: ' + text + #0;

  //rgba.ConstrDbl(0 ,0 ,0.5 );

   appl.m_dlg_searching.change_text(@text[1 ] ,10 ,320 );
   appl.force_redraw;

   appl.m_ShLast:=appl.m_DoShow;

  end;

 g_lock:=false;

end;

{ process_file }
function process_file(file_name : shortstring ) : boolean;
begin
 if g_found < g_max then
  begin
   g_search_results[g_found ]:=file_name;

   inc(g_found );

  end;

end;

{ scan_files }
function scan_files(files : shortstring; appl : the_application_ptr ) : boolean;
var
 SR  : TSearchRec;
 err : integer;

 find ,file_path ,file_name ,file_ext : shortstring;

begin
 result:=false;

{ Scan dirs and go further }
 spread_name(files ,file_path ,file_name ,file_ext );

 while g_lock do;

 g_lock:=true;

 appl.m_DoShow:=file_path;

 g_lock:=false;

 err:=SysUtils.FindFirst(str_dir(file_path ) + '*' ,faDirectory ,SR );

 while err = 0 do
  begin
   if appl.m_DoQuit then
    begin
     SysUtils.FindClose(SR );

     exit;

    end;

   if (SR.Name <> '.' ) and
      (SR.Name <> '..' ) and
      (SR.Attr and faDirectory = faDirectory ) then
    begin
     spread_name(files ,file_path ,file_name ,file_ext );

     if not scan_files(fold_name(str_dir(file_path ) + SR.Name + '\' ,file_name ,file_ext ) ,appl ) then
      exit;

    end;

   err:=SysUtils.FindNext(SR );

  end;

 SysUtils.FindClose(SR );

{ Scan files for Delphi compiler }
 find:=fold_name(file_path ,'dcc32' ,'*.exe' );

 err:=SysUtils.FindFirst(find ,faArchive ,SR );

 while err = 0 do
  begin
   if appl.m_DoQuit then
    begin
     SysUtils.FindClose(SR );

     exit;

    end;

   process_file(fold_name(files ,SR.Name ,SR.Name ) );

   err:=SysUtils.FindNext(SR );

  end;

 SysUtils.FindClose(SR );

{ Scan files for FPC compiler }
 find:=fold_name(file_path ,'ppc386' ,'*.exe' );

 err:=SysUtils.FindFirst(find ,faArchive ,SR );

 while err = 0 do
  begin
   if appl.m_DoQuit then
    begin
     SysUtils.FindClose(SR );

     exit;

    end;

   process_file(fold_name(files ,SR.Name ,SR.Name ) );

   err:=SysUtils.FindNext(SR );

  end;

 SysUtils.FindClose(SR );

{ OK }
 scan_files:=true;

end;

{ FnSearch }
procedure FnSearch(appl : the_application_ptr );
var
 i : unsigned;

begin
 appl.m_ShLast:='';
 appl.m_DoShow:='';

 g_found:=0;

// OK, Go through selected drives and issue search
 appl.m_dlg_searching.set_waiting(@action_while_search );

 if appl.m_dlg_set_drives.m_num_choices > 0 then
  for i:=0 to appl.m_dlg_set_drives.m_num_choices - 1 do
   if appl.m_dlg_set_drives.m_choices[i ].ctrl._status then
    if not scan_files(appl.m_dlg_set_drives.m_choices[i ].attr ,appl ) then
     break;

 appl.m_dlg_searching.set_waiting(NIL );

// Were we forced to quit ?
 if appl.m_DoQuit then
  NoP;

// Depending on the search result activate the next user dialog
 if g_found > 0 then
  begin
   action_configure(appl ,NIL );

   appl.m_cur_dlg:=@appl.m_dlg_found_some;

  end
 else
  appl.m_cur_dlg:=@appl.m_dlg_not_found;

end;

{ ThSearch }
function ThSearch(Parameter : pointer ): integer;
begin
{ Synchronize }
 while the_application_ptr(Parameter ).m_Thread = 0 do;

{ Call Thread }
 FnSearch(Parameter );

{ Exit }
 the_application_ptr(Parameter ).m_Thread:=0;
 the_application_ptr(Parameter ).m_ApplID:=0;

{ Done }
 EndThread(0 );

end;

{ action_begin_search }
function action_begin_search(appl : the_application_ptr; sender : dialog_ptr ) : boolean;
var
 i : unsigned;

begin
 result:=false;

// Check, if we have drives to search
 if appl.m_dlg_set_drives.m_num_choices > 0 then
  for i:=0 to appl.m_dlg_set_drives.m_num_choices - 1 do
   if appl.m_dlg_set_drives.m_choices[i ].ctrl._status then
    begin
     result:=true;

     break;

    end;

 if not result then
  begin
   appl.m_dlg_set_drives.m_actions[0 ].ctrl.cur_item_(-1 );
   appl.m_dlg_set_drives.set_next_status(ds_waiting_input );
   appl.force_redraw;

   exit;

  end;

// Go on to search dialog
 appl.m_cur_dlg:=@appl.m_dlg_searching;

// Start Up the search thread
 appl.m_Thread:=BeginThread(NIL ,65536 ,ThSearch ,appl ,0 ,appl.m_ApplID );

end;

{ action_stop_search }
function action_stop_search(appl : the_application_ptr; sender : dialog_ptr ) : boolean;
begin
 appl.m_DoQuit:=true;

end;

{ action_exit }
function action_exit(appl : the_application_ptr; sender : dialog_ptr ) : boolean;
begin
 appl.quit;

end;

{ CONSTRUCT }
constructor the_application.Construct;
var
 rgba : aggclr;

begin
 inherited Construct(format_ ,flip_y_ );

 m_sl.Construct;
 m_ras.Construct;

 m_cur_dlg:=NIL;

 m_Thread:=0;
 m_ApplID:=0;
 m_DoQuit:=false;
 m_ShLast:='';
 m_DoShow:='';

// Welcome dialog
 m_dlg_welcome.Construct(
  @self ,
  'Welcome to the ' + g_full + '.'#13 +
  ''#13 +
  'This helper utility will scan your system to search'#13 +
  'for all available Object Pascal compilers.'#13 +
  ''#13 +
  'It will also create appropriate batch files with current'#13 +
  'paths and options needed to compile properly all'#13 +
  'the ' + g_appl + ' demos.'#13+
  ''#13 +
  'Currently Delphi and Free Pascal compilers are supported.' );

 m_dlg_welcome.add_action('Continue' ,@action_set_drives ,480 ,15 ,580 ,45 );

// Set drives to search on dialog
 m_dlg_set_drives.Construct(
  @self ,
  'Please select, on which drives of your system should'#13 +
  'this helper utility perform search for Object Pascal compilers:' );

 m_dlg_set_drives.add_action('Continue' ,@action_begin_search ,480 ,15 ,580 ,45 );

// Wait, searching dialog
 m_dlg_searching.Construct(
  @self ,
  'Please wait ...'#13 +
  ''#13 +
  'Helper utility is searching for Object Pascal compilers'#13 +
  'on the drives, you have selected.' );

 m_dlg_searching.add_action('Stop searching' ,@action_stop_search ,440 ,15 ,580 ,45 );

// Found nothing dialog
 rgba.ConstrInt(255 ,0 ,0 );

 m_dlg_not_found.Construct(
  @self ,
  'I am sorry, but NO Object Pascal compilers were found'#13 +
  'on your system.'#13 +
  ''#13 +
  'Please install Delphi or FreePascal'#13+
  'and then rerun this utility.'#13#13+
  'http://www.borland.com'#13#13 +
  '- or - '#13#13 +
  'http://www.freepascal.org' ,
  @rgba );

 m_dlg_not_found.add_action('Exit' ,@action_exit ,500 ,15 ,580 ,45 );

// Compilers found dialog
 rgba.ConstrDbl(0 ,0.5 ,0 );

 m_dlg_found_some.Construct(
  @self ,
  'Following Object Pascal compilers were found your system:' ,
  @rgba );

 m_dlg_found_some.add_action('Exit' ,@action_exit ,500 ,15 ,580 ,45 );

end;

{ DESTRUCT }
destructor the_application.Destruct;
begin
 while m_Thread <> 0 do
  m_DoQuit:=true;

 inherited Destruct;

 m_sl.Destruct;
 m_ras.Destruct;

 m_dlg_welcome.Destruct;
 m_dlg_set_drives.Destruct;
 m_dlg_searching.Destruct;
 m_dlg_not_found.Destruct;
 m_dlg_found_some.Destruct;

end;

{ DRAW_TEXT }
procedure the_application.draw_text;
var
 pixf : pixel_formats;
 rgba : aggclr;

 rb : renderer_base;
 rs : renderer_scanline_aa_solid;

 t  : gsv_text;
 pt : conv_stroke;

begin
 pixfmt_bgr24(pixf ,rbuf_window );

 rb.Construct(@pixf );
 rs.Construct(@rb );

 t.Construct;
 t.size_      (9.5 );
 t.line_space_(10 );

 pt.Construct(@t );
 pt.width_   (1.2 );

 t.start_point_(x ,y );
 t.text_       (msg );

 if clr <> NIL then
  rs.color_(clr )
 else
  begin
   rgba.ConstrDbl(0 ,0 ,0 );
   rs.color_     (@rgba );

  end;

 m_ras.add_path  (@pt );
 render_scanlines(@m_ras ,@m_sl ,@rs );

 t.Destruct;
 pt.Destruct;

end;

{ ON_INIT }
procedure the_application.on_init;
var
 SR  : TSearchRec;
 err : integer;

 find ,file_path ,file_name ,file_ext : shortstring;

 cf : file;
 bf : pointer;
 sz : integer;

 target ,get : shortstring;

begin
 wait_mode_(false );

// Load the list of current projects
 g_num_demos:=0;

 spread_name(ParamStr(0 ) ,file_path ,file_name ,file_ext );

 find:=fold_name(file_path ,'*' ,'*.dpr' );
 err :=SysUtils.FindFirst(find ,faArchive ,SR );

 while err = 0 do
  begin
  // Load keys from the source file
   key_count:=0;

   get:=fold_name(file_path ,SR.Name ,SR.Name );

   AssignFile(cf ,SR.Name );
   reset     (cf ,1 );

   if IOResult = 0 then
    begin
     sz:=System.FileSize(cf );

     if agg_getmem(bf ,sz ) then
      begin
       blockread  (cf ,bf^ ,sz );
       LoadKeys   (bf ,sz );
       agg_freemem(bf ,sz );

      end;

     close(cf );

    end;

   target:='win';

   FirstKey('target' ,target ); 

  // Add To List
   if (cmp_str(target ) <> cmp_str('win' ) ) or
      FirstKey('skip' ,get ) then

   else
    if g_num_demos < g_max_demos then
     begin
      g_demos[g_num_demos ]:=fold_name('' ,SR.Name ,SR.Name );

      inc(g_num_demos );

     end;

   err:=SysUtils.FindNext(SR );

  end;

 SysUtils.FindClose(SR );

end;

{ ON_DRAW }
procedure the_application.on_draw;
var
 pixf : pixel_formats;
 rgba : aggclr;

 rb : renderer_base;
 rs : renderer_scanline_aa_solid;

 i ,plus : unsigned;

begin
// Initialize structures
 pixfmt_bgr24(pixf ,rbuf_window );

 rb.Construct(@pixf );
 rs.Construct(@rb );

 rgba.ConstrDbl(1 ,1 ,1 );
 rb.clear      (@rgba );

// Render Dialog
 if m_cur_dlg <> NIL then
  case m_cur_dlg.m_status of
   ds_waiting_input ,ds_running :
    begin
    // Render logo if has one
     plus:=0;

     if (m_cur_dlg = @m_dlg_welcome ) and
        g_image then
      begin
       rb.copy_from(rbuf_img(1 ) ,NIL ,6 ,330 );

       plus:=rbuf_img(1 )._height + 20;

      end;

    // Render base text
     draw_text(10 ,420 - plus ,m_cur_dlg.m_info ,@m_cur_dlg.m_clri );

    // Render dynamic text
     if m_cur_dlg.m_text <> NIL then
      draw_text(
       m_cur_dlg.m_tx_x ,
       m_cur_dlg.m_tx_y ,
       PChar(m_cur_dlg.m_text ) ,
       @m_cur_dlg.m_clrt );

    // Render choices
     if m_cur_dlg.m_num_choices > 0 then
      for i:=0 to m_cur_dlg.m_num_choices - 1 do
       render_ctrl(@m_ras ,@m_sl ,@rs ,@m_cur_dlg.m_choices[i ] );

    // Render actions
     if m_cur_dlg.m_num_actions > 0 then
      for i:=0 to m_cur_dlg.m_num_actions - 1 do
       render_ctrl(@m_ras ,@m_sl ,@rs ,@m_cur_dlg.m_actions[i ].ctrl );

    end;

  end;

end;

{ ON_CTRL_CHANGE }
procedure the_application.on_ctrl_change;
begin
 if m_cur_dlg <> NIL then
  case m_cur_dlg.m_status of
   ds_waiting_input :
    if m_cur_dlg.find_cur_action then
     m_cur_dlg.set_next_status;

  end;

end;

{ ON_IDLE }
procedure the_application.on_idle;
begin
 if m_cur_dlg = NIL then
  begin
   m_cur_dlg:=@m_dlg_welcome;

   if m_cur_dlg.m_status <> ds_ready then
    m_cur_dlg:=NIL;

  end
 else
  case m_cur_dlg.m_status of
   ds_ready :
    if m_cur_dlg.add_controls then
     force_redraw;

   ds_waiting_input :
    m_cur_dlg.call_waiting;

   ds_running :
    if m_cur_dlg.call_cur_action then
     NoP;

  end;

end;

{ ON_KEY }
procedure the_application.on_key;
begin
 if key = key_f1 then
  message_(
   'This is just an AggPas library helper utility which has nothing to do'#13 +
   'with demonstrating any of graphical possibilities of AGG.'#13#13 +
   'Author of this pascal port (Milano) recomends to proceed with this utility'#13 +
   'on your system right after unpacking the archive, because it will'#13 +
   'scan your computer for all available Object Pascal compilers and'#13 +
   'it will create the up-to-date working batch files for fompiling the library demos.'#13#13 +
   'In the welcome screen of this utility, there is a logo for the AGG library,'#13 +
   'which was designed and proposed by Milano. It has the meaning of spiral primitive'#13 +
   'upon the interactive polygon control, which should mean in "translation" that'#13 +
   '"With AGG the possibilities are endless (the spiral) and custom adjustments'#13 +
   'are easy possible. (interactive polygon)".' +
   #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ' );

end;

VAR
 app : the_application;

BEGIN
 g_lock :=false;
 g_image:=false;

 app.Construct(pix_format_bgr24 ,flip_y );
 app.caption_ (g_appl + ' Startup utility (F1-Help)' );

 if app.load_img(1 ,'aggpas_logo' ) then
  g_image:=true;

 if app.init(600 ,450 ,0 ) then
  app.run;

 app.Destruct;

END.