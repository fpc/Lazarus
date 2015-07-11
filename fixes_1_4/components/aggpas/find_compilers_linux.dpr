{target:linux}
{linux_console_app}
//
// AggPas 2.4 RM3 Helper utility application
// Milan Marusinec alias Milano (c) 2006 - 2008
//
program
 find_compilers_linux ;

uses
 SysUtils ,
 agg_basics ,
 file_utils_ ,
 libc ;

{$I agg_mode.inc }
{$- }
type
 src_key = record
   key ,
   val : string[99 ];

  end;

const
 key_max  = 99; 
 pool_max = 65536;
 make_max = 99;

 fpc_comp = 'ppc386';
 fpc_libs = '-Fu"src;src/ctrl;src/platform/linux;src/util;src/svg;expat-wrap"';
 fpc_incl = '-Fisrc';
 fpc_outd = '-FU_debug';
 fpc_conf = '-Mdelphi -Tlinux -Sg -Se3 -XX -Xs -B -v0i';
 fpc_gapp = '-WG';
 fpc_capp = '-WC';

var
 key_array : array[0..key_max - 1 ] of src_key;
 key_count ,
 key_lastx : unsigned;
 key_scanx : shortstring;

 pool_buff : pointer;
 pool_aloc ,
 pool_size : unsigned;

 make_array : array[0..make_max - 1 ] of string[99 ];
 make_count : unsigned;

{ WRPOOL }
procedure WrPool(str : shortstring; crlf : boolean = false );
begin
 if crlf then
  str:=str + #10;

 if pool_size + length(str ) < pool_aloc then
  begin
   System.move(
    str[1 ] ,
    pointer(ptrcomp(pool_buff ) + pool_size )^ ,
    length(str ) );

   inc(pool_size ,length(str ) );	

  end;

end;

{ WRFILE }
function WrFile(fname : shortstring ) : boolean;
var
 df : file;
 wr : int;

begin
 result:=false;

 AssignFile(df ,fname );
 rewrite   (df ,1 );

 if IOResult = 0 then
  begin
   blockwrite(df ,pool_buff^ ,pool_size ,wr );
   close     (df );

   fname:=fname + #0;

   libc.chmod(
    PChar(@fname[1 ] ) ,
    S_IRWXU or S_IRWXG or S_IROTH or S_IWOTH );

   if pool_size = wr then
    result:=true;

  end;

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
     val   :=key_array[key_lastx - 1 ].val;
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

{ WRITECOMPILESCRIPT }
function WriteCompileScript(name ,ext : shortstring ) : boolean;
var
 cp : shortstring;

begin
 result:=false;

// Create the script in memory
 pool_size:=0;

 WrPool(fpc_comp + ' ' );
 WrPool(fpc_libs + ' ' );
 WrPool(fpc_incl + ' ' );
 WrPool(fpc_outd + ' ' );
 WrPool(fpc_conf + ' ' );

 if FirstKey('linux_console_app' ,cp ) then
  WrPool(fpc_capp + ' ' )
 else
  WrPool(fpc_gapp + ' ' );

 WrPool(name + ext ,true );


// WriteFile
 name:='compile-' + name;

 if WrFile(name ) then
  begin
   if make_count < make_max then
    begin
     make_array[make_count ]:=name;	

     inc(make_count );

    end; 

   result:=true; 

  end; 

end;

{ CREATECOMPILESCRIPT }
procedure CreateCompileScript(name ,ext : shortstring );
var
 loaded : boolean;

 target ,value : shortstring;

 lf : file;
 fs ,
 ls : int;
 bf : pointer;

begin
 write(' ' ,name ,ext ,' ... ' );

// Open Source .DPR file
 AssignFile(lf ,name + ext );
 reset     (lf ,1 );

 if IOResult = 0 then
  begin
   loaded:=false;

  // Load DPR keys
   fs:=filesize(lf );

   if (fs > 0 ) and
      agg_getmem(bf ,fs ) then
    begin
     blockread(lf ,bf^ ,fs ,ls );

     if fs = ls then
      begin
       loaded:=true;

       LoadKeys(bf ,fs );

      end;

     agg_freemem(bf ,fs );

    end;

  // Close DPR
   close(lf );

  // Create compilation script
   if loaded then
    begin
     if FirstKey('skip' ,value ) then
      writeln('to be not included -> skipped' )
     else
      begin
       target:='linux';

       FirstKey('target' ,target );

       if cmp_str(target ) = cmp_str('linux' ) then
        if WriteCompileScript(name ,ext ) then
         writeln('OK' )
        else
         writeln('Failed to generate compile script !' )
       else
        writeln('different target (' ,target ,') -> skipped' );

      end;

    end
   else
    writeln('Failed to read the source file !' );

  end
 else
  writeln('Failed to open !' ); 

end;

{ PROCESSOBJECT }
procedure ProcessObject(found : shortstring );
var
 file_path ,file_name ,file_ext : shortstring;

begin
 spread_name(found ,file_path ,file_name ,file_ext );

 if cmp_str(file_ext ) = cmp_str('.dpr' ) then
  CreateCompileScript(file_name ,file_ext );

end;

{ ITERATEFOLDER }
procedure IterateFolder(inFolder : shortstring );
var
 dp : libc.PDIR;
 ep : libc.Pdirent;

begin
 inFolder:=inFolder + #0;

 dp:=libc.opendir(PChar(@inFolder[1 ] ) );

 if dp <> NIL then
  begin
   repeat
    ep:=libc.readdir(dp );

    if ep <> NIL then
     ProcessObject(strpas(ep.d_name ) );

   until ep = NIL;

   libc.closedir(dp );

  end;

end;

{ CREATEMAKEFILE }
procedure CreateMakeFile;
var
 i : unsigned;

begin
 pool_size:=0;

 i:=0;

 while i < make_count do
  begin
   WrPool('./' + make_array[i ] ,true ); 

   inc(i );

  end;

 WrFile('compile_make_all' ); 

end;

{ SCANDEMOS }
procedure ScanDemos;
begin
 IterateFolder('./' );
 writeln;

 if make_count > 0 then
  begin
   CreateMakeFile;

   writeln('SUCCESS: FPC compilation script files were created' );
   writeln('         for the AggPas demos listed above.' );
   writeln;
   writeln('         To compile the demos, run Terminal, change to the current' );
   writeln('         directory and type "./compile_make_all"' );
   writeln('         or "./compile-xxx", where "xxx" is the name of the demo.' );

  end
 else
  writeln('MESSAGE: No AggPas demo files were found in current folder !' );

 writeln;

end;

BEGIN
 writeln;
 writeln('*************************************************************' );
 writeln('* Welcome to the AggPas 2.4 RM3 vector graphics library.    *' );
 writeln('*************************************************************' ); 
 writeln('*                                                           *' );
 writeln('* This helper utility will generate the compilation script  *' );
 writeln('* files with current paths and options needed to compile    *' );
 writeln('* properly all the AggPas demos on your Linux station.      *' );
 writeln('*                                                           *' );
 writeln('* Currently the Free Pascal compiler is supported.          *' );
 writeln('* (www.freepascal.org)                                      *' ); 
 writeln('*                                                           *' );
 writeln('*************************************************************' );
 writeln;
 writeln('[Press ENTER key to continue ...]' );
 writeln; 
 readln;

 if agg_getmem(pool_buff ,pool_max ) then
  begin
   pool_aloc :=pool_max;
   pool_size :=0;
   make_count:=0;

   ScanDemos;

   agg_freemem(pool_buff ,pool_aloc );

  end
 else
  writeln('ERROR: Not enough memory for the pool buffer !' ); 

END.