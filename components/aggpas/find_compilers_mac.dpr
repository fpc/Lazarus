{target:mac}
{mac_console_app}
//
// AggPas 2.4 RM3 Helper utility application
// Milan Marusinec alias Milano (c) 2006 - 2008
//
program
 find_compilers_mac ;

uses
 SysUtils ,
 agg_basics ,
 file_utils_ ,
 Carbon ;
 
{$I agg_mode.inc }

type
 src_key = record
   key ,
   val : string[99 ];
 
  end;

const  
 key_max  = 99; 
 pool_max = 65536;
 make_max = 99;
 
 fpc_comp = '/usr/local/bin/ppcppc';
 fpc_libs = '-Fu"src;src/ctrl;src/platform/mac;src/util;src/svg;upi;expat-wrap"';
 fpc_incl = '-Fisrc';
 fpc_outd = '-FU_debug';
 fpc_frmw = '-k"-framework Carbon -framework QuickTime"';
 fpc_conf = '-Mdelphi -Tdarwin -Sg -Se3 -XX -Xs -B -v0i';
 fpc_capp = '-WC';
 fpc_gapp = '-WG';

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
function WrFile(destDir : FSRefPtr; name : shortstring ) : boolean;
var
 i : unsigned;

 ossState : OSStatus;
 ossError : OSErr;
 forkName ,
 destName : HFSUniStr255;
 dstFSRef : FSRef;
 forkRef  : SInt16;
 written  : ByteCount;
 catInfo  : FSCatalogInfo;
 
begin
 result:=false;
  
// Fill in Unicode name
 for i:=1 to length(name ) do
  destName.unicode[i - 1 ]:=byte(name[i ] );
  
 destName.length:=length(name );
 
// Write the script to file
 ossError:=FSCreateFileUnicode(destDir^ ,destName.length ,destName.unicode[0 ] ,kFSCatInfoNone ,NIL ,@dstFSRef ,NIL );

 if ossError = noErr then
  begin
   FSGetDataForkName(forkName ); 
 
   ossError:=FSOpenFork(dstFSRef ,forkName.length ,forkName.unicode[0 ] ,fsWrPerm ,forkRef );
  
   if ossError = noErr then
    begin
     ossError:=FSWriteFork(forkRef ,fsFromStart + noCacheBit ,0 ,pool_size ,pool_buff ,written );  

     FSCloseFork(forkRef );
    
     if (ossError = noErr ) and
        (pool_size = written ) then

     else
      exit;
 
    end
   else
    begin write('[FSOpenFork:' ,ossError ,'] ' ); exit; end;
  
  end
 else
  if ossError = dupFNErr then
  else
   begin write('[FSCreateFileUnicode:' ,ossError ,'] ' ); exit; end;
  
// Set The File permissions
 CatInfo.permissions[0 ]:=0;
 CatInfo.permissions[1 ]:=0;
 CatInfo.permissions[2 ]:=0;
 CatInfo.permissions[3 ]:=0;
 
 FSPermissionInfoPtr(@CatInfo.permissions ).mode:=999;

 ossError:=FSSetCatalogInfo(dstFSRef ,kFSCatInfoPermissions ,CatInfo );
 
// OK 
 result:=true; 
 
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
procedure LoadKeys(buff : char_ptr; size : SInt64 );
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
function WriteCompileScript(destDir : FSRefPtr; name ,ext : shortstring ) : boolean;
var
 cp ,fp ,fn ,fx : shortstring;
 
begin
 result:=false;
 
// Create the script in memory
 pool_size:=0;

 WrPool(fpc_comp + ' ' );
 WrPool(fpc_libs + ' ' );
 WrPool(fpc_incl + ' ' );
 WrPool(fpc_outd + ' ' );
 WrPool(fpc_frmw + ' ' );
 WrPool(fpc_conf + ' ' );

 if FirstKey('mac_console_app' ,cp ) then
  WrPool(fpc_capp + ' ' )
 else
  WrPool(fpc_gapp + ' ' );

 WrPool(name + ext ,true );
 
 if not FirstKey('mac_console_app' ,cp ) then
  begin
   WrPool('mkdir -p ' + name + '.app/Contents/MacOS' ,true );
   WrPool('mv -f ' + name + ' ' + name + '.app/Contents/MacOS/' + name ,true );
   
  end; 

 if FirstKey('mac_copy' ,cp ) then
  repeat
   spread_name(cp ,fp ,fn ,fx );

   if cmp_str(fx ) = cmp_str('.bmp' ) then
    WrPool('cp -f bmp/' + cp + ' ' + name + '.app/Contents/MacOS/' + cp ,true )
   else
    if cmp_str(fx ) = cmp_str('.svg' ) then
     WrPool('cp -f svg/' + cp + ' ' + name + '.app/Contents/MacOS/' + cp ,true );

  until not NextKey(cp );
  
// WriteFile
 name:='compile-' + name;
 
 if WrFile(destDir ,name ) then
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
procedure CreateCompileScript(destDir : FSRefPtr; name ,ext : shortstring; inRef : FSRefPtr );
var
 loaded : boolean;

 ossError : OSStatus;
 forkName : HFSUniStr255;
 forkSize : SInt64;
 forkRef  : SInt16;
 forkBuff : pointer;
 forkLoad : ByteCount;
 
 target ,value : shortstring;

begin
 write(' ' ,name ,ext ,' ... ' );
 
// Open Source .DPR file
 FSGetDataForkName(forkName ); 
 
 ossError:=FSOpenFork(inRef^ ,forkName.length ,forkName.unicode[0 ] ,fsRdPerm ,forkRef );

 if ossError = noErr then
  begin
   loaded:=false;
   
  // Load DPR keys
   FSGetForkSize(forkRef ,forkSize );
    
   if (forkSize > 0 ) and
      agg_getmem(forkBuff ,forkSize ) then
    begin
     ossError:=FSReadFork(forkRef ,fsAtMark + noCacheMask ,0 ,forkSize ,forkBuff ,forkLoad );

     if (ossError = noErr ) and
        (forkSize = forkLoad ) then
      begin
       loaded:=true;
   
       LoadKeys(forkBuff ,forkSize );
  
      end;
    
     agg_freemem(forkBuff ,forkSize );
 
    end;

  // Close DPR
   FSCloseFork(forkRef );
   
  // Create compilation script
   if loaded then
    begin
     if FirstKey('skip' ,value ) then
      writeln('to be not included -> skipped' )
     else
      begin
       target:='mac';
   
       FirstKey('target' ,target );
  
       if cmp_str(target ) = cmp_str('mac' ) then
        if WriteCompileScript(destDir ,name ,ext ) then
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
procedure ProcessObject(destDir : FSRefPtr; inCatInfo : FSCatalogInfoPtr; inRef : FSRefPtr; inSpec : FSSpecPtr );
var
 file_path ,file_name ,file_ext : shortstring;

begin
 if inCatInfo.nodeFlags and kFSNodeIsDirectoryMask = kFSNodeIsDirectoryMask then
 else
  begin
   spread_name(inSpec.name ,file_path ,file_name ,file_ext );

   if cmp_str(file_ext ) = cmp_str('.dpr' ) then
    CreateCompileScript(destDir ,file_name ,file_ext ,inRef );

  end;

end;
 
{ ITERATEFOLDER }  
function IterateFolder(var inFolder : FSRef ) : OSStatus;
var
 kRequestCountPerIteration : size_t;
 
 outStatus : OSStatus;
 
 kCatalogInfoBitmap : FSCatalogInfoBitmap;
 
 iterator : FSIterator;
 
 catalogInfoArray : FSCatalogInfoPtr;
 
 FSRefArray  : FSRefPtr;
 FSSpecArray : FSSpecPtr;
 
 actualCount : ItemCount;
 
 index : UInt32;
 
 changed : boolean;
  
begin
 kRequestCountPerIteration:=((4096 * 4 ) div sizeof(FSCatalogInfo ) );

// Get permissions and node flags and Finder info
//
// For maximum performance, specify in the catalog
// bitmap only the information you need to know
 kCatalogInfoBitmap:=kFSCatInfoNodeFlags or kFSCatInfoFinderInfo;
 
// On each iteration of the do-while loop, retrieve this
// number of catalog infos
//
// We use the number of FSCatalogInfos that will fit in
// exactly four VM pages (#113). This is a good balance
// between the iteration I/O overhead and the risk of
// incurring additional I/O from additional memory
// allocation

// Create an iterator
 outStatus:=FSOpenIterator(inFolder ,kFSIterateFlat ,iterator );
 
 if outStatus = noErr then
  begin
  // Allocate storage for the returned information
   agg_getmem(pointer(catalogInfoArray ) ,sizeof(FSCatalogInfo ) * kRequestCountPerIteration );
   agg_getmem(pointer(FSRefArray ) ,sizeof(FSRef ) * kRequestCountPerIteration );
   agg_getmem(pointer(FSSpecArray ) ,sizeof(FSSpec ) * kRequestCountPerIteration );
 
   if catalogInfoArray = NIL then
    outStatus:=memFullErr

   else
    begin
    // Request information about files in the given directory,
    // until we get a status code back from the File Manager
     repeat
      changed:=false;
 
      outStatus:=
       FSGetCatalogInfoBulk(
        iterator ,
        kRequestCountPerIteration ,
        actualCount ,
        changed ,
        kCatalogInfoBitmap , 
        catalogInfoArray ,
        FSRefArray ,
        FSSpecArray ,
        NIL );
 
     // Process all items received
      if (outStatus = noErr ) or
         (outStatus = errFSNoMoreItems ) then
       for index:=0 to actualCount - 1 do
        ProcessObject(
         @inFolder ,
         FSCatalogInfoPtr(
          ptrcomp(catalogInfoArray ) + 
          index * sizeof(FSCatalogInfo ) ) ,
         FSRefPtr(
          ptrcomp(FSRefarray ) +
          index * sizeof(FSRef ) ) , 
         FSSpecPtr(
          ptrcomp(FSSpecArray ) +
          index * sizeof(FSSpec ) ) );

     until outStatus <> noErr;
 
    // errFSNoMoreItems tells us we have successfully processed all
    // items in the directory -- not really an error
     if outStatus = errFSNoMoreItems then
      outStatus:=noErr;
 
    // Free the array memory
     agg_freemem(pointer(catalogInfoArray ) ,sizeof(FSCatalogInfo ) * kRequestCountPerIteration );
     agg_freemem(pointer(FSRefArray ) ,sizeof(FSRef ) * kRequestCountPerIteration );
     agg_freemem(pointer(FSSpecArray ) ,sizeof(FSSpec ) * kRequestCountPerIteration );
 
    end;

  end;
 
 FSCloseIterator(iterator );
 
 result:=outStatus;

end;

{ CREATEMAKEFILE }
procedure CreateMakeFile(destDir : FSRefPtr );
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
  
 WrFile(destDir ,'compile_make_all' ); 

end;
 
{ SCANDEMOS }
procedure ScanDemos;
var
 outStatus : OSStatus;
 folderRef : FSRef;
 fileSpecs : FSSpec;
 
begin
 outStatus:=FSMakeFSSpec(0 ,0 ,'' ,fileSpecs );  

 if outStatus = noErr then
  begin 
   outStatus:=FSpMakeFSRef(fileSpecs ,folderRef );

   if outStatus = noErr then
    begin
     outStatus:=IterateFolder(folderRef );
 
     writeln;
 
     if make_count > 0 then
      begin
       CreateMakeFile(@folderRef );
   
       writeln('SUCCESS: FPC compilation script files were created' );
       writeln('         for the AggPas demos listed above.' );
       writeln;
       writeln('         To compile the demos, run Terminal, change to the current' );
       writeln('         directory and type "./compile_make_all"' );
       writeln('         or "./compile-xxx", where "xxx" is the name of the demo.' );
  
      end
     else
      writeln('MESSAGE: No AggPas demo files were found in current folder !' );
  
    end
   else
    writeln('ERROR: Failed to create FSRef structure for the current folder !' );
  
  end
 else
  writeln('ERROR: Failed to search for files in the current folder !' ); 

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
 writeln('* properly all the AggPas demos on your Mac.                *' );
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