================================================================================

 Anti-Grain Geometry - Version 2.4
 Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)

 Anti-Grain Geometry - Version 2.4 Release Milano 3 (AggPas 2.4 RM3)
 Pascal Port By: Milan Marusinec alias Milano
                 milan@marusinec.sk
                 http://www.aggpas.org
                 Copyright (c) 2005-2008

================================================================================
 News
================================================================================
 
 22.01.2008 - AggPas - version 2.4 Release Milano 3
 ----------
 
 - Library source code is updated all over to keep up with new possibilities 
   of AGG C/C++ 2.4 version. Some code is slightly changed and there are also 
   a newly ported units. However this update is not rewrite, for example image 
   and pattern stuff and path storage is kept from 2.3 (same functionality). 
   See examples for 14 new demos. 
   
 - Flash compound rasterizer - fully ported, full functional
 - Gaussian and stack blurs
 - Two more composition modes - invert & invert rgb
 - Focal gradient with extended center point
 - New gradients: Contour, Assymetric conic, Bitmap
 - Bitmap caching - see the Particles demo
 - SVG demo now works also with expat.dll
 - FreeType2 bindings - freetype demos are now fully ported
 - Agg2D API and also TAgg2D for Delphi VCL (over 100 API commands)

 - Documentation for TAgg2D API is available at:
 
   http://www.aggpas.org/documentation

   This documentation is not a part of software distribution package, because
   it is planned to be an independent live document evolving over time.

   
 26.06.2006 - AggPas - version 2.3 Release Milano 2
 ----------
 
 - SVG demo is ported for now. See svg_test.dpr & svg directory in src.
   As a part of the SVG demo, I had also ported partialy the Expat library.
   See Expat directory & read the readme.txt if you are interested more.

 - Whole source code of AggPas library was adjusted to be compilable under 
   the emerging 64bit FreePascal compiler. The adjustment includes changing
   the "integer(pointer(x ) )" constructs to the "ptrcomp(pointer(x ) )".
   Ptrcomp is defined in the agg_basics.pas and depending on the CPU64 
   conditional it is either integer or int64.

================================================================================
 Remarks
================================================================================
 
 I'm glad to hear that AGG and my work on AggPas inspired Mr. Lars Brubaker 
 to make a native port in C#.
 
 Try it at: http://agg-sharp.sourceforge.net/
 
 From the .9 demo seen so far, it looks very promising. Maybe over a year,
 I will try to help with the C# version to get finished (not promise).
 
 Keep on the good work Lars !
    
================================================================================
 Introduction
================================================================================

 AggPas is an Object Pascal port of the Anti-Grain Geometry library - AGG,
 originally written by Maxim Shemanarev in industrially standard C++. 
 AGG as well as AggPas is Open Source and free of charge 2D vector graphic
 library.
 
 AGG (and AggPas too) doesn't depend on any graphic API or technology. 
 Basically, you can think of AGG as of a rendering engine that produces pixel 
 images in memory from some vectorial data. But of course, AGG can do much 
 more than that. The ideas and the philosophy of AGG are ...
 
 Read more at: http://www.antigrain.com/about
 
 This version of AGG library is the Object Pascal one and this Readme Note 
 is dealing with issues mainly around the Pascal version.

================================================================================
 Installation notes & compiling
================================================================================ 

 AggPas can be currently compiled on the platforms with compilers 
 according to the following matrix:
 
 +----------------------+---------------------+---------------------+
 | Platform / Compiler  |  Free Pascal (FPC)  |   Delphi (Borland)  |
 +======================+=====================+=====================+
 | Windows Win32 API    | versions 2.0 and up | versions 2.0 and up |
 +----------------------+---------------------+---------------------+
 | Linux X11            | versions 2.0 and up | * no support *      |
 +----------------------+---------------------+---------------------+
 | Mac OS X Carbon      | versions 2.0 and up | * no support *      |
 +----------------------+---------------------+---------------------+
 
 After downloading and unpacking the library distribution archive I recommend
 to run the find_compilers_xxx utility, where xxx is the name of the platform.
 It will scan the current working directory for the Agg demo projects and
 create appropriate up to-date compile scripts. On the permission-based 
 file systems (linux & mac) it will also assign the "execute" file attributes
 to the script files. Then, to compile all of the demos, you just run 
 appropriate compile script from the command line (terminal). The utility also
 excludes from compile scripts those demos, which are primarily targeted
 to different platforms (for ex. TT & gpc demos compiles only on windows).

 AggPas was tested on the following systems:

 * Windows XP, Windows 2000
 * Mac OS X 10.4.5
 * Fedora Core 4 
 
================================================================================
 Port comments
================================================================================

 The Object Pascal version (AggPas) was created mainly during the first three
 months of the year 2006. This port is based on the C++ version 2.3, which was
 officialy released on 25 September 2005.

 AggPas port is the pure manual work. There were used no c_to_pascal conversion 
 utilities and the port is the native-one, which means it uses no external dll
 bindings - it's all just the native pascal code. 
 
 This Pascal library has nearly all of the original C++ version functionality. 
 I will mention the differences more around on later. All of the demos were 
 ported (except 3 of them), and they do exactly all that the C++ version demos 
 do.
 
 AggPas supports the following rendering buffer pixel formats:
 
 - gray8 ,gray8_pre
 - gray8_bgr24r ,gray8_pre_bgr24r
 - gray8_bgr24g ,gray8_pre_bgr24g
 - gray8_bgr24b ,gray8_pre_bgr24b
 
 - rgb555 ,rgb555_pre ,rgb555_gamma
 - rgb565 ,rgb565_pre ,rgb565_gamma
 
 - bgr24 ,bgr24_pre ,bgr24_gamma
 - rgb24 ,rgb24_pre ,rgb24_gamma

 - bgra32 ,bgra32_pre
 - rgba32 ,rgba32_pre
 - argb32 ,argb32_pre
 - abgr32 ,abgr32_pre
 
 - custom_blend_rgba

 The high precision pixel formats support is not implemented for now, mainly 
 for the reason, I haven't any real-world examples to try and test with. 
 There are also some other issues related to the color data structure and 
 it's data members, which are of byte size per color component now. In C++ 
 version it was solved using the templates, but in Pascal i would have to find 
 another solution.

 C++ TEMPLATES
 =============

 Well, well, well. I was forced to remove the Maxim's template based 
 architecture and to replace it with the conventional one - object oriented, 
 because in Object Pascal there are no templates. Due to that there is a 
 speed penalty consequence. AggPas is in fact slower by 20 - 50%. 

 Despite the speed decrease, the AggPas is still a very usable super-trooper 2d 
 vector graphic library. Unless you are willing to make a real-time (30 fps >) 
 rendering, there is still a great benefit of incorporating it into your Pascal 
 based projects. Try to compile the demos and see for yourself ...

 On the other hand, I believe I had opened the doors for someone to try to 
 port the Agg into some other language(s). Is there anybody out capable
 of making the template-less Java or C# native-port version ? Trust me, it's 
 worth doing so.

 For those agg-fanatics with spare 3 or so months (like me), willing to create
 some cool agg-native-port, I am giving here some hints:

 AggPas was ported demo by demo in the following order:

 Simple:  pure_api, component_rendering ,polymorphic_renderers ,rasterizers ,
          rounded_rect ,aa_demo ,bspline ,scanline_boolean ,idea ,trans_polar ,
          aa_test ,circles ,gamma_correction ,gamma_ctrl ,gouraud ,rasterizers2
        
 Lion:    lion ,lion_lens ,lion_outline ,perspective ,simple_blur ,alpha_mask ,
          alpha_mask2, multi_clip

 Fonts:   conv_contour ,raster_text ,trans_curve1 ,trans_curve2 ,truetype_test

 England: alpha_mask3 ,gpc_test ,scanline_boolean2

 Complex: alpha_gradient ,bezier_div ,conv_dash_marker ,conv_stroke ,gradients ,
          graph_test ,pattern_fill
         
 Images:  line_patterns ,image1 ,image_alpha ,image_filters ,image_filters2 ,
          image_fltr_graph ,image_perspective ,image_resample ,image_transforms ,
          pattern_perspective ,distortions ,compositing

 After finishing all of the demos above, the whole library consisting of 120+ 
 files folded up. I must appreciate Maxim's work for completness at this point,
 because only four source files left unused in the library:
 
 - agg_conv_close_polygon.h
 - agg_conv_unclose_polygon.h
 - agg_rendering_buffer_dynarow.h
 - agg_trans_lens.h
 
 But as I said, 3 demos were ported not and I believe those source files are 
 used just there (see the What's next on the end of this readme).

 Start porting with pure_api and all units it uses. Other unit's ports comes
 automatically, as you will go through more and more demos.

 Create appropriate basic data types (agg_basics) and then use them. The most
 used types throughout agg are "int" and "unsigned" which are "longint" and
 "longword" in pascal.

 Be cautious of arithmetic shift right operation (>>) on signed integers. 
 In pascal, there was an incompatibility with the C++ implementation.
 On the assembler level, pascal compilers were always generating "shr" (i386)
 "srw" (powerpc) instructions instead of "sar" (i386) "sraw" (powerpc) ops.
 I hacked this situation with shr_intXX function calls whenever arithmeticaly
 shifting right signed integers (see the end of agg_basics.pas file).
 That leads of cource to speed penalty, because of additional function call,
 but not doing so is very crucial - lots of agg algos depends on ">>".


 Back to the Pascal port Issues
 ============================== 
 
 Accessors:

 The names of getters and setters methods of class objects are a little bit 
 modified against the c version mostly due to the fact, that pascal doesn't
 distinguish between same methods names, if they are differentiating only by
 the return value. I used one rule: If the method is setter, then it has
 the underscore after the name (eg: width_, like width=something). If the
 method is getter, then it has the underscore before the name (eg: _width,
 like something=width). [See agg_vcgen_stroke for example].

 Source code comment marks:

 Here and there around the AggPas source code, following comment marks
 can be found:
 
 {untested} - unit or procedure/function, which was ported but not tested
              (there was no reference to it's use from demos)
  
 {not_implemented} - procedure/function, which was not ported, because
                     related features were not implemented in port
 
 {hack} - marks some nonstandard solution
 
 {..} - part of the code, which is unfinished. There is just
        one unit with this marks - agg_color_conv.pas, with lots
        of pixel format conversion functions. Sorry, I was too 
        lazy to finish this one. I did all conversion functions, 
        which are used by the demos. If you'll need some more,
        finish it yourself. It's not that hard (but many of them).

 Compiling mode:

 The "agg_mode.inc" is the main compiler mode configuration file.
 The only one item, you would wish to change is the AGG_DEBUG conditional,
 which turns on/off the debug mode compilation settings. With the
 AGG_DEBUG not defined the range checking for example is turned off,
 which should lead to building the faster and smallest code.


 Bugs
 ====

 All bugs i know of in AggPas are related to the compiler issues.
 These are:

 In Delphi on Windows:

 - The SetDIBitsToDevice WinAPI call sometimes fails, but only in Delphi.
   In FPC it always works OK. Due to Delphi, I hacked this with solution,
   which puts on fail the image to the screen anyway, but with some speed 
   penalty of course. (See agg_win32_bmp.pas::pixel_map::draw fn).

 In FPC on Windows:
 
 - GetGlyphOutline(X) WinAPI call is working not. The consequence of this
   is that the True-type demos compiled with FPC work not. 
   I have reported this problem to the FPC people (bug report 4827).
   They have fixed it and in upcoming version 2.0.3 of FreePascal compiler
   it should be OK.

 In FPC on Windows and Linux:

 - Demos gouraud.dpr & aa_test.dpr generate some broken gouraud shading 
   patterns. I think, this is an compiler error issue, because in FPC
   on Mac (which is powerpc) - it is OK, from the same source code, as
   well as in Delphi.

 If anybody knows how to fix those (or other) bugs, don't hesitate to
 send me a mail or fix it yourself and let me know.
   
   
 MacPort comments
 ================

 The speed of the library on the PowerPC Mac target is rather slow.
 I suspect, this is due to the FPC compiler, which itself is not much
 optimized for PowerPC targets. Maybe I'm wrong, but the size of the
 compiled executables is about 1.4 Megs, compared to the 200 KB C++
 versions of the same demos.

 The agg demo framework has on Mac in Pascal following improvements
 over the C++ version:

 - resizing of windows
 - right click
 - app.message displays the "long" C strings
 - keyboard support is better (sort of hack) with Alt+F4 for quit

 When compiling on Mac, there is a need of additional libraries,
 which are not a part of the Free Pascal installation. They are
 the Universal Pascal Interfaces (UPI), which are a part of the
 Apple Universal Interfaces, which can be downloaded at:
 
  ftp://ftp.apple.com/developer/Development_Kits/UniversalInterfaces3.4.2.dmg

 To compile AggPas on Mac succesfully you will need the following
 files in the upi directory of AggPas:
  
 - Carbon.o
 - Carbon.ppu
 - ImageCompression.o
 - ImageCompression.ppu
 - QuickTimeComponents.o
 - QuickTimeComponents.ppu

 If you had downloaded the Mac .dmg AggPas archive, it already 
 contains above mentioned files, so you don't have to download
 anything else - just compile. In the case, you want to recompile
 the UPI for yourself, do following:
 
  1. Click on the UniversalInterfaces3.4.2.dmg archive
  2. Change directory to Universal/Interfaces/PInterfaces
  3. Copy all files to AggPas/UPInterfaces
  4. Execute ./build_upi script


 LinuxPort comments
 ==================

 The linux port of agg demo framework has one small improvement
 over the C++ version -> It displays the app.message(s) in window
 instead of sending the text to the stderr output.


================================================================================
 What's next
================================================================================
 
 I mentioned above, that 3 demos (out of 50+) were ported not.
 These are:
 
 - mol_view: 
 
   I'm sorry, but this one was the last one, I was lazy to port.
   Besides, there were no new units used.
 
 - freetype_test (& related trans_curve1/2_ft):
   
   [Update] Fully working in AggPas 2.4 RM3.

 
 All for now
 Yours Milano
 
================================================================================
 End of file
================================================================================
