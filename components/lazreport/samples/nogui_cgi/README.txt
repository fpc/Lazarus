I. INTRODUCTION.

This project test the lazreport support for cgi, console and http server
applications. This support is implemented by using the LCL NoGui Widgetset,
and does not require any external DLL library like freetype, it require 
though the used font files for text measuring or rendering purposes.

All tests are implemented in the same program by using different 
build modes which are described below. Some tests use data from 
a database and in this case, simple DBase files are used in order to
minimize target system configuration, it is understood though that
any relational database available can be used instead.

Check the Project->Options->Additions and overrides dialog to see
how LCLWidgetset is assigned by build mode.

NOTE. This program was developed under Windows, the instructions for 
compiling, deployment and test may be different in other operating 
systems, the code however should be platform and OS independent.

II. DESCRIPTION OF THE BUILD MODES.

Build Mode: CGI Debug Local

This build mode creates cgi application for testing the cgi operation
under Windows. It will create a reporter.cgi program which have to be 
run under some http server, like apache, which in this case in done 
through xampp. The build mode set up a compiler define CGI which in turn
creates an application of type TCGIApplication.

In order to test this application the needed files have to be deployed
to the local server in some way, and apache needs to be configured to run
cgi scripts from the respective cgi-bin directory, this is explained 
somewhere else. Here some deployment suggestions:

	METHOD 1: Symbolic link everything to the cgi-bin directory

	Assuming that xampp is installed in drectory XAMPP and this program is in 
	the REPORTER directory, a link to reporter.cgi can be created in the right
	folder using:
	
	  0) Make sure apache is configure to use symbolic links in windows. 
	     In File XAMPP\apache\conf\httpd.conf, in <Directory "XAMPP/cgi-bin">
		 definition add flag FollowSymlinks to the "Option" value, restart apache.
	  1) Open a cmd window with administrator privileges
	  2) mklink XAMPP\cgi-bin\reporter.cgi REPORTER\reporter.cgi
		 mklink XAMPP\cgi-bin\images.lrf REPORTER\images.lrf
		 mklink XAMPP\cgi-bin\demo_cross.lrf REPORTER\demo_cross.lrf
		 mklink XAMPP\cgi-bin\disks.lrf REPORTER\disks.lrf
		 mklink XAMPP\cgi-bin\index.html REPORTER\index.html
		 mklink /D XAMPP\cgi-bin\data REPORTER\data
	  3) start XAMPP and run apache
	  4) point Navigator to http://localhost/cgi-bin/reporter.cgi

	METHOD 2: Deploy everything to the www server directory

	  0) Copy index.html to XAMPP\cgi-bin\ directory
	  1) Copy reporter.cgi to the XAMPP\cgi-bin\ directory
	  2) Copy *.lrf to the XAMPP\cgi-bin\ directory
	  2) Copy data directory to the XAMPP\cgi-bin\ directory
	  
    METHOD 3: Configure apache for serving cgi scripts directly for program directoy
	
	  basically would be a guide for configuring apache and not this app.

Once the deployment has been done and apache is configured, test the cgi application 
in the local server by opening a browser window and go to http://localhost/cgi-bin/reporter.cgi
or the server application at http://localhost:8080/

Build Mode: CGI Cross WinToLinux

Like the CGI Debug Local mode, this build mode set up a CGI define which
creates a TCGIApplication, the difference though is that it will build a 
reporter_linux.cgi linux binary by doing cross compiling to linux which
need to be deployed to the target linux server. In these days crosscompiling
set up of the compiler has been dramatically reduced in complexity by using 
the fpcupdeluxe program which is probably explained somewhere else. 
Here deployment is done with the help of the deploy.bat script, see that file 
for some indications, but basically it does the METHOD 2 described above.

Build Mode: Console App WinToLinux.

The same as CGI Cross WinToLinux but it creates a normal app.

Build Mode: Http Server.

This build mode creates a http stand alone server, on compiling it will produce
a reporter_server[.exe] file that could be run directly from the build directory
you should see the same page than in the cgi modes. It creates a THTTPApplication 
through the HTTP_SERVER define declared in the build mode.

Build Mode: Console App.

This build mode creates a console application whose purpose is debugging
(debugging live cgi programs could be tricky). Included is a bash script
(runtest.sh) that tries all different lazreport export backends with all
included reports.

III. DESCRIPTION OF THE REPORTS 

Todo. [This is (or will be) done in the index.html page]

IV. DESCRIPTION OF THE PROGRAM

Todo.


