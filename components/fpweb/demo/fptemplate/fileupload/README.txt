File upload from html form, example
==========================
(Requires FPC versions past 4/30/2009 /revisions 13062 and later/)

Demonstrates how to handle the file upload (multipart) html forms with using 
templates.

Setup:

CGI
---------
Usually it works if you put the templates next to the CGI executable file.
(uploadform.html)
The example needs a directory to store the files (default is "upfiles/" which 
needs to be created. Also needs read/write access to create the file database 
(default is filelist.txt next to the templates).

http://<WebServer>/cgi-bin/<CGIExecutableName>/listfiles should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/cgi-bin/fileupload.exe/listfiles

Note: You need to change the URLs in the templates if "cgi-bin" or 
"fileupload.exe" changes (for example on Linux it is not fileupload.exe).
(* see at the bottom)


Apache
---------
Usually it works if you put the templates into the Apache main directory (not 
the DocumentRoot, but the main Apache directory).
(uploadform.html)
The example needs a directory to store the files (default is "upfiles/" which 
needs to be created. Also needs read/write access to create the file database 
(default is filelist.txt next to the templates).

http://<WebServer>/<ApacheLocationName>/listfiles should start the 
example if everything is set up properly.
(* see at the bottom)
ex: http://127.0.0.1:8080/myapache/listfiles
if in httpd.conf it was set up as:
LoadModule mod_fileupload "<path_to_mod>/mod_fileupload.dll"
<Location /myapache>
    SetHandler mod_fileupload
    Order allow,deny
    Allow from all
</Location>

Note: You need to change the URLs in the templates if "myapache" changes. 
Also, for example on Linux the module can be mod_fileupload.so and not 
mod_fileupload.dll 

Note: If you recompile an apache module while the module itself is loaded into
the Apache server, the compilation will fail because the file is in use (Apache
 modules stay in the memory). So first you always need to stop the server before
you recompile or before you copy over the new version of the created module.



(*)To use the same templates for both CGI programs and Apache modules the 
links can also be generated dynamically when the response html pages are 
generated. For example instead of putting

/cgi-bin/fileupload.exe/listfiles
/myapache/listfiles

into the templates, a tag can be used to put the right link there.
Ex:
<form action="{+SCRIPTPATH+}/listfiles" method="post" ...
Of course, the {+SCRIPTPATH+} template tag needs to be replaced with the 
correct string.