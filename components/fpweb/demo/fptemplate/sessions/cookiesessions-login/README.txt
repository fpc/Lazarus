Sessions stored in cookies with Login, example
======================================
(Requires FPC versions past 4/30/2009 /revisions 13062 and later/)

Note: Cookies must be enabled for the website in the visitor's browser for it 
to work.

Every visitor needs to log in to get a session ID that is passed in a 
cookie to the web visitor's browser and stored on the web server until it 
expires.
This way fully functional web sites can be built without mixing the different 
visitor's screens.

Note: This example can not distinguish between sessions on the same computer, same 
browser but different tabs. If for example someone opens three tabs and logs in 
with all of them, all three browser tabs will be the same session.
In order to handle sessions differently by browser tabs the best way to go is 
to store session IDs in the URLs, Links, etc. for all pages the web application
 generates (this way cookies are not needed to maintain the session).
The sample user logins are stored in userdb.txt for the example.


Setup:
The web server application needs read/write access to the file where the session
 informations will be stored with the login names (sessiondb.txt by default).

CGI
----------
Usually it works if you put the templates next to the CGI executable file.
(testlogin.html, testlogout.html, testwelcome.html, testsomepage.html and
userdb.txt)

http://<WebServer>/cgi-bin/<CGIExecutableName>/login should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/cgi-bin/cookiesession.exe/login

Note: You need to change the URLs in the templates if "cgi-bin" or 
"cookiesession.exe" changes (for example on Linux it is not cookiesession.exe).
(* see at the bottom)


Apache
----------
Usually it works if you put the templates into the Apache main directory (not 
the DocumentRoot, but the main Apache directory).
(testlogin.html, testlogout.html, testwelcome.html, testsomepage.html and
userdb.txt)

http://<WebServer>/<ApacheLocationName>/login should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/myapache/login
if in httpd.conf it was set up as:
LoadModule mod_cookiesession "<path_to_mod>/mod_cookiesession.dll"
<Location /myapache>
    SetHandler mod_cookiesession
    Order allow,deny
    Allow from all
</Location>

Note: You need to change the URLs in the templates if "myapache" changes. 
Also, for example on Linux the module can be mod_cookiesession.so and not 
mod_cookiesession.dll 
(* see at the bottom)

Note: If you recompile an apache module while the module itself is loaded into
the Apache server, the compilation will fail because the file is in use (Apache
 modules stay in the memory). So first you always need to stop the server before
you recompile or before you copy over the new version of the created module.



(*)To use the same templates for both CGI programs and Apache modules the 
links can also be generated dynamically when the response html pages are 
generated. For example instead of putting

/cgi-bin/cookiesession.exe/login
/myapache/login

into the templates, a tag can be used to put the righ link there.
Ex:
<form action="{*SCRIPTPATH*}/login" method="post" ...
Of course, the {*SCRIPTPATH*} template tag needs to be replaced with the 
correct string.