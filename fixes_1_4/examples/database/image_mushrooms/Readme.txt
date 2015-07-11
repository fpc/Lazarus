MushRoomsDatabase by Jurassic Pork using SQLite - January 2014
Updated for Firebird Embedded - August 2014

Features:
- Use SqlDb and lazreport components.
- Sqlite3 or Firebird embedded database DeadlyMushrooms with 5 mushrooms.
  Sqlite3 will be tried first; if no Sqlite library is available, Firebird 
	embedded will be tried.
- It demonstrates:
  - creating a new SQLite3 database with table if the db does not exist
  - use of TSQLScript to run multiple SQL statements
  - use of FBAdmin to restore Firebird backup (smaller than the live .fdb file) 
    on first run, useful for keeping your setup file small and compatible with
    older Firebird versions
- The images are stored in blob field without extension at the beginning.
  With this you can view blob images with database browser editor 
  (e.g. sqlite2009pro).
- In the database there is also a field with images links (filenames).
- The linked images are stored in the folder images of the project.
- You can see the linked images in a Timage.
- You can change the images in the database:
  - for Tdbimage (image in db): double click on the component and choose your 
    image.
  - for Timage (linked image): click on the button near the image filename 
    (you must be in edit mode).
- Transaction commits when you click on Tdbnavigator refresh button or on close 
  form.
- Small pictures of the mushrooms are in the sqlite3Database. Largest images are
  in files in the folder images.
- Print button to print all the mushrooms (lazreport).
  On each page you have:
   - a title. 
   - the field common_name of the mushroom database.
   - the field notes of the mushroom database.
   - the field picture of the mushroom database (picture picture1).
   - the picture of the field image_link (picture picture2).

The report name is Mushroom_Report.lrf
