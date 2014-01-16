MushRoomsDatabase by Jurassic Pork  - January 2014
Features:
- Use SqlDb and lazreport components.
- Sqlite3 database DeadlyMushrooms with 5 mushrooms.
- The images are stored in blob field without extension at the beginning.
  With this you can view blob images with database browser editor (ex sqlite2009pro).
- In the database there is also a field with images links (filenames).
- You can see the linked images in a Timage.
- The linked images are stored in the folder images of the project.
- You can change the images in the database :
  - for Tdbimage double click on the component and choose your image.
  - for Timage click on the button near the image filename.(you must be in edit mode )
- Transaction commit when you click on Tdbnavigator refresh button or on close form.
- Small pictures of the mushrooms are in the sqlite3Database. Largest images are in files in the folder images.
- Print button to print all the mushrooms (lazreport).
  On each page you have: 
   - a title. 
   - the field common_name of the mushroom database.
   - the field notes of the mushroom database.
   - the field picture of the mushroom database (picture picture1).
   - the picture of the field image_link (picture picture2).

The report  name is Mushroom_Report.lrf
