### Requirements 

* [Maven](https://maven.apache.org/)
* [Java SE Development Kit 13](https://www.oracle.com/technetwork/java/javase/downloads/jdk13-downloads-5672538.html) or above.

### Build and Run

This application was created with Maven
, which you can use to build and run the application. To do this you first need to go to the project root folder (where the pom.xml file is located) and type in the following commands:

1. `javac --version` - Verification that a proper java version is installed. 
2. `mvn --version` - Verification that maven is installed. 
3. `mvn compiler:compile -f pom.xml` - Compiles the application. 
4.  `mvn javafx:compile -f pom.xml` - May be necessary to compile changes to fxml resources. 
5. `mvn javafx:run -f pom.xml` - Starts the application. 

 
 ### Application 
 
 The created application is a simple drawing program. The user can select predefined shapes (lines, ovals, polygons),
 place them on the canvas. Using the menu it is possible to select individual objects to perform actions such as, changing 
 the objects size, color, line width, filling, or filling. The selected object can also be deleted. The program can save and 
 load previos drawings using serialziation. 