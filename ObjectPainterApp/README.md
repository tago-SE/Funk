 ### Application 
 The created application is a simple drawing program. The user can select predefined shapes (lines, ovals, polygons),
 place them on the canvas. Using the menu it is possible to select individual objects to perform actions such as, changing 
 the objects size, color, line width or filling. The selected object can also be deleted. The program can save and 
 load previous drawings using Object-Serialization. 
 
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


 ### Design Patterns
 
This section outlines the design patterns used in the application. 

* [Subject-Subscriber](https://github.com/iluwatar/java-design-patterns/tree/master/observer) - Changes made to the canvas or the saved files are notified to the controller which updates the view. 
* [Composite](https://github.com/iluwatar/java-design-patterns/tree/master/composite) - Selection are treated as a composite of shapes that can be manipulated as one to change color, get its size and so on.
* [Command](https://github.com/iluwatar/java-design-patterns/tree/master/command) - Was implemented to allow do/undo.
* [Template-method](https://github.com/iluwatar/java-design-patterns/tree/master/template-method) - The rendering of shapes was done throuh the template method. 
* [Builder](https://github.com/iluwatar/java-design-patterns/tree/master/builder) - Was implemented because a shape could have a unlimited amount of properties, thus it made sense to move the configuration of these properties into a Builder class.  
* [Prototype](https://github.com/iluwatar/java-design-patterns/tree/master/prototype) - Was added to allow the addition of new shapes, and allowing menus structures to be created at run time rather than at compile time.  
* [Abstract factory](https://github.com/iluwatar/java-design-patterns/tree/master/abstract-factory) - This was only implemented in principle for the creation of a few menu components. 


