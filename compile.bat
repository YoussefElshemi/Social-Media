javac -d bin/ src/socialmedia/*.java
jar cvf socialmedia.jar -C bin .
jar uvf socialmedia.jar -C src .
jar uvf socialmedia.jar doc
jar uvf socialmedia.jar res