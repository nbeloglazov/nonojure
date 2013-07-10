package recognition;

public class Loader {

   public static void loadLibrary(String lib) {
        // Hack to load a library outside of Clojure's classloader
        System.loadLibrary(lib);
   }

}
