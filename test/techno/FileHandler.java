package techno;

import java.io.InputStream;
import java.io.FileInputStream;

public class FileHandler {
    private String base;
    public FileHandler() {
    }
    public FileHandler(String path) {
        base = path;
    }
    public void setBase(String path) {
        base = path;
    }
    public String sketchPath(String file) {
        return base + file;
    }
    public InputStream createInput(String file) throws Exception {
        return new FileInputStream(sketchPath(file));
    }
}
