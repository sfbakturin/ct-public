package md2html;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

/**
 * @author Saveliy Bakturin
 *
 * Don't write off, if you don't wanna be banned!
 */

public class FileInputHelper {
    private BufferedReader input;
    private boolean closed = true;
    private String scanned;

    public FileInputHelper(final String i) {
        try {
            this.input = new BufferedReader(new InputStreamReader(new FileInputStream(i), StandardCharsets.UTF_8));
            this.closed = false;
        } catch (final FileNotFoundException err) {
            System.out.println("File not found: " + err.getMessage());
        }
    }

    public void close() {
        if (this.closed) {
            return;
        } else {
            this.closed = true;
            try {
                this.input.close();
            } catch (final IOException err) {
                System.out.println("Scanner cannot be closed: " + err.getMessage());
            }
        }
    }

    public String nextLine() {
        return this.scanned;
    }

    public boolean hasNextLine() {
        this.scanned = null;

        try {
            this.scanned = this.input.readLine();
        } catch (final IOException err) {
            System.out.println("Line cannot be read: " + err.getMessage());
        }

        return (this.scanned != null);
    }
}
