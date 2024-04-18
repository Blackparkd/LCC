import java.io.*;
import java.net.*;
// javac Client.java para compilar
// java Client 172.26.70.179 <room number> para correr
//             192.168.1.75 se for no fixo !!

public class Client {
    public static void main(String[] args) {
        try {
            if (args.length < 2) {
                System.out.println("Usage: java Client <host> <port>");
                System.exit(1);
            }
            String host = args[0]; // host(ip)
            int port = Integer.parseInt(args[1]); // port(sala)
            
            //String password = args[2]; // password

            Socket s = new Socket(host, port);
            BufferedReader in = new BufferedReader(new InputStreamReader(s.getInputStream()));
            PrintWriter out = new PrintWriter(s.getOutputStream(), true);

            //out.println(password);

            ReaderThread reader = new ReaderThread();
            reader.reader = in;
            Thread readerThread = new Thread(reader);
            readerThread.start();

            WriterThread writer = new WriterThread();
            writer.writer = out;
            Thread writerThread = new Thread(writer);
            writerThread.start();

        } catch (Exception e) {
            e.printStackTrace();
            System.exit(0);
        }
    }

    static class ReaderThread implements Runnable {
        public BufferedReader reader;

        public void run() {
            try {
                while (true) {
                    String res = reader.readLine();
                    if (res != null) {
                        System.out.println(res);
                    }
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    static class WriterThread implements Runnable {
        public PrintWriter writer;

        public void run() {
            try {
                BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));
                while (true) {
                    String res = stdin.readLine();
                    if (res != null) {
                        writer.println(res);
                    }
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}