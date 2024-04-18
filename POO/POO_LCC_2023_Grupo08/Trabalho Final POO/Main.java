import java.io.IOException;

/**
 * Write a description of class Main here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class Main
{
    public static void main(String[] args) throws NaoExisteTransportadorException, NaoExisteUtilizadorException, IOException, ClassNotFoundException{
        Menus m = new Menus();
        m.menu();
      
    }
}
