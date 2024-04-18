import java.util.Scanner;
import java.io.IOException;
import java.io.File;

/**
 * Write a description of class Mens here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class Menus
{
    private Scanner ler;
    private Vintage loja;

    
    public Menus(){
       this.ler = new Scanner(System.in);
        File file = new File("data.obj");
        
        this.loja=new Vintage(0.12, 0.16);
        if(file.length()!=0){
            try
            {
                try
                {
                    this.loja=Vintage.leobj();
                }
                catch (IOException ioe)
                {
                    ioe.printStackTrace();
                }
            }
            catch (ClassNotFoundException cnfe)
            {
                cnfe.printStackTrace();
            }
        }
        else{
            this.loja= new Vintage(0.12, 0.16);
        }
        
        
        System.out.println();
        System.out.println("\t##################################################");
        System.out.println("\t######### TRABALHO REALIZADO POR: ################");
        System.out.println("\t##################################################");
        System.out.println("\t######### JOÃO SILVA, LCC, A91638      ###########");
        System.out.println("\t######### EDUARDO PEREIRA, LCC, A70619 ###########");
        System.out.println("\t######### ÁUREO BENEDITO, LCC, A76068  ###########");
        System.out.println("\t##################################################");
        
        
    }
    
    public void menu() throws NaoExisteUtilizadorException {
        System.out.println();
        System.out.println();
        
        int opt=0;
        while(opt!=-1){
            
            System.out.println("O QUE PRETENDE FAZER?");
            System.out.println("1) ACEDER A SUA CONTA");
            System.out.println("2) OPERAÇOES DA LOJA");
            System.out.println("-1) TERMINAR O PROGRAMA");
            opt=this.ler.nextInt();
            this.ler.nextLine();
            
            if(opt==1){
                System.out.println("INSIRA O SEU NIF");
                int nif = this.ler.nextInt();
                this.ler.nextLine();
                System.out.println("INSIRA O SEU EMAIL");
                String mail = this.ler.nextLine();
                Utilizador u=null;
                try{
                    u=this.loja.procuraUser(nif);
                    
                }catch(NaoExisteUtilizadorException e){
                    System.out.println(e.getMessage());
                }
                
                if(u!=null){
                    int opt2=0;
                    if(u.getEmail().equals(mail)){
                        System.out.println("\tacesso permitido\n");
                        while(opt2!=-1){
                            System.out.println("\tQue Operacao Pretende Realizar?\n");
                            System.out.println("1----Criar artigo");
                            System.out.println("2----Remover artigo");
                            System.out.println("3----Criar encomenda");
                            System.out.println("4----Adicionar artigo a encomenda");
                            System.out.println("5----Remover artigo de encomenda");
                            System.out.println("6----Finalizar encomenda");
                            System.out.println("7----Ver os meus artigos expostos para venda");
                            System.out.println("8----Ver os meus artigos em processo de venda");
                            System.out.println("9----Ver os meus artigos vendidos");
                            System.out.println("10---Ver os artigos que comprei");
                            System.out.println("11---Ver as minhas encomendas");
                            System.out.println("12---Ver as minhas faturas");
                            System.out.println("-1---Fazer logout");
                            opt2=this.ler.nextInt();
                            this.ler.nextLine();
                            if(opt2==1){
                                System.out.println("insira a transportadora a que pretende associar o artigo");
                                for(Transportador t : this.loja.getTransportadores()){
                                    System.out.println(t.toString());
                                }
                                String transp = this.ler.nextLine();
                                Transportador t = null;
                                try{
                                    t=this.loja.procuraTransportador(transp);
                                }
                                catch(NaoExisteTransportadorException e){
                                    System.out.println(e.getMessage());
                                }
                                if(t!=null){
                                    int optart=0;
                                    System.out.println("\tque tipo de artigo pretende vender?");
                                    System.out.println("1--Sapatilha");
                                    System.out.println("2--Sapatilha Premium");
                                    System.out.println("3--Tshirt");
                                    System.out.println("4--Mala");
                                    System.out.println("5--Mala Premium");
                                    optart=this.ler.nextInt();
                                    this.ler.nextLine();
                                    if(optart==1){
                                        criasap(u, t, this.loja);
                                    }
                                    if(optart==2){
                                        criasapprem(u, t , this.loja);
                                    }
                                    if(optart==3){
                                        criatshirt(u, t, this.loja);
                                    }
                                    if(optart==4){
                                        criamala(u, t, this.loja);
                                    }
                                    if(optart==5){
                                        criamalaprem(u, t, this.loja);
                                    }
                                }
                            }
                            if(opt2==2){
                                for(Artigo art : u.todosArtigos()){
                                    System.out.println(art.toString());
                                }
                                System.out.println("insira o codigo do artigo que pretende remover");
                                String cod=this.ler.nextLine();
                                u.retiravenda(u.daArtigo(cod));
                                
                                System.out.println("artigo removido com sucesso");
                            }
                            if(opt2==3){
                                for(Transportador t : this.loja.getTransportadores()){
                                    System.out.println(t.toString());
                                }
                                System.out.println("insira o nome da transportadora pela qual pretende efetuar a encomenda");
                                String t = this.ler.nextLine();
                                try{
                                    Transportador transp=this.loja.procuraTransportador(t);
                                    Encomenda e = new Encomenda(u.getNif(), u.getMorada());
                                    System.out.println("carregue 1) se pretende adicionar o artigo a encomenda");
                                    if(this.loja.associados(transp).size()>0 && this.loja.associados(transp)!=null){
                                        for(Utilizador us : this.loja.todosUsers()){
                                            for(Artigo art : us.todosArtigos()){
                                                if(u.meuartigo(art)==false && us.procuraTransp(art).equals(transp)){
                                                    System.out.println(art.toString());
                                                    int ad=0;
                                                    ad=this.ler.nextInt();
                                                    this.ler.nextLine();
                                                    if(ad==1){
                                                       this.loja.encomendaAdicionaArtigo(e, art);
                                                       System.out.println("artigo adicionado com sucesso");
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    if(e.getNumArtigos()>0){
                                        transp.addEncomenda(e);
                                        System.out.println("encomenda criada com sucesso");
                                    }
                                }catch(NaoExisteTransportadorException e){
                                    System.out.println(e.getMessage());
                                }
                            }
                            
                            if(opt2==4){
                                System.out.println("tcarrega 1) para escolher a encomenda a adicionar artigos");
                                for(Transportador t : this.loja.getTransportadores()){
                                    for(Encomenda e : t.getEncomendas()){
                                        if(e.getEstado().equals("pendente")){
                                            System.out.println(e.toString());
                                            int esc=0;
                                            esc=this.ler.nextInt();
                                            this.ler.nextLine();
                                            if(esc==1){
                                                for(Artigo art : this.loja.associados(t)){
                                                    if(u.meuartigo(art)==false && e.temArtigo(art)==false){
                                                        System.out.println(art.toString());
                                                        int ad=0;
                                                        ad=this.ler.nextInt();
                                                        this.ler.nextLine();
                                                        if(ad==1){
                                                            e.addArtigo(art);
                                                            System.out.println("artigo adicionado com sucesso");
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            
                            if(opt2==5){
                                System.out.println("carregue 1 para escolher a encomenda a remover artigo");
                                for(Encomenda e : this.loja.encomendasCompradas(u)){
                                    if(e.getEstado().equals("pendente") && e.getNumArtigos()>0){
                                        System.out.println(e.toString());
                                        int esc=0;
                                        esc=this.ler.nextInt();
                                        this.ler.nextLine();
                                        Transportador t=null;
                                        for(Artigo a : e.getArtigos()){
                                            t=this.loja.quemPertence(a).procuraTransp(a);
                                        }
                                        if(esc==1){
                                            System.out.println("carregue 1 para escolher o artigo a remover");
                                            for(Artigo a : e.getArtigos()){
                                                System.out.println(a.toString());
                                                int rem=0;
                                                rem=this.ler.nextInt();
                                                this.ler.nextLine();
                                                if(rem==1){
                                                    e.removeArtigo(a);
                                                    this.loja.quemPertence(a).retiraAvender(a);
                                                    this.loja.quemPertence(a).avenda(a, t);
                                                    System.out.println("artigo removido com sucesso");
                                                }
                                                if(e.getArtigos().size()==0){
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                               
                        
                            
                            if(opt2==6){
                                System.out.println("carregue 1 para finalizar a encomenda");
                                for(Transportador t : this.loja.getTransportadores()){
                                    for(Encomenda e : t.getEncomendas()){
                                        if(e.getEstado().equals("pendente") && e.getNumArtigos()>0 && e.getNifComprador()==u.getNif()) {
                                            System.out.println(e.toString());
                                            int fin =0;
                                            fin=this.ler.nextInt();
                                            this.ler.nextLine();
                                            int removeu=0;
                                            if(fin==1){
                                                this.loja.encomendaFinaliza(e); 
                                                System.out.println("encomenda finalizada com sucesso");
                                                t.setNumEncs(t.getNumEncs()+1);
                                            }
                                        }
                                }
                                }
                             
                            }
                            
                            if(opt2==7){
                                for(Artigo a : u.todosArtigos()){
                                    System.out.println(a.toString());
                                }
                            }
                            
                            if(opt2==8){
                                for(Artigo a : u.getAvender()){
                                    System.out.println(a.toString());
                                }
                            }
                            
                            if(opt2==9){
                                for(Artigo a : this.loja.artigosVendidos(u)){
                                    System.out.println(a.toString());
                                }
                            }
                            
                            if(opt2==10){
                                for(Artigo a : this.loja.artigosComprados(u)){
                                    System.out.println(a.toString());
                                }
                            }
                            
                            if(opt2==11){
                                for(Encomenda e : this.loja.encomendasCompradas(u)){
                                    System.out.println(e.toString());
                                }
                            }
                            
                            if(opt2==12){
                                for(Fatura f : u.getFaturas()){
                                    System.out.println(f.toString());
                                }
                            }
                        }
                    }
                    else{
                        System.out.println("\tDADOS DE LOGIN INCORRETOS!!!\n");
                    }
                }
            }
            if(opt==2){
                int opt2=0;
                while(opt2!=-1){
                    System.out.println("\tQue Operacao Pretende Realizar?\n");
                    System.out.println("1---Criar utilizador");
                    System.out.println("2---Remover utilizador");
                    System.out.println("3---Imprimir utilizadores");
                    System.out.println("4---Criar transportador");
                    System.out.println("5---Imprimir transportadores");
                    System.out.println("6---Transportador expede encomenda");
                    System.out.println("7---Transportador entrega encomenda");
                    System.out.println("8---Imprime encomendas de uma transportadora");
                    System.out.println("9---Utilizador que mais vendeu");
                    System.out.println("10--Utilizador que mais gastou em compras da loja");
                    System.out.println("11--Transportadora que mais faturou em encomendas da loja");
                    System.out.println("12--Valor faturado pela loja");
                    System.out.println("13--Informacoes adicionais sobre a loja");
                    System.out.println("-1--Voltar atras");
                    
                
                
                
                    opt2=this.ler.nextInt();
                    this.ler.nextLine();
                    
                    if(opt2==1){
                        criauser(this.loja);
                    }
                    
                    else if(opt2==2){
                        System.out.println("digite o codigo do utilizador que pretende remover");
                        for(Utilizador u : this.loja.todosUsers()){
                            System.out.println(u.toString());
                        }
                        String cod = this.ler.nextLine();
                        if(this.loja.existeCodigo(cod)==true){
                            this.loja.removeUser(this.loja.procuraUserPeloCodigo(cod));
                        }
                        else{
                            System.out.println("NAO EXISTE TAL CODIGO");
                        }
                    }
                    
                    else if(opt2==3){
                        for(Utilizador u : this.loja.todosUsers()){
                            System.out.println(u.toString());
                        }
                    }
                    
                    else if(opt2==4){
                        criatransportador(this.loja);
                    }
                    
                    else if(opt2==5){
                        for(Transportador t : this.loja.getTransportadores()){
                            System.out.println(t.toString());
                        }
                    }
                    
                    else if(opt2==6){
                        System.out.println("DIGITE O NOME DA TRANSPORTADORA COM QUE PRETENDE EFETUAR A OPERACAO");
                        String nomet=this.ler.nextLine();
                        Transportador t = null;
                        try{
                            t=this.loja.procuraTransportador(nomet);
                        }
                        catch(NaoExisteTransportadorException e){
                            System.out.println(e.getMessage());
                        }
                        if(t!=null){
                            System.out.println("Carregue 1) se pretender expedir a encomenda");
                            for(Encomenda e : t.getEncomendas()){
                                if(e.getEstado().equals("finalizada")){
                                    int exp=0;
                                    System.out.println(e.toString());
                                    exp=this.ler.nextInt();
                                    this.ler.nextLine();
                                    if(exp==1){
                                        e.expedida();
                                        System.out.println("encomenda expedida com sucesso");
                                    }
                                }
                                
                            }
                        }
                    }
                    
                    else if(opt2==7){
                        System.out.println("DIGITE O NOME DA TRANSPORTADORA COM QUE PRETENDE EFETUAR A OPERACAO");
                        String nomet=this.ler.nextLine();
                        Transportador t = null;
                        try{
                            t=this.loja.procuraTransportador(nomet);
                            System.out.println("Carregue 1) se pretender entregar a encomenda");
                            for(Encomenda e : t.getEncomendas()){
                                if(e.getEstado().equals("expedida")){
                                    int ent=0;
                                    System.out.println(e.toString());
                                    ent=this.ler.nextInt();
                                    this.ler.nextLine();
                                    if(ent==1){
                                        this.loja.transpEntregaEnc(t, e);
                                        System.out.println("encomenda entregue com sucesso");
                                    }
                                }
                                
                            }
                        }
                        catch(NaoExisteTransportadorException e){
                            System.out.println(e.getMessage());
                        }
                    }
                    
                    else if(opt2==8){
                        System.out.println("DIGITE O NOME DA TRANSPORTADORA DE QUE PRETENDE VISUALIZAR AS ENCOMENDAS");
                        String nomet=this.ler.nextLine();
                        Transportador t = null;
                        try{
                            t=this.loja.procuraTransportador(nomet);
                            for(Encomenda e : t.getEncomendas()){
                                System.out.println(e.toString());
                            }
                        }
                        catch(NaoExisteTransportadorException e){
                            System.out.println(e.getMessage());
                        }
                    }
                    
                    else if(opt2==9){
                        if(this.loja.vazia()==false){
                            System.out.println(this.loja.userMaisVendas().toString());
                            System.out.println(String.valueOf(this.loja.precoMaisVendas()));
                        }
                    }
                    
                    else if(opt2==10){
                        System.out.println(this.loja.userMaisCompras().toString());
                        System.out.println();
                        System.out.println("VALOR GASTO EM COMPRAS");
                        System.out.println(String.valueOf(this.loja.userMaisCompras().quantoGastou()));
                        System.out.println();
                    }
                    
                    else if(opt2==11){
                        System.out.println(this.loja.transpMaisFatura().toString());
                        System.out.println();
                    }
                    
                    else if(opt2==12){
                        System.out.println();
                        System.out.println("VALOR TOTAL FATURADO PELA LOJA");
                        System.out.println(String.valueOf(this.loja.valorFaturado()));
                        System.out.println();
                    }
                    
                    else if(opt2==13){
                        System.out.println("IMPOSTO COBRADO A QUALQUER VENDA");
                        System.out.println(String.valueOf(this.loja.getImpostoVenda()));
                        System.out.println();
                        System.out.println("IMPOSTO COBRADA POR QUALQUER TRANSPORTE");
                        System.out.println(String.valueOf(this.loja.getImpostoTransp()));
                        System.out.println();
                    }
                }
            }
        }
        try
        {
            this.loja.gravaobj();
        }
        catch (IOException ioe)
        {
            ioe.printStackTrace();
        }
    }
    
    public void criasap(Utilizador u, Transportador t, Vintage loja){
        System.out.println("INSIRA A DESCRICAO DO PRODUTO");
        String descricao= this.ler.nextLine();
        System.out.println("POR QUANTOS DONOS PASSADOS ESTE ARTIGO JA PASSOU");
        int exdonos= this.ler.nextInt();
        System.out.println("INSIRA O PRECO BASE DO PRODUTO");
        double precobase=this.ler.nextDouble();
        System.out.println("AVALIACAO DO PRODUTO DE 0.0 ATE 1");
        double aval=this.ler.nextDouble();
        System.out.println("INSIRA O TAMANHO DA SAPATILHA");
        int tamanho= this.ler.nextInt();
        System.out.println("TEM ATACADORES(1) OU NAO(2)?");
        int tematacadores=this.ler.nextInt();
        boolean tipo;
        this.ler.nextLine();
        if(tematacadores==1){tipo=true;}
        else{tipo=false;}
        System.out.println("INSIRA A COR");
        String cor=this.ler.nextLine();
        System.out.println("INSIRA O ANO DA COLECAO");
        int ano = this.ler.nextInt();
        Sapatilha sap=new Sapatilha(loja.geraCodigo(), descricao, exdonos, precobase, aval, tamanho, tipo, cor, ano);
        System.out.println(sap.toString());
        u.avenda(sap, t);
    }
    
    public void criasapprem(Utilizador u, Transportador t, Vintage loja){
        System.out.println("INSIRA A DESCRICAO DO PRODUTO");
        String descricao= this.ler.nextLine();
        System.out.println("POR QUANTOS DONOS PASSADOS ESTE ARTIGO JA PASSOU");
        int exdonos= this.ler.nextInt();
        System.out.println("INSIRA O PRECO BASE DO PRODUTO");
        double precobase=this.ler.nextDouble();
        System.out.println("AVALIACAO DO PRODUTO DE 0.0 ATE 1");
        double aval=this.ler.nextDouble();
        System.out.println("INSIRA O TAMANHO DA SAPATILHA");
        int tamanho= this.ler.nextInt();
        System.out.println("TEM ATACADORES(1) OU NAO(2)?");
        int tematacadores=this.ler.nextInt();
        boolean tipo;
        this.ler.nextLine();
        if(tematacadores==1){tipo=true;}
        else{tipo=false;}
        System.out.println("INSIRA A COR");
        String cor=this.ler.nextLine();
        System.out.println("INSIRA O ANO DA COLECAO");
        int ano = this.ler.nextInt();
        this.ler.nextLine();
        System.out.println("INSIRA O AUTOR DA COLECAO");
        String autor = this.ler.nextLine();
        SapatilhaPremium sp=new SapatilhaPremium(this.loja.geraCodigo(), descricao, exdonos, precobase, aval, tamanho, tipo, cor, ano, autor);
        System.out.println(sp.toString());
        u.avenda(sp, t);
    }
    
    public void criatshirt(Utilizador u, Transportador t, Vintage loja){
        System.out.println("INSIRA A DESCRICAO DO PRODUTO");
        String descricao= this.ler.nextLine();
        System.out.println("POR QUANTOS DONOS PASSADOS ESTE ARTIGO JA PASSOU");
        int exdonos= this.ler.nextInt();
        System.out.println("INSIRA O PRECO BASE DO PRODUTO");
        double precobase=this.ler.nextDouble();
        System.out.println("AVALIACAO DO PRODUTO DE 0.0 ATE 1");
        double aval=this.ler.nextDouble();
        System.out.println("A TSHIRT É DO TAMANHO: 1)S  2)M  3)L  4)XL");
        int tam=this.ler.nextInt();
        String tamanho;
        if(tam==1)tamanho="S";
        else if(tam==2)tamanho="M";
        else if(tam==3)tamanho="L";
        else tamanho="XL";
        System.out.println("A TSHIRT TEM O PADRAO: 1)LISO  2)RISCAS  3)PALMEIRAS");
        int p=this.ler.nextInt();
        String pad;
        if(p==1)pad="liso";
        else if(p==2)pad="riscas";
        else pad="palmeiras";
        Tshirt shirt= new Tshirt(this.loja.geraCodigo(), descricao, exdonos, precobase, aval, tamanho, pad);
        u.avenda(shirt, t);
        System.out.println(shirt.toString());
    }
    
    public void criamala(Utilizador u, Transportador t, Vintage loja){
        System.out.println("INSIRA A DESCRICAO DO PRODUTO");
        String descricao= this.ler.nextLine();
        System.out.println("POR QUANTOS DONOS PASSADOS ESTE ARTIGO JA PASSOU");
        int exdonos= this.ler.nextInt();
        System.out.println("INSIRA O PRECO BASE DO PRODUTO");
        double precobase=this.ler.nextDouble();
        System.out.println("AVALIACAO DO PRODUTO DE 0.0 ATE 1");
        double aval=this.ler.nextDouble();
        System.out.println("INSIRA O VOLUME DA MALA EM CM3");
        double volume = this.ler.nextDouble();
        System.out.println("INSIRA O ANO DE LANCAMENTO DO ARTIGO");
        int ano=this.ler.nextInt();
        System.out.println("QUAL E O MATERIAL DA MALA:  1)abs  2)polipropileno  3)policarbonato  4)nylon  5)poliester");
        int m=this.ler.nextInt();
        this.ler.nextLine();
        String mat;
        if(m==1)mat="abs";
        else if(m==2)mat="polipropileno";
        else if(m==3)mat="policarbonato";
        else if(m==4)mat="nylon";
        else mat="poliester";
        Mala mal = new Mala(this.loja.geraCodigo(), descricao, exdonos, precobase, aval, volume, ano, mat);
        u.avenda(mal, t);
        System.out.println(mal.toString());
    }
    
    public void criamalaprem(Utilizador u, Transportador t, Vintage loja){
        System.out.println("INSIRA A DESCRICAO DO PRODUTO");
        String descricao= this.ler.nextLine();
        System.out.println("POR QUANTOS DONOS PASSADOS ESTE ARTIGO JA PASSOU");
        int exdonos= this.ler.nextInt();
        System.out.println("INSIRA O PRECO BASE DO PRODUTO");
        double precobase=this.ler.nextDouble();
        System.out.println("AVALIACAO DO PRODUTO DE 0.0 ATE 1");
        double aval=this.ler.nextDouble();
        System.out.println("INSIRA O VOLUME DA MALA EM CM3");
        double volume = this.ler.nextDouble();
        System.out.println("INSIRA O ANO DE LANCAMENTO DO ARTIGO");
        int ano=this.ler.nextInt();
        System.out.println("QUAL E O MATERIAL DA MALA:  1)abs  2)polipropileno  3)policarbonato  4)nylon  5)poliester");
        int m=this.ler.nextInt();
        this.ler.nextLine();
        String mat;
        if(m==1)mat="abs";
        else if(m==2)mat="polipropileno";
        else if(m==3)mat="policarbonato";
        else if(m==4)mat="nylon";
        else mat="poliester";
        System.out.println("QUAL E O AUTOR DA COLECAO?");
        String autor = this.ler.nextLine();
        MalaPremium malp = new MalaPremium(this.loja.geraCodigo(), descricao, exdonos, precobase, aval, volume, ano, mat, autor);
        u.avenda(malp, t);
        System.out.println(malp.toString());
    }
    
    public void criauser(Vintage loja){
        try{
            System.out.println("INSIRA O SEU EMAIL");
            String mail=this.ler.nextLine();
            System.out.println("INSIRA O SEU NOME");
            String nome=this.ler.nextLine();
            System.out.println("INSIRA A SUA MORADA");
            String morada=this.ler.nextLine();
            System.out.println("INSIRA O SEU NIF");
            int nif = this.ler.nextInt();
            Utilizador u = new Utilizador(this.loja.geraCodigo(), mail, nome, morada, nif);
            if(loja.jaExisteEmail(mail)==true){
                System.out.println("Email ja associado a uma conta");
            }
            else{
                this.loja.addUser(u);
            }
        }
        
    
        catch(UtilizadorJaExisteException e){
                System.out.println(e.getMessage());
        }
    
    }
    
    public void criatransportador(Vintage loja){
         System.out.println("INSIRA O NOME DA TRANSPORTADORA");
         String nome=this.ler.nextLine();
         System.out.println("INSIRA O VALOR PARA O PRECO DE ENCOMENDAS PEQUENAS");
         double pp=this.ler.nextDouble();
         this.ler.nextLine();
         System.out.println("INSIRA O VALOR PARA O PRECO DE ENCOMENDAS MEDIAS");
         double pm=this.ler.nextDouble();
         this.ler.nextLine();
         System.out.println("INSIRA O VALOR PARA O PRECO DE ENCOMENDAS GRANDES");
         double pg=this.ler.nextDouble();
         this.ler.nextLine();
         Transportador x = new Transportador(nome, pp, pm, pg, this.loja.getImpostoTransp());
         this.loja.addTransportador(x);
    }
    
}


