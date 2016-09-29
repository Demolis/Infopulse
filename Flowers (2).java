import java.util.Scanner;
public class Flowers {

	public static void main(String[] args) {
		Acces[] ac = new Acces[4];
		ac[0] = new Acces("Ћенточка", 3);
		ac[1] = new Acces("ќткрытка", 20);
		ac[2] = new Acces("Ўарик", 30);
		ac[3] = new Acces("Ѕумажна€ обертка", 5);
		Flower[] buk = new Flower[10];
		buk[0] = new DeathFlower("Ѕумага", 8, 6);
		buk[1] = new DeathFlower("“кань", 15, 10);
		buk[2] = new Rose(11,-920, 15, true);
		buk[3] = new Rose(10, 40, 15, true);
		buk[4] = new Rose(8, 35, 13, false);
		buk[5] = new Rose(9, 37, 14, false);
		buk[6] = new Romashki(9, 20, 5, 11);
		buk[7] = new Romashki(1, 5, 5, 3);//дохла€ ромашка
		buk[8] = new Romashki(8, 17, 6, 14);
		buk[9] = new Romashki(6, 15, 6, 10);
		Bouqet b = new Bouqet();
		b.f=buk;
		b.a=ac;
		b.f=b.Sort();
		//b.PrintFlow();
		
		Bouqet b1 = new Bouqet();
		b1.f = buk;
		b1.f=b.NewBou(7);
		b1.PrintFlow();
	}
}
class Acces{
	String name;
	int price;
	public Acces(String n, int p){
		name = n;
		price = p;
	}
}
class Flower{
	private int price;
	int stebel;//äëèíà ñòåáëÿ
	public Flower(int p, int s){
		this.setPrice(p);
		stebel = s;
	}
	public int getPrice(){
		return price;		
	}
	public void setPrice(int price){
		if(price>0){
			this.price = price;
		}else{
			this.price = 0;
		}
	}
	public int Price(int p){
		int pr = 0;
		pr+=price;
		return pr;
	}
}
class LiveFlower extends Flower{
	private int svezh;//ñâåæåñòü
	public LiveFlower(int sv, int p, int s){
		super(p,s);
		this.setSvezh(sv);	
	}
	public int getSvezh(){
		return svezh;
	}
	public void setSvezh(int svezh){
		if(svezh<0&&svezh>10){
			this.svezh = 0;
		}else{
			this.svezh = svezh;
		}
	}
	
}
class DeathFlower extends Flower{
	String material;
	public DeathFlower(String m, int p, int s){
		super(p,s);
		material = m;
	}
}
class Rose extends LiveFlower{
	boolean shbIp;//øûïû 
	public Rose(int sv, int p, int s, boolean sh){
		super(sv, p, s);
		shbIp = sh;
	}
}
class Romashki extends LiveFlower{
	int lepestok;//êîëèåñòâî ëåïåñòêîâ
	public Romashki(int sv, int p, int s, int l){
		super(sv, p, s);
		lepestok = l;
	}
}

class Bouqet{
   Flower[] f;
   Acces[] a;
   
   public Flower[] SearchFlowerByStebel(int s1, int s2){
	   int count = 0;
		for(int i = 0; i<f.length; i++){//поиск кол цветов по заданному диапазону
			if(f[i].stebel>=s1&&f[i].stebel<=s2){
			count++;
			}
		}
		Flower mas[] = new Flower[count-1];
		int l = 0;
		for(int i = 0; i<f.length; i++){
			if(f[i].stebel>=s1&&f[i].stebel<=s2){
				mas[l] = f[i];
				l++;
				}
		}
		return mas;
	}
   public int Price(){
		int pr = 0;
		int prF = 0;
		int prA = 0;
		for(int i = 0; i<f.length;i++){
			prF +=f[i].getPrice();
		}
		for(int i = 0; i<a.length;i++){
			prA +=a[i].price;
		}
		pr = prF+prA;
		return pr;
	}

   public Flower[] Sort(){
	   Flower [] abc = new Flower[f.length];
	   int count = 0;//дл€ неживих
	   int count1 = 0;
	   int nonc = f.length;
	   for(int i = 0; i<f.length; i++){
		   if(f[i] instanceof LiveFlower){
			   abc[count1]=f[i];
			   count1++;
		   }else if(f[i] instanceof DeathFlower){
			   abc[nonc-1]=f[i];
			   nonc--;
			   count++;
		   }
	   }
		for(int i = 1; i<abc.length-count; i++)
			for(int j=abc.length-1-count; j>=i; j--) {
				if(((LiveFlower)abc[j-1]).getSvezh()<((LiveFlower)abc[j]).getSvezh()) {
					LiveFlower t =(LiveFlower) abc[j-1];
					abc[j-1]=abc[j];
					abc[j]=t;
				}
			}
		return abc;
	}
   public void PrintAcc(){
	   for(int i = 0; i<a.length; i++){
		   System.out.println(a[i].name+" "+a[i].price);
		}
   }
   public void PrintFlow(){
	   for(int i = 0; i<f.length; i++){
		   if(f[i] instanceof DeathFlower){
			   System.out.println(((DeathFlower)f[i]).material+" . ≈го цена "+f[i].getPrice());
		   }else if(f[i] instanceof Rose){
			   System.out.println("–оза свежести "+((Rose)f[i]).getSvezh()+" . ≈го цена "+f[i].getPrice());
		   }else if(f[i] instanceof Romashki){
			   System.out.println("–омашки свежести " +((Romashki)f[i]).getSvezh()+" . ≈го цена "+f[i].getPrice());
		   }
	   }
   }
   public Flower[] NewBou(int n){
	   Flower [] bouq = new Flower[n];
	   if(n>2&&n<f.length){
		   for(int i = 0; i<n; i++){
			   int b = 0;
			   b = (int)(Math.random()*100);
			   for(int j = 0; j<f.length; j++){
				    if((b>0&&b<6)&&(f[j] instanceof DeathFlower)&&f[j]!=null){
						   bouq[i] = f[j];
						 f[j] = null;//обнул€ю ссылку что бы не было повторов
			             break;
				    }else if((b>5&&b<36)&&(f[j] instanceof Romashki)&&f[j]!=null){
						   bouq[i] = f[j];
						  f[j] = null;
						  break;
			   }else if((b>35&&b<101)&&(f[j] instanceof Rose)&&f[j]!=null){
						   bouq[i] = f[j];
						   f[j] = null;
						   break;
					   }
				   }
			   }
		   }
	   return bouq;
   }
}
