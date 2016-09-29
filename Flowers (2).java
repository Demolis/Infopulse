
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
	int stebel;//длина стебля
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
	private int svezh;//свежесть
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
	boolean shbIp;//шыпы 
	public Rose(int sv, int p, int s, boolean sh){
		super(sv, p, s);
		shbIp = sh;
	}
}
class Romashki extends LiveFlower{
	int lepestok;//колиество лепестков
	public Romashki(int sv, int p, int s, int l){
		super(sv, p, s);
		lepestok = l;
	}
}
