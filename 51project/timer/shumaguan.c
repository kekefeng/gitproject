#include<reg52.h>
#define uchar unsigned char
#define uint unsigned int
sbit we = P2^7;	//数码管位选
sbit du = P2^6;	//数码管段选
sbit S2 = P3^0;//S2按键位定义
sbit S3 = P3^1;
sbit S4 = P3^2;
sbit beep = P2^3; //蜂鸣器

uchar code leddata[]={ 
 
                0x3F,  //"0"
                0x06,  //"1"
                0x5B,  //"2"
                0x4F,  //"3"
                0x66,  //"4"
                0x6D,  //"5"
                0x7D,  //"6"
                0x07,  //"7"
                0x7F,  //"8"
                0x6F,  //"9"
                0x77,  //"A"
                0x7C,  //"B"
                0x39,  //"C"
                0x5E,  //"D"
                0x79,  //"E"
                0x71,  //"F"
                0x76,  //"H"
                0x38,  //"L"
                0x37,  //"n"
                0x3E,  //"u"
                0x73,  //"P"
                0x5C,  //"o"
                0x40,  //"-"
                0x00,  //熄灭
                0x00  //自定义
 
                         };

void delay(uint z)
{
	uint x,y;
	for(x = z; x > 0; x--)
		for(y = 114; y > 0 ; y--);
}

void display(uchar min,uchar sec)
{
	uchar min_shi = min / 10;
	uchar min_ge = min % 10;
	uchar sec_shi = sec / 10; //显示百位
	uchar sec_ge = sec % 10;	//显示十位
//	ge  = i % 10;//显示个位

	P0 = 0xff; //清除断码
	we = 1;//打开位选
	P0 = 0xdf;//1101 1111 
	we = 0;	//关闭位选

	du = 1;	//打开段选
	P0 = leddata[min_ge] + 0x80;
	du = 0;	//关闭段选
	delay(5);//延时5毫秒

	P0 = 0xff; //清除断码
	we = 1;//打开位选
	P0 = 0xef;//1110 1111 
	we = 0;	//关闭位选

	du = 1;	//打开段选
	P0 = leddata[min_shi];
	du = 0;	//关闭段选
	delay(5);//延时5毫秒
	
	P0 = 0xff;//清除断码
	we = 1;	//打开位选
	P0 = 0xbf;//1011 1111  
	we = 0;	//关闭位选
	
	du = 1;//打开段选
	P0 = leddata[sec_shi]; 
	du = 0;	//关闭段选
	delay(5);//延时5毫秒
	
	P0 = 0xff;//清除断码
	we = 1;	//打开位选
	P0 = 0x7f;// 0111 1111
	we = 0;	//关闭位选
	
	du = 1;//打开段选
	P0 = leddata[sec_ge]; 
	du = 0;	//关闭段选
	delay(5);//延时5毫秒	
}

void main()
{
	char sec,min,a;

	TMOD = 0x10;
	TL1 = 0xFD ;
	TH1 = 0x4B ;//设置初始计时

	while(1)
	{
		if(S3 == 0)//判断S2是否按键
		{
			delay(5);//软件延时
			if(S3 == 0)
			{
				TR1 = 0;
				min++; //计数加1
			}
			while(!S3) display(min,sec);//当wihle停在这边的时候，数码管依然显示是因为段锁存器du=0，依靠锁存器存的信息来显示

			if(min == 10) //当数值为超过9时归零
			{
				min = 0;
			}
		}

		if(S4 == 0)//判断S2是否按键
		{
			delay(5);//软件延时
			if(S4 == 0)
			{		
				TR1 = 0;
				sec += 10; //计数加10
			}
			while(!S4) display(min,sec);//当wihle停在这边的时候，数码管依然显示是因为段锁存器du=0，依靠锁存器存的信息来显示

			if(sec > 60){
				sec -= 60;
				min++;
			}
		}

		if(S2 == 0){
			delay(5);//软件延时
			if(S2 == 0){			
				TR1 = 1; //开启计时器
			}
			while(!S2);
		}

		if(TF1 == 1){
			TF1 = 0;
			TL1 = 0xFD ;
			TH1 = 0x4B ;
			a++;
			
			if(a == 20){  //1秒
				a = 0;
				sec --;
				
				if(sec < 0){
					if(min == 0){
						beep = 0;
						while(1){
							display(0,0);
						};
					}else{
						min --;
						sec = 59;
					}
				}
			}	
		}

		display(min,sec);			
	}
}