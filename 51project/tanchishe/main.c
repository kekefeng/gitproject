#include "main.h"
//定义变量
typedef unsigned int uint;
typedef unsigned char uchar;

uchar temp = 0xfe;
uchar KEY_NUM = 0;

//Keil C51无法直接使用二进制数
//uchar colselect[]={01111111};//,10111111),b(11011111),b(11101111),b(11110111),b(11111011),b(11111101),b(11111110)};

uchar colselect[]={0x7f,0xbf,0xdf,0xef,0xf7,0xfb,0xfd,0xfe};

sbit IR  = P3^2;     //定义红外脉冲数据接口	外部中断O输入口

uchar IRtime;	  //储存检测红外高低电平持续时间
uchar IRcord[4];  //储存解码后的4个字节数据
uchar IRdata[33]; //包含起始码在内的33位数据
bit IRpro_ok;	  //解码后4个字节数据接收完成标志位
bit IRok;		  //33位数据接收完成标志
sbit BEEP = P2^3;
struct snake{
	uchar fangxiang;  //00 上 01下 10左 11右
	uchar length;	  //从1开始
	uchar position[10][2];  //蛇最长有10节; 0代表col坐标，1代表row坐标；
	void (*refreshpos)(uchar direction);
}mysnake;

	uchar food=0;
	uchar food_pos[2];
 //****************************************************
//发送一个字节数据
//****************************************************
void Send_Byte( unsigned char dat)
{
	unsigned char i;
	LEDARRAY_CLK = 0;
	_nop_();	
	LEDARRAY_LAT = 0;
	_nop_();

	for( i = 0 ; i < 8 ; i++ )
	{
		if( dat&0x01 )
		{
			LEDARRAY_DI = 1;	
		}
		else
		{
			LEDARRAY_DI = 0;
		}
		
		LEDARRAY_CLK = 1;				//上升沿发送数据
		//				_nop_();
		LEDARRAY_CLK = 0;
		//				_nop_();
				
		dat >>= 1;			
	}		
}

 void display(uchar col,uchar row){	  //最左下角的坐标为（1，1） 在第col列，第row行的位置显示一个点
	uchar tempcol;
	uchar temprow;

	tempcol=colselect[col-1];
	temprow=~colselect[row-1];
	Send_Byte(tempcol);
	Send_Byte(temprow);

	LEDARRAY_LAT = 1;					//锁存数据
	_nop_();
				
	LEDARRAY_LAT = 0;
	_nop_();		
}

void init()	   //初始化定时器0 和外部中断0
{
	TMOD = 0x22; //定时器0和定时器1工作方式2，8位自动重装
	TH0 = 0x00;  //高8位装入0那么定时器溢出一次的时间是256个机器周期
	TL0 = 0x00;
	EA = 1;      //总中断
	ET0 = 1;	   //定时器0中断
	TR0 = 1;     //启动定时器0

	IT0 = 1;	   //设置外部中断0为跳沿触发方式，来一个下降沿触发一次
	EX0 = 1;	   //启动外部中断0

}
void time0() interrupt 1   //定义定时器0
{
	IRtime++; 			   //检测脉宽，1次为278us
}

void int0() interrupt 0	  		//定义外部中断0
{
	static uchar i;	 			//	声明静态变量（在跳出函数后在回来执行的时候不会丢失数值）i用于把33次高电平的持续时间存入IRdata
	static bit startflag;		//开始储存脉宽标志位
	if(startflag)	 			//开始接收脉宽检测
	{
		if( (IRtime < 53) && (IRtime >= 32) ) /*判断是否是引导码，底电平9000us+高4500us	
		这个自己可以算我以11.0592来算了NEC协议的引导码低8000-10000+高4000-5000 
		如果已经接收了引导码那么i不会被置0就会开始依次存入脉宽*/
			i = 0;				 //如果是引导码那么执行i=0把他存到IRdata的第一个位
		IRdata[i] = IRtime;  		 //以T0的溢出次数来计算脉宽，把这个时间存到数组里面到后面判断
		IRtime = 0;				 //计数清零，下一个下降沿的时候在存入脉宽
		i++; 					 //计数脉宽存入的次数
		if(i == 33) 				 //如果存入34次 数组的下标是从0开始i等于33表示执行了34次
		{
		 	IRok = 1;				 //那么表示脉宽检测完毕
			i = 0; 				 //把脉宽计数清零准备下次存入
		}
	}
	else		  
	{
		IRtime = 0; 				 //引导码开始进入把脉宽计数清零开始计数
		startflag = 1;			 //开始处理标志位置1
	}
}


void IRcordpro()   				 //提取它的33次脉宽进行数据解码
{
	uchar i, j, k, cord, value;	/*i用于处理4个字节，j用于处理一个字节中每一位，k用于33次脉宽中的哪一位
	cord用于取出脉宽的时间判断是否符合1的脉宽时间*/
	k = 1; 						//从第一位脉宽开始取，丢弃引导码脉宽
	for(i = 0; i < 4; i++)
	{
		for(j = 0; j < 8; j++)
		{
			cord = IRdata[k];	    //把脉宽存入cord
			if(cord > 5)	 		//如果脉宽大于我11.0592的t0溢出率为约278us*5=1390那么判断为1
			value = value | 0x80;	/*接收的时候是先接收最低位，
			把最低位先放到value的最高位在和0x08按位或一下
			这样不会改变valua的其他位的数值只会让他最高位为1*/
			if(j < 7)
			{
				value = value >> 1;	//value位左移依次接收8位数据。
			}
			k++;				//每执行一次脉宽位加1
		}
		IRcord[i] = value;	   //每处理完一个字节把它放入IRcord数组中。
		value = 0; 			   //清零value方便下次在存入数据
	}
	IRpro_ok = 1;				   //接收完4个字节后IRpro ok置1表示红外解码完成	
}

void delay()
{
	uchar k;
	uint i=0;
	P1=0xfd;
	while(1){
			for(k=0;k<mysnake.length;k++){
				i++;
				display(mysnake.position[k][0],mysnake.position[k][1]);
			}
			display(food_pos[0],food_pos[1]);
			if(i>1500) return;
	}
	P1=0xff;
}

void create_food(){
	uchar i,j,k;   //i=col,j=row
	uchar ram;
	for(i=1;i<=8;i++){
		for(j=1;j<=8;j++){
			for(k=0;k<mysnake.length;k++){
				if(i!=mysnake.position[k][0] && j!=mysnake.position[k][1]){
					ram=rand()%50;
					if(ram>48){
						food_pos[0]=i;
						food_pos[1]=j;
						return;
					}
				}
			}
		}
	}
	create_food();
}


//****************************************************
//主函数
//****************************************************
void main()
{
	uchar a; //50次数计数
	uchar i,j,k;
	uchar data *pos;
	uchar lastsegment[2];

	mysnake.fangxiang=0x00;
	mysnake.length=2;
	mysnake.position[0][0]=4;	//col
	mysnake.position[0][1]=2;	//row
	mysnake.position[1][0]=4;
	mysnake.position[1][1]=1;
	pos=&mysnake.position[0][0];
//	uchar ifexist_snake[8]; //每一个uchar是一行
	create_food();
//	ifexist_snake[mysnake.position[0][0]-1] 

	TR0 = 1;//启动T0
	TMOD = 0x01;//T0为定时器，工作模式1 16位计数器
	TH0 = 0x4b;
	TL0 = 0xfc;//0x4bfc	定时50ms
//	ram=rand()%60;
	init();
//	unsigned int i;
//	unsigned char j,k;
	while(1)
	{	j=0;
		k=0;


		if(IRok)    //判断脉宽是否检测完毕                    
		{   
			IRcordpro();//根据脉宽解码出4个字节的数据
			IRok = 0;	//重新等待脉宽检测
			if(IRpro_ok) //判断是否解码完毕  
			{
 				P1=0xfe;
				
 				switch(IRcord[2]){
					case 0x18:mysnake.fangxiang=0x00;break;	//shang
					case 0x5a:mysnake.fangxiang=0x10;break;	 //zuo
					case 0x52:mysnake.fangxiang=0x01;break;
					case 0x08:mysnake.fangxiang=0x11;break;
				}
				IRpro_ok = 0;
				P1=0xff;
			}
		}
			
//		mysnake.refreshpos(mysnake);
//		display_bycolandrow(mysnake.position[0][0],mysnake.position[0][1]);

		lastsegment[0]=mysnake.position[mysnake.length-1][0];
		lastsegment[1]=mysnake.position[mysnake.length-1][1];
		for(i=mysnake.length-1;i>0;i--){  //从第二节到最后一节
			mysnake.position[i][0]=mysnake.position[i-1][0];
			mysnake.position[i][1]=mysnake.position[i-1][1];

		}
	
		switch(mysnake.fangxiang){
			case 0x00:	*pos=*(pos+2);*(pos+1)=*(pos+3)+1;break;
			case 0x01:	*pos=*(pos+2);*(pos+1)=*(pos+3)-1;break;
			case 0x10:	*pos=*(pos+2)+1;*(pos+1)=*(pos+3);break;
			case 0x11:	*pos=*(pos+2)-1;*(pos+1)=*(pos+3);break;
		}
 //改变蛇的位置

 		if(mysnake.position[0][0]==food_pos[0] && mysnake.position[0][1]==food_pos[1]){
			mysnake.position[mysnake.length][0]=lastsegment[0];
			mysnake.position[mysnake.length][1]=lastsegment[1];
			mysnake.length++;
			create_food();
			
		}

 		delay();

	}		
}

