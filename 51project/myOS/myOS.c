#include <reg52.h>

/*=================================================
*自定义数据类型
=================================================*/
typedef unsigned char uchar;
typedef unsigned int uint;

#define LCD1602_DB P0	//LCD1602数据总线

sbit LCD1602_RS = P3^5;	 //RS端
sbit LCD1602_RW = P3^6;	 //RW端
sbit LCD1602_EN = P3^4;	 //EN端
sbit DU = P2^6;//
sbit WE = P2^7;//数码管位选段选用于关闭数码管显示

sbit key_s2 = P3^0;//S2按键位定义

/*=================================================
*函数名称：Read_Busy
*函数功能：判断1602液晶忙，并等待
=================================================*/
void Read_Busy()
{
	uchar busy;
	LCD1602_DB = 0xff;//复位数据总线
	LCD1602_RS = 0;	  //拉低RS
	LCD1602_RW = 1;	  //拉高RW读
	do
	{
		LCD1602_EN = 1;//使能EN
		busy = LCD1602_DB;//读回数据
		LCD1602_EN = 0;	 //拉低使能以便于下一次产生上升沿
	}while(busy & 0x80); //判断状态字BIT7位是否为1，为1则表示忙，程序等待
}
/*=================================================
*函数名称：LCD1602_Write_Cmd
*函数功能：写LCD1602命令
*调用：Read_Busy();
*输入：cmd:要写的命令
=================================================*/
void LCD1602_Write_Cmd(uchar cmd)
{
	Read_Busy();	 //判断忙，忙则等待
	LCD1602_RS = 0;
	LCD1602_RW = 0;	//拉低RS、RW操作时序情况1602课件下中文使用说明基本操作时序章节
	LCD1602_DB = cmd;//写入命令
	LCD1602_EN = 1;	 //拉高使能端 数据被传输到LCD1602内
	LCD1602_EN = 0;	 //拉低使能以便于下一次产生上升沿
}
/*=================================================
*函数名称：LCD1602_Write_Dat
*函数功能：写LCD1602数据
*调用：Read_Busy();
*输入：dat：需要写入的数据
=================================================*/
void LCD1602_Write_Dat(uchar dat)
{
	Read_Busy();
	LCD1602_RS = 1;
	LCD1602_RW = 0;
	LCD1602_DB = dat;
	LCD1602_EN = 1;
	LCD1602_EN = 0;
}
/*=================================================
*函数名称：LCD1602_Dis_OneChar
*函数功能：在指定位置显示一个字符
*调用：LCD1602_Write_Cmd(); LCD1602_Write_Dat();	
*输入：x：要显示的横坐标取值0-40，y：要显示的行坐标取值0-1（0为第一行,1为第二行）
		dat：需要显示的数据以ASCLL形式显示
=================================================*/
void LCD1602_Dis_OneChar(uchar x, uchar y,uchar dat)
{
	if(y)	x |= 0x40;   //11111111  0-7 第6位置1  y如果是1，就显示在第二行
	x |= 0x80;           //11111111  0-7 第7位置1  x代表列数
	LCD1602_Write_Cmd(x);
	LCD1602_Write_Dat(dat);		
}
/*=================================================
*函数名称：LCD1602_Dis_Str
*函数功能：在指定位置显示字符串
*调用：LCD1602_Write_Cmd(); LCD1602_Write_Dat();
*输入：x：要显示的横坐标取值0-40，y：要显示的行坐标取值0-1（0为第一行,1为第二行）
		*str：需要显示的字符串
=================================================*/
void LCD1602_Dis_Str(uchar x, uchar y, uchar *str)
{
	if(y) x |= 0x40;
	x |= 0x80;
	LCD1602_Write_Cmd(x);
	while(*str != '\0')
	{
		LCD1602_Write_Dat(*str++);
	}
}
/*=================================================
*函数名称：Init_LCD1602
*函数功能：1602初始化
*调用：	LCD1602_Write_Cmd();
=================================================*/
void Init_LCD1602()
{
	LCD1602_Write_Cmd(0x38); //	设置16*2显示，5*7点阵，8位数据接口
	LCD1602_Write_Cmd(0x0c); //开显示
	LCD1602_Write_Cmd(0x06); //读写一字节后地址指针加1
	LCD1602_Write_Cmd(0x01); //清除显示
}

void delay(uint z)
{
	uint x,y;
	for(x = z; x > 0; x--)
		for(y = 114; y > 0 ; y--);
}

void main()
{
	uchar TestStr[][17] = {{"Welcome to syfOS"},{"     Press S2 ~"},{"system info ~1"},{"wait ~2"}};
	uchar i=0;																		   
	DU = 0;
	WE = 0;//关闭数码管显示
	Init_LCD1602();//1602初始化
	LCD1602_Dis_Str(0, 0, &TestStr[i++]);	//显示字符串 ，对字符串取指针，传址，指向第一个字符
	LCD1602_Dis_Str(0, 1, &TestStr[i]);
//	LCD1602_Dis_OneChar(10, 1, 0xff); //显示一个黑方格
	while(1){
		if(key_s2 == 0)//判断S2是否按键
		{
			delay(5);//软件延时
			if(key_s2 == 0)
			{
				LCD1602_Write_Cmd(0x01); //显示清屏
				LCD1602_Dis_Str(0, 0, &TestStr[i]);
				i = (i==3)?0:i+1;
				LCD1602_Dis_Str(0, 1, &TestStr[i]);
			}
			while(!key_s2);	//松手检测
		}	
	};
}