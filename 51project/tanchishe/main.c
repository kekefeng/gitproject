#include "main.h"
//�������
typedef unsigned int uint;
typedef unsigned char uchar;

uchar temp = 0xfe;
uchar KEY_NUM = 0;

//Keil C51�޷�ֱ��ʹ�ö�������
//uchar colselect[]={01111111};//,10111111),b(11011111),b(11101111),b(11110111),b(11111011),b(11111101),b(11111110)};

uchar colselect[]={0x7f,0xbf,0xdf,0xef,0xf7,0xfb,0xfd,0xfe};

sbit IR  = P3^2;     //��������������ݽӿ�	�ⲿ�ж�O�����

uchar IRtime;	  //���������ߵ͵�ƽ����ʱ��
uchar IRcord[4];  //���������4���ֽ�����
uchar IRdata[33]; //������ʼ�����ڵ�33λ����
bit IRpro_ok;	  //�����4���ֽ����ݽ�����ɱ�־λ
bit IRok;		  //33λ���ݽ�����ɱ�־
sbit BEEP = P2^3;
struct snake{
	uchar fangxiang;  //00 �� 01�� 10�� 11��
	uchar length;	  //��1��ʼ
	uchar position[10][2];  //�����10��; 0����col���꣬1����row���ꣻ
	void (*refreshpos)(uchar direction);
}mysnake;

	uchar food=0;
	uchar food_pos[2];
 //****************************************************
//����һ���ֽ�����
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
		
		LEDARRAY_CLK = 1;				//�����ط�������
		//				_nop_();
		LEDARRAY_CLK = 0;
		//				_nop_();
				
		dat >>= 1;			
	}		
}

 void display(uchar col,uchar row){	  //�����½ǵ�����Ϊ��1��1�� �ڵ�col�У���row�е�λ����ʾһ����
	uchar tempcol;
	uchar temprow;

	tempcol=colselect[col-1];
	temprow=~colselect[row-1];
	Send_Byte(tempcol);
	Send_Byte(temprow);

	LEDARRAY_LAT = 1;					//��������
	_nop_();
				
	LEDARRAY_LAT = 0;
	_nop_();		
}

void init()	   //��ʼ����ʱ��0 ���ⲿ�ж�0
{
	TMOD = 0x22; //��ʱ��0�Ͷ�ʱ��1������ʽ2��8λ�Զ���װ
	TH0 = 0x00;  //��8λװ��0��ô��ʱ�����һ�ε�ʱ����256����������
	TL0 = 0x00;
	EA = 1;      //���ж�
	ET0 = 1;	   //��ʱ��0�ж�
	TR0 = 1;     //������ʱ��0

	IT0 = 1;	   //�����ⲿ�ж�0Ϊ���ش�����ʽ����һ���½��ش���һ��
	EX0 = 1;	   //�����ⲿ�ж�0

}
void time0() interrupt 1   //���嶨ʱ��0
{
	IRtime++; 			   //�������1��Ϊ278us
}

void int0() interrupt 0	  		//�����ⲿ�ж�0
{
	static uchar i;	 			//	������̬�������������������ڻ���ִ�е�ʱ�򲻻ᶪʧ��ֵ��i���ڰ�33�θߵ�ƽ�ĳ���ʱ�����IRdata
	static bit startflag;		//��ʼ���������־λ
	if(startflag)	 			//��ʼ����������
	{
		if( (IRtime < 53) && (IRtime >= 32) ) /*�ж��Ƿ��������룬�׵�ƽ9000us+��4500us	
		����Լ�����������11.0592������NECЭ����������8000-10000+��4000-5000 
		����Ѿ���������������ôi���ᱻ��0�ͻῪʼ���δ�������*/
			i = 0;				 //�������������ôִ��i=0�����浽IRdata�ĵ�һ��λ
		IRdata[i] = IRtime;  		 //��T0������������������������ʱ��浽�������浽�����ж�
		IRtime = 0;				 //�������㣬��һ���½��ص�ʱ���ڴ�������
		i++; 					 //�����������Ĵ���
		if(i == 33) 				 //�������34�� ������±��Ǵ�0��ʼi����33��ʾִ����34��
		{
		 	IRok = 1;				 //��ô��ʾ���������
			i = 0; 				 //�������������׼���´δ���
		}
	}
	else		  
	{
		IRtime = 0; 				 //�����뿪ʼ���������������㿪ʼ����
		startflag = 1;			 //��ʼ�����־λ��1
	}
}


void IRcordpro()   				 //��ȡ����33������������ݽ���
{
	uchar i, j, k, cord, value;	/*i���ڴ���4���ֽڣ�j���ڴ���һ���ֽ���ÿһλ��k����33�������е���һλ
	cord����ȡ�������ʱ���ж��Ƿ����1������ʱ��*/
	k = 1; 						//�ӵ�һλ����ʼȡ����������������
	for(i = 0; i < 4; i++)
	{
		for(j = 0; j < 8; j++)
		{
			cord = IRdata[k];	    //���������cord
			if(cord > 5)	 		//������������11.0592��t0�����ΪԼ278us*5=1390��ô�ж�Ϊ1
			value = value | 0x80;	/*���յ�ʱ�����Ƚ������λ��
			�����λ�ȷŵ�value�����λ�ں�0x08��λ��һ��
			��������ı�valua������λ����ֵֻ���������λΪ1*/
			if(j < 7)
			{
				value = value >> 1;	//valueλ�������ν���8λ���ݡ�
			}
			k++;				//ÿִ��һ������λ��1
		}
		IRcord[i] = value;	   //ÿ������һ���ֽڰ�������IRcord�����С�
		value = 0; 			   //����value�����´��ڴ�������
	}
	IRpro_ok = 1;				   //������4���ֽں�IRpro ok��1��ʾ����������	
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
//������
//****************************************************
void main()
{
	uchar a; //50��������
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
//	uchar ifexist_snake[8]; //ÿһ��uchar��һ��
	create_food();
//	ifexist_snake[mysnake.position[0][0]-1] 

	TR0 = 1;//����T0
	TMOD = 0x01;//T0Ϊ��ʱ��������ģʽ1 16λ������
	TH0 = 0x4b;
	TL0 = 0xfc;//0x4bfc	��ʱ50ms
//	ram=rand()%60;
	init();
//	unsigned int i;
//	unsigned char j,k;
	while(1)
	{	j=0;
		k=0;


		if(IRok)    //�ж������Ƿ������                    
		{   
			IRcordpro();//������������4���ֽڵ�����
			IRok = 0;	//���µȴ�������
			if(IRpro_ok) //�ж��Ƿ�������  
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
		for(i=mysnake.length-1;i>0;i--){  //�ӵڶ��ڵ����һ��
			mysnake.position[i][0]=mysnake.position[i-1][0];
			mysnake.position[i][1]=mysnake.position[i-1][1];

		}
	
		switch(mysnake.fangxiang){
			case 0x00:	*pos=*(pos+2);*(pos+1)=*(pos+3)+1;break;
			case 0x01:	*pos=*(pos+2);*(pos+1)=*(pos+3)-1;break;
			case 0x10:	*pos=*(pos+2)+1;*(pos+1)=*(pos+3);break;
			case 0x11:	*pos=*(pos+2)-1;*(pos+1)=*(pos+3);break;
		}
 //�ı��ߵ�λ��

 		if(mysnake.position[0][0]==food_pos[0] && mysnake.position[0][1]==food_pos[1]){
			mysnake.position[mysnake.length][0]=lastsegment[0];
			mysnake.position[mysnake.length][1]=lastsegment[1];
			mysnake.length++;
			create_food();
			
		}

 		delay();

	}		
}

