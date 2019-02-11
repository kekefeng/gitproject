#include<reg52.h>
#define uchar unsigned char
#define uint unsigned int
sbit we = P2^7;	//�����λѡ
sbit du = P2^6;	//����ܶ�ѡ
sbit S2 = P3^0;//S2����λ����
sbit S3 = P3^1;
sbit S4 = P3^2;
sbit beep = P2^3; //������

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
                0x00,  //Ϩ��
                0x00  //�Զ���
 
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
	uchar sec_shi = sec / 10; //��ʾ��λ
	uchar sec_ge = sec % 10;	//��ʾʮλ
//	ge  = i % 10;//��ʾ��λ

	P0 = 0xff; //�������
	we = 1;//��λѡ
	P0 = 0xdf;//1101 1111 
	we = 0;	//�ر�λѡ

	du = 1;	//�򿪶�ѡ
	P0 = leddata[min_ge] + 0x80;
	du = 0;	//�رն�ѡ
	delay(5);//��ʱ5����

	P0 = 0xff; //�������
	we = 1;//��λѡ
	P0 = 0xef;//1110 1111 
	we = 0;	//�ر�λѡ

	du = 1;	//�򿪶�ѡ
	P0 = leddata[min_shi];
	du = 0;	//�رն�ѡ
	delay(5);//��ʱ5����
	
	P0 = 0xff;//�������
	we = 1;	//��λѡ
	P0 = 0xbf;//1011 1111  
	we = 0;	//�ر�λѡ
	
	du = 1;//�򿪶�ѡ
	P0 = leddata[sec_shi]; 
	du = 0;	//�رն�ѡ
	delay(5);//��ʱ5����
	
	P0 = 0xff;//�������
	we = 1;	//��λѡ
	P0 = 0x7f;// 0111 1111
	we = 0;	//�ر�λѡ
	
	du = 1;//�򿪶�ѡ
	P0 = leddata[sec_ge]; 
	du = 0;	//�رն�ѡ
	delay(5);//��ʱ5����	
}

void main()
{
	char sec,min,a;

	TMOD = 0x10;
	TL1 = 0xFD ;
	TH1 = 0x4B ;//���ó�ʼ��ʱ

	while(1)
	{
		if(S3 == 0)//�ж�S2�Ƿ񰴼�
		{
			delay(5);//�����ʱ
			if(S3 == 0)
			{
				TR1 = 0;
				min++; //������1
			}
			while(!S3) display(min,sec);//��wihleͣ����ߵ�ʱ���������Ȼ��ʾ����Ϊ��������du=0�����������������Ϣ����ʾ

			if(min == 10) //����ֵΪ����9ʱ����
			{
				min = 0;
			}
		}

		if(S4 == 0)//�ж�S2�Ƿ񰴼�
		{
			delay(5);//�����ʱ
			if(S4 == 0)
			{		
				TR1 = 0;
				sec += 10; //������10
			}
			while(!S4) display(min,sec);//��wihleͣ����ߵ�ʱ���������Ȼ��ʾ����Ϊ��������du=0�����������������Ϣ����ʾ

			if(sec > 60){
				sec -= 60;
				min++;
			}
		}

		if(S2 == 0){
			delay(5);//�����ʱ
			if(S2 == 0){			
				TR1 = 1; //������ʱ��
			}
			while(!S2);
		}

		if(TF1 == 1){
			TF1 = 0;
			TL1 = 0xFD ;
			TH1 = 0x4B ;
			a++;
			
			if(a == 20){  //1��
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