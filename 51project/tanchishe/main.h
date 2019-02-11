#ifndef __MAIN_H__
#define __MAIN_H__

#include <reg52.h>
#include <intrins.h>

sbit LEDARRAY_LAT = P3^6;
sbit LEDARRAY_CLK = P3^5; 
sbit LEDARRAY_DI = P3^4;

//函数或者变量声明
extern void Delay_ms(unsigned int n);
extern void Send_Byte( unsigned char dat);

#endif