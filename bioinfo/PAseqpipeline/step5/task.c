#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#define max(a,b) ( ((a)>(b)) ? (a):(b) )
//usage : a.out mpileup fseq

typedef unsigned int uint;
typedef struct{
	char **str;
	short num; 
}IString;

typedef struct{
	char chr[8];
	long pos;
	long number; 
	char strand[2];
}Bedtool;

typedef struct{
	char chr[8];
	long forepos;
	long houpos;
}Fseq;

typedef struct{
	char chr[8];
	long forepos;
	long houpos;
	long raw_tag;
	long raw_length;
	long peak_location;
	long peak_tag;
	long mode_tag;
	float mode_percent;
	long mode_left_boundary;
	long mode_right_boundary;
	long mode_length;
	float all_tag_mean;
	float all_tag_sd;
	float all_loc_mean;
	float all_loc_sd;
}Out;

typedef struct{
	uint ** j_arr;
	short int num;
}Itotal;

void sumcount(Out* ,Itotal,Bedtool*);

int Split(char *src,char *delim,IString* istr){
	int i;
	char *str=NULL,*p=NULL;
	
	istr->num=1;
	str=(char *)calloc(strlen(src)+1,sizeof(char)); //str是一个地址，用来存放src字符串
	if(str==NULL) return 0;
	istr->str=(char **)calloc(1,sizeof(char *)); //istr->str是一个二级指针
	strcpy(str,src);
	
	p=strtok(str,delim);
	istr->str[0] = (char*)calloc(strlen(p)+1,sizeof(char)); //calloc分配一块地址，istr->str[0]
	if(istr->str[0]==NULL)return 0;
	strcpy(istr->str[0],p);
	for(i=1;p=strtok(NULL,delim);i++){
		istr->num++;
		istr->str=(char**)realloc(istr->str,(i+1)*sizeof(char*));
		if(istr->str==NULL) return 0;
		istr->str[i]=(char*)calloc(strlen(p)+1,sizeof(char));
		if(istr->str[0]==NULL) return 0;
		strcpy(istr->str[i],p);
	}
	free(str);
	str=p=NULL;

	return 1;
}

void readfseq(char *filename,Fseq * fseq,long row){
	FILE *fp;
	long i;
	char line[100];
	char delim[]="\t";
	IString mystring;
    if((fp=fopen(filename,"r"))==NULL){
        printf("can't open %s\n",filename);
        exit(EXIT_FAILURE);
    }
	for(i=0;i<row;i++){
		fgets(line,100,fp);
		Split(line,delim,&mystring);
		strcpy(fseq->chr,mystring.str[0]);
		fseq->forepos=atoi(mystring.str[1]);
		fseq->houpos=atoi(mystring.str[2]);
		fseq++;
	}
}


void readbedtools(char *filename,Bedtool * bedtool,long row){
	FILE *fp;
	long i;
	char line[800];
	char delim[]="\t";
	IString mystring;
    if((fp=fopen(filename,"r"))==NULL){
        printf("can't open %s\n",filename);
        exit(EXIT_FAILURE);
    }

	for(i=0;i<row;i++){
		fgets(line,800,fp);
//		fputs(line,stdout);
		Split(line,delim,&mystring);
		strcpy(bedtool->chr,mystring.str[0]);
		bedtool->pos=atoi(mystring.str[1]);
		bedtool->number=atoi(mystring.str[2]);
//		strcpy(bedtool->strand,mystring.str[3]);
		bedtool++;
	}
	return;
}

int main(int argc,char** argv){ //argc:number of arg +1,argv[0]:name of exe.
	long N=atoi(argv[1]);
	long N_fseq=atoi(argv[2]);
	long i,j=0;
	
	Bedtool * bed_fp;
	bed_fp=(Bedtool *)malloc(N * sizeof(Bedtool));
	if(bed_fp==NULL){
		printf("not such a big area of %ld\n",N);
		return 0;
	}
	const Bedtool * const bed_ini=bed_fp;
	const Bedtool * bed_acc=bed_ini;

	
	Fseq * fseq_fp;
	const Fseq * fseq_ini;
	fseq_fp=(Fseq *)malloc(N_fseq * sizeof(Fseq));
	if(fseq_fp==NULL){
		printf("not such a big area of %ld\n",N_fseq);
		return 0;
	}
	fseq_ini=fseq_fp;
	
	Out out={"0",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	Out *out_pt;
	uint raw_left_boundary,raw_right_boundary;
	Itotal itotal;
	int k;

	readbedtools(argv[3],bed_fp,N);
//	printf("%d\n",bed_ini->pos);

	readfseq(argv[4],fseq_fp,N_fseq);
//	printf("%d\n",fseq_ini->forepos);

	for(i=0;i<N_fseq;i++){
//		printf("%ld\t",i+1);
//		strcpy(out.chr,fseq_ini->chr);
//		out.forepos=fseq_ini->forepos;
//		out.houpos=fseq_ini->houpos;
		
		raw_left_boundary=fseq_ini->forepos+1;
		raw_right_boundary=fseq_ini->houpos;
//		out.raw_length=raw_right_boundary-raw_left_boundary+1;
		
		itotal.j_arr=(uint **)calloc(1,sizeof(int *));
		itotal.num=0;
		
		for(;j<N;j++,bed_acc++){
			if(strcmp(bed_acc->chr,fseq_ini->chr)==0){
				if(bed_acc->pos >= raw_left_boundary){
					if(bed_acc->pos <= raw_right_boundary){
						printf("%ld\t",j+1);
					}else{
						break;  //当mpileup大于fseq时，后面的行数也不可能和这个fseq匹配了。进行下一个fseq的匹配,j和bed_acc不变，因为有可能和下一个fseq匹配上
								//有可能j没有到末尾整个大循环就结束
								//也有可能j到了末尾整个循环结束。
					}
				}else{
					continue;  //如果mpileup最小的pos小于最小的fseq.继续。fseq不变
				}
			}else{
				continue;  //如果染色体不同，j++，bed_acc++
			}
//				itotal.num ++;
//				itotal.j_arr=(uint **)realloc(itotal.j_arr,(itotal.num)*sizeof(uint *));
//				itotal.j_arr[itotal.num-1]=(uint *)calloc(1,sizeof(uint));
			
		}
		printf("\n");
		fseq_ini++;
	}
}
		//itotal.j_arr[]存储了这次循环中j的数值
/*
		if(itotal.num == 0){
			1;
		}else if(itotal.num == 1){
			int j = *(itotal.j_arr[0]);
			out.raw_tag=bedtools[j].number;
			out.peak_location=bedtools[j].pos;
			out.peak_tag=out.raw_tag;
			out.mode_tag=out.raw_tag;
			out.mode_percent=1;
			out.mode_left_boundary=out.peak_location;
			out.mode_right_boundary=out.peak_location;
			out.mode_length=1;
			out.all_tag_mean=out.raw_tag;
			out.all_loc_mean=out.raw_tag;
		}else{
			out_pt=&out;
			sumcount(out_pt,itotal,&bedtools[0]);
			for(int i=0;i<Itotal->num-1;i++){
				if(i>0 && (sum()*100/out.raw_tag)>5) break;  //?
				for(int k=Itotal->num-1;k>p;k--){
					
				}
			}
		}
		
	}
}


long sum(Itotal itotal,Bedtool *bedtools,int start,int end){
	long he=0;
	for(int i=start;i<end;i++){
		he=he+*(bedtools+*(itotal.j_arr[i])).number;
	}
	return he;
}

void sumcount(Out* out_pt,Itotal itotal,Bedtool *bedtools){
	long total;
	uint j_peak_pos;
	uint j_peak=0;
	uint j;
	short number=0;
	for (int i=0;i<itotal.num;i++){
		j=*(itotal.j_arr[i]);
		number=*(bedtools+j).number;
		total=total+number;   //raw_tag
		if (j_peak <= number){
			j_peak=number;
			j_peak_pos=j;
		}
	}
	out_pt->raw_tag			=total;
	out_pt->peak_location	=j_peak_pos;
	out_pt->peak_tag		=j_peak;
}
*/
