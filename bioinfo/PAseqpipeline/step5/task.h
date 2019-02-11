typedef unsigned int uint;
typedef struct{
	char **str;
	short int num; 
}IString;

typedef struct{
	char chr[8];
	unsigned int pos;
	short int number; 
	char strand[2];
}Bedtool;

typedef struct{
	char chr[8];
	unsigned int forepos;
	unsigned int houpos;
}Fseq;

typedef struct{
	char chr[8];
	uint forepos;
	uint houpos;
	uint raw_tag;
	uint raw_length;
	uint peak_location;
	uint peak_tag;
	uint mode_tag;
	float mode_percent;
	uint mode_left_boundary;
	uint mode_right_boundary;
	uint mode_length;
	float all_tag_mean;
	float all_tag_sd;
	float all_loc_mean;
	float all_loc_sd;
}Out;
