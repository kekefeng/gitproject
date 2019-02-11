#利用fseq生成的peak位置信息，到mpileup中取寻找对应的tag数（不使用fseq最后一列的信息！）

mpileup <- read.table("MEF_minus_R1_PA_site_count_on.genome.txt",sep="\t",header=FALSE)
fseq <- read.table("MEF_minus_fseq.npf",sep="\t",header=FALSE)
fseq <- fseq[which(fseq[,1]==1),]  #取出chr1！ 这个脚本只处理chr1 peak的情况，所以这里 ==1
peak_mode <- c()
for (i in 1:dim(fseq)[1]){  #从 1到fseq的行数，循环对每个chr1 的peak进行处理
	raw_tag <- 0
	raw_length <- 0
	peak_location <- 0
	peak_tag <- 0
	mode_tag <- 0
	mode_percent <- 0
	mode_left_boundary <- 0
	mode_right_boundary <- 0
	mode_length <- 0
	all_tag_mean <-0
	all_tag_sd <- 0
	all_loc_mean <- 0
	all_loc_sd <- 0  # 这13个值用于最终输出 ,初始化
	
	raw_left_boundary <- as.numeric(fseq[i,2])+1
	raw_right_boundary <- as.numeric(fseq[i,3])
	raw_length <- raw_right_boundary-raw_left_boundary+1  #这三行为了得到raw_length（用于最终输出）

	multibase_pos_count <- c()
	multibase_pos_count <- mpileup[which( (as.numeric(mpileup[,1])==as.numeric(fseq[i,1])) & (as.numeric(mpileup[,2])>=raw_left_boundary) & (as.numeric(mpileup[,2])<=raw_right_boundary) ),c(2,4)]  
	#做位与运算
	#也就是说，需要同时满足下列三个条件的行才被选中：
	#1.染色体位置相同  已改为 as.character
	#2.位置大于fseq起始位置
	#3.位置小于fseq结束位置
	#提取出mpileup第二和第四列信息（第二列：位置，第四列：read数）。
	#一个截短了的mpileup数据框
	if (dim(multibase_pos_count)[1]==0) #这个截短了的数据框行数为0 ，每一行是一个peak点
	{
	}                                #跳到92行
	else if (dim(multibase_pos_count)[1]==1) #这个截短了的数据框行数为1  ，这个范围只有一个peak
	{
		raw_tag <- multibase_pos_count$count  # 该位点的read数目信息 ；只有一行
		peak_location <- multibase_pos_count$pos # 碱基位点
		peak_tag <- raw_tag
		mode_tag <- raw_tag
		mode_percent <- 1
		mode_left_boundary <- multibase_pos_count$pos
		mode_right_boundary <- multibase_pos_count$pos
		mode_length <- 1
		all_tag_mean <-raw_tag
		all_tag_sd <- 0
		all_loc_mean <- raw_tag
		all_loc_sd <- 0
	}else{                                 #这个截短了的数据框行数大于1 ，说明这个范围内有两个或者多个peak
		raw_tag <- sum(multibase_pos_count$count)    #
		peak_location <- multibase_pos_count[which(multibase_pos_count$count==max(multibase_pos_count$count) ) [1],"pos"]   #最高峰所在位置
		peak_tag <- multibase_pos_count[which(multibase_pos_count$count==max(multibase_pos_count$count))[1],"count"]  #最高峰read数目
		candidate_mode <- c()
		for (p in 1:(dim(multibase_pos_count)[1]-1)){      #找出占95%的区间
#如果总共有10行，p从1到9循环
			if (p>1 & (sum(multibase_pos_count[1:(p-1),2])/raw_tag)>0.05){
				break
			#假设p已经循环到3了，这时发现从1-3的所有tag数加起来占的比例已经大于0.05.就返回
			}else{
				for (k in dim(multibase_pos_count)[1]:p)
				{
				#p=1，k从row,row-1,row-2一直到1
				#p=2，k从row，row-1...一直到2
					mode_tag <- sum(multibase_pos_count[p:k,2])
					mode_percent <-  mode_tag/raw_tag
					mode_left_boundary <- multibase_pos_count[p,1]
					mode_right_boundary <- multibase_pos_count[k,1]
					mode_length <- mode_right_boundary-mode_left_boundary+1
					all_tag_mean <- mean(multibase_pos_count[p:k,2])
					all_tag_sd <- sd(multibase_pos_count[p:k,2])
					all_loc_mean <- mode_tag/mode_length
					all_loc_sd <- sqrt((sum(multibase_pos_count[p:k,2]^2)-mode_length*(all_loc_mean^2))/(mode_length-1))
					if (mode_percent>=0.95){
						candidate_mode <- rbind(candidate_mode,cbind(mode_tag,mode_percent,mode_left_boundary,mode_right_boundary,mode_length,all_tag_mean,all_tag_sd,all_loc_mean,all_loc_sd))   #9列
					}
					else{
						break
					}
				}
			}
		}
		min_length <- which(candidate_mode[,5]==min(candidate_mode[,5]))
		#mode_length最小那一行号（大于95%） 不一定只有一个
		max_percent <- which(candidate_mode[min_length,2]==max(candidate_mode[min_length,2]))
		#并且mode_percent最大
		mode_tag <-  candidate_mode[min_length[max_percent[1]],1]
		mode_percent <- candidate_mode[min_length[max_percent[1]],2]
		mode_left_boundary <- candidate_mode[min_length[max_percent[1]],3]
		mode_right_boundary <- candidate_mode[min_length[max_percent[1]],4]
		mode_length <- candidate_mode[min_length[max_percent[1]],5]
		all_tag_mean <-candidate_mode[min_length[max_percent[1]],6]
		all_tag_sd <- candidate_mode[min_length[max_percent[1]],7]
		all_loc_mean <- candidate_mode[min_length[max_percent[1]],8]
		all_loc_sd <- candidate_mode[min_length[max_percent[1]],9]
	}

	peak_mode <- rbind(peak_mode,cbind(fseq[i,],raw_tag,raw_length,peak_location,peak_tag,mode_tag,mode_percent,mode_left_boundary,mode_right_boundary,mode_length,all_tag_mean,all_tag_sd,all_loc_mean,all_loc_sd) ) #cbind一直到最后，10+13 ，最后一共23列
}
write.table(peak_mode,"MEF_minus_peak_mode_chr1.txt",sep="\t",row.names=FALSE)
