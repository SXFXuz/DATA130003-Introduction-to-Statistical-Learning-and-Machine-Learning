setwd("F:/2021秋季/机器学习/HW8")
library(openxlsx)
data <- read.xlsx('NBA.xlsx')
summary(data)

data.pca <- princomp(data[,-1],cor = T)

screeplot(data.pca,type='l', main='碎石图')

summary(data.pca,loadings=TRUE)

# 由碎石图可以看出，其拐点在第二个点，第二个点及之后的主成分所含信
#息较少，故选择第一个主成分。

# 第一主成分比较综合的考虑了各个变量。比如，出场时间多、投篮数多、命中率高的球员得分更高。
# 第二主成分主要考虑了三分球和罚球。我们发现关于三分球变量前的系数为负，说明投三分球多、三分球命中
#率高的球员得分会更低。
#第三主成分主要考虑罚球命中率和投篮命中率。

# 第一个主成分的方差占比约为64%，相对较低，且总特征数为18，如果仅使用第一主成分可能会损失较多的信息。
# 前3个主成分的总方差占比达到80%，且特征值均大于1，符合kaiser准则。另外，这几个主成分对各因子的因子负荷量
# 有较大差异。故可考虑适当增加选择主成分的数量来保留更多有用信息。

k <- 1  # 选择前k个主成分。根据上一问，取k=1。
scores <- as.data.frame(cbind(data$'球员',data.pca$scores[,1:k]))
colnames(scores) <- c('球员','主成分得分')
#展示前10名球员主成分得分
head(scores,n=10)

#可以看出，勒布朗-詹姆斯得分约为36分最高，远高于其他球员，这与其联盟现役第一人的实力相符。
# 迈克尔-乔丹、科比-布莱恩特、蒂姆-邓肯三人的得分相近，约为27分，说明这三名球员的实力相近。
# 但乔丹、科比司职得分后卫，蒂姆·邓肯司职大前锋、中锋，这些区别无法从第一主成分中看出，说明仅
#使用第一主成分会导致信息缺失，可以考虑增加主成分使用个数以保留更多信息。

set.seed(1234)
# 首先通过各样本点到其聚类中心的距离平方和与聚类数k的关系，绘制碎石图
library(factoextra)
library(ggplot2)
data.std = scale(data[,-1])
fviz_nbclust(data.std, kmeans, method = "wss") + geom_vline(xintercept = 3, linetype = 3)
#由图可以看出，选择k=4时曲线有较陡的拐点，因此选择k=3

data.km = kmeans(data.std,3,nstart=10)

library(scatterplot3d)


# 计算各样本点的前三主成分
scores2 = as.data.frame(data.pca$scores[,1:3])
scores2$cluster = factor(data.km$cluster)

library(plotly)
library(dplyr)

p <- plot_ly(scores2, x=~Comp.1, y=~Comp.2, 
             z=~Comp.3, color=~cluster) %>%
  add_markers(size=1.5)

print(p)
# 对数据前三个主成分进行可视化。
# 你可以拖拽图片以从不同角度观察这幅图，使用鼠标滚轮进行放大缩小操作。

# 使用Kmeans进行分类（分成三类），不同类别的样本以前三个主成分为坐标如图所示。
# 可以看出，以主成分为坐标的点也大致分为了三类，说明前三个主成分对数据的信息有较好的概括。
#  第一类数据点的数量最多，且第一主成分很小，其代表了大多数球员，
#  他们的是非明星球员，表现与少数的明星球员有所差距。
#  第二类数据点的第一主成分普遍比第三类大而比第二类小，并且其第三主成分分布
#  比较对称。 第三类数据点有着最大的第一主成分，且较多点有着很小的第二主成分。
#  由之前的分析，第二主成分考虑了三分球数和罚球数，且系数为负数。推测二、三类点
#  在第二主成分上的差异是由于球员的投篮习惯或司职有关。另外，三分球命中率高的球员
#  也很可能是明星球员，这与第三类点同时有着较高的第一主成分和较低的第二主成分相符。

#下面对具体数据进行分析
table(data.km$cluster)
eig <- eigen(cor(data.std))
center.pc1 <- data.km$centers  %*%  -eig$vectors[,1] 
print(center.pc1)
data.cluster <- data.frame(player=data[[1]],cluster=data.km$cluster)
head(data.cluster,n=10)
# 以第一主成分为例。三类点的第一主成分得分由小到大，分别为
# -1.246957, 3.345342, 15.183255。且第三类最高。
# 三类点的数量由多到少，分别为1956, 425, 67。
# 这与之前分析一致。第三类球员表现最好，是明显球员，第二类球员次之，表现较好。
# 第三类球员代表了联赛中的大多数球员，表现普通。

# 分类的结果能够一定程度上反映球员的价值。
# 以第一主成分得分的前10名球员为例，他们都被分为第三类球员，属于明显球员。
#  有很高的价值和水平。